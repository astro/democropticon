{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Application where

import Control.Applicative
import Yesod
import Yesod.Static
import Text.Hamlet
import Network.Wai
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.RequestLogger
import qualified Data.Text as T

import Locations


instance PathPiece Double where
    fromPathPiece t = 
        case reads $ T.unpack t of
          [(d, "")] -> Just d
          _ -> Nothing

data AppState = AppState { 
      appLocs :: MapLocations,
      appPDFs :: Static,
      appStatic :: Static
    }

publicFiles "static/"

mkYesod "AppState" [parseRoutes|
                    / FrontR GET
                    /documents-by-location/#Double/#Double ByLocationR GET
                    /pdf PDFsR Static appPDFs
                    /static StaticR Static appStatic
                    |]

instance Yesod AppState where
    defaultLayout widget =
        do pc <- widgetToPageContent widget
           giveUrlRenderer $(hamletFile "templates/default-layout.hamlet")

    makeSessionBackend = const $ return Nothing
    

getFrontR :: HandlerT AppState IO RepHtml
getFrontR =
    defaultLayout $ 
    do setTitle "Ratskarte"
       toWidget $(hamletFile "templates/front.hamlet")
       toWidgetHead [hamlet|
                     <link rel="stylesheet" href=@{StaticR leaflet_css}>
                     |]
       addScript $ StaticR angular_min_js
       addScript $ StaticR leaflet_js
       addScript $ StaticR ratskarte_js
  
getByLocationR :: Double -> Double -> HandlerT AppState IO RepJson
getByLocationR lon lat =
    do locs <- appLocs <$> getYesod
       let loc = Location lon lat
       matches <- lift $ findDocs loc locs
       return $ repJson $
              array matches

-- | Constructs application
app :: IO Application
app =
    yApp >>=
    toWaiAppPlain >>=
    return . logStdout . autohead . enableCORS
    
    where yApp = AppState <$>
                 loadLocations "data" <*>
                 static "data" <*>
                 static "static"
                 
          enableCORS :: Middleware
          enableCORS app req =
              withHeaders (("Access-Control-Allow-Origin", "*") :) <$> 
              app req
              
          withHeaders f res =
              case res of
                ResponseFile s hs path part ->
                    ResponseFile s (f hs) path part
                ResponseBuilder s hs b ->
                    ResponseBuilder s (f hs) b
                ResponseSource s hs src ->
                    ResponseSource s (f hs) src
