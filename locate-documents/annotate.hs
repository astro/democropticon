{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import System.Environment
import System.Directory (getDirectoryContents)
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Data.List hiding (find, words)
import Data.Maybe
import Data.Char


main :: IO ()
main = 
    do args <- getArgs
       case args of
         [locsFile, dataDir] ->
             loadLocations locsFile >>=
             flip run dataDir
         _ ->
             putStrLn "Call with dresden.json and ratsinfo-scraper/data"
             
             
data Location = Location { locLon :: Double, 
                           locLat :: Double 
                         } deriving (Show, Eq)
instance ToJSON Location where
  toJSON loc =
    object $
    ["lon" .= locLon loc,
     "lat" .= locLat loc
    ]

type Locations = Map (Text, Maybe Text) Location
       
data Address = Address { addrLocation :: Location,
                         addrStreet :: Text,
                         addrHouse :: Maybe Text
                       } deriving (Show, Eq)
instance FromJSON Address where
    parseJSON (Object o) =
        Address <$>
        (Location <$>
         o .: "lon" <*>
         o .: "lat") <*>
        o .: "street" <*>
        o .:? "house"
    parseJSON _ = empty

-- TODO: normalize case?
loadLocations :: String -> IO Locations
loadLocations locsFile =
    do Just json <- decode <$> 
                    LB.readFile locsFile
       let Success addrs = fromJSON json
       return $ 
              Map.fromList $
              map (\addr ->
                       ((addrStreet addr, addrHouse addr), addrLocation addr)
                  ) addrs

findLocation :: (Text, Maybe Text) -> Locations -> Maybe (Int, Location)
findLocation (street, house) locs =
    let (b, c, a) = Map.splitLookup (street, house) locs
    in case c of
         Just loc -> Just (0, loc)
         Nothing ->
             listToMaybe $
             sortBy (\(d1, _) (d2, _) ->
                         compare d1 d2
                    ) $
             mapMaybe (\((street1, house1), loc1) ->
                           do d1 <- distance street house street1 house1
                              return (d1, loc1)
                      ) $
             concat $
             transpose [takeSameStreet $ Map.toDescList b, takeSameStreet $ Map.toAscList a]
    where takeSameStreet = takeWhile $ 
                           \((street1, _), _) ->
                           street1 == street
                   
distance :: Text -> Maybe Text -> Text -> Maybe Text -> Maybe Int
distance street1 house1 street2 house2
    | street1 == street2 &&
      house1 == house2 = 
          Just 0
    | street1 == street2 =
        Just $
        case (house1, house2) of
          (Just h1, Just h2) ->
              case (readNumber h1, readNumber h2) of
                (Just n1, Just n2) -> abs $ n2 - n1
                _ -> 10
          (Just _, Nothing) -> 100
          (Nothing, Just _) -> 50
          (Nothing, Nothing) -> 0
    | otherwise =
          Nothing
        
    where readNumber :: Text -> Maybe Int
          readNumber t =
              case reads $ T.unpack t of
                (n, _):_ -> Just n
                _ -> Nothing

run locs dataDir =
    seq locs $
    void $
    mapDirectory dataDir $ \sessionDir ->
    do filesLocs <- 
           catMaybes <$>
           mapDirectory (dataDir ++ "/" ++ sessionDir)
           (\fileName ->
                case break (== '.') fileName of
                  (baseName, ".txt") ->
                      do let path = dataDir ++ "/" ++ 
                                 sessionDir ++ "/" ++ 
                                 fileName
                         file <- decodeUtf8 <$> B.readFile path
                         let fileLocs = map addrLocation $ 
                                        findFileLocations locs file
                         putStrLn $ path ++ " (" ++ show (length fileLocs) ++ " locations in " ++ show (T.length file) ++ " bytes)"
                         return $
                                case fileLocs of
                                  [] -> Nothing 
                                  _:_ -> Just (baseName, fileLocs)
                  _ -> 
                      return Nothing
           )
       when (not $ null filesLocs) $
            do let json = toJSON $
                          map (\(baseName, fileLocs) ->
                                   object ["file_name" .= (baseName ++ ".pdf"),
                                           "coordinates" .= fileLocs
                                          ]
                              )
                          filesLocs
                     
                   locFile = dataDir ++ "/" ++ sessionDir ++ "/locations.json"
               LB.writeFile locFile $ encode json
              
mapDirectory path f = 
    (filter ((/= '.') . head) <$>
     getDirectoryContents path) >>= mapM f
    
findFileLocations :: Locations -> Text -> [Address]
findFileLocations locs file =
    let wordsTails = tails $ fileWords file
        wordsTails' = take (length wordsTails - 1) wordsTails
    in nub $
       map snd $
       collectMinimal $ 
       mapMaybe wordsToAddresses wordsTails'
  
      where fileWords :: Text -> [Text]
            fileWords t = 
                case T.break isSep t of
                  (word, t')
                      | T.null word && T.null t' ->
                          []
                      | T.null word ->
                          fileWords $ T.tail t'
                      | T.null t' ->
                          [word]
                      | otherwise ->
                          word : fileWords (T.tail t')
            
            isSep = (`elem` ",!.? \t\n\r\f")
            
            wordsToAddresses :: [Text] -> Maybe (Int, Address)
            --wordsToAddresses [] = Nothing
            wordsToAddresses words =
                let wordGroups = tail $ inits words
                    takeWhileJust :: [Maybe a] -> [a]
                    takeWhileJust = map fromJust .
                                    takeWhile isJust
                    getStreetHouse wordGroup =
                        case break (isDigit . T.head) wordGroup of
                          (streetWords, []) ->
                              Just (T.unwords streetWords, Nothing)
                          (streetWords, [houseWord]) ->
                              Just (T.unwords streetWords, Just houseWord)
                          (_, _:_) ->
                              Nothing
                    distancesAddresses = 
                        takeWhileJust $
                        map (\(street, house) ->
                                 do (d, loc) <- findLocation (street, house) locs
                                    return (d, Address loc street house)
                            ) $
                        takeWhileJust $
                        map getStreetHouse 
                        wordGroups
                    findBestAddress [] = Nothing
                    findBestAddress ((d1, a1):das) =
                        case das of
                          (d2, _):_ | d2 <= d1 -> 
                                 findBestAddress das
                          _ ->
                                 Just (d1, a1)

                in findBestAddress distancesAddresses
                   
            collectMinimal :: [(Int, a)] -> [(Int, a)]
            collectMinimal =
                foldl (\result (d, a) ->
                           case result of
                             [] -> [(d, a)]
                             (d', _) : _
                                 | d' < d -> result
                                 | d' > d -> [(d, a)]
                                 | otherwise -> result ++ [(d, a)]
                      ) []
