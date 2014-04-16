{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Locations where

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Data.Maybe
import Data.List
import Data.Aeson
import System.Directory (getDirectoryContents)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as Vec
import qualified Data.Text as T

-- | Helper to deal with Aeson.decode
resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe _ = Nothing

data Location = Location { locLon :: Double, 
                           locLat :: Double 
                         } deriving (Show, Eq, Ord)

instance FromJSON Location where
    parseJSON (Object o) =
        Location <$>
        o .: "lon" <*>
        o .: "lat"
    parseJSON _ = empty
    
data DocumentLocations = DocumentLocations Text [Location]

instance FromJSON DocumentLocations where
    parseJSON (Object o) =
        DocumentLocations <$>
        o .: "file_name" <*>
        o .: "coordinates"
    parseJSON _ = empty

data DocumentReference = DocumentReference FilePath Text  -- ^ sessionId filename
                         deriving (Show, Eq, Ord)
data MapLocations = MapLocations FilePath (Map Location [DocumentReference])

loadLocations :: FilePath -> IO MapLocations
loadLocations dataDir =
    (filter ((/= '.') . head) <$> getDirectoryContents dataDir) >>=
    (MapLocations dataDir <$>) .
    foldM (\result sessionId ->
               do let docRef = DocumentReference sessionId
                      locsPath = dataDir ++ "/" ++ sessionId ++ "/locations.json"
                  mDocLocs <- E.catch (decode <$>
                                       LB.fromChunks <$> (:[]) <$>
                                       B.readFile locsPath) $
                              \(_ :: E.SomeException) -> return Nothing
                  let result''' =
                         case mDocLocs of
                           Just docLocs ->
                               foldl' 
                               (\result' (DocumentLocations filename locs) ->
                                    let docRef' = docRef filename
                                    in foldl' 
                                       (\result'' loc ->
                                            Map.insertWith (++) loc [docRef'] result''
                                       ) result' locs
                               ) result docLocs
                           _ ->
                               result
                  result''' `seq` return result'''
          ) Map.empty

proximity :: Location -> MapLocations -> [(Double, Location, DocumentReference)]
proximity loc (MapLocations _ locs) =
    let (lesser, mMatch, greater) = Map.splitLookup loc locs
        candidates = (case mMatch of
                        Just match -> [(loc, match)]
                        Nothing -> []
                     ) ++ 
                     concat
                     (zipWith (\less great ->
                                   [less, great]
                              )
                      (Map.toDescList lesser)
                      (Map.toAscList greater)
                     )
        -- | No need for sqrt as we just use it for reordering
        distanceSq (Location lon1 lat1) (Location lon2 lat2) =
            ((lon1 - lon2) ** 2) +
            ((lat1 - lat2) ** 2)
        distances = map (\(candidateLoc, candidateDocs) ->
                             (distanceSq loc candidateLoc, 
                              candidateLoc, 
                              candidateDocs)
                        ) candidates
        splitMin :: Ord a => Int -> [a] -> (a, [a])
        splitMin n xs = let (xs', xs''') = splitAt n xs
                            m = minimum xs'
                            xs'' = filter (/= m) xs'
                        in (m, xs'' ++ xs''')
        reorder [] = []
        reorder distances' = 
            let (next, distances'') = splitMin 5000 distances'
            in next : reorder distances''
    in concatMap (\(distSq, docLoc, docRefs) ->
                      map (\docRef ->
                               (distSq, docLoc, docRef)
                          ) docRefs
                 ) $
       reorder distances

newtype SessionMeta = SessionMeta {
      sessionDocuments :: [DocumentMeta]
    } deriving (Show)
               
instance FromJSON SessionMeta where
    parseJSON o@(Object _) =
        case collectDocuments o of
          [] -> empty
          docMetas -> return $ SessionMeta docMetas
        
        where collectDocuments :: Value -> [DocumentMeta]
              collectDocuments value@(Object _) =
                  case fromJSON value of
                    Success docMeta ->
                        -- Found one document
                        [docMeta]
                    _ ->
                        -- Descent
                        fromMaybe [] $
                        do let mo :: Maybe (Map Text Value)
                               mo = resultToMaybe $
                                    fromJSON value
                           subDocs <- concatMap collectDocuments <$>
                                      Map.elems <$>
                                      mo
                           Just $
                                maybe subDocs
                                (\description -> 
                                     -- There's an up-level description,
                                     -- prepend it:
                                     map (\docMeta ->
                                              docMeta {
                                                docDescription = description : docDescription docMeta
                                              }
                                         ) subDocs) $
                                mo >>= 
                                Map.lookup "description" >>= 
                                resultToMaybe . fromJSON
              collectDocuments (Array a) =
                  concatMap collectDocuments $
                  Vec.toList a
              collectDocuments _ = []
              
    parseJSON _ = empty

data DocumentMeta = DocumentMeta { 
      docFileName :: Text, 
      docDescription :: [Text]
    } deriving (Show)
               
instance FromJSON DocumentMeta where
    parseJSON (Object o) =
        DocumentMeta <$>
        o .: "file_name" <*>
        ((:[]) <$> o .: "description")
    parseJSON _ = empty


data Match = Match {
      matchLocation :: Location,
      matchDocument :: DocumentReference,
      matchDescription :: [Text]
    }

instance ToJSON Match where
  toJSON m =
      let loc = matchLocation m
          DocumentReference sessionId baseName = matchDocument m
          path = sessionId ++ "/" ++ T.unpack baseName
      in object
             ["lon" .= locLon loc,
              "lat" .= locLat loc,
              "path" .= path,
              "description" .= matchDescription m
             ]

findDocs :: Location -> MapLocations -> IO [Match]
findDocs loc mapLocs@(MapLocations dataDir _) =
    do let matches = proximity loc mapLocs
           defaultAmount = 50
           maxAmount = 1000
           matches' = map (\(_, docLoc, docRef) ->
                               (docLoc, docRef)
                            ) $
                      take maxAmount $
                      foldl (\rs ms ->
                                 if length rs < defaultAmount
                                 then rs ++ ms
                                 else rs
                            ) [] $
                      groupBy (\(dist1, _, _) (dist2, _, _) ->
                                   dist1 == dist2
                              ) $
                      matches
           sessionIds = nub $
                        map (\(_, (DocumentReference sessionId _)) ->
                                 sessionId
                            ) matches'
       metaMap <- foldM (\metaMap sessionId ->
                             do let metaFile = dataDir ++ "/" ++ 
                                               sessionId ++ "/metadata.json"
                                mMeta <- decode <$>
                                         LB.fromChunks <$> (:[]) <$>
                                         B.readFile metaFile
                                return $
                                       case mMeta of
                                         Just meta ->
                                             let metaMap' = Map.insert sessionId meta metaMap
                                             in metaMap' `seq` metaMap'
                                         _ ->
                                             metaMap
                        ) Map.empty sessionIds
                  
       let lookupDescription (DocumentReference sessionId baseName) =
                         case Map.lookup sessionId metaMap of
                           Just session ->
                               let matchingDocs =
                                       filter ((== baseName) . docFileName) $
                                       sessionDocuments session
                               in case matchingDocs of
                                    [] -> []
                                    doc : _ -> docDescription doc
                           Nothing ->
                               []
       return $
              do (docLoc, docRef) <- matches'
                 let description = lookupDescription docRef
                 return $ Match docLoc docRef description
