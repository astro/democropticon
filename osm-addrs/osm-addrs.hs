{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Main where

import System.Environment
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.String (fromString)
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.XML.Types
import Text.XML.Stream.Parse
import Text.XML.Stream.Render
import System.IO
import Blaze.ByteString.Builder (toByteString)
import qualified Data.Text as T
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.Aeson


main :: IO ()
main = do args <- getArgs
          case args of
            [city] -> 
              run $ fromString city
            _ ->
              putStrLn "Call with city name"

run :: Text -> IO ()
run city =
  sourceHandle stdin $=
  parseBytes def =$=
  processDocument city $$
  -- | Output JSON
  (do yield "["
      let forward = mapM yield . LB.toChunks . encode
          loop first = 
            await >>=
            maybe (return ())
            (\addr ->
              do when (not first) $
                   yield ",\n"
                 forward addr
                 loop False
            )
      loop True
      yield "]"
  ) =$
  -- | Output data suitable for GNUplot
  -- CL.map (\addr -> 
  --          BC.pack $
  --          show (locLat $ addrLocation addr) ++ " " ++
  --          show (locLon $ addrLocation addr) ++ "\n"
  --        ) =$
  sinkHandle stdout
  

data Location = Location { locLon :: Double, 
                           locLat :: Double 
                         } deriving (Show, Eq)
data Address = Address { addrLocation :: Location,
                         addrStreet :: Text,
                         addrHouse :: Maybe Text
                       } deriving (Show, Eq)

instance ToJSON Address where
  toJSON addr =
    object ["lon" .= locLon (addrLocation addr),
            "lat" .= locLat (addrLocation addr),
            "street" .= addrStreet addr,
            "house" .= addrHouse addr
           ]
  
processDocument :: Text -> Monad m => Conduit Event m Address
processDocument city =
  do mEv <- await
     case mEv of
       Nothing ->
         -- EOF
         return ()
       Just (EventBeginElement "osm" _) ->
         CL.sequence processNodes =$=
         CL.map F.toList =$=
         CL.mapMaybe (eventsToAddress city)
         --CL.sequence deduplicate
       Just _ ->
         -- Ignore
         processDocument city
  
  
processNodes :: Monad m => Consumer Event m (Seq Event)
processNodes =
  do mEv <- await
     case mEv of
       Nothing ->
         -- EOF
         return Seq.empty
       Just ev@(EventBeginElement name _) ->
         do let ev' = EventEndElement name
            r <- sinkUntil (== ev')
            return $ ev <| (r |> ev')
       Just _ ->
         -- Ignore
         processNodes
     
sinkUntil :: Monad m => (a -> Bool) -> Consumer a m (Seq a)
sinkUntil cond =
  do mA <- await
     case mA of
       Just a | not (cond a) ->
          fmap (a <|) $ sinkUntil cond
       _ ->
         return Seq.empty

eventsToAddress :: Text -> [Event] -> Maybe Address
eventsToAddress city evs =
  do EventBeginElement "node" nodeAttrs <-
           listToMaybe $
           filter (\ev ->
                      case ev of
                        EventBeginElement "node" _ -> True
                        _ -> False
                  ) evs
     let nodeAttr name =
           read <$> T.unpack <$> contentToText <$> name `lookup` nodeAttrs
     let tagEl k =
           listToMaybe $
           catMaybes $
           map (\ev ->
                  case ev of
                    EventBeginElement "tag" tagAttrs
                      | (contentToText <$> "k" `lookup` tagAttrs) == Just k ->
                        contentToText <$> "v" `lookup` tagAttrs
                    _ ->
                      Nothing
               ) evs
     loc <- Location <$>
            (nodeAttr "lon") <*>
            (nodeAttr "lat")
     city <- tagEl "addr:city"
     Address loc <$>
       (tagEl "addr:street") <*>
       (return $ tagEl "addr:housenumber")

contentToText :: [Content] -> Text
contentToText = T.concat . map (\(ContentText t) -> t)

deduplicate :: Monad m => Sink Address m [Address]
deduplicate = map mergeAddrs <$>
              HM.elems <$>
              -- Group by address:
              CL.fold (flip $ \addr ->
                        let k = (addrStreet addr, addrHouse addr)
                        in HM.insertWith (++) k [addr]
                      ) (HM.empty :: HM.HashMap (Text, Maybe Text) [Address])
  where mergeAddrs :: [Address] -> Address
        mergeAddrs [addr] = addr
        mergeAddrs addrs@(addr:_) =
          let avg xs = sum xs / fromIntegral (length xs)
              locs = map addrLocation addrs
              lats = map locLat locs
              lons = map locLon locs
          in addr { addrLocation = Location (avg lons) (avg lats) }
