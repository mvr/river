{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Maybe
import Data.List.Split
import qualified Data.ByteString as BS
import Control.Monad
import Control.DeepSeq
import Control.Monad.State
import System.Directory
import System.FilePath
import Network.Curl.Download

import Vision.Image hiding (map, read)
import Vision.Image.Storage.DevIL
import Vision.Primitive

import Debug.Trace

import Mercator

import ToLocal
import Grid

zoom = 16
style = "satellite"
tileSize = (640, 640)

data Tile = Tile { center :: (Double, Double),
                   sw :: (Double, Double),
                   ne :: (Double, Double),
                   image :: RGB
                 } deriving (Show)

inTileBounds :: Tile -> (Double, Double) -> Bool
inTileBounds Tile{ sw=(minlat, minlng), ne=(maxlat, maxlng) } (lat, lng) = minlat <= lat && lat <= maxlat && minlng <= lng && lng <= maxlng

latLngToTilePoint :: Tile -> (Double, Double) -> (Int, Int)
latLngToTilePoint t latlng = (round $ (px - swx) * scale, round $ (-py + swy) * scale)
  where scale = 2^zoom
        (px, py) = latLngToPoint latlng
        (swx, swy) = latLngToPoint (sw t)

tilePixelAtLatLng :: Tile -> (Double, Double) -> Maybe RGBPixel
tilePixelAtLatLng t latlng = let (x, y) = latLngToTilePoint t latlng
                                 (w, h) = tileSize in
                             if x > 0 && x < w && y > 0 && y < h then
                               Just $ index (image t) (Z :. (h-y)  :. x)
                             else
                               Nothing

type TileSet = [Tile]

lookupPixel :: (Double, Double) -> StateT TileSet IO RGBPixel
lookupPixel latlng = do
  ts <- get
  let existingTiles = mapMaybe (flip tilePixelAtLatLng latlng) ts
  case existingTiles of
    p:_ -> return p
    []  -> do
      newTile <- liftIO $ loadTileFor latlng
      let (Just p) = tilePixelAtLatLng newTile latlng
      put $ newTile:ts
      return p

loadTileFor :: (Double, Double) -> IO Tile
loadTileFor c@(lat, lng) = do
  let filename = "tiles/" ++ style ++ "_" ++ show lat ++ "_" ++ show lng ++ "_" ++ show zoom ++ ".png"
      url = "https://maps.googleapis.com/maps/api/staticmap?center=" ++ show lat ++ "," ++ show lng ++
            "&zoom=" ++ show zoom ++ "&size=" ++ show w ++ "x" ++ show h ++
            "&maptype=" ++ style
      (w, h) = tileSize

  exists <- doesFileExist filename
  if exists then
    putStrLn $ "loading " ++ filename
  else do
    putStrLn $ "downloading " ++ url
    Right png <- openURI url
    BS.writeFile filename png

  Right image <- load Autodetect filename
  let (sw, ne) = getCorners c zoom tileSize
  return $ Tile c sw ne image

loadExistingTiles :: FilePath -> IO TileSet
loadExistingTiles dir = do
  files <- getDirectoryContents dir
  catMaybes <$> mapM readTile files
  where readTile file = case splitOn "_" file of
          [style',lat,lng,zoom'] ->
            if style /= style' || zoom /= read (head $ splitOn "." zoom') then
              return Nothing
            else do
              Right image <- load Autodetect (dir </> file)
              let (sw, ne) = getCorners (read lat, read lng) zoom tileSize
              return $ Just $ Tile (read lat, read lng) sw ne image
          _ -> return Nothing

main :: IO ()
main = do
  source <- readMesh "grid.off"
  target <- readMesh "transformed.off"
  let width = 4000
      height = floor $ 2 * gridheight * fromIntegral width
      psize = Z :. height :. width :: (Z :. Int :. Int)
      pixels = map (\y -> map (\x -> (x, y)) [0..width-1]) [0..height-1]

      pictureToLocal :: (Int, Int) -> (Double, Double)
      pictureToLocal (x, y) = (squash * fromIntegral x / fromIntegral width , gridheight * squash * (fromIntegral (y - (height `div` 2))) / fromIntegral height)

  transformed <- mapM (\row -> do
                           putStrLn $ "Doing row " ++ show (snd $ head row)
                           return $ map (fromLocal . pointToCoords . transformPoint source target . uncurry Point . pictureToLocal) row
                      ) pixels
  existingTiles <- loadExistingTiles "tiles"
  putStrLn $ "loaded " ++ show (length existingTiles) ++ " tiles"
  rgbs <- evalStateT ((mapM $ mapM lookupPixel) transformed) existingTiles

  let image = fromFunction psize f :: RGB
      f (Z :. y :. x) = rgbs !! y !! x
  save Autodetect "out.png" image
  return ()
