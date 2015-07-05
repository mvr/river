{-# LANGUAGE TupleSections #-}
module ToLocal where

import Control.Applicative
import Data.Csv
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V hiding ((++))

import Mercator

xCenter = 236.8
yCenter = 148.3
scale = 2.2244929251669747
squash = 0.7
gridheight = 0.13

readCoords :: FilePath -> IO [(Double, Double)]
readCoords f = do
  e <- decode NoHeader <$> B.readFile f
  case e of
    Left _ -> error $ "Broken coord file: " ++ f
    Right r -> return (V.toList r)

writeCoords :: FilePath -> [(Double, Double)] -> IO ()
writeCoords f d = B.writeFile f (encode d)

pairs :: [a] -> [(a, a)]
pairs z = zip z (tail z)

midpoint :: (Double, Double) -> (Double, Double) -> (Double, Double)
midpoint (x1, y1) (x2, y2) = ((x1+x2)/2, (y1+y2)/2)

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

toLocal :: (Double, Double) -> (Double, Double)
toLocal p = let (x, y) = latLngToPoint p
            in ((x - xCenter) * scale, (y - yCenter) * scale)

fromLocal :: (Double, Double) -> (Double, Double)
fromLocal (x, y) = let p = ((x / scale) + xCenter, (y / scale) + yCenter)
                   in pointToLatLng p

positions :: [(Double, Double)] -> [Double]
positions ps = scanl (+) 0 distances
  where distances = map (*squash) $ map (uncurry distance) (pairs ps)

pairUp :: [(Double, Double)] -> [((Double, Double), (Double, Double))]
pairUp (a:b:as) = (a,b):(pairUp as)
pairUp [a] = error "Odd number of points"
pairUp [] = []

makeOutline :: Double -> [(Double, Double)] -> [(Double, Double)]
makeOutline verticalSpace latlngs = [(0, verticalSpace*squash),
                                     (end, verticalSpace*squash),
                                     (end, -verticalSpace*squash),
                                     (0, -verticalSpace*squash)] ++ points
  where pairs = pairUp latlngs
        ps = positions $ map (toLocal . uncurry midpoint) pairs
        widths = map ((*squash) . uncurry distance) $ map (\(a,b) -> (toLocal a, toLocal b)) pairs
        points = concatMap (\(x, w) -> [(x,-w/2), (x, w/2)]) (zip ps widths)
        end = last ps

-- main = do
--   coords <- readCoords "kmllatlngs.txt"
--   let outline = makeOutline 0.13 coords
--   writeCoords "outline.csv" outline
--   writeCoords "targets.csv" $ map toLocal coords
