module Mercator where

import Debug.Trace

mercatorRange :: Double
mercatorRange = 256

degToRad :: Floating a => a -> a
degToRad d = d * (pi / 180)
radToDeg :: Floating a => a -> a
radToDeg r = r / (pi / 180)

pixelOrigin :: (Double, Double)
pixelOrigin = (mercatorRange / 2, mercatorRange / 2)

pixelsPerLonDegree :: Double
pixelsPerLonDegree = mercatorRange / 360
pixelsPerLonRadian :: Double
pixelsPerLonRadian = mercatorRange / (2 * pi)

bound :: Ord c => c -> c -> c -> c
bound mn mx = max mn . min mx

latLngToPoint :: (Double, Double) -> (Double, Double)
latLngToPoint (lat, lng) = (x, y)
  where x = (fst pixelOrigin) + lng * pixelsPerLonDegree
        siny = bound (-0.9999) 0.9999 $ sin $ degToRad lat
        y = (snd pixelOrigin) + 0.5 * log ((1 + siny) / (1 - siny)) * (-pixelsPerLonRadian)

pointToLatLng :: (Double, Double) -> (Double, Double)
pointToLatLng (x, y) = (lat, lng)
  where lng = (x - (fst pixelOrigin)) / pixelsPerLonDegree
        latRadians = (y - (snd pixelOrigin)) / (-pixelsPerLonRadian)
        lat = radToDeg $ 2 * atan (exp latRadians) - (pi / 2)

getCorners :: (Double, Double) -> Int -> (Int, Int) -> ((Double, Double), (Double, Double))
getCorners latlng zoom (w, h) = (pointToLatLng swpoint, pointToLatLng nepoint)
  where scale = 2^zoom
        centerPoint = latLngToPoint latlng
        swpoint = ((fst centerPoint) - (fromIntegral w / 2)/scale, (snd centerPoint) + (fromIntegral h / 2)/scale)
        nepoint = ((fst centerPoint) + (fromIntegral w / 2)/scale, (snd centerPoint) - (fromIntegral h / 2)/scale)
