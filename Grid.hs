{-# LANGUAGE OverloadedStrings #-}
module Grid where

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import Debug.Trace

data Point = Point Double Double deriving (Show, Eq)
type Triangle = (Point, Point, Point)
type Mesh = [Triangle]

instance Num Point where
  (Point x1 y1) + (Point x2 y2) = Point (x1+x2) (y1+y2)
  negate (Point x y) = Point (-x) (-y)

(.*) :: Double -> Point -> Point
f .* (Point x y) = Point (f*x) (f*y)

pointToCoords :: Point -> (Double, Double)
pointToCoords (Point x y) = (x, y)

parseMesh :: ByteString -> Mesh
parseMesh bs = go (B.lines bs) [] []
  where go (l:ls) vs fs = case B.split ' ' l of
          "v":x:y:_ -> go ls (Point (read $ B.unpack x) (read $ B.unpack y):vs) fs
          ["f", a, b, c] -> go ls vs ((read $ B.unpack a, read $ B.unpack b, read $ B.unpack c):fs)
          os -> error $ "reading mesh: " ++ show os
        go [] vs fs = doindexes (reverse vs) (reverse fs)
        doindexes vs = map (\(a,b,c) -> (vs !! (a-1), vs !! (b-1), vs !! (c-1)))

readMesh :: FilePath -> IO Mesh
readMesh fn = parseMesh <$> B.readFile fn

toBarycentric :: Point -> (Point, Point, Point) -> (Double, Double, Double)
toBarycentric (Point x y) (Point x1 y1, Point x2 y2, Point x3 y3) = (lambda1, lambda2, 1 - lambda1 - lambda2)
  where det = (y2-y3)*(x1-x3) + (x3-x2)*(y1-y3)
        lambda1 = ((y2-y3)*(x-x3) + (x3-x2)*(y-y3)) / det
        lambda2 = ((y3-y1)*(x-x3) + (x1-x3)*(y-y3)) / det
fromBarycentric :: (Double, Double, Double) -> (Point, Point, Point) -> Point
fromBarycentric (l1, l2, l3) (p1, p2, p3) = (l1 .* p1) + (l2 .* p2) + (l3 .* p3)

transformPoint :: Mesh -> Mesh -> Point -> Point
transformPoint as bs p = case filter (valid . fst) coords of
  (bc, t):_ -> fromBarycentric bc t
  _ -> Point 0 0--error $ "couldn't transform point: " ++ show p
  where pairs = zip as bs
        candidates = filter (\(a,_) -> inBounds a p) pairs
        inBounds ((Point x1 y1), (Point x2 y2), (Point x3 y3)) (Point x y) =
          not (x < x1 && x < x2 && x < x3) && not (x > x1 && x > x2 && x > x3) &&
          not (y < y1 && y < y2 && y < y3) && not (y > y1 && y > y2 && y > y3)
        coords = map (\(s,t) -> (toBarycentric p s, t)) candidates
        valid (a,b,c) = a>=0 && b>=0 && c>=0
