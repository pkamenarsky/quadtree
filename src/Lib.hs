-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Data.IORef
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed.Deriving as V
import qualified Data.List as L
import qualified Data.Set as S
import Data.Word

import System.Random

import GHC.Generics

data Point = Point
  { x :: {-# UNPACK #-} !Word32
  , y :: {-# UNPACK #-} !Word32
  } deriving (Eq, Ord, Show)

V.derivingUnbox "Point"
  [t| Point -> (Word32, Word32) |]
  [| \(Point x y) -> (x, y) |]
  [| \(x, y) -> Point x y |]

data AABB = AABB
  { nw :: {-# UNPACK #-} !Point
  , se :: {-# UNPACK #-} !Point
  } deriving (Eq, Show)

V.derivingUnbox "AABB"
  [t| AABB -> (Point, Point) |]
  [| \(AABB x y) -> (x, y) |]
  [| \(x, y) -> AABB x y |]

insideAABB :: AABB -> Point -> Bool
insideAABB (AABB (Point x1 y1) (Point x2 y2)) (Point x y)
  | x < x1 || x >= x2 || y < y1 || y >= y2 = False
  | otherwise = True

overlapAABB :: AABB -> AABB -> Bool
overlapAABB
  (AABB (Point x1 y1) (Point x2 y2))
  (AABB (Point u1 v1) (Point u2 v2))
  | x1 >= u2 || x2 <= u1 || y1 >= v2 || y2 <= v1 = False
  | otherwise = True

splitAABB :: AABB -> (AABB, AABB, AABB, AABB)
splitAABB (AABB (Point x1 y1) (Point x2 y2))
  | x1 == xh || y1 == yh = error "AABB too small"
  | otherwise =
    ( (AABB (Point x1 y1) (Point xh yh))
    , (AABB (Point xh y1) (Point x2 yh))
    , (AABB (Point x1 yh) (Point xh y2))
    , (AABB (Point xh yh) (Point x2 y2))
    )
  where
    xh = (x1 + x2) `div` 2
    yh = (y1 + y2) `div` 2

--------------------------------------------------------------------------------

maxLeafPoints :: Word8
maxLeafPoints = 100

data Node
  = Node AABB QT QT QT QT 
  | Leaf AABB (IORef Word8) (V.IOVector Point)

type QT = IORef Node

empty :: AABB -> IO QT
empty aabb = emptyLeaf aabb >>= newIORef

emptyLeaf :: AABB -> IO Node
emptyLeaf aabb = Leaf <$> pure aabb <*> newIORef 0 <*> V.new (fromIntegral maxLeafPoints)

aabbForQT :: QT -> IO AABB
aabbForQT qt = do
  node <- readIORef qt
  case node of
    Node aabb _ _ _ _ -> pure aabb
    Leaf aabb _ _ -> pure aabb

-- TODO: descent test aabb on every node or every leaf?
insert :: Point -> QT -> IO ()
insert p qt = do
  node <- readIORef qt

  case node of
    (Node aabb q1 q2 q3 q4) -> do
      aabb <- aabbForQT q1
      if insideAABB aabb p
        then insert p q1
        else do
          aabb <- aabbForQT q2
          if insideAABB aabb p
            then insert p q2
            else do
              aabb <- aabbForQT q3
              if insideAABB aabb p
                then insert p q3
                else do
                  aabb <- aabbForQT q4
                  if insideAABB aabb p
                    then insert p q4
                    else error $ "Out of bounds (node): " <> show aabb <> ", " <> show p

    (Leaf aabb cntRef points) -> do
      if insideAABB aabb p
        then pure ()
        else error $ "Out of bounds (leaf): " <> show aabb <> ", " <> show p
        
      cnt <- readIORef cntRef
    
      if cnt < maxLeafPoints
        then do
          V.write points (fromIntegral cnt) p
          writeIORef cntRef (cnt + 1)
        else do
          node' <- Node
            <$> pure aabb
            <*> (leaf q1 >>= newIORef)
            <*> (leaf q2 >>= newIORef)
            <*> (leaf q3 >>= newIORef)
            <*> (leaf q4 >>= newIORef)
          writeIORef qt node'
      where
        (q1, q2, q3, q4) = splitAABB aabb
    
        leaf naabb = if insideAABB naabb p
          then do
            cntRef <- newIORef 1
            points <- V.new (fromIntegral maxLeafPoints)
            V.write points 0 p
            pure (Leaf naabb cntRef points)
          else
            emptyLeaf naabb

query :: AABB -> QT -> IO [Point]
query aabb qt = do
  node <- readIORef qt

  case node of
    Leaf naabb _ points -> if overlapAABB aabb naabb
      then do
        points' <- V.freeze points
        pure $ filter (insideAABB aabb) $ V.toList points'
      else
        pure []
    Node naabb q1 q2 q3 q4 -> if overlapAABB aabb naabb
      then
        mconcat <$> traverse (query aabb) [q1, q2, q3, q4]
      else
        pure []

someFunc :: IO ()
someFunc = do
  qt <- empty (AABB (Point 0 0) (Point 1000 1000))

  rps <- flip traverse [0..1000] $ \_ -> do
    x <- randomRIO (0, 999)
    y <- randomRIO (0, 999)

    pure (Point x y)

  sequence_ $ flip map rps $ \p -> insert p qt

  let aabb = AABB (Point 100 100) (Point 200 200)

  ps1 <- L.sort <$> query aabb qt
  ps2 <- fmap L.sort $ pure $ filter (insideAABB aabb) rps

  print ps1
  print ps2

  print "Done"
  print (ps1 == ps2)
  
