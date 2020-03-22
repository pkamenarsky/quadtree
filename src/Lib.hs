-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
    ( someFunc
    ) where

import Data.IORef
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed.Deriving as V
import Data.Word

import GHC.Generics

data Point = Point
  { x :: {-# UNPACK #-} !Float
  , y :: {-# UNPACK #-} !Float
  }

V.derivingUnbox "Point"
  [t| Point -> (Float, Float) |]
  [| \(Point x y) -> (x, y) |]
  [| \(x, y) -> Point x y |]

data AABB = AABB
  { nw :: {-# UNPACK #-} !Point
  , se :: {-# UNPACK #-} !Point
  }

V.derivingUnbox "AABB"
  [t| AABB -> (Point, Point) |]
  [| \(AABB x y) -> (x, y) |]
  [| \(x, y) -> AABB x y |]
insideAABB 

--------------------------------------------------------------------------------

maxLeafPoints :: Word8
maxLeafPoints = 10

data QT
  = Node AABB (IORef QT) (IORef QT) (IORef QT) (IORef QT) 
  | Leaf AABB (IORef Word8) (V.IOVector Point)

empty :: AABB -> IO QT
empty aabb = Leaf <$> pure aabb <*> newIORef 0 <*> V.new (fromIntegral maxLeafPoints)

insert :: Point -> QT -> IO ()
insert p (Leaf aabb cntRef points) = do
  cnt <- readIORef cntRef

  if cnt < maxLeafPoints
    then do
      V.write points (fromIntegral cnt) p
      atomicModifyIORef' cntRef $ \x -> (x + 1, ())
    else undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"
