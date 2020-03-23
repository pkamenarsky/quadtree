{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Control.Monad (replicateM)

import Data.IORef
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed.Deriving as V
import qualified Data.Vector.Serialize as V
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Serialize as B
import Data.Word

import System.Random

import GHC.Generics

data Point = Point
  { x :: {-# UNPACK #-} !Word32
  , y :: {-# UNPACK #-} !Word32
  } deriving (Eq, Ord, Show, Generic, B.Serialize)

V.derivingUnbox "Point"
  [t| Point -> (Word32, Word32) |]
  [| \(Point x y) -> (x, y) |]
  [| \(x, y) -> Point x y |]

data AABB = AABB
  { nw :: {-# UNPACK #-} !Point
  , se :: {-# UNPACK #-} !Point
  } deriving (Eq, Show, Generic, B.Serialize)

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
  = Node !AABB !QT !QT !QT !QT 
  | Leaf !AABB !(IORef Word8) !(VM.IOVector Point)

type QT = IORef Node

data INode
  = INode AABB INode INode INode INode
  | ILeaf AABB (V.Vector Point)
  deriving (Generic, B.Serialize)

freeze :: QT -> IO INode
freeze qt = do
  node <- readIORef qt

  case node of
    Leaf aabb cntRef points -> do
      cnt <- readIORef cntRef
      points' <- V.freeze $ VM.slice 0 (fromIntegral cnt) points
      pure $ ILeaf aabb points'
    Node aabb q1 q2 q3 q4 -> do
      q1' <- freeze q1
      q2' <- freeze q2
      q3' <- freeze q3
      q4' <- freeze q4
      pure $ INode aabb q1' q2' q3' q4'

thaw :: INode -> IO QT
thaw (ILeaf aabb points) = do
  points' <- V.thaw points
  cntRef  <- newIORef (fromIntegral $ V.length points)

  VM.grow points' (fromIntegral maxLeafPoints - V.length points)

  newIORef $ Leaf aabb cntRef points'
thaw (INode aabb q1 q2 q3 q4) = do
  q1' <- thaw q1
  q2' <- thaw q2
  q3' <- thaw q3
  q4' <- thaw q4

  newIORef $ Node aabb q1' q2' q3' q4'

empty :: AABB -> IO QT
empty aabb = emptyLeaf aabb >>= newIORef

emptyLeaf :: AABB -> IO Node
emptyLeaf aabb = Leaf <$> pure aabb <*> newIORef 0 <*> VM.new (fromIntegral maxLeafPoints)

aabbForQT :: QT -> IO AABB
aabbForQT qt = do
  node <- readIORef qt
  case node of
    Node aabb _ _ _ _ -> pure aabb
    Leaf aabb _ _ -> pure aabb

-- TODO: remove errors
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
          VM.write points (fromIntegral cnt) p
          writeIORef cntRef (cnt + 1)
        else do
          node' <- Node
            <$> pure aabb
            <*> (emptyLeaf q1 >>= newIORef)
            <*> (emptyLeaf q2 >>= newIORef)
            <*> (emptyLeaf q3 >>= newIORef)
            <*> (emptyLeaf q4 >>= newIORef)
          writeIORef qt node'

          fpoints <- V.unsafeFreeze points
          V.forM_ fpoints $ \p -> insert p qt
          insert p qt
      where
        (q1, q2, q3, q4) = splitAABB aabb

delete :: Point -> QT -> IO ()
delete = go Nothing
  where
    go parent p qt = do
      node <- readIORef qt
      case node of
        Leaf _ _ _ -> undefined
        Node _ _ _ _ _ -> undefined

remove :: VM.Unbox a => VM.IOVector a -> Int -> IO ()
remove v i = VM.move to from
  where
    to   = VM.slice i (l - i - 1) v
    from = VM.slice (i + 1) (l - i - 1) v

    l    = VM.length v

find :: VM.Unbox a => Eq a => VM.IOVector a -> a -> IO (Maybe Int)
find v a = go 0
  where
    l = VM.length v
    go i
      | i < l = do
          b <- VM.read v i
          if a == b
            then pure (Just i)
            else go (i + 1)
      | otherwise = pure Nothing

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
  qt <- empty (AABB (Point 0 0) (Point 1000000 1000000))

  -- rps <- flip traverse [0..10000000] $ \_ -> do
  --   x <- randomRIO (0, 999999)
  --   y <- randomRIO (0, 999999)

  --   pure (Point x y)

  -- putStrLn "Building QT..."
  -- sequence_ $ flip map rps $ \p -> insert p qt

  putStrLn "Building QT..."
  replicateM 10000000 $ do
    x <- randomRIO (0, 999999)
    y <- randomRIO (0, 999999)

    insert (Point x y) qt

  -- putStrLn "Saving QT..."
  -- fqt <- freeze qt
  -- BL.writeFile "tree.bin" $ B.encodeLazy fqt

  putStrLn "Query QT..."
  _ <- flip traverse [0..1000000] $ \i -> do
    x <- randomRIO (0, 900000)
    y <- randomRIO (0, 900000)
    let aabb = AABB (Point x y) (Point (x + 2000) (y + 2000))

    ps1 <- L.sort <$> query aabb qt

    if (i `mod` 100000) == 0
      then print $ (show i) <> ", " <> (show $ length ps1)
      else pure ()

  -- let aabb = AABB (Point 100 100) (Point 1000 1000)

  -- ps1 <- L.sort <$> query aabb qt
  -- ps2 <- fmap L.sort $ pure $ filter (insideAABB aabb) rps

  -- print ps1
  -- print ps2

  -- print "Done"
  -- print (ps1 == ps2)

  pure ()
  
