{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeOperators #-}

module Conway
    ( tick
    , block
    , toad
    , randomWorld
    , Grid
    , gridToList
    ) where

import           Data.Array.Repa              ((:.) (..), Z (..))
import qualified Data.Array.Repa              as R
import qualified Data.Array.Repa.Eval         as RE
import qualified Data.Array.Repa.Stencil      as RS
import           Data.Array.Repa.Algorithms.Randomish
import           System.Random
import           Data.Array.Repa.Stencil.Dim2 (makeStencil2, mapStencil2,
                                               stencil2)


type Grid = R.Array R.U R.DIM2 Int

-- basic shapes

-- Still lifes
block :: Grid
block = RE.fromList (Z :. 4 :. 4)
  [ 0, 0, 0, 0
  , 0, 1, 1, 0
  , 0, 1, 1, 0
  , 0, 0, 0, 0
  ]

-- Oscillators
toad :: Grid
toad = RE.fromList (Z :. 6 :. 6)
  [ 0, 0, 0, 0, 0, 0
  , 0, 0, 0, 0, 0, 0
  , 0, 0, 1, 1, 1, 0
  , 0, 1, 1, 1, 0, 0
  , 0, 0, 0, 0, 0, 0
  , 0, 0, 0, 0, 0, 0
  ]

-- Get a random Grid
randomWorld :: Int -> IO Grid
randomWorld size = do
  seed <- randomRIO (0, 65536)
  return $ randomishIntArray (Z :. size :. size) 0 1 seed

-- | stencil convolution
-- Applying a stencil to an "image" means
-- 1. Put the stencil kernel on every pixel
-- 2. Grab all neighbors and multiply with the corresponding stencil value
-- 3. Add them all and it become the new value of the pixel
--
-- In our case, if we represent live cells as 1, dead cells as 0
-- the stencil we just defined be used to count the number of neighbors
-- for each cell
sten :: RS.Stencil R.DIM2 Int
sten = [stencil2| 1 1 1
                  1 0 1
                  1 1 1 |]

transit :: Int -> Int -> Int
transit 1 2 = 1
transit 1 3 = 1
transit 1 _ = 0
transit 0 3 = 1
transit 0 _ = 0
transit _ _ = 0

tick :: Grid -> IO Grid
tick world = R.computeP $ R.zipWith transit world neighbors
  where neighbors = mapStencil2 (RS.BoundConst 0) sten world

gridToList :: Grid -> [Int]
gridToList = R.toList
