module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Conway
import           Foreign
import           Graphics
import           Linear             hiding (identity)
import           Linear.Affine
import           SDL                (($=))
import qualified SDL


main :: IO ()
main = do
  world <- randomWorld 320
  window <- initWindow
  renderer <- initRenderer window
  gameOfLife renderer world
  destroyWindow window renderer

gameOfLife :: SDL.Renderer -> Grid -> IO ()
gameOfLife renderer world = do
  texture <- initTexture renderer
  let
    loop w = do
      events <- SDL.pollEvents
      let quit = elem SDL.QuitEvent $ SDL.eventPayload <$> events

      (pixels, pitch) <- SDL.lockTexture texture Nothing
      let data8Ptr = castPtr pixels

      drawWorld data8Ptr pitch w

      SDL.unlockTexture texture

      SDL.rendererDrawColor renderer $= V4 0 0 0 maxBound
      SDL.clear renderer
      SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (P (V2 0 0)) (V2 640 640))
      SDL.present renderer

      threadDelay 100000
      unless quit $ loop =<< tick w
  loop world
