{-# LANGUAGE OverloadedStrings #-}

module Graphics
    ( initWindow
    , initRenderer
    , initTexture
    , destroyWindow
    , drawWorld
    ) where

import           Conway
import           Foreign
import           Foreign.C.Types
import           Linear
import           SDL             (($=))
import qualified SDL
import           Control.Monad


screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 640)

initWindow :: IO SDL.Window
initWindow = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleBest
  window <- SDL.createWindow "GoL" windowSize
  SDL.showWindow window
  return window
  where
    windowSize = SDL.defaultWindow
      { SDL.windowInitialSize = V2 screenWidth screenHeight
      , SDL.windowOpenGL = Just SDL.defaultOpenGL
      }

initRenderer :: SDL.Window -> IO SDL.Renderer
initRenderer window = do
  renderer <- SDL.createRenderer window (-1) renderConf
  SDL.rendererLogicalSize renderer $= Just (V2 (screenWidth `div` 2) (screenHeight `div` 2))
  return renderer
  where
    renderConf = SDL.RendererConfig
      { SDL.rendererType = SDL.AcceleratedVSyncRenderer
      , SDL.rendererTargetTexture = False
      }

initTexture :: SDL.Renderer -> IO SDL.Texture
initTexture renderer =
  SDL.createTexture renderer SDL.RGB888 SDL.TextureAccessStreaming $
    V2 (screenWidth `div` 2) (screenHeight `div` 2)

drawWorld :: Ptr Word8 -> CInt -> Grid -> IO ()
drawWorld pixels _ world = do
  let worldList = zip [0..] $ gridToList world :: [(Int, Int)]
  forM_ worldList $ \(i, cell) ->
    writePixel cell pixels (i * 4)

writePixel :: Int -> Ptr Word8 -> Int -> IO ()
writePixel cell pixels offset =
  forM_ [0..2] $ \x -> pokeElemOff pixels (offset + x) $ color cell
  where
    color 0 = 0
    color 1 = 255
    color _ = 0

destroyWindow :: SDL.Window -> SDL.Renderer -> IO ()
destroyWindow window renderer = do
  SDL.destroyWindow window
  SDL.destroyRenderer renderer
  SDL.quit
