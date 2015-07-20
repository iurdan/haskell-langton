module Main where 

import Graphics.UI.SDL as SDL
import Control.Monad (forM_)
import Data.IORef
import System.Environment
import System.Exit (exitSuccess)
import Data.Array.Repa (Z(..),(:.)(..),(!))
import qualified Data.Array.Repa as R
import Ant

draw :: Int -> Grid -> Surface -> IO ()
draw s grid surface = do
  let (Z :. w :. h) = R.extent grid
  forM_ [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)]] $ \(x,y) -> do
    let rect = Rect (x*s) (y*s) s s
        drawCell = fillRect surface (Just rect) . Pixel
    drawCell $ case grid ! (Z :. x :. y) of
      1 -> 0xFFFFFFFF
      3 -> 0x00000000

mainLoop :: Int -> Surface -> Ant -> IORef Int -> IORef Grid -> IO ()
mainLoop s surface (a,b) dirR gridR = do
  dir  <- readIORef dirR
  grid <- readIORef gridR
  draw s grid surface
  SDL.flip surface

  let (Z :. w :. h) = R.extent grid
      torus (x,y) = (mod x w, mod y h)

  -- Turn
  writeIORef dirR $ turn dir (grid ! (Z :. a :. b))
  -- Change colour of the cell
  writeIORef gridR =<< tick (a,b) grid
  e <- SDL.pollEvent
  case e of  
    KeyDown (Keysym SDLK_RETURN _ _) -> do
      toggleFullscreen surface
      exitSuccess
    _ -> do
      -- Move forward and advance to the next iteration
      newDir <- readIORef dirR
      mainLoop s surface (torus (forward (a,b) newDir)) dirR gridR
    
main :: IO ()
main = do
  args <- getArgs
  SDL.init [SDL.InitEverything]
  info <- getVideoInfo
  -- Video fits the screen size
  let w = videoInfoWidth info
      h = videoInfoHeight info
      s = if length args > 0 
            then read (args !! 0)
            else 4
  zeros <- zeroGrid (w `div` s) (h `div` s)
  gridR <- newIORef zeros
  dirR  <- newIORef 0
  SDL.setCaption "Langton's Ant" "Ant"
  setVideoMode w h 32 [Fullscreen]
  surface <- getVideoSurface
  grid <- readIORef gridR
  draw s grid surface
  SDL.flip surface
  mainLoop s surface (w `div` (2*s), h `div` (2*s)) dirR gridR
