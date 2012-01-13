module Main where

import Graphics.Gloss

import Level

main :: IO ()
main = do
    pic <- loadBMP "./resources/test.bmp"
    display (InWindow "blah" (640, 480) (10, 10)) black $ drawLevel (dummyLevel pic)