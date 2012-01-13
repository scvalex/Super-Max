module Main where

import Graphics.Gloss

import Level

main :: IO ()
main = do
    pic <- loadBMP "./resources/test.bmp"
    display (InWindow "SuperMax" (1024, 768) (10, 10)) black $ drawLevel (dummyLevel pic)