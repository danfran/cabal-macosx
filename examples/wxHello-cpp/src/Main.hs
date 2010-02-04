module Main where

import Graphics.UI.WX

main :: IO ()
main = start gui

gui :: IO ()
gui = do f <- frame [text := "Hello World!"]
         staticText f [text := "How you doing?"]
         return ()
