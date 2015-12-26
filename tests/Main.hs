module Main (main) where

import Test.Framework (defaultMain)
import Distribution.MacOSX.Internal.Tests (macosxInternalTests)

main :: IO ()
main = defaultMain
  [ macosxInternalTests
  ]
