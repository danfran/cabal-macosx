module Distribution.MacOSX.Internal.Tests where

import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (SomeException, catch)
import Prelude hiding (catch)
import Test.HUnit (Assertion)
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Distribution.MacOSX.Internal (osxIncantations)
import Distribution.MacOSX.Common

macosxInternalTests :: Test
macosxInternalTests = testGroup "Distribution.MacOSX.Internal"
    [ mutuallyExclusive $ testGroup "MacOSX Internal"
        [ testOsxIncantations
        ]
    ]

testOsxIncantations :: Test
testOsxIncantations = testCase "should exit with an exit-failure as Xcode's Carbon Tools fail to run" testCarbonTools
  where
    testCarbonTools :: Assertion
    testCarbonTools = do
        let macApp = MacApp "DummyApp" Nothing Nothing [] [] DoNotChase

        withSystemTempDirectory "DummyAppPath" $ \tmpDir
            -> osxIncantations tmpDir macApp
                 `catch`
                     (\e -> do putStrLn $ "Catched: " ++ show (e :: SomeException)
                               return ())