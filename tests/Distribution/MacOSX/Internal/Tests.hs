module Distribution.MacOSX.Internal.Tests where

import Test.HUnit (Assertion, assertEqual)
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Distribution.MacOSX.Internal (osxIncantations)
import Distribution.MacOSX.Common
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception

macosxInternalTests :: Test
macosxInternalTests = testGroup "Distribution.MacOSX.Internal"
    [ mutuallyExclusive $ testGroup "MacOSX Internal"
        [ testOsxIncantations
        ]
    ]

testOsxIncantations :: Test
testOsxIncantations = testCase "should not throw any exception even if Xcode's Carbon Tools fail to run" testCarbonTools
  where
    testCarbonTools :: Assertion
    testCarbonTools = do
        let macApp = MacApp "DummyApp" Nothing Nothing [] [] DoNotChase

        withSystemTempDirectory "DummyAppPath" $ \tmpDir
            -> osxIncantations tmpDir macApp

        return ()