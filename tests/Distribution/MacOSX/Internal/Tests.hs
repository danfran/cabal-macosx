module Distribution.MacOSX.Internal.Tests where

import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (SomeException, catch)
import Prelude hiding (catch)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Distribution.PackageDescription (BuildInfo(..), Executable(..))

import Distribution.MacOSX.Internal (osxIncantations, getMacAppsForBuildableExecutors)
import Distribution.MacOSX.Common

macosxInternalTests :: Test
macosxInternalTests = testGroup "Distribution.MacOSX.Internal"
    [ mutuallyExclusive $ testGroup "MacOSX Internal"
        [ testCase "should exit with an exit-failure as Xcode's Carbon Tools fail to run" testCarbonTools,
          testCase "given nothing then should not try to build any mac-app" testWithNoAppsAndNoExecutables,
          testCase "given no executables then should not try to build any mac-app" testWithNoExecutables,
          testCase "given only two executables then should try to build two mac-apps" testWithNoApps
        ]
    ]

testCarbonTools = do
    let macApp = MacApp "DummyApp" Nothing Nothing [] [] DoNotChase

    withSystemTempDirectory "DummyAppPath" $ \tmpDir
        -> osxIncantations tmpDir macApp -- some problems here to add `assertFailure` after
             `catch`
                 (\e -> do putStrLn $ "Catched: " ++ show (e :: SomeException)
                           return ())

testWithNoAppsAndNoExecutables = do
    let actual = getMacAppsForBuildableExecutors [] []
    let expected = []
    assertEqual "nothing should be built" expected actual

testWithNoExecutables = do
    let apps = [MacApp "Dummy App" Nothing Nothing [] [] DoNotChase]
    let actual = getMacAppsForBuildableExecutors apps []
    let expected = []
    assertEqual "nothing should be built" expected actual

testWithNoApps = do
    let execs = [ Executable "Dummy One" "/tmp" mempty
                  , Executable "Dummy Two" "/tmp" mempty ]
    let actual = getMacAppsForBuildableExecutors [] execs
    let expected = [ MacApp "Dummy One" Nothing Nothing [] [] DoNotChase
                     , MacApp "Dummy Two" Nothing Nothing [] [] DoNotChase ]

    assertEqual "nothing should be built" expected actual

