{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Distribution.MacOSX.Internal.Tests where

import Prelude hiding (catch)
import Test.HUnit (Assertion, assertEqual)
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.ExecutableScope
#endif
import Distribution.PackageDescription (BuildInfo(..), Executable(..), emptyBuildInfo)

import Distribution.MacOSX.Internal (getMacAppsForBuildableExecutors)
import Distribution.MacOSX.Common

macosxInternalTests :: Test
macosxInternalTests = testGroup "Distribution.MacOSX.Internal"
    [ mutuallyExclusive $ testGroup "MacOSX Internal"
        [ -- I should consider to use QuickCheck maybe... :)
          testCase "given nothing then should not try to build any mac-app" testBuildMacApp_noInput,
          testCase "given no executables then should not try to build any mac-app" testBuildMacApp_noExecutables,
          testCase "given only two executables then should try to build two mac-apps" testBuildMacApp_twoBuildableExecutables,
          testCase "given only two executables and one not executable then should try to build one mac-app" testBuildMacApp_twoExcetuablesOneBuildableAndOneNot,
          testCase "given two executables and one not executable and two apps then should try to build one mac-app" testBuildMacApp_twoAppsAndTwoExecutablesOneBuildableOneNot
        ]
    ]

testBuildMacApp_noInput :: Assertion
testBuildMacApp_noInput = do
    let actual = getMacAppsForBuildableExecutors [] []
    let expected = []
    assertEqual "nothing should be built" expected actual

testBuildMacApp_noExecutables :: Assertion
testBuildMacApp_noExecutables = do
    let apps = [MacApp "Dummy App" Nothing Nothing [] [] DoNotChase]
    let actual = getMacAppsForBuildableExecutors apps []
    let expected = []
    assertEqual "nothing should be built" expected actual

testBuildMacApp_twoBuildableExecutables :: Assertion
testBuildMacApp_twoBuildableExecutables = do
#if MIN_VERSION_Cabal(2,0,0)
    let execs = [ Executable "Dummy One" "/tmp" ExecutableScopeUnknown emptyBuildInfo
                  , Executable "Dummy Two" "/tmp" ExecutableScopeUnknown  emptyBuildInfo ]
#else
    let execs = [ Executable "Dummy One" "/tmp" emptyBuildInfo
                  , Executable "Dummy Two" "/tmp" emptyBuildInfo ]
#endif
    let actual = getMacAppsForBuildableExecutors [] execs
    let expected = [ MacApp "Dummy One" Nothing Nothing [] [] DoNotChase
                     , MacApp "Dummy Two" Nothing Nothing [] [] DoNotChase ]
    assertEqual "two mac-apps should be built" expected actual

testBuildMacApp_twoExcetuablesOneBuildableAndOneNot :: Assertion
testBuildMacApp_twoExcetuablesOneBuildableAndOneNot = do
#if MIN_VERSION_Cabal(2,0,0)
    let execs = [ Executable "Dummy One" "/tmp" ExecutableScopeUnknown (emptyBuildInfo { buildable = False })
                  , Executable "Dummy Two" "/tmp" ExecutableScopeUnknown  emptyBuildInfo ]
#else
    let execs = [ Executable "Dummy One" "/tmp" (emptyBuildInfo { buildable = False })
                  , Executable "Dummy Two" "/tmp" emptyBuildInfo ]
#endif
    let actual = getMacAppsForBuildableExecutors [] execs
    let expected = [ MacApp "Dummy Two" Nothing Nothing [] [] DoNotChase ]
    assertEqual "two mac-apps should be built" expected actual

testBuildMacApp_twoAppsAndTwoExecutablesOneBuildableOneNot :: Assertion
testBuildMacApp_twoAppsAndTwoExecutablesOneBuildableOneNot = do
#if MIN_VERSION_Cabal(2,0,0)
    let execs = [ Executable "Dummy One" "/tmp" ExecutableScopeUnknown (emptyBuildInfo { buildable = False })
                  , Executable "Dummy Two" "/tmp" ExecutableScopeUnknown  emptyBuildInfo ]
#else
    let execs = [ Executable "Dummy One" "/tmp" (emptyBuildInfo { buildable = False })
                  , Executable "Dummy Two" "/tmp" emptyBuildInfo ]
#endif
    let apps = [ MacApp "Dummy One" Nothing Nothing [] [] DoNotChase
                 , MacApp "Dummy Two" Nothing Nothing [] [] DoNotChase ]
    let actual = getMacAppsForBuildableExecutors apps execs
    let expected = [ MacApp "Dummy Two" Nothing Nothing [] [] DoNotChase ]
    assertEqual "one mac-app should be built" expected actual