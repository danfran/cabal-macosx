# cabal-macosx

[![feed](https://budueba.com/hackage/cabal-macosx)](https://hackage.haskell.org/package/cabal-macosx)
[![Build Status](https://travis-ci.org/danfran/cabal-macosx.svg?branch=master)](https://travis-ci.org/danfran/cabal-macosx)

## Cabal hooks for Mac OSX

This package provides a Cabal post-build hook for building application
bundles for GUIs on Mac OSX.  It also includes a standalone command
line utility (macosx-app) for wrapping one-off GUIs that do not use Cabal.

Under Mac OSX, graphical applications require certain infrastructure
in order to run properly: you can't just build an executable and run
it (as on Linux, say), but must instead wrap it up in an application
bundle, which is a directory having certain structure and marked as an
app using a particular tool.  This package provides Cabal post-build
hook infrastructure for creating such bundles automatically,
optionally including icons, other resources, and even local copies of
shared libraries (for building apps which may be distributed
standalone).

The command line version only provides basic functionality at the
moment.  It takes a single argument, the path to an executable,
and produces an application bundle in the current working directory.

## Usage

To add `cabal-macosx` to your project with Cabal 1.24 or higher, add a
`custom-setup` section with `cabal-macosx` as a dependency. For example,

    custom-setup
      setup-depends: base >= 4.9 && < 5
                   , Cabal >= 1.24 && < 1.25
                   , cabal-macosx >= 0.2 && < 0.3

And modify your `Setup.hs` to call the `cabal-macosx` post-build hook:

    import Distribution.MacOSX
    import Distribution.Simple

    main :: IO ()
    main = defaultMainWithHooks $ simpleUserHooks
      { postBuild = appBundleBuildHook guiApps
      }

    guiApps :: [MacApp]
    guiApps =
      [ MacApp
        { appName = "MyApp"
        , appIcon = Just "icon.icns"
        , appPlist = Nothing -- Build a default Info.plist for the icon.
        , resources = [] -- No other resources.
        , otherBins = [] -- No other binaries.
        , appDeps = DoNotChase -- Try changing to ChaseWithDefaults
        }
      ]

See [the Hackage documentation] for details on the build hook API.

[cabal-macosx-hackage]: https://hackage.haskell.org/package/cabal-macosx

### Stack

If you are using Stack, add the current version of `cabal-macosx` to the
`extra-deps` section of your `stack.yaml`. For example,

    extra-deps:
    - cabal-macosx-0.2.3.5

As of Stack 1.3.2, Cabal's `custom-setup` section is not yet supported, so
you will also need to add `cabal-macosx` to your executable's main
dependency list in your `.cabal` file,

    executable MyApp
      build-depends: base >= 4.9 && < 5
                   , Cabal >= 1.24 && < 1.25
                   , cabal-macosx >= 0.2 && < 0.3

and add an [`explicit-setup-deps`][stack-explicit-setup-deps] section to
your `stack.yaml`:

    explicit-setup-deps:
      "*": true

(`custom-setup` [is implemented in Stack's latest
master][stack-custom-setup-pr] and will hopefully be in a future release.)

[stack-custom-setup-pr]: https://github.com/commercialhaskell/stack/pull/2866
[stack-explicit-setup-deps]: https://docs.haskellstack.org/en/stable/yaml_configuration/#explicit-setup-deps

### Resources

To include resource files in an app bundle, add their paths relative to the
project root to the `resources` field of your `MacApp`. Access these
resources from your application by using the `executable-path` package, for
example:

    readLicense = do
      exePath <- getExecutablePath
      let contentsPath = takeDirectory (takeDirectory exePath)
      readFile (contentsPath </> "Resources/LICENSE")

Do not access resources in your application by relative path without
changing your working directory first, as the working directory that GUI
applications are started in is not specified.

## Troubleshooting

### ~ Install fails inside a sandbox

If you get an error similar to this:

```
exited with an error:

dist/dist-sandbox-8121acba/setup/setup.hs:11:19:
Couldn't match type ‘LocalBuildInfo’
with ‘Cabal-1.22.5.0:Distribution.Simple.LocalBuildInfo.LocalBuildInfo’
NB: ‘LocalBuildInfo’
is defined in ‘Distribution.Simple.LocalBuildInfo’
in package ‘Cabal-1.22.6.0’
‘Cabal-1.22.5.0:Distribution.Simple.LocalBuildInfo.LocalBuildInfo’
is defined in ‘Distribution.Simple.LocalBuildInfo’
in package ‘Cabal-1.22.5.0’
Expected type: Args
-> Distribution.Simple.Setup.BuildFlags
-> Distribution.PackageDescription.PackageDescription
-> LocalBuildInfo
-> IO ()
Actual type: [String]
-> Cabal-1.22.5.0:Distribution.Simple.Setup.BuildFlags
-> Cabal-1.22.5.0:Distribution.PackageDescription.PackageDescription
-> Cabal-1.22.5.0:Distribution.Simple.LocalBuildInfo.LocalBuildInfo
-> IO ()
In the ‘postBuild’ field of a record
In the second argument of ‘($)’, namely
‘simpleUserHooks {postBuild = myPostBuild}’
)
```

You might need to update your version of Cabal with something like this:

```
~$ cabal --version
cabal-install version 1.22.6.0
using version 1.22.4.0 of the Cabal library
~$ cabal update
...
~$ cabal install cabal cabal-install
...
~$ cabal --version
cabal-install version 1.22.7.0
using version 1.22.6.0 of the Cabal library
```

More: https://www.haskell.org/cabal/download.html

### ~ App fails to launch with the `open` command or from Finder

If you get an error similar to this:

    LSOpenURLsWithRole() failed with error -10810 for the file...

This may be because your program is exiting or crashing immediately during
launch.

If your application runs normally when run directly from the terminal, it
may be that your application is depending on being launched from a
particular working directory or with a certain set of environment
variables. For example, loading resource files by relative path or running
programs only on your shell's `PATH`. Applications started in a GUI context
(such as from `open` or the Finder) run in an unspecified working directory
and a standard set of environment variables that are not derived from your
shell.

To view your application's standard output and standard error when run from
a GUI context, use the system Console app (found in /Applications/Utilities).

## About the project

This code was branched from http://code.haskell.org/GenI/Setup.hs
and
http://www.mail-archive.com/wxhaskell-users@lists.sourceforge.net/msg00701.html

The package is extensively documented, including internally.  If
you're interested in modifying it, you may want to

```
runghc Setup haddock --hyperlink-source --internal
```
to produce full internal documentation.

----

Daniele Francesconi - 2015.12.29

Andy Gimblett - 2010.02.16
