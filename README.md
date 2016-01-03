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

## Troubleshooting

### ~ Rez is exiting with error

`cabal-macosx` uses Xcode's Carbon Tools to prepare an app bundle.

In order to run successfully the process, please be sure that Xcode is installed properly on your machine.

Also, note that `cabal-macosx` runs the Carbon tool `Rez` in this way:

```
/path/to/tools/Rez Carbon.r -o dist/build/SomeProject.app/Contents/MacOS/SomeProject
```

So after the Xcode's installation, please run from command line:

```
xcode-select --install
```

to prevent the necessity of specifying the include paths like this:

```
/path/to/tools/Rez -F /developer/path/SDKs/MacOSX10.11.sdk/System/Library/Frameworks/ Carbon.r -o dist/build/SomeProject.app/Contents/MacOS/SomeProject
```

for the Carbon tools. `cabal-macosx` assumes that the paths are properly configured otherwise it will stop the building process and returning an error similar to:

```
failed to find Carbon/Carbon.r
### /Applications/XCode.app/Contents/Developer/Tools/Rez - SysError 0 during open of "Carbon.r".
Fatal Error!
### /Applications/XCode.app/Contents/Developer/Tools/Rez - Fatal Error, can't recover.
Carbon.r: ### /Applications/XCode.app/Contents/Developer/Tools/Rez - Since errors occurred, dist/build/NetMonitor.app/Contents/MacOS/NetMonitor's resource fork was not written.
```

(and eventually deleting the built local binary too).

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
