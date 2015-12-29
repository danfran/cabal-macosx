# cabal-macosx: Cabal hooks for Mac OSX

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

## Requirements

`cabal-macosx` uses Xcode's Carbon Tools to prepare an app bundle.

In order to run successfully the process, please be sure that Xcode is installed properly on your machine.

Also, run from command line:

```
xcode-select --install
```

*Note*

`cabal-macosx` runs the Carbon tool `Rez` in this way:

```
/path/to/tools/Rez Carbon.r -o dist/build/SomeProject.app/Contents/MacOS/SomeProject
```

running the command:

```
xcode-select --install
```

prevent the necessity to specifiy manually the include paths like:

/path/to/tools/Rez -F /developer/path/SDKs/MacOSX10.11.sdk/System/Library/Frameworks/ Carbon.r -o dist/build/SomeProject.app/Contents/MacOS/SomeProject

which is not managed by `cabal-macosx`.

## About the project

This code was branched from http://code.haskell.org/GenI/Setup.hs
and
http://www.mail-archive.com/wxhaskell-users@lists.sourceforge.net/msg00701.html

The package is extensively documented, including internally.  If
you're interested in modifying it, you may want to runghc Setup
haddock --hyperlink-source --internal to produce full internal
documentation.

----

Andy Gimblett
2010.02.16

Daniele Francesconi
2015.12.29
