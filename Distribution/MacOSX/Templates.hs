{-# LANGUAGE OverloadedStrings #-}

-- | Templates for the bundle.

module Distribution.MacOSX.Templates (
  plistTemplate
) where

import Data.Text ( Text )

-- | Default plist template, based on that in macosx-app from wx (but
-- with version stuff removed).
plistTemplate :: Text
plistTemplate = "\
    \<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<!DOCTYPE plist SYSTEM \"file://localhost/System/Library/DTDs/PropertyList.dtd\">\n\
    \<plist version=\"0.9\">\n\
    \<dict>\n\
            \<key>CFBundleInfoDictionaryVersion</key>\n\
            \<string>6.0</string>\n\
            \<key>CFBundleIdentifier</key>\n\
            \<string>org.haskell.$program</string>\n\
            \<key>CFBundleDevelopmentRegion</key>\n\
            \<string>English</string>\n\
            \<key>CFBundleExecutable</key>\n\
            \<string>$program</string>\n\
            \<key>CFBundleIconFile</key>\n\
            \<string>$iconPath</string>\n\
            \<key>CFBundleName</key>\n\
            \<string>$program</string>\n\
            \<key>CFBundlePackageType</key>\n\
            \<string>APPL</string>\n\
            \<key>CFBundleSignature</key>\n\
            \<string>????</string>\n\
            \<key>CFBundleVersion</key>\n\
            \<string>1.0</string>\n\
            \<key>CFBundleShortVersionString</key>\n\
            \<string>1.0</string>\n\
            \<key>CFBundleGetInfoString</key>\n\
            \<string>$program, bundled by cabal-macosx</string>\n\
            \<key>LSRequiresCarbon</key>\n\
            \<true/>\n\
            \<key>CSResourcesFileMapped</key>\n\
            \<true/>\n\
    \</dict>\n\
    \</plist>"