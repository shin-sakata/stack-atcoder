{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings.Template (atcoderTemplate) where

import Data.Convertible.Utf8 (convert)
import Data.Convertible.Utf8.Internal (LazyText, Text)
import qualified Settings
import Text.Shakespeare.Text (stext)
import Data.Text (intercalate)

dependencies :: Settings.Config -> Text
dependencies config =
  intercalate "\n" (map (dependency . convert) (Settings.dependencies config))
  where
    dependency :: LazyText -> Text
    dependency depend =
      convert [stext|- #{depend}|]

sourceTemplate :: Settings.Config -> Text -> Text
sourceTemplate config task = convert [stext|
{-# START_FILE #{task}/Main.hs #-}
#{template}
|]
  where
    template = Settings.template config

sourceTemplates :: Settings.Config -> Text
sourceTemplates config =
  intercalate "\n" (map (sourceTemplate config) ["a", "b", "c", "d", "e", "f"])

atcoderTemplate :: Settings.Config -> Text
atcoderTemplate config =
  convert
    [stext|{-# START_FILE package.yaml #-}
name:                {{name}}
version:             0.1.0.0

dependencies:
#{dependencies_}

#{rest}

#{sourceTemplates_}
|]
  where
    dependencies_ = dependencies config
    sourceTemplates_ = sourceTemplates config

rest = [stext|
executables:
  a:
    main:                Main.hs
    source-dirs:         a
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
  b:
    main:                Main.hs
    source-dirs:         b
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
  c:
    main:                Main.hs
    source-dirs:         c
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
  d:
    main:                Main.hs
    source-dirs:         d
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
  e:
    main:                Main.hs
    source-dirs:         e
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
  f:
    main:                Main.hs
    source-dirs:         f
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N

{-# START_FILE stack.yaml #-}
resolver: lts-15.7
packages:
- .
extra-deps:
- attoparsec-0.13.2.3
- extra-1.7.1
- lens-4.19.1
- massiv-0.5.1.0
- repa-3.4.1.4
|]