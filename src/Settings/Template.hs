{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Settings.Template (atcoderTemplate) where

import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (LazyText, Text)
import qualified Data.Text                      as T
import qualified Settings
import           Text.Shakespeare.Text          (stext)

dependencies :: Settings.Config -> Text
dependencies config =
  T.intercalate "\n" (map (dependency . convert) (Settings.dependencies config))
  where
    dependency :: LazyText -> Text
    dependency depend =
      convert [stext|- #{depend}|]

sourceTemplate :: Settings.Config -> Text -> Text
sourceTemplate config task = convert [stext|{-# START_FILE #{task}/Main.hs #-}
#{template}
|]
  where
    template = Settings.template config

sourceTemplates :: Settings.Config -> Text
sourceTemplates config =
   T.concat (map (sourceTemplate config) ["a", "b", "c", "d", "e", "f"])

atcoderTemplate :: Settings.Config -> Text
atcoderTemplate config =
  convert
    [stext|#{packageYaml config}
#{sourceTemplates config}
#{stackYaml}
|]

packageYaml :: Settings.Config -> Text
packageYaml config = convert
  [stext|{-# START_FILE package.yaml #-}
name:                {{name}}
version:             0.1.0.0

dependencies:
#{dependencies config}

executables:
#{executables config}
|]

executables :: Settings.Config -> Text
executables config =
  T.concat (map executable ["a", "b", "c", "d", "e", "f"])

executable :: Text -> Text
executable task = convert [stext|  #{task}:
    main:                Main.hs
    source-dirs:         #{task}
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
|]

stackYaml :: Text
stackYaml = convert [stext|{-# START_FILE stack.yaml #-}
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
