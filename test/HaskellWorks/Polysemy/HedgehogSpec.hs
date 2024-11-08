module HaskellWorks.Polysemy.HedgehogSpec
  ( hprop_example_property,
    hprop_example_test,
  ) where

import           HaskellWorks.Polysemy.Prelude

import qualified Data.List                           as L
import qualified HaskellWorks.Polysemy.Data.Text     as T
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.Hedgehog.Test

default (String)

hprop_example_property :: Property
hprop_example_property = propertyOnce $ do
  let projectRoot = "."

  contents <- T.readFile (projectRoot </> "LICENSE")
    & trapFail @IOException

  line1 <- T.lines contents
    & L.dropWhile T.null
    & requireHead
    & jotShowM

  let text = T.strip line1

  text === "Apache License"

hprop_example_test :: Property
hprop_example_test = test $ do
  let projectRoot = "."

  contents <- T.readFile (projectRoot </> "LICENSE")
    & trapFail @IOException

  line1 <- T.lines contents
    & L.dropWhile T.null
    & requireHead
    & jotShowM

  let text = T.strip line1

  text === "Apache License"
