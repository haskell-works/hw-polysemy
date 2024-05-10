{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.Polysemy.HedgehogSpec
  ( hprop_example
  ) where

import           HaskellWorks.Polysemy.Prelude

import qualified Data.List                             as L
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.Hedgehog.Assert
import qualified HaskellWorks.Polysemy.Text            as T

default (String)

hprop_example :: Property
hprop_example = propertyOnce $ do
  let projectRoot = "."

  contents <- T.readFile (projectRoot </> "LICENSE")
    & catchFail @IOException

  line1 <- T.lines contents
    & L.dropWhile T.null
    & requireHead
    & jotShowM

  let text = T.strip line1

  text === "Apache License"
