{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.Polysemy.HedgehogSpec
  ( hprop_example
  ) where

import           HaskellWorks.Polysemy.Prelude

import qualified Data.List                       as L
import qualified HaskellWorks.Polysemy.Data.Text as T
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.System.IO
import           System.IO                       (IOMode (..))

default (String)

hprop_example :: Property
hprop_example = propertyOnce $ do
  let projectRoot = "."

  contents <- T.readFile (projectRoot </> "LICENSE")
    & trapFail @IOException

  line1 <- T.lines contents
    & L.dropWhile T.null
    & requireHead
    & jotShowM

  let text = T.strip line1

  text === "Apache License"
