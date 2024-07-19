{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use let" -}

module HaskellWorks.Polysemy.Log
  ( interpretDataLogNoop,
    interpretDataLogLocalNoop,
  ) where

import           HaskellWorks.Prelude
import           Polysemy
import           Polysemy.Internal.Tactics   (liftT)
import           Polysemy.Log
import qualified Polysemy.Log.Effect.DataLog as Log

interpretDataLogNoop :: forall a r. ()
  => InterpreterFor (DataLog a) r
interpretDataLogNoop =
  interpretDataLogLocalNoop id

interpretDataLogLocalNoop :: forall a r. ()
  => (a -> a)
  -> InterpreterFor (DataLog a) r
interpretDataLogLocalNoop _ =
  interpretH \case
    Log.DataLog _ ->
      liftT (pure ())
    Log.Local f ma ->
      raise . interpretDataLogLocalNoop f =<< runT ma
{-# inline interpretDataLogLocalNoop #-}
