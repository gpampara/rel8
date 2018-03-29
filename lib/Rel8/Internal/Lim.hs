{-# language PolyKinds #-}
{-# language RankNTypes #-}

module Rel8.Internal.Lim ( Lim(..), Limit, limit, runLimit ) where

newtype Lim f =
  Lim { runLimit :: forall a. f a }

class Limit f where
  limit :: f a -> Lim f
