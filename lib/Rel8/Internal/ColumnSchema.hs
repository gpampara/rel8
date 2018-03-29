{-# language DataKinds #-}
{-# language KindSignatures #-}

module Rel8.Internal.ColumnSchema ( ColumnSchema(..) ) where

import GHC.TypeLits ( Symbol )

import Rel8.Internal.Lim


newtype ColumnSchema s ( a :: * ) = ColumnSchema String


instance Limit ( ColumnSchema s ) where
  limit ( ColumnSchema a ) =
    Lim ( ColumnSchema a )
