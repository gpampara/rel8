{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Internal.C ( C, AsHaskell ) where

import Data.Tagged ( Tagged )

import qualified Opaleye as O

import Rel8.Internal.ColumnSchema
import Rel8.Internal.TaggedExpr


data EXPR ( m :: * -> * )

data SCHEMA ( m :: * -> * )

data AsHaskell a

type family C f {- columnName hasDefault -} columnType :: * where
  C ( TaggedExpr s ) {- _name _def -} t = TaggedExpr s t
  C ( ColumnSchema s ) {- _name _def -} t = ColumnSchema s ( Tagged "hello" t )
  C AsHaskell t = t
