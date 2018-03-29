{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}

module Rel8.Internal.Aggregation ( Aggregation(..), aggregate, aggregator ) where

import Control.Monad.Trans.Reader ( ReaderT(..) )
import Data.Coerce ( coerce )

import qualified Control.Lens as Lens
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O

import Rel8.Internal.Expr ( Expr, _Column )
import Rel8.Internal.Query ( Query, _Query )
import Rel8.Internal.Select ( Select(..) )
import Rel8.Internal.TaggedExpr ( TaggedExpr )

import qualified Rel8.Internal.AggregationOptions as AggregationOptions


newtype Aggregation s ( m :: * -> * ) a =
  Aggregation ( ReaderT AggregationOptions.AggregationOptions Select a )
  deriving ( Functor, Applicative, Monad )


instance Query ( TaggedExpr s ) ( Aggregation s m ) where


-- | Join a query by aggregating its results.
aggregate
  :: ( Query expr m )
  => ( forall s. Aggregation s m a )
  -> m a
aggregate q =
  Lens.view _Query ( coerce q AggregationOptions.defaultAggregationOptions )


aggregator :: ( Expr expr ) => O.AggrOp -> TaggedExpr s a -> Aggregation s m (expr a)
aggregator op expr =
  Aggregation
    ( ReaderT
        ( \ops ->
            pure
              ( Lens.view
                  ( Lens.from _Column )
                  ( O.Column
                      ( O.AggrExpr
                        ( AggregationOptions.distinct ops )
                        op
                        ( O.unColumn ( Lens.view _Column expr ) )
                        ( AggregationOptions.order ops )
                      )
                  )
              )
        )
    )
