{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}

module Rel8 ( I.Expr, I.Query, I.Aggregation, I.aggregate, I.queryTable, sum_, lit, I.where_, I.Table(..), I.BaseTable(..), I.Expr ) where

import Data.Functor.Identity ( Identity )
import GHC.Generics ( Generic )

import qualified Control.Lens as Lens
import qualified Opaleye as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O

import qualified Rel8.Internal as I

import Data.Text


sum_ :: I.Query expr m => I.TaggedExpr s a -> I.Aggregation s m ( expr a )
sum_ =
  I.aggregator O.AggrSum


lit :: ( I.Expr expr, I.Transport ( O.Column a ) ( Identity hs ) ) => hs -> expr a
lit =
  Lens.view ( Lens.from I._Column ) . I.litTable . pure


select :: I.Transport expr haskell => I.Select expr -> IO [haskell]
select =
  undefined


data TestTable f = TestTable { columnA :: I.C f Text }
  deriving ( Generic )

instance I.Table ( TestTable ( I.TaggedExpr s ) ) 
instance I.Table ( TestTable ( I.ColumnSchema s ) ) 
instance Transportable expr haskell => I.Transport ( TestTable expr ) ( TestTable haskell )
instance BaseTableIsh s schema expr => I.BaseTable ( TestTable schema ) ( TestTable expr ) where
  tableName = "foo"

type BaseTableIsh s schema expr =
  ( schema ~ I.ColumnSchema s, expr ~ I.TaggedExpr s )
    
type Transportable expr haskell =
  ( expr ~ I.TaggedExpr (), haskell ~ I.AsHaskell )




data Join f = Join { joinA :: TestTable f, joinB :: TestTable f }
  deriving ( Generic )

instance I.Table ( Join ( I.TaggedExpr s ) ) where 

instance Transportable expr haskell => I.Transport ( Join expr ) ( Join haskell )
