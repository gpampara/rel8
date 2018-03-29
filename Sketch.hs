{-# language ConstraintKinds #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language NoMonomorphismRestriction #-}

module Rel8 where

import Data.Coerce ( coerce )
import Data.Functor.Rep ( Representable )
import Control.Monad.Trans.Reader ( ReaderT(..) )
import Data.Tagged ( Tagged, unTagged )
import Data.Text ( Text )
import Data.String ( IsString )

import qualified Control.Lens as Lens
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.Table as O
import qualified Opaleye.Internal.Unpackspec as O

import Data.Functor.Identity ( Identity(..) )




































select :: Transport expr haskell => Select expr -> IO [haskell]
select =
  _

type family UnLimit a :: * -> * where
  UnLimit (Lim f) = f

  -- C QueryResult _name _def t = t
  -- C SchemaInfo name hasDefault t = SchemaInfo '(name, hasDefault, t)
  -- C Insert name 'HasDefault t = Default (Expr t)
  -- C Insert name 'NoDefault t = Expr t


---

data TestTable f = TestTable { columnA :: C f Text }

-- data TestTableS ( expr :: * -> * ) = TestTableS { columnAS :: String }

instance Expr expr => Table ( TestTable ( EXPR expr ) ) where
  type Columns ( TestTable ( EXPR expr ) ) =
    Identity

  type ColumnType ( TestTable ( EXPR expr ) ) =
    Lim expr

  toColumns ( TestTable e ) =
    Identity
      ( case Lens.view _Column e of
          O.Column primExpr ->
            Lim ( Lens.view ( Lens.from _Column ) ( O.Column primExpr ) )
      )

  fromColumns ( Identity ( Lim e ) ) =
    TestTable e

instance Table ( TestTable ( SCHEMA expr ) ) where
  type Columns ( TestTable ( SCHEMA expr ) ) =
    Identity

  type ColumnType ( TestTable ( SCHEMA expr ) ) =
    Lim ColumnSchema

  toColumns ( TestTable a ) =
    Identity ( Lim ( ColumnSchema a ) )

  fromColumns ( Identity ( Lim ( ColumnSchema a ) ) ) =
    TestTable a

instance BaseTableRequirements schema expr e => BaseTable ( TestTable schema ) ( TestTable expr ) where
  tableName =
    "test"

  tableSchema =
    TestTable "column_a"

type BaseTableRequirements schema expr e = 
  ( schema ~ SCHEMA e, expr ~ EXPR e, Expr e )


test1 =
  Rel8.all

test2 =
  columnA <$> Rel8.all

test3 = do
  a <- test2
  where_ ( a ==. lit ( "" :: Text ) )
  return a


(==.) :: Expr expr => expr a -> expr a -> expr O.PGBool
a ==. b =
  Lens.view
    ( Lens.from _Column )
    ( Lens.view _Column a O..== Lens.view _Column b )

