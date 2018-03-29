{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language FunctionalDependencies #-}
{-# language NoMonomorphismRestriction #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Rel8.Internal.BaseTable ( BaseTable(..), queryTable ) where

import Data.Proxy ( Proxy(..) )
import Data.Tagged ( Tagged, unTagged )
import GHC.Generics ( (:*:)(..), Generic, K1(..), M1(..), Rep, from, to )
import GHC.TypeLits ( KnownSymbol, symbolVal )

import qualified Control.Lens as Lens
import qualified Opaleye as O
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.Table as O
import qualified Opaleye.Internal.Unpackspec as O

import Rel8.Internal.Expr ( _Column )
import Rel8.Internal.Query ( Query, _Query )
import Rel8.Internal.ColumnSchema ( ColumnSchema(..) )
import Rel8.Internal.Lim ( Lim(..) )
import Rel8.Internal.Table ( Table, Columns, ColumnType, fromColumns, toColumns )


class
  ( Table schema
  , Table expr
  , Columns schema ~ Columns expr
  )
    => BaseTable schema expr | schema -> expr, expr -> schema
  where
  tableName :: Tagged schema String

  tableSchema :: schema

  default tableSchema
    :: ( GBaseTable ( Rep schema ), Generic schema )
    => schema
  tableSchema =
    to gtableSchema
  

queryTable
  :: forall table schema expr m s.
     ( BaseTable schema table
     , Query expr m
     , ColumnType table ~ expr
     , ColumnType schema ~ ColumnSchema s
     )
  => m table
queryTable =
  let
    traversePrimExprs =
      O.PackMap $ \f table ->
      fmap
        fromColumns
        ( traverse
          ( \a ->
              fmap
                ( \x ->
                    ( Lim ( Lens.view ( Lens.from _Column ) ( O.Column x ) ) )
                )
                ( f ( O.unColumn ( Lens.view _Column ( runLimit a ) ) ) )
          ) 
          ( toColumns @table table )
        )

    viewTable =
      fromColumns
        ( fmap
            ( \( Lim ( ColumnSchema columnName ) ) ->
                Lim
                  ( Lens.view
                      ( Lens.from _Column )
                      ( O.Column ( O.BaseTableAttrExpr columnName ) )
                  )
            ) 
            ( toColumns ( tableSchema @schema ) )
        )

  in 
  Lens.view
    _Query
    ( O.queryTableExplicit
        ( O.Unpackspec traversePrimExprs ) 
        ( O.Table
            ( unTagged ( tableName @_ @table ) )
            ( O.TableProperties
                ( O.Writer ( O.PackMap ( \_ _ -> pure () ) ) )
                ( O.View viewTable )
            )
        )
    )


class GBaseTable f where
  gtableSchema :: f a


instance GBaseTable f => GBaseTable ( M1 i1 c1 f ) where
  gtableSchema =
    M1 gtableSchema


instance ( GBaseTable f1, GBaseTable g1 ) => GBaseTable ( f1 :*: g1 )  where
  gtableSchema =
    gtableSchema :*: gtableSchema


instance KnownSymbol columnName => GBaseTable ( K1 i ( ColumnSchema s ( Tagged columnName a ) ) ) where
  gtableSchema =
    K1 ( ColumnSchema ( symbolVal ( Proxy @columnName ) ) )
