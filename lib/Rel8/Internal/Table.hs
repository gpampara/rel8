{-# language DefaultSignatures #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language PolyKinds #-}

module Rel8.Internal.Table ( Table(..) ) where

import Data.Functor.Identity ( Identity, runIdentity )
import Data.Functor.Product ( Product(..) )
import Data.Functor.Rep ( Representable )
import GHC.Generics ( (:*:)(..), Generic, K1(..), M1(..), Rep )

import qualified GHC.Generics
import qualified Opaleye as O 

import Rel8.Internal.ColumnSchema ( ColumnSchema )
import Rel8.Internal.Lim ( Lim, limit, runLimit )
import Rel8.Internal.TaggedExpr ( TaggedExpr )


class Traversable ( Columns table ) => Table table where
  type Columns table :: * -> *

  type ColumnType table :: * -> *

  toColumns :: table -> Columns table ( Lim ( ColumnType table ) )

  fromColumns :: Columns table ( Lim ( ColumnType table ) ) -> table


  -- Default implementation

  type Columns table = GColumns ( Rep table )

  type ColumnType table = GColumnType ( Rep table )

  default toColumns
    :: ( Generic table
       , GTable ( Rep table )
       , Columns table ~ GColumns ( Rep table )
       , GColumnType ( Rep table ) ~ ColumnType table
       )
    => table -> Columns table ( Lim ( ColumnType table ) )
  toColumns =
    gtoColumns . GHC.Generics.from

  default fromColumns
    :: ( Generic table
       , GTable ( Rep table )
       , Columns table ~ GColumns ( Rep table )
       , GColumnType ( Rep table ) ~ ColumnType table
       )
    => Columns table ( Lim ( ColumnType table ) ) -> table
  fromColumns =
    GHC.Generics.to . gfromColumns


instance Table ( TaggedExpr s a ) where
  type Columns ( TaggedExpr s a ) =
    Identity

  type ColumnType ( TaggedExpr s a ) =
    TaggedExpr s

  toColumns =
    pure . limit

  fromColumns =
    runLimit . runIdentity 


type family GColumns (f :: * -> *) :: * -> * where
  GColumns ( M1 i c f ) = GColumns f
  GColumns ( a :*: b ) = Product ( GColumns a ) ( GColumns b )
  GColumns ( K1 i ( ColumnSchema s a ) ) = Identity
  GColumns ( K1 i ( TaggedExpr s a ) ) = Identity
  GColumns ( K1 i a ) = Columns a


type family GColumnType (f :: * -> *) :: * -> * where
  GColumnType ( M1 i c f ) = GColumnType f
  GColumnType ( a :*: b ) = GColumnType a
  GColumnType ( K1 i ( ColumnSchema s a ) ) = ColumnSchema s
  GColumnType ( K1 i ( TaggedExpr s a ) ) = TaggedExpr s
  GColumnType ( K1 i a ) = ColumnType a


class GTable ( f :: * -> * ) where
  gtoColumns :: f a -> GColumns f ( Lim ( GColumnType f ) )

  gfromColumns :: GColumns f ( Lim ( GColumnType f ) ) -> f a


instance GTable f => GTable ( M1 i c f ) where
  gtoColumns ( M1 a ) =
    gtoColumns a

  gfromColumns =
    M1 . gfromColumns


instance ( GTable l, GTable r, GColumnType l ~ GColumnType r ) => GTable ( l :*: r ) where
  gtoColumns ( l :*: r ) =
    Pair ( gtoColumns l ) ( gtoColumns r )

  gfromColumns ( Pair l r ) =
    gfromColumns l :*: gfromColumns r


instance GTable ( K1 i ( TaggedExpr s a ) ) where
  gtoColumns ( K1 a ) =
    pure ( limit a )

  gfromColumns  =
    K1 . runLimit . runIdentity


instance GTable ( K1 i ( ColumnSchema s a ) ) where
  gtoColumns ( K1 a ) =
    pure ( limit a )

  gfromColumns =
    K1 . runLimit . runIdentity


-- | This allows people to have a "table of tables".
instance {-# OVERLAPPABLE #-} ( Table a, ColumnType a ~ GColumnType ( K1 i a ), GColumns ( K1 i a ) ~ Columns a ) => GTable ( K1 i a ) where
  gtoColumns ( K1 a ) =
    toColumns a

  gfromColumns =
    K1 . fromColumns
