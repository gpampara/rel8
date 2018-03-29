{-# language DefaultSignatures #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Internal.Transport ( Transport(..) ) where

import Data.Proxy ( Proxy(..) )
import Data.Functor.Identity ( Identity(..) )
import GHC.Generics ( (:*:)(..), Generic, K1(..), M1(..), Rep, from, to )

import qualified Control.Lens as Lens
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Opaleye.Internal.Column as O 
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O

import Rel8.Internal.Expr
import Rel8.Internal.TaggedExpr


class Transport expr haskell | expr -> haskell, haskell -> expr where
  rowParser :: Pg.RowParser haskell

  litTable :: haskell -> expr

  default rowParser
    :: ( GTransport ( Rep expr ) ( Rep haskell )
       , Generic haskell
       )
    => Pg.RowParser haskell
  rowParser =
    to <$> growParser (Proxy @(Rep expr ()))

  default litTable
    :: ( GTransport ( Rep expr ) ( Rep haskell )
       , Generic haskell
       , Generic expr
       )
    => haskell -> expr
  litTable =
    to . glitTable . from


class GTransport f g where
  growParser :: Functor proxy => proxy ( f a ) -> Pg.RowParser ( g a )

  glitTable :: g a -> f a


instance GTransport f g => GTransport ( M1 i1 c1 f ) ( M1 i2 c2 g ) where
  growParser expr =
    M1 <$> growParser ( fmap ( \( M1 a ) -> a ) expr )

  glitTable ( M1 a ) =
    M1 ( glitTable a ) 


instance ( GTransport f1 f2, GTransport g1 g2) => GTransport ( f1 :*: g1 ) ( f2 :*: g2 ) where
  growParser expr =
    (:*:) <$> growParser ( ( \( l :*: _ ) -> l ) <$> expr )
          <*> growParser ( ( \( _ :*: r ) -> r ) <$> expr )

  glitTable ( l :*: r ) =
    glitTable l :*: glitTable r


instance Transport a b => GTransport ( K1 i a ) ( K1 j b ) where
  growParser expr =
    K1 <$> rowParser

  glitTable ( K1 a ) =
    K1 ( litTable a )


instance {-# OVERLAPS #-} Transport ( TaggedExpr s a ) ( Identity b ) => GTransport ( K1 i ( TaggedExpr s a ) ) ( K1 j b ) where
  growParser expr =
    K1 . runIdentity <$> rowParser

  glitTable ( K1 a ) =
    K1 ( litTable ( Identity a ) )


-- instance GTable ( K1 i ( ColumnSchema s a ) ) where

instance ( Pg.FromField a, Pg.ToField a, s ~ () ) => Transport ( TaggedExpr s a ) ( Identity a ) where
  rowParser =
    Identity <$> Pg.field
    
  litTable ( Identity a ) =
    Lens.view
      ( Lens.from _Column )
      ( O.Column ( actionToPrimExpr ( Pg.toField a ) ) )

actionToPrimExpr :: Pg.Action -> O.PrimExpr
actionToPrimExpr ( Pg.Plain builder ) =
  O.ConstExpr ( O.OtherLit ( Data.ByteString.Lazy.Char8.unpack ( Builder.toLazyByteString builder ) ) )
