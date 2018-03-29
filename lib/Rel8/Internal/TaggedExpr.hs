{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Internal.TaggedExpr ( TaggedExpr(..) ) where

import GHC.Generics ( Generic )
import Data.Functor.Compose ( Compose(..) )
import Data.Tagged ( Tagged )
import Data.Functor.Identity ( Identity(..) )

import qualified Control.Lens as Lens
import qualified Opaleye.Internal.Column as O 
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Opaleye.Internal.Column as O 
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O

import Rel8.Internal.Expr
import Rel8.Internal.Lim


newtype TaggedExpr s a =
  TaggedExpr ( O.Column a )
  deriving ( Generic )

  
instance Expr ( TaggedExpr s ) where
  _Column =
    Lens.coerced 

instance Limit ( TaggedExpr s ) where
  limit ( TaggedExpr column ) =
    Lim ( TaggedExpr ( O.unsafeCoerceColumn column ) )

