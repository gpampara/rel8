{-# language FlexibleInstances #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}

module Rel8.Internal.Expr ( Expr(..) ) where

import Data.Functor.Compose ( Compose(..) )
import Data.Tagged ( Tagged )

import qualified Control.Lens as Lens
import qualified Opaleye as O


class Expr f where
  _Column :: Lens.Iso' ( f a ) ( O.Column a ) 


instance ( o ~ O.Column, t ~ Tagged s ) => Expr ( Compose o t ) where
  _Column =
    Lens.iso ( \( Compose a ) -> O.unsafeCoerceColumn a ) ( Compose . O.unsafeCoerceColumn )
