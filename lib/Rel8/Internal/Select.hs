{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Rel8.Internal.Select ( Select(..) ) where

import qualified Control.Lens as Lens
import qualified Opaleye as O
import qualified Opaleye.Internal.QueryArr as O

import Rel8.Internal.Query ( Query(..) )
import Rel8.Internal.TaggedExpr ( TaggedExpr )


newtype Select a = Select { runSelect :: O.Query a }
  deriving ( Functor, Applicative )


instance Monad Select where
  return =
    pure

  Select ( O.QueryArr f ) >>= g =
    Select $ O.QueryArr $ \( (), primQuery, tag ) ->
      let
        ( a, primQuery', tag' ) =
          f ( (), primQuery, tag )

      in
      case g a of
        Select ( O.QueryArr h ) ->
          h ( (), primQuery', tag' )


instance Query ( TaggedExpr () ) Select where
  _Query =
    Lens.coerced
