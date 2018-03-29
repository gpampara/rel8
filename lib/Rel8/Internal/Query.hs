{-# language FunctionalDependencies #-}

module Rel8.Internal.Query ( Query(..), where_ ) where

import qualified Control.Lens as Lens
import qualified Opaleye as O
import qualified Opaleye.Internal.QueryArr as O

import Rel8.Internal.Expr ( Expr, _Column )


-- Any @Query m@ is isomorphic to @O.Query@.
class ( Expr expr, Monad m ) => Query expr m | m -> expr where
  _Query :: Lens.Iso' ( O.Query a ) ( m a )


where_ :: Query expr m => expr O.PGBool -> m ()
where_ e =
  let
    q =
      O.QueryArr $ \( (), primQuery, tag ) ->
        O.runQueryArr O.restrict ( Lens.view _Column e, primQuery, tag )

  in
  Lens.view _Query q
