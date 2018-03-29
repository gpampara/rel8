{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Rel8.Internal.Operators where

import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Operators as O
import qualified Opaleye.PGTypes as O
import Prelude hiding (not)
import Rel8.Internal.DBType
import Rel8.Internal.Expr

--------------------------------------------------------------------------------
infix 4 ==.,  <. , <=. , >. , >=.
infixr 2 ||.
infixr 3 &&.

--------------------------------------------------------------------------------
-- | Corresponds to @NOT@.
not_ :: ExprT m Bool -> ExprT m Bool
not_ (ExprT a) = ExprT (O.UnExpr O.OpNot a)

-- | Corresponds to @AND@.
(&&.) :: ExprT m Bool -> ExprT m Bool -> ExprT m Bool
ExprT a &&. ExprT b = ExprT (O.BinExpr O.OpAnd a b)

-- | Corresponds to @OR@.
(||.) :: ExprT m Bool -> ExprT m Bool -> ExprT m Bool
ExprT a ||. ExprT b = ExprT (O.BinExpr O.OpOr a b)

class ToNullable a (Maybe Bool) => Predicate a
instance Predicate Bool
instance Predicate (Maybe Bool)

-- | Lift a binary operator over @null@ inputs. It is assumed that the
-- operator returns @null@ if any of its inputs are @null@ (as described
-- by @RETURNS NULL ON NULL INPUT@ to @CREATE FUNCTION@).
liftOpNull
  :: (ExprT m a -> ExprT m b -> ExprT m c)
  -> ExprT m (Maybe a)
  -> ExprT m (Maybe b)
  -> ExprT m (Maybe c)
liftOpNull f a b = unsafeCoerceExpr (unsafeCoerceExpr a `f` unsafeCoerceExpr b)

mapNull
  :: (ExprT m a -> ExprT m b) -> ExprT m (Maybe a) -> ExprT m (Maybe b)
mapNull f = unsafeCoerceExpr . f . unsafeCoerceExpr

--------------------------------------------------------------------------------
-- | The class of types that can be compared for equality within the database.
class DBType a => DBEq a where
  -- | Corresponds to @=@.
  (==.) :: ExprT m a -> ExprT m a -> ExprT m Bool
  ExprT a ==. ExprT b = ExprT (O.BinExpr (O.:==) a b)

instance DBEq Bool where
instance DBEq Char where
instance a ~ Char => DBEq [a] where
instance DBEq Double where
instance DBEq Float where
instance DBEq Int16 where
instance DBEq Int32 where
instance DBEq Int64 where
instance DBEq Text where
instance DBEq UTCTime where


--------------------------------------------------------------------------------
class DBEq a => DBOrd a where
  -- | The PostgreSQL @<@ operator.
  (<.) :: ExprT m a -> ExprT m a -> ExprT m Bool
  a <. b = columnToExpr (exprToColumn @_ @O.PGInt8 a O..< exprToColumn b)

  -- | The PostgreSQL @<=@ operator.
  (<=.) :: ExprT m a -> ExprT m a -> ExprT m Bool
  a <=. b = columnToExpr (exprToColumn @_ @O.PGInt8 a O..<= exprToColumn b)

  -- | The PostgreSQL @>@ operator.
  (>.) :: ExprT m a -> ExprT m a -> ExprT m Bool
  a >. b = columnToExpr (exprToColumn @_ @O.PGInt8 a O..> exprToColumn b)

  -- | The PostgreSQL @>@ operator.
  (>=.) :: ExprT m a -> ExprT m a -> ExprT m Bool
  a >=. b = columnToExpr (exprToColumn @_ @O.PGInt8 a O..>= exprToColumn b)

instance DBOrd Bool where
instance DBOrd Char where
instance DBOrd Double where
instance DBOrd Float where
instance DBOrd Int16 where
instance DBOrd Int32 where
instance DBOrd Int64 where
instance DBOrd Text where
instance DBOrd UTCTime where

-- | Case statement. @case_ [(x,a), (y, b)] c@ corresponds to
-- @CASE WHEN x THEN a WHEN y THEN b ELSE c END@.
case_ :: Predicate bool => [(ExprT m bool, ExprT m a)] -> ExprT m a -> ExprT m a
case_ cases defaultCase =
  columnToExpr
    (O.case_
       (map
          (\(predicate, when) ->
             (exprToColumn (toNullable predicate), exprToColumn when))
          cases)
       (exprToColumn defaultCase))
