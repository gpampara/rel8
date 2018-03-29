{-# language TypeInType, KindSignatures, DataKinds, DeriveGeneric, FunctionalDependencies, TypeFamilies, RankNTypes, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances, GeneralizedNewtypeDeriving, TypeApplications, UndecidableInstances #-}

import Data.Functor.Identity
import Data.Tagged
import Data.Kind ( Type )
import GHC.Generics ( Generic )
import GHC.TypeLits ( Symbol )

data QueryResult a

data ExprT f a

data SchemaInfo (info :: ( Symbol, HasDefault, Type ))

data HasDefault
  = HasDefault
  | NoDefault

type family C f columnName hasDefault columnType :: Type where
  C QueryResult _name _def t = t
  C (ExprT f) _name _def t = ExprT f t
  C SchemaInfo name hasDefault t = SchemaInfo '(name, hasDefault, t)
  -- C Insert name 'HasDefault t = Default (Expr t)
  -- C Insert name 'NoDefault t = Expr t
  -- C Aggregate name _ t = Aggregate t

class Table (expr :: k -> Type) (t :: Type) | t -> expr where
  type RowF t :: Type -> Type

  toRow :: t -> RowF t (Limit expr)
  fromRow :: RowF t (Limit expr) -> t

class Select (expr :: Type) (t :: Type) | expr -> t, t -> expr

class BaseTable t where
  tableName :: Tagged t String

queryTable :: forall t m. (Table (ExprT m) t) => m t
queryTable = undefined

data Q (a :: Type)

instance Functor Q
instance Applicative Q
instance Monad Q
  
select :: (Table (ExprT Q) expr, Select expr hs) => Q expr -> IO hs
select = undefined

newtype AggM s m a = AggM (m a)
  deriving (Functor, Applicative, Monad)

aggregate
  :: (Table (ExprT m) aggregated)
  => (forall s. AggM s m aggregated)
  -> m aggregated
aggregate _ = undefined

-- aggregate
--   :: (Table aggregated', Query e1 m, RowExpr aggregated' ~ e1, RowExpr aggregated ~ e1  )
--   => ( forall m e2.
--          ( Query ( MustAggregate e1 ) m, Table aggregated )
--          => m aggregated )
--   -> m aggregated'
-- aggregate _ = undefined

summing :: ExprT (AggM s m) a -> ExprT m a
summing = undefined

data T f = T { colA :: C f "foo" 'NoDefault Int }
  deriving ( Generic )

newtype Limit f = Limit
  { runLimit :: forall a. f a
  }

instance Table expr (T expr) where
instance ( expr ~ ExprT Q, haskell ~ QueryResult ) => Select (T expr) (T haskell)

newtype Col a = Col a
  
instance Table (ExprT m :: Type -> Type) (ExprT m a)

instance Select (ExprT Q a) (Col a)

test1 = do
  foo <- queryTable
  return foo

test2 = do
  bar <- queryTable
  return (colA bar)

test3 = do
  bar <- queryTable 
  return (summing (colA bar))

test4 = aggregate $ do
  bar <- queryTable
  return (summing (colA bar))

test5 = do
  foo <- queryTable
  bar <- aggregate $ do
    bar <- queryTable
    return (T (summing (colA bar)))
  return (colA foo, bar, foo)

-- test6 = do
--   foo <- queryTable
--   bar <- aggregate $ do
--     return (T $ summing (colA foo))
--   return (foo,bar)
