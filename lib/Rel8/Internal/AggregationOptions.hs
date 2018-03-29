module Rel8.Internal.AggregationOptions ( AggregationOptions(..), defaultAggregationOptions ) where

import qualified Opaleye.Internal.HaskellDB.PrimQuery as O


data AggregationOptions = AggregationOptions
  { distinct :: !O.AggrDistinct
  , order :: ![ O.OrderExpr ] 
  }

defaultAggregationOptions :: AggregationOptions
defaultAggregationOptions =
  AggregationOptions
    { distinct =
        O.AggrAll
    , order =
        []
    }
