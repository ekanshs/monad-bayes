{-|
Module      : Control.Monad.Bayes.Enumerator
Description : Exhaustive enumeration of discrete random variables
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Enumerator (
    Enumerator,
    toPopulation,
    hoist,
    Dist,
    logExplicit,
    explicit,
    evidence,
    mass,
    compact,
    enumerate,
    expectation,
    normalForm
            ) where

import Prelude hiding (sum)

import Data.AEq (AEq, (===), (~==))
import Control.Applicative (Applicative, pure, Alternative)
import Control.Monad (MonadPlus)
import Control.Arrow (second)
import qualified Data.Map as Map
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Numeric.CustomReal
import Numeric.LogDomain (LogDomain, fromLogDomain, toLogDomain)
import Statistics.Distribution.Polymorphic.Discrete
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple
import qualified Control.Monad.Bayes.Population as Pop
import Control.Monad.Bayes.Deterministic


-- | A transformer similar to 'Population', but additionally integrates
-- discrete random variables by enumerating all execution paths.
newtype Enumerator m a = Enumerator {runEnumerator :: Pop.Population m a}
  deriving(Functor, Applicative, Monad, MonadTrans, MonadIO, Alternative, MonadPlus)

instance HasCustomReal m => HasCustomReal (Enumerator m) where
  type CustomReal (Enumerator m) = CustomReal m

instance {-# OVERLAPPING #-} (CustomReal m ~ r, MonadDist m) =>
         Sampleable (Discrete r) (Enumerator m) where
  sample d =
    Enumerator $ Pop.fromWeightedList $ pure $ map (second toLogDomain) $ zip [0..] $ V.toList $ normalize $ weights d

instance {-# OVERLAPPING #-} (Sampleable d m, Monad m) => Sampleable d (Enumerator m) where
  sample = lift . sample

instance (Monad m, HasCustomReal m) => Conditionable (Enumerator m) where
  factor w = Enumerator $ factor w

instance MonadDist m => MonadDist (Enumerator m)
instance MonadDist m => MonadBayes (Enumerator m)

-- | Convert 'Enumerator' to 'Population'.
toPopulation :: Enumerator m a -> Pop.Population m a
toPopulation = runEnumerator

-- | Apply a transformation to the inner monad.
hoist :: (MonadDist m, MonadDist n, CustomReal m ~ CustomReal n) =>
  (forall x. m x -> n x) -> Enumerator m a -> Enumerator n a
hoist f = Enumerator . Pop.hoist f . toPopulation

-- | A monad for discrete distributions enumerating all possible paths.
-- Throws an error if a continuous distribution is used.
type Dist r a = Enumerator (Deterministic r) a

-- | Throws an error if continuous random variables were used in 'Dist'.
ensureDiscrete :: Deterministic r a -> a
ensureDiscrete =
  fromMaybe (error "Dist: there were unhandled continuous random variables") .
  maybeDeterministic

-- | Returns the posterior as a list of weight-value pairs without any post-processing,
-- such as normalization or aggregation
logExplicit :: IsCustomReal r => Dist r a -> [(a, LogDomain r)]
logExplicit = ensureDiscrete . Pop.runPopulation . toPopulation

-- | Same as `toList`, only weights are converted from log-domain.
explicit :: (IsCustomReal r) => Dist r a -> [(a,r)]
explicit = map (second fromLogDomain) . logExplicit

-- | Returns the model evidence, that is sum of all weights.
evidence :: (IsCustomReal r) => Dist r a -> LogDomain r
evidence = ensureDiscrete . Pop.evidence . toPopulation

-- | Normalized probability mass of a specific value.
mass :: (IsCustomReal r, Ord a) => Dist r a -> a -> r
mass d = f where
  f a = case lookup a m of
             Just p -> p
             Nothing -> 0
  m = enumerate d

-- | Aggregate weights of equal values.
-- The resulting list is sorted ascendingly according to values.
compact :: (Num r, Ord a) => [(a,r)] -> [(a,r)]
compact = Map.toAscList . Map.fromListWith (+)

-- | Aggregate and normalize of weights.
-- The resulting list is sorted ascendingly according to values.
--
-- > enumerate = compact . explicit
enumerate :: (IsCustomReal r, Ord a) => Dist r a -> [(a,r)]
enumerate d = compact (zip xs ws) where
  (xs, ws) = second (map fromLogDomain . normalize) $ unzip (logExplicit d)

-- | Expectation of a given function computed using normalized weights.
expectation :: (IsCustomReal r) => (a -> r) -> Dist r a -> r
expectation f = ensureDiscrete . Pop.popAvg f . Pop.normalize . toPopulation

-- | 'compact' followed by removing values with zero weight.
normalForm :: (Ord a, IsCustomReal r) => Dist r a -> [(a,r)]
normalForm = filter ((/= 0) . snd) . compact . explicit

instance (Ord a, IsCustomReal r) => Eq (Dist r a) where
  p == q = normalForm p == normalForm q

instance (Ord a, IsCustomReal r, AEq r) => AEq (Dist r a) where
  p === q = xs == ys && ps === qs where
    (xs,ps) = unzip (normalForm p)
    (ys,qs) = unzip (normalForm q)
  p ~== q = xs == ys && ps ~== qs where
    (xs,ps) = unzip $ filter (not . (~== 0) . snd) $ normalForm p
    (ys,qs) = unzip $ filter (not . (~== 0) . snd) $ normalForm q
