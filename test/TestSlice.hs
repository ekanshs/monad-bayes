{-# LANGUAGE
  TypeFamilies
 #-}

module TestWeighted where

import Test.Hspec
import Data.AEq
import Control.Monad.State
import Data.List
import Data.Bifunctor (second)

import Numeric.LogDomain (LogDomain, fromLogDomain, toLogDomain)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Inference


-- This program should sample from the joint distribution of x and u.
model :: (MonadBayes m, CustomReal m ~ Double) =>   m (Double)
model = sliceSample 100 (uniformDist 0 1) (\x -> uniform 0 x)

result :: (MonadDist m, CustomReal m ~ Double) => m (Double, Double)
result = fmap (second fromLogDomain) $ runWeighted model

