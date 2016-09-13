module Network (
	Network(Network,neuronWeights,nw,maxWeight,minWeight,totalError,outputError),
	mkNetwork,
	updateError,
	feedForward
) where

import Prelude as P
import Rob as ROB
import Data.Sequence as S
import Data.Packed.Vector as VP
import Data.Packed.Matrix as MP
import Data.Foldable as F
import Numeric.Container as N

data Network = Network {
	totalError :: Double,
	outputError :: VP.Vector Double,
	neuronWeights :: S.Seq (MP.Matrix Double),
	nw :: [Int], -- size of each layer, including input and output
	numNeurons :: Int,
	numWeights :: Int,
	activationFunc :: (Double -> Double),
	thresholdFunc :: (Double -> Double),
	minWeight :: Double,
	maxWeight :: Double
}

--{--
-- Generating a new network:
mkWeights [] initWeight g = (S.empty,g)
mkWeights ((a,b):aznw) initWeight g = (value S.<| restValues,g'')
	where
		value = (a MP.>< b) weightsList
		(restValues,g'') = mkWeights aznw initWeight g'

		weightsList = P.map (initWeight) randNumList
		(randNumList,g') = nRands (a*b) g

mkNetwork nw initWeight activationFunc thresholdFunc minWeight maxWeight g = (Network {nw=nw, totalError=totalError, outputError=outputError, neuronWeights=neuronWeights, numNeurons=numNeurons, numWeights=numWeights, activationFunc=activationFunc, thresholdFunc=thresholdFunc, minWeight=minWeight, maxWeight=maxWeight}, g'')
	where
		totalError = 0.0
		outputError = VP.fromList (P.replicate (last nw) 0.0)
		(neuronWeights,g'') = mkWeights (adjZip nw) initWeight g
		numNeurons = P.foldl1 (+) (P.tail $ P.init nw)
		numWeights = F.foldl1 (+) (P.fmap (\m -> (MP.cols m)*(MP.rows m)) neuronWeights)
--}

feedForward n i = F.foldl (func) i (weights)
	where
		func a b = VP.mapVector (thresholdFunc n . activationFunc n) $ (addBias a) `N.vXm` b
		addBias a = VP.join [VP.fromList [1],a]
		weights = neuronWeights n

updateError n i o = n {totalError=totalError, outputError=outputError}
	where
		totalError = VP.foldVector (+) 0 outputError
		outputError = VP.zipVectorWith (\a b -> (abs (a-b))^2) real o
		real = feedForward n i

