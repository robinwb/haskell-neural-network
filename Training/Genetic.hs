module Training.Genetic (
	--train
) where

import Prelude as P
import qualified Network as NET
import Rob as ROB
import Data.Sequence as S
import Data.Packed.Matrix as MP
import Data.Packed.Vector as VP
import Numeric.Container as N
import System.Random (StdGen)

-- x and y are the separation point
-- x is the layer
-- y is the neuron
breed n m x y = ((S.take x (NET.neuronWeights n)) S.|> mixedLayer) S.>< (S.drop (x+1) (NET.neuronWeights m))
	where
		mixedLayer = ROB.joinHoriz $ [MP.takeColumns y (S.index (NET.neuronWeights n) x), MP.dropColumns y (S.index (NET.neuronWeights m) x)]

-- ss being the list of sizes as [(x_size, y_size), ...]
nRandsMatrix [] g = (S.empty,g)
nRandsMatrix (s:ss) g = (x S.<| xs,g'')
	where
		(randsList,g') = nRands ((fst s)*(snd s)) g
		x = ((fst s) MP.>< (snd s)) randsList
                             (xs,g'') = nRandsMatrix ss g'

mutate n gen = (n {NET.neuronWeights=mutatedWeights}, g'')
	where
		mutatedWeights = S.zipWith (N.add) (NET.neuronWeights n) weightChanges
		(randWeights,g'') = nRandsMatrix (adjZip $ NET.nw n) gen :: (Seq (Matrix Double),StdGen)
		weightChanges = P.fmap (MP.mapMatrix (mutateWeight)) randWeights
		mutateWeight x = x*((NET.maxWeight n) - (NET.minWeight n)) - (NET.minWeight n)

-- assumes pop is sorted already
--{--
newGeneration pop gen = (newPop,g')
	where
		newPop = elitist S.>< offspring
		maxError = 5*(F.foldl1 (\a -> (+) (1.0/(totalError a))) (S.take (round $ S.length/5) pop))
		breedingPairs = 
		offspring = breed 
		
--}
{--
-- trains until maxItr or eps
train pop maxItr eps gen = (finalPop,g'')
	where
		(nextPop,g') = train newpop (maxItr-1) eps 
--}
--
sortPop pop i o = sortedPop
	where
		errorPop = P.fmap (lrotate3 NET.updateError i o) pop
		sortMinError n m = compare (NET.totalError n) (NET.totalError m)
		sortedPop = S.unstableSortBy sortMinError errorPop

mkPopulation 0 nw initWeight activationFunc thresholdFunc minWeight maxWeight gen = (S.empty,gen)
mkPopulation n nw initWeight activationFunc thresholdFunc minWeight maxWeight gen = (x S.<| xs,g'')
	where
		(x,g') = NET.mkNetwork nw initWeight activationFunc thresholdFunc minWeight maxWeight gen
		(xs,g'') = mkPopulation (n-1) nw initWeight activationFunc thresholdFunc minWeight maxWeight g'
