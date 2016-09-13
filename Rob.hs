module Rob (
	nRands,
	nRandsMatrix,
	adjZip,
	joinHoriz,
	joinVert,
	lrotate3,
	randMap
) where

import System.Random as R
import Data.Sequence as S
import Data.Packed.Matrix as MP
import Data.Packed.Vector as VP

-- | creates a matrix from a vertical list of matrices
joinVert :: Element t => [Matrix t] -> Matrix t
joinVert ms = reshape (cols $ head ms) $ join (map flatten ms)

-- | creates a matrix from a horizontal list of matrices
joinHoriz :: Element t => [Matrix t] -> Matrix t
joinHoriz ms = MP.trans . joinVert . map trans $ ms

adjZip [a] = []
adjZip (l0:l1:ls) = (l0,l1):(adjZip (l1:ls))

lrotate3 f a b c = f b c a

-- mapRand :: (a -> b) -> g -> Seq a -> Seq b

nRands 0 g = ([],g)
nRands n g = (x:xs,g'')
	where
		(x,g') = R.random g
		(xs,g'') = nRands (n-1) g'

randMap g f [] = ([],g)
randMap g f (y:ys) = (x:xs,g'')
	where
		(x,g') = f y g
		(xs,g'') = randMap g' f ys

seqRandMap g f y = if S.length y == 0 then (S.empty,g) else (x S.<| xs,g'')
	where
		(x,g') = f (S.take 1 y) g 
		(xs,g'') = seqRandMap g' f (S.drop 1 y)
