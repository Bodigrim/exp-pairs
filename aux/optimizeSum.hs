{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Data.Set (Set)
import qualified Data.Set as S
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Data.Bimap (Bimap)
import qualified Data.Bimap as B

import Data.String
import Text.RawString.QQ     (r)
import Data.Char
import Data.Monoid
import Data.Foldable

import Debug.Trace

import Data.Maybe
import Data.Ord

import Text.PrettyPrint.Leijen hiding ((<>))


import Prelude hiding (foldl, foldl1, maximum)

data Sign = Minus | Plus
	deriving (Eq, Ord, Show)

instance Pretty Sign where
	pretty Minus = char '-'
	pretty Plus  = char '+'

data Form = Form {lhs :: String, rhs :: Set (Sign, String)}
	deriving (Eq, Ord, Show)

type Factor = Int
type Expr   = IntSet
data Forms = Forms {ids :: Bimap String Int, exprs :: IntMap Expr}
	deriving (Eq, Show)

nextId :: (Ord a, Num b, Enum b) => Bimap a b -> b
nextId bm
	| B.null bm = 1
	| otherwise  = succ . fst . B.findMaxR $ bm

addForm :: Form -> Forms -> Forms
addForm (Form{..}) (Forms{..}) = Forms ids'' exprs' where
	lhsId = nextId ids
	ids' :: Bimap String Int
	ids' = B.insert lhs lhsId ids

	ids'' :: Bimap String Int
	rhsExpr :: Expr
	(ids'', rhsExpr) = foldl f (ids', mempty) rhs
	exprs' :: IntMap Expr
	exprs' = IM.insert lhsId rhsExpr exprs

	f :: (Bimap String Int, Expr) -> (Sign, String) -> (Bimap String Int, Expr)
	f (ids, expr) (sign, str) = (ids1, expr1) where
		maybeId = B.lookup str ids
		(ids1, newId) = case maybeId of
			Nothing -> (B.insert str (nextId ids) ids, nextId ids)
			Just i  -> (ids, i)
		expr1 = IS.insert (if sign==Plus then newId else negate newId) expr

instance IsString Forms where
	fromString s = foldl (flip addForm) (Forms B.empty mempty) fs where
		fs = map parseForm $ filter (not . null) $ lines s

parseForm :: String -> Form
parseForm xs = case words xs of
	(lhs : "=" : ys) -> Form lhs (f ys) where
		f [] = mempty
		f ("+":z:zs) = S.singleton (Plus, z) <> f zs
		f ("-":z:zs) = S.singleton (Minus, z) <> f zs
		f (z:zs) = S.singleton (Plus, z) <> f zs
	as -> trace (show as) undefined

instance Pretty Forms where
	pretty (Forms{..}) = vsep (IM.elems $ IM.mapWithKey f exprs) where
		toStr i = fromJust $ B.lookupR i ids
		f :: Int -> Expr -> Doc
		f lhsId expr = text (toStr lhsId) <+> equals <+> hsep (map g $ IS.toList expr)
		g n
			| n < 0 = pretty Minus <+> text (toStr (-n))
			| otherwise = pretty Plus <+> text (toStr n)

forms = forms2

forms1 :: Forms
forms1 = [r|
r1  = m5 + m10 + m11 + m12
r2  = m8 + m10 - m14 + m17 - m18 + m19 - m22
r3  = m1 - m11 - m12 - m16 + m17 - m18 + m19 - m22
r4  = m6 - m10 + m11 + m13
r5  = m2 - m10 + m13 + m14 + m15 + m17
r6  = m9 - m11 + m15 - m16 + m17
r7  = m7 - m10 - m11 + m20 - m21 + m22
r8  = m3 - m10 + m14 - m17 + m18 + m20 + m22
r9  = m4 + m11 + m16 - m17 + m18 + m21
|]

forms2 :: Forms
forms2 = [r|
l1  = a3 + c1 - c2
l2  = a2 + b1 + b2
l3  = a2 + b1 + b3
l4  = a3 - c2 - c3
l5  = a1 - c1 + c2
l6  = a1 + b1 + b2
l7  = a1 + b1 + b3 + c2 + c3
l8  = a2
l9  = a3
l10 = b1
l11 = c2
l12 = c1 - c2
l13 = b1 + b2
l14 = a2 + b1
l15 = b2
l16 = a3 - c2
l17 = c2
l18 = b3 - c2 - c3
l19 = c1 + c3 - b1 - b3
l20 = b1 + b3
l21 = c2 + c3
l22 = c2 + c3 - b1 - b3
|]


forms3 :: Forms
forms3 = [r|
r1 =  k1 + k7 - k8 + k9
r2 =  k2 - k4 + k5 - k6
r3 =  k3 - k4 + k5 - k6
r4 =  k3 - k7 + k8 - k9
r5 =  k1
r6 =  k2
r7 =  k3
r8 =  k1 + k4 - k5 + k6
r9 =  k2 + k7 - k8 + k9
r10 =  k4
r11 =  k7
r12 =  k1 + k7
r13 =  k4 - k2
r14 =  k4 - k5 + k6
r15 =  k6
r16 =  k7 - k8 + k9
r17 =  k8
r18 =  k6
r19 =  k8
r20 =  k4 - k3 + k6 + k8
r21 =  k3 + k6 - k7 + k8
r22 =  k6 + k8
|]

flipSign :: Expr -> Expr
flipSign = IS.map negate

splitRoot :: IntSet -> [IntSet]
splitRoot s
	| IS.null s = [s]
	| otherwise = case IS.splitRoot s of
		[_] -> [IS.singleton m, t'] where
			(m, t') = IS.deleteFindMin s
		ts -> ts

subsets :: IntSet -> Set IntSet
subsets s
	| IS.null s      = S.singleton mempty
	| IS.size s == 1 = S.fromList [IS.empty, s]
	| otherwise      = foldl1 f $ map subsets $ splitRoot s where
		f :: Set IntSet -> Set IntSet -> Set IntSet
		f xs ys = S.fromList [xs <> ys | xs <- toList xs, ys <- toList ys]

nonTrivial :: Expr -> Bool
nonTrivial s = IS.size s > 1

absExpr :: Expr -> Expr
absExpr s = if el == IS.findMax s then s else flipSign s where
	el = maximum $ map abs $ IS.toList s

suspicious :: Forms -> Set IntSet
suspicious (Forms{..}) = maxSusp where
	subexprs = IM.toList $ IM.map (S.map absExpr . S.filter nonTrivial . subsets) exprs
	susp = S.fromList [e11 | (k1, e1) <- subexprs, e11 <- toList e1, (k2, e2) <- subexprs, e22 <- toList e2, e11 == e22, k1 /= k2]
	maxL = maximum $ S.map IS.size susp
	maxSusp = S.filter (\s -> IS.size s == maxL) susp

substitute :: Forms -> Expr -> Forms
substitute (Forms{..}) newExpr = Forms ids' exprs'' where
	lhsId = nextId ids
	lhs = "t" ++ show lhsId
	ids' = B.insert lhs lhsId ids

	exprs' :: IntMap Expr
	exprs' = IM.map eliminate exprs

	eliminate expr = if newExpr `IS.isSubsetOf` expr
		then IS.insert lhsId (expr `IS.difference` newExpr)
		else if (flipSign newExpr) `IS.isSubsetOf` expr
		then IS.insert (-lhsId) (expr `IS.difference` flipSign newExpr)
		else expr

	exprs'' :: IntMap Expr
	exprs'' = IM.insert lhsId newExpr exprs'

weight :: Forms -> Int
weight (Forms{..}) = getSum $ foldMap (Sum . pred . IS.size) exprs


optimize :: Forms -> Forms
optimize fs = if S.null susp then fs else fs' where
	fs' = minimumBy (comparing weight) $ map (optimize . substitute fs) $ toList susp
	susp = suspicious fs

main :: IO ()
main = do
	putStrLn $ show $ pretty $ optimize forms1
	putStrLn $ show $ pretty $ optimize forms2
	putStrLn $ show $ pretty $ optimize forms3
