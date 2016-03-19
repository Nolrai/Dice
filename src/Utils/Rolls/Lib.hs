module Utils.Rolls.Lib where

{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction, functionalDependancies  #-}

import Prelude hiding ((>>), (>>=), return)
import qualified Data.MultiSet as MS
import Data.MultiSet (MultiSet)

type Results = MultiSet

sided : Int -> Results Int
n `sided` = MkResults (MS.fromAscList (\ i -> [i]) [1 .. n])

x <*> y =
  do
  xl <- x
  yl <- y
  return (xl ++ yl)

class RMonad m (c : Type -> Constraint) | m -> c where
  return : c a => a
  (>>=) : c b => m a -> (a -> m b) -> m b

instance RMonad Results Ord where
  return = singleton
  a >>= f = bind a f

highest n = take n . sort

partitionEven pools poolSize l := deal pools . take needed $ higest (pools*poolSize) l ++ repeat 0

deal n = go (replicate n [])
  where
  go hands [] = hands
  go hands deck =
    let (delt, newDeck) = splitAt n deck in
    let newHands = zipWith (:) delt hands in
    go newHands newDeck

partitionBest pools poolSize l = take pools $ groupAt poolSize $ sort l

showResults r = unlines $ map (\ ( value, probability) -> show value ++ " " show probability) $ normalize $ toAscOccurList r

normalize l = map (\ (v, i) -> (v, fromIntegral i / theSum) ) l
  where
  theSum :: Double
  theSum = sum $ map (\ (_, i) -> fromIntegral i) l
