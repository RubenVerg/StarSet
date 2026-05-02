{-# LANGUAGE OverloadedLists #-}

module StarSet.Play
  ( PlayResult(..)
  , play
  , hint
  ) where

import StarSet.Game

import Numeric.Natural
import Data.List
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set
import Combinatorics (tuples)

data PlayResult g
  = None
  | NoMoreSets (Set Achievement)
  | FoundSet (Set Achievement) [Card g]
  | Redealt [Card g]
  | AddedMore Natural

subIn :: Eq a => a -> [a] -> [a] -> ([a], [a])
subIn x xs [] = (filter (/= x) xs, [])
subIn x xs (z:zs) = (map (\x' -> if x == x' then z else x') xs, zs)

play :: Game g => g -> Natural -> [Card g] -> [Card g] -> PlayResult g
play g s d sel =
  if genericLength sel >= minimumSet g && maybe True (genericLength sel <=) (maximumSet g) && isSet g (makeSet g $ Set.fromList sel) then let
    (sa, sb) = foldr (\x (xs, zs) -> subIn x xs zs) (genericTake s d, genericDrop s d) sel
    achs = Set.insert FindSet $ Set.map (Specific . S g) $ setAchievements g (makeSet g $ Set.fromList sel)
    in FoundSet achs $ sa ++ sb
  else let
    setPossible = any (isSet g . makeSet g . Set.fromList) $ [minimumSet g..fromMaybe s (maximumSet g)] >>= (flip tuples (genericTake s d) . fromIntegral)
    anySetPossible = any (isSet g . makeSet g . Set.fromList) $ [minimumSet g..fromMaybe s (maximumSet g)] >>= (flip tuples d . fromIntegral)
  in if not setPossible && anySetPossible then case noSetAction g of
    Redeal -> Redealt $ genericDrop s d ++ genericTake s d
    AddMore n -> AddedMore n
  else if not anySetPossible then NoMoreSets $ case completeAchievement g of
    Nothing -> [CompleteGame]
    Just a -> [CompleteGame, Specific $ S g a]
  else None

hint :: Game g => g -> [Card g] -> Maybe [Card g]
hint g shown = find (isSet g . makeSet g . Set.fromList) $ [minimumSet g..fromMaybe (genericLength shown) (maximumSet g)] >>= (flip tuples shown . fromIntegral)
