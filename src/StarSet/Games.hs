module StarSet.Games
  ( games
  ) where

import StarSet.Game
import StarSet.Games.Set
import StarSet.Games.ProSet
import StarSet.Games.NearSet

import Data.Map (Map)
import qualified Data.Map as Map

games :: Map String (String, SomeGame)
games = Map.fromList
  [ ("set", ("SET", set))
  , ("proSet", ("ProSet", proSet 6))
  , ("proSet4", ("Small ProSet", proSet 4))
  , ("7cardProSet", ("7-Card ProSet", sevenCardProSet))
  , ("nearSet", ("NearSet", nearSet))
  ]
