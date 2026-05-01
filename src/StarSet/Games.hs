module StarSet.Games
  ( games
  ) where

import StarSet.Game
import StarSet.Games.Set
import StarSet.Games.ProSet
import StarSet.Games.NearSet
import StarSet.Games.SuperSet

games :: [(String, SomeGame)]
games =
  [ ("set", set)
  , ("proSet", proSet 6)
  , ("proSet4", proSet 4)
  , ("7cardProSet", sevenCardProSet)
  , ("nearSet", nearSet)
  , ("numberNearSet", numberNearSet)
  , ("fillNearSet", fillNearSet)
  , ("shapeNearSet", shapeNearSet)
  , ("colorNearSet", colorNearSet)
  , ("superSet", superSet)
  ]
