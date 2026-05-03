module StarSet.Games
  ( games
  , firstGame
  ) where

import StarSet.Game
import StarSet.Games.Set
import StarSet.Games.ProSet
import StarSet.Games.NearSet
import StarSet.Games.SuperSet
import StarSet.Games.Stun
import StarSet.Games.EvenQuads
import StarSet.Games.WreathSet

games :: [(String, SomeGame)]
games =
  [ firstGame
  , ("proSet", proSet 6)
  , ("proSet4", proSet 4)
  , ("7cardProSet", sevenCardProSet)
  , ("nearSet", nearSet)
  , ("numberNearSet", numberNearSet)
  , ("fillNearSet", fillNearSet)
  , ("shapeNearSet", shapeNearSet)
  , ("colorNearSet", colorNearSet)
  , ("superSet", superSet)
  , ("stun", stun)
  , ("evenQuads", evenQuads)
  , ("wreathSet", wreathSet)
  ]

firstGame :: (String, SomeGame)
firstGame = ("set", set)
