{-# LANGUAGE CPP, OverloadedStrings, RecursiveDo, ViewPatterns, DeriveAnyClass, AllowAmbiguousTypes #-}
{- HLINT ignore "Use ++" -}

module Main (main) where

import StarSet.Game
import StarSet.Games
import StarSet.Play
import StarSet.Message

import Data.List
import Control.Monad
import Numeric.Natural
import Data.Maybe
import Data.IORef
import Type.Reflection
import GHC.Generics
import Data.Type.Bool
import Data.Ord

import Miso hiding (set)
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Miso.CSS as C
import qualified Miso.Date as Date
import Miso.JSON hiding (Config)
import Miso.Subscription.Util
import Miso.WebSocket
import Miso.Router
import qualified Miso.DSL as FFI
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (uniformShuffleList, mkStdGen)
import Data.Time.Clock

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

type Code online = If online (Maybe MisoString) ()
type Score online = If online (Natural, [Natural]) ()
type Players online = If online Natural ()

data GameState (online :: Bool) g
  = Playing
    { stateGame :: g
    , stateDeck :: [Card g]
    , stateShowing :: Natural
    , stateSelected :: [Card g]
    , stateStart :: Double
    , stateCode :: Code online
    , stateConfig :: Config
    , stateShowingRules :: Bool
    , statePlayers :: Players online
    }
  | Finished
    { stateGame :: g
    , stateDeck :: [Card g]
    , stateTime :: DiffTime
    , stateCode :: Code online
    , stateConfig :: Config
    , stateScore :: Score online
    , stateShowingRules :: Bool
    , statePlayers :: Players online
    }

deriving instance Game g => Eq (GameState True g)
deriving instance Game g => Eq (GameState False g)

data GameEvent (online :: Bool) g where
  Startup :: GameEvent online g
  Click :: Card g -> GameEvent online g
  Finish :: [Card g] -> DiffTime -> GameEvent False g
  GotS2C :: S2CMessage g -> GameEvent True g
  ShowRules :: GameEvent online g
  HideRules :: GameEvent online g
  Achieve :: Set Achievement -> GameEvent online g

deriving instance Game g => Show (GameEvent online g)

styleSheet :: C.StyleSheet
styleSheet = C.sheet_
  [ C.selector_ ".__card"
    [ C.height $ C.pct 100
    , C.aspectRatio "5/7"
    , C.border "2px solid black"
    , C.borderRadius "5px"
    , C.backgroundColor C.white
    , C.minWidth $ C.pct 0
    , C.minHeight $ C.pct 0
    ]
  , C.selector_ ".__card.__selected"
    [ C.boxShadow "inset 0 0 15px 15px #94e04444"
    ]
  , C.selector_ ".__card.__hint"
    [ C.borderColor C.cyan
    ]
  , C.selector_ ".__cards"
    [ C.display "flex"
    , C.alignItems "center"
    , C.justifyContent "center"
    , C.width "calc(100% - 2rem)"
    , "margin-inline" =: "auto"
    , C.height "calc(100% - 6rem)"
    , C.maxWidth $ C.pct 100
    , C.maxHeight $ C.pct 100
    ]
  , C.selector_ ".__cards > div"
    [ C.display "grid"
    , C.gridTemplateRows "repeat(3, 1fr)"
    , C.gridTemplateColumns "auto"
    , C.height $ C.pct 100
    , C.gap $ C.em 1
    , C.gridAutoFlow "column"
    , C.justifyContent "center"
    , C.alignItems "center"
    ]
  , C.media_ "(max-aspect-ratio: 1/1)" 
    [ ".__cards > div" =:
      [ C.gridTemplateRows "auto"
      , C.gridTemplateColumns "repeat(3, 1fr)"
      , C.gridAutoFlow "row"
      , C.height "auto"
      , C.width $ C.pct 100
      ]
    , ".__card" =:
      [ C.height "auto"
      , C.width $ C.pct 100
      ]
    ]
  , C.selector_ ".__finished, .__rules"
    [ C.position "absolute"
    , C.top "0"
    , C.bottom "0"
    , C.left "0"
    , C.right "0"
    , C.zIndex "1000000"
    ]
  , C.selector_ ".__cover"
    [ C.width $ C.vw 100
    , C.height $ C.vh 100
    , C.position "fixed"
    , C.top "0"
    , C.left "0"
    , C.background "#777a"
    ]
  , C.selector_ "html, body, .__container"
    [ C.height $ C.pct 100
    ]
  , C.selector_ ".__header"
    [ C.height $ C.rem 3
    , C.background "#f8f8f8"
    , "padding-inline" =: C.rem 1.5
    , "padding-block" =: C.rem 0.5
    , C.boxSizing "border-box"
    , C.alignContent "center"
    , C.marginBottom $ C.rem 1
    , C.textAlign "center"
    ]
  , C.selector_ ".__header > :nth-child(1)"
    [ C.display "inline-block"
    , C.width "calc(100% / 3)"
    , C.textAlign "left"
    ]
  , C.selector_ ".__header > :nth-child(2)"
    [ C.display "inline-block"
    , C.width "calc(100% / 3)"
    , C.textAlign "center"
    ]
  , C.selector_ ".__header > :nth-child(3)"
    [ C.display "inline-block"
    , C.width "calc(100% / 3)"
    , C.textAlign "right"
    ]
  , C.selector_ ".__achievements .__got"
    [ C.color C.green
    ]
  , C.selector_ "body"
    [ C.margin "0"
    ]
  , C.selector_ ":root, button, input, select"
    [ C.fontFamily "'Fira Sans', sans-serif"
    , C.fontWeight "400"
    ]
  , C.selector_ "code, pre"
    [ C.fontFamily "'Fira Mono', monospace"
    ]
  ]

checkForSet :: Game g => Effect parent (GameState False g) (GameEvent False g)
checkForSet = do
  state <- get
  case state of
    Playing{ stateGame = g, stateDeck = d, stateShowing = s, stateSelected = sel, stateStart = st, stateCode = cd, stateConfig = cfg, stateShowingRules = sr, statePlayers = pl } -> case play g s d sel of
      None -> pure ()
      NoMoreSets as -> do
        issue $ Achieve as
        sync $ do
          end <- Date.new >>= Date.getTime
          pure $ Finish d $ realToFrac $ (end - st) / 1000
      FoundSet as d' -> do
        put Playing{ stateGame = g, stateDeck = d', stateShowing = s, stateSelected = [], stateStart = st, stateCode = cd, stateConfig = cfg, stateShowingRules = sr, statePlayers = pl } >> checkForSet
        issue $ Achieve as
      Redealt d' -> put Playing{ stateGame = g, stateDeck = d', stateShowing = s, stateSelected = [], stateStart = st, stateCode = cd, stateConfig = cfg, stateShowingRules = sr, statePlayers = pl } >> checkForSet
      AddedMore n -> put Playing{ stateGame = g, stateDeck = d, stateShowing = s + n, stateSelected = sel, stateStart = st, stateCode = cd, stateConfig = cfg, stateShowingRules = sr, statePlayers = pl } >> checkForSet
    _ -> pure ()

class Game g => OnlineSwitches (online :: Bool) g where
  handleStartup :: Effect parent (GameState online g) (GameEvent online g)
  makeMailbox :: Value -> Maybe (GameEvent online g)
  changeSelected :: [Card g] -> Effect parent (GameState online g) (GameEvent online g)
  wrapCode :: MisoString -> Code online
  nullCode :: Code online
  displayOnline :: Code online -> Players online -> [View model action]
  displayScore :: Score online -> View model action
  onePlayer :: Players online

instance Game g => OnlineSwitches True g where
  handleStartup = do
    broadcast $ sendC2SRequest (C2SGetDeck :: C2SMessage g)
    broadcast $ sendC2SRequest (C2SGetInfo :: C2SMessage g)
  makeMailbox v = case fromJSON v of
    Success s2c -> Just $ GotS2C s2c
    _ -> Nothing
  changeSelected sel = broadcast $ sendC2SRequest $ C2SSelection @g sel
  wrapCode = Just
  nullCode = Nothing
  displayOnline Nothing _ = []
  displayOnline (Just code) players =
    [ icon "people"
    , text " "
    , text $ toMisoString $ show players
    , text " "
    , icon "code-slash"
    , text " "
    , H.code_ [] [text code]
    ]
  displayScore (y, o) = H.div_ []
    [ H.span_ [] [text "Your score: ", text $ toMisoString $ show y]
    , H.div_ []
      [ H.span_ [] [text "Other scores: "]
      , H.ul_ [] $ H.li_ [] . pure . text . toMisoString . show <$> o
      ]
    ]
  onePlayer = 1

instance Game g => OnlineSwitches False g where
  handleStartup = checkForSet
  makeMailbox _ = Nothing
  changeSelected _ = checkForSet
  wrapCode = const ()
  nullCode = ()
  displayOnline = const $ const []
  displayScore = const $ H.span_ [] []
  onePlayer = ()

gameHandle :: forall online g parent. (OnlineSwitches online g, Game g) => GameEvent online g -> Effect parent (GameState online g) (GameEvent online g)
gameHandle Startup = handleStartup
gameHandle (Click card) = do
  gs <- get
  case gs of
    Playing{ stateGame = g, stateSelected = sel } -> do
      let mx = maximumSet g
      let sel' = if card `elem` sel then sel \\ [card] else maybe sel (\m -> if genericLength sel == m then drop 1 sel else sel) mx ++ [card]
      put gs{ stateSelected = sel' }
      changeSelected sel'
    _ -> pure ()
gameHandle (Finish d time) = modify $ \s -> Finished{ stateGame = stateGame s, stateDeck = d, stateTime = time, stateCode = stateCode s, stateConfig = stateConfig s, stateScore = (), stateShowingRules = stateShowingRules s, statePlayers = statePlayers s }
gameHandle (GotS2C (S2CSetDeck s' d')) = modify $ \case
  st@Playing{ stateSelected = sel } -> st{ stateDeck = d', stateShowing = s', stateSelected = filter (`elem` genericTake s' d') sel }
  st -> st{ stateDeck = d' }
gameHandle (GotS2C (S2CInfo start code players)) = modify $ \case
  s@Playing{} -> s{ stateStart = start, stateCode = wrapCode @online @g code, statePlayers = players }
  s -> s{ stateCode = wrapCode @online @g code, statePlayers = players }
gameHandle (GotS2C (S2CGameOver t y o)) = modify $ \s -> Finished{ stateGame = stateGame s, stateDeck = stateDeck s, stateTime = realToFrac $ t / 1000, stateCode = stateCode s, stateConfig = stateConfig s, stateScore = (y, o), stateShowingRules = stateShowingRules s, statePlayers = statePlayers s }
gameHandle (GotS2C (S2CAchieve achs)) = issue $ Achieve achs
gameHandle (Achieve achs) = sync_ $ do
  s <- decode . fromMaybe "[]" <$> getLocalStorage "achievements"
  setLocalStorage "achievements" $ encode $ Set.toList $ Set.union achs $ Set.fromList $ fromMaybe [] s
gameHandle ShowRules = modify $ \s -> s{ stateShowingRules = True }
gameHandle HideRules = modify $ \s -> s{ stateShowingRules = False }

displayTime :: DiffTime -> View model action
displayTime (floor -> secs) = text $ toMisoString $ show (secs `div` 60) ++ ":" ++ reverse (take 2 $ reverse $ '0' : show (secs `mod` 60 :: Integer))

gameRender :: forall online g. (OnlineSwitches online g, Game g) => GameState online g -> View (GameState online g) (GameEvent online g)
gameRender Playing{ stateGame = g, stateDeck = d, stateShowing = s, stateSelected = selected, stateCode = cd, stateConfig = Config{ configHints = doHint }, stateShowingRules = sr, statePlayers = pl } = let
  sh = genericTake s d
  hinted = fromMaybe [] $ hint g $ genericTake s d
  rd = (\card -> H.div_ [H.onClick $ Click card, P.classes_ $ ["__card"] ++ ["__selected" | card `elem` selected] ++ ["__hint" | card `elem` hinted && doHint] ] [renderCard g card]) <$> sh
  in H.div_ [P.classes_ ["__container"]]
    [ H.header_ [P.classes_ ["__header"]]
      [ H.span_ []
        [ icon "stopwatch"
        , text " "
        , mount_ $ timerComponent displayTime stateStart
        , text " "
        , icon "files"
        , text " "
        , text $ toMisoString $ length d
        ]
      , H.span_ []
        [ H.strong_ [] [text $ name g]
        , text " "
        , H.span_ [H.onClick ShowRules] [icon "info-circle"]
        ]
      , H.span_ [] $ displayOnline @online @g cd pl
      ]
    , H.main_ [P.classes_ ["__cards"]] [H.div_ [] rd]
    , if sr then H.dialog_ [P.open_ True, P.classes_ ["__rules"]] [H.h3_ []
      [ text $ name g <> ": Rules"
      , H.span_ [H.onClick HideRules] [icon "x"]
      ], rules g] else text ""
    ]
gameRender Finished{ stateGame = g, stateDeck = d, stateTime = time, stateCode = cd, stateScore = sc, statePlayers = pl } = let
  rd = (\card -> H.div_ [H.onClick $ Click card, P.classes_ ["__card"]] [renderCard g card]) <$> d
  in H.div_ [P.classes_ ["__container"]]
    [ H.header_ [P.classes_ ["__header"]]
      [ H.span_ []
        [ icon "stopwatch"
        , text " "
        , mount_ $ timerComponent displayTime stateStart
        , text " "
        , icon "files"
        , text " "
        , text $ toMisoString $ length d
        ]
      , H.span_ []
        [ H.strong_ [] [text $ name g]
        , text " "
        , H.span_ [H.onClick ShowRules] [icon "info-circle"]
        ]
      , H.span_ [] $ displayOnline @online @g cd pl
      ]
    , H.main_ [P.classes_ ["__cards"]] [H.div_ [] rd]
    , H.div_ [P.classes_ ["__cover"]] []
    , H.dialog_ [P.open_ True, P.classes_ ["__finished"]]
      [ text "Completed in "
      , displayTime time
      , displayScore @online @g sc
      ]
    ]

gameComponent :: forall (online :: Bool) g parent. (OnlineSwitches online g, Game g) => Config -> Double -> g -> [Card g] -> Component parent (GameState online g) (GameEvent online g)
gameComponent cfg st g d = (component Playing{ stateGame = g, stateDeck = d, stateShowing = laidDown g, stateSelected = [], stateStart = st, stateCode = nullCode @online @g, stateConfig = cfg, stateShowingRules = False, statePlayers = onePlayer @online @g } gameHandle gameRender)
  { styles = StarSet.Game.styles g
  , mount = Just Startup
  , mailbox = makeMailbox @online @g
  }

animationFrameSub :: (Double -> action) -> Sub action
animationFrameSub f sink = createSub acquire release sink where
  acquire = mdo
    ref <- newIORef True
    cb <- fmap Function $ asyncCallback1 $ \_ -> do
      Date.new >>= Date.getTime >>= sink . f
      continue <- readIORef ref
      when continue $ void $ toJSVal cb >>= FFI.requestAnimationFrame
    _ <- toJSVal cb >>= FFI.requestAnimationFrame
    pure ref
  release v = writeIORef v False

timerComponent :: (forall model action. DiffTime -> View model action) -> (parent -> Double) -> Component parent (Double, Double) (Maybe (Either Double Double))
timerComponent r start = (component (0, 0) (\case
  Just (Left str) -> modify $ \(_, cur) -> (str,cur)
  Just (Right cur) -> modify (\(str, _) -> (str, cur)) >> parent (Just . Left . start) Nothing
  Nothing -> pure ()) (\(str, cur) -> r $ realToFrac $ (cur - str) / 1000)) { subs = [animationFrameSub $ Just . Right] }

main :: IO ()
main = miso defaultEvents $ \uri -> case route uri of
  Left _ -> app $ Index (QueryFlag False)
  Right r -> app r

data Route
  = Index (QueryFlag "hints")
  | Join (QueryFlag "hints") (QueryParam "code" MisoString)
  deriving (Show, Generic, Router)

newtype Config = Config { configHints :: Bool }
  deriving (Eq, Ord, Show)

data AppState where
  BeginState :: Config -> MisoString -> String -> AppState
  AchievementsState :: Config -> Set Achievement -> AppState
  LocalState :: Game g => Config -> Double -> g -> [Card g] -> AppState
  OnlineState :: Game g => Config -> WebSocket -> Double -> g -> [Card g] -> AppState

appConfig :: AppState -> Config
appConfig (BeginState c _ _) = c
appConfig (AchievementsState c _) = c
appConfig (LocalState c _ _ _) = c
appConfig (OnlineState c _ _ _ _) = c

instance Eq AppState where
  (BeginState a0 a1 a2) == (BeginState b0 b1 b2) = (a0, a1, a2) == (b0, b1, b2)
  (AchievementsState a0 a1) == (AchievementsState b0 b1) = (a0, a1) == (b0, b1)
  (LocalState @a a0 a1 a2 a3) == (LocalState @b b0 b1 b2 b3) = case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> (a0, a1, a2, a3) == (b0, b1, b2, b3)
    Nothing -> False
  (OnlineState @a a0 a1 a2 a3 a4) == (OnlineState @b b0 b1 b2 b3 b4) = case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> (a0, a1, a2, a3, a4) == (b0, b1, b2, b3, b4)
    Nothing -> False
  _ == _ = False

newtype SendC2SRequest = SendC2SRequest { c2sRequest :: Value }
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

sendC2SRequest :: ToJSON a => a -> SendC2SRequest
sendC2SRequest = SendC2SRequest . toJSON

data AppEvent where
  LocalBegin :: Game g => Double -> g -> [Card g] -> AppEvent
  Begin :: AppEvent
  OnlineBegin :: Game g => WebSocket -> g -> AppEvent
  Host :: AppEvent
  Connect :: AppEvent
  DoConnect :: MisoString -> String -> AppEvent
  SelectGame :: String -> AppEvent
  ChangeCode :: MisoString -> AppEvent
  OnlineOpen :: Game g => g -> WebSocket -> AppEvent
  OnlineClose :: Closed -> AppEvent
  OnlineMessage :: Game g => S2CMessage g -> AppEvent
  OnlineError :: MisoString -> AppEvent
  SendC2S :: SendC2SRequest -> AppEvent
  Achievements :: AppEvent
  DoAchievements :: Set Achievement -> AppEvent

handle :: AppEvent -> Effect parent AppState AppEvent
handle Begin = do
  s <- get
  case s of
    BeginState _ _ key -> case lookup key games of
      Nothing -> pure ()
      Just (SomeGame g) -> sync $ do
        n <- now
        let gen = mkStdGen $ floor n
        let (cs,_) = uniformShuffleList (Set.toList $ deck g) gen
        st <- Date.new >>= Date.getTime
        pure $ LocalBegin st g cs
    _ -> pure ()
handle (LocalBegin st g d) = modify $ \(appConfig -> c) -> LocalState c st g d
handle (OnlineBegin sock g) = modify $ \(appConfig -> c) -> OnlineState c sock 0 g []
handle Host = do
  s <- get
  case s of
    BeginState _ _ key -> case lookup key games of
      Just (SomeGame @g g) -> connectJSON @(S2CMessage g) ("/ws/host/" <> toMisoString key) (OnlineOpen g) OnlineClose (OnlineMessage @g) OnlineError
      Nothing -> pure ()
    _ -> pure ()
handle Connect = do
  st <- get
  case st of
    BeginState _ i _ -> getText ("/game-type/" <> i) [] (\(fromMisoString . body -> t) -> DoConnect i t) (OnlineError . body)
    _ -> pure ()
handle (DoConnect i key) = case lookup key games of
  Nothing -> pure ()
  Just (SomeGame @g g) -> connectJSON @(S2CMessage g) ("/ws/join/" <> i) (OnlineOpen g) OnlineClose (OnlineMessage @g) OnlineError
handle (SelectGame key) = modify $ \case
  BeginState c i _ -> BeginState c i key
  o -> o
handle (ChangeCode code) = modify $ \case
  BeginState c _ s -> BeginState c code s
  o -> o
handle (OnlineOpen g sock) = issue $ OnlineBegin sock g
handle (OnlineClose _) = modify $ \(appConfig -> c) -> BeginState c "" $ fst firstGame
handle (OnlineMessage msg) = broadcast msg
handle (OnlineError err) = sync_ $ print err
handle (SendC2S (SendC2SRequest c2s)) = do
  st <- get
  case st of
    OnlineState _ sock _ _ _ -> sendJSON sock c2s
    _ -> pure ()
handle Achievements = sync $ do
  s <- decode . fromMaybe "[]" <$> getLocalStorage "achievements"
  pure $ DoAchievements $ Set.fromList $ fromMaybe [] s
handle (DoAchievements achs) = modify $ \(appConfig -> c) -> AchievementsState c achs

render :: AppState -> View AppState AppEvent
render (BeginState _ code k) = H.div_ []
  [ H.div_ [] $ (\(key, SomeGame (name -> n)) -> H.div_ []
    [ H.input_ [P.type_ "radio", P.name_ $ toMisoString key, P.checked_ $ key == k, H.onClick $ SelectGame key]
    , H.label_ [P.for_ $ toMisoString key] [text $ toMisoString n]
    ]) <$> games
  , H.button_ [H.onClick Begin] [text "Local game"]
  , H.button_ [H.onClick Host] [text "New online game"]
  , H.input_ [H.onInput ChangeCode, P.value_ code]
  , H.button_ [H.onClick Connect] [text "Connect"]
  , H.button_ [H.onClick Achievements] [text "Achievements"]
  ]
render (AchievementsState _ achs) = H.div_ [] [H.dl_ [P.classes_ ["__achievements"]] $ concatMap (\ach ->
  H.dt_ [P.classes_ ["__got" | ach `Set.member` achs]]
    [ icon $ if ach `Set.member` achs then "check-square" else "square"
    , text " "
    , text $ name ach
    ] :
  (H.dd_ [] . pure . text <$> description ach)) $ sortOn (Down . (`Set.member` achs)) allAchievements]
render (LocalState c st g d) = mount_ $ gameComponent @False c st g d
render (OnlineState c _ st g d) = mount_ $ gameComponent @True c st g d

makeApp :: AppState -> App AppState AppEvent
makeApp state = (component state handle render)
  { styles =
    [ Href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.13.1/font/bootstrap-icons.min.css" False
    , Href "https://fonts.googleapis.com/css2?family=Fira+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap" False
    , Href "https://fonts.googleapis.com/css2?family=Fira+Mono:wght@400;500;700&family=Fira+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap" False
    , Sheet styleSheet
    ]
  , mailbox = \v -> case fromJSON v of
    Success c2s -> Just $ SendC2S c2s
    _ -> Nothing
  }

app :: Route -> App AppState AppEvent
app (Index (QueryFlag hints)) = makeApp $ BeginState (Config hints) "" $ fst firstGame
app (Join (QueryFlag hints) (QueryParam Nothing)) = makeApp $ BeginState (Config hints) "" $ fst firstGame
app (Join (QueryFlag hints) (QueryParam (Just code))) = (makeApp $ BeginState (Config hints) code $ fst firstGame){ mount = Just Connect }

icon :: MisoString -> View model action
icon str = H.i_ [P.classes_ ["bi", "bi-" <> str]] []
