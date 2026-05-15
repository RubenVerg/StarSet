{-# LANGUAGE OverloadedStrings, ViewPatterns, OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import StarSet.Game
import StarSet.Games
import StarSet.Play
import StarSet.Message

import Numeric.Natural
import Control.Exception (catch)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.List
import Data.Maybe

import Servant
import Servant.API.WebSocket
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip
import Network.WebSockets hiding (send)
import Network.WebSockets.Connection (pingThread)
import Network.Wai.Application.Static
import WaiAppStatic.Types
import qualified Data.Text.Encoding as T
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random.MWC
import System.Random.MWC.Distributions
import Data.NanoID
import qualified Data.Vector as V
import Miso.JSON
import Miso.String (toMisoString)
import Data.Time.Clock.POSIX

deriving instance Ord NanoID

instance FromHttpApiData NanoID where
  parseUrlPiece = Right . NanoID . T.encodeUtf8

data ConnInfo = ConnInfo
  { connConnection :: Connection
  , connPoints :: Natural
  , connName :: String
  }

data GameState g = GameState
  { stateGame :: g
  , stateDeck :: [Card g]
  , stateShowing :: Natural
  , stateStart :: Double
  , stateConnections :: Map NanoID ConnInfo
  }

data SomeGameState where SomeGameState :: Game g => GameState g -> SomeGameState

data ServerState = ServerState
  { serverRandom :: GenIO
  , serverGames :: TVar (Map NanoID SomeGameState)
  }

type AppM = ReaderT ServerState Handler

type WebSocketApi = "ws" :> "host" :> Capture "game" String :> QueryParam' '[Required, Strict] "name" String :> WebSocket
               :<|> "ws" :> "join" :> Capture "id" NanoID :> QueryParam' '[Required, Strict] "name" String :> WebSocket

hostServer :: ServerT ("ws" :> "host" :> Capture "game" String :> QueryParam' '[Required, Strict] "name" String :> WebSocket) AppM
hostServer game nam conn = case lookup game games of
  Just (SomeGame @g g) -> do
    ServerState{ serverRandom = r, serverGames = gs } <- ask
    liftIO $ void $ forkIO $ pingThread conn 10 (pure ())
    i <- liftIO $ customNanoID defaultAlphabet 12 r
    d <- liftIO $ V.toList <$> uniformShuffle (V.fromList $ Set.toList $ deck g) r
    n <- liftIO $ (* 1000) . realToFrac <$> getPOSIXTime
    ci <- liftIO $ customNanoID defaultAlphabet 12 r
    let s = SomeGameState $ GameState g d (laidDown g) n $ Map.fromList [(ci, ConnInfo conn 0 nam)]
    liftIO $ atomically $ readTVar gs >>= writeTVar gs . Map.insert i s
    send conn $ S2CAchieve @g [HostGame]
    gameServer i ci conn
  Nothing -> pure ()

joinServer :: ServerT ("ws" :> "join" :> Capture "id" NanoID :> QueryParam' '[Required, Strict] "name" String :> WebSocket) AppM
joinServer i nam conn = do
  ServerState{ serverRandom = r, serverGames = gs } <- ask
  liftIO $ void $ forkIO $ pingThread conn 10 (pure ())
  ci <- liftIO $ customNanoID defaultAlphabet 12 r
  liftIO $ atomically $ readTVar gs >>= writeTVar gs . Map.update (\(SomeGameState g) -> Just $ SomeGameState g{ stateConnections = Map.insert ci (ConnInfo conn 0 nam) $ stateConnections g }) i
  sg <- liftIO $ Map.lookup i <$> readTVarIO gs
  case sg of
    Just (SomeGameState @g _) -> send conn $ S2CAchieve @g [ConnectToGame]
    _ -> pure ()
  gameServer i ci conn

send :: (MonadIO m, Game g) => Connection -> S2CMessage g -> m ()
send conn msg = liftIO $ sendTextData conn (encode msg) `catch` (\(_ :: ConnectionException) -> pure ())

gameServer :: NanoID -> NanoID -> Connection -> AppM ()
gameServer i@(NanoID (T.decodeUtf8 -> is)) ci conn = do
  do
    gs <- asks serverGames
    sg <- liftIO $ Map.lookup i <$> readTVarIO gs
    case sg of
      Nothing -> pure ()
      Just (SomeGameState @g GameState{ stateStart = n, stateConnections = conns }) -> forM_ (map connConnection $ Map.elems conns) $ flip send $ S2CInfo @g n is $ fromIntegral $ Map.size conns
  forever $ do
    gs <- asks serverGames
    dt <- liftIO $ receiveData conn
    sg <- liftIO $ Map.lookup i <$> readTVarIO gs
    case sg of
      Nothing -> fail $ "Unknown game! " <> show i
      Just (SomeGameState @g gt@GameState{ stateDeck = d, stateStart = n, stateShowing = s, stateConnections = conns }) -> do
        let msg = decode @(C2SMessage g) dt
        case msg of
          Just (C2SSelection sel) -> do
            gt' <- checkForSet ci gt sel
            liftIO $ atomically $ readTVar gs >>= writeTVar gs . Map.insert i (SomeGameState gt')
            forM_ (map connConnection $ Map.elems $ stateConnections gt') $ flip send $ S2CSetDeck @g (stateShowing gt') (stateDeck gt')
          Just C2SGetDeck -> send conn $ S2CSetDeck @g s d
          Just C2SGetInfo -> send conn $ S2CInfo @g n is $ fromIntegral $ Map.size conns
          Nothing -> pure ()

checkForSet :: forall g. Game g => NanoID -> GameState g -> [Card g] -> AppM (GameState g)
checkForSet ci gt@GameState{ stateGame = g, stateDeck = d, stateShowing = s, stateConnections = conns, stateStart = st } sel = case play g s d sel of
  None -> pure gt
  NoMoreSets as -> do
    n <- liftIO $ (* 1000) . realToFrac <$> getPOSIXTime
    forM_ (Map.toList conns) $ \(ci1, ConnInfo conn yours _) -> do
      send conn $ S2CGameOver @g (n - st) yours (fmap (liftA2 (,) (toMisoString . connName) connPoints) $ Map.elems $ Map.delete ci1 conns)
      when (foldr (max . connPoints) 0 (Map.elems conns) == yours) $ send conn $ S2CAchieve @g [WinOnlineGame]
    forM_ (map connConnection $ Map.elems conns) $ flip send $ S2CAchieve @g as
    pure gt
  FoundSet as d' -> do
    let gt' = gt{ stateDeck = d', stateConnections = Map.update (\c@ConnInfo{ connPoints = p } -> Just c{ connPoints = p + 1 }) ci conns }
    send (connConnection $ fromJust $ Map.lookup ci conns) $ S2CAchieve @g as
    checkForSet ci gt' []
  Redealt d' -> do
    let gt' = gt{ stateDeck = d' }
    checkForSet ci gt' []
  AddedMore n -> do
    let gt' = gt{ stateShowing = s + n }
    checkForSet ci gt' []

socketServer :: ServerT WebSocketApi AppM
socketServer = hostServer :<|> joinServer

type Api = WebSocketApi
      :<|> "game-type" :> Capture "id" NanoID :> Get '[PlainText] String
      :<|> Raw

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server = socketServer
  :<|> (\i -> do
    sg <- asks serverGames
    gs <- liftIO $ readTVarIO sg
    case Map.lookup i gs of
      Nothing -> pure ""
      Just (SomeGameState (stateGame -> g)) -> case find (\(_, g') -> SomeGame g == g') games of
        Just (k, _) -> pure k
        _ -> pure "")
  :<|> serveDirectoryWith (defaultWebAppSettings "dist")
    { ssIndices = unsafeToPiece <$> ["index.html", "index.htm"]
    , ssMaxAge = NoCache
    , ss404Handler = Just $ \_ rs -> do
      rs $ responseFile status200 [("Content-Type", "text/html")] "dist/index.html" Nothing
    }

main :: IO ()
main = do
  random <- createSystemRandom
  gams <- newTVarIO Map.empty
  let st = ServerState random gams
  putStrLn "Serving on http://localhost:9050"
  run 9050 $ gzip defaultGzipSettings
    { gzipCheckMime = (== "application/wasm")
    , gzipFiles = GzipCompress } $ serve api $ hoistServer api (`runReaderT` st) server
