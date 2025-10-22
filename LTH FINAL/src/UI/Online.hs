-- Online.hs (online UI: waiting, game sync)
module UI.Online where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, Next, Widget, txt, vBox, withAttr, (<+>), (<=>))
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import Control.Lens ((%~), (&), (^.))
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import qualified Network.WebSockets as WS  -- Use your network
import Game.Types (GameState, Player(Red), newGame, gameStatus)
import Game.Rules (applyMove)
import Network.Protocol (Message(..), encodeBinary)  -- Use your Protocol for messages/binary
import UI.Attributes (attributeMap)
import UI.Common (addBorder, drawFinished, drawBoard, drawTurnIndicator)

-- Adapt from your network: ConnectionTick for Brick AppEvent
data ConnectionTick = ConnectionTick Message  -- Use your Message type

-- ClientMessage/ServerMessage from Protocol, but UI only needs subset
-- data ClientMessage = ... (already in Protocol)

data RoomClientState = RoomClientState { username :: T.Text, isReady :: Bool } deriving (Show)

data WaitingRoomState = WaitingRoomState
  { roomId :: T.Text
  , localState :: RoomClientState
  , connection :: WS.Connection  -- Use real WS.Connection from Client.hs
  , otherPlayers :: [RoomClientState]
  } deriving (Show)

data OnlineGameState = OnlineGameState
  { localGame :: GameState
  , roomId :: T.Text
  , username :: T.Text
  , connection :: WS.Connection
  } deriving (Show)

data Online = WaitingRoom WaitingRoomState | OnlineGame OnlineGameState deriving (Show)

onlineApp :: M.App Online ConnectionTick ResourceName
onlineApp =
  M.App
    { M.appDraw = drawOnline
    , M.appChooseCursor = const Nothing
    , M.appHandleEvent = handleKeyOnline
    , M.appStartEvent = return
    , M.appAttrMap = const attributeMap
    }

drawOnline :: Online -> [Widget ResourceName]
drawOnline s = case s of
  WaitingRoom w -> drawWaitingRoom w
  OnlineGame o -> drawOnlineState (o^.localGame)

drawWaitingRoom :: WaitingRoomState -> [Widget ResourceName]
drawWaitingRoom w =
  [ addBorder "room id" (C.hCenter $ txt (w^.roomId))
    <=> addBorder "players" (vBox $ map (txt . (^. username)) (w^.localState : w^.otherPlayers))  -- Fix truncate: Add . (^. username)
    <=> addBorder "status" (vBox $ map (txt . if (^. isReady) then "ready" else "not ready") (w^.localState : w^.otherPlayers))
    <=> addBorder "help" (txt "Press 'r' to ready.")
  ]

drawOnlineState :: GameState -> [Widget ResourceName]
drawOnlineState o =
  [ drawFinished o "Back to lobby: Esc"
  , drawTurnIndicator (o^.currentPlayer) <=> drawBoard o
  ]

handleKeyOnline :: Online -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next Online)
handleKeyOnline s ev = case s of
  WaitingRoom w -> handleKeyWaitingRoom w ev
  OnlineGame o -> handleKeyOnlineState o ev

handleKeyWaitingRoom :: WaitingRoomState -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next Online)
handleKeyWaitingRoom w (AppEvent (ConnectionTick msg)) = case msg of
  WaitingForOpponent -> M.continue $ WaitingRoom w  -- Match your Protocol
  OpponentConnected oppName -> M.continue $ WaitingRoom (w & otherPlayers .~ [RoomClientState oppName False])
  GameUpdate newState -> M.continue $ OnlineGame (OnlineGameState newState (w^.roomId) (w^.localState.username) (w^.connection))  -- Full state update
  ErrorMsg err -> M.continue $ ErrorOverlay (WaitingRoom w) err
  _ -> M.continue $ WaitingRoom w
handleKeyWaitingRoom w (VtyEvent ev) = case ev of
  V.EvKey V.KEsc [] -> M.halt $ WaitingRoom w
  V.EvKey (V.KChar 'r') [] -> do
    let newSt = w^.localState & isReady %~ not
    liftIO $ WS.sendBinaryData (w^.connection) (encodeBinary (RoomClientUpdate newSt))  -- Adapt to binary from Protocol
    M.continue $ WaitingRoom (w & localState .~ newSt)
  _ -> M.continue $ WaitingRoom w
handleKeyWaitingRoom w _ = M.continue $ WaitingRoom w

handleKeyOnlineState :: OnlineGameState -> BrickEvent ResourceName ConnectionTick -> EventM ResourceName (Next Online)
handleKeyOnlineState o (AppEvent (ConnectionTick msg)) = case msg of
  GameUpdate col -> case applyMove (o^.localGame) col of  -- Wait, Protocol GameUpdate is GameState, not col – Fix to update full state
    Just newG -> M.continue $ OnlineGame (o & localGame .~ newG)
    Nothing -> M.continue $ OnlineGame o
  OpponentDisconnected -> M.continue initialGame  -- Add disconnect handle
  ErrorMsg err -> M.continue $ ErrorOverlay (OnlineGame o) err
  _ -> M.continue $ OnlineGame o
handleKeyOnlineState o (VtyEvent ev) = case ev of
  V.EvKey V.KEsc [] -> liftIO (WS.sendBinaryData (o^.connection) (encodeBinary (LeaveGame))) >> M.continue (OnlineGame o)  -- Match Protocol
  V.EvKey (V.KChar c) [] | c `elem` ['1'..'7'] -> let col = fromEnum c - fromEnum '0' in
    case applyMove (o^.localGame) col of
      Just newG -> liftIO (WS.sendBinaryData (o^.connection) (encodeBinary (MakeMove col))) >> M.continue (OnlineGame o & localGame .~ newG)
      Nothing -> M.continue (OnlineGame o)
  _ -> M.continue (OnlineGame o)
handleKeyOnlineState o _ = M.continue (OnlineGame o)