-- Offline.hs (offline UI: menu, practice vs AI)
module UI.Offline where

import Brick
  ( BrickEvent (VtyEvent), EventM, Next, Padding (Max), Widget, hLimitPercent, padBottom, txt, withAttr, (<+>), (<=>)
  )
import Brick.Forms
  ( Form, FormState(formState), handleFormEvent, newForm, renderForm, editTextField
  )
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Vector as V  -- For list
import qualified Graphics.Vty as V
import System.Random (randomRIO)
import Game.Types (GameState, Player(Red), newGame)
import Game.Board (isValidMove, getValidColumns)  -- For AI
import Game.Rules (applyMove, gameStatus)
import Game.AI (getAIMove, mediumAI, hardAI, easyAI)  -- Integrate your AI.hs
import UI.Attributes (attributeMap, errorAttr)
import UI.Common (addBorder, drawFinished, drawBoard, drawTurnIndicator, titleWidget, listDrawElement)
import Network.Client (runClient)  -- Your Client.hs for online transition
import Storage.GameState (saveGame, loadGame, quickSave, quickLoad)  -- Integrate storage for save/load
import Storage.Statistics (updateProfileDB, getProfile, PlayerProfile)  -- For stats update

-- Difficulty enum from your AI
data Difficulty = Easy | Medium | Hard deriving (Enum, Show)

-- ResourceName type (generic for Brick)
type ResourceName = ()

-- MenuList using Brick List
type MenuList = L.List ResourceName T.Text

-- RoomForm for create/join (adapt from Thock, use T.Text for username/roomId match your Protocol)
type RoomForm a = Form a () ResourceName
data RoomFormData = RoomFormData { username :: T.Text, roomId :: T.Text } deriving (Show)
data RoomInitData = RoomInitData { uname :: T.Text } deriving (Show)

-- Game variants (add SaveLoad/Stats for storage link)
data Game = 
    MainMenu MenuList
  | PracticeSelectDiff MenuList
  | OnlineSelect MenuList
  | CreateRoomMenu (RoomForm RoomInitData)
  | JoinRoomMenu (RoomForm RoomFormData)
  | Practice (GameState, Difficulty)
  | SaveLoadMenu MenuList  -- New: Menu for save/load/stats
  | ErrorOverlay Game T.Text
  deriving (Show)

initialGame :: Game
initialGame = MainMenu (L.list () (V.fromList ["Practice", "Online", "Save/Load/Stats"]) 1)  -- Add save option

localApp :: M.App Game () ResourceName
localApp =
  M.App
    { M.appDraw = drawGame
    , M.appChooseCursor = const Nothing
    , M.appHandleEvent = handleKeyGame
    , M.appStartEvent = return
    , M.appAttrMap = const attributeMap
    }

drawGame :: Game -> [Widget ResourceName]
drawGame g = case g of
  MainMenu l -> drawList l
  PracticeSelectDiff l -> drawList l
  OnlineSelect l -> drawList l
  CreateRoomMenu form -> drawForm form
  JoinRoomMenu form -> drawForm form
  Practice (gs, _) -> drawPractice gs
  SaveLoadMenu l -> drawList l  -- New menu draw
  ErrorOverlay prev t -> drawError prev t

drawList :: MenuList -> [Widget ResourceName]
drawList l = [drawMenu $ L.renderList listDrawElement True l]

drawForm :: RoomForm a -> [Widget ResourceName]
drawForm form = [drawMenu $ hLimitPercent 80 $ renderForm form]

drawMenu :: Widget ResourceName -> Widget ResourceName
drawMenu w = addBorder "" (C.center titleWidget <=> padBottom Max (C.hCenter w))

drawPractice :: GameState -> [Widget ResourceName]
drawPractice gs =
  [ drawFinished gs "Back: Esc | New Game: ^n | Save: ^s"
  , drawTurnIndicator (gs^.currentPlayer) <=> drawBoard gs
  ]

drawError :: Game -> T.Text -> [Widget ResourceName]
drawError g t = errorPopup : drawGame g
  where errorPopup = C.centerLayer $ addBorder "error" $ withAttr errorAttr (txt t)

handleKeyGame :: Game -> BrickEvent ResourceName () -> EventM ResourceName (Next Game)
handleKeyGame gs ev = case gs of
  MainMenu l -> handleKeyMainMenu l ev
  PracticeSelectDiff l -> handleKeyPracDiff l ev
  OnlineSelect l -> handleKeyOnlineSelect l ev
  CreateRoomMenu form -> handleKeyForm CreateRoomMenu cr (^. uname) form ev
  JoinRoomMenu form -> handleKeyForm JoinRoomMenu jr (^. username) form ev
  Practice (g, diff) -> handleKeyPractice g diff ev
  SaveLoadMenu l -> handleKeySaveLoad l ev  -- New handler
  ErrorOverlay prev _ -> M.continue prev

handleKeyMainMenu :: MenuList -> BrickEvent ResourceName e -> EventM ResourceName (Next Game)
handleKeyMainMenu l (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.halt (MainMenu l)
  V.EvKey V.KEnter [] | Just i <- L.listSelected l -> case i of
    0 -> M.continue $ PracticeSelectDiff (L.list () (V.fromList ["Easy", "Medium", "Hard"]) 1)
    1 -> M.continue $ OnlineSelect (L.list () (V.fromList ["Create Room", "Join Room"]) 1)
    2 -> M.continue $ SaveLoadMenu (L.list () (V.fromList ["Quick Save", "Quick Load", "View Stats"]) 1)  -- New: Link storage
  ev' -> M.continue . MainMenu =<< L.handleListEvent ev' l
handleKeyMainMenu l _ = M.continue (MainMenu l)

handleKeyPracDiff :: MenuList -> BrickEvent ResourceName e -> EventM ResourceName (Next Game)
handleKeyPracDiff l (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.continue initialGame
  V.EvKey V.KEnter [] | Just i <- L.listSelected l -> do
    let diff = toEnum i :: Difficulty
    let gs = newGame Red
    M.continue $ Practice (gs, diff)
  ev' -> M.continue . PracticeSelectDiff =<< L.handleListEvent ev' l
handleKeyPracDiff l _ = M.continue (PracticeSelectDiff l)

handleKeyOnlineSelect :: MenuList -> BrickEvent ResourceName e -> EventM ResourceName (Next Game)
handleKeyOnlineSelect l (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.continue initialGame
  V.EvKey V.KEnter [] | Just i <- L.listSelected l -> case i of
    0 -> M.continue $ CreateRoomMenu (makeCreateRoomForm (RoomInitData ""))
    _ -> M.continue $ JoinRoomMenu (makeJoinRoomForm (RoomFormData "" ""))
  ev' -> M.continue . OnlineSelect =<< L.handleListEvent ev' l
handleKeyOnlineSelect l _ = M.continue (OnlineSelect l)

handleKeyForm :: (RoomForm a -> Game) -> (a -> IO (Maybe T.Text)) -> (a -> T.Text) -> RoomForm a -> BrickEvent ResourceName () -> EventM ResourceName (Next Game)
handleKeyForm ctr onEnter getUser form ev@(VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.continue initialGame
  V.EvKey V.KEnter [] ->
    if T.null (getUser (formState form))
      then M.continue (ErrorOverlay (ctr form) "username cannot be empty")
      else M.suspendAndResume $ maybe initialGame (ErrorOverlay (ctr form)) <$> onEnter (formState form)
  _ -> handleFormEvent ev form >>= M.continue . ctr
handleKeyForm ctr _ _ form _ = M.continue (ctr form)

makeCreateRoomForm :: RoomInitData -> RoomForm RoomInitData
makeCreateRoomForm = newForm [ editTextField uname "UsernameField" (Just 1) ]

makeJoinRoomForm :: RoomFormData -> RoomForm RoomFormData
makeJoinRoomForm = newForm 
  [ editTextField username "UsernameField" (Just 1)
  , editTextField roomId "RoomIdField" (Just 1)
  ]

-- cr/jr use your runClient (adapt to take T.Text username/roomId, match Protocol JoinGame)
cr :: RoomInitData -> IO (Maybe T.Text)
cr data = runClient (data^.uname)  -- Your Client.hs

jr :: RoomFormData -> IO (Maybe T.Text)
jr data = runClient (data^.username)  -- Assume roomId for join, but your runClient takes playerName only – Adapt if need roomId

handleKeyPractice :: GameState -> Difficulty -> BrickEvent ResourceName e -> EventM ResourceName (Next Game)
handleKeyPractice g diff (VtyEvent ev) = case ev of
  V.EvKey V.KEsc [] -> M.continue initialGame
  V.EvKey (V.KChar 'n') [V.MCtrl] -> M.continue $ Practice (newGame Red, diff)
  V.EvKey (V.KChar 's') [V.MCtrl] -> liftIO (quickSave g) >> M.continue (Practice (g, diff))  -- New: Link storage quickSave
  V.EvKey (V.KChar c) [] | c `elem` ['1'..'7'] -> let col = fromEnum c - fromEnum '0' in do  -- Fix '1'-'7' to 0-6
    case applyMove g col of
      Just newG -> do
        if newG^.gameStatus /= InProgress then liftIO (updateStats newG)  -- New: Update stats on end
        else return ()
        if newG^.currentPlayer == Black && newG^.gameStatus == InProgress
          then do aiCol <- liftIO $ aiMove diff newG
                  case applyMove newG aiCol of
                    Just finalG -> M.continue $ Practice (finalG, diff)
                    Nothing -> M.continue $ Practice (newG, diff)
          else M.continue $ Practice (newG, diff)
      Nothing -> M.continue $ Practice (g, diff)
  _ -> M.continue $ Practice (g, diff)
handleKeyPractice g diff _ = M.continue $ Practice (g, diff)

-- Update aiMove to use your AI.hs getAIMove with config
aiMove :: Difficulty -> GameState -> IO Int
aiMove diff gs = getAIMove config (gs^.board) (gs^.currentPlayer)
  where config = case diff of  -- Map to your AI configs
          Easy -> easyAI
          Medium -> mediumAI
          Hard -> hardAI

-- New: Update stats after game (link storage)
updateStats :: GameState -> IO ()
updateStats gs = do
  profile <- getProfile "Player"  -- Assume username, adapt for multi-player
  updateProfileDB (updateProfile profile gs)  -- Your update logic

-- New handler for SaveLoadMenu (link storage)
handleKeySaveLoad :: MenuList -> BrickEvent ResourceName e -> EventM ResourceName (Next Game)
handleKeySaveLoad l (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> M.continue initialGame
  V.EvKey V.KEnter [] | Just i <- L.listSelected l -> case i of
    0 -> liftIO (quickSave newGame Red) >> M.continue initialGame  -- Quick save current (adapt to current gs)
    1 -> do mGs <- liftIO quickLoad
            case mGs of
              Just gs -> M.continue $ Practice (gs, Medium)  -- Load to practice
              Nothing -> M.continue (ErrorOverlay initialGame "Load failed")
    2 -> liftIO printStats >> M.continue initialGame  -- View stats (impl below)
  ev' -> M.continue . SaveLoadMenu =<< L.handleListEvent ev' l
handleKeySaveLoad l _ = M.continue (SaveLoadMenu l)

-- Helper: Print stats/leaderboard (link Statistics.hs)
printStats :: IO ()
printStats = do
  db <- loadProfiles
  print $ generateLeaderboard (Map.elems db)  -- Your function