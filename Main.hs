import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)

import System.IO (Handle, hClose)

import Data.Time.Clock

import qualified Data.ByteString as B (append, null)
import Data.ByteString.UTF8 (toString, fromString)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM

import Text.JSON

import Debug.Trace (trace)

main = withSocketsDo $ do
    socket <- listenOn (PortNumber 8080)
    putStrLn "Listening on port 8080."
    newTime <- getCurrentTime
    let player = Entity {
            identifier = "player",
            position = Vector 500 500,
            velocity = Vector 0 0,
            acceleration = Vector 0 0,
            angle = 0,
            rotation = 0
            }
    let newControls = Controls {
            upKey = False,
            downKey = False,
            leftKey = False,
            rightKey = False,
            shootKey = False
            }
    gameVariable <- newTVarIO Game {
        controls = newControls,
        oldControls = newControls,
        entities = Map.singleton "player" (player, player),
        changedEntities = Set.empty,
        oldTime = newTime
        }
    forkIO $ updateLoop gameVariable
    forever $ do
        (handle, _, _) <- accept socket
        forkIO (talkTo gameVariable handle)

talkTo :: TVar Game -> Handle -> IO ()
talkTo gameVariable handle = do
    request <- shakeHands handle
    case request of
        Left err -> print err
        Right _ -> do
            putStrLn "Shook hands with new client."
            let message = "[\"updateEntity\", \"player\", {\"position\": {\"x\": 500, \"y\": 500}}]"
            putFrame handle $ fromString message
            forkIO $ sendLoop gameVariable handle
            receiveLoop gameVariable handle

receiveLoop :: TVar Game -> Handle -> IO ()
receiveLoop gameVariable handle = do
    input <- getFrame handle
    if B.null input
        then do
            putStrLn "EOF encountered. Closing handle."
            hClose handle
        else do
            let Ok (JSArray (JSString kind : arguments)) = decode (toString input)
            case fromJSString kind of
                "key" -> do
                    let [JSString key, JSBool pressed] = arguments
                    case fromJSString key of
                        "up" -> change (\controls -> controls { upKey = pressed })
                        "down" -> change (\controls -> controls { downKey = pressed })
                        "left" -> change (\controls -> controls { leftKey = pressed })
                        "right" -> change (\controls -> controls { rightKey = pressed })
                        "shoot" -> change (\controls -> controls { shootKey = pressed })
            receiveLoop gameVariable handle
    where
        change f = atomically $ do
            game <- readTVar gameVariable
            writeTVar gameVariable game { controls = f (controls game) }
            
sendLoop :: TVar Game -> Handle -> IO ()
sendLoop gameVariable handle = do
    updates <- atomically $ do
        game <- readTVar gameVariable
        forM (Map.elems (entities game)) $ \(actual, observed) -> do
            let updates = [
                    ("position", position, \entity value -> entity { position = value }),
                    ("velocity", velocity, \entity value -> entity { velocity = value }),
                    ("acceleration", acceleration, \entity value -> entity { acceleration = value })
                    ]
            let updates' = filter (\(name, select, _) -> not (select actual .~~. select observed)) updates
            updates'' <- forM updates' $ \(name, select, update) -> trace ("Sending " ++ name) $ do
                    game <- readTVar gameVariable
                    let observed' = update observed (select actual)
                    writeTVar gameVariable game {
                        entities = Map.insert (identifier actual) (actual, observed') (entities game)
                        }
                    let Vector x y = select actual
                    return (name, jsonObject [
                        ("x", jsonNumber x), 
                        ("y", jsonNumber y)
                        ])
            if not (null updates'') 
                then return $ encode $ jsonArray [
                    jsonString "updateEntity",
                    jsonString (identifier observed),
                    jsonObject updates''
                    ]
                else return ""
    forM_ updates $ \update -> putFrame handle $ fromString update
    threadDelay (20 * 1000)
    sendLoop gameVariable handle

jsonString = JSString . toJSString
jsonObject = makeObj
jsonArray = JSArray
jsonNumber = JSRational True . toRational


data Vector = Vector Double Double deriving (Eq, Show)

Vector x1 y1 .+. Vector x2 y2 = Vector (x1 + x2) (y1 + y2)
Vector x1 y1 .-. Vector x2 y2 = Vector (x1 - x2) (y1 - y2)
Vector x y .* scale = Vector (x * scale) (y * scale)
Vector x1 y1 .~~. Vector x2 y2 = x1 ~~ x2 && y1 ~~ y2
x ~~ y = abs (x - y) < 0.01

data Controls = Controls {
    upKey :: Bool,
    downKey :: Bool,
    leftKey :: Bool,
    rightKey :: Bool,
    shootKey :: Bool
    }

data Game = Game {
    controls :: Controls,
    oldControls :: Controls,
    entities :: Map String (Entity, Entity),
    changedEntities :: Set String,
    oldTime :: UTCTime
    }

data Entity = Entity {
    identifier :: String,
    position :: Vector,
    velocity :: Vector,
    acceleration :: Vector,
    angle :: Double,
    rotation :: Double
    }

updateEntity :: Entity -> Double -> Entity
updateEntity entity deltaSeconds = Entity {
    identifier = identifier entity,
    position = position entity .+. (velocity entity .* deltaSeconds),
    velocity = velocity entity .+. (acceleration entity .* deltaSeconds),
    acceleration = acceleration entity,
    angle = angle entity + (rotation entity * deltaSeconds),
    rotation = rotation entity
    }

updateLoop :: TVar Game -> IO ()
updateLoop gameVariable = do
    newTime <- getCurrentTime
    atomically $ do
        game <- readTVar gameVariable
        let deltaSeconds = (fromRational . toRational) (diffUTCTime newTime (oldTime game))
        let entities' = Map.map (\(actual, observed) -> 
                (updateEntity actual deltaSeconds, updateEntity observed deltaSeconds))
                (entities game)
        let entities'' = controlEntities (controls game) entities' deltaSeconds
        writeTVar gameVariable game { 
            entities = entities'',
            oldTime = newTime,
            oldControls = controls game
            }
    threadDelay (20 * 1000)
    updateLoop gameVariable 

controlEntities :: Controls -> Map String (Entity, Entity) -> Double -> Map String (Entity, Entity)
controlEntities controls entities deltaSeconds = flip execState entities $ do
    change "player" (\entity -> entity { acceleration = Vector 0 0 })
    when (upKey controls) $ change "player" (\entity -> entity { 
        acceleration = acceleration entity .+. Vector 0 (-500) })
    when (downKey controls) $ change "player" (\entity -> entity { 
        acceleration = acceleration entity .+. Vector 0 500 })
    when (leftKey controls) $ change "player" (\entity -> entity { 
        acceleration = acceleration entity .+. Vector (-500) 0 })
    when (rightKey controls) $ change "player" (\entity -> entity { 
        acceleration = acceleration entity .+. Vector 500 0 })
    where
        change identifier f = do
            entities <- get
            let entity' = (\(actual, observed) -> (f actual, observed)) (entities Map.! identifier)
            put (Map.insert identifier entity' entities)

