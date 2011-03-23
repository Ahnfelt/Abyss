import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)

import System.IO (Handle, hClose)
import System.Random

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
    gameVariable <- newTVarIO Game {
        players = [],
        entities = Map.empty,
        newEntities = [],
        blocked = False,
        oldTime = newTime
        }
    forkIO $ updateLoop gameVariable
    forkIO $ broadcastLoop gameVariable
    forever $ do
        (handle, _, _) <- accept socket
        forkIO (acceptClient gameVariable handle)

acceptClient :: TVar Game -> Handle -> IO ()
acceptClient gameVariable handle = do
    request <- shakeHands handle
    case request of
        Left err -> print err
        Right _ -> do
            putStrLn "Shook hands with new client."
            identifierNumber <- randomRIO (0, 1000000) :: IO Int -- TODO
            let identifier = "player-" ++ show identifierNumber
            let entity = newEntity identifier 500 500
            let player = newPlayer identifier handle
            blockBroadcastWhile gameVariable $ do
                existingEntities <- atomically $ do
                    game <- readTVar gameVariable
                    writeTVar gameVariable game {
                        entities = Map.insert identifier (entity, entity) (entities game),
                        newEntities = identifier : newEntities game,
                        players = player : players game
                        }
                    return (map snd $ Map.elems (entities game))
                putFrame handle $ fromString "[\"keepAlive\"]"
                mapM_ (sendNewEntity handle) existingEntities
            receiveLoop gameVariable handle identifier

sendNewEntity :: Handle -> Entity -> IO ()
sendNewEntity handle entity = do
    let Vector x y = position entity
    let Vector x' y' = velocity entity
    let Vector x'' y'' = acceleration entity
    let angle' = angle entity
    let angle'' = rotation entity
    putFrame handle $ fromString $ encode $ jsonArray [
        jsonString "newEntity",
        jsonString (identifier entity),
        jsonObject [
            ("id", jsonString (identifier entity)),
            ("position", jsonObject [
                ("x", jsonNumber x), 
                ("y", jsonNumber y)
            ]),
            ("velocity", jsonObject [
                ("x", jsonNumber x'), 
                ("y", jsonNumber y')
            ]),
            ("acceleration", jsonObject [
                ("x", jsonNumber x''), 
                ("y", jsonNumber y'')
            ]),
            ("angle", jsonNumber angle'),
            ("rotation", jsonNumber angle'')
        ]]
    

blockBroadcastWhile :: TVar Game -> IO a -> IO a
blockBroadcastWhile gameVariable monad = do
    atomically $ do
        game <- readTVar gameVariable
        when (blocked game) retry
        writeTVar gameVariable game {
            blocked = True
            }
    result <- monad
    atomically $ do
        game <- readTVar gameVariable
        writeTVar gameVariable game {
            blocked = False
            }
    return result

receiveLoop :: TVar Game -> Handle -> String -> IO ()
receiveLoop gameVariable handle identifier = do
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
            receiveLoop gameVariable handle identifier
    where
        change f = atomically $ do
            game <- readTVar gameVariable
            let players' = map (\player -> if entityIdentifier player == identifier 
                    then player { controls = f (controls player) } 
                    else player) 
                    (players game)
            writeTVar gameVariable game { players = players' }
            
broadcastLoop :: TVar Game -> IO ()
broadcastLoop gameVariable = do
    blockBroadcastWhile gameVariable $ do
        (messages, handles, newEntities) <- atomically $ do
            game <- readTVar gameVariable
            messages <- forM (Map.elems (entities game)) $ \(actual, observed) -> do
                let updates = [
                        ("position", position, \entity value -> entity { position = value }),
                        ("velocity", velocity, \entity value -> entity { velocity = value }),
                        ("acceleration", acceleration, \entity value -> entity { acceleration = value })
                        ]
                let updates' = filter (\(name, select, _) -> not (select actual .~~. select observed)) updates
                updates'' <- forM updates' $ \(name, select, update) -> trace ("Sending " ++ name ++ " " ++ show (select actual)) $ do
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
            game <- readTVar gameVariable
            writeTVar gameVariable game {
                newEntities = []
                }
            return (messages, map handle (players game), map (snd . (entities game Map.!)) (newEntities game))
        forM_ handles $ \handle -> forM_ newEntities $ sendNewEntity handle
        let messages' = filter (not . null) messages
        forM_ handles $ \handle -> forM_ messages' $ putFrame handle . fromString
    threadDelay (10 * 1000)
    broadcastLoop gameVariable

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

data Player = Player {
    controls :: Controls,
    oldControls :: Controls,
    entityIdentifier :: String,
    handle :: Handle
    }

data Game = Game {
    players :: [Player],
    entities :: Map String (Entity, Entity),
    newEntities :: [String],
    blocked :: Bool,
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

newPlayer entityIdentifier handle = Player {
    controls = newControls,
    oldControls = newControls,
    entityIdentifier = entityIdentifier,
    handle = handle
    }

newEntity identifier x y = Entity {
    identifier = identifier,
    position = Vector x y,
    velocity = Vector 0 0,
    acceleration = Vector 0 0,
    angle = 0,
    rotation = 0
    }

newControls = Controls {
    upKey = False,
    downKey = False,
    leftKey = False,
    rightKey = False,
    shootKey = False
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
        let entities'' = map (\player -> 
                let identifier = entityIdentifier player in
                let pair = entities' Map.! identifier in
                (identifier, (controlEntity (controls player) (fst pair), snd pair))) (players game)
        let entities''' = (Map.fromList entities'') `Map.union` entities'
        let players' = map (\player -> player { oldControls = controls player } ) (players game)
        writeTVar gameVariable game { 
            players = players',
            entities = entities''',
            oldTime = newTime
            }
    threadDelay (10 * 1000)
    updateLoop gameVariable 

controlEntity :: Controls -> Entity -> Entity
controlEntity controls entity = flip execState entity $ do
    modify (\entity -> entity { acceleration = Vector 0 0 })
    when (upKey controls) $ modify (\entity -> entity { 
        acceleration = acceleration entity .+. Vector 0 (-500) })
    when (downKey controls) $ modify (\entity -> entity { 
        acceleration = acceleration entity .+. Vector 0 500 })
    when (leftKey controls) $ modify (\entity -> entity { 
        acceleration = acceleration entity .+. Vector (-500) 0 })
    when (rightKey controls) $ modify (\entity -> entity { 
        acceleration = acceleration entity .+. Vector 500 0 })

