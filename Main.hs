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
    startTime <- getCurrentTime
    gameVariable <- newTVarIO Game {
        players = [],
        entities = Map.empty,
        newEntities = [],
        startTime = startTime,
        blocked = False
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
    putFrame handle $ fromString $ encode $ jsonArray [
        jsonString "newEntity",
        jsonString (identifier entity),
        jsonObject ([("id", jsonString (identifier entity))] ++ jsonPath (positionPath entity))]

jsonPath :: Path -> [(String, JSValue)]    
jsonPath (Path t0 (Vector x'' y'') (Vector x' y') (Vector x y)) =
    [
        ("time", jsonNumber t0),
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
        ])
    ]

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
receiveLoop gameVariable handle' identifier' = do
    input <- getFrame handle'
    if B.null input
        then do
            putStrLn "EOF encountered. Closing handle."
            blockBroadcastWhile gameVariable $ do
                atomically $ do
                    game <- readTVar gameVariable
                    writeTVar gameVariable game {
                        entities = Map.filter (\(actual, observed) -> identifier' /= identifier observed) (entities game),
                        players = filter (\(player) -> identifier' /= entityIdentifier player) (players game)
                        }
                game <- readTVarIO gameVariable
                forM_ (map handle (players game)) $ \handle -> 
                    putFrame handle $ fromString $ encode $ jsonArray [
                        jsonString "removeEntity",
                        jsonString identifier'
                    ]
                hClose handle'
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
                "ping" -> do
                    newTime <- getCurrentTime
                    game <- readTVarIO gameVariable
                    let time = ((fromRational . toRational) (diffUTCTime newTime (startTime game))) / 1000
                    trace ("Sent pong " ++ show time) $ putFrame handle' $ fromString $ encode $ jsonArray [
                        jsonString "pong",
                        jsonNumber time
                        ]
            receiveLoop gameVariable handle' identifier'
    where
        change f = atomically $ do
            game <- readTVar gameVariable
            let players' = map (\player -> if entityIdentifier player == identifier'
                    then player { controls = f (controls player) } 
                    else player) 
                    (players game)
            writeTVar gameVariable game { players = players' }
            
broadcastLoop :: TVar Game -> IO ()
broadcastLoop gameVariable = do
    newTime <- getCurrentTime
    blockBroadcastWhile gameVariable $ do
        (messages, handles, newEntities) <- atomically $ do
            game <- readTVar gameVariable
            let time = ((fromRational . toRational) (diffUTCTime newTime (startTime game))) / 1000
            messages <- forM (Map.elems (entities game)) $ \(actual, observed) -> do
                let p = getPosition (positionPath actual) time
                let p' = getPosition (positionPath observed) time
                if not (p .~~. p') 
                    then trace ("Sending " ++ show (positionPath actual)) $ do
                        game <- readTVar gameVariable
                        let observed' = observed { positionPath = (positionPath actual) }
                        writeTVar gameVariable game {
                            entities = Map.insert (identifier actual) (actual, observed') (entities game)
                            }
                        return $ encode $ jsonArray [
                            jsonString "updateEntity",
                            jsonString (identifier observed),
                            jsonObject (jsonPath (positionPath actual))
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

infixl 4 ~~
infixl 4 .~~.
infixl 6 .+.
infixl 6 .-.
infixl 7 .*

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
    } deriving Eq

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
    startTime :: UTCTime,
    blocked :: Bool
    }

data Entity = Entity {
    identifier :: String,
    positionPath :: Path
    }

    
data Path = Path Time Vector Vector Vector deriving Show
type Time = Double


getPosition :: Path -> Time -> Vector
getPosition (Path t0 a0 v0 p0) t = a0 .* (t - t0) ^ 2 .+. v0 .* (t - t0) .+. p0

getVelocity :: Path -> Time -> Vector
getVelocity (Path t0 a0 v0 _) t = a0 .* (2 * (t - t0)) .+. v0

getAcceleration :: Path -> Time -> Vector
getAcceleration (Path t0 a0 _ _) _ = a0


setAcceleration :: Vector -> Time -> Path -> Path
setAcceleration a t path = Path t a (getVelocity path t) (getPosition path t)

setVelocity :: Vector -> Time -> Path -> Path
setVelocity v t path = Path t (getAcceleration path t) v (getPosition path t)

setPosition :: Vector -> Time -> Path -> Path
setPosition p t path = Path t (getAcceleration path t) (getVelocity path t) p


staticPath :: Vector -> Path
staticPath p = Path 0 (Vector 0 0) (Vector 0 0) p


newPlayer entityIdentifier handle = Player {
    controls = newControls,
    oldControls = newControls,
    entityIdentifier = entityIdentifier,
    handle = handle
    }

newEntity identifier x y = Entity {
    identifier = identifier,
    positionPath = staticPath (Vector x y)
    }

newControls = Controls {
    upKey = False,
    downKey = False,
    leftKey = False,
    rightKey = False,
    shootKey = False
    }


updateLoop :: TVar Game -> IO ()
updateLoop gameVariable = do
    newTime <- getCurrentTime
    atomically $ do
        game <- readTVar gameVariable
        let time = ((fromRational . toRational) (diffUTCTime newTime (startTime game))) / 1000
        let entities' = map (\player -> 
                let identifier = entityIdentifier player in
                let (actual, observed) = (entities game) Map.! identifier in
                (identifier, (controlEntity (oldControls player) (controls player) time actual, observed))) (players game)
        let entities'' = (Map.fromList entities') `Map.union` (entities game)
        let players' = map (\player -> player { oldControls = controls player } ) (players game)
        writeTVar gameVariable game { 
            players = players',
            entities = entities''
            }
    threadDelay (10 * 1000)
    updateLoop gameVariable


controlEntity :: Controls -> Controls -> Time -> Entity -> Entity
controlEntity oldControls controls time entity | oldControls == controls = entity
controlEntity oldControls controls time entity = 
    let inputForces = foldr (.+.) (Vector 0 0) [
            if upKey controls then Vector 0 (-500) else Vector 0 0,
            if downKey controls then Vector 0 500 else Vector 0 0,
            if leftKey controls then Vector (-500) 0 else Vector 0 0,
            if rightKey controls then Vector 500 0 else Vector 0 0] in
    let path = positionPath entity in
    entity { positionPath = setAcceleration inputForces time path }

