import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)

import System.IO (Handle, hClose)
import System.Random

import Data.Time.Clock
import Data.Maybe (catMaybes)

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
        nextIdentifier = 1,
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
            identifier' <- atomically (generateIdentifier gameVariable)
            let identifier'' = "player-" ++ identifier'
            let entity = newEntity identifier'' 500 500
            let player = newPlayer identifier'' handle
            blockBroadcastWhile gameVariable $ do
                existingEntities <- atomically $ do
                    game <- readTVar gameVariable
                    writeTVar gameVariable game {
                        newEntities = (entity, True) : newEntities game,
                        players = player : players game
                        }
                    return (map snd $ Map.elems (entities game))
                putFrame handle $ fromString "[\"welcome\"]"
                mapM_ (sendNewEntity handle) existingEntities
            receiveLoop gameVariable handle identifier''

sendNewEntity :: Handle -> Entity -> IO ()
sendNewEntity handle entity = do
    putFrame handle $ fromString $ encode $ jsonArray [
        jsonString "newEntity",
        jsonString (identifier entity),
        jsonObject ([("id", jsonString (identifier entity)), ("positionPath", jsonPath (positionPath entity))])
        ]

jsonPath :: Path -> JSValue
jsonPath (Path t0 (Vector x'' y'') (Vector x' y') (Vector x y)) = jsonObject
    [
        ("t0", jsonNumber t0),
        ("a0", jsonObject [
            ("x", jsonNumber x''), 
            ("y", jsonNumber y'')
        ]),
        ("v0", jsonObject [
            ("x", jsonNumber x'), 
            ("y", jsonNumber y')
        ]),
        ("p0", jsonObject [
            ("x", jsonNumber x), 
            ("y", jsonNumber y)
        ])
    ]

blockBroadcastWhile :: TVar Game -> IO a -> IO a
blockBroadcastWhile gameVariable monad = do
    atomically $ do
        game <- readTVar gameVariable
        when (blocked game) retry
        writeTVar gameVariable game { blocked = True }
    result <- monad
    atomically $ modifyTVar gameVariable $ \game -> game { blocked = False }
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
                "synchronize" -> do
                    time <- currentTime gameVariable
                    trace ("Sent time " ++ show time) $ putFrame handle' $ fromString $ encode $ jsonArray [
                        jsonString "time",
                        jsonNumber time
                        ]
                "ping" -> do
                    time <- currentTime gameVariable
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
    time <- currentTime gameVariable
    blockBroadcastWhile gameVariable $ do
        (messages, handles, newEntities') <- atomically $ do
            game <- readTVar gameVariable
            messages <- forM (Map.elems (entities game)) $ \(actual, observed) -> do
                let p = getPosition (positionPath actual) time
                let p' = getPosition (positionPath observed) time
                if not (p .~~. p') 
                    then trace ("Sending " ++ show (positionPath actual)) $ do
                        modifyTVar gameVariable $ \game -> 
                            let observed' = observed { positionPath = (positionPath actual) } in 
                            game { entities = Map.insert (identifier actual) (actual, observed') (entities game) }
                        return $ encode $ jsonArray [
                            jsonString "updateEntityPath",
                            jsonString (identifier observed),
                            jsonPath (positionPath actual)
                            ]
                    else return ""
            game <- readTVar gameVariable
            newEntities' <- forM (newEntities game) $ \(entity, hasIdentifier) -> if hasIdentifier then return entity else do
                identifier' <- generateIdentifier gameVariable
                return entity { identifier = identifier' }
            modifyTVar gameVariable $ \game -> game {
                entities = Map.fromList (map (\entity -> (identifier entity, (entity, entity))) newEntities') `Map.union` entities game,
                newEntities = []
                }
            return (messages, map handle (players game), newEntities')
        forM_ newEntities' $ \entity -> trace ("Sending entity " ++ identifier entity) (return ())
        forM_ handles $ \handle -> forM_ newEntities' $ sendNewEntity handle
        let messages' = filter (not . null) messages
        forM_ handles $ \handle -> forM_ messages' $ putFrame handle . fromString
    threadDelay (10 * 1000)
    broadcastLoop gameVariable

currentTime :: TVar Game -> IO Double
currentTime gameVariable = do
    newTime <- getCurrentTime
    game <- readTVarIO gameVariable
    return $ (fromRational . toRational) (diffUTCTime newTime (startTime game))

generateIdentifier :: TVar Game -> STM String
generateIdentifier gameVariable = do
    game <- readTVar gameVariable
    let nextIdentifier' = nextIdentifier game
    writeTVar gameVariable game { nextIdentifier = nextIdentifier' + 1 }
    return ("entity-" ++ show nextIdentifier')

modifyTVar variable f = do
    a <- readTVar variable
    writeTVar variable (f a)

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

distance :: Vector -> Vector -> Double
distance a b = let Vector x y = b .-. a in sqrt (x*x + y*y)

data Controls = Controls {
    upKey :: Bool,
    downKey :: Bool,
    leftKey :: Bool,
    rightKey :: Bool,
    shootKey :: Bool
    } deriving (Eq, Show)

data Player = Player {
    controls :: Controls,
    oldControls :: Controls,
    entityIdentifier :: String,
    handle :: Handle
    } deriving Show

data Game = Game {
    players :: [Player],
    entities :: Map String (Entity, Entity),
    newEntities :: [(Entity, Bool)],
    nextIdentifier :: Integer,
    startTime :: UTCTime,
    blocked :: Bool
    }

data Entity = Entity {
    identifier :: String,
    positionPath :: Path
    } deriving (Show)

    
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
    time <- currentTime gameVariable
    atomically $ do
        game <- readTVar gameVariable
        let entities' = catMaybes $ map (\player -> 
                let identifier' = entityIdentifier player in
                case Map.lookup identifier' (entities game) of
                    Just (actual, observed) -> Just (identifier', (controlEntity (oldControls player) (controls player) time actual, observed))
                    Nothing -> Nothing
                ) (players game)
        let (entities'', newEntities') = unzip $ map 
                (\(identifier, ((actual, newEntities'), observed)) -> ((identifier, (actual, observed)), newEntities')) 
                entities'
        let entities''' = (Map.fromList entities'') `Map.union` (entities game)
        let players' = map (\player -> player { oldControls = controls player }) (players game)
        --when ((not . null) players') $ trace (show players') $ return ()
        --when ((not . Map.null) entities''') $ trace (show entities''') $ return ()
        --when ((not . Map.null) (entities game)) $ trace (show (entities game)) $ return ()
        writeTVar gameVariable game { 
            players = players',
            entities = entities''',
            newEntities = concat newEntities' ++ newEntities game
            }
    threadDelay (10 * 1000)
    updateLoop gameVariable


controlEntity :: Controls -> Controls -> Time -> Entity -> (Entity, [(Entity, Bool)])
controlEntity oldControls controls time entity | oldControls == controls = (entity, [])
controlEntity oldControls controls time entity = 
    let inputForces = foldr (.+.) (Vector 0 0) [
            if upKey controls then Vector 0 (-500) else Vector 0 0,
            if downKey controls then Vector 0 500 else Vector 0 0,
            if leftKey controls then Vector (-500) 0 else Vector 0 0,
            if rightKey controls then Vector 500 0 else Vector 0 0] in
    let path = positionPath entity in
    let path' = setAcceleration inputForces time path in
    let entity' = entity {positionPath = path'} in
    let Vector x y = getPosition path time in
    let bulletEntity = newEntity undefined x y in
    let bulletEntity' = bulletEntity { positionPath = Path time (Vector 0 0) (getVelocity path time .* 2) (Vector x y) } in
    let newEntities' = if not (shootKey controls) then [] else [(bulletEntity', False)] in
    (entity', newEntities')
    
