{-# LANGUAGE NamedFieldPuns #-}

import Arithmetic
import Collision

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
import qualified Data.List as List
import Data.Ord
import Data.Maybe
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
        reservedPlayerNames = [],
        entities = Map.empty,
        entityPathChange = True,
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


-----------------------------------------------------------
-- Thread bodies
-----------------------------------------------------------

acceptClient :: TVar Game -> Handle -> IO ()
acceptClient gameVariable handle = do
    request <- shakeHands handle
    case request of
        Left err -> print err
        Right _ -> do
            putStrLn "Shook hands with new client."
            playerName <- atomically (generatePlayerName gameVariable)
            let entity = newEntity playerName 0 0
            let player = newPlayer playerName handle
            blockBroadcastWhile gameVariable $ do
                existingEntities <- atomically $ do
                    game <- readTVar gameVariable
                    writeTVar gameVariable game {
                        newEntities = (entity, True) : newEntities game,
                        players = player : players game,
                        entityPathChange = True
                        }
                        
                    return (map snd $ Map.elems (entities game))
                putFrame handle $ fromString "[\"welcome\"]"
                mapM_ (sendNewEntity handle) existingEntities
                trace ("Welcome " ++ playerName ++ "!") $ return ()
            receiveLoop gameVariable handle playerName


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
                        players = filter (\(player) -> identifier' /= entityIdentifier player) (players game),
                        reservedPlayerNames = List.delete identifier' (reservedPlayerNames game)
                        }
                game <- readTVarIO gameVariable
                forM_ (map handle (players game)) $ \handle -> 
                    putFrame handle $ fromString $ encode $ jsonArray [
                        jsonString "removeEntity",
                        jsonString identifier'
                    ]
                hClose handle'
            trace ("Bye " ++ identifier') (return ())
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
                    putFrame handle' $ fromString $ encode $ jsonArray [
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
                let p = pathPositionAt time (positionPaths actual)
                let p' = pathPositionAt time (positionPaths observed)
                if not (p .~~. p')
                    then trace ("Sending " ++ show (positionPaths actual)) $ do
                        modifyTVar gameVariable $ \game -> 
                            let observed' = observed { positionPaths = (positionPaths actual) } in 
                            game { entities = Map.insert (identifier actual) (actual, observed') (entities game) }
                        return $ encode $ jsonArray [
                            jsonString "updateEntityPaths",
                            jsonString (identifier observed),
                            jsonArray (map jsonPath (positionPaths actual))
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

updateLoop :: TVar Game -> IO ()
updateLoop gameVariable = do
    time <- currentTime gameVariable
    atomically $ do
        game <- readTVar gameVariable
        let entities' = catMaybes $ map (\player -> 
                let identifier' = entityIdentifier player in do
                    (actual, observed) <- Map.lookup identifier' (entities game)
                    return (identifier', (controlEntity (oldControls player) (controls player) time actual, observed))
                ) (players game)
        (entities', newEntities') <- return $ unzip $ map 
                (\(identifier, ((actual, newEntities'), observed)) -> ((identifier, (actual, observed)), newEntities')) 
                entities'
        entities' <- return $ (Map.fromList entities') `Map.union` (entities game)
        entities' <- return $ if (True || entityPathChange game)
            then
                let trimmedEntities = 
                        Map.map (first $ \e@Entity{positionPaths} -> e{positionPaths = [pathAt time positionPaths]}) entities' in
                updateEntityCollisions time trimmedEntities
            else entities'
        let players' = map (\player -> player { oldControls = controls player }) (players game)
        --when ((not . null) players') $ trace (show players') $ return ()
        --when ((not . Map.null) entities''') $ trace (show entities''') $ return ()
        --when ((not . Map.null) (entities game)) $ trace (show (entities game)) $ return ()
        writeTVar gameVariable game { 
            players = players',
            entities = entities',
            entityPathChange = False,
            newEntities = concat newEntities' ++ newEntities game
            }
    threadDelay (10 * 1000)
    updateLoop gameVariable


-----------------------------------------------------------
-- Socket operations
-----------------------------------------------------------

sendNewEntity :: Handle -> Entity -> IO ()
sendNewEntity handle entity = do
    putFrame handle $ fromString $ encode $ jsonArray [
        jsonString "newEntity",
        jsonString (identifier entity),
        jsonObject ([
            ("id", jsonString (identifier entity)), 
            ("pendingPositionPaths", jsonArray (map jsonPath (positionPaths entity))),
            ("newPositionPath", jsonPath (head (positionPaths entity))),
            ("oldPositionPath", jsonPath (head (positionPaths entity)))
            ]
        )]


-----------------------------------------------------------
-- Game State
-----------------------------------------------------------

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
    reservedPlayerNames :: [String],
    entities :: Map String (Entity, Entity),
    entityPathChange :: Bool,
    newEntities :: [(Entity, Bool)],
    nextIdentifier :: Integer,
    startTime :: UTCTime,
    blocked :: Bool
    }

data Entity = Entity {
    identifier :: String,
    positionPaths :: [Path]
    } deriving (Show)
    
newPlayer entityIdentifier handle = Player {
    controls = newControls,
    oldControls = newControls,
    entityIdentifier = entityIdentifier,
    handle = handle
    }

newEntity identifier x y = Entity {
    identifier = identifier,
    positionPaths = [staticPath (Vector x y)]
    }

newControls = Controls {
    upKey = False,
    downKey = False,
    leftKey = False,
    rightKey = False,
    shootKey = False
    }
   
    
-- STM & IO
-----------------------------

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

generatePlayerName :: TVar Game -> STM String
generatePlayerName gameVariable = do
    game <- readTVar gameVariable
    let usedNames = reservedPlayerNames game
    let names = ["Linus Torvalds", "Alan Turing", "Richard Stallman", "Bjarne Stroustrup", 
            "Simon Peyton Jones", "Edsger Dijkstra", "Peter Naur"]
    let unusedNames = names List.\\ usedNames
    choosen <- if null unusedNames then generateIdentifier gameVariable else return (head unusedNames)
    game <- readTVar gameVariable
    writeTVar gameVariable game { reservedPlayerNames = choosen : usedNames }
    return choosen

blockBroadcastWhile :: TVar Game -> IO a -> IO a
blockBroadcastWhile gameVariable monad = do
    atomically $ do
        game <- readTVar gameVariable
        when (blocked game) retry
        writeTVar gameVariable game { blocked = True }
    result <- monad
    atomically $ modifyTVar gameVariable $ \game -> game { blocked = False }
    return result
    
modifyTVar variable f = do
    a <- readTVar variable
    writeTVar variable (f a)


-----------------------------------------------------------
-- Entity logic
-----------------------------------------------------------

controlEntity :: Controls -> Controls -> Time -> Entity -> (Entity, [(Entity, Bool)])
controlEntity oldControls controls time entity | oldControls == controls = (entity, [])
controlEntity oldControls controls time entity = 
    let inputForces = foldr (.+.) (Vector 0 0) [
            if upKey controls then Vector 0 500 else Vector 0 0,
            if downKey controls then Vector 0 (-500) else Vector 0 0,
            if leftKey controls then Vector (-500) 0 else Vector 0 0,
            if rightKey controls then Vector 500 0 else Vector 0 0] in
    let (path : _) = pathsFrom time (positionPaths entity) in
    let path' = setAcceleration inputForces time path in
    let entity' = entity {positionPaths = [path']} in
    let Vector x y = getPosition path time in
    let bulletEntity = newEntity undefined x y in
    let bulletEntity' = bulletEntity { positionPaths = [Path time (Vector 0 0) (getVelocity path time .* 2) (Vector x y)] } in
    let newEntities' = if not (shootKey controls) then [] else [(bulletEntity', False)] in
    (entity', newEntities')

updateEntityCollisions :: Time -> Map String (Entity, Entity) -> Map String (Entity, Entity)
updateEntityCollisions time entities = 
    let pairs = pairAllOnce (map fst (Map.elems entities)) in
    let shape = (200, 100) in
    let justCollide = (\(e1, e2) -> do
                (collisionTime, normal) <- pathsBoxCollision time (positionPaths e1) shape (positionPaths e2) shape
                return (collisionTime, normal, e1, e2)) in
    let collisions = List.sortBy (comparing fst4) (mapMaybe justCollide pairs) in
    if (null collisions) then entities
    else
        let t@(c, normal, e1, e2) = head collisions in
        let (e1', e2') = handleCollision t in
        --trace ("COLLISION @ " ++ show c ++ " in " ++ show (c - time) ++ " seconds between " 
        --    ++ identifier e1 ++ " (" ++ show (pathPositionAt c (positionPaths e1')) ++ ") and " 
        --    ++ identifier e2 ++ " (" ++ show (pathPositionAt c (positionPaths e2')) ++ ")!!!") $
        --trace ("    New paths for " ++ identifier e1 ++ ": " ++ show (positionPaths e1')) $
        --trace ("    New paths for " ++ identifier e2 ++ ": " ++ show (positionPaths e2')) $
        let observed1 = snd $ entities Map.! (identifier e1) in
        let observed2 = snd $ entities Map.! (identifier e2) in
        Map.fromList [(identifier e1, (e1', observed1)), (identifier e2, (e2', observed2))] `Map.union` entities 
    where
        fst4 (a, _, _, _) = a
        handleCollision :: (Time, Vector, Entity, Entity) -> (Entity, Entity)
        handleCollision (time, normal, e1, e2) = 
            let paths1 = (positionPaths e1) in
            let paths2 = (positionPaths e2) in
            let collisionPath1 = pathAt time paths1 in
            let collisionPath2 = pathAt time paths2 in
            let v1 = getVelocity collisionPath1 time in
            let v2 = getVelocity collisionPath2 time in
            let (v1', v2') = bounce2 v1 v2 1 1 normal 0 in
            let beforeCollision1 = pathsUntil time paths1 in
            let beforeCollision2 = pathsUntil time paths2 in
            let time' = time - 0.0001 in
            let afterCollision1 =  beforeCollision1 ++ [Path time (getAcceleration collisionPath1 time) v1' (getPosition collisionPath1 time')] in
            let afterCollision2 =  beforeCollision2 ++ [Path time (getAcceleration collisionPath2 time) v2' (getPosition collisionPath2 time')] in
            (e1{positionPaths = afterCollision1}, e2{positionPaths = afterCollision2})

pairAllOnce :: [a] -> [(a, a)]
pairAllOnce [] = [] 
pairAllOnce (x : xs) = pairAllOnce' (x : xs) xs
    where
        pairAllOnce' [x] [] = []
        pairAllOnce' (x1 : x2 : xs) [] = pairAllOnce' (x2 : xs) xs
        pairAllOnce' (x : xs) (y : ys) = (x, y) : pairAllOnce' (x : xs) (ys)
-----------------------------------------------------------
-- JSON Serialization
-----------------------------------------------------------

jsonString = JSString . toJSString
jsonObject = makeObj
jsonArray = JSArray
jsonNumber = JSRational True . toRational


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

