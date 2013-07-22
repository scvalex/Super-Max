{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}

module SuperMax.GL (
        -- * The Game
        Game,

        -- * Engine interface
        play, quitGame, getGameState, getsGameState, modifyGameState,
        withAlternateGameState, randomR, mkUid, getGameTick, upon,
        getResourceDirectory, getResource
    ) where

import Control.Applicative ( Applicative(..), (<$>) )
import Control.Concurrent ( ThreadId, forkIO )
import Control.Concurrent.STM ( atomically
                              , TChan, newTChanIO, writeTChan, tryReadTChan )
import Control.Exception ( Exception )
import Control.Monad ( when, forM, forM_ )
import Data.Dynamic ( Dynamic )
import Data.Foldable ( foldlM )
import Data.Function ( on )
import Data.List ( groupBy )
import Data.Map ( Map )
import Data.Set ( Set )
import Data.Vect.Float ( Mat4(..), Vec4(..), (.*.), idmtx )
import Data.Time ( UTCTime, getCurrentTime, addUTCTime )
import Data.Typeable ( Typeable )
import Foreign.C.String ( withCString )
import Foreign.Marshal.Safe ( with, withArrayLen )
import Foreign.Ptr ( Ptr, nullPtr, plusPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL hiding ( normalize )
import Graphics.Rendering.OpenGL.GL.Capability ( EnableCap(..), makeCapability )
import Graphics.Rendering.OpenGL.GL.Shaders.Program ( Program(..) )
import qualified Control.Exception as CE
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Graphics.Rendering.OpenGL.Raw as Raw
import qualified Graphics.UI.GLFW as GLFW
import qualified System.Random as R
import SuperMax.GL.Drawing ( Drawing(..), SomeDrawable(..), Drawable(..), Model(..)
                           , serializeVertex
                           , Text(..) )
import SuperMax.GL.Utils ( initRendering, checkError, makeShaderProgram
                         , GLFont, loadFontFromImage, writeText2D )
import SuperMax.Input ( InputEvent, fromGlfwKeyEvent )
import System.Environment ( getEnv )
import System.FilePath ( (</>) )
import System.Random ( Random, StdGen, newStdGen )
import System.Exit ( exitWith, ExitCode(..) )
import Text.Printf ( printf )
import Unsafe.Coerce ( unsafeCoerce )

--------------------------------
-- Game/Engine interface
--------------------------------

data IOAction s = forall a. IOAction (IO a) (a -> Game s ())

data EngineState s = EngineState { engineInnerState :: s
                                 , engineEventCh  :: TChan (EngineEvent s)
                                 , getThreads    :: Set ThreadId
                                 , isTerminating :: Bool
                                 , getGen        :: StdGen
                                 , getNextUid    :: Int
                                 , getTick       :: Int
                                 , getQueuedIO   :: [IOAction s]
                                 , getResDir     :: FilePath
                                 , getRes        :: Map String Dynamic
                                 }

-- | The events the engine's asynchronous components may send/receive.
data EngineEvent s = GameInput InputEvent
                   | GameAction (Game s ())

-- | Psych!  It's (almost) a state monad!
newtype Game s a = Game { runGame :: EngineState s -> (a, EngineState s) }

instance Monad (Game s) where
    return x = Game (\s -> (x, s))
    (Game h) >>= f = Game (\s -> let (x, s') = h s in
                                 let (Game g) = f x in
                                 g s')

instance Functor (Game s) where
    f `fmap` (Game h) = Game (\s -> let (x, s') = h s in (f x, s'))

instance Applicative (Game s) where
    pure x = Game (\s -> (x, s))
    Game f <*> Game g = Game (\s -> let (h, s') = f s in
                                    let (x, s'') = g s' in
                                    (h x, s''))

-- | Get the current game state.
getGameState :: Game s s
getGameState = Game (\s -> (engineInnerState s, s))

-- | Get an inner field of the game state.
getsGameState :: (s -> a) -> Game s a
getsGameState getField = Game (\s -> (getField (engineInnerState s), s))

--FIXME The game state should be identified by a counter incremented by modifyGameState.
-- | Pass the current game state through the given function.
modifyGameState :: (s -> s) -> Game s ()
modifyGameState f = Game (\s -> ((), s { engineInnerState = f (engineInnerState s) }))

-- 'setGameState' is unsafe and should not be used; use 'modifyGameState' instead.

-- | Instruct the engine to close after the current callback returns.
quitGame :: Game s ()
quitGame = Game (\s -> ((), s { isTerminating = True }))

-- FIXME The half-functioning state thing in 'withAlternateGameState' is horrible.
-- | Run an action in the 'Game' monad but with a different game state.  If the alternate
-- state is 'Nothing', doesn't do anything.
withAlternateGameState :: forall s t a.
                          (s -> Maybe t)    -- ^ get alternate state
                       -> (t -> s)          -- ^ set alternate state
                       -> Game t (Maybe a)  -- ^ action with alternate state
                       -> Game s (Maybe a)
withAlternateGameState getAlternateState setAlternateState innerAction =
    Game (\s -> case getAlternateState (engineInnerState s) of
                     Just alternateState ->
                         -- Note that the alternate engine state is *not* fully functional.
                         let as = EngineState { engineInnerState = alternateState
                                              , engineEventCh  = undefined
                                              , getThreads    = getThreads s
                                              , isTerminating = isTerminating s
                                              , getGen        = getGen s
                                              , getNextUid    = getNextUid s
                                              , getTick       = getTick s
                                              , getQueuedIO   = [] -- there's not point in
                                                                   -- passing them down
                                              , getResDir     = getResDir s
                                              , getRes        = getRes s
                                              } in
                         let (x, as') = runGame innerAction as in
                         -- FIXME Only change the parts of the state that should change.
                         let s' = EngineState { engineInnerState = setAlternateState (engineInnerState as')
                                              , engineEventCh  = engineEventCh s
                                              , getThreads    = getThreads as'
                                              , isTerminating = isTerminating as'
                                              , getGen        = getGen as'
                                              , getNextUid    = getNextUid as'
                                              , getTick       = getTick as'
                                              , getQueuedIO   = getQueuedIO s ++
                                                                mapToInnerState (getQueuedIO as')
                                              , getResDir     = getResDir as'
                                              , getRes        = getRes as'
                                              } in
                         (x, s')
                     Nothing -> (Nothing, s))
  where
    mapToInnerState :: [IOAction t] -> [IOAction s]
    mapToInnerState = map (\(IOAction act handler) -> IOAction act (toInnerState handler))

    toInnerState :: (b -> Game t ()) -> (b -> Game s ())
    toInnerState act = \x -> do
        _ <- withAlternateGameState
            getAlternateState
            setAlternateState
            (act x >> return Nothing)
        return ()

-- | Generate a random number using the game's random generator.
randomR :: (Random a) => (a, a) -> Game s a
randomR bounds = Game (\s -> let (x, g) = R.randomR bounds (getGen s) in (x, s { getGen = g }))

-- | Return a unique 'Int'.  This function never returns the same value twice.
mkUid :: Game s Int
mkUid = Game (\s -> let uid = getNextUid s in (uid, s { getNextUid = uid + 1 }))

-- | Get the current tick of the game.  This is the total number of 'Tick' events that
-- have taken place.
getGameTick :: Game s Int
getGameTick = Game (\s -> (getTick s, s))

-- | Run an 'IO' action on a separate thread, and then run the handler on the result.
-- Remember, there is only one game thread, so the handler will be run at some
-- indeterminate time in the future.
upon :: IO a -> (a -> Game s ()) -> Game s ()
upon act handler = Game (\s -> ((), s { getQueuedIO = IOAction act handler : getQueuedIO s }))

-- | Get the directory from where resources can be loaded.
getResourceDirectory :: Game s FilePath
getResourceDirectory = Game (\s -> (getResDir s, s))

-- | Get a named pre-loaded resource.
getResource :: String -> Game s (Maybe Dynamic)
getResource name = Game (\s -> (M.lookup name (getRes s), s))

--------------------------------
-- Drawing
--------------------------------

draw :: Map String Program -> Map String GLFont -> Drawing -> IO ()
draw programs fonts drawing = do
    -- FIXME Don't generate objects in the drawing function.

    -- Setup a VBO with the drawing
    [vertexBuffer] <- genObjectNames 1
    bindBuffer ArrayBuffer $= Just vertexBuffer

    -- Create VP matrix
    (width, height) <- GLFW.getWindowDimensions
    let projection = matPerspective (pi / 3) (fromIntegral width / fromIntegral height)
        view = drawingViewMatrix drawing
        vp = projection .*. view

    forM_ drawablesByProgram $ \(name, drawables) -> do
        -- Set shader program
        let program = maybe (error (printf "no such program %s" name)) id (M.lookup name programs)
        currentProgram $= Just program

        forM_ drawables $ \drawable -> do
            -- Transfer MVP to OpenGL
            mvpId <- withCString "MVP" $ Raw.glGetUniformLocation (programID program)
            let mvp = vp .*. drawableModelMatrix drawable
            with mvp $ Raw.glUniformMatrix4fv mvpId 1 0 . unsafeCoerce

            -- Transfer vertices and colours to OpenGL
            case drawableModel drawable of
                Nothing ->
                    return ()
                Just (TriangleModel triangles) -> do
                    let drawingData = concatMap serializeVertex $
                                      concatMap (\(a, b, c) -> [a, b, c]) triangles
                    withArrayLen drawingData $ \len drawingDataPtr ->
                        bufferData ArrayBuffer $= ( fromIntegral (len * sizeOfFloat)
                                                  , drawingDataPtr
                                                  , StaticDraw )


                    -- Perform drawing
                    vertexArrayId <- get (attribLocation program "vertexPosition")
                    colourArrayId <- get (attribLocation program "vertexColour")

                    vertexAttribArray vertexArrayId $= Enabled
                    vertexAttribArray colourArrayId $= Enabled

                    bindBuffer ArrayBuffer $= Just vertexBuffer
                    vertexAttribPointer vertexArrayId $=
                        (ToFloat, VertexArrayDescriptor 3
                                                        Float
                                                        (fromIntegral (sizeOfFloat * 6))
                                                        nullPtr)
                    vertexAttribPointer colourArrayId $=
                        (ToFloat, VertexArrayDescriptor 3
                                                        Float
                                                        (fromIntegral (sizeOfFloat * 6))
                                                        (makeOffset (3 * sizeOfFloat)))

                    drawArrays Triangles 0 (fromIntegral (length drawingData `div` 2))

                    vertexAttribArray colourArrayId $= Disabled
                    vertexAttribArray vertexArrayId $= Disabled

    -- Cleanup
    deleteObjectNames [vertexBuffer]

    -- Draw HUD text
    forM_ (drawingDrawables drawing) $ \drawable -> do
        forM_ (drawableHudTexts drawable) $ \text -> do
            let name = textFontName text
                font = maybe (error (printf "no such font %s" name)) id (M.lookup name fonts)
                (x, y) = textPosition text
            writeText2D font (textText text) x y (textSize text) idmtx
  where
    -- | Group drawables that use the same program together.
    drawablesByProgram :: [(String, [SomeDrawable])]
    drawablesByProgram =
        let ds = drawingDrawables drawing
            dgs = groupBy ((==) `on` drawableProgramName) ds
        in map (\dg@(d:_) -> (drawableProgramName d, dg)) dgs

    sizeOfFloat :: Int
    sizeOfFloat = sizeOf (undefined :: GLfloat)

    makeOffset :: Int -> Ptr GLchar
    makeOffset = plusPtr (nullPtr :: Ptr GLchar) . fromIntegral

    -- | Make a perspective projection matrix: the visible cube is (-1, -1, -1) to (1, 1,
    -- 1), and objects with lesser z indices are hidden behind those with greater z
    -- indices (the opposite of OpenGL).  Note that we don't take zNear and zFar into
    -- account.
    --
    -- https://unspecified.wordpress.com/2012/06/21/calculating-the-gluperspective-matrix-and-other-opengl-matrix-maths/
    matPerspective :: Float -> Float -> Mat4
    matPerspective fovy aspect =
        let f = 1.0 / tan (fovy / 2.0)
        in Mat4 (Vec4 (f / aspect) 0.0 0.0 0.0)
                (Vec4 0.0 f 0.0 0.0)
                (Vec4 0.0 0.0 (-1.0) 0.0)
                (Vec4 0.0 0.0 0.0 1.0)

--------------------------------
-- The Engine loop
--------------------------------

-- | 'Shutdown' is sent to each non-main game thread when the engine shuts down.
data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

play :: forall w.
        String                                -- ^ Window title
     -> Int                                   -- ^ Logical ticks per second
     -> Map String (FilePath, FilePath)       -- ^ Shader program and associated vertex
                                              -- and fragment shader paths
     -> Map String FilePath                   -- ^ Fonts
     -> (FilePath -> IO (Map String Dynamic)) -- ^ Resource pre-loader.
     -> (Int -> Int -> w)                     -- ^ Take the screen width and height, and
                                              -- return the initial game state
     -> Game w ()                             -- ^ An initialization action.
     -> (w -> Drawing)                        -- ^ Draw a particular state
     -> (InputEvent -> Game w ())             -- ^ Update the state after an 'InputEvent'
     -> (Float -> Game w ())                  -- ^ Update the state after a tick
     -> IO ()
play title tps programPaths fontPaths loadResources wInit start drawGame onInput onTick = do
    glfwInitialization "GLFW.initialize" GLFW.initialize

    -- Setup window
    glfwInitialization "GLFW.openWindow" $ GLFW.openWindow (GLFW.defaultDisplayOptions
        { GLFW.displayOptions_numFsaaSamples = Just 4
          -- My Intel graphics card only supports OpenGL 3.1
        , GLFW.displayOptions_openGLVersion  = (3, 1)
          -- My Intel graphics card doesn't support an extension GLFW needs for
          -- CoreProfile.
          -- , GLFW.displayOptions_openGLProfile = GLFW.DefaultProfile
        , GLFW.displayOptions_width          = 768
        , GLFW.displayOptions_height         = 1024
        , GLFW.displayOptions_displayMode    = GLFW.Window
        , GLFW.displayOptions_numDepthBits   = 32
        })
    GLFW.setWindowTitle title

    -- Depth testing
    makeCapability CapDepthTest $= Enabled
    depthFunc $= Just Less

    initRendering

    -- Set the background colour
    clearColor $= Color4 0 0 0.0 0

    putStrLn "GL initialised"

    -- FIXME Also look for resources in cabal data dir.

    -- Resources path
    spellslingerDir <- CE.handle (\(_ :: CE.SomeException) -> return Nothing) $
                       Just <$> getEnv "SPELLSLINGER_DIR"
    let resDir = maybe "r" (</> "r") spellslingerDir
    _ <- printf "Loading resources from %s\n" resDir

    -- Load shaders
    programs <- makeShaderPrograms resDir

    -- Load fonts

    fonts <- loadFonts resDir

    -- fonts <- M.fromList <$> forM [10..80] (\size -> do
    --     font <- TTF.openFont (resDir </> "Ubuntu-C.ttf") size
    --     return (size, font))

    -- Load game resources
    resources <- loadResources resDir

    putStrLn "Resources loaded"

    -- This channel is used to collect events from multiple sources for the game.
    eventCh <- newTChanIO

    -- The random number generator used throughout the game.
    gen <- newStdGen

    (screenW, screenH) <- GLFW.getWindowDimensions

    let es = EngineState { engineInnerState = wInit screenW screenH
                         , engineEventCh  = eventCh
                         , getThreads    = S.empty
                         , isTerminating = False
                         , getGen        = gen
                         , getNextUid    = 1
                         , getTick       = 0
                         , getQueuedIO   = []
                         , getResDir     = resDir
                         , getRes        = resources
                         }

    -- Setup callbacks
    -- GLFW.setWindowRefreshCallback (drawScene state)
    GLFW.setKeyCallback (keyPressed es)
    GLFW.setWindowCloseCallback (shutdown es)

    -- Start the game
    let ((), es') = runGame start es

    fixedSliceLoop (1.0 / fromIntegral tps) programs fonts es'
  where
    -- | Callback for key events.  Convert them to our uniform representation and write
    -- them to the event channel.
    keyPressed :: EngineState w -> GLFW.KeyCallback
    keyPressed es GLFW.KeyEsc True =
        shutdown es >> return ()
    keyPressed es key press =
        case fromGlfwKeyEvent key press of
            Nothing    -> return ()
            Just event -> atomically $ writeTChan (engineEventCh es) (GameInput event)

    -- | Raise an error if the given action returns false.
    glfwInitialization :: String -> IO Bool -> IO ()
    glfwInitialization tag act = do
        ok <- act
        when (not ok) (error ("Could not initialize GLFW: " ++ tag))

    -- FIXME Non-deterministic games should just run at max speed.

    -- | A fixed slice loop.  See this article for details:
    -- http://fabiensanglard.net/timer_and_framerate/index.php
    fixedSliceLoop :: Float              -- ^ Slice in s
                   -> Map String Program -- ^ Program name => Shader Program
                   -> Map String GLFont  -- ^ Font name => Font
                   -> EngineState w      -- ^ Game state
                   -> IO ()
    fixedSliceLoop slice programs fonts es0 = do
        now <- getCurrentTime
        fixedSliceLoop' now es0
      where
        fixedSliceLoop' simulationTime1 es1 = do
            now <- getCurrentTime
            (simulationTime2, es2) <- updateUntilSimulationInTheFuture now simulationTime1 es1

            -- Draw the current state.
            renderWorld (drawGame (engineInnerState es2))

            -- FIXME Support interpolating drawings between updates.

            -- Start queued IO actions
            es3 <- startQueuedIO es2

            if isTerminating es3
                then shutdown es3 >> return ()
                else fixedSliceLoop' simulationTime2 es3

        renderWorld drawing = do
            clear [ ColorBuffer, DepthBuffer ]
            draw programs fonts drawing
            GLFW.swapBuffers
            checkError "renderWorld"

            -- draw screen fonts idmtx white $
            --     -- The origin is in the bottom right corner
            --     Translate 0.0 (fromIntegral screenH) $
            --     Scale 1.0 (-1.0) $
            --     (drawGame (engineInnerState es2))

        updateUntilSimulationInTheFuture :: UTCTime
                                         -> UTCTime
                                         -> EngineState w
                                         -> IO (UTCTime, EngineState w)
        updateUntilSimulationInTheFuture now simulationTime1 es1 = do
            if simulationTime1 < now
                then do
                    es2 <- processEvents es1
                    let ((), es3) = runGame (onTick slice) es2  -- Fixed slice
                    let simulationTime2 = addUTCTime (fromRational (toRational slice))
                                                     simulationTime1
                    return (simulationTime2, es3)
                else do
                    return (simulationTime1, es1)

        processEvents :: EngineState w -> IO (EngineState w)
        processEvents es1 = do
            -- Get next event, if any
            mevent <- atomically $ tryReadTChan (engineEventCh es1)

            case mevent of
                Nothing -> do
                    return es1
                Just event -> do
                    -- Notify game of event.
                    let ((), es2) = case event of
                            GameInput gevent -> runGame (onInput gevent) es1
                            GameAction gact  -> runGame gact es1
                    return es2

    startQueuedIO :: EngineState w -> IO (EngineState w)
    startQueuedIO (es@EngineState { getQueuedIO = ioacts }) = do
        es' <- foldlM startIOAction es ioacts
        return (es' { getQueuedIO = [] })
      where
        startIOAction es0 (IOAction act handler) =
            managedForkIO es0 $ do
                x <- act
                atomically $ writeTChan (engineEventCh es0) (GameAction (handler x))

    -- | Fork a thread and keep track of its id.
    managedForkIO :: EngineState s -> IO () -> IO (EngineState s)
    managedForkIO es act = do
        tid <- forkIO (CE.handle (\(_ :: Shutdown) -> return ()) act)
        -- FIXME Memory leak: thread ids are never removed from the set.
        return es { getThreads = S.insert tid (getThreads es) }

    -- | Terminate the game window.
    terminateGame :: GLFW.WindowCloseCallback
    terminateGame = do
        GLFW.closeWindow
        GLFW.terminate
        _ <- exitWith ExitSuccess
        return True

    -- | Shutdown the engine by throwing 'Shutdown' to all its non-main threads.
    shutdown :: EngineState s -> IO Bool
    shutdown es = do
        putStrLn "Shutdown"
        _ <- forM (S.toList (getThreads es)) (\tid -> CE.throwTo tid Shutdown)
        terminateGame

    -- | Compile the given shaders into programs.
    makeShaderPrograms :: FilePath -> IO (Map String Program)
    makeShaderPrograms resDir = do
        M.fromList <$> forM (M.toList programPaths)
                            (\(name, (vertexShaderPath, fragmentShaderPath)) -> do
                                  let vertexShaderPath' = resDir </> vertexShaderPath
                                      fragmentShaderPath' = resDir </> fragmentShaderPath
                                  program <- makeShaderProgram vertexShaderPath' fragmentShaderPath'
                                  return (name, program))

    loadFonts :: FilePath -> IO (Map String GLFont)
    loadFonts resDir = do
        M.fromList <$> forM (M.toList fontPaths)
                            (\(name, fontPath) -> do
                                  let fontPath' = resDir </> fontPath
                                      vertexShaderPath = resDir </> "textVertex.glsl"
                                      fragmentShaderPath = resDir </> "textFragment.glsl"
                                  font <- loadFontFromImage fontPath' vertexShaderPath fragmentShaderPath
                                  return (name, font))
