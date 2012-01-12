module Main where

import qualified Graphics.UI.SDL as S
import qualified Graphics.UI.SDL.Image as S
import qualified Physics.Hipmunk as H
import Data.StateVar
import System.FilePath
import Text.Printf
import Control.Monad (forM)

resources :: FilePath
resources = "./resources"

(+:) :: H.CpFloat -> H.CpFloat -> H.Vector
(+:) = H.Vector

sdlGary :: S.Surface -> IO ()
sdlGary screen = do
    -- Load image
    gary <- S.loadTyped (resources </> "Gary.jpg") S.JPG
    -- Display it
    S.blitSurface gary Nothing screen Nothing
    S.flip screen

    S.delay 2000

hipmunkHello :: IO ()
hipmunkHello = do
    space <- H.newSpace
    let gravity = 0 +: (-100)
    H.gravity space $= gravity
    static <- H.newBody H.infinity H.infinity
    ground <- H.newShape static (H.LineSegment ((-20) +: 5) (20 +: 5) 0) 0
    H.friction ground $= 1
    H.spaceAdd space ground
    let radius = 5
        mass   = 1
        moment = H.momentForCircle mass (0, radius) (0 +: 0)
    ballBody <- H.newBody mass moment
    H.spaceAdd space ballBody
    H.position ballBody $= 0 +: 15
    ballShape <- H.newShape ballBody (H.Circle radius) (0 +: 0)
    H.spaceAdd space ballShape
    H.friction ballShape $= 0.7
    let timeStep = 1.0/60.0
    forM [0,timeStep..2] $ \time -> do
        H.Vector px py <- get $ H.position ballBody
        H.Vector vx vy <- get $ H.velocity ballBody
        printf ("Time is %5.2f. ballBody is at (%5.2f, %5.2f). It's velocity " ++
                "is (%5.2f, %5.2f)\n") time px py vx vy
        H.step space timeStep
    H.freeSpace space

main :: IO ()
main = do
    -- SDL
    S.init [S.InitEverything]
    screen <- S.setVideoMode 640 480 32 [S.SWSurface]
    sdlGary screen

    -- Hipmunk
    H.initChipmunk
    hipmunkHello

    -- Clean up
    S.freeSurface screen
    S.quit
