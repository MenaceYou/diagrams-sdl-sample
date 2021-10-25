{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTSyntax #-}

import qualified SDL as SDL
import Foreign.Ptr ( castPtr, nullPtr )
import qualified Graphics.Rendering.Cairo as Cairo
import qualified SDL.Input.Keyboard.Codes as SDL
-- diagrams-cairo
import Diagrams.Backend.Cairo as Cairo

-- diagrams
import Diagrams.Prelude hiding (view, Vector, (*^), (^+^), (^-^), signorm)
import Diagrams.TwoD.Text (text)

-- base
import Data.Int (Int32)
import Data.Word (Word8)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar, modifyMVar_) -- (newIORef, readIORef, writeIORef, modifyIORef)
import Control.Monad (forM, forM_, replicateM)
import Data.Maybe (listToMaybe)
import Data.Complex
import Data.List (intersect, sortOn, groupBy)
import Data.Tuple (swap)

-- palette
-- import Data.Colour.Palette.ColorSet

type Vector = (Double, Double)
type Position = (Double, Double)

----------------------------------------
-- 各種パラメタ
----------------------------------------

gravity :: Vector
gravity = (0, -0.05)

aDash :: Double
aDash = 0.1

jump :: Vector
jump = (0, 0.7)

k :: Double
k = 0.1

vMax :: Double
vMax = 0.99

vxMax :: Double
vxMax = 0.3

roundVelocity :: Vector -> Vector
roundVelocity v
    | normV v > vMax = vMax *^ signorm v
    | otherwise      = v

----------------------------------------
-- 補助関数
----------------------------------------

projectX ::Vector -> Vector
projectX (x, y) = (x, 0)

projectY ::Vector -> Vector
projectY (x, y) = (0, y)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (l, r) = (f l, f r)

(^+^) :: Vector -> Vector -> Vector
(x1, y1) ^+^ (x2, y2) = (x1 + x2, y1 + y2)

negateV :: Vector -> Vector
negateV v = (-1) *^ v

(^-^) :: Vector -> Vector -> Vector
v1 ^-^ v2 = v1 ^+^ negateV v2

normV :: Vector -> Double
normV (x, y) = sqrt (x^2 + y^2)

signorm :: Vector -> Vector
signorm v
    | absV == 0 = (0, 0)
    | otherwise = (1 / absV) *^ v
 where absV = normV v

(*^) :: Double -> Vector -> Vector
a *^ (x, y) = (a * x, a * y)

isGrounded :: Position -> Bool
isGrounded (x, y) = not $ null $ map (, y - 1) [floor x, ceiling x] `intersect` stage'
 where stage' :: [(Int, Double)]
       stage' = map (\ (x, y) -> (x, fromIntegral y)) stage

calcNextPos
    :: (Double, Double) -- p
    -> (Double, Double) -- v
    -> ((Double, Double), (Bool, Bool)) -- p', (isColideX, isColideY)
calcNextPos p v
    | normByX == normByY = (posColideX, (isColideX, isColideY))
    | normByX < normByY  = (posColideX, (isColideX, False))
    | otherwise          = (posColideY, (False, isColideY))
 where (normByX, posColideX, isColideX) = calcNextPos' False p v
       (normByY, posColideY, isColideY) = calcNextPos' True p v
       calcNextPos'
           :: Bool
           -> (Double, Double) -- p
           -> (Double, Double) -- v
           -> (Double, (Double, Double), Bool) -- |v|, p'
       calcNextPos' isSwap pos vec
--            |  truncate x == truncate (x + vx)
           | isNotColide              = (normV v, mSwap $ p ^+^ v, False)
           | otherwise                 = (normV v', mSwap $ p ^+^ v', True)
        where p@(x, y) = mSwap pos
              v@(vx, vy) = mSwap vec
              mSwap
                  | isSwap = swap
                  | otherwise = id
              x' :: Int
              x' = truncate (x + vx)
              vx' = fromIntegral x' - x
              r
                  | vx == 0   = 1
                  | otherwise = abs $ vx' / vx
              v'@(_, vy') = r *^ v
              y' = y + vy'
              isNotColide = null $ bs `intersect` stage
               where bs = map (mSwap . (next x',)) [floor y', ceiling y']
              truncate :: Double -> Int
              truncate
                  | 0 < vx = floor
                  | otherwise = ceiling
              next :: Int -> Int
              next
                  | 0 < vx = (+1)
                  | otherwise = subtract 1

----------------------------------------
-- Model, view, update
----------------------------------------

data Model = MkModel Position Vector

data Model where
  MkModel :: Position -> Vector -> Model

playerPos :: Model -> Position
playerPos (MkModel pos vec) = pos
velocity :: Model -> Vector
velocity (MkModel pos vec) = vec

initialModel :: Model
initialModel = MkModel (5, 15) (0, 0)

stage :: [(Int, Int)]
stage = concat $ take 3 $ iterate (map (\ (x, y) -> (x + 2, y + 10))) stage'
 where stage' = concat
           [ map (, 7) $ concat $ take 10 $ iterate (map (+10)) [5, 6, 7] -- 底
           , map (, 4) $ concat $ take 10 $ iterate (map (+10)) [0, 1, 2] -- 底
           , map (, 2) $ concat $ take 10 $ iterate (map (+10)) [5, 6, 7] -- 底
           , map (, 0) $ concat $ take 10 $ iterate (map (+8)) [0, 1, 2, 3, 4, 5] -- 底
           , concat [map (x, ) [1, 2, 3, 4] | x <- [1, 10, 20, 30]] -- 壁
           ]

updateWithTimer :: (SDL.Scancode -> Bool) -> Model -> Model
updateWithTimer isPressed model = MkModel playerPos' (roundX $ roundY $ velocity')
 where isLeftPressed :: Bool
       isLeftPressed = isPressed SDL.ScancodeLeft
       isRightPressed :: Bool
       isRightPressed = isPressed SDL.ScancodeRight
       isUpPressed :: Bool
       isUpPressed = isPressed SDL.ScancodeUp
       isLanded :: Bool
       isLanded = isGrounded $ playerPos model

       (playerPos', (isColideX, isColideY)) = calcNextPos (playerPos model) velocity'

       roundX (vx, vy)
           | isColideX = (0, vy)
           | otherwise = (vx, vy)

       roundY (vx, vy)
           | isColideY = (vx, 0)
           | otherwise = (vx, vy)

       velocity' :: Vector
       velocity' = roundVelocity $ friction $ velocity model ^+^ aG ^+^ aL ^+^ aR ^+^ aJ

       friction :: Vector -> Vector
       friction v@(vx, vy)
           | haveFriction && abs vx < k = (0, vy)
           | haveFriction               = v ^-^ (k *^ (signorm $ projectX v))
           | otherwise                  = v
        where  haveFriction = and
                   [ not isLeftPressed
                   , not isRightPressed
                   , isLanded
                   ]
       
       aL :: Vector
       aL
           | isLeftPressed && isLanded && -vxMax < vx = (- aDash, 0)
           | otherwise                  = (0, 0)
        where (vx, _) = velocity model

       aR :: Vector
       aR
           | isRightPressed && isLanded && vx < vxMax = (aDash, 0)
           | otherwise                  = (0, 0)
        where (vx, _) = velocity model

       aG :: Vector
       aG
           | not $ isLanded             = gravity
           | otherwise                  = (0, 0)

       aJ :: Vector
       aJ
           | isUpPressed && isLanded    = jump
           | otherwise                  = (0, 0)

view :: Model -> SelectableDiagram
view (MkModel playerPos _)
    = scale 20
    $ value []
    $ translateV (negateV playerPos)
    $ mconcat
    $ map drawOneBlock
    $ playerPos : stage'
 where drawOneBlock :: Position -> NormalDiagram
       drawOneBlock v = translateV v r
       translateV :: Position -> NormalDiagram -> NormalDiagram
       translateV (x, y) = translate (V2 x y)
       r = rect 1 1
       stage' = map (mapPair fromIntegral) stage

updateWithClick :: String -> Model -> Model
updateWithClick button model = model

-- updateWithKeyPress :: SDL.Keycode -> Model -> Model
-- updateWithKeyPress SDL.KeycodeUp (MkModel pos vec)
--     | isGrounded pos = MkModel pos $ vec ^+^ jump
--     | otherwise      = MkModel pos vec
updateWithKeyPress :: SDL.Keycode -> Model -> Model
updateWithKeyPress _ model = model

----------------------------------------
-- GUIのあれこれ
----------------------------------------

type NormalDiagram = Diagram V2

type GenericDiagram a = QDiagram V2 Double a

type SelectableDiagram = GenericDiagram [String]

-- rasterize :: SizeSpec V2 Int -> Diagram V2 -> Diagram V2
-- rasterize sz d = sizedAs d $ imageEmb $ ImageRGBA8 $ renderImage sz d

modifyMVarPure_ :: MVar a -> (a -> a) -> IO ()
modifyMVarPure_ var f = modifyMVar_  var $ return . f

value :: Monoid m => m -> QDiagram v n Any -> QDiagram v n m
value m = fmap fromAny
  where fromAny (Any True)  = m
        fromAny (Any False) = mempty

resetValue :: (Eq m, Monoid m) => QDiagram v n m -> QDiagram v n Any
resetValue = fmap toAny
  where toAny m | m == mempty = Any False
                | otherwise   = Any True

clearValue :: QDiagram v n m -> QDiagram v n Any
clearValue = fmap (const (Any False))

fullHDRect :: NormalDiagram
fullHDRect = rect screenWidth screenHeight # fc white

screenWidth :: Num a => a
screenWidth = 800
screenHeight :: Num a => a
screenHeight = 600

main :: IO ()
main = do
    -- 編集の初期化
    vModel <- newMVar initialModel
    vRender <- newMVar $ view initialModel
    -- SDL初期化
    SDL.initialize [ SDL.InitVideo ]
    window <- SDL.createWindow
        "SDL / Cairo Example"
        SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 screenWidth screenHeight}
    SDL.showWindow window
    
    screenSdlSurface <- SDL.getWindowSurface window

    sdlSurface <- SDL.createRGBSurface (SDL.V2 screenWidth screenHeight) SDL.ARGB8888
    buffer <- fmap castPtr $ SDL.surfacePixels sdlSurface
    cairoSurface <- Cairo.createImageSurfaceForData buffer Cairo.FormatRGB24 screenWidth screenHeight (screenWidth * 4)

    SDL.updateWindowSurface window

    -- Userイベントの登録
    mRegisteredEventType <- SDL.registerEvent decodeUserEvent encodeUserEvent
    let pushCustomEvent :: CustomEvent -> IO ()
        pushCustomEvent userEvent = forM_ mRegisteredEventType $ \ regEventType -> SDL.pushRegisteredEvent regEventType userEvent
        getCustomEvent :: SDL.Event -> IO (Maybe CustomEvent)
        getCustomEvent event = case mRegisteredEventType of
            Nothing -> return $ Nothing
            Just regEventType -> SDL.getRegisteredEvent regEventType event

    -- 定周期の処理
    _ <- SDL.addTimer 33 $ const $ do
        isPressed <- SDL.getKeyboardState
        modifyMVarPure_ vModel $ updateWithTimer isPressed
        pushCustomEvent CustomExposeEvent
        return $ SDL.Reschedule 33

    pushCustomEvent CustomExposeEvent
    
    -- Eventハンドラ
    let loop :: IO ()
        loop = do
            event <- SDL.waitEvent
            mUserEvent <- getCustomEvent event
            forM_ mUserEvent $ \case
                CustomExposeEvent -> do
                    model <- readMVar vModel
--                     putStrLn $ show $ triangleClickCount model
                    let selectableDiagram :: SelectableDiagram
                        selectableDiagram = toSDLCoord $ view model

                    SDL.surfaceFillRect sdlSurface Nothing whiteRect
                    Cairo.renderWith cairoSurface $ Cairo.toRender mempty $ clearValue selectableDiagram
                    SDL.surfaceBlit sdlSurface Nothing screenSdlSurface Nothing

                    SDL.updateWindowSurface window
                    swapMVar vRender selectableDiagram
                    return ()
            case SDL.eventPayload event of
                SDL.MouseButtonEvent SDL.MouseButtonEventData{..} -> do
                    case mouseButtonEventMotion of
                        SDL.Pressed -> do
                            selectableDiagram <- readMVar vRender
                            let mClickedObj = listToMaybe $ reverse $ sample selectableDiagram $ toFloatingPoint $ mouseButtonEventPos
                            case mClickedObj of
                                Nothing -> return ()
                                Just obj -> modifyMVarPure_ vModel $ updateWithClick obj
                            pushCustomEvent CustomExposeEvent
                            loop
                        _           -> loop
                SDL.KeyboardEvent SDL.KeyboardEventData{..} | keyboardEventKeyMotion == SDL.Pressed -> do
                    let SDL.Keysym _ key SDL.KeyModifier{..} = keyboardEventKeysym
                    modifyMVarPure_ vModel $ updateWithKeyPress key
                    pushCustomEvent CustomExposeEvent
                    loop
                SDL.QuitEvent       -> return ()
                _                   -> loop
    loop
    putStrLn "Exitting"

data CustomEvent = CustomExposeEvent

decodeUserEvent :: SDL.RegisteredEventData -> SDL.Timestamp -> IO (Maybe CustomEvent)
decodeUserEvent SDL.RegisteredEventData{..} _ = case registeredEventCode of
    0 -> return $ Just CustomExposeEvent
    _ -> return Nothing

encodeUserEvent :: CustomEvent -> IO SDL.RegisteredEventData
encodeUserEvent CustomExposeEvent = return $ SDL.RegisteredEventData Nothing 0 nullPtr nullPtr

toSDLCoord :: SelectableDiagram -> SelectableDiagram
toSDLCoord = translate (V2 (screenWidth / 2) (screenHeight / 2)) . reflectY

toFloatingPoint :: Point V2 Int32 -> Point V2 Double
toFloatingPoint p = fmap fromIntegral p

whiteRect :: SDL.V4 Word8
whiteRect = SDL.V4 maxBound maxBound maxBound maxBound

alphaRect :: SDL.V4 Word8
alphaRect = SDL.V4 maxBound maxBound maxBound minBound

