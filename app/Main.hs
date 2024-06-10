import GHC.Generics (Generic)

import qualified SDL as SDL
import Foreign.Ptr ( castPtr )
import qualified Graphics.Rendering.Cairo as Cairo

-- diagrams
import Diagrams.Prelude hiding (view)
import Diagrams.TwoD.Text (text)

-- diagrams-cairo
import Diagrams.Backend.Cairo as Cairo

-- base
import Data.Int (Int32)
import Data.Word (Word8)
import Data.IORef (newIORef, writeIORef, readIORef, modifyIORef)
import Foreign.Ptr (nullPtr)
import Control.Monad (forM, forM_, replicateM)
import Data.Maybe (listToMaybe)
import Data.Complex
import Data.List

-- palette
import Data.Colour.Palette.ColorSet
import System.Random
import Data.Time.Clock.POSIX
import Data.Time
import Data.Int

-- lens, generic-lens
-- import Control.Lens()
-- import Data.Generics.Labels()

-- Model, view, update

type Position = (Double, Double)
sq :: NormalDiagram
sq = rect 1 1 # lc black # fc gray

data Tetrimino
    = BlockO
    | BlockI
    | BlockT
    | BlockL
    | BlockJ
    | BlockZ
    | BlockS
    deriving (Generic, Show)

data Orientation
    = Orient0
    | Orient90
    | Orient180
    | Orient270
    deriving (Generic, Show)

data Model = Model
    { currentTetrimino :: Tetrimino
    , currentCursorPos :: Position
    , currentOrientation :: Orientation
    , currentBlocks :: [Position]
    , score :: Int
    } deriving (Generic, Show)

fromTetriminoToBlocks :: Tetrimino -> [Position]
fromTetriminoToBlocks BlockO = [(0, 0), (0, 1), (1, 0), (1, 1)]
fromTetriminoToBlocks BlockI = [(0, 2), (0, 1), (0, 0), (0, -1)]
fromTetriminoToBlocks BlockT = [(-1, 0), (0, 0), (1, 0), (0, -1)]
fromTetriminoToBlocks BlockL = [(0, 2), (0, 1), (0, 0), (1, 0)]
fromTetriminoToBlocks BlockJ = [(0, 2), (0, 1), (0, 0), (-1, 0)]
fromTetriminoToBlocks BlockZ = [(-1, 1), (0, 1), (0, 0), (1, 0)]
fromTetriminoToBlocks BlockS = [(1, 1), (0, 1), (0, 0), (-1, 0)]

rotateByOrientation :: Orientation -> Position -> Position
rotateByOrientation Orient0 pos = pos
rotateByOrientation Orient90 pos = rotate90 pos
rotateByOrientation Orient180 pos = rotate90 $ rotate90 pos
rotateByOrientation Orient270 pos = rotate90 $ rotate90 $ rotate90 pos



rotate90 :: Position -> Position
rotate90 (x, y) = (x', y')
 where
    x' :+ y' = (x :+ y) * (0 :+ 1)

reify :: Model -> [Position]
reify (Model t (x,  y) o bs _) = concat
    [ reifyTetrimino t (x, y) o, bs ]


reifyTetrimino :: Tetrimino -> Position -> Orientation -> [Position]
reifyTetrimino t (x, y) o = map shift $ map (rotateByOrientation o) $ fromTetriminoToBlocks t
 where
    shift :: Position -> Position
    shift (a, b) = (a + x, b + y)

sourcePosition :: Position
sourcePosition = (5, 19)

blockChooser :: [Int] -> Tetrimino
blockChooser l =
    case l of
        [] -> BlockT
        r:rs -> (case r of
            1 -> BlockO
            2 -> BlockI
            3 -> BlockT
            4 -> BlockL
            5 -> BlockJ
            6 -> BlockZ
            7 -> BlockS
            _ -> BlockT)

initialModel :: Int -> Model
initialModel n = Model
    --{ currentTetrimino = BlockT
    --, currentCursorPos = sourcePosition
    --, currentOrientation = Orient0
    --, currentBlocks = nub $ [(x, 0) | x <- [0..11]] ++ [(0, y) | y <- [0..20]] ++ [(11, y) | y <- [0..20]]
    --, score = 0
    --}

    --{ currentTetrimino = chooser (take 1 $ randomRs (1, 7) (mkStdGen 124))
    { currentTetrimino = blockChooser [n]
    , currentCursorPos = sourcePosition
    , currentOrientation = Orient0
    , currentBlocks = nub $ [(x, 0) | x <- [0..11]] ++ [(0, y) | y <- [0..20]] ++ [(11, y) | y <- [0..20]]
    , score = 0
    }
        
renderBlocks :: [Position] -> NormalDiagram
renderBlocks bs = mconcat $ map renderBlock bs
 where 
    renderBlock :: Position -> NormalDiagram
    renderBlock (x, y) = translate (V2 x y) sq

view :: Model -> SelectableDiagram
view model = toSDLCoord $ center $ scale 20 $ hsep 3
    [ controlPanel model
    , value [] $ renderBlocks $ reify model
    ]

controlPanel :: Model -> SelectableDiagram
controlPanel model = center $ vsep 1
    [value [] $ text ("score:" <> show (score model))
    ,center $ hsep 0.2
        [button "orient"
        ,center $ hsep 0.2
            [button "left"
            ,button "down"
            ,button "right"
            ]
        ]
    ]


    where button str = square 1 # lc black # fc gray # value [str]


hasDuplication :: [Position] -> Bool
hasDuplication ps = any ((>1) . length) $ group $ sort ps


updateWithClick :: String -> Model -> Model
updateWithClick button (Model t (x, y) o bs s)
    | hasDuplication (reify model') = Model t (x, y) o bs s
    | otherwise                     = model'
 where
    model' = case button of
        "left"   -> Model t (x - 1, y) o bs s
        "right"  -> Model t (x + 1, y) o bs s
        "down"   -> Model t (x, y - 1) o bs s
        "orient" -> Model t (x, y) (incrementOrientation o) bs s
        _        -> Model t (x, y) o bs s


incrementOrientation :: Orientation -> Orientation
incrementOrientation Orient0 = Orient90
incrementOrientation Orient90 = Orient180
incrementOrientation Orient180 = Orient270
incrementOrientation Orient270 = Orient0


-- ���̑�
updateWithTimer :: Model -> Model
updateWithTimer (Model t (x, y) o bs s)
    | hasDuplication (reify model') = Model t sourcePosition o (bs ++ piledBlocks) s
    | otherwise                     = model'
 where
    model' = Model t (x, y - 1) o bs s
    piledBlocks = reifyTetrimino t (x, y) o

{-updateWithTimer :: Int -> Model -> Model
updateWithTimer pr (Model _ (x, y) o bs s)
    | hasDuplication (reify model') = Model nextTetrimino sourcePosition o (bs ++ piledBlocks) s
    | otherwise                     = model'
 where
    nextTetrimino = blockChooser [pr]
    model' = Model nextTetrimino (x, y - 1) o bs s
    piledBlocks = reifyTetrimino nextTetrimino (x, y) o
-}

fullHDRect :: NormalDiagram
fullHDRect = rect screenWidth screenHeight # fc white

screenWidth :: Num a => a
screenWidth = 800
screenHeight :: Num a => a
screenHeight = 600

cnv2sec :: UTCTime -> Int
cnv2sec = floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

pickRand :: [Int] -> (Int,[Int])  
pickRand box = ((head box), (tail box))

main :: IO ()
main = do
    putStrLn "Starting"
    timeNow <- getCurrentTime
    let seed = cnv2sec timeNow

    --let randBox = randomRs (1, 7) (mkStdGen seed)
    vRandBox <- newIORef $ randomRs (1, 7) (mkStdGen seed)

    -- �ҏW�̏�����
    randBox <- readIORef vRandBox
    let (rand, sndRand) = pickRand randBox
    writeIORef vRandBox sndRand
    vModel <- newIORef $ initialModel rand
    vRender <- newIORef $ view (initialModel rand)
    -- SDL������
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

    -- User�C�x���g�̓o�^
    mRegisteredEventType <- SDL.registerEvent decodeUserEvent encodeUserEvent
    let pushCustomEvent :: CustomEvent -> IO ()
        pushCustomEvent userEvent = forM_ mRegisteredEventType $ \ regEventType -> SDL.pushRegisteredEvent regEventType userEvent
        getCustomEvent :: SDL.Event -> IO (Maybe CustomEvent)
        getCustomEvent event = case mRegisteredEventType of
            Nothing -> return $ Nothing
            Just regEventType -> SDL.getRegisteredEvent regEventType event

    -- ������̏���
    _ <- SDL.addTimer 1000 $ const $ do
        --xRandBox <- readIORef vRandBox
        --let (rand', _) = pickRand xRandBox
        --modifyIORef vModel $ updateWithTimer rand'
        modifyIORef vModel $ updateWithTimer
        pushCustomEvent CustomExposeEvent
        return $ SDL.Reschedule 1000

    pushCustomEvent CustomExposeEvent
    
    -- Event�n���h��
    let loop :: IO ()
        loop = do
            event <- SDL.waitEvent
            mUserEvent <- getCustomEvent event
            forM_ mUserEvent $ \ case
                CustomExposeEvent -> do
                    randBox <- readIORef vRandBox
                    let (_, sndRand) = pickRand randBox
                    _ <- writeIORef vRandBox sndRand

                    model <- readIORef vModel
                    --putStrLn $ show $ triangleClickCount model
                    let selectableDiagrams = view model

                    SDL.surfaceFillRect sdlSurface Nothing whiteRect
                    Cairo.renderWith cairoSurface $ Cairo.toRender mempty $ clearValue selectableDiagrams
                    SDL.surfaceBlit sdlSurface Nothing screenSdlSurface Nothing

                    SDL.updateWindowSurface window
                    writeIORef vRender selectableDiagrams
            case SDL.eventPayload event of
                SDL.MouseButtonEvent SDL.MouseButtonEventData{..} -> do
                    case mouseButtonEventMotion of
                        SDL.Pressed -> do
                            selectableDiagram <- readIORef vRender
                            let mClickedObj = listToMaybe $ reverse $ sample selectableDiagram $ toFloatingPoint $ mouseButtonEventPos
                            mapM_ (modifyIORef vModel . updateWithClick) mClickedObj
                            putStrLn $ show mClickedObj
                            pushCustomEvent CustomExposeEvent
                            loop
                        _           -> loop
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

-- diagrams�֘A

type NormalDiagram = Diagram V2

type GenericDiagram a = QDiagram V2 Double a

type SelectableDiagram = GenericDiagram [String]

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
