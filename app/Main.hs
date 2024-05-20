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

-- palette
import Data.Colour.Palette.ColorSet

-- lens, generic-lens
-- import Control.Lens()
-- import Data.Generics.Labels()

-- Model, view, update

type Position = (Double, Double)
sq :: NormalDiagram
sq = rect 1 1

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
fromTetriminoToBlocks BlockZ = [(-1, -1), (0, 1), (0, 0), (1, 0)]
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
--comment

reify :: Tetrimino -> Position -> Orientation -> [Position]
reify t (x,  y) o = map shift $ map (rotateByOrientation o) $ fromTetriminoToBlocks t
 where
    shift :: Position -> Position
    shift (a, b) = (a + x, b + y)


initialModel :: Model
initialModel = Model
    { currentTetrimino = BlockT
    , currentCursorPos = (5, 19)
    , currentOrientation = Orient0
    , currentBlocks = [(x, 0) | x <- [0..11]] ++ [(0, y) | y <- [0..20]] ++ [(11, y) | y <- [0..20]]
    , score = 0
    }
renderBlocks :: [Position] -> NormalDiagram
renderBlocks bs = mconcat $ map renderBlock bs
 where 
    renderBlock :: Position -> NormalDiagram
    renderBlock (x, y) = translate (V2 x y) sq

view :: Model -> SelectableDiagram
view Model{..} = toSDLCoord $ center $ scale 20 $ hsep 3
    [ controlPanel
    , value [] $ renderBlocks $ currentBlocks ++ reify currentTetrimino currentCursorPos currentOrientation
            --, rect 1 1 # fc blue # value ["square"]    
    ]


controlPanel :: SelectableDiagram
controlPanel = center $ vsep 0.2
    [button "up"
    ,center $ hsep 0.2
        [button "left"
        ,button "down"
        ,button "right"
        ]
    ]

    where button str = square 1 # value [str]



updateWithClick :: String -> Model -> Model
updateWithClick "left" (Model t (x, y) o bs s) = Model t (x - 1, y) o bs s
updateWithClick "right" (Model t (x, y) o bs s) = Model t (x + 1, y) o bs s
updateWithClick "down" (Model t (x, y) o bs s) = Model t (x, y - 1) o bs s
updateWithClick "up" (Model t (x, y) o bs s) = Model t (x, y) (incrementOrientation o) bs s
updateWithClick _ model = model

incrementOrientation :: Orientation -> Orientation
incrementOrientation Orient0 = Orient90
incrementOrientation Orient90 = Orient180
incrementOrientation Orient180 = Orient270
incrementOrientation Orient270 = Orient0


-- ���̑�

updateWithTimer :: Model -> Model
updateWithTimer model = model

fullHDRect :: NormalDiagram
fullHDRect = rect screenWidth screenHeight # fc white

screenWidth :: Num a => a
screenWidth = 800
screenHeight :: Num a => a
screenHeight = 600

main :: IO ()
main = do
    putStrLn "Starting"
    -- �ҏW�̏�����
    vModel <- newIORef initialModel
    vRender <- newIORef $ view initialModel
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
