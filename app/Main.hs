{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- palette
import Data.Colour.Palette.ColorSet

type NormalDiagram = Diagram V2

type GenericDiagram a = QDiagram V2 Double a

type SelectableDiagram = GenericDiagram [String]

-- rasterize :: SizeSpec V2 Int -> Diagram V2 -> Diagram V2
-- rasterize sz d = sizedAs d $ imageEmb $ ImageRGBA8 $ renderImage sz d

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

--

data Model = Model
    { clockCount :: Int
    , triangleClickCount :: Int
    , squareClickCount :: Int
    }

initialModel :: Model
initialModel = Model 0 0 0

view :: Model -> SelectableDiagram
view Model{..} = toSDLCoord $ mconcat
    [ scale 50 $ center $ vsep 1
        [ value [] $ text ("Clock count: " <> show clockCount) <> phantom (rect 10 1 :: NormalDiagram)
        , value [] $ text ("Triangle click count: " <> show triangleClickCount) <> phantom (rect 10 1 :: NormalDiagram)
        , value [] $ text ("Square click count: " <> show squareClickCount) <> phantom (rect 10 1 :: NormalDiagram)
        , hsep 1
            [ triangle 1 # fc red # value ["triangle"]
            , rect 1 1 # fc blue # value ["square"]
            ]
        ]
    , sized (mkHeight screenHeight) $ center $ vcat $ replicate triangleClickCount $ hcat $ replicate triangleClickCount $ circle 1 # fc (d3Colors2 Dark clockCount) # value []
    ]

updateWithClick :: String -> Model -> Model
updateWithClick "triangle" Model{..} = Model clockCount (triangleClickCount + 1) squareClickCount
updateWithClick "square" Model{..}   = Model clockCount triangleClickCount (squareClickCount + 1)
updateWithClick _ model              = model

updateWithTimer :: Model -> Model
updateWithTimer Model{..} = Model (clockCount + 1) triangleClickCount squareClickCount

-- sampleRasterImage :: NormalDiagram
-- sampleRasterImage = rasterize (mkWidth 100) $ text "a" # fc green <> phantom (rect 1 1 :: NormalDiagram)

fullHDRect :: NormalDiagram
fullHDRect = rect screenWidth screenHeight # fc white

screenWidth :: Num a => a
screenWidth = 800
screenHeight :: Num a => a
screenHeight = 600

main :: IO ()
main = do
    -- 編集の初期化
    vModel <- newIORef initialModel
    vRender <- newIORef $ view initialModel
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
    _ <- SDL.addTimer 1000 $ const $ do
        modifyIORef vModel $ updateWithTimer
        pushCustomEvent CustomExposeEvent
        return $ SDL.Reschedule 1000

    pushCustomEvent CustomExposeEvent
    
    -- Eventハンドラ
    let loop :: IO ()
        loop = do
            event <- SDL.waitEvent
            mUserEvent <- getCustomEvent event
            forM_ mUserEvent $ \case
                CustomExposeEvent -> do
                    model <- readIORef vModel
                    putStrLn $ show $ triangleClickCount model
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

