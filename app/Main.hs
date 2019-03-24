{-# LANGUAGE StrictData #-}
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
import Diagrams.TwoD.Image (imageEmb)

-- diagrams-cairo
import Diagrams.Backend.Cairo

-- diagrams-rasterific
import Diagrams.Backend.Rasterific (renderImage)

-- JuicyPixels
import Codec.Picture.Types (DynamicImage(..))

import Data.Time
import Data.Text (Text)
import TextShow
-- base
import Data.Int (Int32)
import Data.Word (Word8)
import Data.IORef (newIORef, writeIORef, readIORef, modifyIORef)
import Foreign.Ptr (nullPtr)
import Control.Monad (forM_)

-- safe
import Safe (headMay)

-- palette
import Data.Colour.Palette.ColorSet

type NormalDiagram = Diagram V2

type GenericDiagram a = QDiagram V2 Double a

type SelectableDiagram = GenericDiagram [Text]

rasterize :: SizeSpec V2 Int -> Diagram V2 -> Diagram V2
rasterize sz d = sizedAs d $ imageEmb $ ImageRGBA8 $ renderImage sz d

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
        [ value [] $ text ("Clock count: " <> showt clockCount) <> phantom (rect 10 1 :: NormalDiagram)
        , value [] $ text ("Triangle click count: " <> showt triangleClickCount) <> phantom (rect 10 1 :: NormalDiagram)
        , value [] $ text ("Square click count: " <> showt squareClickCount) <> phantom (rect 10 1 :: NormalDiagram)
        , hsep 1
            [ triangle 1 # fc red # value ["triangle"]
            , rect 1 1 # fc blue # value ["square"]
            ]
        ]
    , sized (mkHeight screenHeight) $ center $ vcat $ replicate triangleClickCount $ hcat $ replicate triangleClickCount $ sampleCircle # fc (d3Colors2 Dark triangleClickCount) # value []
    ]

 where sampleText :: NormalDiagram
       sampleText =  text "a" <> phantom (rect 1 1 :: NormalDiagram)
       sampleTri :: NormalDiagram
       sampleTri = triangle 1
       sampleCircle :: NormalDiagram
       sampleCircle = circle 1

updateWithClick :: Text -> Model -> Model
updateWithClick "triangle" Model{..} = Model clockCount (triangleClickCount + 1) squareClickCount
updateWithClick "square" Model{..}   = Model clockCount triangleClickCount (squareClickCount + 1)
updateWithClick _ model              = model

updateWithTimer :: Model -> Model
updateWithTimer Model{..} = Model (clockCount + 1) triangleClickCount squareClickCount

sampleRasterImage :: NormalDiagram
sampleRasterImage = rasterize (mkWidth 100) $ text "a" # fc green <> phantom (rect 1 1 :: NormalDiagram)

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
    
    screenSurface <- SDL.getWindowSurface window
    pixels <- fmap castPtr $ SDL.surfacePixels screenSurface
    canvas <- Cairo.createImageSurfaceForData pixels Cairo.FormatRGB24 screenWidth screenHeight (screenWidth * 4)

--     bufferSurface <- Cairo.createImageSurface Cairo.FormatRGB24 screenWidth screenHeight (screenWidth * 4)
--     Cairo.renderWith canvas demo3
--     Cairo.withImageSurfaceForData pixels Cairo.FormatRGB24 600 600 (600 * 4) $ \canvas -> do
--         Cairo.renderWith canvas demo3

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
                    let selectableDia = view model
                    SDL.surfaceFillRect screenSurface Nothing whiteRect
                    Cairo.renderWith canvas $ toRender mempty $ clearValue selectableDia
--                     SDL.surfaceBlit bufferSurface Nothing screenSurface Nothing
                    SDL.updateWindowSurface window
                    writeIORef vRender selectableDia
            case SDL.eventPayload event of
                SDL.MouseButtonEvent SDL.MouseButtonEventData{..} -> do
                    case mouseButtonEventMotion of
                        SDL.Pressed -> do
                            selectableDia <- readIORef vRender
                            let mClickedObj = headMay $ reverse $ sample selectableDia $ toFloatingPoint $ mouseButtonEventPos
                            mapM_ (modifyIORef vModel . updateWithClick) mClickedObj
                            pushCustomEvent CustomExposeEvent
                            loop
                        _           -> loop
                SDL.QuitEvent       -> return ()
                _                   -> loop
--             let (_, _, r) = renderDiaT (mkOptions $ mkWidth 600 :: Options Cairo) $ a n
--             SDL.surfaceFillRect screenSurface Nothing whiteRect
--             measuringTime $ Cairo.renderWith canvas r
--             if SDL.QuitEvent `elem` map SDL.eventPayload events
--                 then return ()
--                 else loop $ n + 1
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

measuringTime :: IO () -> IO ()
measuringTime io = do
     t1 <- getCurrentTime
     io
     t2 <- getCurrentTime
     let diffTime = diffUTCTime t2 t1
     putStrLn $ show diffTime
