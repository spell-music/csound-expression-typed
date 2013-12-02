module Csound.Typed.Gui.Widget(
    -- * Panels
    panel, tabs, panels, panelBy, tabsBy,

    -- * Types
    Input, Output, Inner,
    noInput, noOutput, noInner,
    Widget, widget, Source, source, Sink, sink, Display, display,

    -- * Widgets
    count, countSig, joy, knob, roller, slider, sliderBank, numeric, meter, box,
    button, buttonSig, butBank, butBankSig, toggle, toggleSig,
    value    
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class

import Csound.Dynamic

import Csound.Typed.Gui.Gui
import Csound.Typed.GlobalState
import Csound.Typed.Types

-- | Renders a list of panels.
panels :: [Gui] -> SE ()
panels = mapM_ panel

-- | Renders the GUI elements on the window. Rectangle is calculated
-- automatically.
panel :: Gui -> SE ()
panel = geToSe . saveGuiRoot . Single . Win "" Nothing 

-- | Renders the GUI elements with tabs. Rectangles are calculated
-- automatically.
tabs :: [(String, Gui)] -> SE ()
tabs = geToSe . saveGuiRoot . Tabs "" Nothing . fmap (\(title, gui) -> Win title Nothing gui)

-- | Renders the GUI elements on the window. We can specify the window title
-- and rectangle of the window.
panelBy :: String -> Maybe Rect -> Gui -> SE ()
panelBy title mrect gui = geToSe $ saveGuiRoot $ Single $ Win title mrect gui

-- | Renders the GUI elements with tabs. We can specify the window title and
-- rectangles for all tabs and for the main window.
tabsBy :: String -> Maybe Rect -> [(String, Maybe Rect, Gui)] -> SE ()
tabsBy title mrect gui = geToSe $ saveGuiRoot $ Tabs title mrect $ fmap (\(a, b, c) -> Win a b c) gui

-- | Widgets that produce something has inputs.
type Input  a = a

-- | Widgets that consume something has outputs.
type Output a = a -> SE ()

-- | Widgets that just do something inside them or have an inner state.
type Inner    = SE ()

-- | A value for widgets that consume nothing.
noOutput :: Output ()
noOutput = return 

-- | A value for widgets that produce nothing.
noInput :: Input ()
noInput  = ()

-- | A value for stateless widgets.
noInner :: Inner
noInner = return ()

-- | A widget consists of visible element (Gui), value consumer (Output) 
-- and producer (Input) and an inner state (Inner).
type Widget a b = SE (Gui, Output a, Input b, Inner)

-- | A consumer of the values.
type Sink   a = SE (Gui, Output a)

-- | A producer of the values.
type Source a = SE (Gui, Input a)

-- | A static element. We can only look at it.
type Display  = SE Gui

-- | A handy function for transforming the value of producers.
mapSource :: (a -> b) -> Source a -> Source b
mapSource f = fmap $ \(gui, ins) -> (gui, f ins) 

-- | A widget constructor.
widget :: SE (Gui, Output a, Input b, Inner) -> Widget a b
widget x = go =<< x
    where
        go :: (Gui, Output a, Input b, Inner) -> Widget a b
        go (gui, outs, ins, inner) = geToSe $ do     
            handle <- newGuiHandle
            appendToGui (GuiNode gui handle) (unSE inner)
            return (fromGuiHandle handle, outs, ins, inner)

-- | A producer constructor.
source :: SE (Gui, Input a) -> Source a
source x = fmap select $ widget $ fmap append x
    where 
        select (g, _, i, _) = (g, i)
        append (g, i) = (g, noOutput, i, noInner)

-- | A consumer constructor.
sink :: SE (Gui, Output a) -> Sink a
sink x = fmap select $ widget $ fmap append x
    where 
        select (g, o, _, _) = (g, o)
        append (g, o) = (g, o, noInput, noInner)

-- | A display constructor.
display :: SE Gui -> Display 
display x = fmap select $ widget $ fmap append x
    where 
        select (g, _, _, _) = g
        append g = (g, noOutput, noInput, noInner)        

-----------------------------------------------------------------------------  
-- primitive elements

setLabelSource :: String -> Source a -> Source a
setLabelSource a 
    | null a    = id
    | otherwise = fmap (first $ setLabel a)

setLabelSink :: String -> Sink a -> Sink a
setLabelSink a 
    | null a    = id
    | otherwise = fmap (first $ setLabel a)

singleOut :: Maybe Double -> Elem -> Source Sig 
singleOut v0 el = geToSe $ do
    (var, handle) <- newGuiVar
    let handleVar = guiHandleToVar handle
        inits = maybe [] (return . InitMe handleVar) v0
        gui = fromElem [var, handleVar] inits el
    appendToGui (GuiNode gui handle) (unSE noInner)
    return (fromGuiHandle handle, readSig var)

singleIn :: (GuiHandle -> Output Sig) -> Maybe Double -> Elem -> Sink Sig 
singleIn outs v0 el = geToSe $ do
    (_, handle) <- newGuiVar
    let handleVar = guiHandleToVar handle        
        inits = maybe [] (return . InitMe handleVar) v0
        gui = fromElem [handleVar] inits el
    appendToGui (GuiNode gui handle) (unSE noInner)
    return (fromGuiHandle handle, outs handle)

-- | A variance on the function 'Csound.Gui.Widget.count', but it produces 
-- a signal of piecewise constant function. 
countSig :: ValDiap -> ValStep -> Maybe ValStep -> Double -> Source Sig
countSig diap step1 mValStep2 v0 = singleOut (Just v0) $ Count diap step1 mValStep2

-- | Allows the user to increase/decrease a value with mouse 
-- clicks on a corresponding arrow button. Output is an event stream that contains 
-- values when counter changes.
-- 
-- > count diapason fineValStep maybeCoarseValStep initValue 
-- 
-- doc: http://www.csounds.com/manual/html/FLcount.html
count :: ValDiap -> ValStep -> Maybe ValStep -> Double -> Source (Evt D)
count diap step1 mValStep2 v0 = mapSource snaps $ countSig diap step1 mValStep2 v0

-- | It is a squared area that allows the user to modify two output values 
-- at the same time. It acts like a joystick. 
-- 
-- > joy valueSpanX valueSpanY (initX, initY) 
--
-- doc: <http://www.csounds.com/manual/html/FLjoy.html>
joy :: ValSpan -> ValSpan -> (Double, Double) -> Source (Sig, Sig)
joy sp1 sp2 (x, y) = geToSe $ do
    (var1, handle1) <- newGuiVar
    (var2, handle2) <- newGuiVar
    let handleVar1 = guiHandleToVar handle1
        handleVar2 = guiHandleToVar handle2
        outs  = [var1, var2, handleVar1, handleVar2]
        inits = [InitMe handleVar1 x, InitMe handleVar2 y]
        gui   = fromElem outs inits (Joy sp1 sp2)
    appendToGui (GuiNode gui handle1) (unSE noInner)
    return ( fromGuiHandle handle1, (readSig var1, readSig var2))

-- | A FLTK widget opcode that creates a knob.
--
-- > knob valueSpan initValue
--
-- doc: <http://www.csounds.com/manual/html/FLknob.html>
knob :: String -> ValSpan -> Double -> Source Sig
knob name sp v0 = setLabelSource name $ singleOut (Just v0) $ Knob sp

-- | FLroller is a sort of knob, but put transversally. 
--
-- > roller valueSpan step initVal
--
-- doc: <http://www.csounds.com/manual/html/FLroller.html>
roller :: String -> ValSpan -> ValStep -> Double -> Source Sig
roller name sp step v0 = setLabelSource name $ singleOut (Just v0) $ Roller sp step

-- | FLslider puts a slider into the corresponding container.
--
-- > slider valueSpan initVal 
--
-- doc: <http://www.csounds.com/manual/html/FLslider.html>
slider :: String -> ValSpan -> Double -> Source Sig
slider name sp v0 = setLabelSource name $ singleOut (Just v0) $ Slider sp

-- | Constructs a list of linear unit sliders (ranges in [0, 1]). It takes a list
-- of init values.
sliderBank :: String -> [Double] -> Source [Sig]
sliderBank name ds = source $ do
    (gs, vs) <- fmap unzip $ zipWithM (\n d -> slider (show n) uspan d) [(1::Int) ..] ds 
    gui <- if null name
        then return (hor gs)
        else do
            gTitle <- box name
            return $ ver [sca 0.05 gTitle, hor gs ]
    return (gui, vs)

-- | numeric (originally FLtext in the Csound) allows the user to modify 
-- a parameter value by directly typing it into a text field.
--
-- > numeric diapason step initValue 
--
-- doc: <http://www.csounds.com/manual/html/FLtext.html>
numeric :: String -> ValDiap -> ValStep -> Double -> Source Sig
numeric name diap step v0 = setLabelSource name $ singleOut (Just v0) $ Text diap step 

-- | A FLTK widget that displays text inside of a box.
--
-- > box text
--
-- doc: <http://www.csounds.com/manual/html/FLbox.html>
box :: String -> Display
box label = geToSe $ do
    (_, handle) <- newGuiVar
    let gui = fromElem [guiHandleToVar handle] [] (Box label)
    appendToGui (GuiNode gui handle) (unSE noInner)
    return $ fromGuiHandle handle

-- | A FLTK widget opcode that creates a button. 
--
-- > button text
-- 
-- doc: <http://www.csounds.com/manual/html/FLbutton.html>
button :: String -> Source (Evt ())
button name = mapSource sigToEvt $ buttonSig name

-- | A variance on the function 'Csound.Gui.Widget.button', but it produces 
-- a signal of piecewise constant function. 
buttonSig :: String -> Source Sig
buttonSig name = setLabelSource name $ singleOut Nothing Button

-- | A FLTK widget opcode that creates a toggle button.
--
-- > button text
-- 
-- doc: <http://www.csounds.com/manual/html/FLbutton.html>
toggle :: String -> Source (Evt D)
toggle name = mapSource snaps $ toggleSig name

-- | A variance on the function 'Csound.Gui.Widget.toggle', but it produces 
-- a signal of piecewise constant function. 
toggleSig :: String -> Source Sig
toggleSig name = setLabelSource name $ singleOut Nothing Toggle

-- | A FLTK widget opcode that creates a bank of buttons.
-- 
-- > butBank xNumOfButtons yNumOfButtons
-- 
-- doc: <http://www.csounds.com/manual/html/FLbutBank.html>
butBank :: Int -> Int -> Source (Evt D)
butBank xn yn = mapSource snaps $ butBankSig xn yn

-- | A variance on the function 'Csound.Gui.Widget.butBank', but it produces 
-- a signal of piecewise constant function. 
butBankSig :: Int -> Int -> Source Sig 
butBankSig xn yn = singleOut Nothing $ ButBank xn yn

-- | FLvalue shows current the value of a valuator in a text field.
--
-- > value initVal
--
-- doc: <http://www.csounds.com/manual/html/FLvalue.html>
value :: String -> Double -> Sink Sig 
value name v = setLabelSink name $ singleIn printk2 (Just v) Value

-- | A slider that serves as indicator. It consumes values instead of producing.
--
-- > meter valueSpan initValue
meter :: String -> ValSpan -> Double -> Sink Sig
meter name sp v = setLabelSink name $ singleIn setVal (Just v) (Slider sp)

-- Outputs

readD :: Var -> SE D
readD v = fmap (D . return) $ SE $ readVar v 

readSig :: Var -> Sig
readSig v = Sig $ return $ readOnlyVar v 


refHandle :: GuiHandle -> SE D
refHandle h = readD (guiHandleToVar h)

setVal :: GuiHandle -> Sig -> SE ()
setVal handle val = flSetVal (changed [val]) val =<< refHandle handle

printk2 :: GuiHandle -> Sig -> SE ()
printk2 handle val = flPrintk2 val =<< refHandle handle

-------------------------------------------------------------
-- set gui value

flSetVal :: Sig -> Sig -> D -> SE ()
flSetVal trig val handle = SE $ (depT_ =<<) $ lift $ f <$> unSig trig <*> unSig val <*> unD handle
    where f a b c = opcs "FLsetVal" [(Xr, [Kr, Kr, Ir])] [a, b, c]

flPrintk2 :: Sig -> D -> SE ()
flPrintk2 val handle = SE $ (depT_ =<<) $ lift $ f <$> unSig val <*> unD handle
    where f a b = opcs "FLprintk2" [(Xr, [Kr, Ir])] [a, b]

-- | This opcode outputs a trigger signal that informs when any one of its k-rate 
-- arguments has changed. Useful with valuator widgets or MIDI controllers.
--
-- > ktrig changed kvar1 [, kvar2,..., kvarN]
--
-- doc: <http://www.csounds.com/manual/html/changed.html>
changed :: [Sig] -> Sig
changed = Sig . fmap f . mapM unSig
    where f = opcs "changed" [(Kr, repeat Kr)]

