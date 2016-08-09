{-# Language GeneralizedNewtypeDeriving  #-}
module Csound.Typed.Gui.Cabbage.Cabbage(
    Cab, runCab,
    
    -- * Widgets
    button, filebutton, infobutton, checkbox, combobox, csoundoutput, encoder, gentable, 
    hrange, vrange, form, groupbox, image, keyboard, label, hslider, vslider,
    rslider, soundfiler, signaldisplay, textbox, texteditor, xypad,

    -- * Properties
    bounds, channel, text1, text2, value, colour, colour0, colour1, backgroundcolour, textcolour, trackercolour, outlinecolour, 
    fontcolour, fontcolour0, fontcolour1, latched, identchannel, rotate, alpha, visible, caption, widgetarray, popuptext, 
    active, svgfile, populate, mode, file, shape, corners, channeltype, align, sliderincr, max, min, textbox', trackerthickness,
    linethickness, range, range2, size, pluginid, guirefresh, plant, child, show, middlec, keywidth, scrollbars, fontstyle,
    scrubberpos, zoom, displaytype, updaterate, wrap

) where

import Prelude hiding (show, min, max)

import Data.Maybe
import Control.Monad.Trans.Writer.Strict

import Csound.Typed.Gui.Cabbage.CabbageLang	

newtype Cab a = Cab { unCab :: Writer [Line] a }
	deriving (Functor, Applicative, Monad)

runCab :: Cab a -> [Line]
runCab = snd . runWriter . unCab

---------------------------------------
-- widgets

widget :: String -> [Property] -> Cab ()
widget name props = Cab $ tell [Line name props]

---------------------------------------

button, filebutton, infobutton, checkbox, combobox, csoundoutput, encoder, gentable, 
    hrange, vrange, form, groupbox, image, keyboard, label, hslider, vslider,
    rslider, soundfiler, signaldisplay, textbox, texteditor, xypad :: [Property] -> Cab ()

button 			= widget "button"
filebutton 		= widget "filebutton"
infobutton 		= widget "infobutton"
checkbox 		= widget "checkbox"
combobox 		= widget "combobox"
csoundoutput	= widget "csoundoutput"
encoder 		= widget "encoder"
gentable 		= widget "gentable"
hrange 			= widget "hrange"
vrange 			= widget "vrange"
form 			= widget "form"
groupbox 		= widget "groupbox"
image 			= widget "image"
keyboard 		= widget "keyboard"
label 			= widget "label"
hslider 		= widget "hslider"
vslider 		= widget "vslider"
rslider 		= widget "rslider"
soundfiler 		= widget "soundfiler"
signaldisplay	= widget "signaldisplay"
textbox 		= widget "textbox"
texteditor 		= widget "texteditor"
xypad 			= widget "xypad"
	
---------------------------------------
-- properties

data Col = Hash String | Rgb Int Int Int

colProp x = case x of
	Hash a -> [StringArg a]
	Rgb r g b -> fmap IntArg [r, g, b]

boolProp x = IntArg $ if x then 1 else 0	

bounds :: Int -> Int -> Int -> Int -> Property
bounds x y w h = Property "bounds" (fmap IntArg [x, y, w, h])

channel :: String -> Property
channel name = Property "channel" [StringArg name]

text1 :: String -> Property
text1 name = Property "text" [StringArg name]

text2 :: String -> String -> Property
text2 name1 name2 = Property "text" [StringArg name1, StringArg name2]

value :: Float -> Property
value x = Property "value" [FloatArg x]

colour :: Col -> Property
colour col = Property "colour" (colProp col)

colour0 :: Col -> Property
colour0 col = Property "colour:0" (colProp col)

colour1 :: Col -> Property
colour1 col = Property "colour:1" (colProp col)

backgroundcolour :: Col -> Property
backgroundcolour col = Property "backgroundcolour" (colProp col)

textcolour :: Col -> Property
textcolour col = Property "textcolour" (colProp col)

trackercolour :: Col -> Property
trackercolour col = Property "trackercolour" (colProp col)

outlinecolour :: Col -> Property
outlinecolour col = Property "outlinecolour" (colProp col)

fontcolour :: Col -> Property
fontcolour col = Property "fontcolour" (colProp col)

fontcolour0 :: Col -> Property
fontcolour0 col = Property "fontcolour:0" (colProp col)

fontcolour1 :: Col -> Property
fontcolour1 col = Property "fontcolour:1" (colProp col)

latched :: Bool -> Property
latched b = Property "latched" [boolProp b]

identchannel :: String -> Property
identchannel s = Property "identchannel" [StringArg s]

rotate :: Float -> Float -> Float -> Property
rotate radians pivotx pivoty = Property "rotate" $ fmap FloatArg [radians, pivotx, pivoty]

alpha :: Float -> Property
alpha a = Property "alpha" [FloatArg a]

visible :: Bool -> Property
visible a = Property "visible" [boolProp a]

caption :: String -> Property
caption a = Property "caption" [StringArg a]

widgetarray :: String -> Int -> Property
widgetarray name n = Property "widgetarray" [StringArg name, IntArg n]

popuptext :: String -> Property
popuptext a = Property "popuptext" [StringArg a]

active :: Bool -> Property
active a = Property "active" [boolProp a]

svgfile :: String -> String -> Property
svgfile ty file = Property "svgfile" (fmap StringArg [ty, file])

populate :: String -> String -> Property
populate filetype dir = Property "populate" (fmap StringArg [filetype, dir])

mode :: String -> Property
mode a = Property "mode" [StringArg a]

file :: String -> Property
file a = Property "file" [StringArg a]

shape :: String -> Property
shape a = Property "shape" [StringArg a]

corners :: Float -> Property
corners a = Property "corners" [FloatArg a]

channeltype :: String -> Property
channeltype a = Property "channeltype" [StringArg a]

align :: String -> Property
align a = Property "align" [StringArg a]

sliderincr :: Float -> Property
sliderincr a = Property "sliderincr" [FloatArg a]

max :: Float -> Property
max a = Property "max" [FloatArg a]

min :: Float -> Property
min a = Property "min" [FloatArg a]

textbox' :: Bool -> Property
textbox' a = Property "textbox" [boolProp a]

trackerthickness :: Float -> Property
trackerthickness a = Property "trackerthickness" [FloatArg a]

linethickness :: Float -> Property
linethickness a = Property "linethickness" [FloatArg a]

range :: Float -> Float -> (Float, Float) -> Property
range min max value = range2 min max value Nothing Nothing

range2 :: Float -> Float -> (Float, Float) -> Maybe Float -> Maybe Float -> Property
range2 min max value mskew mincr = Property "range" $ catMaybes [Just $ FloatArg min, Just $ FloatArg max, Just $ (uncurry ColonArg) value, fmap FloatArg mskew, fmap FloatArg mincr]

size :: Int -> Int -> Property
size w h = Property "size" (fmap IntArg [w, h])

pluginid :: String -> Property
pluginid a = Property "pluginid" [StringArg a]

guirefresh :: Int -> Property
guirefresh a = Property "guirefresh" [IntArg a]

plant :: String -> Property
plant a = Property "plant" [StringArg a]

child :: Bool -> Property
child a = Property "child" [boolProp a]

show :: Bool -> Property
show a = Property "show" [boolProp a]

middlec :: Int -> Property
middlec a = Property "middlec" [IntArg a]

keywidth :: Int -> Property
keywidth a = Property "keywidth" [IntArg a]

scrollbars :: Bool -> Property
scrollbars a = Property "scrollbars" [boolProp a]

fontstyle :: String -> Property
fontstyle a = Property "fontstyle" [StringArg a]

scrubberpos :: Int -> Property
scrubberpos a = Property "scrubberpos" [IntArg a]

zoom :: Float -> Property
zoom a = Property "zoom" [FloatArg a]

displaytype :: String -> Property
displaytype a = Property "displaytype" [StringArg a]

updaterate :: Int -> Property
updaterate a = Property "updaterate" [IntArg a]

wrap :: Bool -> Property
wrap a = Property "wrap" [boolProp a]
