{-# Language GeneralizedNewtypeDeriving  #-}
module Csound.Typed.Gui.Cabbage.Cabbage(

) where

import Control.Monad.State.Strict

import Csound.Typed.Gui.Cabbage.CabbageLang	

newtype Cab a = Cab { unWidget :: Writer [Line] a }
	deriving (Functor, Applicative, Monad)

runCab :: Cab -> [Line]
runCab = runWriter . unCab

runCabProp :: CabProp -> [Property]
runCabProp = runWriter . unCabProp

---------------------------------------
-- widgets

widget :: String -> [CabProp] -> Cab
widget name props = Cab $ tell [Line name props]

---------------------------------------

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

colour0 :: Col -> Property
colour0 col = Property "colour:0" (colProp col)

colour1 :: Col -> Property
colour1 col = Property "colour:1" (colProp col)

fontcolour0 :: Col -> Property
fontcolour0 col = Property "fontcolour:0" (colProp col)

fontcolour1 :: Col -> Property
fontcolour1 col = Property "fontcolour:1" (colProp col)

latched :: Bool -> Property
latched b = Property "latched" [IntArg $ boolProp b]

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
popuptext a = Property "popuptext" [StringArg a]popuptext

active :: Bool -> Property
active a = Property "active" [boolProp a]

svgfile :: String -> String -> Property
svgfile ty file = Property "svgfile" (fmap StringArg [ty, file])
