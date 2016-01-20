-------------------------------------------------------------------------------
--- $Id: ViewerColours.hs#2 2009/07/18 22:48:30 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/ViewerColours.hs $
-------------------------------------------------------------------------------

module GUI.ViewerColours (Color, module GUI.ViewerColours) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

-------------------------------------------------------------------------------

-- Colours

--Color specifies a color with three integer values for red, green and blue.
--All values range from 0 (least intense) to 65535 (highest intensity).
--https://hackage.haskell.org/package/gtk-0.14.2/docs/Graphics-UI-Gtk-Gdk-GC.html#t:Color

runningColour :: Color
runningColour = darkGreen

gcColour :: Color
gcColour = orange

gcStartColour, gcWorkColour, gcIdleColour, gcEndColour :: Color
gcStartColour = orange
gcWorkColour  = orange
gcIdleColour  = white
gcEndColour   = orange

txStartColour :: Color
txStartColour = yellow

createThreadColour :: Color
createThreadColour = lightBlue

seqGCReqColour :: Color
seqGCReqColour = cyan

parGCReqColour :: Color
parGCReqColour = darkBlue

migrateThreadColour :: Color
migrateThreadColour = darkRed

threadWakeupColour :: Color
threadWakeupColour = green

shutdownColour :: Color
shutdownColour = darkBrown

labelTextColour :: Color
labelTextColour = white

bookmarkColour :: Color
bookmarkColour = Color 0xff00 0x0000 0xff00 -- pinkish

fizzledDudsColour, createdConvertedColour, overflowedColour :: Color
fizzledDudsColour      = grey
createdConvertedColour = darkGreen
overflowedColour       = red

userMessageColour :: Color
userMessageColour = darkRed

outerPercentilesColour :: Color
outerPercentilesColour = lightGrey

-------------------------------------------------------------------------------

black :: Color
black = Color 0 0 0

blue :: Color
blue = Color 0 0 65535

cyan :: Color
cyan = Color 0 65535 65535

darkBlue :: Color
darkBlue = Color 0 0 47872

darkBrown :: Color
darkBrown = Color 26112 0 0

darkGreen :: Color
darkGreen = Color 0 26112 0

darkPurple :: Color
darkPurple = Color 26112 0 26112

darkRed :: Color
darkRed = Color 52224 0 0

green :: Color
green = Color 0 65535 0

grey :: Color
grey = Color 32768 32768 32768

lightBlue :: Color
lightBlue = Color 26112 39168 65280

lightGrey :: Color
lightGrey = Color 53248 53248 53248

magenta :: Color
magenta = Color 65535 0 65535

orange :: Color
orange = Color 57344 28672 0

profileBackground :: Color
profileBackground = Color 65535 65535 65535

purple :: Color
purple = Color 39168 0 52224

red :: Color
red = Color 65535 0 0

tickColour :: Color
tickColour = Color 13107 13107 65535

white :: Color
white = Color 65535 65535 65535

yellow :: Color
yellow = Color 65280 65280 13056


-------------------------------------------------------------------------------
setSourceRGBAhex :: Color -> Double -> Render ()
setSourceRGBAhex (Color r g b) t
  = setSourceRGBA (fromIntegral r/0xFFFF) (fromIntegral g/0xFFFF)
                  (fromIntegral b/0xFFFF) t

-------------------------------------------------------------------------------
