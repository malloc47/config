--
-- David Beckingsale's xmonad config
-- 
-- Started out as avandael's xmonad.hs 
-- Also uses stuff from pbrisbin.com:8080/
--
 
--{{{ Imports 
import Data.List

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib

import System.IO

import XMonad

import XMonad.Actions.GridSelect

import XMonad.Core

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile
import XMonad.Layout.Reflect

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell

import XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified Data.Map as M
--}}}

--{{{ Helper Functions
stripIM s = if ("ReflectX IM " `isPrefixOf` s) then drop (length "ReflectX IM ") s else s

wrapIcon icon = "^p(5)^i(" ++ icons ++ icon ++ ")^p(5)"
--}}}

--{{{ Path variables
icons = "/home/malloc47/.icons/"
--}}}

main = do
   myStatusBarPipe <- spawnPipe myStatusBar
   conkyBar <- spawnPipe myConkyBar
   xmonad $ myUrgencyHook $ defaultConfig
      { terminal = "urxvtc"
      , normalBorderColor  = myInactiveBorderColor
      , focusedBorderColor = myActiveBorderColor
      , borderWidth = myBorderWidth
      , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
      , layoutHook = smartBorders $ avoidStruts $ myLayoutHook
      , logHook = dynamicLogWithPP $ myDzenPP myStatusBarPipe
      , modMask = mod4Mask
      , keys = myKeys
      , XMonad.Core.workspaces = myWorkspaces
      , startupHook = setWMName "LG3D"
     }   
 
--{{{ Theme 

--Font
--myFont = "Terminus-6"
myFont = "-xos4-terminus-medium-*-*-*-12-*-*-*-*-*-iso8859-2"

 
-- Colors

--- Main Colours
myFgColor = "#DCDCCC"
myBgColor = "#3f3f3f"
myHighlightedFgColor = myFgColor
myHighlightedBgColor = "#7F9F7F"

--- Borders
myActiveBorderColor = myCurrentWsBgColor
myInactiveBorderColor = "#262626"
myBorderWidth = 1

--- Ws Stuff
myCurrentWsFgColor = myHighlightedFgColor
myCurrentWsBgColor = myHighlightedBgColor
myVisibleWsFgColor = myBgColor
myVisibleWsBgColor = "#CCDC90"
myHiddenWsFgColor = myHighlightedFgColor
myHiddenEmptyWsFgColor = "#8F8F8F"
myUrgentWsBgColor = "#DCA3A3"
myTitleFgColor = myFgColor


--- Urgency
myUrgencyHintFgColor = "red"
myUrgencyHintBgColor = "blue"
 
-- }}}

-- dzen general options
myDzenGenOpts = "-fg '" ++ myFgColor ++ "' -bg '" ++ myBgColor ++ "' -h '15'" ++ " -e 'onstart=lower' -fn '" ++ myFont ++ "'"
 
-- Status Bar
myStatusBar = "dzen2 -w 900 -ta l " ++ myDzenGenOpts
 
-- Conky Bar
myConkyBar = "conky -c ~/.conky_bar | dzen2 -x 900 -w 466 -ta l " ++ myDzenGenOpts
 
-- Layouts
myLayoutHook = avoidStruts $ onWorkspace " 2 im " imLayout $ standardLayouts
               where standardLayouts = tiled ||| Mirror tiled ||| Full
                     imLayout = reflectHoriz $  withIM (1/8) (Role "buddy_list") (standardLayouts)
                     tiled = ResizableTall nmaster delta ratio []
                     nmaster = 1 
                     delta = 0.03
                     ratio = 0.5
-- Workspaces
myWorkspaces =
   [
      " 1 www ",
      " 2 im ",
      " 3 ",
      " 4 ",
      " 5 ",
      " 6 ",
      " 7 ",
      " 8 ",
      " 9 "
   ]
 
-- Urgency hint configuration
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
      args = [
         "-x", "0", "-y", "576", "-h", "15", "-w", "1024",
         "-ta", "r",
         "-fg", "" ++ myUrgencyHintFgColor ++ "",
         "-bg", "" ++ myUrgencyHintBgColor ++ ""
         ]
    }
 
--{{{ Hook for managing windows
myManageHook = composeAll
   [ resource  =? "Do"               --> doIgnore,              -- Ignore GnomeDo
     className =? "Pidgin"           --> doShift " 2 im ",      -- Shift Pidgin to im desktop 
     className =? "Chrome"           --> doShift " 1 www ",     -- Shift Chromium to www
     className =? "Firefox"          --> doShift " 1 www ",     -- Shift Firefox to www
--     className =? "Emacs"            --> doShift " 3 ed ",      -- Shift emacs to emacs
     className =? "Wicd-client.py"   --> doFloat,                -- Float Wicd window 
     isFullscreen 		     --> (doF W.focusDown <+> doFullFloat)
   ]
--}}}
 
-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
 
--{{{ Keybindings 
--    Add new and/or redefine key bindings
newKeys conf@(XConfig {XMonad.modMask = modm}) = [
  ((modm, xK_p), spawn "dmenu_run -nb '#3F3F3F' -nf '#DCDCCC' -sb '#7F9F7F' -sf '#DCDCCC'"),  --Uses a colourscheme with dmenu
--  {-((modm, xK_b), spawn "firefox"),-}
  ((modm, xK_z), goToSelected myGSConfig),
  ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle"),
  ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 2+"),
  ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 2-"),
  ((modm .|. shiftMask, xK_l), spawn "xscreensaver-command --lock"),
--  {-((0, xF86XK_AudioPlay), spawn "exaile -t"),-}
--  {-((0, xF86XK_AudioStop), spawn "exaile -s"),-}
--  {-((0, xF86XK_AudioNext), spawn "exaile -n"),-}
--  {-((0, xF86XK_AudioPrev), spawn "exaile -p"),-}
  ((modm, xK_y), sendMessage ToggleStruts)
--  ((modm, xK_u), sendMessage MirrorShrink),
--  ((modm, xK_i), sendMessage MirrorExpand)
   ]
--}}}

---{{{ Dzen Config
myDzenPP h = defaultPP {
  ppOutput = hPutStrLn h,
  ppSep = (wrapFg myHighlightedBgColor "|"),
  ppWsSep = "",
  ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor,
  ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor,
  ppHidden = wrapFg myHiddenWsFgColor,
  ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor,
  ppUrgent = wrapBg myUrgentWsBgColor,
  ppTitle = (\x -> "  " ++ wrapFg myTitleFgColor x),
  ppLayout  = dzenColor myFgColor"" .
                (\x -> case x of
                    "ResizableTall" -> wrapIcon "dzen_bitmaps/tall.xbm"
                    "Mirror ResizableTall" -> wrapIcon "dzen_bitmaps/mtall.xbm"
                    "Full" -> wrapIcon "dzen_bitmaps/full.xbm"
                ) . stripIM
  }
  where
    wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
    wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
    wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
--}}}

--{{{ GridSelect
myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    --, gs_colorizer = ""
    , gs_font = "" ++ myFont ++ ""
    --, gs_navigate = ""
    --, gs_originFractX = ""
    --, gs_originFractY = ""
    }
--}}}    
