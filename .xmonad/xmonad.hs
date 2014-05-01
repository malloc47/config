import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import qualified Data.Map as M

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_y)

-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig
  { terminal    = myTerminal
  , modMask     = myModMask
  , borderWidth = myBorderWidth
  , workspaces  = myWorkspaces
  , keys = myKeys
  , startupHook = setWMName "LG3D"
  }

myTerminal    = "urxvtc"
myModMask     = mod4Mask
myBorderWidth = 1
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
newKeys conf@(XConfig {XMonad.modMask = modm}) =
  [ ((modm, xK_p), spawn "dmenu_run")
  , ((modm .|. shiftMask, xK_l), spawn "xscreensaver-command --lock")
  , ((modm .|. shiftMask, xK_e), spawn "emacsclient -c -a \"\"")
  , ((modm .|. shiftMask, xK_t), spawn "urxvtc")
  , ((0, xK_Print), spawn "import -window root ~/screenshot-`date '+%Y%m%d-%H%M%S'`.png")
  ]
