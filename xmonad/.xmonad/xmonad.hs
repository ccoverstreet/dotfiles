-- default desktop configuration for Fedora

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)
import Data.List

import XMonad
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.ResizableTile
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Hooks.FadeInactive
import qualified XMonad.StackSet as W


scratchpads = [
	NS "terminal" "gnome-terminal --profile='Scratchpad' --role=SCRATCHPAD" (stringProperty "WM_WINDOW_ROLE" =? "SCRATCHPAD") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
			  ]

myTabConfig = def { activeBorderWidth = 0
                  , inactiveBorderWidth = 0
                  , activeColor="#003C70"
                  , inactiveColor="#000000"}

myLayout = avoidStruts $ (tabbed shrinkText myTabConfig ||| ResizableTall 1 (3/100) (1/2) [] ||| noBorders Full)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes


myWorkspaces = ["1:main", "2:school"]
  ++ map show [3..8]
  ++ ["9:notes"]

myManageHook = composeAll
  [ 
  resource =? "Dialog" --> doFloat
  , className =? "kdeconnect.daemon" --> doFullFloat
  , className =? "python3" --> doFloat
  , className =? "zoom" <&&> title /=? "Zoom Meeting" --> doFloat
  , className =? "gksqt" --> doFloat
  , fmap ("Figure" `isPrefixOf`) windowName --> doFloat
  , windowName =? "Figure 1" --> doFloat
  ]
  where 
    windowName = stringProperty "WM_NAME"


main = do
  spawn "feh --bg-fill ~/Pictures/Fantastic-HD-Black-Wallpapers-620x388.jpg"
  spawn "xcompmgr"
  spawn "xsetroot -cursor_name left_ptr"
  -- spawn "setxkbmap -option caps:escape"
  spawn "xmodmap -e \"keycode 64 = Escape\""
  spawn "/usr/libexec/polkit-gnome-authentication-agent-1"
  xmproc <- spawnPipe $ "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
  xmonad $  ewmh $ docks def
    {
    modMask = mod4Mask
    , workspaces = myWorkspaces
    , focusedBorderColor = "#FF8200"
    , borderWidth = 0
    , layoutHook = myLayout
    , manageHook = myManageHook <+> namedScratchpadManageHook scratchpads <+> manageDocks <+> manageHook def

    , terminal = "gnome-terminal"
    , logHook = updatePointer (0.5, 0.5) (0.0, 0.0) <+> dynamicLogWithPP xmobarPP
      {
      ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "green" "" . shorten 50
      }
    } `additionalKeysP`
        [ 
        -- ("M-p", spawn "/usr/bin/rofi -combi-modi window,run,ssh -theme Arc-Dark.rasi -show combi -window-thumbnail -show-icons -theme-str 'element-icon { size: 20ch;}' -window-format '{t}'")
        ("M-p", spawn "/usr/bin/rofi -combi-modi window,run,ssh -theme sidebar -show combi -window-thumbnail -show-icons")
		, ("M-S-h", sendMessage MirrorShrink)
		, ("M-S-l", sendMessage MirrorExpand)
        , ("M-i", spawn "brave")
        , ("M-f", spawn "nautilus --new-window")
        , ("M-C-s", spawn "jsuspend")
        , ("<Print>", spawn "gnome-screenshot -i")
        , ("M-<Tab>", moveTo Next (hiddenWS :&: Not emptyWS))
        , ("M-o", nextScreen)
        , ("M-u", spawn "autorandr common")
        , ("M-S-u", spawn "autorandr horizontal")
        , ("M-b", sendMessage ToggleStruts)
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -2%")
        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +2%")
        , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
        , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%")
        , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
        , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    	, ("M-t", namedScratchpadAction scratchpads "terminal")
        ]
