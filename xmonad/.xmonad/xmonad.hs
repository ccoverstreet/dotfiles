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
import XMonad.Hooks.Place as WP
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Promote
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.Magnifier
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Hooks.FadeInactive
import XMonad.Layout.TrackFloating
import XMonad.Layout.Fullscreen
import XMonad.Actions.Navigation2D
import qualified XMonad.StackSet as W


scratchpads = [
	NS "terminal" "kitty --class=SCRATCHPAD" (className =? "SCRATCHPAD") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
			  ]

myTabConfig = def { activeBorderWidth = 0
                  , inactiveBorderWidth = 0
                  , activeColor="#719899"
                  , inactiveColor="#323232"}

myLayout = avoidStruts $ (tabbed shrinkText myTabConfig ||| ResizableTall 1 (3/100) (1/2) [] ||| toggleLayouts Full (fullscreenFull (noBorders Full)))
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes


myWorkspaces = ["1", "2"]
  ++ map show [3..8]
  ++ ["9"]

myManageHook = composeAll
  [ 
  resource =? "Dialog" --> doFloat
  , className =? "kdeconnect.daemon" --> doFullFloat
  , className =? "python3" --> doCenterFloat
  , className =? "zoom" <&&> netName /=? "Zoom Meeting" --> doFloat
  , className =? "gksqt" --> doFloat
  , fmap ("Figure" `isPrefixOf`) windowName --> doCenterFloat
  , windowName =? "Figure 1" --> doFloat
  , className =? "org-openscience-jmol-app-jmolpanel-JmolPanel" --> doFloat
  , fmap ("PGPLOT" `isPrefixOf`) windowName --> doFloat
  ]
  where 
    windowName = stringProperty "WM_NAME"
    netName = stringProperty "_NET_WM_NAME"


main = do
  spawn "feh --bg-fill ~/Pictures/pawel-czerwinski-IcOg3D9yh0o-unsplash.jpg"
  spawn "compton"
  spawn "ibus-daemon -rxR"
  spawn "xsetroot -cursor_name left_ptr"
  -- spawn "setxkbmap -option caps:escape"
  -- spawn "xmodmap -e \"keycode 64 = Escape\""
  spawn "/usr/libexec/polkit-gnome-authentication-agent-1"
  xmproc <- spawnPipe $ "/usr/bin/xmobar -x 0 ~/.xmonad/xmobar.hs"
  xmproc1 <- spawnPipe $ "/usr/bin/xmobar -x 1 ~/.xmonad/xmobar.hs"
  xmonad $  ewmh $ navigation2DP def
                              ("<Up>", "<Left>", "<Down>", "<Right>")
                              [("M-",   windowGo  ),
                               ("M-S-", windowSwap)]
                              False
              $ docks def
    {
    modMask = mod4Mask
    , workspaces = myWorkspaces
    , focusFollowsMouse = False
    , focusedBorderColor = "#c8c8c8"
    , normalBorderColor = "#000000"
    , borderWidth = 1
    , layoutHook = myLayout
    --, manageHook = myManageHook <+> namedScratchpadManageHook scratchpads <+> manageDocks 
    --	<+> WP.placeHook (WP.withGaps (16, 0, 16, 0) (WP.smart (0.5,0.5))) <> manageHook def
    , manageHook = myManageHook <+> namedScratchpadManageHook scratchpads <+> manageDocks 
    	<+> manageHook def

    , terminal = "konsole"
    --, logHook = updatePointer (0.5, 0.5) (0.0, 0.0) <+> dynamicLogWithPP xmobarPP
    , logHook = dynamicLogWithPP xmobarPP
      {
      ppOutput = \x ->  hPutStrLn xmproc x >> hPutStrLn xmproc1 x
      , ppTitle = xmobarColor "#97bb98" "" . shorten 50
      }
    } `additionalKeysP`
        [ 
        -- ("M-p", spawn "/usr/bin/rofi -combi-modi window,run,ssh -theme Arc-Dark.rasi -show combi -window-thumbnail -show-icons -theme-str 'element-icon { size: 21ch;}' -window-format '{t}'")
        ("M-p", spawn "/usr/bin/rofi -modi brotab:~/.config/rofi/brofi.py -combi-modi window,brotab,run,ssh -show combi -show-icons")
        , ("M-j", windows W.focusDown)
        , ("M-k", windows W.focusUp)
		, ("M-S-h", sendMessage MirrorShrink)
		, ("M-S-l", sendMessage MirrorExpand)
		, ("M-e", spawn "ibus engine xkb:us::eng")
		, ("M-a", spawn "ibus engine anthy")
        , ("M-i", spawn "firefox")
        , ("M-f", spawn "dolphin")
        , ("M-C-s", spawn "jsuspend")
        , ("<Print>", spawn "gnome-screenshot -i")
        , ("M-<Tab>", moveTo Next (hiddenWS :&: Not emptyWS))
        , ("M-o", nextScreen)
        , ("M-u", spawn "autorandr common")
        , ("M-S-u", spawn "autorandr horizontal")
        , ("M-r", spawn "autorandr --change")
        , ("M-b", sendMessage ToggleStruts)
        , ("M-n", spawn "dunstctl set-paused toggle")
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -2%")
        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +2%")
        , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
        , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%")
        , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
        , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    	, ("M-s", namedScratchpadAction scratchpads "terminal")
        ]
