-- Xmonad.
import XMonad

-- Hooks.
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- I don't know about this one, we need the newer version.
import XMonad.Hooks.ManageDocks

-- Layout.
import XMonad.Layout.IfMax
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier

-- Utilities.
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- Variables.
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myEmacs :: String
myEmacs = "emacsclient -c -a ''"

myBorderWidth :: Dimension
myBorderWidth = 3

myNormalBorderColor :: String
myNormalBorderColor = "#282c34"

myFocusedBorderColor :: String
myFocusedBorderColor = "#bbc2cf"

-- Keybindings.
myKeys :: [(String, X ())]
myKeys = 
    [
      -- Launch dmenu.
      ("M-p", spawn "j4-dmenu-desktop"),

      -- Raise / lower volume.
      ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-,5%-"),
      ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+,5%+"),

      -- Play / pause.
      ("<XF86AudioPlay>", spawn "playerctl play-pause"),

      -- Screenshots.
      ("<Print>", spawn "flameshot gui"),

      -- Emacs.
      ("M-e", spawn myEmacs),

      -- Terminal.
      ("M-r", spawn myTerminal),

      -- Fix keyboard speed.
      ("M-y", spawn "xset r rate 264 48"),
      
      -- Shutdown.
      ("M-C-S-q", spawn "shutdown now")
    ]

-- Spacing for tiling windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Layouts.
myTall = mySpacing 8
  $ withBorder myBorderWidth
  $ Tall 1 (3/100) (1/2)

myWide = mySpacing 8
  $ withBorder myBorderWidth
  $ Mirror $ Tall 1 (3/100) (1/2)

myFull = Full

-- The layout hook.
myLayoutHook = smartBorders
  $ IfMax 1 myFull $ myTall ||| myWide ||| myFull

-- The manage hook.
myManageHook :: ManageHook
myManageHook = composeAll
    [ isDialog --> doFloat ]

-- xset r rate {delay} {rate}
-- note that xset is very unreliable :(
-- https://askubuntu.com/questions/255890/how-can-i-adjust-the-mouse-scroll-speed/709184#709184

myStartupHook :: X ()
myStartupHook = do
  -- spawnOnce "lxsession"
  
  -- Spawn emacs as a daemon.
  spawn "emacs --daemon"

  -- Setup your wallpaper.
  spawnOnce "nitrogen --restore &"

  -- Compatibility with java apps.
  setWMName "LG3D"

-- The entry point.
main :: IO ()
main = xmonad $ ewmh $ def
  {
    modMask            = myModMask,
    terminal           = myTerminal,
    borderWidth        = myBorderWidth,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    layoutHook         = myLayoutHook,
    manageHook         = myManageHook,
    startupHook        = myStartupHook,
    
    -- I don't know about this one, we need the newer version.
    -- Specifically, use this instead: ewmhFullscreen
    handleEventHook    = docksEventHook <+> fullscreenEventHook
  }
  `additionalKeysP` myKeys
