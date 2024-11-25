-- Xmonad.
import XMonad

-- Hooks.
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
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

-- Keybindings.
import XMonad.Actions.PhysicalScreens

-- System.
import System.Exit

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

      -- Play / pause.
      ("<XF86AudioPlay>", spawn "playerctl play-pause"),

      -- Raise / lower volume. Lifted from here:
      -- https://lambdablob.com/posts/xmonad-audio-volume-alsa-pulseaudio/
      ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -1%"),
      ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1%"),

      -- Screenshots.
      ("<Print>", spawn "flameshot gui"),

      -- Fix keyboard speed.
      ("M-y", spawn "xset r rate 264 48"),

      -- Emacs.
      ("M-S-e", spawn myEmacs),

      -- Unbind these keys.
      ("M-S-q", return ()),
      ("M-S-w", return ()),
      ("M-S-r", return ()),

      -- Switch focus to screen 1, 2, or 3.
      ("M-q", viewScreen def 0),
      ("M-w", viewScreen def 1),
      ("M-e", viewScreen def 2),

      -- Recompile.
      ("M-r", spawn "xmonad --recompile && xmonad --restart"),
      
      -- Logout (keypad 1).
      ("M-C-S-<KP_End>", io (exitWith ExitSuccess)),
      
      -- Shutdown (keypad subtract).
      ("M-C-S-<KP_Subtract>", spawn "shutdown now")
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

myStartupHook :: X ()
myStartupHook = do
  -- Spawn emacs as a daemon.
  spawnOnce "emacs --daemon"

  -- Setup your wallpaper.
  spawnOnce "nitrogen --restore &"

  -- Setup your screensaver.
  spawnOnce "xscreensaver -no-splash"

  -- Compatibility with java apps.
  setWMName "LG3D"

-- The entry point.
main :: IO ()
main = xmonad $ ewmh . docks $ def
  {
    modMask            = myModMask,
    terminal           = myTerminal,
    borderWidth        = myBorderWidth,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    layoutHook         = myLayoutHook,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
  }
  `additionalKeysP` myKeys
