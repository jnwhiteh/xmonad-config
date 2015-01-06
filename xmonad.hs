import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Dishes
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.DwmStyle
import XMonad.Layout.ResizableTile
import XMonad.Util.Run
import System.IO
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W

import Data.Ratio ((%))

main = xmonad =<< myXmobar (withUrgencyHook NoUrgencyHook $ myConfig)
--main = xmonad =<< dzen (withUrgencyHook NoUrgencyHook $ myConfig)

myStartupHook :: X ()
myStartupHook = do
    spawn "bash ~/.xmonad/startup.sh"
    spawn "bash ~/.xmonad/randombg.sh delay"

-- onWorkspace "9:im" (named "IM" (reflectHoriz $ withIM (1%6) (Title "Buddy List") (reflectHoriz $ dwmStyle shrinkText myTheme tiled ||| (smartBorders $ tabs)))) $
--
myLayoutHook = avoidStruts $ smartBorders $
    onWorkspace "9:im" (named "IM" imLayout) $
    (tiled ||| named "Mirror" (Mirror tiled) ||| tabs ||| dishes ||| Full)
        where
            imLayout = withIM (1%7) (Title "Buddy List") (reflectHoriz (withIM (1%6) (Title "jnwhiteh - Skype™ (Beta)") (dwmStyle shrinkText myTheme tiled ||| (smartBorders $ tabs))))
            tiled = Tall 1 (3/100) (1/2)
            tabs = smartBorders simpleTabbed
            dishes = smartBorders (Dishes 2 (1/6))
            myTheme = defaultTheme { decoHeight = 16
                                   , activeColor = "#a6c292"
                                   , activeBorderColor = "#a6c292"
                                   , activeTextColor = "#000000"
                                   , inactiveBorderColor = "#000000"
                                   }

myManageHook = composeAll
    [ className =? "Spotify" --> doF (W.shift "8:spotify")
    , className =? "Pidgin" --> doF (W.shift "9:im")
    , className =? "Skype" --> doF (W.shift "9:im")
    , manageDocks
    ]

myXmobar conf = statusBar "xmobar" xmobarPP toggleStrutsKey conf
    where
        xmobarPP = defaultPP { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                             , ppTitle   = xmobarColor "green"  "" . shorten 60
                             , ppVisible = wrap "(" ")"
                             , ppUrgent  = xmobarColor "red" ""
                             }
        toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

myWorkspaces = map show [1..7] ++ ["8:spotify", "9:im"]

myConfig = gnomeConfig
--    { manageHook = manageDocks <+> manageHook gnomeConfig
--    , layoutHook = avoidStruts $ layoutHook gnomeConfig
    { manageHook = myManageHook <+> manageHook gnomeConfig
    , layoutHook = myLayoutHook
    , logHook = dynamicLogXinerama
    , startupHook = myStartupHook
    , workspaces = myWorkspaces
    }
    `additionalKeysP`
    ([ ("M-S-q", spawn "gnome-session-quit") -- logout
    , ("M-p", spawn "dmenu_run -nb black -nf white") --call dmenu
    , ("M-S-[", spawn "amixer -c 0 sset Master,0 5%-") -- volume down
    , ("M-S-]", spawn "amixer -c 0 sset Master,0 5%+") -- volume up
    , ("M-S-,", spawn "xbacklight -20")
    , ("M-S-.", spawn "xbacklight +20")
    , ("M-S-w", spawn "/usr/lib/jupiter/scripts/wifi") -- toggle wifi
    , ("M-S-b", spawn "/usr/lib/jupiter/scripts/bluetooth") -- toggle bluetooth
    , ("M-S-d", spawn "/usr/lib/jupiter/scripts/vga-out") -- toggle displays
    , ("M-S-t", spawn "/usr/lib/jupiter/scripts/touchpad") -- toggle touchpad
    -- These do not work
    , ("<XF86MonBrightnessUp>", spawn "xbacklight +20") -- brightness up
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -10") -- brightness down
    , ("<XF86KbdBrightnessUp>", spawn "xbacklight +20") -- brightness up
    , ("<XF86KbdBrightnessDown>", spawn "xbacklight -10") -- brightness down
    -- Use view instead of greeedyView
    ])
--     ] ++
--     [ (otherModMasks ++ "M-" ++ [key], action tag)
--       | (tag, key) <- zip myWorkspaces "123456789"
--       , (otherModMasks, action) <- [ ("", windows . W.view)
--                                    , ("S-", windows . W.shift)]
--     ])
