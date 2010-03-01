module Main(main) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import XMonad
import XMonad.Prompt
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Layout.Circle
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid

import qualified Data.Map as M
import System.IO
import Control.Monad.State
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect

import qualified XMonad.StackSet as W

myPP h = defaultPP {
    ppOutput = hPutStrLn h
}

myXPConfig = greenXPConfig {
    font = "terminus-iso8859-1-12"
}

myGSConfig = defaultGSConfig

myWorkspaces = [ "term", "2", "3", "4", "5", "6", "7", "8", "www" ]

selectWorkspace =
    do selected <- gridselect myGSConfig $ (id &&& id) <$> myWorkspaces
       case selected of
        Just selected' -> windows . W.greedyView $ selected'
        _ -> return ()

myLayout = tiled ||| Mirror tiled ||| Full ||| Circle ||| Grid
 where
   tiled    = Tall nmaster delta ratio
   nmaster  = 1
   ratio    = 1/2
   delta    = 3/100

myManageHook =   (className =? "Conkeror" --> doShift "www")
             <+> (className =? "Firefox" --> doShift "8")
             <+> (isFullscreen --> doFullFloat)
             <+> manageDocks

myKeys sp = \conf -> mkKeymap conf $
         [ ("C-<Escape>",   spawn "xscreensaver-command --lock")
         , ("M-p",   shellPromptHere sp myXPConfig)
         , ("M-S-h", return ())
         , ("M-w",   selectWorkspace)
         , ("M-S-d", spawn "/home/jos/bin/keymap-switch.sh")
         , ("M-S-e", spawn "/home/jos/bin/keymap-switch.sh")
         ]

newKeys sp x = M.union (myKeys sp x) (keys defaultConfig x)

main = do
    h <- spawnPipe "dzen2 -bg '#76848F' -fg '#000000' -h 18 -ta l -e '' -w 840"
    sp <- mkSpawner
    xmonad $ defaultConfig {
        logHook = (dynamicLogWithPP $ myPP h)
                  >> ewmhDesktopsLogHook
                  >> setWMName "LG3D"
      , workspaces = myWorkspaces
      , terminal = "urxvtcd -bg black -fg white +sb"
      , modMask = mod4Mask
      , normalBorderColor = "#303030"
      , focusedBorderColor = "#76848F"
      , keys = newKeys sp
      , manageHook = manageSpawn sp <+> myManageHook <+> manageHook defaultConfig
      , layoutHook = smartBorders $ avoidStruts $ myLayout
    }

