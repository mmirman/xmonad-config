module Main (main) where

import XMonad

import qualified XMonad.StackSet as W
import Graphics.X11.Xlib

import XMonad.Util.EZConfig
import XMonad.Config.Gnome
import XMonad.Hooks.SetWMName
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.WindowArranger
import XMonad.Config.Desktop

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad

import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.DynamicWorkspaces

import XMonad.Actions.Navigation2D

import XMonad.Hooks.ManageDocks


import qualified Data.Map as M

main = xmonad $ gnomeConfig
         { modMask = mod4Mask
         , terminal = "gnome-terminal"
         , startupHook = do
           startupHook gnomeConfig
           setWMName "LG3D"
         , workspaces = defaultSpaces
         , layoutHook = myLayout
         , keys = myKeysC <+> keys gnomeConfig
         }

xpconf = defaultXPConfig

resizeDragger  m = m { draggerType = FixedDragger 1 8 }

myLayout = desktopLayoutModifiers 
         $  resizeDragger mouseResizableTile 
        ||| resizeDragger mouseResizableTileMirrored 
        
defaultSpaces = map show [0..9]

selectSpace = selectWorkspace xpconf

renameSpace = renameWorkspace xpconf

withSpace = withWorkspace xpconf

removeSpace = do
  selectSpace
  removeWorkspace
  

selectGroup = promptWSGroupView xpconf "Go to group: "

addGroup = promptWSGroupAdd xpconf "Group Name: "

removeGroup = promptWSGroupForget xpconf "Delete Group: "


myKeysC conf@(XConfig {XMonad.modMask = modm}) = M.fromList keys
  where mykeys = myKeys modm

        prompt = xmonadPromptC (map (\(_,act,nm) -> (nm, act)) mykeys) xpconf
          
        askPrompt = ((modm , xK_x), prompt)  -- "prompt"   mod-x   
        
        allKeys = askPrompt:map (\(a,b,_) -> (a,b)) mykeys
        
        keys = allKeys
               ++
               [ ((otherModMasks modm, key), windows $ action tag)
               | (tag, key)  <- zip defaultSpaces [xK_0..xK_9]
               , (otherModMasks, action) <- [ (id , W.view) , ((.|. shiftMask), W.shift)]  -- was W.greedyView
               ]
               
               
gnome_screensaver = spawn "gnome-screensaver-command -l"

rename_workspace = withSpace $ windows . W.shift

myKeys modm =
  [ ((modm              , xK_u        ), sendMessage ShrinkSlave, "shrink")  -- mod-u
  , ((modm              , xK_i        ), sendMessage ExpandSlave, "expand")  -- mod-i
  , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace        , "remove-current-workspace") -- mod-SHIFT-BACKSPACE
  , ((modm .|. shiftMask, xK_d        ), removeSpace            , "remove-workspace") -- mod-D
  , ((modm              , xK_m        ), selectSpace            , "select-workspace") -- mod-m
  , ((modm .|. shiftMask, xK_m        ), rename_workspace       , "move-to-workspace") -- mod-M
  , ((modm .|. shiftMask, xK_r        ), renameSpace            , "rename-workspace") -- mod-R
  , ((modm .|. shiftMask, xK_q        ), spawn "kill -9 -1"     , "kill-everything") -- mod-Q
  , ((modm              , xK_g        ), selectGroup            , "select-group")  -- mod-s
  , ((modm .|. shiftMask, xK_s        ), addGroup               , "add-group")  -- mod-S
  , ((modm .|. shiftMask, xK_y        ), removeGroup            , "remove-group")  -- mod-D
  , ((modm .|. shiftMask, xK_l        ), gnome_screensaver      , "lock screensaver") -- mod-L
  , ((modm              , xK_t        ), spawn "google-chrome"  , "google-chrome")  -- mod-t

  , ((modm .|. controlMask , xK_b        ), sendMessage ToggleStruts, "toggle-docks")
    
  , ((modm              , xK_f        ), windowGo R False       , "select-right")
  , ((modm              , xK_b        ), windowGo L False       , "select-left")
  , ((modm              , xK_p        ), windowGo U False       , "select-up")
  , ((modm              , xK_n        ), windowGo D False       , "select-down")

  , ((modm .|. shiftMask, xK_f        ), windowSwap R False     , "swap-right")
  , ((modm .|. shiftMask, xK_b        ), windowSwap L False     , "swap-left")
  , ((modm .|. shiftMask, xK_p        ), windowSwap U False     , "swap-up")
  , ((modm .|. shiftMask, xK_n        ), windowSwap D False     , "swap-down")
  ]
