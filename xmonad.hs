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

import XMonad.Prompt.XMonad

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

resizeDragger  m = m { draggerType = FixedDragger 1 8 }

myLayout = desktopLayoutModifiers 
         $  resizeDragger mouseResizableTile 
        ||| resizeDragger mouseResizableTileMirrored 
        
defaultSpaces = map show [0..9]

selectSpace = selectWorkspace defaultXPConfig

renameSpace = renameWorkspace defaultXPConfig

withSpace = withWorkspace defaultXPConfig

removeSpace = do
  selectSpace
  removeWorkspace
  

selectGroup = promptWSGroupView defaultXPConfig "Go to group: "

addGroup = promptWSGroupAdd defaultXPConfig "Group Name: "

removeGroup = promptWSGroupForget defaultXPConfig "Delete Group: "


myKeysC conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  allKeys modm
  ++
  [ ((otherModMasks modm, key), windows $ action tag)
  | (tag, key)  <- zip defaultSpaces [xK_0..xK_9]
  , (otherModMasks, action) <- [ (id , W.view) , ((.|. shiftMask), W.shift)]  -- was W.greedyView
  ]

allKeys modm = doAction:map (\(a,b,_) -> (a,b)) keys
  where keys = myKeys modm
        doAction = ((modm , xK_x), action)
        action = xmonadPromptC (map (\(_,act,nm) -> (nm, act)) keys) defaultXPConfig

myKeys modm =
  [ ((modm              , xK_u), sendMessage ShrinkSlave, "shrink")
  , ((modm              , xK_i), sendMessage ExpandSlave, "expand")
  , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace, "remove-current-workspace")
  , ((modm .|. shiftMask, xK_d), removeSpace, "remove-workspace")
  , ((modm              , xK_m), selectSpace, "select-workspace")
  , ((modm .|. shiftMask, xK_m), withSpace $ windows . W.shift, "move-to-workspace")
  , ((modm .|. shiftMask, xK_r), renameSpace, "rename-workspace")
  , ((modm .|. shiftMask, xK_q), spawn "gnome-session-quit", "gnome-session-quit")
  , ((modm .|. shiftMask, xK_s), selectGroup, "select-group")
  , ((modm .|. shiftMask, xK_n), addGroup, "add-group")
  , ((modm .|. shiftMask, xK_d), removeGroup, "remove-group")
  ]