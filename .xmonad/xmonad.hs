import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO
import Control.Monad

myWorkspaces = ["main", "web", "dev","misc", "media", "float", "chat"]

-- Define the workspace an application has to go to
myManageHook = composeAll . concat $
    [
          -- Applications that go to web
      [ className =? a --> doF (W.shift "web") | a <- myClassWebShifts  ]
         -- Applications that go to chat
    , [ resource  =? b --> doF (W.shift "main") | b <- myClassMainShifts ]   
    , [ resource  =? c --> doF (W.shift "chat") | c <- myClassChatShifts ]
    , [ resource  =? d --> doF (W.shift "dev") | d <- myClassDevShifts ]
    , [ resource  =? e --> doF (W.shift "float") | e <- myClassFloatShifts ]
    , [ resource  =? f --> doF (W.shift "misc") | f <- myClassMiscShifts ] 
    , [ resource  =? g --> doF (W.shift "media") | g <- myClassMediaShifts ]  
   
    ]
    where
       --  viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassMainShifts = ["gnome-terminal-server"]
        myClassWebShifts  = ["Firefox"]
        myClassChatShifts = ["Irc"]
        myClassDevShifts = ["emacs"]
        myClassFloatShifts = ["gimp"]
        myClassMiscShifts = ["nautilus"]
	myClassMediaShifts = ["vlc"]

main = do
  
  xmproc <- spawnPipe "/bin/xmobar /home/arjun/.xmobarrc"
  xmonad $ defaultConfig
    {
      borderWidth = 0
    , manageHook = manageDocks <+> myManageHook
                   <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    , startupHook = do
        spawn "firefox"
        spawn "emacs"
        spawn "nautilus"
        spawn "gnome-terminal"
    , workspaces = myWorkspaces
    , terminal = "gnome-terminal"
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    ]
    
-- main = do
--   xmonad $ defaultConfig
