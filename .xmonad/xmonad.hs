import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO
import Control.Monad
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus

myWorkspaces = ["main", "web", "dev","idea", "chat", "misc", "media", "float", "vbox"]

-- Define the workspace an application has to go to
myManageHook = composeAll . concat $
    [
    
      [ className =? a --> doF (W.shift "web") | a <- myClassWebShifts  ]
    , [ resource  =? b --> doF (W.shift "main") | b <- myClassMainShifts ]   
    , [ className =? c --> doF (W.shift "vbox") | c <- myClassVBoxShifts ]
    , [ resource  =? d --> doF (W.shift "dev") | d <- myClassDevShifts ]
    , [ resource  =? e --> doF (W.shift "float") | e <- myClassFloatShifts ]
    , [ resource  =? f --> doF (W.shift "misc") | f <- myClassMiscShifts ] 
    , [ resource  =? g --> doF (W.shift "media") | g <- myClassMediaShifts ]  
    , [ resource  =? g --> doF (W.shift "idea") | g <- myClassIdeaShifts ]  
    , [ resource  =? g --> doF (W.shift "chat") | g <- myClassChatShifts ]
    ]
    where
       --  viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassMainShifts = ["gnome-terminal"]
        myClassWebShifts  = ["Firefox"]
        myClassVBoxShifts = ["VirtualBox"]
        myClassDevShifts = ["emacs"]
        myClassFloatShifts = ["gimp"]
        myClassMiscShifts = ["nautilus"]
	myClassMediaShifts = ["vlc"]
	myClassIdeaShifts = ["jetbrains-idea-ce", "sun-awt-X11-XFramePeer", "jetbrains-android-studio"]	
        myClassChatShifts = ["Pidgin"]


main = do
  
  xmproc <- spawnPipe "/usr/bin/xmobar $HOME/.xmobarrc"
  xmonad $ defaultConfig
    {
      borderWidth = 0
    , XMonad.focusedBorderColor = "grey"
    , manageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> myManageHook
                   <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , logHook = takeTopFocus <+> dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    , startupHook = do
        spawn "firefox"
        spawn "emacs"
        spawn "gnome-terminal -e screen"
	spawn "xmodmap ~/.dotfiles/.Xmodmap"
	spawn "xrandr --output VGA1 --right-of DP1"
        spawn "xscreensaver &"
        spawn "xsetroot -cursor_name left_ptr"
        spawn "feh --bg-fill /home/local/ANT/mutarjun/Pictures/Wallpapers/adwaita.jpg"
        setWMName "LG3D"
    , workspaces = myWorkspaces
    , terminal = "gnome-terminal -e screen"
    } `additionalKeys` myKeys
    
myKeys = [((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    ]
    ++
    [((m .|. mod4Mask, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
         | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    
-- main = do
--   xmonad $ defaultConfig
