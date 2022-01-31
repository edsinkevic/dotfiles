import XMonad
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.MouseResize ( mouseResize )
import XMonad.Actions.RotSlaves (rotAllDown)
import XMonad.Actions.WithAll (sinkAll)
import XMonad.Actions.CycleWS ( prevWS, nextWS )
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)

    -- Data
import Data.Maybe ( fromJust )
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.StatusBar.PP
    ( wrap, xmobarPP, xmobarColor, shorten, PP(..) )
import XMonad.Hooks.EwmhDesktops ( ewmh )  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Hooks.StatusBar (withSB, StatusBarConfig, statusBarProp)

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat ( simplestFloat )
import XMonad.Layout.ResizableTile ( ResizableTall(ResizableTall) )
import XMonad.Layout.Magnifier (magnifier)

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier ( ModifiedLayout )
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
    ( noBorders, withBorder, smartBorders )
import XMonad.Layout.Renamed ( renamed, Rename(Replace) )
import XMonad.Layout.Spacing
    ( spacingRaw,
      incScreenSpacing,
      decScreenSpacing,
      incWindowSpacing,
      decWindowSpacing,
      Border(Border),
      Spacing )
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.WindowNavigation ( windowNavigation )
import XMonad.Layout.IndependentScreens (countScreens)
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce ( spawnOnce )

   -- Prompts
import XMonad.Prompt
    ( XPPosition(Top),
      XPConfig(font, fgColor, position, height, promptBorderWidth) )
import XMonad.Prompt.Shell ( shellPrompt )

colorScheme :: [Char]
colorScheme = "doom-one"
colorBack :: [Char]
colorBack = "#282c34"

color02 :: [Char]
color02 = "#ff6c6b"
color08 :: [Char]
color08 = "#d356a2"
color15 :: [Char]
color15 = "#40e0d0"
color16 :: [Char]
color16 = "#dfdfdf"
black :: [Char]
black = "#000000"
white :: [Char]
white = "#ffffff"

myTerminal :: String
myTerminal      = "alacritty"

myBrowser :: String
myBrowser       = "firefox"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth   = 1

myFont :: String
myFont = "xft:Mononoki:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask       = mod4Mask

myWorkspaces :: [[Char]]
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)

myNormColor :: String
myNormColor   = colorBack

myFocusColor :: String
myFocusColor  = color15
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
--
-- START_KEYS
myKeys :: [(String, X ())]
myKeys =
    -- KB_GROUP Xmonad
        [ ("M-M1-r", spawn "xmonad --recompile")       -- Recompiles xmonad
        , ("M-M1-S-r", spawn "xmonad --restart")         -- Restarts xmonad
        , ("M-M1-q", io exitSuccess)                   -- Quits xmonad

    -- KB_GROUP Run Prompt
        , ("M-d", shellPrompt shellConfig)

    -- KB_GROUP Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn myTerminal)
        , ("M-b", spawn myBrowser)

    -- KB_GROUP Kill windows
        , ("M-S-q", kill1)     -- Kill the currently focused client

    -- KB_GROUP Workspaces
        , ("M-S-<KP_Add>", nextWS)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", prevWS)  -- Shifts focused window to prev ws
        , ("M-e", viewEmptyWorkspace)  -- Shifts focused window to prev ws

    -- KB_GROUP Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- KB_GROUP Increase/decrease spacing (gaps)
        , ("C-M1-j", decWindowSpacing 4)         -- Decrease window spacing
        , ("C-M1-k", incWindowSpacing 4)         -- Increase window spacing
        , ("C-M1-h", decScreenSpacing 4)         -- Decrease screen spacing
        , ("C-M1-l", incScreenSpacing 4)         -- Increase screen spacing

    -- KB_GROUP Windows navigation
        --, ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        --, ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        --, ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        --, ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- KB_GROUP Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    -- KB_GROUP Monitors
        , ("M-M1-<Tab>", toggleExtMonitor)           -- Switch to next layout
        , ("M-M1-S-<Tab>", toggleLapMonitor)           -- Switch to next layout

    -- KB_GROUP Language
        , ("M-m", spawn "~/Code/bash/scripts/swlang.sh")           -- Switch to next layout
        ]
        -- ++ [(otherModMasks ++ "M-" ++ [key], action tag) | (tag, key)  <- zip myWorkspaces "123456789", (otherModMasks, action) <- [ ("", windows . W.view), ("S-", windows . W.shift)]]
-- END_KEYS
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []

monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ windowNavigation
           $ limitWindows 20 Full

magnify  = renamed [Replace "magnify"]
           $ smartBorders
           $ windowNavigation
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []

grid     = renamed [Replace "grid"]
           $ smartBorders
           $ windowNavigation
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)

floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
    where
        myDefaultLayout =     withBorder myBorderWidth tall
                          ||| noBorders monocle
                          ||| grid
                          ||| magnify

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

myStartupHook :: X ()
myStartupHook = do
    spawn "killall conky"
    spawn "killall trayer"
    --spawn "$HOME/Code/bash/scripts/start_picom.sh"
    spawnOnce "/usr/bin/emacs --daemon"
    spawnOnce "nm-applet &"
    spawn ("sleep 2 && conky -c $HOME/.config/conky/xmonad/" ++ colorScheme ++ "-01.conkyrc")
    spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 " ++ "--tint 0x000000" ++ " --height 22")
    spawn "feh --bg-fill ~/.wallpapers/peakpx_colorified.jpg"
    setWMName "LG3D"

main :: IO ()
main = do
    xmonad
    . withSB (xmobarPropOnMonitor 0 <> xmobarPropOnMonitor 1)
    . docks
    $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        }
        `additionalKeysP` myKeys

-- My functions

toggleExtMonitor :: X ()
toggleExtMonitor = do
    screencount <- countScreens
    if screencount > (1 :: Integer)
       then spawn "xrandr --output DP-2 --off" >> myStartupHook
     else spawn "xrandr --output DP-2 --auto --left-of DP-3" >> myStartupHook

toggleLapMonitor :: X ()
toggleLapMonitor = do
    screencount <- countScreens
    if screencount > (1 :: Integer)
       then spawn "xrandr --output DP-3 --off" >> myStartupHook
     else spawn "xrandr --output DP-3 --primary --mode 2560x1440 --rate 144 --right-of DP-2" >> myStartupHook

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

clickable :: [Char] -> [Char]
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

-- My configs

xmobarPPConfig :: PP
xmobarPPConfig = otherBS
    where otherBS = xmobarPP
              {
                ppCurrent = xmobarColor color08 "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ color08 ++ ">") "</box>"
              , ppVisible = xmobarColor color08 "" . clickable
              , ppHidden = xmobarColor color15 "" . wrap ("<box type=Top width=2 mt=2 color=" ++ color15 ++ ">") "</box>" . clickable
              , ppHiddenNoWindows = xmobarColor black ""  . clickable
              , ppTitle = xmobarColor color16 "" . shorten 40
              , ppSep =  "<fc=" ++ white ++ "> <fn=1>|</fn> </fc>"
              , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
              , ppExtras  = [windowCount]
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
              }

xmobarPropOnMonitor :: Int -> StatusBarConfig
xmobarPropOnMonitor monitorNumber =
    statusBarProp ("xmobar -x " ++ show monitorNumber ++ " $HOME/.config/xmobar/xmobarrc") $ pure xmobarPPConfig


shellConfig :: XPConfig
shellConfig = def
    { font = "xft:Terminus:pixelsize=15:width=1:antialias=true:hinting=true"
    , fgColor = color08
    , position = Top
    , height = 24
    , promptBorderWidth = 0
    }
