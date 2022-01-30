import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess, exitWith, ExitCode(ExitSuccess))
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe ( fromJust, isJust )
import Data.Monoid
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.StatusBar.PP (filterOutWsPP)
import qualified XMonad.Hooks.StatusBar as StatusBar (withSB, statusBarProp, statusBarPropTo, StatusBarConfig, xmonadDefProp)

    -- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier (magnifier)

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import XMonad.Layout.IndependentScreens (countScreens)
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.ClickableWorkspaces
import Colors.DoomOne

   -- Prompts
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

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

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myTabTheme = def { fontName            = myFont
                 , activeColor         = color15
                 , inactiveColor       = color08
                 , activeBorderColor   = color15
                 , inactiveBorderColor = colorBack
                 , activeTextColor     = colorBack
                 , inactiveTextColor   = color16
                 }

myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

xpConfig :: XPConfig
xpConfig = def
    { font = "xft:Terminus:pixelsize=15:width=1:antialias=true:hinting=true"
    , fgColor = "#d356a2"
    , position = Top
    , height = 24
    , promptBorderWidth = 0
    -- , searchPredicate = fuzzyMatch
    }

-- Border colors for unfocused and focused windows, respectively.
--
--
myNormColor :: String       -- Border color of normal windows
myNormColor   = colorBack   -- This variable is imported from Colors.THEME

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = color15     -- This variable is imported from Colors.THEME
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
        , ("M-d", shellPrompt xpConfig)

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
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- KB_GROUP Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    -- KB_GROUP Monitors
        , ("M-M1-<Tab>", toggleExtMonitor)           -- Switch to next layout
        , ("M-M1-S-<Tab>", toggleLapMonitor)           -- Switch to next layout
        ]
        -- ++ [(otherModMasks ++ "M-" ++ [key], action tag) | (tag, key)  <- zip myWorkspaces "123456789", (otherModMasks, action) <- [ ("", windows . W.view), ("S-", windows . W.shift)]]
-- END_KEYS
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []

monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full

magnify  = renamed [Replace "magnify"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []

grid     = renamed [Replace "grid"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)

floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Terminus:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
    where
        myDefaultLayout =     withBorder myBorderWidth tall
                          ||| noBorders monocle
                          ||| grid
                          ||| magnify

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawn "killall conky"
    spawn "killall trayer"  -- kill current trayer on each restart
    spawn "$HOME/Code/bash/scripts/start_picom.sh"
    spawnOnce "/usr/bin/emacs --daemon"
    spawnOnce "nm-applet &"
    spawn ("sleep 2 && conky -c $HOME/.config/conky/xmonad/" ++ colorScheme ++ "-01.conkyrc")
    spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 " ++ "--tint 0x000000" ++ " --height 22")
    spawn "feh --bg-fill ~/.wallpapers/peakpx_colorified.jpg"  -- feh set random wallpaper
    setWMName "LG3D"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    -- let sb0 = StatusBar.statusBarPropTo StatusBar.xmonadDefProp ("xmobar -x 0 $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc") $ pure myXmobarPP
    xmonad . StatusBar.withSB (xmobarPropOnMonitor 0 <> xmobarPropOnMonitor 1) . docks $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
                               -- Uncomment this line to enable fullscreen support on things like YouTube/Netflix.
                               --d356a2 This works perfect on SINGLE monitor systems. On multi-monitor systems,
                               -- it adds a border around the window if screen does not have focus. So, my solution
                               -- is to use a keybinding to toggle fullscreen noborders instead.  (M-<Space>)
                               -- <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        } `additionalKeysP` myKeys

-- My functions

toggleExtMonitor :: X ()
toggleExtMonitor = do
    screencount <- countScreens
    if screencount > 1
       then spawn "xrandr --output DP-2 --off" >> myStartupHook
     else spawn "xrandr --output DP-2 --auto --left-of DP-3" >> myStartupHook

toggleLapMonitor :: X ()
toggleLapMonitor = do
    screencount <- countScreens
    if screencount > 1
       then spawn "xrandr --output DP-3 --off" >> myStartupHook
     else spawn "xrandr --output DP-3 --primary --mode 2560x1440 --rate 144 --right-of DP-2" >> myStartupHook

-- My configs

myXmobarPP :: PP
myXmobarPP = otherBS
    where otherBS = xmobarPP
              { 
                ppCurrent = xmobarColor "#d356a2" "" . wrap
                            ("<box type=Bottom width=2 mb=2 color=" ++ "#d356a2" ++ ">") "</box>"
                -- Visible but not current workspace
              , ppVisible = xmobarColor "#d356a2" "" . clickable
                -- Hidden workspace
              , ppHidden = xmobarColor "#40e0d0" "" . wrap
                           ("<box type=Top width=2 mt=2 color=" ++ "#40E0D0" ++ ">") "</box>" . clickable
                -- Hidden workspaces (no windows)
              , ppHiddenNoWindows = xmobarColor  "#000000" ""  . clickable
                -- Title of active window
              , ppTitle = xmobarColor color16 "" . shorten 40
                -- Separator character
              , ppSep =  "<fc=" ++ "#ffffff" ++ "> <fn=1>|</fn> </fc>"
                -- Urgent workspace
              , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
                -- Adding # of windows on current workspace to the bar
              , ppExtras  = [windowCount]
                -- order of things in xmobar
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
              }

xmobarPropOnMonitor :: Int -> StatusBar.StatusBarConfig
xmobarPropOnMonitor monitorNumber = 
    StatusBar.statusBarPropTo StatusBar.xmonadDefProp ("xmobar -x " ++ show monitorNumber ++ " $HOME/.config/xmobar/" ++ colorScheme ++ "-xmobarrc") $ pure myXmobarPP
