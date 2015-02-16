
{-
TODO:
 - get width of Screen for logHook
 - UTF8 in title
   -> using decodeString from utf8-string
   -> works for title but not for seperator
      -> works for seperator if using encodeString
 - UTF8 in shell-prompt
-}

-- more informations:
-- http://xmonad.org/xmonad-docs/xmonad/index.html
-- http://xmonad.org/xmonad-docs/xmonad-contrib/index.html

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

-- imported from xmonad-ext
import XMonad.Libs.Completion

import XMonad.Prompt
import XMonad.Prompt.AppLauncher
import XMonad.Prompt.XMonad
import XMonad.Prompt.Shell

import XMonad.Util.Font
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Replace

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.IO.Handle.Types
import Codec.Binary.UTF8.String

-- some default configurations
terminal'     = "xterm"
borderWidth'  = 2
focusedColor' = cRed
borderColor'  = cGrey
modMask'      = mod4Mask
editor        = "emacsclient -c -a \"emacs\" "
dzenExec      = "dzen2"
xmobarExec    = "/home/odi/progs/xmobar-0.22.1/.cabal-sandbox/bin/xmobar"

-- used colors
cRed     = "#ff6347"
cGreen   = "#c0ff3e"
cYellow  = "#ffc125"
cBlue    = "#1e90ff"
cMagenta = "#ff34b3"
cCyan    = "#00ced1"
cBlack   = "#000000"
cWhite   = "#ffffff"
cGrey    = "#222222"
cLGrey   = "#c0c0c0"

-- font with parameterized size and weight
-- TODO: maybe switch to `XMonad.Util.Font'.
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-Font.html
defaultFont :: Int -> String -> String
defaultFont size weight =
  concat [ "xft:monospace:pixelsize="
         , show size
         , ":weight="
         , weight
         ]

-- configuration for the shell-prompt
promptConf :: XPConfig
promptConf = defaultXPConfig
  { font              = defaultFont 15 "normal"
  , bgColor           = cBlack
  , fgColor           = cWhite
  , fgHLight          = cGrey
  , bgHLight          = cYellow
  , borderColor       = cGreen
  , promptBorderWidth = 2
  , position          = Bottom
  , height            = 24
  , defaultText       = []
  }

logHook' :: Handle -> X ()
logHook' h = dynamicLogWithPP $ defaultPP
  { ppOutput  = hPutStrLn h
  , ppTitle   = dzenColor cGrey cYellow . shorten 70 . wrap " " " "
  , ppCurrent = dzenColor cYellow "" . wrap "[" "]"
  , ppLayout  = dzenColor cBlue ""
  , ppUrgent  = dzenColor cWhite cRed . wrap "!" "!"
  , ppSep     = " • "
  , ppOrder   = \(ws:l:t:_) -> [ws,l,t]
  }

layoutHook' = avoidStruts $ layoutHook defaultConfig

-- use default keys and overwrite it with keys_
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' = \c -> keys_ c `M.union` keys defaultConfig c

keys_ :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys_ (XConfig {modMask = modm}) = M.fromList $
  [ ((modm, xK_Return), spawn terminal')
  , ((modm, xK_g), windowPromptGoto' promptConf)
  , ((modm, xK_b), windowPromptBring' promptConf)
    -- dirty hack of switching to firefox/vimperator windows
  , ((modm, xK_v), windowPromptGotoPropClass "vimperator" promptConf)
  , ((modm, xK_u), sendMessage $ ToggleStrut U)
  , ((modm, xK_i), sendMessage $ ToggleStrut D)
  , ((modm .|. shiftMask, xK_l), spawn "lock.sh")
  , ((modm, xK_x), xmonadPrompt promptConf)
  , ((modm, xK_p), shellPrompt promptConf)
  , ((modm, xK_BackSpace), focusUrgent)
  , ((modm, xK_q), spawn "killall dzen2 xmobar" >> restart "xmonad" True)
  , ((modm, xK_l), launchApp promptConf "firefox-nw.sh")
  ]

{-
 | workspaceBar |              |infoBar |
 |--------------------------------------|
 | ~
 |--------------------------------------|
 | statusBar                            |

workspaceBar ... works with dzen2 and will spawn by logHook from XMonad
infoBar      ... xmobar (top) with major information about the system
statusBar    ... xmobar (bottom) with minor informations about the system

the workspaceBar + infoBar could be toggled with modm + u and the statusBar
could be toggled with modm + i.
-}

-- workspaceBar
-- the logHook will be spawned to it
-- h <- spawnPipe $ workspaceBar width "l" 0
-- xmonad $ defaultConfig {
--   logHook = logHook' h
-- }
workspaceBar :: Int -> String -> Int -> String
workspaceBar w a x =
  unwords [ dzenExec
          , "-w", show w, "-ta", a, "-x", show x
          , "-bg", "'" ++ cBlack ++ "'"
          , "-fn", "'" ++ defaultFont 14 "normal" ++ "'"
          ]

-- call `xmobar' with a configuration-file
xmobarBar :: FilePath -> String
xmobarBar fp = unwords [ xmobarExec, fp ]

main :: IO ()
main = do
  -- TODO: get width from Graphics.X11.Xrandr?
  let width = 683 -- half size of screen
  h <- spawnPipe $ workspaceBar (width) "l" 0
  spawnPipe $ xmobarBar "/home/odi/conf/infoBarrc"    -- infoBar top-right
  spawnPipe $ xmobarBar "/home/odi/conf/statusBarrc"  -- stautsBar bottom
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal           = terminal'
    , modMask            = modMask'
    , borderWidth        = borderWidth'
    , normalBorderColor  = borderColor'
    , focusedBorderColor = focusedColor'
    , logHook            = logHook' h
    , layoutHook         = layoutHook'
    , keys               = keys'
    }
