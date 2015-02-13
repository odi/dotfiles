
import XMonad
import XMonad.Prompt

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Util.Font
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Replace

import Data.Maybe (fromMaybe)
import GHC.IO.Handle.Types

import Data.Traversable (traverse)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W
import Data.Maybe (maybeToList)
import Data.List ((\\))

-- some default configurations
terminal'     = "xterm"
borderWidth'  = 2
focusedColor' = cRed
borderColor'  = cGrey
modMask'      = mod4Mask
editor        = "emacsclient -c -a \"emacs\" "
dzenExec      = "dzen2"

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
--  concat [ "xft:DejaVu Sans Mono:pixelsize="
  concat [ "xft:monospace:pixelsize="
         , show size
         , ":weight="
         , weight
         ]

-- configuration for the shell-prompt
shellPrompt :: XPConfig
shellPrompt = defaultXPConfig
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

logTitles :: X (Maybe String)
logTitles = withWindowSet $
            fmap (Just . unwords)
            . traverse (fmap show . getName)
            . (\ws -> W.index ws \\ maybeToList (W.peek ws))

-- Logger for notmuch counting queries.
logMailNotmuch :: String -> X (Maybe String)
logMailNotmuch query = logCmd ("notmuch count " ++ query)

-- Logger for network interfaces.
-- TODO: show ESSID if interface is wlp2s0
logActiveIF :: X (Maybe String)
logActiveIF = do
  nif <- logCmd ("ip a | grep \"state UP\" | awk '{print $2}' | tr -d :")
  essid <- logCmd ("iwconfig wlp2s0 | grep ESSID | awk -F\":\" '{print $2}'")
  dzenColorL cGreen "" $ return nif

-- Logger for memory usage
logMemUsage :: X (Maybe String)
logMemUsage = do
  mem <- logCmd ("ps aux | awk '{sum +=$4}; END {print sum}'")
  case ((read (fromMaybe "0" mem) :: Double) < 90) of
   True  -> wrapL "" "%" . dzenColorL "" "" $ return mem
   False -> wrapL "" "%" . dzenColorL cRed "" $ return mem

-- Logger for audio volume
-- aumixVolume does not work for me
logAMixer :: X (Maybe String)
logAMixer = do
  vol <- logCmd $ concat
         [ "amixer sget Master "
         , "| grep \"Front Left:\" | cut -d\" \" -f7 "
         , "| tr -d \"[\" | tr -d \"]\" | tr -d \"%\""
         ]
  wrapL "" "%" . dzenColorL "" "" $ return vol

logHook' :: Handle -> Handle -> X ()
logHook' lh rh =
  (dynamicLogWithPP $ leftPP lh) >> (dynamicLogWithPP $ rightPP rh)
  where
    leftPP lh  = defaultPP    -- configuration of left logHook
      { ppOutput  = hPutStrLn lh
      , ppTitle   = dzenColor cGrey cYellow . shorten 70
      , ppCurrent = dzenColor cYellow "" . wrap "[" "]"
      , ppLayout  = dzenColor cBlue ""
      , ppSep     = " • "
      }
    rightPP rh = defaultPP    -- configuration of right logHook
      { ppOutput = hPutStrLn rh
      , ppSep    = " • "
      , ppExtras =
        [ logActiveIF
        , logMemUsage
        , dzenColorL cCyan "" $ logMailNotmuch "tag:unread"
        , dzenColorL cRed "" $ logMailNotmuch "tag:flagged"
        , dzenColorL cLGrey "" $ battery
        , dzenColorL cLGrey "" $ date "%a %b %d"
        , dzenColorL cBlue "" $ date "[%V]"
        , dzenColorL cWhite "" $ date "%R "
        ]
      , ppOrder  = \(ws:l:_:xs) -> xs
      }

layoutHook' = avoidStruts $ layoutHook defaultConfig

dzenBar :: Int -> String -> Int -> String
dzenBar w a x =
  unwords [ dzenExec
          , "-w", show w
          , "-ta", a
          , "-bg", "'" ++ cBlack ++ "'"
          , "-fn", "'" ++ defaultFont 14 "normal" ++ "'"
          , "-x", show x
          ]

main :: IO ()
main = do
  -- TODO: get width from Graphics.X11.Xrandr?
  let width = 640 -- half size of screen
  lh <- spawnPipe $ dzenBar (width) "l" 0
  rh <- spawnPipe $ dzenBar (width) "r" (0 - width)
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal           = terminal'
    , modMask            = mod1Mask
    , borderWidth        = borderWidth'
    , normalBorderColor  = borderColor'
    , focusedBorderColor = focusedColor'
    , logHook            = logHook' lh rh
    , layoutHook         = layoutHook'
    }
