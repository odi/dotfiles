
{-
TODO:
 - Volume percentage in rightHook
 - get width of Screen for logHook
 - UTF8 in title
   -> using decodeString from utf8-string
   -> works for title but not for seperator
      -> works for seperator if using encodeString
 - UTF8 in shell-prompt
-}

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

-- imported from xmonad-ext
import XMonad.Libs.Completion

import XMonad.Prompt
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

-- Logger for notmuch counting queries.
logMailNotmuch :: String -> X (Maybe String)
logMailNotmuch query = logCmd ("notmuch count " ++ query)

-- Logger for network interfaces.
logActiveIF :: X (Maybe String)
logActiveIF = do
  nif   <- logCmd ipCmd
  return nif
  where
    ipCmd = "ip a | grep \"state UP\" | awk '{print $2}' | tr -d ':'"

-- Logger for ESSID
logActiveESSID :: X (Maybe String)
logActiveESSID =
  logCmd $ unwords [ "iwconfig wlp2s0 | grep ESSID "
                   , "| sed -e 's/off\\/any//g'"
                   , "| awk -F\":\" '{print $2}' "
                   , "| tr -d '\"' | tr -d ' '"
                   ]

-- Logger for memory usage
logMemUsage :: X (Maybe String)
logMemUsage = do
  mem <- logCmd ("ps aux | awk '{sum +=$4}; END {print sum}'")
  let val = round (read (fromMaybe "0" mem) :: Double)
  case (val < 90) of
   True  -> wrapL "" "%" . dzenColorL "" "" $ return (Just (show val))
   False -> wrapL "" "%" . dzenColorL cRed "" $ return (Just (show val))

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
      , ppUrgent  = dzenColor cWhite cRed . wrap "!" "!"
      , ppSep     = " • "
      , ppOrder   = \(ws:l:t:_) -> [ws,l,t]
      }
    rightPP rh = defaultPP    -- configuration of right logHook
      { ppOutput = hPutStrLn rh
      , ppSep    = " • "
      , ppExtras =
        [ dzenColorL cGreen "" $ logActiveIF
        , dzenColorL cMagenta "" $ logActiveESSID
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
  , ((modm .|. shiftMask, xK_l), spawn "lock.sh")
  , ((modm, xK_x), xmonadPrompt promptConf)
  , ((modm, xK_p), shellPrompt promptConf)
  , ((modm, xK_BackSpace), focusUrgent)
  , ((modm, xK_q), spawn "killall dzen2" >> restart "xmonad" True)
  ]

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
  let width = 683 -- half size of screen
  lh <- spawnPipe $ dzenBar (width) "l" 0
  rh <- spawnPipe $ dzenBar (width) "r" (0 - width)
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal           = terminal'
    , modMask            = modMask'
    , borderWidth        = borderWidth'
    , normalBorderColor  = borderColor'
    , focusedBorderColor = focusedColor'
    , logHook            = logHook' lh rh
    , layoutHook         = layoutHook'
    , keys               = keys'
    }
