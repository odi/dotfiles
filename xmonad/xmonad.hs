{-# LANGUAGE OverloadedStrings #-}
{-
TODO:
 - get width of Screen for logHook
 - UTF8 in title
   -> using decodeString from utf8-string
   -> works for title but not for seperator
      -> works for seperator if using encodeString
 - UTF8 in shell-prompt
 - move history part and search engine to xmonad-ext?
-}

-- more informations:
-- http://xmonad.org/xmonad-docs/xmonad/index.html
-- http://xmonad.org/xmonad-docs/xmonad-contrib/index.html

import           XMonad

import qualified XMonad.Actions.Search   as S  (Browser, SearchEngine(..),
                                                search, hoogle, google, hackage, isPrefixOf,
                                                searchEngine)

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook


import           XMonad.Prompt
import           XMonad.Prompt.AppLauncher
import           XMonad.Prompt.Input
import           XMonad.Prompt.XMonad
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window

import           XMonad.Util.Font
import           XMonad.Util.Loggers
import           XMonad.Util.Run
import           XMonad.Util.Replace

import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           GHC.IO.Handle.Types

import           Control.Applicative

import           System.Environment

-- some default configurations
terminal'     = "xterm"
borderWidth'  = 2
focusedColor' = cRed
borderColor'  = cGrey
modMask'      = mod4Mask
editor        = "emacsclient -c -a \"emacs\" "
dzenExec      = "dzen2"
xmobarExec    = "xmobar -f " ++ (defaultFont 16 "normal")
browser       = "conkeror"
placesDB      = "/home/odi/.conkeror.mozdev.org/conkeror/l65w5mjs.odi/places.sqlite"

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
  { font              = defaultFont 16 "normal"
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
  , ppUrgent  = dzenColor cBlack cRed . wrap "[" "]"
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
  , ((modm, xK_g), windowPromptGoto promptConf)
  , ((modm, xK_b), windowPromptBring promptConf)
    -- dirty hack of switching to firefox/vimperator windows
--  , ((modm, xK_v), windowPromptGotoPropClass "conkeror" promptConf)
  , ((modm, xK_u), sendMessage $ ToggleStrut U)
  , ((modm, xK_i), sendMessage $ ToggleStrut D)
  , ((modm .|. shiftMask, xK_l), spawn "lock.sh")
  , ((modm, xK_x), xmonadPrompt promptConf)
  , ((modm, xK_p), shellPrompt promptConf)
  , ((modm, xK_BackSpace), focusUrgent)
  , ((modm, xK_q), spawn "pkill dzen2; pkill xmobar" >> restart "xmonad" True)
  , ((modm .|. shiftMask, xK_l), spawn "lock.sh")
  , ((modm, xK_s), searchEnginePrompt promptConf chromium S.google searchEngineMap)
  , ((modm, xK_r), prompt ("xterm" ++ " -e") promptConf)
    -- get keysym from `xev'
--  , ((0, 0x1008ff13), spawn "amixer sset Master 2%+")  -- increase volume
  , ((0, 0x1008ff13), spawn "avol.sh inc")  -- increase volume
  , ((0, 0x1008ff11), spawn "avol.sh dec")  -- decrease volume
--  , ((modm, xK_s), searchEnginePrompt promptConf duckduck searchEngineMap)
--  , ((modm, xK_o), urlHistoryPrompt promptConf (browserHistory Conkeror 
-- placesDB))
  ]

chromium = "/home/odi/.nix-profile/bin/chromium"

--------------------------------------------------------------------------------
-- Search Engines

-- Defines a new datatype for my search-engine prompt.
-- Creates a function for two-level prompting
--   1. ask which search-engine to use
--   2. ask for the search-string
data SearchEngine = SearchEngine
data Search = Search String

instance XPrompt SearchEngine where
    showXPrompt SearchEngine   = "Search-Engine: "
    commandToComplete _ c      = c
    nextCompletion _           = getNextCompletion

instance XPrompt Search where
    showXPrompt (Search name) = "Search [" ++ name ++ "]: "
    commandToComplete _ c     = c
    nextCompletion _          = getNextCompletion

-- | Define a prompt for searching through some search-engines.
-- It consists of two prompts; first it will ask which search-engine to use and
-- second for the search-string.
searchEnginePrompt :: XPConfig                          -- ^ xpconfig to use
                   -> S.Browser                         -- ^ which browser to use
                   -> S.SearchEngine                    -- ^ default Search-Engine
                   -> M.Map (String) (S.SearchEngine)   -- ^ map of: name -> search-engine
                   -> X ()
searchEnginePrompt config browser engine sem = do
    mkXPrompt SearchEngine config complF fireSearchEngine
    where
        complF = historyCompletionP ("Search-Engine:" `S.isPrefixOf`)
        fireSearchEngine :: String -> X ()
        fireSearchEngine name = promptSearchBrowser config browser $
                                   fromMaybe engine (M.lookup name sem)

-- extend `promptSearchBrowser` from XMonad.Actions.Search to only use
-- history for one search history instead of all seach-engine histories
promptSearchBrowser :: XPConfig -> S.Browser -> S.SearchEngine -> X ()
promptSearchBrowser config browser (S.SearchEngine name site) =
    mkXPrompt (Search name) config (complF name) $ S.search browser site
    where
        complF name = historyCompletionP (("Search [" ++ name) `S.isPrefixOf`)

-- a map of my search-engines
searchEngineMap :: M.Map (String) (S.SearchEngine)
searchEngineMap = M.fromList $ map se
    [ S.google, S.hoogle, S.hackage, hayoo ]
    where
        se :: S.SearchEngine -> (String, S.SearchEngine)
        se x@(S.SearchEngine name _) = (name, x)

        hayoo = S.searchEngine "hayoo" "http://hayoo.fh-wedel.de/?query="


-- TODO: move it to a util module
-- mkComplListFuzzy :: [String] -> String -> IO [String]
-- mkComplListFuzzy ss p = return $ mapMaybe (\x -> match x (filter isLetter p)) ss
--   where
--     fuzzyMatch s p = s =~ (intercalate ".*?" $ map (:[]) p)
--     match s p      = if (fuzzyMatch s p) then Just s else Nothing


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
          , "-fn", "'" ++ defaultFont 16 "normal" ++ "'"
          ]

-- call `xmobar' with a configuration-file
xmobarBar :: FilePath -> String
xmobarBar fp = unwords [ xmobarExec, fp ]

getFontByHost :: String
getFontByHost = undefined

main :: IO ()
main = do
  setEnv "BROWSER" browser
  -- TODO: get width from Graphics.X11.Xrandr?
  let width = 683 -- half size of screen
  h <- spawnPipe $ workspaceBar (width) "l" 0
  spawnPipe $ xmobarBar "/home/odi/.xmonad/infoBarrc"    -- infoBar top-right
--  spawnPipe $ xmobarBar "/home/odi/conf/statusBarrc"  -- stautsBar bottom
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
