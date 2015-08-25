{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

-- more informations:
-- http://xmonad.org/xmonad-docs/xmonad/index.html
-- http://xmonad.org/xmonad-docs/xmonad-contrib/index.html

-- basic stuff
import           Data.Char                       (isLetter)
import           Data.List                       (intercalate)
import qualified Data.Map                as M    (Map(..), union, fromList, lookup, toList, keys)
import           Data.Maybe                      (fromMaybe, mapMaybe)
import           GHC.IO.Handle.Types             (Handle)
import           System.Environment              (setEnv)
import           Text.Regex.Posix                ((=~))

-- xmonad stuff
import           XMonad                          
import           XMonad.Actions.CopyWindow       (copyWindow)
import qualified XMonad.Actions.Search   as S    (Browser, SearchEngine(..),
                                                  search, hoogle, google, hackage, isPrefixOf,
                                                  searchEngine, youtube)
import           XMonad.Actions.WindowBringer    (windowMap, bringWindow)
import           XMonad.Hooks.DynamicLog         (dynamicLogWithPP, defaultPP, dzenColor, shorten, wrap,
                                                  ppOutput, ppTitle, ppCurrent, ppLayout, ppUrgent,
                                                  ppOrder, ppSep)
import           XMonad.Hooks.ManageDocks        (avoidStruts, ToggleStruts(..))
import           XMonad.Hooks.UrgencyHook        (focusUrgent, withUrgencyHook, NoUrgencyHook(..))
import           XMonad.Hooks.Script             (execScriptHook)
import           XMonad.Prompt                   (XPrompt(..), XPConfig(..), XPPosition(Bottom),
                                                  defaultXPConfig, getNextCompletion, mkXPrompt,
                                                  historyCompletionP, mkComplFunFromList',
                                                  deleteAllDuplicates)
import           XMonad.Prompt.Shell             (shellPrompt, prompt)
import           XMonad.Prompt.XMonad            (xmonadPrompt)
import           XMonad.StackSet         as SS   (focusWindow)
import           XMonad.Util.Run                 (spawnPipe, hPutStrLn)
import           XMonad.Util.Types               (Direction2D(U, D))

-- some default configurations
terminal'     = "xterm"
borderWidth'  = 2
focusedColor' = cRed
borderColor'  = cGrey
modMask'      = mod4Mask
editor        = "emacsclient -c -a \"emacs\" "
dzenExec      = "dzen2"
xmobarExec    = "xmobar -f " ++ defaultFont 16 "normal"
browser       = "/home/odi/bin/browser.sh"
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
  { font              = defaultFont 17 "normal"
  , bgColor           = cBlack
  , fgColor           = cWhite
  , fgHLight          = cGrey
  , bgHLight          = cYellow
  , borderColor       = cGreen
  , promptBorderWidth = 2
  , position          = Bottom
  , height            = 24
  , defaultText       = []
  , historyFilter     = deleteAllDuplicates
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
keys' c = keys_ c `M.union` keys defaultConfig c

keys_ :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys_ (XConfig {modMask = modm}) = M.fromList
  [ ((modm, xK_Return),       spawn terminal')
  , ((modm, xK_g),            windowPromptGoto_ promptConf)
  , ((modm, xK_b),            windowPromptBring_ promptConf)
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
  , ((modm, xK_s), searchEnginePrompt promptConf browser S.google searchEngineMap)
  , ((modm, xK_r), prompt ("xterm" ++ " -e") promptConf)
    -- get keysym from `xev'
--  , ((0, 0x1008ff13), spawn "amixer sset Master 2%+")  -- increase volume
  , ((0, 0x1008ff13), spawn "avol.sh inc")  -- increase volume
  , ((0, 0x1008ff11), spawn "avol.sh dec")  -- decrease volume
  , ((modm .|. shiftMask, xK_c), spawn "emacs-float.sh")
--  , ((modm, xK_s), searchEnginePrompt promptConf duckduck searchEngineMap)
--  , ((modm, xK_o), urlHistoryPrompt promptConf (browserHistory Conkeror 
-- placesDB))
  ]

--------------------------------------------------------------------------------
-- Prompts

-- This is more or less equal to the `XMonad.Prompt.Window` except that we
-- use fuzzy matching for completing search predicates.
data WindowPrompt = Goto | Bring
instance XPrompt WindowPrompt where
    showXPrompt Goto      = "Go to window: "
    showXPrompt Bring     = "Bring window: "
    commandToComplete _ c = c
    nextCompletion      _ = getNextCompletion

doPrompt :: WindowPrompt -> Bool -> XPConfig -> X ()
doPrompt t fuzzy conf = do
    a <- case t of
        Goto      -> fmap gotoAction windowMap
        Bring     -> fmap bringAction windowMap
    wm <- windowMap
    mkXPrompt t conf (compList wm) a
    where
        winAction a m    = flip whenJust (windows . a) . flip M.lookup m
        gotoAction       = winAction SS.focusWindow
        bringAction      = winAction bringWindow

        compList m s = if fuzzy
                       then return $ mkComplListFuzzy (map fst . M.toList $ m) s
                       else return . filter (searchPredicate conf s) . map fst . M.toList $ m 

windowPromptGoto, windowPromptBring :: XPConfig -> X ()
windowPromptGoto  = doPrompt Goto False
windowPromptBring = doPrompt Bring False

windowPromptGoto_, windowPromptBring_ :: XPConfig -> X ()
windowPromptGoto_  = doPrompt Goto True
windowPromptBring_ = doPrompt Bring True

-- | Function for fuzzy pattern matching
-- TODO: make it not case sensitive
mkComplListFuzzy :: [String] -> String -> [String]
mkComplListFuzzy ss p = mapMaybe (\x -> match x (filter isLetter p)) ss
  where
    fuzzyMatch s p = s =~ (intercalate ".*?" $ map (:[]) p)
    match s p      = if fuzzyMatch s p then Just s else Nothing

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
                   -> M.Map String S.SearchEngine       -- ^ map of: name -> search-engine
                   -> X ()
searchEnginePrompt config browser engine sem =
    mkXPrompt SearchEngine config complF fireSearchEngine
    where
        complF = mkComplFunFromList' $ M.keys searchEngineMap

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
searchEngineMap :: M.Map String S.SearchEngine
searchEngineMap = M.fromList $ map se
    [ S.google, S.hoogle, S.hackage, hayoo, S.youtube ]
    where
        se :: S.SearchEngine -> (String, S.SearchEngine)
        se x@(S.SearchEngine name _) = (name, x)

        hayoo = S.searchEngine "hayoo" "http://hayoo.fh-wedel.de/?query="


manageHook' :: ManageHook
manageHook' = composeAll (
  [ stringProperty "WM_NAME" =? "emacs-capture" --> doFloat
  ] )
--------------------------------------------------------------------------------

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
  h <- spawnPipe $ workspaceBar width "l" 0
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
    , manageHook         = manageHook'
    , keys               = keys'
    , startupHook        = execScriptHook "startup"
    }
