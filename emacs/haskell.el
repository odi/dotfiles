;; ## haskell-mode

(require 'haskell)

;; turn on scanning of declerations
(require 'haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; turn on indentation
(require 'haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; indentation configure -> default 4 spaces
(setq haskell-indentation-layout-offset 4
      haskell-indentation-starter-offset 4
      haskell-indentation-left-offset 4
      haskell-indentation-ifte-offset 4
      haskell-indentation-where-pre-offset 2
      haskell-indentation-where-post-offset 2)

;; turn on unicode input
(require 'haskell-unicode-input-method)
(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)

;; turn on interactive mode
(require 'haskell-process)
(require 'haskell-interactive-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq haskell-process-log t)

;; turn on git-gutter by default
(add-hook 'haskell-mode-hook 'git-gutter-mode)

;; define hydra for haskell-stuff
(require 'hydra)
(defhydra hydra-haskell-dev (:hint nil)
  "
Haskell
_i_: toggle indent-guide   _._: jump to
_g_: git gutter            _y_: search in hayoo
_l_: load/reload
_r_: run in ROOT
"
  ("i" indent-guide-mode :color blue)
  ("g" git-gutter-mode :color blue)
  ("." haskell-mode-jump-to-def-or-tag :color blue)
  ("y" engine/search-hayoo :color blue)
  ("l" haskell-process-load-or-reload :color blue)
  ("r" projectile-run-shell-command-in-root :color blue)
  ("q" nil))
(define-key haskell-mode-map (kbd "C-c C-h") 'hydra-haskell-dev/body)

;; use nix-shell as wrapper around 'cabal repl'
(setq haskell-process-wrapper-function
      (lambda (argv)
	(append (list "nix-shell" "--command")
		(list (mapconcat 'identity argv " ")))))

;; turn on presentation-mode for C-c C-i and C-c C-t
(setq haskell-process-use-presentation-mode t)
(defun haskell-quit-and-clear-presentation ()
  (interactive)
  (haskell-presentation-clear)
  (quit-window))
;; delete and quit presentation-window with 'Q'
(define-key haskell-presentation-mode-map (kbd "Q")
  'haskell-quit-and-clear-presentation)

;; keyboard shortcuts
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

;; define search-engines
(defengine hayoo
  "http://hayoo.fh-wedel.de/?query=%s"
  :keybinding "y")
(defengine haskell-wiki
  "https://wiki.haskell.org/Special:Search?search=%s"
  :keybinding "w")

