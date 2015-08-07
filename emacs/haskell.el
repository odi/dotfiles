;; ## haskell-mode

(require 'haskell)
(require 'haskell-indentation)
(require 'haskell-decl-scan)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'haskell-unicode-input-method)
(require 'w3m)
(require 'w3m-haddock)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(add-hook 'haskell-mode-hook 'git-gutter-mode)

(setq haskell-process-log t)

;; use nix-shell as wrapper around 'cabal repl'
(setq haskell-process-wrapper-function
      (lambda (argv)
	(append (list "nix-shell" "--command")
		(list (mapconcat 'identity argv " ")))))

;; search online-hoogle instead of local hoogle-db
(setq haskell-hoogle-command nil)

;; indentation configure
(setq haskell-indentation-layout-offset 4
      haskell-indentation-starter-offset 4
      haskell-indentation-left-offset 4
      haskell-indentation-ifte-offset 4
      haskell-indentation-where-pre-offset 4
      haskell-indentation-where-post-offset 4)

;; (define-key haskell-mode-map (kbd "C-c l") 'haskell-check)

;; (setq compile-command "nix-shell --command 'cabal build'")
;; (define-key haskell-mode-map (kbd "C-c ,") 'compile)

;; ;; call hoogle online w/ eww
;; (defun eww-hoogle (str)
;;   (interactive "sEnter hoogle search string: ")
;;   (eww (concat "https://www.haskell.org/hoogle/?hoogle=" str)))
