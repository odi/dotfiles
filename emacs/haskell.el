;; ## haskell-mode
(require 'haskell)
(require 'haskell-indentation)
(require 'haskell-decl-scan)

(add-hook 'haskell-mode-hook 'turn-on-haskell-identation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

(define-key haskell-mode-map (kbd "C-c l") 'haskell-check)

(setq compile-command "nix-shell --command 'cabal build'")
(define-key haskell-mode-map (kbd "C-c ,") 'compile)


(setq haskell-indentation-layout-offset 4)
(setq haskell-indentation-starter-offset 4)
(setq haskell-indentation-left-offset 4)
(setq haskell-indentation-ifte-offset 4)
(setq haskell-indentation-where-pre-offset 4)
(setq haskell-indentation-where-post-offset 4)

;; call hoogle online w/ eww
(defun eww-hoogle (str)
  (interactive "sEnter hoogle search string: ")
  (eww (concat "https://www.haskell.org/hoogle/?hoogle=" str)))
