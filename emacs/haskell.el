;; ## haskell-mode

(require 'haskell)
(require 'haskell-decl-scan)
(require 'haskell-interactive-mode)
(require 'haskell-process)
;;(require 'haskell-font-lock)
;; ;;(require 'haskell-unicode-input-method)
;; (require 'w3m)
;; (require 'w3m-haddock)

;; turn on unicode input
(require 'haskell-unicode-input-method)
(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)

;; ## TEST ## hi2 indentation
;; https://github.com/nilcons/hi2
(add-to-list 'load-path "~/.emacs.d/elisp/hi2")
(require 'hi2)
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(setq hi2-layout-offset 4)
(setq hi2-ifte-offset 4)
(setq hi2-left-offset 4)
(setq hi2-where-pre-offset 4)
(setq hi2-where-post-offset 4)

;; use indent-guide
(add-hook 'haskell-mode-hook 'indent-guide-mode)

;; ;; haskell indentation
;; (require 'haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (setq haskell-indentation-layout-offset 4
;;       haskell-indentation-starter-offset 4
;;       haskell-indentation-left-offset 4
;;       haskell-indentation-ifte-offset 4
;;       haskell-indentation-where-pre-offset 4
;;       haskell-indentation-where-post-offset 4)

(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'git-gutter-mode)

;; TODO: does not work properly
;;(add-hook 'haskell-mode-hook 'haskell-doc-current-info)

(custom-set-variables
 '(haskell-font-lock-symbols 'unicode))
;; load file again because of not reading haskell-font-lock-symbols
(load-file "~/.emacs.d/elisp/haskell-mode/haskell-font-lock.el")

(setq haskell-process-log t)

;; use nix-shell as wrapper around 'cabal repl'
(setq haskell-process-wrapper-function
      (lambda (argv)
	(append (list "nix-shell" "--command")
		(list (mapconcat 'identity argv " ")))))

;; ;; search online-hoogle instead of local hoogle-db
;; (setq haskell-hoogle-command nil)

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
