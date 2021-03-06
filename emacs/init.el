;; -*- emacs-lisp -*-
;; 2015-07-06 by Oliver Dunkl

;; this seams to be a bug to load some of the installed modules
;; see: http://xahlee.blogspot.co.at/2012/06/emacs-24-package-system-problems.html
(setq nix-dotfiles-dir "~/.nix-profile/share/emacs/site-lisp/")
;; load all modules from nix-dotfiles-dir/elpa into load-path
(dolist
    (module (directory-files (concat nix-dotfiles-dir "elpa") t "\\w+"))
  (when (file-directory-p module)
    (add-to-list 'load-path module)))

;; basic configurations
(tool-bar-mode -1)     ;; disable tool-bar
(scroll-bar-mode -1)   ;; disable scroll-bar
(tooltip-mode t)       ;; enable tool tips
(display-time-mode t)  ;; display time/date in modeline
(mouse-wheel-mode -1)  ;; disable mouse wheel
(show-paren-mode t)    ;; show fitting braces
(winner-mode t)        ;; activate winner-mode
(menu-bar-mode -1)     ;; disable menu-bar - get with F10
(column-number-mode t) ;; show column number too

;; prefered coding system
(prefer-coding-system 'utf-8)  ;; use UTF-8 as my prefered coding system

;; set title format of the frames
(setq frame-title-format '("" "emacs :: %b"))

;; yank content at point and from primary selection method
(setq mouse-yank-at-point t)
(setq x-select-enable-primary t)

;; `y` instead of `yes` and `n` instead of `no`
(defalias 'yes-or-no-p 'y-or-n-p)

;; backups and autosave
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq version-control t)
(setq vc-make-backup-files t)

;; default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-new-window-flag t
      browse-url-generic-program "browser.sh")

;; load all my configurations which i'll not show to the public
(load-file "~/etc/emacs/secure.el")

;; email configurations
;;(require 'gnus)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")

;; ## ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-0") 'ace-jump-mode)

;; ## bbdb
(require 'bbdb-loaddefs)
(require 'bbdb)
(setq bbdb-file "~/.bbdb")
(add-hook 'message-setup-hook 'bbdb-mail-aliases)

;; from: https://github.com/tohojo/bbdb-vcard
;; be sure to check out bbdb-vcard to ~/.emacs.d/elisp
(add-to-list 'load-path "~/.emacs.d/elisp/bbdb-vcard")
(require 'bbdb-vcard)
(setq bbdb-default-dir "~/data/vcards/")
;; TODO: xmpp -> X-JABBER
(setq bbdb-vcard-export-translation-table
      '(("Mobile" . "cell")
	("Work" . "work")))

;; ## TEST ## git-gutter
;; https://github.com/syohex/emacs-git-gutter
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-git-gutter")
(require 'git-gutter)
(add-hook 'emacs-lisp-mode-hook 'git-gutter-mode)
(add-hook 'nix-mode-hook 'git-gutter-mode)
(setq git-gutter:update-interval 0
      git-gutter:added-sign "▐"
      git-gutter:modified-sign "▐"
      git-gutter:deleted-sign "▐")

;; key-binding for increasing/decreasing text in buffer
(define-key global-map (kbd "C-c +") 'text-scale-increase)
(define-key global-map (kbd "C-c -") 'text-scale-decrease)

;; ## jabber, erc
(load-file "~/etc/emacs/im_irc.el")

;; ## helm, helm-swoop, ...
(load-file "~/etc/emacs/helm.el")

;; ## hydra
(add-to-list 'load-path "~/.emacs.d/elisp/hydra")
(load-file "~/etc/emacs/hydra.el")

;; use engine-mode
(require 'engine-mode)
(engine-mode t)

;; ## indent-guide
;; https://github.com/zk-phi/indent-guide
;; TODO: move to haskell??
(add-to-list 'load-path "~/.emacs.d/elisp/indent-guide")
(require 'indent-guide)

;; ## haskell-mode
;; use git-version instead of releases in the nix-store
(add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode")
(load-file "~/etc/emacs/haskell.el")

;; ## org-mode
(load-file "~/etc/emacs/org.el")

;; ## nix-mode
(require 'nix-mode)

;; ## magit
(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;; ## projectile
(require 'projectile)
(projectile-global-mode)

;; ## yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/etc/emacs/yasnippet"))
(yas-global-mode 1)

;; ## notmuch
(load-file "~/etc/emacs/notmuch.el")

;; ## TEST ## backups-mode
;; https://github.com/chadbraunduin/backups-mode
;; (add-to-list 'load-path "~/.emacs.d/elisp/backups-mode")
;; (require 'backups-mode)
;; (backups-mode-start)

;; start ansi-term (zsh) with C-x C-a
(define-key global-map (kbd "C-x C-a")
  (lambda ()
    (interactive)
    (ansi-term "zsh")))

;; ## highlight-symbol
(add-to-list 'load-path "~/.emacs.d/elisp/highlight-symbol.el")
(require 'highlight-symbol)
(define-key global-map (kbd "C-<f9>") 'highlight-symbol)
(define-key global-map (kbd "<f9>") 'highlight-symbol-next)
(define-key global-map (kbd "M-<f9>") 'highlight-symbol-prev)

;; short names for major modes
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "EL")))
(add-hook 'haskell-mode-hook (lambda () (setq mode-name "HS")))
(add-hook 'haskell-interactive-mode-hook (lambda () (setq mode-name "IntHS")))
(add-hook 'notmuch-search-mode-hook (lambda () (setq mode-name "⚲")))

;; diminish
(load-file "~/etc/emacs/diminish.el")
(diminish 'yas-minor-mode "")
(diminish 'git-gutter-mode "")

;; use ace-window
;;(add-to-list 'load-path "~/.emacs.d/elisp/avy")
(add-to-list 'load-path "~/.emacs.d/elisp/ace-window")
(require 'ace-window)
(setq aw-scope 'frame)
(define-key global-map (kbd "M-o") 'ace-window)

;; TODO: does not work with emacs --daemon!
;; ;; elfeed
;; (add-to-list 'load-path "~/.emacs.d/elisp/elfeed")
;; (require 'elfeed)

;; (setq elfeed-feeds
;;       '(("http://www.functionalgeekery.com/feed/" fp podcast)
;; 	("http://planet.nixos.org/rss20.xml" nixos)
;; 	("http://planet.haskell.org/rss20.xml" haskell)
;; 	("http://www.haskellcast.com/feed.xml" haskell podcast))

;; preserves me to close emacs accidentially
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure to quit emacs? "))
      (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;; load theme
;; this should be the last 
(load-file "~/etc/emacs/theme.el")
