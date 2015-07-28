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

;; prefered coding system
(prefer-coding-system 'utf-8)  ;; use UTF-8 as my prefered coding system

;; set title format of the frames
(setq frame-title-format '("" "emacs :: %b"))

;; do host specific configurations
;; currently i use following hosts: rise-io, io
(cond
 ((string= system-name "rise-io")
  ;; set default font
  (set-face-attribute 'default nil
		      :font "DejaVu Sans Mono 13"))
 ((string= system-name "io")
  ;; set default font
  (set-face-attribute 'default nil
		      :font "DejaVu Sans Mono 15")))

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
(setq browse-url-browser-function 'browse-url-chromium
      browse-url-new-window-flag t)

;; load all my configurations which i'll not show to the public
(load-file "~/etc/emacs/secure.el")

;; email configurations
(require 'gnus)
(setq message-send-mail-function 'message-send-mail-with-sendmail)

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

;; ## jabber, erc
(load-file "~/etc/emacs/im_irc.el")

;; ## helm, helm-swoop, ...
(load-file "~/etc/emacs/helm.el")

;; ## haskell-mode
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
;; TODO: remove this if yasnippet is in nixpkgs
(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/etc/emacs/yasnippet"))
(yas-global-mode 1)

;; ## backups-mode
;; https://github.com/chadbraunduin/backups-mode
(add-to-list 'load-path "~/.emacs.d/elisp/backups-mode")
(require 'backups-mode)
(backups-mode-start)

;; ## highlight-symbol
(add-to-list 'load-path "~/.emacs.d/elisp/highlight-symbol.el")
(require 'highlight-symbol)

;; load theme
;; this should be the last 
(load-file "~/etc/emacs/theme.el")
