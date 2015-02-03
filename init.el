;; emacs configuration file
;; created: 2014-02-02

;; TODO:
;; - add tags -unread +send to send messages
;; - XWindow notifications
;; - emms configuration
;; - org-mode configuration
;; - magit
;; - projectile
;; - use yasnippet
;; - fix functions for changing cursor color if abbrev/yasnippet available
;; - working with dired
;; - working with tramp
;; - server-mode
;; - use fill-column-indicator? (http://www.emacswiki.org/emacs/FillColumnIndicator)
;; - using smart-mode-line?

;; ## Basic configurations
(tool-bar-mode -1)		;; disable tool-bar
(scroll-bar-mode -1)		;; disable scroll-bar
(tooltip-mode t)		;; enable tool-tips
(display-time-mode t)		;; show time/date in modeline
(prefer-coding-system 'utf-8)	;; utf-8 as coding system
(desktop-save-mode t)		;; save desktop configuration
(show-paren-mode t)             ;; show fitting braces

;; load all emacs-lisp files from given directory
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; FIXME: add some packages from elpa into load-path because of
;;        a bug in Emacs.
;; see: http://xahlee.blogspot.co.at/2012/06/emacs-24-package-system-problems.html
(add-to-list 'load-path "~/.emacs.d/elpa/use-package-20150118.722")
(add-to-list 'load-path "~/.emacs.d/elpa/bind-key-20150102.1532")
(add-to-list 'load-path "~/.emacs.d/elpa/helm-20150201.2142")
(add-to-list 'load-path "~/.emacs.d/elpa/helm-swoop-20150201.2203")

;; ## Package management
;; initialize package-management
(require 'package)
(package-initialize t)

;; connection to package-archives
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; check whether 'use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; use package from https://github.com/jwiegley/use-package
(require 'use-package)

;; disable startup screen
(setq display-startup-screen nil)

;; set frame title format
(setq frame-title-format '("" "emacs ➔ %b"))

;; yank content at point and from primary selection method
(setq mouse-yank-at-point t)
(setq x-select-enable-primary t)

;; define step for increasing or decreasing text
(setq text-scale-mode-step 1.1)

;; for lazy people like my y instead of yes and n instead of no
(defalias 'yes-or-no-p 'y-or-n-p)

;; define backup directory; instead of $HOME
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))

;; set some useful settings for email
(setq message-send-mail-function 'sendmail-send-it)
(setq mail-host-address "mail.io")
(setq user-full-name "Oliver Dunkl")
(setq user-mail-address "oliver.dunkl@gmail.com")

;; helm configuration
(use-package helm
  :ensure helm
  :init
  (progn
    (require 'helm-config)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-idle-delay 0.0)
    (setq helm-input-idle-delay 0.01)
    (setq helm-quick-update t)
    (setq helm-ff-skip-boring-files t)
    (helm-mode t))
  :bind (("M-x"     . helm-M-x) ;; default: execute-extended-command
	 ("C-x b"   . helm-buffers-list)
	 ("C-c h i" . helm-imenu)
	 ("C-c h o" . helm-occur)
	 ("M-y"     . helm-show-kill-ring)))

;; helm-Swoop
(use-package helm-swoop
  :ensure helm-swoop
  :init
  (progn
    (require 'helm-swoop))
  :bind (("C-c h s" . helm-swoop)))

;; Haskell-Mode
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook (lambda ()
				(turn-on-haskell-indentation))))

;; ace-jump-mode
(use-package ace-jump-mode
  :bind (("C-c 0" . ace-jump-mode)))

;; erc
(use-package erc
  :init
  (progn
    (require 'erc))
  :config
  (progn
    ;; don't ask for password -> ~/.authinfo
    (setq erc-prompt-for-password nil)
    (setq erc-user-full-name "Oliver Dunkl")
    ;; don't show some loggin (e.g. joining/leaving user, ...)
    (setq erc-hide-list '("JOIN" "PART" "QUIT"))
    (setq erc-autojoin-channels-alist '(("freenode.net"
					 "#nixos")))))

;; jabber
(use-package jabber
  :init
  (progn
    (require 'jabber))
  :config
  (progn
    (setq jabber-account-list
	  ;; password from ~/.authinfo
	  '(("odi@jabber.ccc.de/mobile"
	     (:network-server . "jabber.ccc.de")
	     (:connection-type . ssl))))
    (setq jabber-history-enabled t)
    (setq jabber-use-global-history nil)
    (setq jabber-history-dir "~/.emacs.d/jabber-history")
    (setq jabber-backlog-number 100)
    (setq jabber-backlog-days 60)))

;; notmuch
(use-package notmuch
  :init
  (progn
    (require 'notmuch))
  :config
  (progn
    (setq notmuch-search-oldest-first nil)
    ;; define some faces
    (setq notmuch-search-line-faces '(("unread" . (:weight 'normal))))
    (setq notmuch-tag-formats '(("unread"
				 (propertize tag 'face
					     '(:inherit font-lock-constant-face
							:weight 'normal)))
				("flagged"
				 (propertize tag 'face
					     '(:inherit font-lock-comment-face)))))
    (set-face-attribute 'notmuch-search-date nil
			:inherit font-lock-variable-name-face)
    (set-face-attribute 'notmuch-search-count nil
			:inherit font-lock-type-face)
    (set-face-attribute 'notmuch-search-matching-authors nil
			:inherit font-lock-function-name-face)
    (set-face-attribute 'notmuch-tag-face nil
			:weight 'normal
			:foreground nil
			:inherit font-lock-string-face)
    ;; define some keyboard shortcuts
    (define-key notmuch-search-mode-map "k"
      (lambda ()
	(interactive)
	(notmuch-search-tag '("+killed" "-unread"))
	(notmuch-search-next-thread)))
    (bind-key "C-c n u" '(lambda () (interactive) (notmuch-search "tag:unread"))))
  :bind (("C-c n s" . notmuch-search)))

;; connect to freenode with username, password from ~/.authinfo
(defun odi/erc-connect ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "odi"))

(setq default-cursor-color "black")
(setq yas-can-fire-cursor-color "red")

(defun odi/yas-can-fire-p (&optional field) nil)

(defun odi/change-cursor-color-when-expandable (&optional field)
  (interactive)
  (when (eq last-command 'self-insert-command)
    (if (odi/can-expand)
	yas-can-fire-cursor-color
      default-cursor-color)))

(defun odi/can-expand ()
  "Return true if right after an expandable thing."
  (or (abbrev--before-point) (odi/yas-can-fire-p)))

(define-abbrev-table 'global-abbrev-table
  '(("test" "TEST")))

;; function from jwiegley
(defun odi/mark-line (&optional arg)
  "This function marks the current line and jumps to the
first position of the line."
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" 'odi/mark-line)
