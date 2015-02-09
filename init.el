;; emacs configuration file
;; created: 2014-02-02

;; TODO:
;; - emms configuration
;; - use yasnippet
;; - fix functions for changing cursor color if abbrev/yasnippet available
;; - working with dired
;; - working with tramp
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
(server-mode t)                 ;; start emacs as server
(mouse-wheel-mode -1)           ;; disable mouse scrolling

;; load all emacs-lisp files from given directory
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; FIXME: add some packages from elpa into load-path because of
;;        a bug in Emacs.
;; see: http://xahlee.blogspot.co.at/2012/06/emacs-24-package-system-problems.html
(add-to-list 'load-path "~/.emacs.d/elpa/use-package-20150118.722")
(add-to-list 'load-path "~/.emacs.d/elpa/bind-key-20150102.1532")
(add-to-list 'load-path "~/.emacs.d/elpa/helm-20150203.36")
(add-to-list 'load-path "~/.emacs.d/elpa/helm-swoop-20150201.2203")
(add-to-list 'load-path "~/.emacs.d/elpa/haskell-mode-20150202.632")
(add-to-list 'load-path "~/.emacs.d/elpa/jabber-20150127.745")
(add-to-list 'load-path "~/.emacs.d/elpa/bbdb-20140830.2031")
(add-to-list 'load-path "~/.emacs.d/elpa/projectile-20150201.1134")
(add-to-list 'load-path "~/.emacs.d/elpa/helm-projectile-20150204.107")
(add-to-list 'load-path "~/.emacs.d/elpa/dash-20141220.1452")
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20150201.150")
(add-to-list 'load-path "~/.emacs.d/elpa/popup-20150116.1223")
(add-to-list 'load-path "~/.emacs.d/elpa/paradox-20150208.1211")

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

;; load secrets file
(load "~/.emacs.d/elisp/secrets.el" t)

;; ## backup and autosave
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))

;; ## email
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq mail-host-address "mail.io")
(setq user-full-name "Oliver Dunkl")
(setq user-mail-address "oliver.dunkl@gmail.com")

;; ## default browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "google-chrome-stable")

;; ## helm
;; howto write a helm-extension: http://wikemacs.org/wiki/How_to_write_helm_extensions
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
    (helm-mode t)
    (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido)))
  :bind (("M-x"     . helm-M-x) ;; default: execute-extended-command
	 ("C-x b"   . helm-buffers-list)
	 ("C-c h i" . helm-imenu)
	 ("C-c h o" . helm-occur)
	 ("M-y"     . helm-show-kill-ring)))

;; ## helm-swoop
;; https://github.com/ShingoFukuyama/helm-swoop
(use-package helm-swoop
  :ensure helm-swoop
  :init
  (progn
    (require 'helm-swoop))
  :bind (("C-c h s" . helm-swoop)))

;; ## haskell-mode
;; https://github.com/haskell/haskell-mode
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan))

;; ## ace-jump-mode
(use-package ace-jump-mode
  :bind (("C-c 0" . ace-jump-mode)))

;; ## erc
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

;; ## jabber
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
    (setq jabber-backlog-days 60)
    (setq jabber-alert-message-hooks '(jabber-message-echo
				       jabber-message-scroll
				       odi/xmonad-notify))))

;; ## notmuch
(use-package notmuch
  :init
  (progn
    (require 'notmuch))
  :config
  (progn
    (setq notmuch-search-oldest-first nil)
    (setq message-kill-buffer-on-exit t)
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
    ;; jump to next link
    (define-key notmuch-show-mode-map (kbd "C-c C-l") 'org-next-link)
    ;; open link at point in default-browser
    (define-key notmuch-show-mode-map (kbd "C-c C-o") 'browse-url-at-point)
    (bind-key "C-c n u" '(lambda () (interactive) (notmuch-search "tag:unread"))))
  :bind (("C-c n s" . notmuch-search)))

;; ## magit
(use-package magit
  :ensure magit
  :bind (("C-c m" . magit-status)))

;; ## bbdb
(use-package bbdb
  :ensure bbdb
  :init
  (progn
    (bbdb-initialize))
  :config
  (progn
    (setq bbdb-complete-mail-allow-cycling t)
    (setq bbdb-pop-up-window-size 10)
    (setq bbdb-pop-up-layout nil)
    (setq bbdb-phone-style nil)))

;; ## switch-window
(use-package switch-window
  :ensure switch-window
  :bind (("C-," . switch-window)))

;; ## projectile
(use-package projectile
  :ensure projectile
  :config
  (progn
    (projectile-global-mode t)
    (setq projectile-indexing-method 'alien)))

;; ## helm-projectile
(use-package helm-projectile
  :ensure helm-projectile)

;; ## auto-complete
(use-package auto-complete
  :ensure auto-complete
  :idle (ac-config-default)
  :config
  (progn
    (setq ac-modes nil) ;; delete default values and add only used modes
    (add-to-list 'ac-modes 'emacs-lisp-mode)
    (add-to-list 'ac-modes 'lisp-mode)
    (add-to-list 'ac-modes 'lisp-interaction-mode)))

;; ## org-mode
(setq org-directory "~/wiki")
(setq calendar-week-start-day 1)
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; ## org-mode key bindings
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)

;; ## org-mode agenda
(setq org-agenda-files
      '("~/wiki/org/work/TSA.org"      ;; primary employer
	;; TODO move Links to org/Links.org
	"~/wiki/Links.org"             ;; collection of links
	"~/wiki/org/Anniversaries.org"
	"~/wiki/org/Diary.org"))
(setq org-agenda-custom-commands
      '(("L" . "Links")
	("Ll" "Links for reading"
	 ((todo "TODO"
		((org-agenda-files '("~/wiki/Links.org"))
		 (org-agenda-overriding-header "Links for reading")))))))

;; ## org-mode capture
(setq org-capture-templates
      '(("c" "Calendar entries") ;; all calendar entries
	("ca" "Calendar entry in `Private'"
	 entry (file+headline "~/wiki/org/Diary.org" "Private")
	 "** %?\n")
	("cb" "Calendar entry in `Work'"
	 entry (file+headline "~/wiki/org/Diary.org" "Work")
	 "** %?\n")
	("cc" "Calendar entry in `Sandra'"
	 entry (file+headline "~/wiki/org/Diary.org" "Sandra")
	 "** %?\n")
	("cd" "Calendar entry in `Family'"
	 entry (file+headline "~/wiki/org/Diary.org" "Family")
	 "** %?\n")
	("ce" "Calendar entry in `Scouts'"
	 entry (file+headline "~/wiki/org/Diary.org" "Scouts")
	 "** %?\n")
	("n" "Quicknote"
	 ;; TODO: move Notes.org to org/Notes.org
	 entry (file "~/wiki/notes/Notes.org")
	 "** TODO %?\n  :PROPERTIES:\n :CREATED: %^U\n  :END:")))

;; ## paradox
;; https://github.com/Bruce-Connor/paradox/
(use-package paradox
  :ensure paradox)

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

;; define a function which set's the urgent flag
;; from: http://www.emacswiki.org/emacs/JabberEl
(defun odi/x-urgency-hint (frame arg &optional source)
  (let* ((wm-hints (append (x-window-property
			    "WM_HINTS" frame "WM_HINTS" source nil t) nil))
	 (flags (car wm-hints)))
    (setcar wm-hints
	    (if arg
		(logior flags #x100)
	      (logand flags (lognot #x100))))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

;; wrapper function for jabber-notification
(defun odi/xmonad-notify (&optional from buffer text proposed-alert)
  (odi/x-urgency-hint (selected-frame) t))
