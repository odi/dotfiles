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
;; - flyspell for emails?
;; - configure org-mobile

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
(winner-mode t)                 ;; activate winner-mode
(eldoc-mode t)                  ;; activate emacs-lisp-documentation

;; load all emacs-lisp files from given directory
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; FIXME: add some packages from elpa into load-path because of
;;        a bug in Emacs.
;; see: http://xahlee.blogspot.co.at/2012/06/emacs-24-package-system-problems.html
(add-to-list 'load-path "~/.emacs.d/elpa/use-package-20150118.722")
(add-to-list 'load-path "~/.emacs.d/elpa/bind-key-20150102.1532")
(add-to-list 'load-path "~/.emacs.d/elpa/helm-20150224.852")
(add-to-list 'load-path "~/.emacs.d/elpa/helm-swoop-20150209.806")
(add-to-list 'load-path "~/.emacs.d/elpa/haskell-mode-20150222.908")
(add-to-list 'load-path "~/.emacs.d/elpa/jabber-20150211.1330")
;;(add-to-list 'load-path "~/.emacs.d/elpa/bbdb-20140830.2031")
(add-to-list 'load-path "~/.emacs.d/elpa/projectile-20150223.8")
(add-to-list 'load-path "~/.emacs.d/elpa/helm-projectile-20150222.312")
(add-to-list 'load-path "~/.emacs.d/elpa/dash-20141220.1452")
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20150218.819")
(add-to-list 'load-path "~/.emacs.d/elpa/popup-20150116.1223")
(add-to-list 'load-path "~/.emacs.d/elpa/paradox-20150214.1342")
(add-to-list 'load-path "~/.emacs.d/elpa/switch-window-20150114.215")
(add-to-list 'load-path "~/.emacs.d/elpa/key-chord-20140929.2246")
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20150212.240")
(add-to-list 'load-path "~/.emacs.d/elpa/hydra-20150224.1022")

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

;;(set-face-attribute 'org-mode-line-clock nil :inherit 'secondary-selection)

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
;; It uses a shell script which loads a new firefox-window with
;; the url under the point.
;; `firefox-nw.sh' just opens an url in a new window.
(setq browse-url-browser-function 'browse-url-default-browser)
(setq browse-url-generic-program "conkeror")

;; ## helm
;; http://wikemacs.org/wiki/How_to_write_helm_extensions
;; http://kitchingroup.cheme.cmu.edu/blog/2015/01/24/Anatomy-of-a-helm-source/
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

;; ## hydra
(use-package hydra
  :ensure hydra)

;; ## notmuch
(use-package notmuch
  :init
  (progn
    (require 'notmuch))
  :config
  (progn
    (setq notmuch-search-oldest-first nil)
    (setq message-kill-buffer-on-exit t)

    ;; add tag `muted` to messages in search-mode
    (define-key notmuch-search-mode-map "k"
      (lambda (&optional beg end)
	(interactive (notmuch-search-interactive-region))
	(notmuch-search-tag '("+mute" "-unread") beg end)
	(notmuch-search-next-thread)))

    ;; add tag `muted` to messages in show-mode
    (define-key notmuch-show-mode-map "k"
      (lambda () (interactive) (notmuch-show-tag '("+mute" "-unread"))))

    ;; add tag `archived` to message in search-mode
    (define-key notmuch-search-mode-map "a"
      (lambda (&optional beg end)
	(interactive (notmuch-search-interactive-region))
	(notmuch-search-tag '("+archive" "-unread") beg end)
	(notmuch-search-next-thread)))

    ;; add tag `archived` to message in show-mode
    (define-key notmuch-show-mode-map "a"
      (lambda () (interactive) (notmuch-show-tag '("+archive" "-unread"))))

    ;; add tag `deleted` to message for deleting in search-mode
    (define-key notmuch-search-mode-map "d"
      (lambda (&optional beg end)
	(interactive (notmuch-search-interactive-region))
	(notmuch-search-tag '("+delete" "-unread") beg end)
	(notmuch-search-next-thread)))

    ;; add tag `deleted` to message for deleting in show-mode
    (define-key notmuch-show-mode-map "d"
      (lambda () (interactive) (notmuch-show-tag '("+delete" "-unread"))))

    (define-key notmuch-show-mode-map "E"
      (lambda ()
	(interactive)
	(odi/notmuch-show-edit-message (notmuch-show-get-message-id))))

    ;; jump to next link
    (define-key notmuch-show-mode-map (kbd "C-c C-l") 'org-next-link)
    ;; open link at point in default-browser
    (define-key notmuch-show-mode-map (kbd "C-c C-o") 'browse-url-at-point)
    (bind-key "C-c n"
    	      (defhydra hydra-notmuch (:color blue)
    		"notmuch"
    		("d" (lambda () ;; to delete messages
    		       (interactive)
    		       (notmuch-search "date:..3month tag:killed tag:lists tag:spam tag:mute"))
    		 "to delete messages")
    		("o" (lambda () ;; old messages
    		       (interactive)
    		       (notmuch-search "date:..6month"))
    		 "old messages")
		("r" (lambda () ;; review messages
		       (interactive)
		       (notmuch-search "tag:review"))
		 "review messages")
    		("s" notmuch-search "search messages")
    		("S" (lambda () ;; spam messages
    		       (interactive)
    		       (notmuch-search "tag:spam and tag:killed tag:delete"))
    		 "spam messages")
    		("t" (lambda () ;; todo messages
    		       (interactive)
    		       (notmuch-search "tag:todo"))
    		 "todo messages")
    		("u" (lambda () ;; unread messages
    		       (interactive)
    		       (notmuch-search "tag:unread"))
    		 "unread messages")))))

(defvar odi/notmuch-tmp-tags-file "/tmp/notmuch-tags") ;; tmp-file for temporary stored tags
(defvar odi/notmuch-tmp-edit-file "/tmp/notmuch-edit") ;; tmp-file for temporary stored message

;; ;; INFO: works only right if synchronize_flags=false in ~/.notmuch-config!
(defun odi/notmuch-show-edit-message (message-id)
  (odi/notmuch-dump-tags message-id odi/notmuch-tmp-tags-file)
  (find-file (notmuch-show-get-filename))
  (message-mode)
  (local-set-key (kbd "C-c C-e")
		 '(lambda ()
		    (interactive)
		    (let ((orig-file (buffer-file-name)))
		      (save-buffer)
		      (write-file odi/notmuch-tmp-edit-file)
		      (kill-buffer)
		      (delete-file orig-file)
		      (notmuch-poll)
		      (copy-file odi/notmuch-tmp-edit-file orig-file)
		      (notmuch-poll)
		      (odi/notmuch-restore-tags odi/notmuch-tmp-tags-file)
		      (delete-file odi/notmuch-tmp-edit-file)
		      (delete-file odi/notmuch-tmp-tags-file)
		      (notmuch-bury-or-kill-this-buffer)))))

(defun odi/notmuch-dump-tags (message-id file)
  "Dump tags from message with MESSAGE-ID to FILE."
  (shell-command (concat "notmuch dump " (concat "--output=" file) " " message-id)))

(defun odi/notmuch-restore-tags (file)
  "Restore all tags from FILE and delete file after executing the program."
  (shell-command
   (concat "notmuch " "tag "
	   (with-temp-buffer (insert-file-contents file) (buffer-string)))))

;; ## magit
(use-package magit
  :ensure magit
  :bind (("C-c m" . magit-status)))

;; ## bbdb
;; (use-package bbdb
;;   :ensure bbdb
;;   :init
;;   (progn
;;     (bbdb-initialize))
;;   :config
;;   (progn
;;     (setq bbdb-complete-mail-allow-cycling t)
;;     (setq bbdb-pop-up-window-size 10)
;;     (setq bbdb-pop-up-layout nil)
;;     (setq bbdb-phone-style nil)))

;; ## switch-window
;; - deactivated C-, because of conflicting with org-mode
;;   and using key-chord 'ow'
(use-package switch-window
  :ensure switch-window)

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
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	(sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANC(c@)")))

;; set auto-fill for org-mode buffers
(add-hook 'org-mode-hook 'auto-fill-mode)

;; number of shown days in default agenda view
(setq org-agenda-span 'day)

;; add a :LOGBOOK: drawer for clocking and logging
(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)

;; ## org-contacts
(require 'org-contacts)
(setq org-contacts-files '("~/wiki/Contacts.org"))
(setq org-contacts-icon-use-gravatar nil)
(setq org-contacts-birthday-format "%l (%y)")
(setq org-link-abbrev-alist
      '(("contact" . "~/wiki/Contacts.org::/\*.*%s/")))

(setq org-show-entry-below t)
(setq org-show-siblings t)
(setq org-show-hierarchy-above t)
(setq org-show-following-heading t)

;; ## org-mode key bindings
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c o" (lambda ()
		    (interactive)
		    (find-file "~/wiki/org/Notes.org")))
(bind-key "C-c C-w" 'org-refile) ;; C-u C-c C-w to jump to heading

;; define my project files
(setq org-project-files
      '("~/wiki/org/work/TSA.org" ;; TODO: move it to projects
	"~/wiki/org/projects/Scouts.org"
	"~/wiki/org/projects/OrgWorkflow.org"))

;; define my agenda-files
(setq org-agenda-files
      (append '("~/wiki/org/Anniversaries.org"
		"~/wiki/org/Diary.org"
		"~/wiki/org/Notes.org"
		) org-project-files))  ;; all projects to my agenda

;; ## org-refile
(setq org-refile-targets '((nil :maxlevel . 6)
			   (org-agenda-files :maxlevel . 6)))
(setq org-refile-use-outline-path t)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-target-verify-function
      '(lambda ()
	 (not (member (nth 2 (org-heading-components)) org-done-keywords))))
(setq org-outline-path-complete-in-steps nil)

;; ## org-mode agenda
(setq org-agenda-compact-blocks t)

;; define stuck projects
(setq org-tags-exclude-from-inheritance '("project" "recipe"))
(setq org-stuck-projects
      '("+project/-DONE-CANC" ("NEXT") nil ""))

(setq org-agenda-custom-commands
      '(("@" . "Links")
	("@l" "Links for reading"
	 ((todo "TODO"
		((org-agenda-files '("~/wiki/Links.org"))
		 (org-agenda-overriding-header "Links for reading")))))
	("H" "Habits" tags-todo "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
	("N" . "Notes")
	("Nr" "notes to refile"
	 ((todo "TODO"
		((org-agenda-files '("~/wiki/org/Notes.org"))
		 (org-agenda-overriding-header "Notes")))))
	("Nn" "next tasks"
	 ((todo "NEXT"
		((org-agenda-overriding-header "Next Tasks:")))))
	("Ns" "stuck projects"
	 ((stuck ""
		 ((org-agenda-files org-project-files)
		  (org-agenda-overriding-header "List of stuck projects:")))))
	("Nw" "waiting tasks"
	 ((todo "WAIT"
		((org-agenda-overriding-header "Waiting Tasks:")))))
	("R" . "Recipes")
	("Rl" "list all Recipes"
	 ((todo "COCKED"
		((org-agenda-files '("~/wiki/Recipes.org"))
		 (org-agenda-sorting-strategy '(tsia-up))
;;		 (org-agenda-prefix-format " %s %T")
		 (org-agenda-overriding-header "List of all Recipes")))))
	("O" "Agenda verbose overview"
	 ((agenda "" nil)
	  (todo "TODO"
		((org-agenda-files '("~/wiki/org/Notes.org"))
		 (org-agenda-overriding-header "Tasks to Refile:")))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks:")))
	  (stuck ""
		 ((org-agenda-files org-project-files) ;; look only in my projects
		  (org-agenda-overriding-header "List of stuck projects:")))
	  (tags-todo "+project"
		     ((org-agenda-files org-project-files)
		      (org-agenda-overriding-header "List of all projects:")))
	  (todo "WAIT|HOLD"
		((org-agenda-overriding-header "Waiting and Postponed Tasks:")))))))

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
	("a" "Action"
	 entry (file+headline "~/wiki/org/Diary.org" "Actions")
	 "** %?\n")
	("q" "Quickentry"
	 entry (file "~/wiki/org/Notes.org")
	 "* %?\n")
	("n" "Notes"
	 entry (file "~/wiki/org/Notes.org")
	 "* TODO %?\n :PROPERTIES:\n :CREATED: %^U\n :END:")))

;; ## paradox
;; https://github.com/Bruce-Connor/paradox/
(use-package paradox
  :ensure paradox)

;; ## key-chord
;; TODO: test key-chords if they conflicts with my typings
(use-package key-chord
  :ensure key-chord
  :config
  (progn
    (key-chord-mode t)
    (key-chord-define-global "ww" 'other-window)))

;; ## yasnippet
(use-package yasnippet
  :ensure yasnippet
  :config
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/yasnippet"))
    ;; https://github.com/capitaomorte/yasnippet/issues/362
    (setq yas-indent-line 'fixed)
    (add-hook 'org-mode-hook '(lambda () (yas-minor-mode)))))

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

;; load theme
(load "~/.emacs.d/elisp/theme.el" t)
