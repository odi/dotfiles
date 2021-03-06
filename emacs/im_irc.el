;; define a function which set's the urgent flag
;; from: http://www.emacswiki.org/emacs/JabberEl
(defun odi/x-urgendcy-hint (frame arg &optional source)
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
  (odi/x-urgendcy-hint (selected-frame) t))

(defun odi/twmnc-notify (from buffer text proposed-alert)
  (if (< (length text) 15)
      (with-temp-buffer
	(shell-command (format "twmnc -t \"Jabber\" -c \"%s: %s\"" proposed-alert text)))
    (with-temp-buffer
      (shell-command (format "twmnc -t \"Jabber\" -c \"%s: %s\"" proposed-alert (concat (substring text 0 15) " …"))))))

;; ## Jabber
;; http://emacs-jabber.sourceforge.net/
(require 'jabber)
(setq jabber-history-enabled t)
(setq jabber-use-global-history nil)
(setq jabber-history-dir (concat "~/log/jabber-history-" system-name))
(setq jabber-backlog-number 100)
(setq jabber-backlog-days 60)
(setq jabber-vcard-avatars-retrieve nil)  ;; disable avatars
(setq jabber-vcard-avatars-publish nil)   ;; dont publish my avatar vcard photo
(setq jabber-chat-buffer-show-avatar nil) ;; disable avatars in chat buffer
(setq jabber-alert-message-hooks
      '(jabber-message-echo    ;; put notification message to echo area
	jabber-message-scroll  ;; ?
	odi/twmnc-notify
	odi/xmonad-notify))    ;; set urgendcy-flag of window-manager

;; disable presence notification about users
;; it is not nice if i have a lot of users in my roster
(setq jabber-alert-presence-hooks nil)

;; sent messages to `io` if all of them are available
(cond
 ((string= system-name "rise-io")
  (setq jabber-default-priority "10"))
 ((string= system-name "io")
  (setq jabber-default-priority "20")))

;; ## Erc
(require 'erc)
(setq erc-prompt-for-password nil)
(setq erc-user-full-name "Oliver Dunkl")
;; don't show some logging (e.g. joining/leaving user, ...)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#nixos")
	("freenode.net" "#haskell")))


;; add notifications to erc-modules
(add-to-list 'erc-modules 'notifications)
(erc-update-modules)

(defun erc-notifications-notify (nick msg)
  (odi/x-urgendcy-hint (selected-frame) t)
  (if (< (length msg) 15)
      (with-temp-buffer
	(shell-command (format "twmnc -t \"IRC (%s)\" -c \"%s\"" nick msg)))
    (with-temp-buffer
      (shell-command (format "twmnc -t \"IRC (%s)\" -c \"%s\"" nick (concat (substring msg 0 15) " …"))))))

;; connect to freenode with username and password from ~/.authinfo
(defun odi/erc-connect ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "odi"))

(defun odi/erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer."
  (interactive)
  (cond
   ((get-buffer "irc.freenode.net:6667")
    (erc-track-switch-buffer 1))
   (t
    (odi/erc-connect))))
