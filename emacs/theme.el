;; common configurations
(set-face-attribute 'erc-nick-default-face nil
		    :foreground "DarkCyan"
		    :weight 'normal)
(set-face-attribute 'gnus-group-mail-3 nil
		    :foreground "firebrick"
		    :weight 'bold)

;; defines two functions for light/dark theme
;; these defines some variables in respect to their theme

;; set some useful configurations for a dark theme
(defun odi/dark-theme ()
  (interactive)
  (set-face-attribute 'default nil
		      :background "black"
		      :foreground "white")
  (set-face-attribute 'region nil
		      :background "MidnightBlue")
  (set-face-attribute 'mode-line nil
		      :background "gray75"
		      :foreground "black")
  (set-face-attribute 'jabber-roster-user-online nil
		      :foreground "DodgerBlue"
		      :weight 'bold)
  (set-face-attribute 'diff-added nil
		      :background "DarkGreen")
  (set-face-attribute 'diff-removed nil
		      :background "DarkRed")
  (setq org-todo-keyword-faces
	'(("TODO" :foreground "firebrick" :weight bold)
	  ("NEXT" :foreground "DarkCyan" :weight bold)
	  ("DONE" :foreground "LimeGreen" :weight bold)
	  ("WAIT" :foreground "orange" :weight bold)
	  ("HOLD" :foreground "magenta" :weight bold)
	  ("CANC" :foreground "DarkGoldenrod" :weight bold))))

;; set some useful configurations for a light theme
(defun odi/light-theme ()
  (interactive)
  (set-face-attribute 'default nil
		      :background "white"
		      :foreground "black")
  (set-face-attribute 'region nil
		      :background "lightgoldenrod2")
  ;; this is also the default value
  (set-face-attribute 'mode-line nil
		      :background "gray20"
		      :foreground "gray75")
  (set-face-attribute 'jabber-roster-user-online nil
		      :foreground "blue"
		      :weight 'bold)
  (setq org-todo-keyword-faces
	'(("TODO" :foreground "firebrick" :weight bold)
	  ("NEXT" :foreground "DarkCyan" :weight bold)
	  ("DONE" :foreground "LimeGreen" :weight bold)
	  ("WAIT" :foreground "orange" :weight bold)
	  ("HOLD" :foreground "magenta" :weight bold)
	  ("CANC" :foreground "DarkGoldenrod" :weight bold))))
