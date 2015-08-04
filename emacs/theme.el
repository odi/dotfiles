;; see: http://ethanschoonover.com/solarized
(setq solarized-base03  "#002b36") ;; -> dark background 1
(setq solarized-base02  "#073642") ;; -> dark background 2
(setq solarized-base01  "#586e75") ;; -> content color 1
(setq solarized-base00  "#657b83") ;; -> content color 2
(setq solarized-base0   "#839496") ;; -> content color 3
(setq solarized-base1   "#93a1a1") ;; -> content color 4
(setq solarized-base2   "#eee8d5") ;; -> light background 1
(setq solarized-base3   "#fdf6e3") ;; -> light background 2
(setq solarized-yellow  "gold")
(setq solarized-orange  "#cb4b16")
(setq solarized-red     "#dc322f")
(setq solarized-magenta "#d33682")
(setq solarized-violet  "#6c71c4")
(setq solarized-blue    "#268bd2")
(setq solarized-cyan    "#2aa198")
(setq solarized-green   "LimeGreen");;"#859900")

;; my prefered colors
(setq cyellow  "gold"
      corange  "DarkOrange3"
      cred     "firebrick"
      cgreen   "LimeGreen")

;; common configurations
(set-face-attribute 'erc-nick-default-face nil
		    :foreground solarized-magenta
		    :weight 'normal)
(set-face-attribute 'erc-timestamp-face nil
		    :foreground cgreen
		    :weight 'normal)
(set-face-attribute 'erc-notice-face nil
		    :foreground solarized-blue
		    :weight 'normal)
(set-face-attribute 'erc-input-face nil
		    :foreground corange)
(set-face-attribute 'erc-current-nick-face nil
		    :foreground solarized-cyan)
(set-face-attribute 'gnus-group-mail-3 nil
		    :foreground solarized-red
		    :weight 'bold)
(set-face-attribute 'org-agenda-clocking nil
		    :background solarized-yellow)
(set-face-attribute 'org-mode-line-clock nil
		    :inherit 'org-agenda-clocking)

;; defines two functions for light/dark theme
;; these defines some variables in respect to their theme

;; set some useful configurations for a dark theme
(defun odi/dark-theme ()
  (interactive)
  (set-face-attribute 'default nil
		      :background solarized-base03
		      :foreground "white")
  (set-face-attribute 'region nil
		      :background solarized-base01)
  (set-face-attribute 'mode-line nil
		      :background "gray75"
		      :foreground "black")
  (set-face-attribute 'jabber-roster-user-online nil
		      :foreground "DodgerBlue"
		      :weight 'bold)
  (set-face-attribute 'jabber-chat-prompt-local nil
		      :foreground "#2aa198")
  (set-face-attribute 'jabber-chat-prompt-foreign nil
		      :foreground "#dc322f")
  (set-face-attribute 'jabber-rare-time-face nil
		      :foreground solarized-green)
  (set-face-attribute 'diff-added nil
		      :foreground  "black"
		      :background solarized-green)
  (set-face-attribute 'diff-removed nil
		      :foreground "black"
		      :background solarized-red)
  (set-face-attribute 'magit-item-highlight nil
		      :background solarized-base00)
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
