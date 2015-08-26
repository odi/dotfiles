;; default face - dark theme
(set-face-attribute 'default nil
		    :background "#111"
		    :foreground "#fff")

(set-face-attribute 'mode-line nil
		    :background "grey15"
		    :foreground "white"
		    :box "grey50")
(set-face-attribute 'mode-line-inactive nil
		    :background "grey10"
		    :box "grey10")

;; default base faces
(set-face-attribute 'link nil
		    :foreground "DodgerBlue")
(set-face-attribute 'region nil
		    :background "DodgerBlue4")

;; Jabber faces
(set-face-attribute 'jabber-roster-user-online nil
		    :foreground "GreenYellow"
		    :weight 'normal)
(set-face-attribute 'jabber-roster-user-away nil
		    :foreground "DodgerBlue3"
		    :slant 'normal)
(set-face-attribute 'jabber-roster-user-xa nil
		    :foreground "DodgerBlue4"
		    :slant 'normal)
(set-face-attribute 'jabber-roster-user-chatty nil
		    :foreground "orange2"
		    :weight 'normal)
(set-face-attribute 'jabber-chat-prompt-local nil
		    :foreground "DodgerBlue2"
		    :weight 'normal)
(set-face-attribute 'jabber-chat-prompt-foreign nil
		    :foreground "#dc322f"
		    :weight 'normal)
(set-face-attribute 'jabber-rare-time-face nil
		    :foreground "DarkGoldenrod1")
(set-face-attribute 'jabber-activity-face nil
		    :foreground "YellowGreen"
		    :weight 'normal)
(set-face-attribute 'jabber-activity-personal-face nil
		    :foreground "YellowGreen"
		    :weight 'bold)

;; git gutter
(set-face-foreground 'git-gutter:added "ForestGreen")
(set-face-foreground 'git-gutter:modified "DodgerBlue4")
(set-face-foreground 'git-gutter:deleted "firebrick4")

;; org-mode
(setq org-todo-keyword-faces
      '(("TODO" :foreground "firebrick" :weight bold)
	("NEXT" :foreground "DarkCyan" :weight bold)
	("DONE" :foreground "LimeGreen" :weight bold)
	("WAIT" :foreground "orange" :weight bold)
	("HOLD" :foreground "magenta" :weight bold)
	("CANC" :foreground "DarkGoldenrod" :weight bold)))
(set-face-attribute 'org-date nil
		    :foreground "DarkGoldenrod1")
(set-face-attribute 'org-special-keyword nil
		    :foreground "MediumOrchid3")

;; ansi-colors
(setq ansi-color-names-vector
      [ "black" "firebrick1" "LimeGreen"
	"DarkGoldenrod1" "DodgerBlue" "magenta3"
	"cyan3" "gray90" ])
(setq ansi-color-map (ansi-color-make-color-map))

;; ;; common configurations
;; (set-face-attribute 'erc-nick-default-face nil
;; 		    :foreground solarized-magenta
;; 		    :weight 'normal)
;; (set-face-attribute 'erc-timestamp-face nil
;; 		    :foreground cgreen
;; 		    :weight 'normal)
;; (set-face-attribute 'erc-notice-face nil
;; 		    :foreground solarized-blue
;; 		    :weight 'normal)
;; (set-face-attribute 'erc-input-face nil
;; 		    :foreground corange)
;; (set-face-attribute 'erc-current-nick-face nil
;; 		    :foreground solarized-cyan)
;; ;; (set-face-attribute 'gnus-group-mail-3 nil
;; ;; 		    :foreground solarized-red
;; ;; 		    :weight 'bold)
;; (set-face-attribute 'org-agenda-clocking nil
;; 		    :background solarized-yellow)
;; (set-face-attribute 'org-mode-line-clock nil
;; 		    :inherit 'org-agenda-clocking)
;; (set-face-attribute 'magit-item-highlight nil
;; 		    :background cyellowlight)

;; (set-face-foreground 'indent-guide-face "LightGrey")
