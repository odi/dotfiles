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
(set-face-attribute 'hl-line nil
		    :background "grey20"
		    :box nil)

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
(set-face-attribute 'org-agenda-clocking nil
		    :background "salmon4")

;; ansi-colors
(setq ansi-color-names-vector
      [ "black" "firebrick1" "LimeGreen"
	"DarkGoldenrod1" "DodgerBlue" "magenta3"
	"cyan3" "gray90" ])
(setq ansi-color-map (ansi-color-make-color-map))

;; helm-swoop
(set-face-attribute 'helm-swoop-target-line-face nil
		    :foreground nil
		    :background "grey20")
(set-face-attribute 'helm-swoop-target-word-face nil
		    :foreground "grey15"
		    :background "goldenrod1")

;; highlight-symbols
(setq highlight-symbol-colors
      '("LightGoldenrod4" "VioletRed4" "RoyalBlue4" "PaleGreen4"
	"burlywood4" "CadetBlue4" "tomato4"))

;; helm
(set-face-attribute 'helm-selection nil
		    :background "grey20"
		    :underline nil)
(set-face-attribute 'helm-source-header nil
		    :background "tomato4"
		    :foreground "white"
		    :height 1.0
		    :weight 'bold
		    :font "Inconsolata-14")
(set-face-attribute 'helm-buffer-file nil
		    :foreground "GreenYellow")
(set-face-attribute 'helm-buffer-process nil
		    :foreground "SeaGreen")
(set-face-attribute 'helm-buffer-directory nil
		    :foreground "DodgerBlue2"
		    :background nil
		    :weight 'bold)
(set-face-attribute 'helm-grep-match nil
		    :foreground "goldenrod1"
		    :background nil)
(set-face-attribute 'helm-grep-file nil
		    :foreground "magenta3")
(set-face-attribute 'helm-grep-lineno nil
		    :foreground "GreenYellow")
(set-face-attribute 'helm-grep-finish nil
		    :foreground "YellowGreen")

;; notmuch
(set-face-attribute 'notmuch-wash-cited-text nil
		    :foreground "SteelBlue4")
(set-face-attribute 'notmuch-wash-toggle-button nil
		    :foreground "SteelBlue3")
(set-face-attribute 'notmuch-tag-face nil
		    :foreground "MediumOrchid"
		    :weight 'normal
		    :height 120)
(set-face-attribute 'notmuch-tree-match-tag-face nil
		    :inherit 'notmuch-tag-face)
(set-face-attribute 'notmuch-search-matching-authors nil
		    :foreground "tomato1")
(set-face-attribute 'notmuch-tree-match-author-face nil
		    :foreground nil
		    :inherit 'notmuch-search-matching-authors)
(set-face-attribute 'notmuch-search-count nil
		    :foreground "DodgerBlue")
(set-face-attribute 'notmuch-search-date nil
		    :foreground "YellowGreen")
(set-face-attribute 'notmuch-tree-match-date-face nil
		    :foreground nil
		    :inherit 'notmuch-search-date)
(set-face-attribute 'message-header-subject nil
		    :foreground "tomato1"
		    :weight 'normal)

;; haskell
(set-face-attribute 'haskell-warning-face nil
		    :background "grey15"
		    :underline '(:style line :color "yellow"))
(set-face-attribute 'haskell-error-face nil
		    :background "grey15"
		    :underline '(:style line :color "tomato1"))
