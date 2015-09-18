
;; set default face within custom-set-face because of emacs --daemon
;; has problems to set font with set-face-attribute for new frames
(custom-set-faces
 '(default ((t (:inherit nil
			 :font "DejaVu Sans Mono"
			 :height 123
			 :background "grey13"
			 :foreground "white"))))
 '(tooltip ((t (:inherit nil
 			 :font "DejaVu Sans Mono"
 			 :height 0.8
 			 :background "grey20"
 			 :foreground "white")))))

(set-face-attribute 'mode-line nil
		    :background "grey20"
		    :foreground "LightCyan1"
		    :box "grey30")
(set-face-attribute 'mode-line-inactive nil
		    :background "grey10"
		    :foreground "grey40"
		    :box "grey20")
(set-face-attribute 'fringe nil
		    :background "grey7")

;; default base faces
(set-face-attribute 'link nil
		    :foreground "DodgerBlue")
(set-face-attribute 'region nil
		    :inverse-video t)
(set-face-attribute 'hl-line nil
		    :background "grey30"
		    :weight 'bold)
(set-face-attribute 'font-lock-comment-face nil
		    :foreground "grey50")
(set-face-attribute 'font-lock-doc-face nil
		    :foreground "grey60")
(set-face-attribute 'font-lock-string-face nil
		    :foreground "LightSalmon1")
(set-face-attribute 'font-lock-keyword-face nil
		    :foreground "VioletRed1")

;; mouse face
(set-face-attribute 'mouse nil
		    :foreground "white"
		    :background "black")

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
		    :foreground "ForestGreen"
		    :weight 'normal)
(set-face-attribute 'jabber-activity-personal-face nil
		    :foreground "ForestGreen"
		    :weight 'bold)

;; git gutter
(set-face-foreground 'git-gutter:added "ForestGreen")
(set-face-foreground 'git-gutter:modified "DodgerBlue4")
(set-face-foreground 'git-gutter:deleted "firebrick4")

;; org-mode
(setq org-todo-keyword-faces
      '(("TODO" :foreground "tomato1" :weight bold)
	("NEXT" :foreground "DarkCyan" :weight bold)
	("DONE" :foreground "PaleGreen" :weight bold)
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
      [ "black" "firebrick1" "YellowGreen"
	"DarkGoldenrod1" "DodgerBlue" "magenta3"
	"cyan3" "gray90" ])
(setq ansi-color-map (ansi-color-make-color-map))

;; helm-swoop
(set-face-attribute 'helm-swoop-target-line-face nil
		    :foreground "white"
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
		    :background "grey30"
		    :weight 'bold
		    :underline t)
(set-face-attribute 'helm-source-header nil
		    :background "DarkSlateGray3"
		    :foreground "grey10"
		    :height 1.0)
(set-face-attribute 'helm-buffer-directory nil
		    :foreground "DodgerBlue2"
		    :background nil
		    :weight 'bold)
(set-face-attribute 'helm-grep-match nil
		    :foreground "grey10"
		    :background "goldenrod1")
(set-face-attribute 'helm-grep-file nil
		    :foreground "magenta3")
(set-face-attribute 'helm-grep-lineno nil
		    :foreground "ForestYellow")
(set-face-attribute 'helm-grep-finish nil
		    :foreground "ForestGreen")
;;(set-face-attribute 'helm-M-x-key nil
;;		    :foreground "magenta")

;; notmuch
(set-face-attribute 'notmuch-wash-cited-text nil
		    :foreground nil
		    :inherit 'font-lock-comment-face)
(set-face-attribute 'notmuch-wash-toggle-button nil
		    :foreground nil
		    :inherit 'font-lock-doc-face)
(set-face-attribute 'notmuch-tag-face nil
		    :foreground "DodgerBlue"
		    :weight 'normal
		    :height 120)
(set-face-attribute 'notmuch-tree-match-tag-face nil
		    :foreground nil
		    :inherit 'notmuch-tag-face)
(set-face-attribute 'notmuch-search-matching-authors nil
		    :foreground "tomato1")
(set-face-attribute 'notmuch-tree-match-author-face nil
		    :foreground nil
		    :inherit 'notmuch-search-matching-authors)
(set-face-attribute 'notmuch-search-count nil
		    :foreground "goldenrod1")
(set-face-attribute 'notmuch-search-date nil
		    :foreground "YellowGreen")
(set-face-attribute 'notmuch-tree-match-date-face nil
		    :foreground nil
		    :inherit 'notmuch-search-date)
(set-face-attribute 'notmuch-message-summary-face nil
		    :background "DarkSlateGrey"
		    :weight 'normal
		    :box nil)
(set-face-attribute 'notmuch-crypto-part-header nil
		    :background nil
		    :foreground "tomato1")

;; message-mode
(set-face-attribute 'message-header-subject nil
		    :foreground "goldenrod1"
		    :weight 'normal)
(set-face-attribute 'message-header-name nil
		    :foreground "YellowGreen")
(set-face-attribute 'message-header-to nil
		    :foreground "DodgerBlue"
		    :weight 'normal)
(set-face-attribute 'message-header-cc nil
		    :foreground "DodgerBlue"
		    :weight 'normal)
(set-face-attribute 'message-header-cc nil
		    :inherit 'message-header-to)
(set-face-attribute 'message-cited-text nil
		    :foreground "grey40")
(set-face-attribute 'message-mml nil
		    :background nil
		    :foreground "PaleGreen3")

;; haskell
(set-face-attribute 'haskell-warning-face nil
		    :background "grey15"
		    :underline '(:style line :color "yellow"))
(set-face-attribute 'haskell-error-face nil
		    :background "grey15"
		    :underline '(:style line :color "tomato1"))
(set-face-attribute 'haskell-interactive-face-compile-warning nil
		    :foreground "goldenrod1"
		    :weight 'normal)
(set-face-attribute 'haskell-definition-face nil
		    :foreground "DodgerBlue1"
		    :weight 'normal)
(set-face-attribute 'haskell-keyword-face nil
		    :foreground "VioletRed1")
(set-face-attribute 'haskell-operator-face nil
		    :foreground "Goldenrod1")
(set-face-attribute 'haskell-pragma-face nil
		    :foreground "tan3")

;; hydra
(set-face-attribute 'hydra-face-blue nil
		    :foreground "DodgerBlue3")
(set-face-attribute 'hydra-face-red nil
		    :foreground "tomato1")

(set-face-attribute 'aw-leading-char-face nil
		    :background nil
		    :foreground "Goldenrod1"
		    :weight 'bold
		    :height 1.2)

;; diff
(set-face-attribute 'diff-added nil
		    :inherit nil
		    :foreground "grey90"
		    :background "DarkOliveGreen")
(set-face-attribute 'diff-refine-added nil
		    :foreground "grey90"
		    :background "DarkOliveGreen")
(set-face-attribute 'diff-removed nil
		    :foreground "grey90"
		    :background "IndianRed4")
(set-face-attribute 'diff-refine-removed nil
		    :foreground "grey90"
		    :background "IndianRed4")
(set-face-attribute 'diff-header nil
		    :background "grey20"
		    :foreground "SteelBlue1")
(set-face-attribute 'diff-function nil
		    :weight 'bold)
(set-face-attribute 'diff-file-header nil
		    :background "grey20"
		    :foreground "goldenrod")
