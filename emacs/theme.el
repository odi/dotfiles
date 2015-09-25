
;; set default face within custom-set-face because of emacs --daemon
;; has problems to set font with set-face-attribute for new frames
(custom-set-faces
 '(default ((t (:inherit nil
			 :font "DejaVu Sans Mono"
			 :height 123
			 :background "grey10"
			 :foreground "white"))))
 '(tooltip ((t (:inherit nil
 			 :font "DejaVu Sans Mono"
 			 :height 0.9
 			 :background "lightgoldenrod1"
 			 :foreground "black")))))

;; additional basic faces
(set-face-attribute 'mode-line nil
		    :background "grey20"
		    :foreground "grey90"
		    :box '(:line-width -1 :color "grey30" :style nil))
(set-face-attribute 'mode-line-inactive nil
		    :background "grey10"
		    :foreground "grey40"
		    :box '(:line-width -1 :color "grey20" :style nil))
(set-face-attribute 'fringe nil
		    :background "black")
(set-face-attribute 'cursor nil
		    :background "PaleGreen")


;; default base faces
(set-face-attribute 'link nil
		    :foreground "LightSeaGreen")
(set-face-attribute 'region nil
		    :inverse-video t)
(set-face-attribute 'hl-line nil
		    :background "grey30"
		    :weight 'bold)
(set-face-attribute 'font-lock-comment-face nil
		    :foreground "grey50")
(set-face-attribute 'font-lock-doc-face nil
		    :foreground "grey60")
(set-face-attribute 'font-lock-keyword-face nil
		    :foreground "VioletRed2")

;; mouse face
(set-face-attribute 'mouse nil
		    :foreground "white"
		    :background "tomato1")

;; Jabber faces
(set-face-attribute 'jabber-roster-user-online nil
		    :foreground "PaleGreen1"
		    :weight 'normal)
(set-face-attribute 'jabber-roster-user-away nil
		    :foreground "LightSkyBlue"
		    :slant 'normal)
(set-face-attribute 'jabber-roster-user-xa nil
		    :foreground "LightSkyBlue4"
		    :slant 'normal)
(set-face-attribute 'jabber-roster-user-chatty nil
		    :foreground "LightGoldenrod1"
		    :weight 'normal)
(set-face-attribute 'jabber-chat-prompt-local nil
		    :foreground "LightSkyBlue"
		    :weight 'normal)
(set-face-attribute 'jabber-chat-prompt-foreign nil
		    :foreground "tomato3"
		    :weight 'normal)
(set-face-attribute 'jabber-rare-time-face nil
		    :foreground "LightGoldenrod3")
(set-face-attribute 'jabber-activity-face nil
		    :foreground "PaleGreen1"
		    :weight 'normal)
(set-face-attribute 'jabber-activity-personal-face nil
		    :foreground "PaleGreen1"
		    :weight 'bold)

;; erc
(set-face-attribute 'erc-nick-default-face nil
		    :foreground "PaleGreen3")
(set-face-attribute 'erc-notice-face nil
		    :foreground "LightSkyBlue"
		    :weight 'normal)
(set-face-attribute 'erc-timestamp-face nil
		    :foreground "LightGoldenrod1"
		    :weight 'normal)
(set-face-attribute 'erc-button nil
		    :foreground "LightSeaGreen"
		    :weight 'normal)
(set-face-attribute 'erc-input-face nil
		    :foreground "LightSalmon1")
(set-face-attribute 'erc-current-nick-face nil
		    :foreground "tomato3"
		    :weight 'normal)
(set-face-attribute 'erc-error-face nil
		    :foreground "IndianRed"
		    :background nil
		    :weight 'bold)
(set-face-attribute 'erc-direct-msg-face nil
		    :foreground "PaleGreen4"
		    :slant 'italic)

;; git gutter
(set-face-foreground 'git-gutter:added "PaleGreen3")
(set-face-foreground 'git-gutter:modified "LightSkyBlue3")
(set-face-foreground 'git-gutter:deleted "tomato3")

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
(set-face-attribute 'org-level-3 nil
		    :foreground nil
		    :inherit 'outline-5)
(set-face-attribute 'org-level-4 nil
		    :foreground nil
		    :inherit 'outline-8)
(set-face-attribute 'org-level-5 nil
		    :foreground nil
		    :inherit 'outline-6)
(set-face-attribute 'org-level-6 nil
		    :foreground nil
		    :inherit 'outline-7)
(set-face-attribute 'org-level-7 nil
		    :foreground nil
		    :inherit 'outline-3)
(set-face-attribute 'org-level-8 nil
		    :foreground nil
		    :inherit 'outline-4)

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
      '("LightGoldenrod3" "PaleVioletRed3" "LightSteelBlue3" "PaleGreen3"
	"burlywood3"))

;; helm
(set-face-attribute 'helm-selection nil
		    :background "grey30"
		    :weight 'bold
		    :underline t)
(set-face-attribute 'helm-source-header nil
		    :background "DarkSlateGray3"
		    :foreground "black"
		    :height 1.1)
(set-face-attribute 'helm-buffer-directory nil
		    :foreground "DodgerBlue2"
		    :background nil
		    :weight 'bold)
(set-face-attribute 'helm-buffer-process nil
		    :foreground "PaleGreen3")
(set-face-attribute 'helm-grep-match nil
		    :foreground "grey10"
		    :background "goldenrod1")
(set-face-attribute 'helm-grep-file nil
		    :foreground "magenta3")
(set-face-attribute 'helm-grep-lineno nil
		    :foreground "ForestYellow")
(set-face-attribute 'helm-grep-finish nil
		    :foreground "ForestGreen")

;; notmuch
(set-face-attribute 'notmuch-wash-cited-text nil
		    :foreground nil
		    :inherit 'font-lock-comment-face)
(set-face-attribute 'notmuch-wash-toggle-button nil
		    :foreground "lightgoldenrod3"
		    :inherit 'font-lock-doc-face)
(set-face-attribute 'notmuch-tag-face nil
		    :foreground "goldenrod1"
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
		    :foreground "DodgerBlue")
(set-face-attribute 'notmuch-search-date nil
		    :foreground "YellowGreen")
(set-face-attribute 'notmuch-tree-match-date-face nil
		    :foreground nil
		    :inherit 'notmuch-search-date)
(set-face-attribute 'notmuch-message-summary-face nil
		    :background "grey30"
		    :weight 'normal
		    :slant 'italic
		    :height 1.0
		    :box nil)
(set-face-attribute 'notmuch-crypto-part-header nil
		    :background nil
		    :foreground "tomato1")

;; message-mode
(set-face-attribute 'message-header-subject nil
		    :foreground "PaleGreen"
		    :background nil
		    :weight 'normal)
(set-face-attribute 'message-header-name nil
		    :foreground "LightSkyBlue"
		    :underline nil)
(set-face-attribute 'message-header-to nil
		    :foreground "PaleGreen"
		    :weight 'normal)
(set-face-attribute 'message-header-cc nil
		    :foreground "PaleGreen"
		    :weight 'normal)
(set-face-attribute 'message-header-cc nil
		    :inherit 'message-header-to)
(set-face-attribute 'message-header-other nil
		    :inherit 'message-header-to)
(set-face-attribute 'message-cited-text nil
		    :foreground "grey40")
(set-face-attribute 'message-mml nil
		    :background nil
		    :foreground "plum4")

;; haskell
(set-face-attribute 'haskell-keyword-face nil
		    :foreground "VioletRed1")
(set-face-attribute 'haskell-pragma-face nil
		    :foreground "tan3")
(set-face-attribute 'haskell-warning-face nil
		    :background "grey30"
		    :underline "LightGoldenrod1")
(set-face-attribute 'haskell-error-face nil
		    :background "grey30"
		    :underline "tomato4")
(set-face-attribute 'haskell-interactive-face-compile-error nil
		    :foreground "salmon"
		    :weight 'normal)
(set-face-attribute 'haskell-interactive-face-result nil
		    :foreground "PaleGreen")
(set-face-attribute 'haskell-interactive-face-compile-warning nil
		    :foreground "LightGoldenrod")

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
;; (set-face-attribute 'diff-added nil
;; 		    :inherit nil
;; 		    :foreground "grey90"
;; 		    :background "DarkOliveGreen")
;; (set-face-attribute 'diff-refine-added nil
;; 		    :foreground "grey90"
;; 		    :background "DarkOliveGreen")
;; (set-face-attribute 'diff-removed nil
;; 		    :foreground "grey90"
;; 		    :background "IndianRed4")
;; (set-face-attribute 'diff-refine-removed nil
;; 		    :foreground "grey90"
;; 		    :background "IndianRed4")
;; (set-face-attribute 'diff-header nil
;; 		    :background "grey20"
;; 		    :foreground "SteelBlue1")
;; (set-face-attribute 'diff-function nil
;; 		    :weight 'bold)
;; (set-face-attribute 'diff-file-header nil
;; 		    :background "grey20"
;; 		    :foreground "goldenrod")
