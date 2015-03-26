;; ## default
(set-face-attribute 'region nil :background "lightgoldenrod2")

;; ## notmuch
;;(setq notmuch-search-line-faces '(("unread" . (:weight 'normal))))
(setq notmuch-search-line-faces
      '(("unread"  . (:weight normal))
	("delete"  . (:background "PeachPuff"))
	("archive" . (:background "AliceBlue"))
	("mute"    . (:background "LemonChiffon1"))))
(setq notmuch-tag-formats
      '(("unread"
	 (propertize tag 'face
		     '(:inherit font-lock-constant-face)))
	("archive"
	 (propertize tag 'face
		     '(:inherit font-lock-function-name-face)))
	("todo"
	 (propertize tag 'face
		     '(:inherit font-lock-comment-face)))
	("review"
	 (propertize tag 'face
		     '(:inherit font-lock-type-face)))
	("mute"
	 (propertize tag 'face
		     '(:inherit font-lock-variable-name-face)))
	("delete"
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

;; ## org-mode
(setq org-todo-keyword-faces
      (quote (("NEXT" :foreground "blue" :weight bold)
	      ("WAIT" :foreground "orange" :weight bold)
	      ("HOLD" :foreground "magenta" :weight bold))))
