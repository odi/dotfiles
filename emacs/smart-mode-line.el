(setq sml/no-confirm-load-theme t)

(setq sml/theme 'dark)
(sml/setup)

;; size of some categories in the mode-line
(setq sml/mode-width 10)
(setq sml/name-width 15)

;; don't show this minor-modes
(setq sml/hidden-modes '("yas"))

;; different character for modification state
(setq sml/modified-char "✖")

;; change major-mode names for some modes
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "EL")))
(add-hook 'haskell-mode-hook (lambda () (setq mode-name "HS")))
(add-hook 'haskell-interactive-mode-hook (lambda () (setq mode-name "IntHS")))
(add-hook 'org-agenda-mode-hook (lambda () (setq mode-name "OrgA")))

(set-face-attribute 'sml/global nil
		    :foreground "grey70")
(set-face-attribute 'sml/discharging nil
		    :foreground "firebrick1")
(set-face-attribute 'sml/vc nil
		    :foreground "LimeGreen")
(set-face-attribute 'sml/vc-edited nil
		    :foreground "DodgerBlue")
