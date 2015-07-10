;; defines two functions for light/dark theme
;; these defines some variables in respect to their theme

;; set some useful configurations for a dark theme
(defun odi/dark-theme ()
  (interactive)
  (set-face-attribute 'mode-line nil
		      :background "gray75"
		      :foreground "black"))

;; set some useful configurations for a light theme
(defun odi/light-theme ()
  (interactive)
  (set-face-attribute 'region nil
		      :background "lightgoldenrod2")
  ;; this is also the default value
  (set-face-attribute 'mode-line nil
		      :background "gray20"
		      :foreground "gray75"))
