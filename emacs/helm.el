;; ## helm
;; http://wikiemacs.org/wiki/How_to_write_helm_extensions
;; http://kitchingroup.cheme.cmu.edu/blog/2015/01/24/Anatomy-of-a-helm-source/
(require 'helm-config)
(setq helm-buffers-fuzzy-matching t)
(setq helm-idle-delay 0.0)
(setq helm-input-idle-delay 0.01)
(setq helm-quick-update t)
(setq helm-ff-skip-boring-files t)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c h i") 'helm-imenu)

;; ## helm-swoop
(require 'helm-swoop)
(global-set-key (kbd "C-c h s") 'helm-swoop)

;; ## helm-projectile
(require 'helm-projectile)
(global-set-key (kbd "C-c h p") 'helm-projectile)
