(setq org-directory "~/wiki")
(setq calendar-week-start-day 1)
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	(sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANC(c@)")))

;; faces for my keywords
(setq org-todo-keyword-faces
      (quote (("NEXT" :foreground "blue" :weight bold)
              ("WAIT" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold))))

;; set auto-fill for org-mode buffers
(add-hook 'org-mode-hook 'auto-fill-mode)

;; number of shown days in default agenda view
(setq org-agenda-span 'day)

;; add a :LOGBOOK: drawer for clocking and logging
(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)

(setq org-show-entry-below t)
(setq org-show-siblings t)
(setq org-show-hierarchy-above t)
(setq org-show-following-heading t)

;; ## org-mode key bindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; my project files
(setq org-project-files
      '("~/wiki/org/projects/OrgMode.org"
	"~/wiki/org/projects/OS.org"))

;; define my agenda-files
(setq org-agenda-files
      (append '("~/wiki/org/Anniversaries.org"
		"~/wiki/org/Diary.org" ;; all my calendars
		"~/wiki/org/Notes.org" ;; inbox in GTD
		) org-project-files))

;; ## org-refile
(setq org-refile-targets '((nil :maxlevel . 6)
			   (org-agenda-files :maxlevel . 3)
			   (("~/wiki/Links.org") :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-target-verify-function nil)
(setq org-outline-path-complete-in-steps t)

;; ## org-mode agenda
(setq org-agenda-compact-blocks t)

;; define stuck projects
(setq org-tags-exclude-from-inheritance '("project" "recipe"))
(setq org-stuck-projects
      '("+project/-WAIT-HOLD-DONE-CANC" ("NEXT") nil ""))

(setq org-agenda-custom-commands
      '(("O" "Agenda verbose overview"
	 ((agenda "" nil)
	  (todo "TODO"
		((org-agenda-files '("~/wiki/org/Notes.org"))
		 (org-agenda-overriding-header "TASKS TO REFILE:")))
	  (todo "NEXT"
		((org-agenda-overriding-header "NEXT TASKS:")))
	  (stuck ""
		 ((org-agenda-files org-project-files) ;; look only in my projects
		  (org-agenda-overriding-header "STUCK PROJECTS:")))
	  (tags-todo "+project"
		     ((org-agenda-files org-project-files)
		      (org-agenda-overriding-header "ALL PROJECTS:")))
	  (todo "WAIT|HOLD"
		((org-agenda-overriding-header "WAITING AND POSTPONED TASKS:")))))
	("N" . "Notes")
	("Nr" "Notes to refile"
	 ((todo "TODO"
		((org-agenda-files '("~/wiki/org/Notes.org"))
		 (org-agenda-overriding-header "Notes to refile:")))))
	("Nn" "Next Tasks"
	  ((todo "NEXT"
		 ((org-agenda-overriding-header "Next Tasks:")))))
	("Nw" "Notes for waiting"
	 ((todo "WAIT"
		 ((org-agenda-overriding-header "Notes for waiting:")))))
	("Np" "Postponed notes"
	 ((todo "HOLD"
		((org-agenda-overriding-header "Postponed notes:")))))
	("R" . "Recipes")
	("Ra" "All Recipes"
	 ((tags "recipe"
		((org-agenda-files '("~/wiki/Recipes.org"))
		 (org-agenda-sorting-strategy '(tsia-down))
		 (org-agenda-overriding-header "All Recipes:")))))
	("L" . "Links")
	("La" "All links"
	 ((tags "link"
		((org-agenda-files '("~/wiki/Links.org"))
		 (org-agenda-overriding-header "All Links:")))))
	("Lr" "Links to read"
	 ((todo "TODO|HOLD|WAIT"
		((org-agenda-files '("~/wiki/Links.org"))
		 (org-agenda-overriding-header "Links to read:")))))
	("Ln" "News"
	 ((tags "link+look"
		((org-agenda-files '("~/wiki/Links.org"))
		 (org-agenda-overriding-header "Links with News:")))))))


;; all capture templates
(setq org-capture-templates
      '(("c" "Calendar entries")
	("ca" "Calendar entry in `Private`"
	 entry (file+headline "~/wiki/org/Diary.org" "Private")
	 "** %?\n")
	("cb" "Calendar entry in `Sandra`"
	 entry (file+headline "~/wiki/org/Diary.org" "Sandra")
	 "** %?\n")
	("cc" "Calendar entry in `Family`"
	 entry (file+headline "~/wiki/org/Diary.org" "Family")
	 "** %?\n")
	("cd" "Calendar entry in `Scouts`"
	 entry (file+headline "~/wiki/org/Diary.org" "Scouts")
	 "** %?\n")
	("ce" "Calendar entry in `Work`"
	 entry (file+headline "~/wiki/org/Diary.org" "Work")
	 "** %?\n")
	("l" "Links"
	 entry (file "~/wiki/Links.org")
	 "* TODO [[%c][%?]] :link:\n :PROPERTIES:\n :CREATED: %^U\n :END:")
	("n" "Notes"
	 entry (file "~/wiki/org/Notes.org")
	 "* TODO %?\n :PROPERTIES:\n :CREATED: %^U\n :END:")))
