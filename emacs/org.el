(setq org-directory "~/wiki")
(setq calendar-week-start-day 1)
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)")
	(sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANC(c@)")))

;; faces for my keywords
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold)
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
	"~/wiki/org/projects/OS.org"
	"~/wiki/org/projects/RiseMain.org"))

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

;; configure the clockreport format
;; see: http://orgmode.org/manual/The-clock-table.html
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 2 :fileskip0 t :compact t))

;; add support for magit-links
;; https://github.com/emacsattic/org-magit
(add-to-list 'load-path "~/.emacs.d/elisp/org-magit")
(require 'org-magit)

(require 'org-protocol)

(defadvice org-capture
  (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))

;; disable current task in mode-line
(setq org-mode-line-string "")
(setq org-clock-clocked-in-display nil)

;; set key binding for splitting interval
(global-set-key (kbd "C-c s") 'org-clock-split-current-interval)

;; TODO: add time in front of the splittet timestamp
;; e.g.
;;   CLOCK: [2015-08-06 Thu 07:00]--[2015-08-06 Thu 16:00] =>  9:00
;; to
;;   CLOCK: [2015-08-06 Thu 12:00]--[2015-08-06 Thu 16:00] =>  4:00
;;   CLOCK: [2015-08-06 Thu 07:00]--[2015-08-06 Thu 11:30] =>  4:30
;; currently:
;;   CLOCK: [2015-08-06 Thu 11:30]--[2015-08-06 Thu 16:00] =>  4:30
;;   CLOCK: [2015-08-06 Thu 07:00]--[2015-08-06 Thu 11:30] =>  4:30
(defun org-clock-split-current-interval (end-as-default)
  "If this is a CLOCK line, split its clock time interval into two.
Let the current time interval be A--C.  By default, this function
interactively prompts for a time B (suggesting A as a default), and then
replaces A--C by B--C and A--B.  When called with a prefix argument, the
function uses C as a default for B.  The point is left on the later
interval, so that this line can, e.g., be moved to another entry."
     (interactive "P")
     (save-excursion
       (beginning-of-line nil)
       (skip-chars-forward " \t")
       (when (looking-at org-clock-string)
         (beginning-of-line nil)
         (let ((re (concat "\\([ \t]*" org-clock-string " *\\)"
			   "\\([[<][^]>]+[]>]\\)\\(-+\\)\\([[<][^]>]+[]>]\\)"
                           "\\(?:[ \t]*=>.*\\)?")))
           (when (looking-at re)
             (let ((indentation (match-string 1))
                   (start (match-string 2))
                   (to (match-string 3))
                   (end (match-string 4))
                   (use-start-as-default (equal end-as-default nil)))
               ;; interactively change A--C to B--C,
               ;; or (given prefix argument) to A--B, …
               (re-search-forward (concat org-clock-string " \\([[<]\\)"))
               (when (not use-start-as-default) (re-search-forward
						 "\\([[<]\\)"))
               ;; respecting whether A or C is an active or an
	       ;; inactive timestamp
               (call-interactively (if (equal (match-string 1) "<")
				       'org-time-stamp
				     'org-time-stamp-inactive))
               ;; If there were a function that implemented the actual
	       ;; body of org-clock-update-time-maybe, we could call that function, as in
	       ;; this context we _know_ that we are on a CLOCK line.
               (org-clock-update-time-maybe)
               ;; copy changed time B
               (re-search-backward org-ts-regexp-both)
               (let ((middle (match-string 0)))
                 ;; insert A--B below, or (given prefix argument) insert B--C above
                 (end-of-line (if use-start-as-default 1 0))
                 (insert "\n" indentation
                         (if use-start-as-default start middle)
                         to
                         (if use-start-as-default middle end))
                 (org-clock-update-time-maybe))))))))

;; because of a bug in the report-table
;; http://emacs.stackexchange.com/questions/9528/is-it-possible-to-remove-emsp-from-clock-report-but-preserve-indentation
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "`"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "-> "))))
(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)
