(require 'notmuch)

;; load also org-mode stuff for notmuch
(load-file "~/etc/emacs/org-notmuch.el")
(require 'org-notmuch)

;; basic configurations for sending emails
(setq mail-specify-envelope-from t
      message-sendmail-envelope-from 'header)

;; disable hello-logo
(setq notmuch-hello-logo nil)

;; show all tags in the tags-list
(setq notmuch-show-all-tags-list t)

;; jump to the first saved-search in notmuch-hello
(add-hook 'notmuch-hello-refresh-hook
	  (lambda ()
	    (beginning-of-line)
	    (goto-char 0)
	    (widget-forward 4)))

;; set right fcc header
;; defines where outgoing messages will be saved
(setq notmuch-fcc-dirs
      '(("oliver.dunkl@gmail.com" . "priv/sent")
	("oliver.dunkl@rise-world.com" . "work/sent")
	(".*" . "priv/sent")))

;; show newest messages first in the search view
(setq notmuch-search-oldest-first nil)

;;(notmuch-crypto-process-mime t)

;; do not indent my messages
(setq notmuch-show-indent-messages-width 0)

;; define archiving tags
(setq notmuch-archive-tags
      '("+archived" "-inbox" "-flagged"))

;; saved searches
(setq notmuch-saved-searches
      '((:name "priv" :query "tag:unread and tag:priv" :key "u")
	(:name "work" :query "tag:unread and tag:work" :key "w")
	(:name "nix" :query "tag:unread and tag:nix" :key "x")
	(:name "haskell" :query "tag:unread and tag:haskell" :key "h")
	(:name "notmuch" :query "tag:unread and tag:notmuch" :key "n")
	(:name "emacs" :query "tag:unread and tag:emacs" :key "e")
	(:name "metalab" :query "tag:unread and tag:metalab" :key "m")
	(:name "orgmode" :query "tag:unread and tag:orgmode" :key "o")
	(:name "flagged" :query "tag:flagged" :key "f")
	(:name "misc" :query "tag:misc")
	(:name "today" :query "date:today..today or (tag:muted and date:today..today) or (tag:deleted and date:today..today) or (tag:archived and date:today..today)")
	(:name "trash" :query "folder:priv/trash or (folder:priv/trash and (tag:deleted or tag:archived))")))

;; count the result with `notmuch count'
;; used with hydra to show ho many messages are in the query
(defun notmuch-search-count (query)
  (string-to-number
   (shell-command-to-string (concat "notmuch count " query))))

(setq notmuch-search-line-faces
      '(("unread" :weight bold)))
(setq notmuch-tag-formats
      '(("unread" (propertize tag 'face '(:weight normal)))
	("deleted" (propertize tag 'face '(:weight normal)))
	("priv" (propertize tag 'face '(:weight normal)))
	("flagged" (propertize tag 'face '(:weight normal)))
	("attachment" (propertize tag 'face '(:weight normal)))
	("work" (propertize tag 'face '(:weight normal)))
	("list" (propertize tag 'face '(:weight normal)))
	("orgmode" (propertize tag 'face '(:weight normal)))
	("haskell" (propertize tag 'face '(:weight normal)))
	("nix" (propertize tag 'face '(:weight normal)))
	("metalab" (propertize tag 'face '(:weight normal)))
	("notmuch" (propertize tag 'face '(:weight normal)))
	("scouts" (propertize tag 'face '(:weight normal)))
	("signed" (propertize tag 'face '(:weight normal)))
	("replied" (propertize tag 'face '(:weight normal)))
	("cal" (propertize tag 'face '(:weight normal)))
	("muted" (propertize tag 'face '(:weight normal)))))
(setq notmuch-tag-added-formats
      '((".*" (notmuch-apply-face tag '(:underline "YellowGreen")))))
(setq notmuch-tag-deleted-formats
      '((".*" (notmuch-apply-face tag '(:strike-through "tomato1")))))

(setq notmuch-search-result-format
      '(("date" . "%12s ")
	("count" . "%-7s")
	("authors" . "%-30s")
	("subject" . "%s")
	("tags" . " %s")))

;; show the attached patch in diff-mode
(defun notmuch-show-view-as-patch ()
  "View the the current message as a patch."
  (interactive)
  (let* ((id (notmuch-show-get-message-id))
         (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
         (diff-default-read-only t)
         (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
	 (part (notmuch-show-get-part-properties))
         (map (make-sparse-keymap)))
    (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert subject)
      (insert (plist-get part :content)))
    (set-buffer-modified-p nil)
    (diff-mode)
    (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
                 (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
    (goto-char (point-min))))

;; ## keyboard shortcuts

;; goto notmuch-hello
(define-key global-map (kbd "C-c n n") 'notmuch-hello)
(define-key global-map (kbd "C-c n s") 'notmuch-search)
(define-key global-map (kbd "C-c n u")
  (lambda ()
    (interactive)
    (notmuch-search "tag:unread")))
(define-key global-map (kbd "C-c n i")
  (lambda ()
    (interactive)
    (notmuch-search "tag:unread and tag:inbox")))
(define-key global-map (kbd "C-c n w")
  (lambda ()
    (interactive)
    (notmuch-search "tag:unread and tag:work")))
(define-key global-map (kbd "C-c n l")
  (lambda ()
    (interactive)
    (notmuch-search "tag:unread and tag:list")))
(define-key global-map (kbd "C-c n f")
  (lambda ()
    (interactive)
    (notmuch-search "tag:flagged")))

;; toggle deleted tag in show-mode
(define-key notmuch-show-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
	(notmuch-show-tag (list "-deleted"))
      (notmuch-show-tag (list "+deleted" "-unread" "-flagged")))))

;; toggle delete tag for whole thread
;; if some of the messages in a thread has a tag `deleted' it will remove it
;; if none of the messages in a thread has a tag `deleted' all will get it
(define-key notmuch-search-mode-map "d"
  (lambda ()
    "toggle deleted tag for thread"    ;; in notmuch-search-mode
    (interactive)
    (notmuch-search-tag (list "+deleted" "-unread" "-flagged"))
    (notmuch-search-next-thread)))
    ;; (if (member "deleted" (notmuch-search-get-tags))
    ;; 	(notmuch-search-tag (list "-deleted"))
    ;;   ((notmuch-search-tag (list "+deleted" "-unread" "-flagged"))
    ;;    (notmuch-search-next-thread)))))

;; toggle tag for muting threads
;; this is only useful in the search-mode-map
(define-key notmuch-search-mode-map ","
  (lambda ()
    "toggle muting a whole thread"
    (interactive)
    (if (member "muted" (notmuch-search-get-tags))
	(notmuch-search-tag (list "-muted"))
      (notmuch-search-tag (list "+muted" "-unread")))
    (notmuch-search-next-thread)))

(define-key notmuch-show-stash-map "g"
  (lambda ()
    "Copy a link to gmane archive of the current message to kill-ring"
    (interactive)
    (notmuch-common-do-stash
     (concat "http://mid.gmane.org/"
	     (replace-regexp-in-string
	      "^id:\"\\(.*\\)\"$" "\\1" (notmuch-show-get-message-id t))))))

;; edit message
;; call a seperate script `edit-mail.sh'
;; (define-key 'notmuch-show-mode-map "E"
;;   (lambda ()
;;     (interactive)
;;     (let ((mid (notmuch-show-get-message-id)))
;;       (message mid)
;;       (start-process "edit-email" nil "edit-mail.sh" mid))))

;; call sync-mail.sh
(defun notmuch-sync ()
  (interactive)
  (async-shell-command "sync-mail.sh" "*syncing mail*" nil))

(defun notmuch-cleanup ()
  (interactive)
  (async-shell-command "notmuch-cleanup.sh" "*notmuch cleanup*" nil))

;; in notmuch-hello start syncing messages with G
(define-key 'notmuch-hello-mode-map "G" nil)
  ;; (lambda ()
  ;;   "syncing emails with notmuch-cleanup.sh and offlineimap"
  ;;   (interactive)
  ;;   (notmuch-sync)))

;; define D for showing patches in diff-mode in notmuch-show-mode
(define-key 'notmuch-show-mode-map "D" 'notmuch-show-view-as-patch)

;; define RET for tree-search view to enter a message in tree-view instead of show-view
(define-key notmuch-search-mode-map (kbd "RET") 'notmuch-search-show-thread)

;; TODO: override notmuch-show-insert-headerline
(setq notmuch-message-headers '("Subject" "To" "Cc" "Date" "Message-ID"))

;; -- overridden
(defun notmuch-show-fontify-header ()
  (let ((face (cond
	       ((looking-at "[Tt]o:")
		'message-header-to)
	       ((looking-at "[Bb]?[Cc][Cc]:")
		'message-header-cc)
	       ((looking-at "[Ss]ubject:")
		'message-header-subject)
	       ((looking-at "[Ff]rom:")
		'message-header-from)
	       (t
		'message-header-other))))

    (overlay-put (make-overlay (point) (re-search-forward ":"))
		 'face 'message-header-name)
    (overlay-put (make-overlay (point) (re-search-forward ".*$"))
		 'face face)))

;; overrides function because of not showing the header-line
;; we do not need them
(defun notmuch-show-build-buffer ()
  (let ((inhibit-read-only t))

    (notmuch-show-mode)
    (add-hook 'post-command-hook #'notmuch-show-command-hook nil t)

    ;; Don't track undo information for this buffer
    (set 'buffer-undo-list t)

    (notmuch-tag-clear-cache)
    (erase-buffer)
    (goto-char (point-min))
    (save-excursion
      (let* ((basic-args (list notmuch-show-thread-id))
	     (args (if notmuch-show-query-context
		       (append (list "\'") basic-args
			       (list "and (" notmuch-show-query-context ")\'"))
		     (append (list "\'") basic-args (list "\'"))))
	     (cli-args (cons "--exclude=false"
			     (when notmuch-show-elide-non-matching-messages
			       (list "--entire-thread=false")))))

	(notmuch-show-insert-forest (notmuch-query-get-threads (append cli-args args)))
	;; If the query context reduced the results to nothing, run
	;; the basic query.
	(when (and (eq (buffer-size) 0)
		   notmuch-show-query-context)
	  (notmuch-show-insert-forest
	   (notmuch-query-get-threads (append cli-args basic-args)))))

      (jit-lock-register #'notmuch-show-buttonise-links)

      (notmuch-show-mapc (lambda () (notmuch-show-set-prop :orig-tags (notmuch-show-get-tags))))

      (run-hooks 'notmuch-show-hook))))

