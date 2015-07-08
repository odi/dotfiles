(require 'nnir)
(setq nnir-notmuch-remove-prefix "/mnt/secure/mail/")

;; default select method
(setq gnus-select-method
      '(nnmaildir "mbox"
		  (directory "~/mail/")
		  (get-new-mail t)
		  (nnir-search-engine notmuch)))

;; use gwene for rss-feeds and search gmane nntp-archive
(add-to-list 'gnus-secondary-select-methods
	     '(nntp "news.gwene.org"))

;; dont save outgoing messages in an archive group
;; I would save my outgoing messages in the group itself
(setq gnus-message-archive-group nil)

;; directory of my drafts
(setq nndraft-directory "~/mail/misc.drafts")

;; define a function for expiring messages from different groups
(setq nnmail-expiry-wait-function
      (lambda (group)
	(cond ((string= group "mail.private") 90)
	      ((string= group "misc.spam") 20)
	      ((string= group "misc.trash") 60)
	      (t 60))))

;; define some useful parameters for different groups
(setq gnus-parameters
      '(("mail.private"
	 (comment . "my private messages")
	 (gcc-self . t)
	 (total-expire . t)
	 (expire-group . (concat "arch." (format-time-string "%Y"))))
	("misc.spam"
	 (total-expire . t)
	 (expire-group . "misc.trash"))
	("misc.trash"
	 (total-expire . t)
	 (expire-group . delete))
	("list.*"
	 (gcc-self . t)
	 (total-expire . t)
	 (expire-group . "misc.trash")
	 (gnus-summary-line-format "%U%R%z%( %d %[ %-23,23a%]%) %B%s\n"))))

;; define layout of summary line
;; default: "%U%R%z%I%(%[%4L: %-23,23f%]%) %s"
;; TODO: is it possible to use a different style for different groups?
(setq gnus-summary-line-format "%U%R%z%( %d |%5k| %[ %-23,23a%]%) %B%s\n"
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root "> "
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-false-root "> "
      gnus-sum-thread-tree-vertical "| "
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-single-leaf "\\-> ")

;; layout of threads
(setq gnus-thread-indent-level 4
      gnus-thread-sort-functions '(gnus-thread-sort-by-number
				   gnus-thread-sort-by-date)
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

(setq gnus-group-line-format "%M%S%p%P%3y:%B%(%-25uG%) %5t\n")

;; mark sent items as read; that means all Gcc articles will be markes as read
(setq gnus-gcc-mark-as-read t)

(defun gnus-user-format-function-G (arg)
  (let ((mapped-name (assoc gnus-tmp-group gnus-group-name-map)))
    (if (null mapped-name)
	gnus-tmp-group
      (cdr mapped-name))))

(setq gnus-group-name-map
      '(("nntp+news.gwene.org:gwene.com.reddit.emacs" . "Emacs Reddit")
	("nntp+news.gwene.org:gwene.com.reddit.r.nixos" . "NixOS Reddit")
	("nntp+news.gwene.org:gwene.com.reddit.r.haskell" . "Haskell Reddit")
	("nntp+news.gwene.org:gwene.com.dilbert" . "Dilbert")
	("nntp+news.gwene.org:gwene.at.derstandard.web" . "derStandard Web")
	("nntp+news.gwene.org:gwene.org.haskell.planet" . "Planet Haskell")
	("nnimap+mail.rise-world.com:INBOX" . "rise.inbox")
	("nnimap+mail.rise-world.com:Sent" . "rise.sent")))
