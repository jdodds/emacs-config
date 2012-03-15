(defun org-feed-active-collab-formatter (entry)
  "Parses an ActiveCollab RSS entry and returns an Org outline node with useful
  information." 
  (require 'xml)
  (let ((desc (plist-get entry :description))
	(link (plist-get entry :guid))
	(deadline nil)
	(headline nil)
	(description nil))
    (if (string-match-p "Due on:" desc)
	(progn
	  (string-match "<b>Due on:</b>\\([^<]+\\)" desc)
	  (setq deadline (match-string 1 desc))))
    (string-match
     "\\(?:Ticket\\|Milestone\\):</b> <a[^>]+>\\([^<]+\\)</a"
     desc)
    (setq headline (match-string 1 desc))
    (string-match "<hr />\\(?:\n\\|<p>\\|\\\\n\\| \\)+\\([^]<]+\\)" desc)
    (setq description (match-string 1 desc))
    (concat
     "* TODO [["
     link
     "]["
     headline
     "]]"
     (if (not (null deadline))
	 (concat
	  "\nDEADLINE: <"
	  (format-time-string
	   "%Y-%m-%d %a"
	   (apply 'encode-time
		  (org-fix-decoded-time (parse-time-string deadline))))
	  ">")
       "")
     "\n"
     description)))

(setq org-log-done 'time
      org-src-fontify-natively t
      org-startup-indented t
      org-startup-folded t
      org-startup-align-all-tables t
      org-footnote-auto-adjust t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-agenda-dim-blocked-todo nil
      org-agenda-include-diary t
      org-log-into-drawer t
      org-todo-keywords
      '((sequence "TODO(t)" "STARTED(!)" "WAITING(w@/!)" "|" "DONE(d!)"
		  "CANCELLED(c@/1)"))
      org-directory "~/Dropbox"
      org-clock-idle-time 120
      org-mobile-inbox-for-pull "~/Dropbox/mobile-inbox.org"
      org-cycle-separator-lines 0
      org-list-demote-modify-bullet
      '(("+" . "-") ("-" . "+"))
      org-tag-alist '(("@itr" . ?i)
		      ("@shopping" . ?s)
		      ("@personal" . ?p)
		      ("@bills" . ?b)
		      ("@review" . ?r))
      org-blank-before-new-entry '((heading . nil)
				   (plain-list-item . nil))
      org-return-follows-link t
      org-footnote-auto-label 'confirm
      org-hierarchical-todo-statistics nil
      org-clock-persist 'history
      org-default-notes-file (concat org-directory "/notes.org")
      org-archive-location (concat org-directory "/archive.org::")
      org-agenda-todo-list-sublevels t
      org-id-track-globally t
      org-feed-alist '(("itr tickets"
			"https://www.intheround.net/ac/index.php?path_info=assignments/23/rss&token=36-b5E55f1Tog8FNkmHS6eZJO56BaROTH58lDQmWwTV"
			"~/Dropbox/itr.org"
			"Open Tickets"
			:formatter org-feed-active-collab-formatter))
      org-feed-retrieve-method 'curl
      org-agenda-files '("~/Dropbox/software.org" "~/Dropbox/personal.org" "~/Dropbox/learning.org" "~/Dropbox/music.org" "~/Dropbox/MobileOrg/flagged.org" "~/Dropbox/shopping.org" "~/Dropbox/bills.org" "~/Dropbox/TODO.org" "~/Dropbox/scheduled.org" "~/Dropbox/itr.org" "~/Dropbox/immediate.org")
      org-directory "~/Dropbox"
      org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
				     ("WAITING" ("WAITING" . t))
				     (done ("WAITING"))
				     ("TODO" ("WAITING") ("CANCELLED"))
				     ("DONE" ("WAITING") ("CANCELLED")))
				     
      org-modules '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)

      )
(org-clock-persistence-insinuate)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (java . t)
;   (scala . t)
   (ruby . t)))


(provide 'org-settings)
