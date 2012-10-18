
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refiling targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(org-clock-persistence-insinuate)

(setq org-log-done 'time
      org-modules '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)
      org-src-fontify-natively t

      org-startup-indented t
      org-startup-folded t
      org-startup-align-all-tables t

      org-footnote-auto-adjust t

      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t

      org-agenda-dim-blocked-todo nil
      org-agenda-dim-blocked-tasks t

      org-agenda-include-diary nil
      org-agenda-compact-blocks nil
      org-agenda-show-all-dates nil

      org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels nil)))
        ("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        (" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")))
          (tags-todo "-WAITING-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
                     ((org-agenda-overriding-header "Tasks")
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-HOLD-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED+WAITING/!"
                     ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                      (org-agenda-skip-function 'bh/skip-stuck-projects)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-todo-ignore-deadlines 'future)))
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil))))
         nil)
        ("r" "Tasks to Refile" tags "REFILE"
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-tags-match-list-sublevels nil)))
        ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
         ((org-agenda-overriding-header "Stuck Projects")
          (org-tags-match-list-sublevels nil)))
        ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
         ((org-agenda-overriding-header "Next Tasks")
          (org-agenda-todo-ignore-scheduled t)
          (org-agenda-todo-ignore-deadlines t)
          (org-agenda-todo-ignore-with-date t)
          (org-tags-match-list-sublevels nil)
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
         ((org-agenda-overriding-header "Tasks")
          (org-agenda-sorting-strategy
           '(category-keep))))
        ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
         ((org-agenda-overriding-header "Projects")
          (org-agenda-sorting-strategy
           '(category-keep))))
        ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
         ((org-agenda-overriding-header "Waiting and Postponed tasks"))
         (org-tags-match-list-sublevels nil))
        ("A" "Tasks to Archive" tags "-REFILE/"
         ((org-agenda-overriding-header "Tasks to Archive")
          (org-tags-match-list-sublevels nil))))
                                   
      org-edit-src-content-indentation 0
      org-src-window-setup 'current-window
      
      org-startup-with-inline-images t
      
      org-log-into-drawer t
      
      org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
                                    "CANCELLED(c@/!)"))

      ;; moving to CANCELLED adds a CANCELLED tag,
      ;; moving to WAITING adds a WAITING tag,
      ;; moving to HOLD adds a WAITING tag,
      ;; marking as done removes a WAITING tag
      ;; moving to TODO removes WAITING and CANCELLED tag
      ;; moving to NEXT removes a WAITING tag
      ;; moving to DONE removes WAITING and CANCELLED tags
      org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                     ("WAITING" ("WAITING" . t))
                                     ("HOLD" ("WAITING" . t) ("HOLD" . t))
                                     (done ("WAITING") ("HOLD"))
                                     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                     ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                     ("DONE" ("WAITING") ("CANCELLED")
                                      ("HOLD")))


      org-todo-keyword-face '(("TODO" :foreground "red" :weight bold)
                              ("NEXT" :foreground "blue" :weight bold)
                              ("DONE" :foreground "forest green" :weight bold)
                              ("WAITING" :foreground "orange" :weight bold)
                              ("HOLD" :foreground "dark grey" :weight bold)
                              ("CANCELLED" :foreground "forest green"
                               :weight bold))

      ;; change todo states with a fast selection key menu (C-c C-t KEY)
      org-use-fast-todo-selection t

      ;; change todo states with S-left and S-right, but don't do things like
      ;; entering timestamps or notes.
      org-treat-S-cursor-todo-selection-as-state-change nil
      
      org-directory "~/workspace/planning"

      org-clock-idle-time 120
      org-clock-persist t
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-in-resume t
      org-clock-persist-query-resume nil
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      org-clock-report-include-clocking-task t
      
      org-cycle-separator-lines 0

      org-list-demote-modify-bullet
      '(("+" . "-") ("-" . "+"))

      org-blank-before-new-entry '((heading . nil)
                                   (plain-list-item . nil))
      org-return-follows-link t
      org-footnote-auto-label 'confirm
      org-hierarchical-todo-statistics nil

      org-default-notes-file (concat org-directory "/refile.org")

      org-archive-mark-done nil
      org-archive-location "%s_archive::* Archived Tasks"
      
      org-agenda-todo-list-sublevels nil
      org-id-track-globally t
      org-feed-retrieve-method 'curl


      org-pretty-entities t
      org-capture-templates '(("t" "todo" entry
                               (file (concat org-directory "/refile.org"))
                               "* TODO %?\n%U\n%a\n"
                               :clock-in t
                               :clock-resume t)
                              ("r" "respond" entry
                               (file (concat org-directory "/refile.org"))
                               "* TODO Respond to %:from on %:subject\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                              ("n" "note" entry
                               (file (concat org-directory "/refile.org"))
                               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                              ("j" "Journal" entry
                               (file+datetree (concat org-directory "/diary.org"))
                               "* %?\n%U\n" :clock-in t :clock-resume t)
                              ("h" "Habit" entry
                               (file (concat org-directory "/refile.org"))
                               "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))
      org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9))
      org-refile-use-outline-path t
      org-habit-graph-column 82
      org-outline-path-complit-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-target-verify-function 'bh/verify-refile-target
      org-completion-use-ido t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (java . t)
   (ruby . t)))

(setq keys-to-set
      `(("\C-cb" org-iswitchb)
        ("\C-ca" org-agenda)
        ("\C-cl" org-store-link)
        (,(kbd "<f12>") org-agenda)
        (,(kbd "<f5>") bh/org-todo)
        (,(kbd "<S-f5>") bh/widen)
        (,(kbd "<f7>") bh/set-truncate-lines)
        (,(kbd "<f8>") org-cycle-agenda-files)
        (,(kbd "<f9> <f9>") bh/show-org-agenda)
        (,(kbd "<f9> b") bbdb)
        (,(kbd "<f9> c") calendar)
        (,(kbd "<f9> f") boxquote-insert-file)
        (,(kbd "<f9> g") gnus)
        (,(kbd "<f9> h") bh/hide-other)
        (,(kbd "<f9> n") org-narrow-to-subtree)
        (,(kbd "<f9> w") widen)
        (,(kbd "<f9> u") bh/narrow-up-one-level)
        (,(kbd "<f9> I") bh/punch-in)
        (,(kbd "<f9> O") bh/punch-out)
        (,(kbd "<f9> o") bh/make-org-scratch)
        (,(kbd "<f9> r") boxquote-region)
        (,(kbd "<f9> s") bh/switch-to-scratch)
        (,(kbd "<f9> t") bh/insert-inactive-timestamp)
        (,(kbd "<f9> T") tabify)
        (,(kbd "<f9> U") untabify)
        (,(kbd "<f9> v") visible-mode)
        (,(kbd "<f9> SPC") bh/clock-in-last-task)
        (,(kbd "C-<f9>") previous-buffer)
        (,(kbd "M-<f9>") org-toggle-inline-images)
        (,(kbd "C-x n r") narrow-to-region)
        (,(kbd "C-<f10>") next-buffer)
        (,(kbd "<f11>") org-clock-goto)
        (,(kbd "C-<f11>") org-clock-in)
        (,(kbd "C-s-<f12>") bh/save-then-publish)
        (,(kbd "C-M-r") org-capture)))                                       

(dolist (a-key keys-to-set)
  (global-set-key (first a-key) (second a-key)))




(provide 'org-settings)
