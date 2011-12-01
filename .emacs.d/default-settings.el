(setq ring-bell-function '(lambda () t))
(setq comint-output-filter-functions
      (function (lambda (STR) (comint-show-output))))

(setq qooxdoo-api-url "http://cogneato.local/qx/controlcenter/api/index.html#")

(if (string-match "destructor" system-name)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "chromium")
  (setq browse-url-generic-program 'browse-url-generic
        browse-url-generic-program "chrome"))

(setq tags-revert-without-query t)

;keep TRAMP from saving backups
(if (boundp 'tramp-file-name-regexp)
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp nil)))

(setq tramp-default-method "ssh")

(setq backup-directory-alist
      `(("." . "~/.saves")))
(setq backup-by-copying t)

(setq enable-recursive-minibuffers t)
(setq display-buffer-reuse-frames t)


(setq truncate-partial-width-windows nil)

(setq-default fill-column 80)
(setq-default auto-fill-function 'do-auto-fill)

(setq erc-auto-query 'buffer)

(setq org-log-done 'time)
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(setq org-startup-align-all-tables t)
(setq org-footnote-auto-adjust t)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-todo nil)
(setq org-agenda-include-diary t)
(setq org-log-into-drawer t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@/1)")))
(setq org-clock-idle-time 120)
(setq org-directory "~/Dropbox")
(setq org-mobile-inbox-for-pull "~/Dropbox/mobile-inbox.org")
(setq org-cycle-separator-lines 0)
(setq org-list-demote-modify-bullet
      '(("+" . "-") ("-" . "+")))
(setq org-tag-alist '(("@itr" . ?i)
                      ("@shopping" . ?s)
                      ("@personal" . ?p)
                      ("@bills" . ?b)
                      ("@review" . ?r)))

(setq org-footnote-auto-label 'confirm)
(setq org-hierarchical-todo-statistics nil)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-default-notes-file (concat org-directory "~/Dropbox/notes.org"))
(setq org-archive-location (concat org-directory "/Dropbox/archive.org::"))
(setq org-agenda-todo-list-sublevels t)
(setq org-id-track-globally t)

(put 'narrow-to-region 'disabled nil)

(put 'upcase-region 'disabled nil)

(setq uniquify-buffer-name-style 'post-forward)

(setq yaml-indent-offset 4)

(add-to-list 'which-func-modes 'php-mode)
(which-func-mode 1)

(setq redisplay-dont-pause t)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa")
                         ("gnu" . "http://elpa.gnu.org/packages")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(tooltip-mode -1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer'always)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      kill-buffer-query-functions (remq
				   'process-kill-buffer-query-function
				   kill-buffer-query-functions)
      tooltip-mode-use-echo-area t
      shell-command-default-error-buffer 'shell-errors)
      
(ido-mode 1)

(provide 'default-settings)
;;; default-settings.el ends here
