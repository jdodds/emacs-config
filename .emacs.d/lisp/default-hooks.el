(add-hook 'qooxdoo-project-file-visit-hook
          '(lambda ()
             (setq whitespace-action '("auto-cleanup"))
             (auto-fill-mode t)
             (setq indent-tabs-mode nil)
             (setq espresso-indent-level 4)
             (setq tab-width 4)))

(add-hook 'php-mode-hook
          '(lambda ()
             (rainbow-delimiters-mode t)
             (setq c-default-style "bsd")
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
             (setq c-basic-offset 4)
             (defun sane-php-lineup-arglist-intro (langelem)
               (save-excursion
                 (goto-char (cdr langelem))
                 (vector (+ (current-column) c-basic-offset))))
             (defun sane-php-lineup-arglist-close (langelem)
               (save-excursion
                 (goto-char (cdr langelem))
                 (vector (current-column))))
             (c-set-offset 'arglist-intro 'sane-php-lineup-arglist-intro)
             (c-set-offset 'arglist-close 'sane-php-lineup-arglist-close)))

(add-hook 'js-mode-hook
          '(lambda ()
             (rainbow-delimiters-mode t)))

(add-hook 'haskell-mode-hook
          '(lambda()
             (turn-on-haskell-doc-mode)
             (turn-on-haskell-indent)))

(add-hook 'before-save-hook 'whitespace-cleanup)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

(add-hook 'html-mode-hook
          (lambda ()
            (setq sgml-basic-offset 4)
            (setq tab-width 4)
            (setq indent-tabs-mode nil)
            (setq sgml-unclosed-tags nil)))


(add-hook 'gnus-summary-exit-hook
          (lambda ()
            (gnus-summary-bubble-group)))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

(provide 'default-hooks)
;;; default-hooks.el ends here
