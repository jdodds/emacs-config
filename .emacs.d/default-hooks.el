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
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
	     (setq c-default-style "bsd")
             (setq c-basic-offset 4)
;	     (flymake-mode 1)
	     (defun sane-php-lineup-arglist-intro (langelem)
	       (save-excursion
		 (goto-char (cdr langelem))
		 (vector (+ (current-column) c-basic-offset))))
	     (defun sane-php-lineup-arglist-close (langelem)
	       (save-excursion
		 (goto-char (cdr langelem))
		 (vector (current-column))))
;	     (define-key php-mode-map '[M-g n] 'flymake-goto-next-error)
;	     (define-key php-mode-map '[M-g p] 'flymake-goto-prev-error)
	     (c-set-offset 'arglist-intro 'sane-php-lineup-arglist-intro)
	     (c-set-offset 'arglist-close 'sane-php-lineup-arglist-close)))


(add-hook 'js-mode-hook
          '(lambda ()
             (rainbow-delimiters-mode t)))

(add-hook 'haskell-mode-hook
          '(lambda()
             (turn-on-haskell-doc-mode)
             (turn-on-haskell-indent)))


;(remove-hook 'before-save-hook 'whitespace-cleanup)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
;(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'html-mode-hook
	  (lambda ()
	    (setq sgml-basic-offset 4)
	    (setq tab-width 4)
	    (setq indent-tabs-mode t)))

(add-hook 'gnus-summary-exit-hook
	  (lambda ()
	    (gnus-summary-bubble-group)))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(provide 'default-hooks)
;;; default-hooks.el ends here
