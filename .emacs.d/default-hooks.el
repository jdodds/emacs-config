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
             (setq c-basic-offset 4)
             (c-set-offset 'arglist-intro 4)
             (c-set-offset 'arglist-close 0)))

(add-hook 'js-mode-hook
          '(lambda ()
             (rainbow-delimiters-mode t)))

(add-hook 'haskell-mode-hook
          '(lambda()
             (turn-on-haskell-doc-mode)
             (turn-on-haskell-indent)))

(add-hook 'sgml-mode-hook
          '(lambda()
             (zencoding-mode t)))

(add-hook 'before-save-hook 'whitespace-cleanup)

(provide 'default-hooks)
;;; default-hooks.el ends here
