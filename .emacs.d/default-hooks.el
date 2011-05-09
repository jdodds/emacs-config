(add-hook 'qooxdoo-project-file-visit-hook
	  '(lambda ()
	     (setq whitespace-action '("auto-cleanup"))
	     (auto-fill-mode t)
	     (setq indent-tabs-mode nil)
	     (setq espresso-indent-level 4)
	     (setq tab-width 4)))

(add-hook 'php-mode-hook
          'set-electrics)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(provide 'default-hooks)
;;; default-hooks.el ends here
