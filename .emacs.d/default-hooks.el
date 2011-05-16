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
	     (set-electrics)))

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

(provide 'default-hooks)
;;; default-hooks.el ends here
