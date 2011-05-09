(setq modes-list
      '(("\\.md" . markdown-mode)
	("\\.twig$" . django-html-mode)
	("PKGBUILD" . sh-mode)
	("\\.yml$" . yaml-mode)
	("\\.rst$" . rst-mode)
	("\\.org$" . org-mode)
	("\\.lua$" . lua-mode)))
(dolist (alist modes-list)
  (add-to-list 'auto-mode-alist alist))

(provide 'default-auto-modes)
;;; default-auto-modes.el ends here
