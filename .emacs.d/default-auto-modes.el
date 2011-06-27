(setq modes-list
      '(("\\.md" . markdown-mode)
	("\\.twig$" . django-html-mode)
	("PKGBUILD" . sh-mode)
	("\\.yml$" . yaml-mode)
	("\\.rst$" . rst-mode)
	("\\.org$" . org-mode)
	("\\.php$" . php-mode)
	("\\.lua$" . lua-mode)
        ("\\.htaccess$" . apache-mode)
        ("httpd.*\\.conf$" . apache-mode)
        ("srm\\.conf$" . apache-mode)
        ("access\\.conf$" . apache-mode)
        ("sites-\\(available\\|enabled\\)/" . apache-mode)))
(dolist (alist modes-list)
  (add-to-list 'auto-mode-alist alist))

(provide 'default-auto-modes)
;;; default-auto-modes.el ends here
