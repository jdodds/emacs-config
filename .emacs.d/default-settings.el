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
      `((".*" . ,temporary-file-directory)))

(setq enable-recursive-minibuffers t)
(setq display-buffer-reuse-frames t)


(setq truncate-partial-width-windows nil)

(setq-default fill-column 80)

(setq erc-auto-query 'buffer)

(setq org-log-done 'time)
(setq org-src-fontify-natively t)
(setq org-startup-indented t)

(put 'narrow-to-region 'disabled nil)

(put 'upcase-region 'disabled nil)

(provide 'default-settings)
;;; default-settings.el ends here
