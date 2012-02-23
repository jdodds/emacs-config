(setq ring-bell-function '(lambda () t)
      tags-revert-without-query t
      tramp-default-method "ssh"
      backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying t
      enable-recursive-minibuffers t
      display-buffer-reuse-frames t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'post-forward
      redisplay-dont-pause t
      package-archives '(("ELPA" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/"))
      ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer'always
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      kill-buffer-query-functions (remq
				   'process-kill-buffer-query-function
				   kill-buffer-query-functions)
      tooltip-mode-use-echo-area t
      yaml-indent-offset 4
      monky-process-type 'cmdserver
      temporary-file-directory "/tmp"
      line-move-visual nil
      gnus-select-method '(nnimap "imap.gmail.com")
      gnus-group-line-format "%N\t:%P%G\n"
      message-subscribed-address-functions '(gnus-find-subscribed-addresses)
      send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 465 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 465
				   "jeremiah.dodds@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
;      smtpmail-debug-verb t
;      smtpmail-debug-nfo t
      smtpmail-local-domain nil
      smtpmail-stream-type 'ssl
      user-mail-address "jeremiah.dodds@gmail.com"
;      setq mm-discouraged-alternatives '("text/html" "text/richtext")
      track-eol t)

(setq-default fill-column 80
	      auto-fill-function 'do-auto-fill
	      indent-tabs-mode t)
      
(if (string-match "destructor" system-name)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "chromium")
  (setq browse-url-generic-program 'browse-url-generic
        browse-url-generic-program "chrome"))

;keep TRAMP from saving backups
(if (boundp 'tramp-file-name-regexp)
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp nil)))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(add-to-list 'which-func-modes 'php-mode)
(which-func-mode 1)
(tooltip-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(ido-mode 1)
(hl-line-mode 1)
(provide 'default-settings)
;;; default-settings.el ends here
