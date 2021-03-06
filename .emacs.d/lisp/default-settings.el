(setq tags-revert-without-query t
      tramp-default-method "ssh"
      display-buffer-reuse-frames t
      backup-directory-alist `(("." . ,(expand-file-name "~/.saves/")))
      backup-by-copying t
      enable-recursive-minibuffers t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'post-forward
      redisplay-dont-pause t
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/"))
      ido-enable-flex-matching t
      ido-create-new-buffer 'prompt
      ido-evereywhere nil
      ido-max-directory-size 100000
      ido-mode 'buffer
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      tooltip-mode-use-echo-area t
      line-move-visual nil
      compilation-scroll-output nil
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
      smtpmail-local-domain nil
      smtpmail-stream-type 'ssl
      user-mail-address "jeremiah.dodds@gmail.com"
      langtool-language-tool-jar "/usr/share/languagetool/LanguageTool.jar"
      multi-term-program "/bin/zsh"
      sauron-hide-mode-line t
      sauron-sticky-frame t
      track-eol t
      ditaa-cmd "/usr/bin/ditaa")

(setq-default fill-column 80
	      indent-tabs-mode nil
	      auto-fill-function 'do-auto-fill)

;keep TRAMP from saving backups
(if (boundp 'tramp-file-name-regexp)
    (add-to-list 'backup-directory-alist
		 (cons tramp-file-name-regexp nil)))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-to-list 'which-func-modes 'php-mode)
(add-to-list 'which-func-modes 'python-mode)
(add-to-list 'which-func-modes 'sh-mode)

(which-func-mode 1)
(tooltip-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(ido-mode 1)
(hl-line-mode 1)
(rcirc-track-minor-mode 1)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(provide 'default-settings)
;;; default-settings.el ends here
