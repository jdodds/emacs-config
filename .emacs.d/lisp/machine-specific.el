(cond
 ((eq system-type 'darwin)
  (setq-default indent-tabs-mode nil)
  (setq custom-file "~/.emacs.d/lisp/darwin-custom.el"))
 ((eq system-type 'gnu/linux)
  (setq custom-file "~/.emacs.d/lisp/linux-custom.el"
        org-mobile-directory "~/Dropbox/MobileOrg"
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "chromium")))
;  (require 'magit)
;  (require 'artlogic)
;  (require 'rvm)
;  (rvm-use-default)))

(provide 'machine-specific)
