(cond
 ((eq system-type 'darwin)
  (setq-default indent-tabs-mode nil)
  (setq custom-file "~/.emacs.d/lisp/darwin-custom.el"))
 ((eq system-type 'gnu/linux)
  (setq custom-file "~/.emacs.d/lisp/linux-custom.el"
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "chromium")))

(provide 'machine-specific)
