(cond
 ((eq system-type 'darwin)
  (setq-default indent-tabs-mode nil)
  (setq custom-file "~/.emacs.d/darwin-custom.el"))
 ((eq system-type 'gnu/linux)
  (setq custom-file "~/.emacs.d/linux-custom.el")
  (require 'artlogic)))

(provide 'machine-specific)
