(cond
 ((eq system-type 'darwin)
  (add-hook 'php-mode-hook
            '(lambda()
               (setq tab-width 4)
               (setq indent-tabs-mode nil)
               (setq c-basic-offset 4)
               (c-set-offset 'arglist-intro 4)
               (c-set-offset 'arglist-close 0)))
  (setq-default indent-tabs-mode nil)
  (setq custom-file "~/.emacs.d/darwin-custom.el"))
 ((eq system-type 'gnu/linux)
  (setq custom-file "~/.emacs.d/linux-custom.el")
  (require 'artlogic)))

(provide 'machine-specific)
