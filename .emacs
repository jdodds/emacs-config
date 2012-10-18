(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-subdirs-to-load-path))

(package-initialize)

(load "~/.emacs.d/lisp/requires.el")

(require 'autoloads)
(require 'utils)
(require 'global-modes)
(require 'global-keys)
(require 'default-hooks)
(require 'default-settings)
(require 'default-advice)
(require 'org-settings)
(require 'default-auto-modes)
(require 'machine-specific)
(require 'sensitive)

(load "~/.emacs.d/haskell-mode/haskell-site-file")
(load custom-file)
(load-theme 'ir-black)
(php+-mode-setup)
