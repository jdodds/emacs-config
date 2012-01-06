(add-to-list 'load-path "~/.emacs.d")

(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-subdirs-to-load-path))

(load "~/.emacs.d/requires.el")
(load "~/.emacs.d/feature-mode/feature-mode")
(load "~/.emacs.d/tomorrow-theme/GNU Emacs/color-theme-tomorrow.el")

(require 'autoloads)
(require 'utils)
(require 'global-modes)
(require 'global-keys)
(require 'default-hooks)
(require 'default-settings)
(require 'org-settings)
(require 'default-auto-modes)
(require 'machine-specific)


(load "~/.emacs.d/haskell-mode/haskell-site-file")
(load custom-file)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-tomorrow-night-bright)))
;(enable-theme 'zenburn)
(require 'org-install)

(unless (boundp 'server-process)
  (server-start))
