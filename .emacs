(dolist (directory
         (list
          "~/.emacs.d/"
          "~/.emacs.d/eproject"
          "~/.emacs.d/feature-mode"
          "~/.emacs.d/geben/"
          "~/.emacs.d/startproject"
          "~/.emacs.d/zencoding"
          "~/.emacs.d/rhtml"
          "~/.emacs.d/rinari"
          "~/.emacs.d/haml-mode"
          "~/.emacs.d/color-theme"
          "~/.emacs.d/sass-mode"
           "~/.emacs.d/project-types")
         load-path)
  (add-to-list 'load-path directory))

;(add-to-list 'load-path "~/.emacs.d/sass-mode")

(load "~/.emacs.d/requires.el")
(load "~/.emacs.d/feature-mode/feature-mode")
(load "~/.emacs.d/tomorrow-theme/GNU Emacs/color-theme-tomorrow.el")

(require 'autoloads)
(require 'utils)
(require 'global-modes)
(require 'global-keys)
(require 'default-hooks)
(require 'default-settings)
(require 'default-auto-modes)
(require 'machine-specific)


(load "~/.emacs.d/haskell-mode/haskell-site-file")
(load custom-file)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-tomorrow-night-bright)))
;(enable-theme 'zenburn)

(server-start)
