(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/eproject")
(add-to-list 'load-path "~/.emacs.d/geben/")
(add-to-list 'load-path "~/.emacs.d/startproject")
(add-to-list 'load-path "~/.emacs.d/project-types")

(require 'requires)
(require 'autoloads)
(require 'global-modes)
(require 'global-keys)
(require 'default-settings)
(require 'default-auto-modes)
(require 'machine-specific)

(load "~/.emacs.d/haskell-mode/haskell-site-file")
(load custom-file)

(server-start)

