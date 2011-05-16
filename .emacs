(dolist (directory
	 (list
	  "~/.emacs.d/"
	  "~/.emacs.d/eproject"
	  "~/.emacs.d/geben/"
	  "~/.emacs.d/startproject"
	  "~/.emacs.d/zencoding"
	  "~/.emacs.d/project-types")
	 load-path)
  (add-to-list 'load-path directory))

(load "~/.emacs.d/requires.el")
(require 'autoloads)
(require 'global-modes)
(require 'global-keys)
(require 'default-hooks)
(require 'default-settings)
(require 'default-auto-modes)
(require 'machine-specific)


(load "~/.emacs.d/haskell-mode/haskell-site-file")
(load custom-file)

(server-start)

