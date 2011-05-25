(add-to-list 'load-path (expand-file-name
			 "../../lisp" (file-name-directory load-file-name)))
(require 'org-odt)
(org-odt-unit-test 'linger)
