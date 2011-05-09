(require 'startproject)

(setq dir-local-def
      (prin1-to-string
       '((nil \. ((indent-tabs-mode \. nil)
		(tab-width \. 3)))
	  (espresso-mode \. ((espresso-indent-level  \. 3)))
	  (js-mode \. ((js-indent-level \. 3))))
       t))

(startproject-add-commands
 "artlogic"
 "mkdir trunk"
 "mkdir trunk/Documents"
 "mkdir trunk/AlDocs"
 "touch trunk/AlDocs/ReadMe.txt"
 "echo '// Copyright (c)' $(date +%Y) 'Art & Logic Software Development, Inc' >> trunk/AlDocs/ReadMe.txt"
 (concat "echo '" dir-local-def "' > .dir-locals.el")
 "echo '// $id$' >> trunk/AlDocs/ReadMe.txt")

(provide 'artlogic)
