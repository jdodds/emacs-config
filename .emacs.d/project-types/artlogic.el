(require 'startproject)

(startproject-add-commands
 "artlogic"
 "mkdir trunk"
 "mkdir trunk/Documents"
 "mkdir trunk/AlDocs"
 "touch trunk/AlDocs/ReadMe.txt"
 "echo '// Copyright (c)' $(date +%Y) 'Art & Logic Software Development, Inc' >> trunk/AlDocs/ReadMe.txt"
 "echo '// $id$' >> trunk/AlDocs/ReadMe.txt")
