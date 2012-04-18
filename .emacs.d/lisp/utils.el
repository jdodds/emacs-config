(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format "%A, %d. %B %Y")
	(system-time-locale "en_US"))
    (insert (format-time-string format))))

(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

(defun untabify-and-indent ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun pretty-print-xml ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region (point-min) (point-max)))
  (message "Aahhhh"))

(defun untabify-directory (dir)
  (interactive "DDirectory Root:")
  (dolist (file (directory-files dir t))
    (if (file-regular-p file)
        (with-temp-buffer
          (find-file file)
          (untabify-and-indent)
          (save-buffer))
      (message "Skipping directory %s." file))))

(defun create-tags (tag-dir proj-dir)
  (setq excludes "--exclude='TAGS*' --exclude='.#*' --exclude='.min.*'")
  "Create a ctags file for a given directory"
  (interactive "DSave In: \nDProject Directory")
  (shell-command
   (format "%s -f %s/TAGS.vim --fields=afKnsSzt %s -R %s" ctags-path tag-dir excludes proj-dir))
  (shell-command
   (format "%s -f %s/TAGS.emacs --fields=afKnsSzt %s -e -R %s" ctags-path tag-dir excludes proj-dir)))

(defun set-electrics ()
  "Set common-to-most-languages electric pairs"
  (setq parens-require-spaces nil)
  (local-set-key "\"" 'electric-pair)
  (local-set-key "\'" 'electric-pair)
  (local-set-key "("  'electric-pair)
  (local-set-key "["  'electric-pair)
  (local-set-key "{"  'electric-pair))

(defun electric-pair ()
  "Insert character pair without surrounding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun jump-to-window (buffer-name)
  (interactive "bEnter buffer to jump to: ")
  (let ((visible-buffers (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list)))
        window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have visible window" buffer-name)
      (setq window-of-buffer
            (delq nil (mapcar '(lambda (window) 
                                 (if (equal buffer-name (buffer-name (window-buffer window)))
                                     window nil)) (window-list))))
      (select-window (car window-of-buffer)))))

(defun macro-query (arg)
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
         (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                  (read-from-minibuffer prompt))))
    (unless (string= "" input) (insert input))))

;always reindent after yanking for most major modes
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (not (member major-mode '(text-mode fundamental-mode)))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))


(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking
   FILE-NAME. This is a replacement for
   `flymake-create-temp-inplace'. The difference is that it gives
   a file name in `temporary-file-directory' instead of the same
   directory as FILE-NAME.

   For the use of PREFIX see that function.

   Note that not making the temporary file in another directory
   \(like here) will not if the file you are checking depends on
   relative paths to other files \(for the type of checks flymake
   makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun flymake-php-init ()
  (let* ((temp
	  (flymake-init-create-temp-buffer-copy 'flymake-create-temp-intemp))
	 (local
	  (file-relative-name temp (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local "-l"))))

(add-to-list
 'flymake-err-line-patterns
 '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))

(add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
(provide 'utils)
;;; utils.el ends here
