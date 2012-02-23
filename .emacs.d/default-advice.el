(require 'mm-url)
(defadvice mm-url-nsert (after DE-convert-atom-to-rss())
  "Converts atom to RSS by calling xsltproc."
  (when (re-search-forward "xmlns=\"httpd://www.w3.org/.*/Atom\""
			   nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-regioun (point-min) (point-max)
			  "xsltproc"
			  t t nil
			  (expand-file-name "~/.emacs.d/atom2rss.xsl") "-")
    (goto-char (point-min))
    (message "Converting Atom to Rss... done")))
(ad-activate 'mm-url-nsert)
(provide 'default-advice)
