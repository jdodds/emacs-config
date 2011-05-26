;;; org-html.el --- HTML export for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;   Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 7.5
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;;; Code:

(require 'org-exp)
(require 'format-spec)

(eval-when-compile (require 'cl) (require 'table))

(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))

(defgroup org-export-html nil
  "Options specific for HTML export of Org-mode files."
  :tag "Org Export HTML"
  :group 'org-export)

(defcustom org-export-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-coding-system nil
  "Coding system for HTML export, defaults to `buffer-file-coding-system'."
  :group 'org-export-html
  :type 'coding-system)

(defcustom org-export-html-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
    ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations."
  :group 'org-export-html
  :type '(choice
	  (string :tag "Single declaration")
	  (repeat :tag "Dependent on extension"
		  (cons (string :tag "Extension")
			(string :tag "Declaration")))))

(defcustom org-export-html-style-include-scripts t
  "Non-nil means include the JavaScript snippets in exported HTML files.
The actual script is defined in `org-export-html-scripts' and should
not be modified."
  :group 'org-export-html
  :type 'boolean)

(defconst org-export-html-scripts
"<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = \"code-highlighted\";
     elem.className   = \"code-highlighted\";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>"
"Basic JavaScript that is needed by HTML files produced by Org-mode.")

(defconst org-export-html-style-default
"<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  html { font-family: Times, serif; font-size: 12pt; }
  .title  { text-align: center; }
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color: #add8e6; font-weight:normal }
  .target { }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .right  {margin-left:auto; margin-right:0px;  text-align:right;}
  .left   {margin-left:0px;  margin-right:auto; text-align:left;}
  .center {margin-left:auto; margin-right:auto; text-align:center;}
  p.verse { margin-left: 3% }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
        font-size: 90%;
        overflow:auto;
  }
  table { border-collapse: collapse; }
  td, th { vertical-align: top;  }
  th.right  { text-align:center;  }
  th.left   { text-align:center;   }
  th.center { text-align:center; }
  td.right  { text-align:right;  }
  td.left   { text-align:left;   }
  td.center { text-align:center; }
  dt { font-weight: bold; }
  div.figure { padding: 0.5em; }
  div.figure p { text-align: center; }
  textarea { overflow-x: auto; }
  .linenr { font-size:smaller }
  .code-highlighted {background-color:#ffff00;}
  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }
  /*]]>*/-->
</style>"
  "The default style specification for exported HTML files.
Please use the variables `org-export-html-style' and
`org-export-html-style-extra' to add to this style.  If you wish to not
have the default style included, customize the variable
`org-export-html-style-include-default'.")

(defcustom org-export-html-style-include-default t
  "Non-nil means include the default style in exported HTML files.
The actual style is defined in `org-export-html-style-default' and should
not be modified.  Use the variables `org-export-html-style' to add
your own style information."
  :group 'org-export-html
  :type 'boolean)
;;;###autoload
(put 'org-export-html-style-include-default 'safe-local-variable 'booleanp)

(defcustom org-export-html-style ""
  "Org-wide style definitions for exported HTML files.

This variable needs to contain the full HTML structure to provide a style,
including the surrounding HTML tags.  If you set the value of this variable,
you should consider to include definitions for the following classes:
 title, todo, done, timestamp, timestamp-kwd, tag, target.

For example, a valid value would be:

   <style type=\"text/css\">
    <![CDATA[
       p { font-weight: normal; color: gray; }
       h1 { color: black; }
      .title { text-align: center; }
      .todo, .timestamp-kwd { color: red; }
      .done { color: green; }
    ]]>
   </style>

If you'd like to refer to en external style file, use something like

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to add arbitrary text to the header.
See also the variable `org-export-html-style-extra'."
  :group 'org-export-html
  :type 'string)
;;;###autoload
(put 'org-export-html-style 'safe-local-variable 'stringp)

(defcustom org-export-html-style-extra ""
  "Additional style information for HTML export.
The value of this variable is inserted into the HTML buffer right after
the value of `org-export-html-style'.  Use this variable for per-file
settings of style information, and do not forget to surround the style
settings with <style>...</style> tags."
  :group 'org-export-html
  :type 'string)
;;;###autoload
(put 'org-export-html-style-extra 'safe-local-variable 'stringp)

(defcustom org-export-html-mathjax-options
  '((path  "http://orgmode.org/mathjax/MathJax.js")
    (scale "100")
    (align "center")
    (indent "2em")
    (mathml nil))
  "Options for MathJax setup.

path        The path where to find MathJax
scale       Scaling for the HTML-CSS backend, usually between 100 and 133
align       How to align display math: left, center, or right
indent      If align is not center, how far from the left/right side?
mathml      Should a MathML player be used if available?
            This is faster and reduces bandwidth use, but currently
            sometimes has lower spacing quality.  Therefore, the default is
            nil.  When browsers get better, this switch can be flipped.

You can also customize this for each buffer, using something like

#+MATHJAX: scale:\"133\" align:\"right\" mathml:t path:\"/MathJax/\""
  :group 'org-export-html
  :type '(list :greedy t
	      (list :tag "path   (the path from where to load MathJax.js)"
		    (const :format "       " path) (string))
	      (list :tag "scale  (scaling for the displayed math)"
		    (const :format "       " scale) (string))
	      (list :tag "align  (alignment of displayed equations)"
		    (const :format "       " align) (string))
	      (list :tag "indent (indentation with left or right alignment)"
		    (const :format "       " indent) (string))
	      (list :tag "mathml (should MathML display be used is possible)"
		    (const :format "       " mathml) (boolean))))

(defun org-export-html-mathjax-config (template options in-buffer)
  "Insert the user setup into the matchjax template."
  (let (name val (yes "   ") (no "// ") x)
    (mapc
     (lambda (e)
       (setq name (car e) val (nth 1 e))
       (if (string-match (concat "\\<" (symbol-name name) ":") in-buffer)
	   (setq val (car (read-from-string
			   (substring in-buffer (match-end 0))))))
       (if (not (stringp val)) (setq val (format "%s" val)))
       (if (string-match (concat "%" (upcase (symbol-name name))) template)
	   (setq template (replace-match val t t template))))
     options)
    (setq val (nth 1 (assq 'mathml options)))
    (if (string-match (concat "\\<mathml:") in-buffer)
	(setq val (car (read-from-string
			(substring in-buffer (match-end 0))))))
    ;; Exchange prefixes depending on mathml setting
    (if (not val) (setq x yes yes no no x))
    ;; Replace cookies to turn on or off the config/jax lines
    (if (string-match ":MMLYES:" template)
	(setq template (replace-match yes t t template)))
    (if (string-match ":MMLNO:" template)
	(setq template (replace-match no t t template)))
    ;; Return the modified template
    template))

(defcustom org-export-html-mathjax-template
  "<script type=\"text/javascript\" src=\"%PATH\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        // Only one of the two following lines, depending on user settings
        // First allows browser-native MathML display, second forces HTML/CSS
        :MMLYES: config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],
        :MMLNO: jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>"
  "The MathJax setup for XHTML files."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-tag-class-prefix ""
  "Prefix to class names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-todo-kwd-class-prefix ""
  "Prefix to class names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-preamble t
  "Non-nil means insert a preamble in HTML export.

When `t', insert a string as defined by one of the formatting
strings in `org-export-html-preamble-format'.  When set to a
string, this string overrides `org-export-html-preamble-format'.
When set to a function, apply this function and insert the
returned string.  The function takes the property list of export
options as its only argument.

Setting :html-preamble in publishing projects will take
precedence over this variable."
  :group 'org-export-html
  :type '(choice (const :tag "No preamble" nil)
		 (const :tag "Default preamble" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-export-html-preamble-format
  '(("en" "<h1 class=\"title\">%t</h1>"))
  "The format for the HTML preamble.

%t stands for the title.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-postamble 'auto
  "Non-nil means insert a postamble in HTML export.

When `t', insert a string as defined by the formatting string in
`org-export-html-postamble-format'.  When set to a string, this
string overrides `org-export-html-postamble-format'.  When set to
'auto, discard `org-export-html-postamble-format' and honor
`org-export-author/email/creator-info' variables.  When set to a
function, apply this function and insert the returned string.
The function takes the property list of export options as its
only argument.

Setting :html-postamble in publishing projects will take
precedence over this variable."
  :group 'org-export-html
  :type '(choice (const :tag "No postamble" nil)
		 (const :tag "Auto preamble" 'auto)
		 (const :tag "Default formatting string" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-export-html-postamble-format
  '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">Generated by %c</p>
<p class=\"xhtml-validation\">%v</p>
"))
  "The format for the HTML postamble.

%a stands for the author.
%e stands for the email(s).
%d stands for the date.
%c will be replaced by information about Org/Emacs.
%v will be replaced by `org-export-html-validation-link'.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-home/up-format
  "<div id=\"org-div-home-and-up\" style=\"text-align:right;font-size:70%%;white-space:nowrap;\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `org-export-html-link-up' and
`org-export-html-link-home' are empty, the entire snippet will be
ignored."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-link-org-files-as-html t
  "Non-nil means make file links to `file.org' point to `file.html'.
When org-mode is exporting an org-mode file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked org-mode file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-export-html-inline-images 'maybe
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.  If this option is `maybe', then images in links with
an empty description will be inlined, while images with a description will
be linked only."
  :group 'org-export-html
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When there is no description" maybe)))

(defcustom org-export-html-inline-image-extensions
  '("png" "jpeg" "jpg" "gif" "svg")
  "Extensions of image files that can be inlined into HTML."
  :group 'org-export-html
  :type '(repeat (string :tag "Extension")))

(defcustom org-export-html-table-tag
  "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">"
  "The HTML tag that is used to start a table.
This must be a <table> tag, but you may change the options like
borders and spacing."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
  "The opening tag for table header fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-export-html-table-use-header-tags-for-first-column'.
See also the variable `org-export-html-table-align-individual-fields'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-data-tags '("<td%s>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-export-html-table-align-individual-fields'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-row-tags '("<tr>" . "</tr>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
Instead of strings, these can be Lisp forms that will be evaluated
for each row in order to construct the table row tags.  During evaluation,
the variable `head' will be true when this is a header line, nil when this
is a body line.  And the variable `nline' will contain the line number,
starting from 1 in the first header line.  For example

  (setq org-export-table-row-tags
        (cons '(if head
                   \"<tr>\"
                 (if (= (mod nline 2) 1)
                     \"<tr class=\\\"tr-odd\\\">\"
                   \"<tr class=\\\"tr-even\\\">\"))
              \"</tr>\"))

will give even lines the class \"tr-even\" and odd lines the class \"tr-odd\"."
  :group 'org-export-tables
  :type '(cons
	  (choice :tag "Opening tag"
		  (string :tag "Specify")
		  (sexp))
	  (choice :tag "Closing tag"
		  (string :tag "Specify")
		  (sexp))))

(defcustom org-export-html-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-html-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-html-validation-link
  "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"
  "Link to HTML validation service."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-with-timestamp nil
  "If non-nil, write timestamp into the exported HTML text.
If non-nil, write `org-export-html-html-helper-timestamp' into the
exported HTML text.  Otherwise, the buffer will just be saved to
a file."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-export-html-html-helper-timestamp
  "<br/><br/><hr/><p><!-- hhmts start --> <!-- hhmts end --></p>\n"
  "The HTML tag used as timestamp delimiter for HTML-helper-mode."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-html-protect'."
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "HTML equivalent"))))

(defgroup org-export-htmlize nil
  "Options for processing examples with htmlize.el."
  :tag "Org Export Htmlize"
  :group 'org-export-html)

(defcustom org-export-htmlize-output-type 'inline-css
  "Output type to be used by htmlize when formatting code snippets.
Choices are `css', to export the CSS selectors only, or `inline-css', to
export the CSS attribute values inline in the HTML.  We use as default
`inline-css', in order to make the resulting HTML self-containing.

However, this will fail when using Emacs in batch mode for export, because
then no rich font definitions are in place.  It will also not be good if
people with different Emacs setup contribute HTML files to a website,
because the fonts will represent the individual setups.  In these cases,
it is much better to let Org/Htmlize assign classes only, and to use
a style file to define the look of these classes.
To get a start for your css file, start Emacs session and make sure that
all the faces you are interested in are defined, for example by loading files
in all modes you want.  Then, use the command
\\[org-export-htmlize-generate-css] to extract class definitions."
  :group 'org-export-htmlize
  :type '(choice (const css) (const inline-css)))

(defcustom org-export-htmlize-css-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications."
  :group 'org-export-htmlize
  :type 'string)

(defcustom org-export-htmlized-org-css-url nil
  "URL pointing to a CSS file defining text colors for htmlized Emacs buffers.
Normally when creating an htmlized version of an Org buffer, htmlize will
create CSS to define the font colors.  However, this does not work when
converting in batch mode, and it also can look bad if different people
with different fontification setup work on the same website.
When this variable is non-nil, creating an htmlized version of an Org buffer
using `org-export-as-org' will remove the internal CSS section and replace it
with a link to this URL."
  :group 'org-export-htmlize
  :type '(choice
	  (const :tag "Keep internal css" nil)
	  (string :tag "URL or local href")))

;;; Hooks

(defvar org-export-html-after-blockquotes-hook nil
  "Hook run during HTML export, after blockquote, verse, center are done.")

(defvar org-export-html-final-hook nil
  "Hook run at the end of HTML export, in the new buffer.")

;;; HTML export

(defun org-export-html-preprocess (parameters)
  "Convert LaTeX fragments to images."
  (when (and org-current-export-file
	     (plist-get parameters :LaTeX-fragments))
    (org-format-latex
     (concat "ltxpng/" (file-name-sans-extension
			(file-name-nondirectory
			 org-current-export-file)))
     org-current-export-dir nil "Creating LaTeX image %s"
     nil nil
     (cond
      ((eq (plist-get parameters :LaTeX-fragments) 'verbatim) 'verbatim)
      ((eq (plist-get parameters :LaTeX-fragments) 'mathjax ) 'mathjax)
      ((eq (plist-get parameters :LaTeX-fragments) t        ) 'mathjax)
      ((eq (plist-get parameters :LaTeX-fragments) 'dvipng  ) 'dvipng)
      (t nil))))
  (goto-char (point-min))
  (let (label l1)
    (while (re-search-forward "\\\\ref{\\([^{}\n]+\\)}" nil t)
      (org-if-unprotected-at (match-beginning 1)
	(setq label (match-string 1))
	(save-match-data
	  (if (string-match "\\`[a-z]\\{1,10\\}:\\(.+\\)" label)
	      (setq l1 (substring label (match-beginning 1)))
	    (setq l1 label)))
	(replace-match (format "[[#%s][%s]]" label l1) t t)))))

;;;###autoload
(defun org-export-as-and-open (backend arg)
  "Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "Mbackend: \nP")
  (org-export-as backend arg 'hidden)
  (message "Opening file %s" (buffer-file-name))
  (org-open-file buffer-file-name)
  (when org-export-kill-product-buffer-when-displayed
    (kill-buffer (current-buffer))))

;;;###autoload
(defun org-export-as-batch (backend)
  "Call the function `org-export-as-html'.
This function can be used in batch processing as:
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-html-batch"
  (org-export-as backend org-export-headline-levels 'hidden))

;;;###autoload
(defun org-export-as-to-buffer (backend arg)
  "Call `org-export-as-html` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-html'."
  (interactive "Mbackend: \nP")
  (let ((tempbuf (format "*Org %s Export*" (upcase backend))))
      (org-export-as backend arg nil nil tempbuf)
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window tempbuf))))

;;;###autoload
(defun org-replace-region-by (backend beg end)
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an HTML buffer and then use this
command to convert it."
  (interactive "Mbackend: \nr")
  (let (reg backend-string buf pop-up-frames)
    (save-window-excursion
      (if (org-mode-p)
	  (setq backend-string (org-export-region-as
				backend beg end t 'string))
	(setq reg (buffer-substring beg end)
	      buf (get-buffer-create "*Org tmp*"))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert reg)
	  (org-mode)
	  (setq backend-string (org-export-region-as
				backend (point-min) (point-max) t 'string)))
	(kill-buffer buf)))
    (delete-region beg end)
    (insert backend-string)))

;;;###autoload
(defun org-export-region-as (backend beg end &optional body-only buffer)
  "Convert region from BEG to END in org-mode buffer to HTML.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted HTML.  If BUFFER is the symbol `string', return the
produced HTML as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq html (org-export-region-as-html beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (interactive "Mbackend: \nr\nP")
  (when (interactive-p)
    (setq buffer (format "*Org %s Export*" (upcase backend))))
  (let ((transient-mark-mode t) (zmacs-regions t)
	ext-plist rtn)
    (setq ext-plist (plist-put ext-plist :ignore-subtree-p t))
    (goto-char end)
    (set-mark (point)) ;; to activate the region
    (goto-char beg)
    (setq rtn (org-export-as backend nil nil ext-plist buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (interactive-p) (bufferp rtn))
	(switch-to-buffer-other-window rtn)
      rtn)))

(defvar html-table-tag nil) ; dynamically scoped into this.
(defvar org-par-open nil)

;;; org-html-cvt-link-fn
(defconst org-html-cvt-link-fn
   nil
   "Function to convert link URLs to exportable URLs.
Takes two arguments, TYPE and PATH.
Returns exportable url as (TYPE PATH), or nil to signal that it
didn't handle this case.
Intended to be locally bound around a call to `org-export-as-html'." )

(defun org-html-cvt-org-as-html (opt-plist type path)
   "Convert an org filename to an equivalent html filename.
If TYPE is not file, just return `nil'.
See variable `org-export-html-link-org-files-as-html'"

   (save-match-data
      (and
	 org-export-html-link-org-files-as-html
	 (string= type "file")
	 (string-match "\\.org$" path)
	 (progn
	    (list
	       "file"
	       (concat
		  (substring path 0 (match-beginning 0))
		  "."
		  (plist-get opt-plist :html-extension)))))))


;;; org-html-should-inline-p
(defun org-html-should-inline-p (filename descp)
   "Return non-nil if link FILENAME should be inlined.
The decision to inline the FILENAME link is based on the current
settings.  DESCP is the boolean of whether there was a link
description.  See variables `org-export-html-inline-images' and
`org-export-html-inline-image-extensions'."
   (declare (special
	     org-export-html-inline-images
	     org-export-html-inline-image-extensions))
   (and (or (eq t org-export-html-inline-images)
	    (and org-export-html-inline-images (not descp)))
	(org-file-image-p
	 filename org-export-html-inline-image-extensions)))

;;; org-html-format-org-link
(defun org-html-format-org-link (opt-plist type-1 path fragment desc attr
					   descp)
  "Make an HTML link.
OPT-PLIST is an options list.
TYPE is the device-type of the link (THIS://foo.html)
PATH is the path of the link (http://THIS#locationx)
FRAGMENT is the fragment part of the link, if any (foo.html#THIS)
DESC is the link description, if any.
ATTR is a string of other attributes of the a element.
MAY-INLINE-P allows inlining it as an image."
  (declare (special org-par-open))
  (when (string= type-1 "coderef")
    (setq attr
	  (format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, '%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
		  fragment fragment)))
  (save-match-data
    (let* ((may-inline-p
	    (and (member type-1 '("http" "https" "file"))
		 (org-html-should-inline-p path descp)
		 (not fragment)))
	   (type (if (equal type-1 "id") "file" type-1))
	   (filename path)
	   ;;First pass.  Just sanity stuff.
	   (components-1
	    (cond
	     ((string= type "file")
	      (list
	       type
	       ;;Substitute just if original path was absolute.
	       ;;(Otherwise path must remain relative)
	       (if (file-name-absolute-p path)
		   (concat "file://" (expand-file-name path))
		 path)))
	     ((string= type "")
	      (list nil path))
	     (t (list type path))))

	   ;;Second pass.  Components converted so they can refer
	   ;;to a remote site.
	   (components-2
	    (or
	     (and org-html-cvt-link-fn
		  (apply org-html-cvt-link-fn
			 opt-plist components-1))
	     (apply #'org-html-cvt-org-as-html
		    opt-plist components-1)
	     components-1))
	   (type    (first  components-2))
	   (thefile (second components-2)))


      ;;Third pass.  Build final link except for leading type
      ;;spec.
      (cond
       ((or
	 (not type)
	 (string= type "http")
	 (string= type "https")
	 (string= type "file")
	 (string= type "coderef"))
	(if fragment
	    (setq thefile (concat thefile "#" fragment))))

       (t))

      ;;Final URL-build, for all types.
      (setq thefile
	    (let
		((str (org-export-html-format-href thefile)))
	      (if (and type (not (or (string= "file" type)
				     (string= "coderef" type))))
		  (concat type ":" str)
		str)))

      (if may-inline-p
	  (org-export-html-format-image thefile)
	(org-parse-format
	 'LINK (org-export-html-format-desc desc) thefile attr)))))

(defun org-html-format-inline-image (desc)
  ;; FIXME: alt text missing here?
  (org-html-format-tags "<img src=\"%s\" alt=\"\"/>" "" desc))

(defvar org-parse-link-description-is-image)
(defun org-parse-format-org-link (line opt-plist)
  "Return LINE with markup of Org mode links.
OPT-PLIST is the export options list."
  (let ((start 0)
	(current-dir (if buffer-file-name
			 (file-name-directory buffer-file-name)
		       default-directory))
	(link-validate (plist-get opt-plist :link-validation-function))
	type id-file fnc
	rpl path attr desc descp desc1 desc2 link
	org-parse-link-description-is-image)
    (while (string-match org-bracket-link-analytic-regexp++ line start)
      (setq org-parse-link-description-is-image nil)
      (setq start (match-beginning 0))
      (setq path (save-match-data (org-link-unescape
				   (match-string 3 line))))
      (setq type (cond
		  ((match-end 2) (match-string 2 line))
		  ((save-match-data
		     (or (file-name-absolute-p path)
			 (string-match "^\\.\\.?/" path)))
		   "file")
		  (t "internal")))
      (setq path (org-extract-attributes (org-link-unescape path)))
      (setq attr (get-text-property 0 'org-attributes path))
      (setq desc1 (if (match-end 5) (match-string 5 line))
	    desc2 (if (match-end 2) (concat type ":" path) path)
	    descp (and desc1 (not (equal desc1 desc2)))
	    desc (or desc1 desc2))
      ;; Make an image out of the description if that is so wanted
      (when (and descp (org-file-image-p
			desc org-export-html-inline-image-extensions))
	(setq org-parse-link-description-is-image t)
	(save-match-data
	  (if (string-match "^file:" desc)
	      (setq desc (substring desc (match-end 0)))))
	(save-match-data
	  (setq desc (org-add-props
			 (org-parse-format 'INLINE-IMAGE desc)
			 '(org-protected t)))))
      (cond
       ((equal type "internal")
	(let
	    ((frag-0
	      (if (= (string-to-char path) ?#)
		  (substring path 1)
		path)))
	  (setq rpl
		(org-parse-format
		 'ORG-LINK opt-plist "" "" (org-solidify-link-text
					    (save-match-data
					      (org-link-unescape frag-0))
					    nil) desc attr descp))))
       ((and (equal type "id")
	     (setq id-file (org-id-find-id-file path)))
	;; This is an id: link to another file (if it was the same file,
	;; it would have become an internal link...)
	(save-match-data
	  (setq id-file (file-relative-name
			 id-file
			 (file-name-directory org-current-export-file)))
	  (setq rpl
		(org-parse-format
		 'ORG-LINK opt-plist type id-file
		 (concat (if (org-uuidgen-p path) "ID-") path)
		 desc attr descp))))
       ((member type '("http" "https"))
	;; standard URL, can inline as image
	(setq rpl
	      (org-parse-format
	       'ORG-LINK opt-plist type path nil desc attr descp)))
       ((member type '("ftp" "mailto" "news"))
	;; standard URL, can't inline as image
	(setq rpl
	      (org-parse-format
	       'ORG-LINK opt-plist type path nil desc attr descp)))

       ((string= type "coderef")
	(setq rpl
	      (org-parse-format
	       'ORG-LINK opt-plist type "" (format "coderef-%s" path)
	       (format
		(org-export-get-coderef-format
		 path
		 (and descp desc))
		(cdr (assoc path org-export-code-refs))) nil descp)))

       ((functionp (setq fnc (nth 2 (assoc type org-link-protocols))))
	;; The link protocol has a function for format the link
	(setq rpl
	      (save-match-data
		(funcall fnc (org-link-unescape path) desc1 'html))))

       ((string= type "file")
	;; FILE link
	(save-match-data
	  (let*
	      ((components
		(if
		    (string-match "::\\(.*\\)" path)
		    (list
		     (replace-match "" t nil path)
		     (match-string 1 path))
		  (list path nil)))

	       ;;The proper path, without a fragment
	       (path-1
		(first components))

	       ;;The raw fragment
	       (fragment-0
		(second components))

	       ;;Check the fragment.  If it can't be used as
	       ;;target fragment we'll pass nil instead.
	       (fragment-1
		(if
		    (and fragment-0
			 (not (string-match "^[0-9]*$" fragment-0))
			 (not (string-match "^\\*" fragment-0))
			 (not (string-match "^/.*/$" fragment-0)))
		    (org-solidify-link-text
		     (org-link-unescape fragment-0))
		  nil))
	       (desc-2
		;;Description minus "file:" and ".org"
		(if (string-match "^file:" desc)
		    (let
			((desc-1 (replace-match "" t t desc)))
		      (if (string-match "\\.org$" desc-1)
			  (replace-match "" t t desc-1)
			desc-1))
		  desc)))

	    (setq rpl
		  (if
		      (and
		       (functionp link-validate)
		       (not (funcall link-validate path-1 current-dir)))
		      desc
		    (org-parse-format
		     'ORG-LINK opt-plist "file" path-1 fragment-1
		     desc-2 attr descp))))))

       (t
	;; just publish the path, as default
	(setq rpl (concat "<i>&lt;" type ":"
			  (save-match-data (org-link-unescape path))
			  "&gt;</i>"))))
      (setq line (replace-match rpl t t line)
	    start (+ start (length rpl))))
    line))

(defmacro with-org-parse-preserve-paragraph-state (&rest body)
  `(let ((org-do-open-par org-par-open))
     (org-parse-end-paragraph)
     ,@body
     (when org-do-open-par
       (org-parse-begin-paragraph))))

(defvar org-export-backends-alist
  '((html . org-html-get)
    (odt . org-odt-get)))

(defun org-export-as (backend arg &optional hidden ext-plist
			      to-buffer body-only pub-dir)
  (interactive "Mbackend: \nP")
  (let* ((backend-desc (assoc-string backend org-export-backends-alist t))
	 (backend-get (cdr backend-desc))
	 (backend-get (and (functionp backend-get) backend-get)))
    (unless backend-get
      (error "Don't know how to export to backend %s" backend))
    (run-hooks 'org-export-first-hook)
    (let* ((org-parse-get-callback backend-get))
      (org-do-export arg hidden ext-plist to-buffer body-only pub-dir))))

(defvar org-html-insert-tag-with-newlines 'both)

;; Following variables are let-bound while `org-do-export' is in
;; progress
(defvar org-html-dyn-first-heading-pos)
(defvar org-parse-table-of-contents)
(defvar org-parse-entity-control-callbacks-alist)
(defvar org-parse-entity-format-callbacks-alist)
(defvar org-parse-get-callback)
(defvar org-parse-backend)
(defvar org-parse-body-only)
(defvar org-parse-to-buffer)
(defun org-do-export (arg &optional hidden ext-plist
			  to-buffer body-only pub-dir)
  "Export the outline as a pretty HTML file."
  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
	     (not buffer-file-name))
    (if (buffer-base-buffer)
	(org-set-local 'buffer-file-name
		       (with-current-buffer (buffer-base-buffer)
			 buffer-file-name))
      (error "Need a file name to be able to export")))

  (message "Exporting...")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (setq-default org-deadline-line-regexp org-deadline-line-regexp)
  (setq-default org-done-keywords org-done-keywords)
  (setq-default org-maybe-keyword-time-regexp org-maybe-keyword-time-regexp)
  (let* (org-html-protect
	 org-par-open
	 org-parse-outline-text-open
	 (org-parse-latex-fragment-fallback ; currently used only by
					    ; odt exporter
	  (or (ignore-errors (org-parse-get 'LATEX-FRAGMENT-FALLBACK))
	      (if (and (org-check-external-command "latex" "" t)
		       (org-check-external-command "dvipng" "" t))
		  'dvipng
		'verbatim)))
	 (org-html-insert-tag-with-newlines 'both)
	 (org-parse-to-buffer to-buffer)
	 (org-parse-body-only body-only)
	 (org-export-html-special-string-regexps ; FIXME: genericize this
	  (org-parse-get 'SPECIAL-STRING-REGEXPS))
	 (org-parse-entity-control-callbacks-alist
	  (org-parse-get 'ENTITY-CONTROL))
	 (org-parse-entity-format-callbacks-alist
	  (org-parse-get 'ENTITY-FORMAT))
	 (opt-plist
	  (org-export-process-option-filters
	   (org-combine-plists (org-default-export-plist)
			       ext-plist
			       (org-infile-export-plist))))
	 (body-only (or body-only (plist-get opt-plist :body-only)))
	 valid org-html-dyn-first-heading-pos
	 (odd org-odd-levels-only)
	 (region-p (org-region-active-p))
	 (rbeg (and region-p (region-beginning)))
	 (rend (and region-p (region-end)))
	 (subtree-p
	  (if (plist-get opt-plist :ignore-subtree-p)
	      nil
	    (when region-p
	      (save-excursion
		(goto-char rbeg)
		(and (org-at-heading-p)
		     (>= (org-end-of-subtree t t) rend))))))
	 (level-offset (if subtree-p
			   (save-excursion
			     (goto-char rbeg)
			     (+ (funcall outline-level)
				(if org-odd-levels-only 1 0)))
			 0))
	 (opt-plist (setq org-export-opt-plist
			  (if subtree-p
			      (org-export-add-subtree-options opt-plist rbeg)
			    opt-plist)))
	 ;; The following two are dynamically scoped into other
	 ;; routines below.
	 (org-current-export-dir
	  (or pub-dir (org-parse-get 'EXPORT-DIR opt-plist)))
	 (org-current-export-file buffer-file-name)
	 (level 0) (line "") (origline "") txt todo
	 (umax nil)
	 (umax-toc nil)
	 (filename (if to-buffer nil
		     (expand-file-name
		      (concat
		       (file-name-sans-extension
			(or (and subtree-p
				 (org-entry-get (region-beginning)
						"EXPORT_FILE_NAME" t))
			    (file-name-nondirectory buffer-file-name)))
		       "." (org-parse-get 'FILE-NAME-EXTENSION opt-plist))
		      (file-name-as-directory
		       (or pub-dir (org-parse-get 'EXPORT-DIR opt-plist))))))
	 (current-dir (if buffer-file-name
			  (file-name-directory buffer-file-name)
			default-directory))
	 (buffer (if to-buffer
		     (cond
		      ((eq to-buffer 'string)
		       (get-buffer-create (org-parse-get 'EXPORT-BUFFER-NAME)))
		      (t (get-buffer-create to-buffer)))
		   (find-file-noselect
		    (or (let ((f (org-parse-get 'INIT-METHOD)))
			  (and f (functionp f) (funcall f filename)))
			filename))))
	 (org-levels-open (make-vector org-level-max nil))
	 (date (plist-get opt-plist :date))
	 (date (cond
		((and date (string-match "%" date))
		 (format-time-string date))
		(date date)
		(t (format-time-string "%Y-%m-%d %T %Z"))))
	 (dummy (setq opt-plist (plist-put opt-plist :effective-date date)))
	 (title       (org-html-expand
		       (or (and subtree-p (org-export-get-title-from-subtree))
			   (plist-get opt-plist :title)
			   (and (not body-only)
				(not
				 (plist-get opt-plist :skip-before-1st-heading))
				(org-export-grab-title-from-buffer))
			   (and buffer-file-name
				(file-name-sans-extension
				 (file-name-nondirectory buffer-file-name)))
			   "UNTITLED")))
	 (dummy (setq opt-plist (plist-put opt-plist :title title)))
	 (html-table-tag (plist-get opt-plist :html-table-tag))
	 (quote-re0   (concat "^[ \t]*" org-quote-string "\\>"))
	 (quote-re    (concat "^\\(\\*+\\)\\([ \t]+" org-quote-string "\\>\\)"))
	 (org-parse-dyn-current-environment nil)
	 ;; Get the language-dependent settings
	 (lang-words (or (assoc (plist-get opt-plist :language)
				org-export-language-setup)
			 (assoc "en" org-export-language-setup)))
	 (dummy (setq opt-plist (plist-put opt-plist :lang-words lang-words)))
	 (head-count  0) cnt
	 (start       0)
	 (coding-system-for-write (org-html-get-coding-system-for-write))
	 (save-buffer-coding-system (org-html-get-coding-system-for-save))
	 (region
	  (buffer-substring
	   (if region-p (region-beginning) (point-min))
	   (if region-p (region-end) (point-max))))
	 (org-export-have-math nil)
	 (insertion-point-for-normalized-footnotes 'point-min)
	 (org-parse-backend (org-parse-get 'BACKEND))
	 (lines
	  (org-split-string
	   (org-export-preprocess-string
	    region
	    :emph-multiline t
	    :for-backend org-parse-backend
	    :skip-before-1st-heading
	    (plist-get opt-plist :skip-before-1st-heading)
	    :drawers (plist-get opt-plist :drawers)
	    :todo-keywords (plist-get opt-plist :todo-keywords)
	    :tasks (plist-get opt-plist :tasks)
	    :tags (plist-get opt-plist :tags)
	    :priority (plist-get opt-plist :priority)
	    :footnotes (plist-get opt-plist :footnotes)
	    :timestamps (plist-get opt-plist :timestamps)
	    :archived-trees
	    (plist-get opt-plist :archived-trees)
	    :select-tags (plist-get opt-plist :select-tags)
	    :exclude-tags (plist-get opt-plist :exclude-tags)
	    :add-text
	    (plist-get opt-plist :text)
	    :LaTeX-fragments
	    (plist-get opt-plist :LaTeX-fragments))
	   "[\r\n]"))
	 table-open
	 table-buffer table-orig-buffer
	 ind
	 rpl path attr desc descp desc1 desc2 link
	 snumber fnc
	 footnotes footref-seen
	 org-html-output-buffer
	 org-html-footnote-definitions
	 org-html-footnote-number
	 org-html-footnote-buffer
	 org-parse-table-of-contents
	 href
	 )

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (message "Exporting...")
    (org-init-section-numbers)

    ;; Switch to the output buffer
    (setq org-html-output-buffer buffer)
    (set-buffer org-html-output-buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (org-install-letbind)

    (and (fboundp 'set-buffer-file-coding-system)
	 (set-buffer-file-coding-system coding-system-for-write))

    (let ((case-fold-search nil)
	  (org-odd-levels-only odd))
      ;; create local variables for all options, to make sure all called
      ;; functions get the correct information
      (mapc (lambda (x)
	      (set (make-local-variable (nth 2 x))
		   (plist-get opt-plist (car x))))
	    org-export-plist-vars)
      (setq umax (if arg (prefix-numeric-value arg)
		   org-export-headline-levels))
      (setq umax-toc (if (integerp org-export-with-toc)
			 (min org-export-with-toc umax)
		       umax))

      (when (and org-export-with-toc (not body-only))
	(setq lines (org-parse-prepare-toc
		     lines level-offset opt-plist umax-toc)))

      (unless body-only
	(org-parse-begin 'DOCUMENT-CONTENT opt-plist)
	(org-parse-begin 'DOCUMENT-BODY opt-plist))

      (setq head-count 0)
      (org-init-section-numbers)

      (org-parse-begin-paragraph)

      (while (setq line (pop lines) origline line)
	(catch 'nextline
	  (when (and (org-parse-current-environment-p 'quote)
		     (string-match "^\\*+ " line))
	    (org-parse-end-environment 'quote))

	  (when (org-parse-current-environment-p 'quote)
	    (insert (org-parse-format 'PLAIN line))
	    (throw 'nextline nil))

	  ;; Fixed-width, verbatim lines (examples)
	  (when (and org-export-with-fixed-width
		     (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)" line))
	    (when (not (org-parse-current-environment-p 'fixedwidth))
	      (org-parse-begin-environment 'fixedwidth))
	    (insert (org-parse-format 'PLAIN (match-string 3 line)))
	    (when (or (not lines)
		      (not (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)"
					 (car lines))))
	      (org-parse-end-environment 'fixedwidth))
	    (throw 'nextline nil))

	  ;; Notes: The baseline version of org-html.el (git commit
	  ;; 3d802e), while encoutering a *line-long* protected text,
	  ;; does one of the following two things based on the state
	  ;; of the export buffer.

	  ;; 1. If a paragraph element has just been opened and
	  ;;    contains only whitespace as content, insert the
	  ;;    protected text as part of the previous paragraph.

	  ;; 2. If the paragraph element has already been opened and
	  ;;    contains some valid content insert the protected text
	  ;;    as part of the current paragraph.

	  ;; I think --->

	  ;; Scenario 1 mentioned above kicks in when a block of
	  ;; protected text has to be inserted enbloc. For example,
	  ;; this happens, when inserting an source or example block
	  ;; or preformatted content enclosed in #+backend,
	  ;; #+begin_bakend ... #+end_backend)

	  ;; Scenario 2 mentioned above kicks in when the protected
	  ;; text is part of a running sentence. For example this
	  ;; happens in the case of an *multiline* LaTeX equation that
	  ;; needs to be inserted verbatim.

	  ;; org-html.el in the master branch seems to do some
	  ;; jugglery by moving paragraphs around. Inorder to make
	  ;; these changes backend-agnostic introduce a new text
	  ;; property org-native-text and impose the added semantics
	  ;; that these protected blocks appear outside of a
	  ;; conventional paragraph element.
	  ;;
	  ;; Extra Note: Check whether org-example and org-native-text
	  ;; are entirely equivalent.

	  ;; Fixes bug reported by Christian Moe concerning verbatim
	  ;; LaTeX fragments.
	  ;; on git commit 533ba3f90250a1f25f494c390d639ea6274f235c
	  ;; http://repo.or.cz/w/org-mode/org-jambu.git/shortlog/refs/heads/staging
	  ;; See http://lists.gnu.org/archive/html/emacs-orgmode/2011-03/msg01379.html

	  ;; Native Text
	  (when (and (get-text-property 0 'org-native-text line)
		     ;; Make sure it is the entire line that is protected
		     (not (< (or (next-single-property-change
				  0 'org-native-text line) 10000)
			     (length line))))
	    (let ((ind (get-text-property 0 'original-indentation line)))
	      (org-parse-begin-environment 'native)
	      (insert (org-parse-format 'PLAIN line))
	      (while (and lines
			  (or (= (length (car lines)) 0)
			      (not ind)
			      (equal ind (get-text-property
					  0 'original-indentation (car lines))))
			  (or (= (length (car lines)) 0)
			      (get-text-property 0 'org-native-text (car lines))))
		(insert (org-parse-format 'PLAIN (pop lines))))
	      (org-parse-end-environment 'native))
	    (throw 'nextline nil))

	  ;; Protected HTML
	  (when (and (get-text-property 0 'org-protected line)
		     ;; Make sure it is the entire line that is protected
		     (not (< (or (next-single-property-change
				  0 'org-protected line) 10000)
			     (length line))))
	    (let ((ind (get-text-property 0 'original-indentation line)))
	      (insert (org-parse-format 'PLAIN line))
	      (while (and lines
			  (or (= (length (car lines)) 0)
			      (not ind)
			      (equal ind (get-text-property
					  0 'original-indentation (car lines))))
			  (or (= (length (car lines)) 0)
			      (get-text-property 0 'org-protected (car lines))))
		(insert (org-parse-format 'PLAIN (pop lines)))))
	    (throw 'nextline nil))

	  ;; Blockquotes, verse, and center
	  (when (string-match  "^ORG-\\(.+\\)-\\(START\\|END\\)$" line)
	    (let* ((style (intern (downcase (match-string 1 line))))
		   (f (cdr (assoc (match-string 2 line)
				  '(("START" . org-parse-begin-environment)
				    ("END" . org-parse-end-environment))))))
	      (when (memq style '(blockquote verse center))
		(funcall f style)
		(throw 'nextline nil))))

	  (run-hooks 'org-export-html-after-blockquotes-hook)
	  (when (org-parse-current-environment-p 'verse)
	    (let ((i (org-get-string-indentation line)))
	      (if (> i 0)
		  (setq line (concat
			      (let ((org-html-protect t))
				(org-parse-format 'SPACES (* 2 i)))
			      " " (org-trim line))))
	      (unless (string-match "\\\\\\\\[ \t]*$" line)
		(setq line (concat line "\\\\")))))

	  ;; make targets to anchors
	  (setq start 0)
	  (while (string-match
		  "<<<?\\([^<>]*\\)>>>?\\((INVISIBLE)\\)?[ \t]*\n?" line start)
	    (cond
	     ((get-text-property (match-beginning 1) 'org-protected line)
	      (setq start (match-end 1)))
	     ((match-end 2)
	      (setq line (replace-match
			  (let ((org-html-protect t))
			    (org-parse-format
			     'ANCHOR "" (org-solidify-link-text
					 (match-string 1 line))))
			  t t line)))
	     ((and org-export-with-toc (equal (string-to-char line) ?*))
	      ;; FIXME: NOT DEPENDENT on TOC?????????????????????
	      (setq line (replace-match
			  (let ((org-html-protect t))
			    (org-parse-format
			     'FONTIFY (match-string 1 line) "target"))
			  ;; (concat "@<i>" (match-string 1 line) "@</i> ")
			  t t line)))
	     (t
	      (setq line (replace-match
			  (concat
			   (let ((org-html-protect t))
			     (org-parse-format
			      'ANCHOR (match-string 1 line)
			      (org-solidify-link-text (match-string 1 line))
			      "target")) " ")
			  t t line)))))

	  (let ((org-html-protect t))
	    (setq line (org-html-handle-time-stamps line)))

	  ;; replace "&" by "&amp;", "<" and ">" by "&lt;" and "&gt;"
	  ;; handle @<..> HTML tags (replace "@&gt;..&lt;" by "<..>")
	  ;; Also handle sub_superscripts and checkboxes
	  (or (string-match org-table-hline-regexp line)
	      (string-match "^[ \t]*\\([+]-\\||[ ]\\)[-+ |]*[+|][ \t]*$" line)
	      (setq line (org-html-expand line)))

	  (setq line (org-parse-format-org-link line opt-plist))

	  ;; TODO items
	  (if (and (string-match org-todo-line-regexp line)
		   (match-beginning 2))
	      (setq line (concat
			  (substring line 0 (match-beginning 2))
			  (org-parse-format 'TODO (match-string 2 line))
			  (substring line (match-end 2)))))

	  ;; Does this contain a reference to a footnote?
	  (when org-export-with-footnotes
	    (setq start 0)
	    (while (string-match "\\([^* \t].*?\\)[ \t]*\\[\\([0-9]+\\)\\]" line start)
	      (if (get-text-property (match-beginning 2) 'org-protected line)
		  (setq start (match-end 2))
		(let ((n (match-string 2 line)) refcnt a)
		  (if (setq a (assoc n footref-seen))
		      (progn
			(setcdr a (1+ (cdr a)))
			(setq refcnt (cdr a)))
		    (setq refcnt 1)
		    (push (cons n 1) footref-seen))
		  (setq line
			(replace-match
			 (concat
			  (or (match-string 1 line) "")
			  (org-parse-format
			   'FOOTNOTE-REFERENCE
			   n (cdr (assoc n org-html-footnote-definitions))
			   refcnt))
			 t t line))))))

	  (cond
	   ((string-match "^\\(\\*+\\)[ \t]+\\(.*\\)" line)
	    ;; This is a headline
	    (setq level (org-tr-level (- (match-end 1) (match-beginning 1)
					 level-offset))
		  txt (match-string 2 line))
	    (if (string-match quote-re0 txt)
		(setq txt (replace-match "" t t txt)))
	    (if (<= level (max umax umax-toc))
		(setq head-count (+ head-count 1)))
	    (unless org-html-dyn-first-heading-pos
	      (setq org-html-dyn-first-heading-pos (point)))
	    (org-parse-begin-level level txt umax head-count)

	    ;; QUOTES
	    (when (string-match quote-re line)
	      (org-parse-begin-environment 'quote)))

	   ((and org-export-with-tables
		 (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	    (when (not table-open)
	      ;; New table starts
	      (setq table-open t table-buffer nil table-orig-buffer nil))

	    ;; Accumulate lines
	    (setq table-buffer (cons line table-buffer)
		  table-orig-buffer (cons origline table-orig-buffer))
	    (when (or (not lines)
		      (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
					 (car lines))))
	      (setq table-open nil
		    table-buffer (nreverse table-buffer)
		    table-orig-buffer (nreverse table-orig-buffer))
	      (org-parse-end-paragraph)
	      (insert (org-parse-format 'TABLE table-buffer table-orig-buffer))))

	   ;; Normal lines

	   (t
	    ;; This line either is list item or end a list.
	    (when (get-text-property 0 'list-item line)
	      (setq line (org-html-export-list-line
			  line
			  (get-text-property 0 'list-item line)
			  (get-text-property 0 'list-struct line)
			  (get-text-property 0 'list-prevs line))))

	    ;; Horizontal line
	    (when (string-match "^[ \t]*-\\{5,\\}[ \t]*$" line)
	      (with-org-parse-preserve-paragraph-state
	       (insert (org-parse-format 'HORIZONTAL-LINE)))
	      (throw 'nextline nil))

	    ;; Empty lines start a new paragraph.  If hand-formatted lists
	    ;; are not fully interpreted, lines starting with "-", "+", "*"
	    ;; also start a new paragraph.
	    (when (string-match "^ [-+*]-\\|^[ \t]*$" line)
	      (when org-html-footnote-number
		(org-parse-end-footnote-definition org-html-footnote-number)
		(setq org-html-footnote-number nil))
	      (org-parse-begin-paragraph))

	    ;; Is this the start of a footnote?
	    (when org-export-with-footnotes
	      (when (and (boundp 'footnote-section-tag-regexp)
			 (string-match (concat "^" footnote-section-tag-regexp)
				       line))
		;; ignore this line
		(throw 'nextline nil))
	      (when (string-match "^[ \t]*\\[\\([0-9]+\\)\\]" line)
		(org-parse-end-paragraph)
		(setq org-html-footnote-number (match-string 1 line))
		(setq line (replace-match "" t t line))
		(org-parse-begin-footnote-definition org-html-footnote-number)))
	    ;; Check if the line break needs to be conserved
	    (cond
	     ((string-match "\\\\\\\\[ \t]*$" line)
	      (setq line (replace-match
			  (org-parse-format 'LINE-BREAK)
			  t t line)))
	     (org-export-preserve-breaks
	      (setq line (concat line (org-parse-format 'LINE-BREAK)))))

	    ;; Check if a paragraph should be started
	    (let ((start 0))
	      (while (and org-par-open
			  (string-match "\\\\par\\>" line start))
		(error "FIXME")
		;; Leave a space in the </p> so that the footnote matcher
		;; does not see this.
		(if (not (get-text-property (match-beginning 0)
					    'org-protected line))
		    (setq line (replace-match "</p ><p >" t t line)))
		(setq start (match-end 0))))

	    (insert (org-parse-format 'PLAIN line))))))

      ;; Properly close all local lists and other lists
      (when (org-parse-current-environment-p 'quote)
	(org-parse-end-environment 'quote))

      (org-parse-end-level 1 umax)

      ;; the </div> to close the last text-... div.
      (when (and (> umax 0) org-html-dyn-first-heading-pos)
	(org-parse-end-outline-text-or-outline))

      (org-parse-end 'DOCUMENT-BODY opt-plist)
      (unless body-only
	(org-parse-end 'DOCUMENT-CONTENT))

      (unless (plist-get opt-plist :buffer-will-be-killed)
	(set-auto-mode t))

      (org-parse-end 'EXPORT)

      (goto-char (point-min))
      (or (org-export-push-to-kill-ring
	   (upcase (symbol-name org-parse-backend)))
	  (message "Exporting... done"))

      (cond
       ((not to-buffer)
	(let ((f (org-parse-get 'SAVE-METHOD)))
	  (or (and f (functionp f) (funcall f filename opt-plist))
	      (save-buffer)))
	(current-buffer))
       ((eq to-buffer 'string)
	(prog1 (buffer-substring (point-min) (point-max))
	  (kill-buffer (current-buffer))))
       (t (current-buffer))))))

(defun org-export-html-format-href (s)
  "Make sure the S is valid as a href reference in an XHTML document."
  (save-match-data
    (let ((start 0))
      (while (string-match "&" s start)
	(setq start (+ (match-beginning 0) 3)
	      s (replace-match "&amp;" t t s)))))
  s)

(defun org-export-html-format-desc (s)
  "Make sure the S is valid as a description in a link."
  (if (and s (not (get-text-property 1 'org-protected s)))
      (save-match-data
	(org-html-do-expand s))
    s))

(defun org-export-html-format-image (src)
  "Create image tag with source and attributes."
  (save-match-data
    (if (string-match "^ltxpng/" src)
	(format "<img src=\"%s\" alt=\"%s\"/>"
                src (org-find-text-property-in-string 'org-latex-src src))
      (let* ((caption (org-find-text-property-in-string 'org-caption src))
	     (attr (org-find-text-property-in-string 'org-attributes src))
	     (label (org-find-text-property-in-string 'org-label src))
	     (caption (and caption (org-html-do-expand caption)))
	     (img (format "<img src=\"%s\"%s />"
			  src
			  (if (string-match "\\<alt=" (or attr ""))
			      (concat " " attr )
			    (concat " " attr " alt=\"" src "\""))))
	     (extra (concat
		     (and label
			  (format "id=\"%s\" " (org-solidify-link-text label)))
		     "class=\"figure\"")))
	(if caption
	    (with-temp-buffer
	      (with-org-parse-preserve-paragraph-state
	       (insert
		(org-parse-format
		 '("<div %s>" . "\n</div>")
		 (concat
		  (org-parse-format '("\n<p>" . "</p>") img)
		  (org-parse-format '("\n<p>" . "</p>") caption))
		 extra)))
	      (buffer-string))
	  img)))))

(defun org-export-html-get-bibliography ()
  "Find bibliography, cut it out and return it."
  (catch 'exit
    (let (beg end (cnt 1) bib)
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^[ \t]*<div \\(id\\|class\\)=\"bibliography\"" nil t)
	  (setq beg (match-beginning 0))
	  (while (re-search-forward "</?div\\>" nil t)
	    (setq cnt (+ cnt (if (string= (match-string 0) "<div") +1 -1)))
	    (when (= cnt 0)
	      (and (looking-at ">") (forward-char 1))
	      (setq bib (buffer-substring beg (point)))
	      (delete-region beg (point))
	    (throw 'exit bib))))
	nil))))

(defun org-html-format-table (lines olines)
  (let ((org-html-format-table-no-css nil))
    (org-parse-format-table lines olines)))

;; Following variable is defined for native tables i.e., when
;; `org-table-is-styled' evals to t.
(defvar org-html-format-table-no-css)
(defvar org-table-number-regexp) ; defined in org-table.el
(defun org-parse-format-table (lines olines)
  "Retuns backend-specific code for org-type and table-type
tables."
  (if (stringp lines)
      (setq lines (org-split-string lines "\n")))
  (if (string-match "^[ \t]*|" (car lines))
      ;; A normal org table
      (org-parse-format-org-table lines nil)
    ;; Table made by table.el
    (or (org-parse-format-table-table-using-table-generate-source
	 org-parse-backend olines
	 (not org-export-prefer-native-exporter-for-tables))
	;; We are here only when table.el table has NO col or row
	;; spanning and the user prefers using org's own converter for
	;; exporting of such simple table.el tables.
	(org-parse-format-table-table lines))))

(defun org-format-table-html (lines olines &optional no-css)
  "Find out which HTML converter to use and return the HTML code.
NO-CSS is passed to the exporter."
  (let* ((org-parse-get-callback 'org-html-get)
	 (org-parse-entity-control-callbacks-alist
	  (org-parse-get 'ENTITY-CONTROL))
	 (org-parse-entity-format-callbacks-alist
	  (org-parse-get 'ENTITY-FORMAT))
	 (org-html-format-table-no-css no-css))
    (org-parse-format-table lines olines)))

(defun org-table-get-colalign-info (lines)
  (let ((forced-aligns (org-find-text-property-in-string
			'org-forced-aligns (car lines))))
    (when (and forced-aligns org-table-clean-did-remove-column)
      (setq forced-aligns
	    (mapcar (lambda (x) (cons (1- (car x)) (cdr x))) forced-aligns)))

    forced-aligns))

(defvar org-table-style)
(defvar org-table-ncols)
(defvar org-table-rownum)

(defvar org-table-is-styled)
(defvar org-table-begin-marker)
(defvar org-table-num-numeric-items-per-column)
(defvar org-table-colalign-info)
(defvar org-table-colalign-vector)
(defvar org-table-number-fraction) ; defined in org-table.el
(defun org-parse-do-format-org-table (lines &optional splice)
  "Format a org-type table into backend-specific code.
LINES is a list of lines.  Optional argument SPLICE means, do not
insert header and surrounding <table> tags, just format the lines.
Optional argument NO-CSS means use XHTML attributes instead of CSS
for formatting.  This is required for the DocBook exporter."
  (require 'org-table)
  ;; Get rid of hlines at beginning and end
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (when org-export-table-remove-special-lines
    ;; Check if the table has a marking column.  If yes remove the
    ;; column and the special lines
    (setq lines (org-table-clean-before-export lines)))

  (let* ((caption (org-find-text-property-in-string 'org-caption (car lines)))
	 (caption (and caption (org-html-do-expand caption)))
	 (label (org-find-text-property-in-string 'org-label (car lines)))
	 (org-table-colalign-info (org-table-get-colalign-info lines))
	 (attributes (org-find-text-property-in-string 'org-attributes
						       (car lines)))
	 (head (and org-export-highlight-first-table-line
		    (delq nil (mapcar
			       (lambda (x) (string-match "^[ \t]*|-" x))
			       (cdr lines)))))
	 (org-table-rownum -1) org-table-ncols i (cnt 0)
	 tbopen line fields
	 org-table-current-rowgroup-is-header
	 org-table-rowgroup-open
	 org-table-num-numeric-items-per-column
	 org-table-colalign-vector n
	 org-table-rowgroup-info
	 org-table-begin-marker
	 (org-table-style 'org-table)
	 org-table-is-styled)
    (cond
     (splice
      (setq org-table-is-styled nil)
      (while (setq line (pop lines))
	(unless (string-match "^[ \t]*|-" line)
	  (insert
	   (org-parse-format-table-row
	    (org-split-string line "[ \t]*|[ \t]*")) "\n"))))
     (t
      (setq org-table-is-styled t)
      (org-parse-begin 'TABLE caption label attributes)
      (setq org-table-begin-marker (point))
      (org-parse-begin-table-rowgroup head)
      (while (setq line (pop lines))
	(cond
	 ((string-match "^[ \t]*|-" line)
	  (when lines (org-parse-begin-table-rowgroup)))
	 (t
	  (insert
	   (org-parse-format-table-row
	    (org-split-string line "[ \t]*|[ \t]*")) "\n"))))
      (org-parse-end 'TABLE-ROWGROUP)
      (org-parse-end-table)))))

(defun org-format-org-table-html (lines &optional splice no-css)
  ;; This routine might get called outside of org-export-as-html. For
  ;; example, this could happen as part of org-table-export or as part
  ;; of org-export-as-docbook. Explicitly bind the parser callback to
  ;; the html ones for the duration of the call.
  (let* ((org-parse-get-callback 'org-html-get)
	 (org-parse-entity-control-callbacks-alist
	  (org-parse-get 'ENTITY-CONTROL))
	 (org-parse-entity-format-callbacks-alist
	  (org-parse-get 'ENTITY-FORMAT))
	 (org-html-format-table-no-css no-css))
    (org-parse-format-org-table lines splice)))

(defun org-parse-format-org-table (lines &optional splice)
  (with-temp-buffer
    (org-parse-do-format-org-table lines splice)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-export-splice-attributes (tag attributes)
  "Read attributes in string ATTRIBUTES, add and replace in HTML tag TAG."
  (if (not attributes)
      tag
    (let (oldatt newatt)
      (setq oldatt (org-extract-attributes-from-string tag)
	    tag (pop oldatt)
	    newatt (cdr (org-extract-attributes-from-string attributes)))
      (while newatt
	(setq oldatt (plist-put oldatt (pop newatt) (pop newatt))))
      (if (string-match ">" tag)
	  (setq tag
		(replace-match (concat (org-attributes-to-string oldatt) ">")
			       t t tag)))
      tag)))

(defun org-parse-do-format-table-table (lines)
  "Format a table generated by table.el into backend-specific code.
This conversion does *not* use `table-generate-source' from table.el.
This has the advantage that Org-mode's HTML conversions can be used.
But it has the disadvantage, that no cell- or row-spanning is allowed."
  (let (line field-buffer
	     (org-table-current-rowgroup-is-header
	      org-export-highlight-first-table-line)
	     (caption nil)
	     (attributes nil)
	     (label nil)
	     (org-table-style 'table-table)
	     (org-table-is-styled nil)
	     fields org-table-ncols i (org-table-rownum -1)
	     (empty (org-parse-format 'SPACES 1)))
    (org-parse-begin 'TABLE caption label attributes)
    (while (setq line (pop lines))
      (cond
       ((string-match "^[ \t]*\\+-" line)
	(when field-buffer
	  (let ((org-export-table-row-tags '("<tr>" . "</tr>"))
		(org-export-html-table-use-header-tags-for-first-column nil))
	    (insert (org-parse-format-table-row field-buffer empty)))
	  (setq org-table-current-rowgroup-is-header nil)
	  (setq field-buffer nil)))
       (t
	;; Break the line into fields and store the fields
	(setq fields (org-split-string line "[ \t]*|[ \t]*"))
	(if field-buffer
	    (setq field-buffer (mapcar
				(lambda (x)
				  (concat x (org-parse-format 'LINE-BREAK)
					  (pop fields)))
				field-buffer))
	  (setq field-buffer fields)))))
    (org-parse-end-table)))

(defun org-parse-format-table-table (lines)
  (with-temp-buffer
    (org-parse-do-format-table-table lines)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-format-table-table-html (lines)
  (let* ((org-parse-get-callback 'org-html-get)
	 (org-parse-entity-control-callbacks-alist
	  (org-parse-get 'ENTITY-CONTROL))
	 (org-parse-entity-format-callbacks-alist
	  (org-parse-get 'ENTITY-FORMAT)))
    (org-parse-format-table-table lines)))

(defun org-parse-format-table-table-using-table-generate-source (backend
								 lines
								 &optional
								 spanned-only)
  "Format a table into BACKEND, using `table-generate-source' from table.el.
Use SPANNED-ONLY to suppress exporting of simple table.el tables.

When SPANNED-ONLY is nil, all table.el tables are exported.  When
SPANNED-ONLY is non-nil, only tables with either row or column
spans are exported.

This routine returns the generated source or nil as appropriate.

Refer docstring of `org-export-prefer-native-exporter-for-tables'
for further information."
  (require 'table)
  (with-current-buffer (get-buffer-create " org-tmp1 ")
    (erase-buffer)
    (insert (mapconcat 'identity lines "\n"))
    (goto-char (point-min))
    (if (not (re-search-forward "|[^+]" nil t))
	(error "Error processing table"))
    (table-recognize-table)
    (when (or (not spanned-only)
	      (let* ((dim (table-query-dimension))
		     (c (nth 4 dim)) (r (nth 5 dim)) (cells (nth 6 dim)))
		(not (= (* c r) cells))))
      (with-current-buffer (get-buffer-create " org-tmp2 ") (erase-buffer))
      (cond
       ((member backend table-source-languages)
	(table-generate-source backend " org-tmp2 ")
	(set-buffer " org-tmp2 ")
	(buffer-substring (point-min) (point-max)))
       (t
	;; table.el doesn't support the given backend. Currently this
	;; happens in case of odt export.  Strip the table from the
	;; generated document. A better alternative would be to embed
	;; the table as ascii text in the output document.
	(org-parse-warn
	 (concat
	  "Found table.el-type table in the source org file. "
	  (format "table.el doesn't support %s backend. "
		  (upcase (symbol-name backend)))
	  "Skipping ahead ..."))
	"")))))

(defun org-export-splice-style (style extra)
  "Splice EXTRA into STYLE, just before \"</style>\"."
  (if (and (stringp extra)
	   (string-match "\\S-" extra)
	   (string-match "</style>" style))
      (concat (substring style 0 (match-beginning 0))
	      "\n" extra "\n"
	      (substring style (match-beginning 0)))
    style))

(defun org-html-handle-time-stamps (s)
  "Format time stamps in string S, or remove them."
  (catch 'exit
    (let (r b)
      (while (string-match org-maybe-keyword-time-regexp s)
	(or b (setq b (substring s 0 (match-beginning 0))))
	(setq r (concat
		 r (substring s 0 (match-beginning 0))
		 (org-parse-format
		  'FONTIFY
		  (concat
		   (if (match-end 1)
		       (org-parse-format
			'FONTIFY
			(match-string 1 s) "timestamp-kwd"))
		   (org-parse-format
		    'FONTIFY
		    (substring (org-translate-time (match-string 3 s)) 1 -1)
		    "timestamp"))
		  "timestamp-wrapper"))
	      s (substring s (match-end 0))))
      ;; Line break if line started and ended with time stamp stuff
      (if (not r)
	  s
	(setq r (concat r s))
	(unless (string-match "\\S-" (concat b s))
	  (setq r (concat r (org-parse-format 'LINE-BREAK))))
	r))))

(defvar htmlize-buffer-places)  ; from htmlize.el
(defun org-export-htmlize-region-for-paste (beg end)
  "Convert the region to HTML, using htmlize.el.
This is much like `htmlize-region-for-paste', only that it uses
the settings define in the org-... variables."
  (let* ((htmlize-output-type org-export-htmlize-output-type)
	 (htmlize-css-name-prefix org-export-htmlize-css-font-prefix)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

;;;###autoload
(defun org-export-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-export-htmlize-output-type' to `css', calls to
the function `org-export-htmlize-region-for-paste' will produce code
that uses these same face definitions."
  (interactive)
  (require 'htmlize)
  (and (get-buffer "*html*") (kill-buffer "*html*"))
  (with-temp-buffer
    (let ((fl (face-list))
	  (htmlize-css-name-prefix "org-")
	  (htmlize-output-type 'css)
	  f i)
      (while (setq f (pop fl)
		   i (and f (face-attribute f :inherit)))
	(when (and (symbolp f) (or (not i) (not (listp i))))
	  (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max))))
  (switch-to-buffer "*html*")
  (goto-char (point-min))
  (if (re-search-forward "<style" nil t)
      (delete-region (point-min) (match-beginning 0)))
  (if (re-search-forward "</style>" nil t)
      (delete-region (1+ (match-end 0)) (point-max)))
  (beginning-of-line 1)
  (if (looking-at " +") (replace-match ""))
  (goto-char (point-min)))

(defun org-html-protect (s)
  "Convert characters to HTML equivalent.
Possible conversions are set in `org-export-html-protect-char-alist'."
  (let ((cl org-export-html-protect-char-alist) c)
    (while (setq c (pop cl))
      (let ((start 0))
	(while (string-match (car c) s start)
	  (setq s (replace-match (cdr c) t t s)
		start (1+ (match-beginning 0))))))
    s))

(defun org-html-expand (string)
  "Prepare STRING for HTML export.  Apply all active conversions.
If there are links in the string, don't modify these."
  (let* ((re (concat org-bracket-link-regexp "\\|"
		     (org-re "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$")))
	 m s l res)
    (while (setq m (string-match re string))
      (setq s (substring string 0 m)
	    l (match-string 0 string)
	    string (substring string (match-end 0)))
      (push (org-html-do-expand s) res)
      (push l res))
    (push (org-html-do-expand string) res)
    (apply 'concat (nreverse res))))

(defun org-html-do-expand (s)
  "Apply all active conversions to translate special ASCII to HTML."
  (setq s (org-html-protect s))
  (if org-export-html-expand
      (while (string-match "@&lt;\\([^&]*\\)&gt;" s)
	(setq s (replace-match "<\\1>" t nil s))))
  (if org-export-with-emphasize
      (setq s (org-export-html-convert-emphasize s)))
  (if org-export-with-special-strings
      (setq s (org-export-html-convert-special-strings s)))
  (if org-export-with-sub-superscripts
      (setq s (org-export-html-convert-sub-super s)))
  (if org-export-with-TeX-macros
      (let ((start 0) wd rep)
	(while (setq start (string-match "\\\\\\([a-zA-Z]+[0-9]*\\)\\({}\\)?"
					 s start))
	  (if (get-text-property (match-beginning 0) 'org-protected s)
	      (setq start (match-end 0))
	    (setq wd (match-string 1 s))
	    (if (setq rep (org-entity-get-representation wd 'html))
		(setq s (replace-match rep t t s))
	      (setq start (+ start (length wd))))))))
  s)

(defun org-export-html-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (let ((all org-export-html-special-string-regexps)
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(if (get-text-property (match-beginning 0) 'org-protected string)
	    (setq start (match-end 0))
	  (setq string (replace-match rpl t nil string)))))
    string))

(defun org-export-html-convert-sub-super (string)
  "Convert sub- and superscripts in STRING to HTML."
  (let (key c (s 0) (requireb (eq org-export-with-sub-superscripts '{})))
    (while (string-match org-match-substring-regexp string s)
      (cond
       ((and requireb (match-end 8)) (setq s (match-end 2)))
       ((get-text-property  (match-beginning 2) 'org-protected string)
	(setq s (match-end 2)))
       (t
	(setq s (match-end 1)
	      key (if (string= (match-string 2 string) "_")
		      'subscript 'superscript)
	      c (or (match-string 8 string)
		    (match-string 6 string)
		    (match-string 5 string))
	      string (replace-match
		      (concat (match-string 1 string)
			      (org-parse-format 'FONTIFY c key))
		      t t string)))))
    (while (string-match "\\\\\\([_^]\\)" string)
      (setq string (replace-match (match-string 1 string) t t string)))
    string))

(defconst org-html-emphasis-alist
  `(("*" bold)
    ("/" emphasis)
    ("_" underline)
    ("=" code)
    ("~" verbatim)
    ("+" strike)))

(defun org-export-html-convert-emphasize (string)
  "Apply emphasis."
  (let ((s 0) rpl)
    (while (string-match org-emph-re string s)
      (if (not (equal
		(substring string (match-beginning 3) (1+ (match-beginning 3)))
		(substring string (match-beginning 4) (1+ (match-beginning 4)))))
	  (setq s (match-beginning 0)
		rpl
		(concat
		 (match-string 1 string)
		 (org-parse-format
		  'FONTIFY (match-string 4 string)
		  (nth 1 (assoc (match-string 3 string)
				org-html-emphasis-alist)))
		 (match-string 5 string))
		string (replace-match rpl t t string)
		s (+ s (- (length rpl) 2)))
	(setq s (1+ s))))
    string))

(defvar body-only) ; dynamically scoped into this.
(defun org-export-html-get-tag-class-name (tag)
  "Turn tag into a valid class name.
Replaces invalid characters with \"_\" and then prepends a prefix."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" tag)
      (setq tag (replace-match "_" t t tag))))
  (concat org-export-html-tag-class-prefix tag))

(defun org-export-html-get-todo-kwd-class-name (kwd)
  "Turn todo keyword into a valid class name.
Replaces invalid characters with \"_\" and then prepends a prefix."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  (concat org-export-html-todo-kwd-class-prefix kwd))

(defun org-html-export-list-line (line pos struct prevs)
  "Insert list syntax in export buffer. Return LINE, maybe modified.

POS is the item position or line position the line had before
modifications to buffer. STRUCT is the list structure. PREVS is
the alist of previous items."
  (let* ((get-type
	  (function
	   ;; Translate type of list containing POS to "d", "o" or
	   ;; "u".
	   (lambda (pos struct prevs)
	     (let ((type (org-list-get-list-type pos struct prevs)))
	       (cond
		((eq 'ordered type) "o")
		((eq 'descriptive type) "d")
		(t "u"))))))
	 (get-closings
	  (function
	   ;; Return list of all items and sublists ending at POS, in
	   ;; reverse order.
	   (lambda (pos)
	     (let (out)
	       (catch 'exit
		 (mapc (lambda (e)
			 (let ((end (nth 6 e))
			       (item (car e)))
			   (cond
			    ((= end pos) (push item out))
			    ((>= item pos) (throw 'exit nil)))))
		       struct))
	       out)))))
    ;; First close any previous item, or list, ending at POS.
    (mapc (lambda (e)
	    (let* ((lastp (= (org-list-get-last-item e struct prevs) e))
		   (first-item (org-list-get-list-begin e struct prevs))
		   (type (funcall get-type first-item struct prevs)))
	      (org-parse-end-paragraph)
	      ;; Ending for every item
	      (org-close-li type)
	      ;; We're ending last item of the list: end list.
	      (when lastp
		(org-parse-end 'LIST type)
		(org-parse-begin-paragraph))))
	  (funcall get-closings pos))
    (cond
     ;; At an item: insert appropriate tags in export buffer.
     ((assq pos struct)
      (string-match
       (concat "[ \t]*\\(\\S-+[ \t]*\\)"
	       "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\]\\)?"
	       "\\(?:\\(\\[[ X-]\\]\\)[ \t]+\\)?"
	       "\\(?:\\(.*\\)[ \t]+::[ \t]+\\)?"
	       "\\(.*\\)") line)
      (let* ((checkbox (match-string 3 line))
	     (desc-tag (or (match-string 4 line) "???"))
	     (body (or (match-string 5 line) ""))
	     (list-beg (org-list-get-list-begin pos struct prevs))
	     (firstp (= list-beg pos))
	     ;; Always refer to first item to determine list type, in
	     ;; case list is ill-formed.
	     (type (funcall get-type list-beg struct prevs))
	     (counter (let ((count-tmp (org-list-get-counter pos struct)))
			(cond
			 ((not count-tmp) nil)
			 ((string-match "[A-Za-z]" count-tmp)
			  (- (string-to-char (upcase count-tmp)) 64))
			 ((string-match "[0-9]+" count-tmp)
			  count-tmp)))))
	(when firstp
	  (org-parse-end-paragraph)
	  (org-parse-begin 'LIST type))

	(let ((arg (cond ((equal type "d") desc-tag)
			 ((equal type "o") counter))))
	  (org-parse-begin 'LIST-ITEM type arg))

	;; If line had a checkbox, some additional modification is required.
	(when checkbox
	  (setq body
		(concat
		 (org-parse-format
		  'FONTIFY (concat
			    "["
			    (cond
			     ((string-match "X" checkbox) "X")
			     ((string-match " " checkbox)
			      (org-parse-format 'SPACES 1))
			     (t "-"))
			    "]")
		  'code)
		 " "
		 body)))
	;; Return modified line
	body))
     ;; At a list ender: go to next line (side-effects only).
     ((equal "ORG-LIST-END-MARKER" line) (throw 'nextline nil))
     ;; Not at an item: return line unchanged (side-effects only).
     (t line))))

;; miscellaneous

(defun org-html-bind-local-variables (opt-plist)
  (mapc (lambda (x)
	  (set (make-local-variable (nth 2 x))
	       (plist-get opt-plist (car x))))
	org-export-plist-vars))


;; This replaces org-emphasis-alist
(defvar org-table-rowgroup-open)
(defvar org-table-current-rowgroup-is-header)
(defvar org-html-footnote-number)
(defvar org-html-footnote-definitions)
(defvar org-html-footnote-buffer)
(defvar org-html-output-buffer)

(defun org-html-end-export ()
  ;; insert the table of contents
  (when (and org-export-with-toc (not body-only) org-parse-table-of-contents)
    (org-html-insert-toc org-parse-table-of-contents))

  ;; remove empty paragraphs
  (goto-char (point-min))
  (while (re-search-forward "<p>[ \r\n\t]*</p>" nil t)
    (replace-match ""))

  ;; Convert whitespace place holders
  (goto-char (point-min))
  (let (beg end n)
    (while (setq beg (next-single-property-change (point) 'org-whitespace))
      (setq n (get-text-property beg 'org-whitespace)
	    end (next-single-property-change beg 'org-whitespace))
      (goto-char beg)
      (delete-region beg end)
      (insert (format "<span style=\"visibility:hidden;\">%s</span>"
		      (make-string n ?x)))))

  ;; Remove empty lines at the beginning of the file.
  (goto-char (point-min))
  (when (looking-at "\\s-+\n") (replace-match ""))

  ;; Remove display properties
  (remove-text-properties (point-min) (point-max) '(display t))

  ;; kill temporary buffers
  (when org-html-footnote-buffer
    (kill-buffer org-html-footnote-buffer))

  ;; Run the hook
  (run-hooks 'org-export-html-final-hook))


;;;_ org-parse.el
;;;_. preamble
;;;_ , user-specific
;;;_  . custom-settings
(defcustom org-parse-debug nil
  ""
  :group 'org-parse
  :type 'boolean)




;;;_ , callbacks
;;;_  . control callbacks
;;;_   , generic
(defun org-parse-begin (entity &rest args)
  (when (and (member org-parse-debug '(t control))
	     (not (eq entity 'DOCUMENT-CONTENT)))
    (insert (org-parse-format 'COMMENT "%s BEGIN %S" entity args)))

  (let ((f (cadr (assoc entity org-parse-entity-control-callbacks-alist))))
    (unless f (error "Unknown entity: %s" entity))
    (apply f args)))

(defun org-parse-end (entity &rest args)
  (when (and (member org-parse-debug '(t control))
	     (not (eq entity 'DOCUMENT-CONTENT)))
    (insert (org-parse-format 'COMMENT "%s END %S" entity args)))

  (let ((f (caddr (assoc entity org-parse-entity-control-callbacks-alist))))
    (unless f (error "Unknown entity: %s" entity))
    (apply f args)))


;;;_   , paragraph
(defun org-parse-begin-paragraph (&optional style)
  "Insert <p>, but first close previous paragraph if any."
  (org-parse-end-paragraph)
  (org-parse-begin 'PARAGRAPH style)
  (setq org-par-open t))

(defun org-parse-end-paragraph ()
  "Close paragraph if there is one open."
  (when org-par-open
    (org-parse-end 'PARAGRAPH)
    (setq org-par-open nil)))

;;;_   , list
(defun org-close-li (&optional type)
  "Close <li> if necessary."
  (org-parse-end-paragraph)
  (org-parse-end 'LIST-ITEM (or type "u")))


;;;_   , environment
(defvar org-parse-dyn-current-environment nil)
(defun org-parse-begin-environment (style)
  (assert (not org-parse-dyn-current-environment) t)
  (setq org-parse-dyn-current-environment style)
  (org-parse-begin 'ENVIRONMENT  style))

(defun org-parse-end-environment (style)
  (org-parse-end 'ENVIRONMENT style)

  (assert (eq org-parse-dyn-current-environment style) t)
  (setq org-parse-dyn-current-environment nil))

(defun org-parse-current-environment-p (style)
  (eq org-parse-dyn-current-environment style))


;;;_   , footnote definition
(defun org-parse-begin-footnote-definition (n)
  (unless org-html-footnote-buffer
    (setq org-html-footnote-buffer
	  (get-buffer-create "*Org HTML Export Footnotes*")))
  (set-buffer org-html-footnote-buffer)
  (erase-buffer)
  (setq org-html-insert-tag-with-newlines nil)
  (org-parse-begin 'FOOTNOTE-DEFINITION n))

(defun org-parse-end-footnote-definition (n)
  (org-parse-end 'FOOTNOTE-DEFINITION n)
  (setq org-html-insert-tag-with-newlines 'both)
  (push (cons n (buffer-string)) org-html-footnote-definitions)
  (set-buffer org-html-output-buffer))




;;;_  . format callbacks
;;;_   , generic
(defun org-parse-format (entity &rest args)
  (when (and (member org-parse-debug '(t format))
	     (not (equal entity 'COMMENT)))
    (insert (org-parse-format 'COMMENT "%s: %S" entity args)))
  (cond
   ((consp entity)
    (let ((text (pop args)))
      (apply 'org-parse-format 'TAGS entity text args)))
   (t
    (let ((f (cdr (assoc entity org-parse-entity-format-callbacks-alist))))
      (unless f (error "Unknown entity: %s" entity))
      (apply f args)))))


;;;_   , toc (rename these routines)
(defun org-html-format-toc-entry (snumber todo headline tags href)
  (setq headline (concat
		  (and org-export-with-section-numbers
		       (concat snumber " "))
		  headline
		  (and tags
		    (concat
		     (org-parse-format 'SPACES 3)
		     (org-parse-format 'FONTIFY tags "tag")))))
  (when todo
    (setq headline (org-parse-format 'FONTIFY headline "todo")))
  (org-parse-format 'LINK headline (concat  "#" href)))

(defun org-html-format-toc-item (toc-entry level org-last-level)
  (when (> level org-last-level)
    (let ((cnt (- level org-last-level)))
      (while (>= (setq cnt (1- cnt)) 0)
	(org-parse-begin 'LIST 'unordered)
	(org-parse-begin 'LIST-ITEM 'unordered))))
  (when (< level org-last-level)
    (let ((cnt (- org-last-level level)))
      (while (>= (setq cnt (1- cnt)) 0)
	(org-close-li)
	(org-parse-end 'LIST 'unordered))))

  (org-close-li)
  (org-parse-begin 'LIST-ITEM 'unordered)
  (insert toc-entry))

(defun org-html-begin-toc (lang-specific-heading)
  (org-html-insert-tag "<div id=\"table-of-contents\">")
  (insert
   (org-parse-format 'HEADING lang-specific-heading
		     (or (org-parse-get 'TOPLEVEL-HLEVEL) 1)))
  (org-html-insert-tag "<div id=\"text-table-of-contents\">")
  (org-parse-begin 'LIST 'unordered)
  (org-parse-begin 'LIST-ITEM 'unordered))

(defun org-html-end-toc ()
  (while (> org-last-level (1- org-min-level))
    (setq org-last-level (1- org-last-level))
    (org-close-li)
    (org-parse-end 'LIST 'unordered))
  (org-html-insert-tag "</div>")
  (org-html-insert-tag "</div>")

  ;; cleanup empty list items in toc
  (while (re-search-backward "<li>[ \r\n\t]*</li>\n?" (point-min) t)
    (replace-match "")))

(defun org-parse-prepare-toc (lines level-offset opt-plist umax-toc)
  (let* ((quote-re0 (concat "^[ \t]*" org-quote-string "\\>"))
	 (org-min-level (org-get-min-level lines level-offset))
	 (org-last-level org-min-level)
	 level)
    (with-temp-buffer
      (org-html-bind-local-variables opt-plist)
      (erase-buffer)
      (org-parse-begin 'TOC (nth 3 (plist-get opt-plist :lang-words)))
      (setq
       lines
       (mapcar
	'(lambda (line)
	   (when (and (string-match org-todo-line-regexp line)
		      (not (get-text-property 0 'org-protected line))
		      (<= (setq level (org-tr-level
				       (- (match-end 1) (match-beginning 1)
					  level-offset)))
			  umax-toc))
	     (let ((txt (save-match-data
			  (org-html-expand
			   (org-export-cleanup-toc-line
			    (match-string 3 line)))))
		   (todo (and
			  org-export-mark-todo-in-toc
			  (or (and (match-beginning 2)
				   (not (member (match-string 2 line)
						org-done-keywords)))
			      (and (= level umax-toc)
				   (org-search-todo-below
				    line lines level)))))
		   tags)
	       ;; Check for targets
	       (while (string-match org-any-target-regexp line)
		 (setq line
		       (replace-match
			(let ((org-html-protect t))
			  (org-parse-format 'FONTIFY
					    (match-string 1 line) "target"))
			t t line)))
	       (when (string-match
		      (org-re "[ \t]+:\\([[:alnum:]_@:]+\\):[ \t]*$") txt)
		 (setq tags (match-string 1 txt)
		       txt (replace-match "" t nil txt)))
	       (when (string-match quote-re0 txt)
		 (setq txt (replace-match "" t t txt)))
	       (while (string-match "&lt;\\(&lt;\\)+\\|&gt;\\(&gt;\\)+" txt)
		 (setq txt (replace-match "" t t txt)))
	       (org-parse-format
		'TOC-ITEM
		(let* ((snumber (org-section-number level))
		       (href (replace-regexp-in-string
			      "\\." "_" (format "sec-%s" snumber)))
		       (href
			(or
			 (cdr (assoc
			       href org-export-preferred-target-alist))
			 href))
		       (href (org-solidify-link-text href)))
		  (org-parse-format 'TOC-ENTRY snumber todo txt tags href))
		level org-last-level)
	       (setq org-last-level level)))
	   line)
	lines))
      (org-parse-end 'TOC)
      (setq org-parse-table-of-contents (buffer-string))))
  lines)


;;;_   , table row
(defun org-parse-format-table-row (fields &optional text-for-empty-fields)
  (unless org-table-ncols
    ;; first row of the table
    (setq org-table-ncols (length fields))
    (when org-table-is-styled
      (setq org-table-num-numeric-items-per-column (make-vector org-table-ncols 0))
      (setq org-table-colalign-vector (make-vector org-table-ncols nil))
      (let ((c -1))
	(while  (< (incf c) org-table-ncols)
	  (let ((cookie (cdr (assoc (1+ c) org-table-colalign-info))))
	    (setf (aref org-table-colalign-vector c)
		  (cond
		   ((string= cookie "l") "left")
		   ((string= cookie "r") "right")
		   ((string= cookie "c") "center")
		   (t nil))))))))
  (incf org-table-rownum)

  (let ((i -1))
    (org-parse-format
     'TABLE-ROW
     (mapconcat
      (lambda (x)
	(when (string= x "")
	  (setq x text-for-empty-fields))
	(incf i)
	(and org-table-is-styled
	     (< i org-table-ncols)
	     (string-match org-table-number-regexp x)
	     (incf (aref org-table-num-numeric-items-per-column i)))
	(org-parse-format 'TABLE-CELL x org-table-rownum i))
      fields "\n"))))

;;;_  . get callback
(defun org-parse-get (what &optional opt-plist)
  (funcall org-parse-get-callback what opt-plist))

;;;_. postamble


;;;_ org-newhtml.el
;;;_. preamble
;;;_. obsolete
;;;_. maybe
;;;_. hacks (to be documented)

;; Refer `org-html-emphasis-alist'

;;;_. user-specific
;;;_ , custom settings
;;;_  . potential custom settings
;;;_ , interactive commands

;;;###autoload
(defun org-export-as-html-and-open (arg)
  "Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-export-as-and-open "html" arg))

;;;###autoload
(defun org-export-as-html-batch ()
  "Call the function `org-export-as-html'.
This function can be used in batch processing as:
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-html-batch"
  (org-export-as-batch "html"))

;;;###autoload
(defun org-export-as-html-to-buffer (arg)
  "Call `org-export-as-html` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-html'."
  (interactive "P")
  (org-export-as-to-buffer "html" arg))

;;;###autoload
(defun org-replace-region-by-html (beg end)
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an HTML buffer and then use this
command to convert it."
  (interactive "r")
  (org-replace-region-by "html" beg end))

;;;###autoload
(defun org-export-region-as-html (beg end &optional body-only buffer)
  "Convert region from BEG to END in org-mode buffer to HTML.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted HTML.  If BUFFER is the symbol `string', return the
produced HTML as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq html (org-export-region-as-html beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (interactive "r\nP")
  (org-export-region-as "html" beg end body-only buffer))

;;; org-export-as-html
;;;###autoload
(defun org-export-as-html (arg &optional hidden ext-plist
			       to-buffer body-only pub-dir)
  "Export the outline as a pretty HTML file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.  HIDDEN is obsolete and does nothing.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol
`string', don't leave any buffer behind but just return the
resulting HTML as a string.  When BODY-ONLY is set, don't produce
the file header and footer, simply return the content of
<body>...</body>, without even the body tags themselves.  When
PUB-DIR is set, use this as the publishing directory."
  (interactive "P")
  (org-export-as "html" arg hidden ext-plist to-buffer body-only pub-dir))

;;;_. parser
;;;_ , initialization
(defvar org-html-entity-control-callbacks-alist
  `((EXPORT
     . (org-html-begin-export org-html-end-export))
    (DOCUMENT-CONTENT
     . (org-html-begin-document-content org-html-end-document-content))
    (DOCUMENT-BODY
     . (org-html-begin-document-body org-html-end-document-body))
    (TOC
     . (org-html-begin-toc org-html-end-toc))
    (ENVIRONMENT
     . (org-html-begin-environment org-html-end-environment))
    (FOOTNOTE-DEFINITION
     . (org-html-begin-footnote-definition org-html-end-footnote-definition))
    (TABLE
     . (org-html-begin-table org-html-end-table))
    (TABLE-ROWGROUP
     . (org-html-begin-table-rowgroup org-html-end-table-rowgroup))
    (LIST
     . (org-html-begin-list org-html-end-list))
    (LIST-ITEM
     . (org-html-begin-list-item org-html-end-list-item))
    (OUTLINE
     . (org-html-begin-outline org-html-end-outline))
    (OUTLINE-TEXT
     . (org-html-begin-outline-text org-html-end-outline-text))
    (PARAGRAPH
     . (org-html-begin-paragraph org-html-end-paragraph)))
  "")

(defvar org-html-entity-format-callbacks-alist
  `((EXTRA-TARGETS . org-html-format-extra-targets)
    (ORG-TAGS . org-html-format-org-tags)
    (SECTION-NUMBER . org-html-format-section-number)
    (HEADLINE . org-html-format-headline)
    (TOC-ENTRY . org-html-format-toc-entry)
    (TOC-ITEM . org-html-format-toc-item)
    (TAGS . org-html-format-tags)
    (SPACES . org-html-format-spaces)
    (TABS . org-html-format-tabs)
    (LINE-BREAK . org-html-format-line-break)
    (FONTIFY . org-html-format-fontify)
    (TODO . org-html-format-todo)
    (ORG-LINK . org-html-format-org-link)
    (LINK . org-html-format-link)
    (INLINE-IMAGE . org-html-format-inline-image)
    (HEADING . org-html-format-heading)
    (ANCHOR . org-html-format-anchor)
    (TABLE . org-html-format-table)
    (TABLE-ROW . org-html-format-table-row)
    (TABLE-CELL . org-html-format-table-cell)
    (FOOTNOTES-SECTION . org-html-format-footnotes-section)
    (FOOTNOTE-REFERENCE . org-html-format-footnote-reference)
    (HORIZONTAL-LINE . org-html-format-horizontal-line)
    (PLAIN . org-html-format-plain)
    (COMMENT . org-html-format-comment))
  "")

;;;_ , callbacks
;;;_  . control callbacks
;;;_   , generic
(defun org-html-insert-tag (tag &rest args)
  (when (member org-html-insert-tag-with-newlines '(lead both))
    (insert  "\n"))
  (insert (apply 'format tag args))
  (when (member org-html-insert-tag-with-newlines '(trail both))
    (insert  "\n")))

;;;_   , document body
(defun org-html-begin-document-body (opt-plist)
  (let ((link-up (and (plist-get opt-plist :link-up)
		      (string-match "\\S-" (plist-get opt-plist :link-up))
		      (plist-get opt-plist :link-up)))
	(link-home (and (plist-get opt-plist :link-home)
			(string-match "\\S-" (plist-get opt-plist :link-home))
			(plist-get opt-plist :link-home))))
    (insert "\n<body>")
    (org-html-insert-tag "<div id=\"content\">")
    (insert  "\n"
	     (or (and (or link-up link-home)
		      (format org-export-html-home/up-format
			      (or link-up link-home)
			      (or link-home link-up))) "")
	     "\n"))
  (org-html-insert-preamble opt-plist))

(defun org-html-end-document-body (opt-plist)
  (org-html-insert-postamble opt-plist)
  (unless body-only
    (org-html-insert-tag "</div>")
    (insert "\n</body>")))


;;;_   , document content
(defun org-html-begin-document-content (opt-plist)
  (let* ((language (plist-get opt-plist :language))
	 (charset (or (and coding-system-for-write
			   (fboundp 'coding-system-get)
			   (coding-system-get coding-system-for-write
					      'mime-charset))
		      "iso-8859-1"))
	 (style (concat (if (plist-get opt-plist :style-include-default)
			    org-export-html-style-default)
			(plist-get opt-plist :style)
			(plist-get opt-plist :style-extra)
			"\n"
			(if (plist-get opt-plist :style-include-scripts)
			    org-export-html-scripts)))
	 (mathjax
	  (if (or (eq (plist-get opt-plist :LaTeX-fragments) 'mathjax)
		  (and org-export-have-math
		       (eq (plist-get opt-plist :LaTeX-fragments) t)))

	      (org-export-html-mathjax-config
	       org-export-html-mathjax-template
	       org-export-html-mathjax-options
	       (or (plist-get opt-plist :mathjax) "")) "")))
    (insert (format
	     "%s
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
	       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\"
lang=\"%s\" xml:lang=\"%s\">
<head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>
<meta name=\"generator\" content=\"Org-mode\"/>
<meta name=\"generated\" content=\"%s\"/>
<meta name=\"author\" content=\"%s\"/>
<meta name=\"description\" content=\"%s\"/>
<meta name=\"keywords\" content=\"%s\"/>
%s
%s
</head>
"
	     (format
	      (or (and (stringp org-export-html-xml-declaration)
		       org-export-html-xml-declaration)
		  (cdr (assoc (plist-get opt-plist :html-extension)
			      org-export-html-xml-declaration))
		  (cdr (assoc "html" org-export-html-xml-declaration))

		  "")
	      charset)
	     language language
	     (plist-get opt-plist :title)
	     charset
	     (plist-get opt-plist :effective-date)
	     (plist-get opt-plist :author)
	     (plist-get opt-plist :description)
	     (plist-get opt-plist :keywords)
	     style
	     mathjax))))

(defun org-html-end-document-content ()
  (insert "\n</html>\n"))

(defun org-parse-target-from-snumber (snumber) ; can be removed
  (let* ((snu (replace-regexp-in-string "\\." "_" snumber))
	 (sec-snu (concat "sec-" snu)))
    (org-solidify-link-text
     (or (cdr (assoc sec-snu org-export-preferred-target-alist))
	 sec-snu))))

(defun org-parse-get-targets-from-title (title)
  (let* ((target (org-get-text-property-any 0 'target title))
	 (extra-targets (assoc target org-export-target-aliases))
	 (target (or (cdr (assoc target org-export-preferred-target-alist))
		     target)))
    (cons target (remove target extra-targets))))

(defun org-parse-suffix-from-snumber (snumber)
  (let* ((snu (replace-regexp-in-string "\\." "_" snumber))
	 (href (cdr (assoc (concat "sec-" snu)
			   org-export-preferred-target-alist))))
    (org-solidify-link-text (or href snu))))

;;;_   , level
(defun org-parse-begin-level (level title umax head-count)
  "Insert a new level in HTML export.
When TITLE is nil, just close all open levels."
  (org-parse-end-level level umax)
  (unless title (error "Why is heading nil"))
  (let* ((targets (org-parse-get-targets-from-title title))
	 (target (car targets)) (extra-targets (cdr targets))
	 (target (and target (org-solidify-link-text target)))
	 (extra-class (org-get-text-property-any 0 'html-container-class title))
	 snumber tags level1 class)
    (when (string-match (org-re "\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$") title)
      (setq tags (and org-export-with-tags (match-string 1 title)))
      (setq title (replace-match "" t t title)))
    (if (> level umax)
	(progn
	  (if (aref org-levels-open (1- level))
	      (org-close-li)
	    (aset org-levels-open (1- level) t)
	    (org-parse-end-paragraph)
	    (org-parse-begin 'LIST 'unordered))
	  (org-parse-begin
	   'LIST-ITEM 'unordered target
	   (org-parse-format 'HEADLINE title extra-targets tags)))
      (aset org-levels-open (1- level) t)
      (setq snumber (org-section-number level))
      (setq level1 (+ level (or (org-parse-get 'TOPLEVEL-HLEVEL) 1) -1))
      (unless (= head-count 1)
	(org-parse-end-outline-text-or-outline))
      (org-parse-begin-outline-and-outline-text
       level1 snumber title tags target extra-targets extra-class)
      (org-parse-begin-paragraph))))

(defun org-parse-end-level (level umax)
  (org-parse-end-paragraph)
  (loop for l from org-level-max downto level
	do (when (aref org-levels-open (1- l))
	     ;; Terminate one level in HTML export
	     (if (<= l umax)
		 (org-parse-end-outline-text-or-outline)
	       (org-close-li)
	       (org-parse-end 'LIST 'unordered))
	     (aset org-levels-open (1- l) nil))))


;;;_   , outline and outline text
(defvar org-parse-outline-text-open)
(defun org-parse-begin-outline-and-outline-text (level1 snumber title tags
							target extra-targets
							extra-class)
  (org-parse-begin
   'OUTLINE level1 snumber title tags target extra-targets extra-class)
  (org-parse-begin-outline-text level1 snumber extra-class))

(defun org-parse-end-outline-text-or-outline ()
  (cond
   (org-parse-outline-text-open
    (org-parse-end 'OUTLINE-TEXT)
    (setq org-parse-outline-text-open nil))
   (t (org-parse-end 'OUTLINE))))

(defun org-parse-begin-outline-text (level1 snumber extra-class)
  (assert (not org-parse-outline-text-open) t)
  (setq org-parse-outline-text-open t)
  (org-parse-begin 'OUTLINE-TEXT level1 snumber extra-class))

;;;_   , outline
(defun org-html-begin-outline (level1 snumber title tags
				      target extra-targets extra-class)
  (let* ((class (format "outline-%d" level1))
	 (class (if extra-class (concat  class " " extra-class) class))
	 (id (format "outline-container-%s"
		     (org-parse-suffix-from-snumber snumber)))
	 (extra (concat (when id (format " id=\"%s\"" id))
			(when class (format " class=\"%s\"" class)))))
    (org-html-insert-tag "<div%s>" extra)
    (insert
     (org-parse-format 'HEADING
		       (org-parse-format
			'HEADLINE title extra-targets tags snumber level1)
		       level1 target))))

(defun org-html-end-outline ()
  (org-html-insert-tag  "</div>"))

;;;_   , outline-text
(defun org-html-begin-outline-text (level1 snumber extra-class)
  (let* ((class (format "outline-text-%d" level1))
	 (class (if extra-class (concat  class " " extra-class) class))
	 (id (format "text-%s" (org-parse-suffix-from-snumber snumber)))
	 (extra (concat (when id (format " id=\"%s\"" id))
			(when class (format " class=\"%s\"" class)))))
    (org-html-insert-tag "<div%s>" extra)))

(defun org-html-end-outline-text ()
  (org-html-insert-tag "</div>"))

;;;_   , paragraph
(defun org-html-begin-paragraph (&optional style)
  (let* ((class (cdr (assoc style '((footnote . "footnote")
				    (verse . nil)))))
	 (extra (if class (format " class=\"%s\"" class) "")))
    (org-html-insert-tag "<p%s>" extra)))

(defun org-html-end-paragraph ()
  (insert "</p>"))

;;;_   , environment
(defun org-html-format-environment (style beg-end)
  (assert (memq style '(blockquote center verse fixedwidth quote native)) t)
  (case style
    (blockquote
     (case beg-end
       (BEGIN
	(org-parse-end-paragraph)
	(insert "<blockquote>\n")
	(org-parse-begin-paragraph))
       (END
	(org-parse-end-paragraph)
	(insert "\n</blockquote>\n")
	(org-parse-begin-paragraph))))
    (verse
     (case beg-end
       (BEGIN
	(org-parse-end-paragraph)
	(insert "\n<p class=\"verse\">\n")
	(setq org-par-open t))
       (END
	(insert "</p>\n")
	(setq org-par-open nil)
	(org-parse-begin-paragraph))))
    (center
     (case beg-end
       (BEGIN
	(org-parse-end-paragraph)
	(insert "\n<div style=\"text-align: center\">")
	(org-parse-begin-paragraph))
       (END
	(org-parse-end-paragraph)
	(insert "\n</div>")
	(org-parse-begin-paragraph))))
    (fixedwidth
     (case beg-end
       (BEGIN
	(org-parse-end-paragraph)
	(insert "<pre class=\"example\">\n"))
       (END
	(insert "</pre>\n")
	(org-parse-begin-paragraph))))
    (quote
     (case beg-end
       (BEGIN
	(org-parse-end-paragraph)
	(insert "<pre>"))
       (END
	(insert "</pre>\n")
	(org-parse-begin-paragraph))))
    (native
     (case beg-end
       (BEGIN (org-parse-end-paragraph))
       (END (org-parse-begin-paragraph))))
    (t (error "Unknown environment %s" style))))


(defun org-html-begin-environment (style)
  (org-html-format-environment style 'BEGIN))

(defun org-html-end-environment (style)
  (org-html-format-environment style 'END))


;;;_   , list
(defun org-html-html-list-type-to-canonical-list-type (ltype)
  (cdr (assoc ltype '(("o" . ordered)
		      ("u" . unordered)
		      ("d" . description)))))

(defun org-html-begin-list (ltype &optional arg1)
  (setq ltype (or (org-html-html-list-type-to-canonical-list-type ltype)
		  ltype))

  (case ltype
    (ordered (let ((extra (if arg1 (format " start=\"%d\"" arg1) "")))
	       (org-html-insert-tag "<ol%s>" extra)))
    (unordered (org-html-insert-tag "<ul>"))
    (description (org-html-insert-tag "<dl>"))
    (t (error "Unknown list type: %s"  ltype))))

(defun org-html-end-list (ltype)
  (setq ltype (or (org-html-html-list-type-to-canonical-list-type ltype)
		  ltype))

  (org-html-insert-tag
     (case ltype
       (ordered "</ol>")
       (unordered "</ul>")
       (description "</dl>")
       (t (error "Unknown list type: %s" ltype)))))

(defun org-html-begin-list-item (ltype &optional arg headline)
  (setq ltype (or (org-html-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (case ltype
    (ordered
     (assert (not headline) t)
     (let* ((counter arg)
	   (extra (if counter (format " value=\"%s\"" counter) "")))
       (org-html-insert-tag "<li%s>" extra)))
    (unordered
     (let* ((id arg)
	   (extra (if id (format " id=\"%s\"" id) "")))
       (org-html-insert-tag "<li%s>" extra)
       (when headline
	 (insert headline (org-parse-format 'LINE-BREAK) "\n"))))
    (description
     (assert (not headline) t)
     (let* ((desc-tag (or arg "(no term)")))
       (insert
	(org-html-format-tags '("<dt>" . "</dt>") desc-tag))
       (org-html-insert-tag "<dd>")))
    (t (error "Unknown list type"))))

(defun org-html-end-list-item (ltype)
  (setq ltype (or (org-html-html-list-type-to-canonical-list-type ltype)
		  ltype))
  (case ltype
    (ordered (org-html-insert-tag "</li>"))
    (unordered (org-html-insert-tag "</li>"))
    (description (org-html-insert-tag "</dd>"))
    (t (error "Unknown list type"))))


;;;_   , table

(defvar org-table-rowgroup-info)
(defun org-parse-begin-table-rowgroup (&optional is-header-row)
  (push (cons (1+ org-table-rownum) :start) org-table-rowgroup-info)
  (org-parse-begin 'TABLE-ROWGROUP is-header-row))

(defun org-html-begin-table-rowgroup (&optional is-header-row)
  (when org-table-rowgroup-open
    (org-parse-end 'TABLE-ROWGROUP))
  (org-html-insert-tag (if is-header-row "<thead>" "<tbody>"))
  (setq org-table-rowgroup-open t)
  (setq org-table-current-rowgroup-is-header is-header-row))

(defun org-html-end-table-rowgroup ()
  (when org-table-rowgroup-open
    (setq org-table-rowgroup-open nil)
    (org-html-insert-tag
     (if org-table-current-rowgroup-is-header "</thead>" "</tbody>"))))

(defun org-html-begin-table (caption label attributes)
  (let ((html-table-tag
	 (org-export-splice-attributes html-table-tag attributes)))
    (when label
      (setq html-table-tag
	    (org-export-splice-attributes
	     html-table-tag
	     (format "id=\"%s\"" (org-solidify-link-text label)))))
    (org-html-insert-tag html-table-tag))

  ;; Since the output of HTML table formatter can also be used in
  ;; DocBook document, we want to always include the caption to make
  ;; DocBook XML file valid.
  (insert (format "<caption>%s</caption>" (or caption "")) "\n"))

(defun org-parse-end-table ()
  (when org-table-is-styled
    ;; column groups
    (unless (car org-table-colgroup-info)
      (setq org-table-colgroup-info
	    (cons :start (cdr org-table-colgroup-info))))

    ;; column alignment
    (let ((c -1))
      (mapc
       (lambda (x)
	 (incf c)
	 (setf (aref org-table-colalign-vector c)
	       (or (aref org-table-colalign-vector c)
		   (if (> (/ (float x) (1+ org-table-rownum))
			  org-table-number-fraction)
		       "right" "left"))))
       org-table-num-numeric-items-per-column)))
  (org-parse-end 'TABLE))

(defun org-html-end-table ()
  (when org-table-is-styled
    (goto-char org-table-begin-marker)
    (setq org-table-begin-marker nil)

    (let ((c -1) gr colgropen)
      (insert
       (mapconcat
	(lambda (x)
	  (incf c)
	  (setq gr (pop org-table-colgroup-info))

	  (concat
	   (if (memq gr '(:start :startend))
	       (prog1
		   (if colgropen
		       "</colgroup>\n<colgroup>"
		     "<colgroup>")
		 (setq colgropen t))
	     "")

	   (let* ((align (aref org-table-colalign-vector c))
		  (alignspec (if org-html-format-table-no-css
				 " align=\"%s\"" " class=\"%s\""))
		  (extra (format alignspec  align)))
	     (format "<col%s />" extra))

	   (if (memq gr '(:end :startend))
	       (progn (setq colgropen nil) "</colgroup>")
	     "")))
	org-table-num-numeric-items-per-column ""))

      (if colgropen (insert "</colgroup>")))

    ;; fill style attributes for table cells
    (while (re-search-forward "@@class\\([0-9]+\\)@@" nil t)
      (let ((c (string-to-number (match-string 1))))
	(replace-match
	 (if org-export-html-table-align-individual-fields
	     (format (if org-html-format-table-no-css " align=\"%s\""
		       " class=\"%s\"")
		     (or (aref org-table-colalign-vector c) "left")) "")
	 t t)))
    (goto-char (point-max)))
  (org-html-insert-tag "</table>\n"))

(defun org-html-format-table-row (row)
  (org-html-format-tags
   (cons (eval (car org-export-table-row-tags))
	 (eval (cdr org-export-table-row-tags))) row))

(defun org-html-format-table-cell (text r c)
  (let ((cell-style-cookie (or (and org-table-is-styled
				    (format "@@class%03d@@" c)) "")))
    (cond
     (org-table-current-rowgroup-is-header
      (org-html-format-tags
       org-export-table-header-tags text  "col" cell-style-cookie))
     ((and (= c 0) org-export-html-table-use-header-tags-for-first-column)
      (org-html-format-tags
       org-export-table-header-tags text "row" cell-style-cookie))
     (t
      (org-html-format-tags
       org-export-table-data-tags text cell-style-cookie)))))


;;;_   , footnote definition
(defun org-html-begin-footnote-definition (n)
  (org-parse-begin-paragraph 'footnote)
  (insert
   (format
    (format org-export-html-footnote-format
	    "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>")
    n n n)))

(defun org-html-end-footnote-definition (n)
  (org-parse-end-paragraph))

;;;_  . format callbacks

(defvar org-html-protect nil)
;;;_   , spaces
(defun org-html-format-spaces (n)
  (let ((space (or (and org-html-protect "\\nbsp") "&nbsp;")) out)
    (while (> n 0)
      (setq out (concat out space))
      (setq n (1- n))) out))

;;;_   , tabs
(defun org-html-format-tabs (&optional n)
  (ignore))

;;;_   , line break
(defun org-html-format-line-break ()
  (org-html-format-tags "<br/>" ""))

;;;_   , horizontal line
(defun org-html-format-horizontal-line ()
  (concat  "\n" "<hr/>" "\n"))

;;;_   , line

(defun org-html-format-plain (line)
  (case org-parse-dyn-current-environment
    ((quote fixedwidth) (concat (org-html-protect line) "\n"))
    (t (concat line "\n"))))

(defun org-html-format-comment (fmt &rest args)
  (let ((comment (apply 'format fmt args)))
    (format "\n<!-- %s  -->\n" comment)))


;;;_   , character styles
(defun org-html-format-fontify (text style &optional id)
  (let (class extra how)
    (cond
     ((eq style 'underline)
      (setq extra " style=\"text-decoration:underline;\"" ))
     ((setq how (cdr (assoc style
			    '((bold . ("<b>" . "</b>"))
			      (emphasis . ("<i>" . "</i>"))
			      (code . ("<code>" . "</code>"))
			      (verbatim . ("<code>" . "</code>"))
			      (strike . ("<del>" . "</del>"))
			      (subscript . ("<sub>" . "</sub>"))
			      (superscript . ("<sup>" . "</sup>")))))))
     ((listp style)
      (setq class (mapconcat 'identity style " ")))
     ((stringp style)
      (setq class style))
     (t (error "Unknown style %S" style)))

    (setq extra (concat (when class (format " class=\"%s\"" class))
			(when id (format " id=\"%s\""  id))
			extra))
    (org-html-format-tags
     (or how '("<span%s>" . "</span>")) text extra)))

;;;_   , link
(defun org-html-format-link (text href &optional extra)
  (let ((extra (concat (format " href=\"%s\"" href)
		       (and extra (concat  " " extra)))))
    (org-html-format-tags '("<a%s>" . "</a>") text extra)))

;;;_   , heading
(defun org-html-format-heading (text level &optional id)
  (let* ((extra (concat (when id (format " id=\"%s\"" id)))))
    (concat (format "<h%d%s>" level extra) text (format "</h%d>" level))))

;;;_   , headline
(defun org-html-format-headline (title extra-targets tags
					    &optional snumber level)
  (concat
   (org-parse-format 'EXTRA-TARGETS extra-targets)
   (concat (org-parse-format 'SECTION-NUMBER snumber level) " ")
   title
   (and tags (concat (org-parse-format 'SPACES 3)
		     (org-parse-format 'ORG-TAGS tags)))))

;;;_   , anchor
(defun org-html-format-anchor (text name &optional class)
  (let* ((id name)
	 (extra (concat
		 (when name (format " name=\"%s\""  name))
		 (when id (format " id=\"%s\""  id))
		 (when class (format " class=\"%s\""  class)))))
    (org-html-format-tags '("<a%s>" . "</a>") text extra)))



;;;_   , target
;;;_   , footnote reference
(defun org-html-format-footnote-reference (n def refcnt)
  (let ((extra (if (= refcnt 1) "" (format ".%d"  refcnt))))
    (format org-export-html-footnote-format
	    (format
	     "<a class=\"footref\" name=\"fnr.%s%s\" href=\"#fn.%s\">%s</a>"
	     n extra n n))))



;;;_   , footnotes section
(defun org-html-format-footnotes-section (section-name definitions)
  (if (not definitions) ""
    (format org-export-html-footnotes-section section-name definitions)))


;;;_   , image
;;;_   , generic
(defun org-html-format-tags (tag text &rest args)
  (let ((prefix (when org-html-protect "@"))
	(suffix (when org-html-protect "@")))
    (cond
     ((consp tag)
      (concat prefix (apply 'format (car tag) args) text suffix
	      (format (cdr tag))))
     ((stringp tag)			; singleton tag
      (concat prefix (apply 'format tag args) text)))))

;;;_  . maintenance callbacks
;;;_   , init method
;;;_   , save method
;;;_   , cleanup method
;;;_  . get callback
(defun org-html-get (what &optional opt-plist)
  (case what
    (BACKEND 'html)
    (INIT-METHOD nil)
    (SAVE-METHOD nil)
    (CLEANUP-METHOD nil)
    (EXPORT-DIR (org-export-directory :html opt-plist))
    (FILE-NAME-EXTENSION (plist-get opt-plist :html-extension))
    (EXPORT-BUFFER-NAME "*Org HTML Export*")
    (ENTITY-CONTROL org-html-entity-control-callbacks-alist)
    (ENTITY-FORMAT org-html-entity-format-callbacks-alist)
    (TOPLEVEL-HLEVEL org-export-html-toplevel-hlevel)
    (SPECIAL-STRING-REGEXPS org-export-html-special-string-regexps)
    (t (error "Unknown property: %s"  what))))

;;;_   , coding system
(defun org-html-get-coding-system-for-write ()
  (or org-export-html-coding-system
      (and (boundp 'buffer-file-coding-system) buffer-file-coding-system)))

(defun org-html-get-coding-system-for-save ()
  (or org-export-html-coding-system
      (and (boundp 'buffer-file-coding-system) buffer-file-coding-system)))


;;;_. newhtml (non-parser & misc)
(defun org-html-insert-toc (toc)
  ;; locate where toc needs to be inserted
  (goto-char (point-min))
  (cond
   ((or (re-search-forward "<p>\\s-*\\[TABLE-OF-CONTENTS\\]\\s-*</p>" nil t)
	(re-search-forward "\\[TABLE-OF-CONTENTS\\]" nil t))
    (goto-char (match-beginning 0))
    (replace-match "")
    (insert toc))
   (org-html-dyn-first-heading-pos
    (goto-char org-html-dyn-first-heading-pos)
    (when (looking-at "\\s-*</p>")
      (goto-char (match-end 0))
      (insert "\n"))
    (insert toc))
   (t (ignore))))

(defun org-html-insert-preamble (opt-plist)
  (when (plist-get opt-plist :html-preamble)
    (let ((html-pre (plist-get opt-plist :html-preamble))
	  (title (plist-get opt-plist :title))
	  (date (plist-get opt-plist :effective-date))
	  (author (plist-get opt-plist :author))
	  (lang-words (plist-get opt-plist :lang-words))
	  (email (plist-get opt-plist :email)))
      (cond ((stringp html-pre)
	     (insert
	      (format-spec html-pre `((?t . ,title) (?a . ,author)
				      (?d . ,date) (?e . ,email)))))
	    ((functionp html-pre)
	     (funcall html-pre opt-plist))
	    (t
	     (insert
	      (format-spec
	       (or (cadr (assoc (nth 0 lang-words)
				org-export-html-preamble-format))
		   (cadr (assoc "en" org-export-html-preamble-format)))
	       `((?t . ,title) (?a . ,author)
		 (?d . ,date) (?e . ,email)))))))))

(defun org-html-insert-postamble (opt-plist)
  (when org-html-footnote-definitions
    (insert
     (org-parse-format
      'FOOTNOTES-SECTION (nth 4 (plist-get opt-plist :lang-words))
      (mapconcat (lambda (x) (cdr x))
		 (nreverse org-html-footnote-definitions) "\n"))))
  (let ((bib (org-export-html-get-bibliography)))
    (when bib
      (insert "\n" bib "\n")))

  ;; export html postamble
  (unless body-only
    (let* ((html-post (plist-get opt-plist :html-postamble))
	   (date (plist-get opt-plist :effective-date))
	   (author (plist-get opt-plist :author))
	   (email  (plist-get opt-plist :email))
	   (lang-words (plist-get opt-plist :lang-words))
	   (html-validation-link (or org-export-html-validation-link ""))
	   (email
	    (mapconcat (lambda(e)
			 (format "<a href=\"mailto:%s\">%s</a>" e e))
		       (split-string email ",+ *")
		       ", "))
	   (creator-info
	    (concat "Org version " org-version " with Emacs version "
		    (number-to-string emacs-major-version))))
      (when (plist-get opt-plist :html-postamble)
	(cond ((stringp html-post)
	       (insert "<div id=\"postamble\">\n")
	       (insert (format-spec html-post
				    `((?a . ,author) (?e . ,email)
				      (?d . ,date)   (?c . ,creator-info)
				      (?v . ,html-validation-link))))
	       (insert "</div>"))
	      ((functionp html-post)
	       (funcall html-post opt-plist))
	      ((eq html-post 'auto)
	       ;; fall back on default postamble
	       (insert "<div id=\"postamble\">\n")
	       (when (plist-get opt-plist :time-stamp-file)
		 (insert "<p class=\"date\">" (nth 2 lang-words) ": " date "</p>\n"))
	       (when (and (plist-get opt-plist :author-info) author)
		 (insert "<p class=\"author\">" (nth 1 lang-words) ": " author "</p>\n"))
	       (when (and (plist-get opt-plist :email-info) email)
		 (insert "<p class=\"email\">" email "</p>\n"))
	       (when (plist-get opt-plist :creator-info)
		 (insert "<p class=\"creator\">"
			 (concat "Org version " org-version " with Emacs version "
				 (number-to-string emacs-major-version) "</p>\n")))
	       (insert html-validation-link "\n</div>"))
	      (t
	       (insert "<div id=\"postamble\">\n")
	       (insert (format-spec
			(or (cadr (assoc (nth 0 lang-words)
					 org-export-html-postamble-format))
			    (cadr (assoc "en" org-export-html-postamble-format)))
			`((?a . ,author) (?e . ,email)
			  (?d . ,date)   (?c . ,creator-info)
			  (?v . ,html-validation-link))))
	       (insert "</div>"))))))

  (if org-export-html-with-timestamp
      (insert org-export-html-html-helper-timestamp)))

(defun org-html-format-todo (todo)
  (org-parse-format 'FONTIFY
		   (org-export-html-get-todo-kwd-class-name todo)
		   (list (if (member todo org-done-keywords) "done" "todo")
			 todo)))

(defun org-html-format-extra-targets (extra-targets)
  (if (not extra-targets) ""
      (mapconcat (lambda (x)
	       (setq x (org-solidify-link-text
			(if (org-uuidgen-p x) (concat "ID-" x) x)))
	       (org-parse-format 'ANCHOR "" x))
	     extra-targets "")))

(defun org-html-format-org-tags (tags)
  (if (not tags) ""
    (org-parse-format
     'FONTIFY (mapconcat
	       (lambda (x)
		 (org-parse-format
		  'FONTIFY x (org-export-html-get-tag-class-name x)))
	       (org-split-string tags ":")
	       (org-parse-format 'SPACES 1)) "tag")))

(defun org-html-format-section-number (&optional snumber level)
  (and org-export-with-section-numbers
       (not body-only) snumber level
       (org-parse-format 'FONTIFY snumber (format "section-number-%d" level))))

(defun org-parse-warn (msg)
  (put-text-property 0 (length msg) 'face 'font-lock-warning-face msg)
  (message msg)
  (sleep-for 3))

;;;_. preprocessor
;;;_. postamble

(provide 'org-html)

;; arch-tag: 8109d84d-eb8f-460b-b1a8-f45f3a6c7ea1
;;; org-html.el ends here
