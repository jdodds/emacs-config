                OpenDocumentText Exporter for Orgmode
                =====================================

Author: Jambunathan K 
Date: 2011-05-19 04:13:10 




Table of Contents
=================
1 Summary 
2 Implementation Details 
3 Package Layout 
4 Obtaining OpenDocumentExporter 
    4.1 git checkout 
    4.2 Conventional tar 
    4.3 ELPA Tarball 
5 Test driving the Exporter 
6 Bug Reports and Feature Requests 
7 Frequently Asked Questions 
    7.1 What features does the OpenDocumentExporter support? 
    7.2 Is OpenDocumentExporter part of Orgmode or Emacs? 
    7.3 How can I export to doc or docx format? 


1 Summary 
----------
  
  This package adds support for exporting of Orgmode files to
  OpenDocumentText.

  The latest version of this document is available at 

  Text version: [http://repo.or.cz/w/org-mode/org-jambu.git/blob\_plain/HEAD:/doc/ReleaseNotes.org]
  Web page: [http://repo.or.cz/w/org-mode/org-jambu.git/blob\_plain/HEAD:/packages/README.html]


  [http://repo.or.cz/w/org-mode/org-jambu.git/blob\_plain/HEAD:/doc/ReleaseNotes.org]: http://repo.or.cz/w/org-mode/org-jambu.git/blob_plain/HEAD:/doc/ReleaseNotes.org
  [http://repo.or.cz/w/org-mode/org-jambu.git/blob\_plain/HEAD:/packages/README.html]: http://repo.or.cz/w/org-mode/org-jambu.git/blob_plain/HEAD:/packages/README.html

2 Implementation Details 
-------------------------

  This package enhances Orgmode in the following manner:
  1. A new line-oriented generic exporter
  2. All new html exporter re-implemented as a plugin to (1).
  3. A odt backend as a plugin to (1).

  Features (1) and (2) are provided by lisp/org-html.el while (3) is
  provided by lisp/org-odt.el.

  The new html exporter is feature-compatible with the official html
  exporter.

3 TODO Package Layout 
----------------------


4 Obtaining OpenDocumentExporter 
---------------------------------

  The OpenDocumentExporter could be downloaded by one of the following
  methods:

4.1 git checkout 
=================
   Checkout URL:  [http://repo.or.cz/r/org-mode/org-jambu.git]
   Web URL: [http://repo.or.cz/w/org-mode/org-jambu.git/]

4.2 Conventional tar 
=====================
   Download URL: [http://repo.or.cz/w/org-mode/org-jambu.git/snapshot/HEAD.tar.gz]

4.3 ELPA Tarball 
=================
   Archive URL: [http://repo.or.cz/w/org-mode/org-jambu.git/blob\_plain/HEAD:/packages/]
                      
   The tarball is distributed as an org-odt package (for example
   org-odt-20110519.tar).

   *WARNING*: If you have installed org through ELPA, delete or
   uninstall the =org-YYYYMMDD= package before installing the
   =org-odt-YYYYMMDD= package.
   
   The most hassle-free way to download and install org-odt is through
   ELPA.

   More help on all the above methods are available at
   [http://orgmode.org/worg/org-faq.html].


   [http://repo.or.cz/w/org-mode/org-jambu.git/blob\_plain/HEAD:/packages/]: http://repo.or.cz/w/org-mode/org-jambu.git/blob_plain/HEAD:/packages/

5 Test driving the Exporter 
----------------------------

  Once the package is installed in to your load-path, use 
  =C-u M-x org-odt-unit-test= to visit an example org file bundled
  with this package. Then Use =C-c C-e O= to export the buffer to
  OpenDocumentText.

6 Bug Reports and Feature Requests 
-----------------------------------

  Send in your bug report and feature requests to
  emacs-orgmode@gnu.org or to kjambunathan at gmail dot com.

  Please search the Mailing List Archive -
  [http://lists.gnu.org/archive/html/emacs-orgmode/] before posting a
  question or a request either to me or the mailing list.


7 Frequently Asked Questions 
-----------------------------

7.1 What features does the OpenDocumentExporter support? 
=========================================================

   At the moment, the exporter supports the following most commonly
   used features of Org

   - Various Character Styles
   - Various Paragraph Styles (including Source Blocks)
   - Embedded ODT
   - Embedded MathML
   - Numbered, Bulleted and Description lists
   - Embedding and Resizing of Images including embedding of LaTeX fragments
   - Fuzzy, Dedicated and Radio Targets and Links
   - Tables
   - Footnotes
   - Outline Numbering and Table Of Contents
   - Special markups for elements like TODOs, Timestamps and Tags

   The exporter is quite usable and stable.

7.2 Is OpenDocumentExporter part of Orgmode or Emacs? 
======================================================

   Not yet. I have expressed my willingness to merge this package in
   to official Orgmode and thus to Emacs. I have 

   The current maintainer of Orgmode - Bastien Guerry bzg at gnu.org -
   has agreed to consider the package for integration.

   If you are interested in having this package merged with Orgmode
   send your requests to the maintainer.

   For the sake of record, I am the sole author of the changes
   included in this package and I am consenting to have this work or
   derivative works make it's way into Emacs proper. My FSF copyright
   assignment number is #618390.

7.3 How can I export to doc or docx format? 
============================================
   
   Many applications and tools support exporting of odt file to
   numerous formats (including pdf and doc formats).

   Choose one of soffice, unoconv, PyODConverter, JODConverter
   according to your tastes.

   If you are interested in using soffice as the converter (and would
   rather not install yet another package), then refer to the
   following post for some recipes and inspiration.

   [http://lists.gnu.org/archive/html/emacs-devel/2011-05/msg00239.html]
