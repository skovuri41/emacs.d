;;; org-src.el --- org src babel                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  shyam

;; Author: shyam <shyam@hanuman>
;; Keywords:

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (js         . t)
   (emacs-lisp . t)
   (ditaa      . t)
   (ledger     . t)
   (perl       . t)
   (clojure    . t)
   (python     . t)
   (ruby       . t)
   (dot        . t)
   (css        . t)
   (plantuml   . t)))

;; Let's have pretty source code blocks
(setq org-edit-src-content-indentation 0
               org-src-tab-acts-natively t
               org-src-fontify-natively t
               org-confirm-babel-evaluate nil)
;; preserve the indentation inside of source blocks
(validate-setq org-src-preserve-indentation t)
(validate-setq org-confirm-babel-evaluate nil)
;; how org-src windows are set up when hitting C-c '
(validate-setq org-src-window-setup 'current-window)
;; blank lines are removed when exiting the code edit buffer
(validate-setq org-src-strip-leading-and-trailing-blank-lines t)
(validate-setq org-src-fontify-natively t)

(add-to-list 'org-structure-template-alist '("A" "#+DATE: ?"))
(add-to-list 'org-structure-template-alist '("C" "#+BEGIN_CENTER\n?\n#+END_CENTER\n"))
(add-to-list 'org-structure-template-alist '("D" "#+DESCRIPTION: ?"))
(add-to-list 'org-structure-template-alist '("E" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE\n"))
(add-to-list 'org-structure-template-alist '("H" "#+LATEX_HEADER: ?"))
(add-to-list 'org-structure-template-alist '("I" ":INTERLEAVE_PDF: ?"))
(add-to-list 'org-structure-template-alist '("L" "#+BEGIN_LaTeX\n?\n#+END_LaTeX"))
(add-to-list 'org-structure-template-alist '("M" "#+LATEX_HEADER: \\usepackage{minted}\n"))
(add-to-list 'org-structure-template-alist '("N" "#+NAME: ?"))
(add-to-list 'org-structure-template-alist '("P" "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"org.css\"/>\n"))
(add-to-list 'org-structure-template-alist '("S" "#+SUBTITLE: ?"))
(add-to-list 'org-structure-template-alist '("T" ":DRILL_CARD_TYPE: twosided"))
(add-to-list 'org-structure-template-alist '("V" "#+BEGIN_VERSE\n?\n#+END_VERSE"))
(add-to-list 'org-structure-template-alist '("X" "#+EXCLUDE_TAGS: reveal?"))
(add-to-list 'org-structure-template-alist '("a" "#+AUTHOR: ?"))
(add-to-list 'org-structure-template-alist '("c" "#+CAPTION: ?"))
(add-to-list 'org-structure-template-alist '("d" "#+OPTIONS:
  ':nil *:t -:t ::t <:t H:3 \\n:nil ^:t arch:headline\n#+OPTIONS:
  author:t email:nil e:t f:t inline:t creator:nil d:nil
  date:t\n#+OPTIONS: toc:nil num:nil tags:nil todo:nil p:nil
  pri:nil stat:nil c:nil d:nil\n#+LATEX_HEADER:
  \\usepackage[margin=2cm]{geometry}\n#+LANGUAGE:
  en\n\n#+REVEAL_TRANS: slide\n#+REVEAL_THEME:
  white\n#+REVEAL_ROOT:
  file:///Users/Documents/workspace/github/reveal.js\n\n?"))
(add-to-list 'org-structure-template-alist '("e" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist '("f" "#+TAGS: @?"))
(add-to-list 'org-structure-template-alist '("h" "#+BEGIN_HTML\n?\n#+END_HTML\n"))
(add-to-list 'org-structure-template-alist '("i" "#+INTERLEAVE_PDF: ?"))
(add-to-list 'org-structure-template-alist '("k" "#+KEYWORDS: ?"))
(add-to-list 'org-structure-template-alist '("l" "#+LABEL: ?"))
(add-to-list 'org-structure-template-alist '("m" "#+BEGIN_SRC matlab\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist '("n" "#+BEGIN_NOTES\n?\n#+END_NOTES"))
(add-to-list 'org-structure-template-alist '("o" "#+OPTIONS: ?"))
(add-to-list 'org-structure-template-alist '("p" "#+BEGIN_SRC python\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist '("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE"))
(add-to-list 'org-structure-template-alist '("r" ":PROPERTIES:\n?\n:END:"))
(add-to-list 'org-structure-template-alist '("s" "#+BEGIN_SRC ?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("t" "#+TITLE: ?"))
(add-to-list 'org-structure-template-alist '("v" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM"))


(unless (boundp 'org-babel-default-header-args:sh)
  (validate-setq org-babel-default-header-args:sh '()))

;;add a default shebang header argument for shell scripts
(add-to-list 'org-babel-default-header-args:sh
             '(:shebang . "#!/usr/bin/env bash"))

;;add a default shebang header argument for python
(add-to-list 'org-babel-default-header-args:python
             '(:shebang . "#!/usr/bin/env python"))

;; ob-clojure
(use-package ob-clojure
  :config
  (validate-setq org-babel-clojure-backend 'cider)
  ;; Clojure-specific org-babel stuff
  (defvar org-babel-default-header-args:clojure
    '((:results . "silent")))
  (defun org-babel-execute:clojure (body params)
    "Execute a block of Clojure code with Babel."
    (let ((result-plist
           (nrepl-send-string-sync
            (org-babel-expand-body:clojure body params) nrepl-buffer-ns))
          (result-type (cdr (assoc :result-type params))))
      (org-babel-script-escape
       (cond ((eq result-type 'value) (plist-get result-plist :value))
             ((eq result-type 'output) (plist-get result-plist :value))
             (t (message "Unknown :results type!")))))))

(provide 'org-src)
