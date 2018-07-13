;;; org-publish.el --- org publish                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  shyam

;; Author: shyam <shyam@hanuman>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package ox-latex
  :config
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-packages-alist '("" "minted")))

(use-package ox-reveal
  :disabled t
  :config
  (setq org-reveal-root (concat "file://" (getenv "HOME") "/Public/js/reveal.js"))
  (setq org-reveal-postamble "ox reveal presentation"))

(use-package org-present
  :disabled t
  :defer 20
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

(use-package org
  :config
  (setq org-html-postamble nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-coding-system 'utf-8)
  (setq org-export-with-toc nil)
  (setq org-export-with-timestamps nil)
  (setq org-html-head-extra "
     <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic&subset=latin,latin-ext' rel='stylesheet' type='text/css'>
     <link href='http://fonts.googleapis.com/css?family=Source+Code+Pro:400,700' rel='stylesheet' type='text/css'>
     <style type='text/css'>
        body {
           font-family: 'Source Sans Pro', sans-serif;
        }
        pre, code {
           font-family: 'Source Code Pro', monospace;
        }
     </style>")

  (defun my/org-inline-css-hook (exporter)
    "Insert custom inline css to automatically set the
   background of code to whatever theme I'm using's background"
    (when (eq exporter 'html)
      (let* ((my-pre-bg (face-background 'default))
             (my-pre-fg (face-foreground 'default)))
        ;;(setq org-html-head-include-default-style nil)
        (setq
         org-html-head-extra
         (concat
          org-html-head-extra
          (format
           "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
           my-pre-bg my-pre-fg))))))

  ;; (add-hook 'org-export-before-processing-hook #'my/org-inline-css-hook)
  )

(provide 'org-publish)
;;; org-publish.el ends here

