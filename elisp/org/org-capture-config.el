;;; org-capture-config.el --- org capture                   -*- lexical-binding: t; -*-

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

(require 'org-protocol)
(require 'org-capture)
;; Org todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROGRESS(i)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "APPT(p)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; Org faces
(validate-setq org-todo-keyword-faces
               '(("TODO" :foreground "red" :weight bold)
                 ("INPROGRESS" :foreground "#9f7e7e" :weight normal :underline t)
                 ("DONE" :foreground "forest green" :weight bold)
                 ("APPT" :foreground "magenta" :weight bold)
                 ("CANCELLED" :foreground "forest green" :weight bold)))


;;; refer todo types to assign to people
;;; https://orgmode.org/manual/TODO-types.html#TODO-types

;; add or remove tags on state change
(validate-setq org-todo-state-tags-triggers
               '(("CANCELLED" ("CANCELLED" . t))
                 ("DONE" ("CANCELLED"))
                 ("APPT" ("CANCELLED"))
                 ("TODO" ("CANCELLED"))))

;; quick access to common tags
(setq org-tag-alist
      '(("oss" . ?o)
        ("home" . ?h)
        ("work" . ?w)
        ("book" . ?b)
        ("office" . ?o)
        ("docs" . ?d)
        ("emacs" . ?x)
        ("errand" . ?e)
        ("noexport" . ?n)
        ("recurring" . ?r)))

;;; refer
;;; https://orgmode.org/manual/Tag-hierarchy.html#Tag-hierarchy

;; org-capture
(require 'org-contacts)
(setq org-capture-templates '())
(setq org-capture-templates
      (quote (
              ("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
               "* TODO %? %:description %^g
:PROPERTIES:
:CREATED: %U
:END:
" :clock-in t :clock-resume t :empty-lines 1)

              ("n" "Note" entry (file+headline org-default-notes-file "Notes")
               "* %? :NOTE:
:PROPERTIES:
:CREATED: %U
:END:
%a
" :clock-in t :clock-resume t :empty-lines 1)

              ("a" "Appointment" entry (file+headline "~/org/agenda.org" "Appointment")
               "* APPT %i%? \n %^T")

              ("T" "Tickler" entry (file+headline "~/org/tickler.org" "Tickler")
               "* %i%? \n %^t")

              ("c" "Contacts" entry (file "~/org/contacts.org")
               "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")

              ("b" "Link from browser" entry (file+headline org-default-notes-file "Bookmarks")
               "* TODO %? %:description %^g
:PROPERTIES:
:CREATED: %U
:SOURCE:
:END:
%(org-cliplink-capture)
")
              ("s" "Code Snippet" entry
               (file+headline org-default-notes-file "Code")
               ;; Prompt for tag and language
               "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")

              ("j" "Journal" entry (file+olp+datetree "~/org/diary.org")
               "* %?
:PROPERTIES:
:CREATED: %U
:END:
" :clock-in t :clock-resume t :empty-lines 1))))

(add-hook 'org-capture-mode-hook
          '(lambda ()
             (xah-fly-insert-mode-activate)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (xah-fly-command-mode-activate))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (xah-fly-command-mode-activate))

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (progn (delete-frame)
             (xah-fly-command-mode-activate))
    (xah-fly-command-mode-activate)))

(defvar my-org-capture-before-config nil
  "Window configuration before `org-capture'.")

(defadvice org-capture (before save-config activate)
  "Save the window configuration before `org-capture'."
  (setq my-org-capture-before-config (current-window-configuration)))

(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun my-org-capture-cleanup ()
  "Clean up the frame created while capturing via org-protocol."
  ;; In case we run capture from emacs itself and not an external app,
  ;; we want to restore the old window config
  (when my-org-capture-before-config
    (set-window-configuration my-org-capture-before-config))
  (-when-let ((&alist 'name name) (frame-parameters))
    (when (memq name '("org-protocol-capture" "emacs-capture"))
      (delete-frame))))

(add-hook 'org-capture-after-finalize-hook 'my-org-capture-cleanup)

(provide 'org-capture-config)
;;; org-capture-config.el ends here
