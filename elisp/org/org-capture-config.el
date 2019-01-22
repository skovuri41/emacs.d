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

;; Org todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
        (sequence "APPT(p)" "|" "DONE(d)" "CANCELLED(a)")))

;; Org faces
(validate-setq org-todo-keyword-faces
               '(("TODO" :foreground "red" :weight bold)
                 ("DONE" :foreground "forest green" :weight bold)
                 ("APPT" :foreground "magenta" :weight bold)
                 ("CANCELLED" :foreground "forest green" :weight bold)))


;;; refer todo types to assign to people
;;; https://orgmode.org/manual/TODO-types.html#TODO-types

;; add or remove tags on state change
(validate-setq org-todo-state-tags-triggers
               '(("CANCELLED" ("CANCELLED" . t))
                 ("DONE" ("WAITING"))
                 ("TODO" ("WAITING") ("CANCELLED"))))

;; quick access to common tags
(setq org-tag-alist
      '(("oss" . ?o)
        ("home" . ?h)
        ("work" . ?w)
        ("book" . ?b)
        ("support" . ?s)
        ("docs" . ?d)
        ("emacs" . ?e)
        ("noexport" . ?n)
        ("recurring" . ?r)))

;;; refer
;;; https://orgmode.org/manual/Tag-hierarchy.html#Tag-hierarchy



;; org-capture
(require 'org-contacts)
(setq org-capture-templates '())
(progn
  (add-to-list 'org-capture-templates
               '("t" "Todo [inbox]" entry (file+headline org-default-notes-file "Tasks")
                 "* TODO %i%?"))
  (add-to-list 'org-capture-templates
               '("l" "Todo (with link) [inbox]" entry (file+headline org-default-notes-file "Tasks")
                 "* TODO %a"))
  (add-to-list 'org-capture-templates
               '("p" "Appointment" entry (file+headline my/org-agenda-file "Appointment")
                 "* APPT %i%? \n %^T"))
  (add-to-list 'org-capture-templates
               '("T" "Tickler" entry (file+headline "~/org/tickler.org" "Tickler")
                 "* %i%? \n %^t"))
  (add-to-list 'org-capture-templates
               '("c" "Contacts" entry (file "~/org/contacts.org")
                 "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")))

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

;; make the frame contain a single window. by default org-capture
;; splits the window.
(add-hook 'org-capture-mode-hook
          'delete-other-windows)


(provide 'org-capture-config)
;;; org-capture-config.el ends here
