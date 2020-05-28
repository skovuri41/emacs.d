;;; org-agenda-config.el --- org agenda                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  shyam

;; Author: shyam <shyam@hanuman>
;; Keywords: abbrev,

;;; Code:

(setq org-agenda-files `(,org-default-notes-file
                         "~/org/agenda.org"
                         "~/org/gtd.org"
                         "~/org/diary.org"
                         "~/org/tickler.org"))
;; "~/org/someday.org" is not part of agenda files
;; [[https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html][Orgmode for GTD]]


(setq org-refile-targets `(("~/org/gtd.org" :maxlevel . 3)
                           ("~/org/diary.org" :level . 3)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/agenda.org" :level . 1)
                           ("~/org/tickler.org" :maxlevel . 2)
                           ("~/org/inbox.org" :maxlevel . 1)))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-agenda-start-with-log-mode t)
(validate-setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-include-all-todo t)
(validate-setq org-agenda-include-diary t)
;;(validate-setqorg-agenda-ndays7)
(validate-setq org-agenda-skip-deadline-if-done t)
(validate-setq org-agenda-skip-scheduled-if-done t)
;;;add state to the sorting strategy of todo
(setcdr (assq 'todo org-agenda-sorting-strategy)
        '(todo-state-up priority-down category-keep))
(setq
 ;; Sorting order for tasks on the agenda
 org-agenda-sorting-strategy
 '((agenda habit-down
           time-up
           priority-down
           user-defined-up
           effort-up
           category-keep)
   (todo priority-down category-up effort-up)
   (tags priority-down category-up effort-up)
   (search priority-down category-up))

 ;; Enable display of the time grid so we can see the marker for the
 ;; current time
 org-agenda-time-grid
 '((daily today remove-match)
   #("----------------" 0 16 (org-heading t))
   (900 1100 1300 1500 1700))
 ;; keep the agenda filter until manually removed
 org-agenda-persistent-filter t
 ;; show all occurrences of repeating tasks
 org-agenda-repeating-timestamp-show-all t
 ;; always start the agenda on today
 org-agenda-start-on-weekday nil
 ;; Use sticky agenda's so they persist
 org-agenda-sticky t
 ;; show 4 agenda days
 org-agenda-span 4
 ;; Compact the block agenda view
 org-agenda-compact-blocks t
 ;; Show all agenda dates - even if they are empty
 org-agenda-show-all-dates t)

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda ()
;;              (xah-fly-insert-mode-activate)))

;; (add-hook 'org-agenda-finalize-hook
;;           '(lambda ()
;;              (xah-fly-command-mode-activate)))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(defun my/save-all-agenda-buffers ()
  "Function used to save all agenda buffers that are
         currently open, based on `org-agenda-files'."
  (interactive)
  (save-current-buffer
    (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (member (buffer-file-name)
                    (mapcar 'expand-file-name (org-agenda-files t)))
        (save-buffer)))))

;; save all the agenda files after each capture
(add-hook 'org-capture-after-finalize-hook 'my/save-all-agenda-buffers)


(use-package org-super-agenda
  :ensure t
  :diminish
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config)



(provide 'org-agenda-config)
