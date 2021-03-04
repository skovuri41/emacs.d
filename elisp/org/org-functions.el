;;; org-functions.el --- org functions               -*- lexical-binding: t; -*-

(defun set-org-mode-app-defaults ()
  (setq org-file-apps
        '((auto-mode . emacs)
          (directory . emacs)
          ("\\.org\\'" . emacs)
          ("\\.txt\\'" . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . system)
          ("\\.pdf\\'" . system))))

(defun clever-insert-item ()
  "Clever insertion of org item."
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item)))

(defun org-eol-call (fun)
  "Go to end of line and call provided function.
FUN function callback"
  (end-of-line)
  (funcall fun))

(defun org-goto-refile-target ()
  (interactive)
  (find-file org-default-notes-file))

(defun ora-org-schedule-today ()
  (interactive)
  (org-agenda-schedule 0 "+0d"))

(defun ora-org-schedule-tomorrow ()
  (interactive)
  (org-agenda-schedule 0 "+1d"))

(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))

;; Don't use the same TODO state as the current heading for new heading
(defun my-org-insert-todo-heading ()
  (interactive)
  (org-insert-todo-heading t))


(defun meeting-notes ()
  "Call this after creating an org-mode heading for where the notes for the meeting
     should be. After calling this function, call 'meeting-done' to reset the environment."
  (interactive)
  (outline-mark-subtree) ;; Select org-mode section
  (narrow-to-region (region-beginning) (region-end)) ;; Only show that region
  (deactivate-mark)
  (delete-other-windows) ;; Get rid of other windows
  (text-scale-set 2)     ;; Text is now readable by others
  (fringe-mode 0)
  (message "When finished taking your notes, run meeting-done."))

(defun meeting-done ()
  "Attempt to 'undo' the effects of taking meeting notes."
  (interactive)
  (widen)            ;; Opposite of narrow-to-region
  (text-scale-set 0) ;; Reset the font size increase
  (fringe-mode 1)
  (winner-undo))

(defun org-text-bold () "Wraps the region with asterisks."
       (interactive)
       (surround-text "*"))
(defun org-text-italics () "Wraps the region with slashes."
       (interactive)
       (surround-text "/"))
(defun org-text-code () "Wraps the region with equal signs."
       (interactive)
       (surround-text "="))

(defun my-org-imenu (&optional arg)
  "Like `counsel-imenu'.
With a prefix argument ARG, narrow the
buffer to the matched subtree."
  (interactive "P")
  (counsel-imenu)
  (when arg
    (org-narrow-to-subtree)))

(defun my/find-notes-file ()
  (interactive)
  (find-file org-default-notes-file))


(add-hook 'before-save-hook
          #'my/org-align-all-tags)

(defun my/org-align-all-tags ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-align-all-tags)))

(defun my/org-update-parent-cookie ()
  (when (eq major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (my/org-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (my/org-update-parent-cookie))

(defadvice org-archive-subtree (after fix-cookies activate)
  (my/org-update-parent-cookie))

(defun my/org-export-buffer ()
  (let* ((file-name (buffer-file-name))
         (file-exists (and file-name
                           (file-exists-p file-name))))
    (when (and file-name (eq major-mode 'org-mode))
      (org-export-to-file 'html (format "%s.html" (file-name-sans-extension file-name))))))

(defun my/org-archive-done-tasks ()
  "Archive all DONE entries"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun org-toggle-link-display ()
  "Toggle the literal or descriptive display of links."
  (interactive)
  (if org-descriptive-links
      (progn (org-remove-from-invisibility-spec '(org-link))
             (org-restart-font-lock)
             (setq org-descriptive-links nil))
    (progn (add-to-invisibility-spec '(org-link))
           (org-restart-font-lock)
           (setq org-descriptive-links t))))

(provide 'org-functions)
