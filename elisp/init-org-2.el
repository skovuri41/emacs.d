(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-startup-folded t)
  (setq org-startup-indented nil)
  (setq org-startup-with-inline-images t)
  (setq org-startup-truncated t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-confirm-babel-evaluate nil)
  (setq org-use-speed-commands t)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-html-postamble nil)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-habit-graph-column 60)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "WAITING(w)"
           "SCHEDULED(s)"
           "FUTURE(f)"
           "|"
           "DONE(d)")))
  (setq org-todo-keyword-faces
        '(("SCHEDULED" . warning)
          ("WAITING" . font-lock-doc-face)
          ("FUTURE" . "white")))
  (setq org-log-into-drawer t) ;don't clutter files with state logs

  :config
  (require 'ox-org)
  (require 'ox-md)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-structure-template-alist
               '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist
               '("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n"))

  ;; Don't use the same TODO state as the current heading for new heading
  (defun my-org-insert-todo-heading ()
    (interactive)
    (org-insert-todo-heading t))
  (define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)

;;;;; org-agenda
  (use-package org-agenda
    :init
    (setq org-agenda-start-with-log-mode t)
    (setq org-agenda-todo-ignore-scheduled 'future) ;don't show future scheduled
    (setq org-agenda-todo-ignore-deadlines 'far)    ;show only near deadlines

    :config
    ;; add state to the sorting strategy of todo
    (setcdr (assq 'todo org-agenda-sorting-strategy) '(todo-state-up priority-down category-keep))

    ;; create the file for the agendas if it doesn't exist
    (let ((agendas "~/.agenda_files"))
      (unless (file-readable-p agendas)
        (with-temp-file agendas nil))
      (setq org-agenda-files agendas))

    ;; display the agenda first
    (setq org-agenda-custom-commands
          '(("n" "Agenda and all TODO's"
             ((alltodo "")
              (agenda "")))))

    ;; add new appointments when saving the org buffer, use 'refresh argument to do it properly
    (defun my-org-agenda-to-appt-refresh () (org-agenda-to-appt 'refresh))
    (defun my-org-mode-hook ()
      (add-hook 'after-save-hook 'my-org-agenda-to-appt-refresh nil 'make-it-local))
    (add-hook 'org-mode-hook 'my-org-mode-hook))

  (use-package notifications
    :config
    (defun my-appt-disp-window-function (min-to-app new-time msg)
      (notifications-notify :title (format "Appointment in %s min" min-to-app) :body msg))
    (setq appt-disp-window-function 'my-appt-disp-window-function)
    (setq appt-delete-window-function (lambda (&rest args))))

;;;;; org-capture
  (use-package org-capture
    :init
    (setq org-capture-templates
          '(("t" "Task" entry (file "") "* TODO %?\n %a")
            ("s" "Simple Task" entry (file "") "* TODO %?\n"))))

;;;;; org-clock
  (use-package org-clock
    :init
    (setq org-clock-idle-time 15)
    (setq org-clock-in-resume t)
    (setq org-clock-persist t)
    (setq org-clock-persist-query-resume nil)
    (setq org-clock-clocked-in-display 'both)
    (setq org-clock-frame-title-format
          (append '((t org-mode-line-string)) '(" ") frame-title-format))
    (when (executable-find "xprintidle")
      (setq org-x11idle-exists-p t)
      (setq org-clock-x11idle-program-name "xprintidle"))

    :config (org-clock-persistence-insinuate))

;;;;; ox-latex
  (use-package ox-latex
    :config
    (setq org-latex-listings 'minted)
    (setq org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    (add-to-list 'org-latex-packages-alist '("" "minted"))))
