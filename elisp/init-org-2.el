(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
  (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode)) ;; Journal entries
  (setq org-directory "~/notes/")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-replace-disputed-keys t)
  (setq org-startup-folded t)
  (setq org-startup-indented t)
  (setq org-startup-with-inline-images t)
  (setq org-startup-truncated t)
  (setq org-hide-leading-stars t)
  (setq org-odd-levels-only nil)
  (setq org-list-allow-alphabetical t)
  (setq org-cycle-include-plain-lists t)
  (setq org-cycle-separator-lines 0)
  (setq org-blank-before-new-entry (quote ((heading)
                                           (plain-list-item . auto))))
  (setq org-src-fontify-natively t)
  (setq org-use-speed-commands t)
  (setq org-hide-emphasis-markers t)
  (setq org-reverse-note-order nil)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-tags-column 0)
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
  ;; Block entries from changing state to DONE while they have children
  ;; that are not DONE
  (setq org-enforce-todo-dependencies t)
  (setq org-use-fast-todo-selection t)
  (defun set-org-mode-app-defaults ()
    (setq org-file-apps
          '(((auto-mode . emacs)
             ("\\.mm\\'" . default)
             ("\\.x?html?\\'" . system)
             ("\\.pdf\\'" . system)))))
  (add-hook 'org-mode-hook 'set-org-mode-app-defaults)
  (defun org-text-bold () "Wraps the region with asterisks."
         (interactive)
         (surround-text "*"))
  (defun org-text-italics () "Wraps the region with slashes."
         (interactive)
         (surround-text "/"))
  (defun org-text-code () "Wraps the region with equal signs."
         (interactive)
         (surround-text "="))

  (setq org-log-into-drawer t)    ;don't clutter files with state logs
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  (add-hook 'org-mode-hook
            (lambda ()
              (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
              (auto-fill-mode)
              (org-indent-mode)))
  (add-hook 'org-mode-hook 'yas-minor-mode-on)
  (add-hook 'org-mode-hook 'company-mode)
  :config
  (require 'ox-org)
  (require 'ox-md)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (add-to-list 'org-structure-template-alist
               '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))
  (add-to-list 'org-structure-template-alist
               '("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n"))

  ;; Don't use the same TODO state as the current heading for new heading
  (defun my-org-insert-todo-heading ()
    (interactive)
    (org-insert-todo-heading t))
  (define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)

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

  (use-package org-src
    :init
    ;; Let's have pretty source code blocks
    (setq org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          org-src-fontify-natively t
          org-confirm-babel-evaluate nil)
    (setq org-src-tab-acts-natively t)
    (setq org-confirm-babel-evaluate nil)
    )

  (use-package org-indent
    :init)

  (use-package org-table
    :init)

  (use-package org-archive
    :init)

  (use-package org-plus-contrib
    :ensure t
    :init (progn
            (setq org-startup-indented t
                  org-modules '(org-drill))
            (setq org-latex-pdf-process
                  '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
            (evil-leader/set-key-for-mode 'org-mode
              "m E" 'org-export-dispatch
              "m p l" 'org-preview-latex-fragment
              "m p i" 'org-toggle-inline-images))
    :config (progn
              (org-load-modules-maybe)))


  ;; org-agenda
  (use-package org-agenda
    :init
    (setq org-agenda-start-with-log-mode t)
    (setq org-agenda-dim-blocked-tasks t)
    ;; (setq org-agenda-todo-ignore-scheduled 'future) ;don't show future scheduled
    ;; (setq org-agenda-todo-ignore-deadlines 'far)    ;show only near deadlines
    (setq org-agenda-include-all-todo t)
    (setq org-agenda-include-diary t)
    ;;(setq org-agenda-ndays 7)
    (setq org-agenda-show-all-dates t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-start-on-weekday nil)
    :config
    ;; add state to the sorting strategy of todo
    (setcdr (assq 'todo org-agenda-sorting-strategy)
            '(todo-state-up priority-down category-keep))

    ;; create the file for the agendas if it doesn't exist
    (let ((agendas "~/.agenda_files"))
      (unless (file-readable-p agendas)
        (with-temp-file agendas nil))
      (setq org-agenda-files agendas))

    (setq org-agenda-files '("~/org/personal"
                             "~/org/technical"
                             "~/org/project"))

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


  (use-package org-research
    :init
    (progn
      (setq org-research-root "~/research")
      (evil-leader/set-key-for-mode 'org-research-mode
        "m r o" 'org-research-open-paper
        "m r a" 'org-research-add-reference)))

  (use-package org-journal
    :ensure t
    :init
    (progn
      (evil-leader/set-key
        "+" 'org-journal-new-entry
        "=" '(lambda () (interactive) (org-journal-new-entry t)))
      (which-key-add-key-based-replacements
        "SPC +" "Add entry to journal"
        "SPC =" "View today's journal")
      (setq org-journal-dir (expand-file-name "~/journal/")
            org-journal-file-format "%Y-%m-%d.org"
            org-journal-date-format "%A, %d-%m-%Y"
            org-journal-enable-encryption nil)
      (evil-leader/set-key-for-mode 'calendar-mode
        "m j j" 'org-journal-read-entry
        "m j i" 'org-journal-new-date-entry
        "m j [" 'org-journal-previous-entry
        "m j ]" 'org-journal-next-entry
        "m j f f" 'org-journal-search-forever
        "m j f m" 'org-journal-search-calendar-month
        "m j f w" 'org-journal-search-calender-week
        "m j f y" 'org-journal-search-calendar-year)
      (evil-leader/set-key-for-mode 'org-journal-mode
        "m j [" 'org-journal-open-previous-entry
        "m j ]" 'org-journal-open-next-entry))
    :config
    (defun get-journal-file-today ()
      "Return filename for today's journal entry."
      (let ((daily-name (format-time-string "%Y%m%d")))
        (expand-file-name (concat org-journal-dir daily-name))))

    (defun journal-file-today ()
      "Create and load a journal file based on today's date."
      (interactive)
      (find-file (get-journal-file-today)))

    (defun get-journal-file-yesterday ()
      "Return filename for yesterday's journal entry."
      (let ((daily-name (format-time-string "%Y%m%d" (time-subtract (current-time) (days-to-time 1)))))
        (expand-file-name (concat org-journal-dir daily-name))))

    (defun journal-file-yesterday ()
      "Creates and load a file based on yesterday's date."
      (interactive)
      (find-file (get-journal-file-yesterday)))
    )


  (use-package org-pomodoro
    :commands (org-pomodoro)
    :ensure t
    :config
    (setq pomodoro-break-time 2)
    (setq pomodoro-long-break-time 5)
    (setq pomodoro-work-time 15)
    ;; (setq-default mode-line-format
    ;;               (cons '(pomodoro-mode-line-string pomodoro-mode-line-string)
    ;;                     mode-line-format))
    )

  (use-package notifications
    :config
    (defun my-appt-disp-window-function (min-to-app new-time msg)
      (notifications-notify :title (format "Appointment in %s min" min-to-app) :body msg))
    (setq appt-disp-window-function 'my-appt-disp-window-function)
    (setq appt-delete-window-function (lambda (&rest args))))

  ;; org-capture
  (use-package org
    :init
    (setq org-capture-templates
          '(("t" "Task" entry (file "") "* TODO %?\n %a")
            ("s" "Simple Task" entry (file "") "* TODO %?\n")
            ;;capturebookmarks
            ("b" "Bookmark" plain (file "~/notes/bookmarks.org" "Bookmarks"))
            ;;captureTasks
            ("t" "Todo" entry (file+headline "~/notes/gtd.org" "Tasks")
             "* TODO %?\n  %i\n  %c")))
    :config
    (add-hook 'org-capture-mode-hook
              (lambda ()
                (evil-insert-state)))
    )

  ;; org-clock
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

  (use-package ox-html
    :init
    (setq org-html-postamble nil)
    (setq org-export-with-section-numbers nil)
    (setq org-cycle-include-plain-lists t)
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
     </style>"))


  ;; ob-clojure
  (use-package ob-clojure
    :config
    (setq org-babel-clojure-backend 'cider))

  (use-package org
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sh         . t)
       (js         . t)
       (emacs-lisp . t)
       (perl       . t)
       (scala      . t)
       (clojure    . t)
       (python     . t)
       (ruby       . t)
       (dot        . t)
       (css        . t)
       (plantuml   . t))))


  (use-package ox-latex
    :config
    (setq org-latex-listings 'minted)
    (setq org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    (add-to-list 'org-latex-packages-alist '("" "minted")))

  (use-package ox-reveal
    :init
    (setq org-reveal-root (concat "file://" (getenv "HOME") "/Public/js/reveal.js"))
    (setq org-reveal-postamble "ox reveal presentation"))

  (use-package org-mobile
    :disabled t
    :init
    (progn
      (setq org-mobile-directory "~/Documents/mobileorg"
            org-mobile-files (list
                              "~/Documents/org"
                              "~/Documents/Work/org")
            org-mobile-inbox-for-pull "~/Documents/org/inbox.org")
      ))

  (use-package org-crypt
    :commands (org-decrypt-entries
               org-encrypt-entries
               org-crypt-use-before-save-magic)
    :init
    (progn
      ;; GPG key to use for encryption
      ;; Either the Key ID or set to nil to use symmetric encryption.
      (setq org-crypt-key nil)
      )
    :config
    (progn
      (org-crypt-use-before-save-magic)
      (setq org-tags-exclude-from-inheritance (quote ("crypt")))
      ))

  (use-package org-protocol))

(provide 'init-org-2)
