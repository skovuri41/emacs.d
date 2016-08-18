(require 'worf)

(defun my/org-mode-hook ()
  (interactive)
  (turn-on-auto-fill)
  (turn-on-flyspell)
  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode 1)))

(defun set-org-mode-app-defaults ()
  (setq org-file-apps
        '(((auto-mode . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . system)
           ("\\.pdf\\'" . system)))))

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

(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter  _q_uote    _L_aTeX:
_l_atex   _e_xample  _i_ndex:
_a_scii   _v_erse    _I_NCLUDE:
_s_rc     ^ ^        _H_TML:
_h_tml    ^ ^        _A_SCII:
"
  ("s" (hot-expand "<s"))
  ("e" (hot-expand "<e"))
  ("q" (hot-expand "<q"))
  ("v" (hot-expand "<v"))
  ("c" (hot-expand "<c"))
  ("l" (hot-expand "<l"))
  ("h" (hot-expand "<h"))
  ("a" (hot-expand "<a"))
  ("L" (hot-expand "<L"))
  ("i" (hot-expand "<i"))
  ("I" (hot-expand "<I"))
  ("H" (hot-expand "<H"))
  ("A" (hot-expand "<A"))
  ("t" (hot-expand "<t"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))
;;* Mode Objects
(defhydra hydra-org-objects (:exit t)
  "org-objects"
  ("t" hydra-org-timer/body "timer")
  ("c" hydra-org-clock/body "clock"))

;;** Timer
(defhydra hydra-org-timer (:exit t
                                 :columns 2)
  "org-timer"
  ;; "run"
  ("rr" org-timer-start "run relative")
  ("rd" org-timer-set-timer "run descending")
  ;; "kill"
  ("k" org-timer-stop "kill")
  ;; "pause"
  ("z" org-timer-pause-or-continue "suspend/resume")
  ;; "insert"
  ("is" org-timer "insert simple")
  ("ii" org-timer-item "insert item")
  ("im" org-timer-show-remaining-time "insert as message")
  ;; "change"
  ("c" org-timer-change-times-in-region "change in region")
  ;; "menu"
  ("b" hydra-org-objects/body "back")
  ("q" nil "quit"))

;;** Clock
(defun ora-org-clock-goto ()
  (interactive)
  (ring-insert
   find-tag-marker-ring
   (point-marker))
  (org-clock-goto))

(defhydra hydra-org-clock (:color teal
                                  :columns 2)
  "org-clock"
  ;; "run"
  ("ri" org-clock-in "in here")
  ("rj" (org-clock-in '(4)) "in choice")
  ("rl" org-clock-in-last "in last")
  ("ro" org-clock-out "out")
  ;; "kill"
  ("k" org-clock-cancel "kill")
  ;; "goto"
  ("g" ora-org-clock-goto "goto")
  ;; "change"
  ("ce" org-clock-modify-effort-estimate "change estimate")
  ("cs" hydra-org-clock-timestamps/body "timestamps")
  ;; "insert"
  ("ir" org-clock-report "insert report")
  ("it" hydra-org-clock-display/body "display time")
  ;; "menu"
  ("b" hydra-org-objects/body "back")
  ("q" nil "quit"))

(defhydra hydra-org-clock-display (:color teal
                                          :columns 1)
  "org-clock-display"
  ("i" org-clock-display "for buffer")
  ("t" (org-clock-display '(4)) "for today")
  ("j" (org-clock-display '(16)) "for interval")
  ("a" (org-clock-display '(64)) "for all")
  ("k" org-clock-remove-overlays "kill")
  ;; "menu"
  ("b" hydra-org-clock/body "back")
  ("q" nil "quit"))

(defhydra hydra-org-clock-timestamps ()
  "org-clock-timestamps"
  ("h" backward-word "left")
  ("j" org-clock-timestamps-down "down")
  ("k" org-clock-timestamps-up "up")
  ("l" forward-word "right")
  ("b" hydra-org-clock/body "back" :exit t)
  ("q" nil "quit"))

(defun org-agenda-cts ()
  (and (eq major-mode 'org-agenda-mode)
       (let ((args (get-text-property
                    (min (1- (point-max)) (point))
                    'org-last-args)))
         (nth 2 args))))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
_w_: ?w? week       _[_: inactive       _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
_m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
_y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
  ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
  ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("[" (let ((org-agenda-include-inactive-timestamps t))
         (org-agenda-check-type t 'timeline 'agenda)
         (org-agenda-redo)
         (message "Display now includes inactive timestamps as well")))
  ("q" (message "Abort") :exit t)
  ("x" org-agenda-exit :exit t)
  ("v" nil))

(defun my/org-mode-key-bindings ()
  ;;calendar tool on C-c .
  (define-key org-read-date-minibuffer-local-map (kbd "M-h")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-l")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-k")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-j")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-H")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-L")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-K")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-J")
    (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))

  (bind-keys :map org-mode-map
             ((kbd "M-h") . org-metaleft)
             ((kbd "M-H") . org-metaleft)
             ((kbd "M-j") . org-shiftleft)
             ((kbd "M-J") . org-metadown)
             ((kbd "M-k") . org-shiftright)
             ((kbd "M-K") . org-metaup)
             ((kbd "M-l") . org-metaright)
             ((kbd "M-L") . org-metaright)
             ((kbd "M-o") . (lambda () (interactive)
                              (org-eol-call
                               '(lambda()
                                  (org-insert-heading)
                                  (org-metaright)))))
             ((kbd "M-t") . (lambda () (interactive)
                              (org-eol-call
                               '(lambda()
                                  (org-insert-todo-heading nil)
                                  (org-metaright))))))
  (define-key org-mode-map "<"
    (defun org-self-insert-or-less ()
      (interactive)
      (if (looking-back "^")
          (hydra-org-template/body)
        (self-insert-command 1))))

  (bind-keys :map orgstruct-mode-map
             ((kbd "M-H") . org-metaleft)
             ((kbd "M-j") . org-shiftleft)
             ((kbd "M-J") . org-metadown)
             ((kbd "M-k") . org-shiftright)
             ((kbd "M-K") . org-metaup)
             ((kbd "M-L") . org-metaright)
             ((kbd "M-o") . (lambda () (interactive)
                              (org-eol-call
                               '(lambda()
                                  (org-insert-heading)
                                  (org-metaright)))))
             ((kbd "M-t") . ( ( ( ( ( ( (lambda () (interactive)
                                          (org-eol-call
                                           '(lambda()
                                              (org-insert-todo-heading nil)
                                              (org-metaright))))))))))))
  (bind-keys :map org-agenda-mode-map
             ("j" . org-agenda-next-line)
             ("k" . org-agenda-previous-line)
             ((kbd "M-j") . org-agenda-next-item)
             ((kbd "M-k") . org-agenda-previous-item)
             ((kbd "M-h") . org-agenda-earlier)
             ((kbd "M-l") . org-agenda-later)
             ("i" . org-agenda-clock-in)
             ("o" . org-agenda-clock-out)
             ("0" . ora-org-schedule-today)
             ("1" . ora-org-schedule-tomorrow)
             ("v" . hydra-org-agenda-view/body)
             ("T" . worf-clock-in-and-out)
             ;; ((kbd "gd") . org-agenda-toggle-time-grid)
             ;; ((kbd "gr") . org-agenda-redo)
             )

  (bind-keys :prefix-map my-org-prefix-map
             :prefix "C-c o"
             ("." . org-edit-special)
             ("C" . helm-org-capture-templates)
             ("c" . org-capture)
             ("d" . org-deadline)
             ("D" . org-insert-drawer)
             ("e" . org-export-dispatch)
             ("f" . org-set-effort)
             ("P" . org-set-property)
             ("p" . org-pomodoro)
             (":" . org-set-tags)

             ("a" . org-agenda)
             ("b" . org-tree-to-indirect-buffer)
             ("A" . org-archive-subtree)
             ("l" . org-open-at-point)
             ("T" . org-show-todo-tree)

             ("." . org-time-stamp)
             ("!" . org-time-stamp-inactive)

             ;; headings
             ("hi" . org-insert-heading-after-current)
             ("hI" . org-insert-heading)

             ;; More cycling options (timestamps, headlines, items, properties)
             ;; ("L" . org-shiftright)
             ;; ("H" . org-shiftleft)
             ;; ("J" . org-shiftdown)
             ("H" . worf-back-to-heading)

             ;; Change between TODO sets
             ("C-S-l" . org-shiftcontrolright)
             ("C-S-h" . org-shiftcontrolleft)
             ("C-S-j" . org-shiftcontroldown)
             ("C-S-k" . org-shiftcontrolup)

             ;; Subtree editing
             ("Sl" . org-demote-subtree)
             ("Sh" . org-promote-subtree)
             ("Sj" . org-move-subtree-down)
             ("Sk" . org-move-subtree-up)

             ;; tables
             ("ta" . org-table-align)
             ("tb" . org-table-blank-field)
             ("tc" . org-table-convert)
             ("tdc" . org-table-delete-column)
             ("tdr" . org-table-kill-row)
             ("te" . org-table-eval-formula)
             ("tE" . org-table-export)
             ("th" . org-table-previous-field)
             ("tH" . org-table-move-column-left)
             ("tic" . org-table-insert-column)
             ("tih" . org-table-insert-hline)
             ("tiH" . org-table-hline-and-move)
             ("tir" . org-table-insert-row)
             ("tI" . org-table-import)
             ("tj" . org-table-next-row)
             ("tJ" . org-table-move-row-down)
             ("tK" . org-table-move-row-up)
             ("tl" . org-table-next-field)
             ("tL" . org-table-move-column-right)
             ("tn" . org-table-create)
             ("tN" . org-table-create-with-table)
             ("tr" . org-table-recalculate)
             ("ts" . org-table-sort-lines)
             ("ttf" . org-table-toggle-formula-debugger)
             ("tto" . org-table-toggle-coordinate-overlays)
             ("tw" . org-table-wrap-region)

             ("*" . org-ctrl-c-star)
             ("RET" . org-ctrl-c-ret)
             ("-" . org-ctrl-c-minus)
             ("]" . org-cycle-list-bullet)
             ("^" . org-sort)
             ("/" . org-sparse-tree)
             ("I" . org-clock-in)
             ("N" . org-narrow-to-subtree)
             ("W" . widen)
             ("O" . org-clock-out)
             ("q" . org-clock-cancel)
             ("R" . org-refile)
             ("s" . org-schedule)

             ;; insertion of common elements
             ("il" . org-insert-link)
             ("iu" . org-cliplink)
             ("if" . org-footnote-new)
             ;; ("ol" . org-store-link)
             ("x" . org-encrypt-entry)
             ("X" . org-decrypt-entry)))


(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
  (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode)) ;; Journal entries
  (add-hook 'org-mode-hook #'hl-line-mode)
  (add-hook 'org-mode-hook #'my/org-mode-hook)
  (add-hook 'org-mode-hook #'my/org-mode-key-bindings)
  (add-hook 'org-mode-hook
            (lambda ()
              (auto-fill-mode)
              (worf-mode)
              (org-indent-mode)))
  (add-hook 'org-mode-hook 'yas-minor-mode-on)
  (add-hook 'org-mode-hook 'company-mode)
  (add-hook 'org-mode-hook 'set-org-mode-app-defaults)

  (setq org-directory (file-truename "~/org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-replace-disputed-keys t)
  (setq org-startup-folded t)
  (setq org-startup-indented t)
  (setq org-startup-with-inline-images t)
  (setq org-startup-truncated t)

  ;; follow links by pressing ENTER on them
  (setq org-return-follows-link t)
  ;; don't adapt indentation
  (setq  org-adapt-indentation nil)
  ;; Imenu should use 3 depth instead of 2
  (setq  org-imenu-depth 3)
  ;; special begin/end of line to skip tags and stars
  (setq  org-special-ctrl-a/e t)
  ;; special keys for killing a headline
  (setq  org-special-ctrl-k t)
  ;; don't adjust subtrees that I copy
  (setq  org-yank-adjusted-subtrees nil)
  ;; try to be smart when editing hidden things
  (setq  org-catch-invisible-edits 'smart)

  (setq org-hide-leading-stars t)
  (setq org-odd-levels-only nil)
  (setq org-list-allow-alphabetical t)
  (setq org-cycle-include-plain-lists t)
  (setq org-cycle-separator-lines 0)
  (setq org-cycle-include-plain-lists t)
  (setq org-blank-before-new-entry (quote ((heading)
                                           (plain-list-item . auto))))
  (setq org-use-speed-commands t)
  (setq org-hide-emphasis-markers t)
  (setq org-reverse-note-order nil)
  (setq org-tags-column 80)
  ;; Block entries from changing state to DONE while they have children
  ;; that are not DONE
  (setq org-enforce-todo-dependencies t)
  (setq org-use-fast-todo-selection t)
  ;; put state change log messages into a drawer
  (setq org-log-into-drawer t)
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))

  ;; Org todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "TODO(t)"
                    "SOMEDAY(s)"
                    "INPROGRESS(i)"
                    "HOLD(h)"
                    "WAITING(w@/!)"
                    "NEEDSREVIEW(n@/!)"
                    "|" "DONE(d)" "CANCELLED(c@/!)")))

  ;; Org faces
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("INPROGRESS" :foreground "deep sky blue" :weight bold)
          ("SOMEDAY" :foreground "purple" :weight bold)
          ("NEEDSREVIEW" :foreground "#edd400" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)))

  ;; add or remove tags on state change
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("INPROGRESS" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

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

  ;; refile targets all level 1 and 2 headers in current file and agenda files
  (setq org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2)))
  (setq org-refile-use-outline-path 'file)

  (require 'ox-org)
  (require 'ox-md)
  (require 'org-habit)

  (add-to-list 'org-modules 'org-habit)

  (setq org-habit-graph-column 60)
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-graph-column 40)
  (setq org-habit-preceding-days 7)
  (setq org-habit-following-days 1)

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

  (defun org-text-bold () "Wraps the region with asterisks."
         (interactive)
         (surround-text "*"))
  (defun org-text-italics () "Wraps the region with slashes."
         (interactive)
         (surround-text "/"))
  (defun org-text-code () "Wraps the region with equal signs."
         (interactive)
         (surround-text "="))

  (use-package org-src
    :config
    ;; Let's have pretty source code blocks
    (setq org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          org-src-fontify-natively t
          org-confirm-babel-evaluate nil)
    (setq org-src-tab-acts-natively t)
    ;; preserve the indentation inside of source blocks
    (setq org-src-preserve-indentation t)
    (setq org-confirm-babel-evaluate nil)
    ;; how org-src windows are set up when hitting C-c '
    (setq org-src-window-setup 'current-window)
    ;; blank lines are removed when exiting the code edit buffer
    (setq org-src-strip-leading-and-trailing-blank-lines t)
    (setq org-src-fontify-natively t)
    )

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

  (use-package org-indent
    :init)
  (use-package org-table
    :init)
  (use-package org-install)
  (use-package ob-core)
  ;; org-export
  (use-package ox)
  ;; Enable archiving things
  (use-package org-archive)

  (use-package org
    :ensure t
    :config
    (progn
      (setq org-startup-indented t)
      (setq org-latex-pdf-process
            '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")))
    (progn
      (org-load-modules-maybe)))

  ;; org-agenda
  (use-package org-agenda
    :config
    (progn
      (setq org-agenda-start-with-log-mode t)
      (setq org-agenda-dim-blocked-tasks t)
      ;;(setqorg-agenda-todo-ignore-scheduled'future);don'tshowfuturescheduled
      ;;(setqorg-agenda-todo-ignore-deadlines'far);showonlyneardeadlines
      (setq org-agenda-include-all-todo t)
      (setq org-agenda-include-diary t)
      ;;(setqorg-agenda-ndays7)
      (setq org-agenda-show-all-dates t)
      (setq org-agenda-skip-deadline-if-done t)
      (setq org-agenda-skip-scheduled-if-done t)
      ;;addstatetothesortingstrategyoftodo
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
         (0900 1100 1300 1500 1700))
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
       ;; Do not dim blocked tasks
       org-agenda-dim-blocked-tasks nil
       ;; Compact the block agenda view
       org-agenda-compact-blocks t
       ;; Show all agenda dates - even if they are empty
       org-agenda-show-all-dates t
       ;; Agenda org-mode files
       org-agenda-files `(,(file-truename "~/org/refile.org")
                          ,(file-truename "~/org/todo.org")
                          ,(file-truename "~/org/bibliography.org")
                          ,(file-truename "~/org/notes.org")
                          ))

      ;; (setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")
      (setq org-agenda-file-regexp "\\([^.].*\\.org\\)\\|\\([0-9]+\\)")

      ;;Custom agenda command definitions
      (setq org-agenda-custom-commands
            '(("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              (" " "Agenda"
               ((agenda "" nil)
                ;; All items with the "REFILE" tag, everything in refile.org
                ;; automatically gets that applied
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                ;; All "INPROGRESS" todo items
                (todo "INPROGRESS"
                      ((org-agenda-overriding-header "Current work")))
                ;; All headings with the "support" tag
                (tags "support/!"
                      ((org-agenda-overriding-header "Support cases")))
                ;; All "NEESREVIEW" todo items
                (todo "NEEDSREVIEW"
                      ((org-agenda-overriding-header "Waiting on reviews")))
                ;; All "WAITING" items without a "support" tag
                (tags "WAITING-support"
                      ((org-agenda-overriding-header "Waiting for something")))
                ;; All TODO items
                (todo "TODO"
                      ((org-agenda-overriding-header "Task list")
                       (org-agenda-sorting-strategy
                        '(time-up priority-down category-keep))))
                ;; Everything on hold
                (todo "HOLD"
                      ((org-agenda-overriding-header "On-hold")))
                ;; All headings with the "recurring" tag
                (tags "recurring/!"
                      ((org-agenda-overriding-header "Recurring"))))
               nil)))

      ;;add new appointments when saving the org buffer, use'refresh argument todo it properly
      (defun my-org-agenda-to-appt-refresh () (org-agenda-to-appt 'refresh))
      (defun my-org-mode-hook ()
        (add-hook 'after-save-hook 'my-org-agenda-to-appt-refresh nil 'make-it-local))
      (add-hook 'org-mode-hook 'my-org-mode-hook)

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
      ))
  )

;; org-capture
(use-package org
  :config
  ;; capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/org/refile.org")
           "* TODO %?\n%U\n")
          ("m" "Email" entry (file "~/org/refile.org")
           "* TODO [#B] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
          ("n" "Notes" entry (file+headline "~/org/notes.org" "Notes")
           "* %? :NOTE:\n%U\n")
          ;;capture bookmarks
          ("b" "Bookmark" plain (file "~/org/bookmarks.org" "Bookmarks"))
          ("e" "Emacs note" entry
           (file+headline "~/org/notes.org" "Emacs Links")
           "* %? :NOTE:\n%U\n")
          ("j" "Journal Note"     entry
           (file (get-journal-file-today))
           "* %?\n\n  %i\n\n  From: %f" :empty-lines 1)
          ("B" "Book/Bibliography" entry
           (file+headline "~/org/bibliography.org" "Refile")
           "* %?%^{TITLE}p%^{AUTHOR}p%^{TYPE}p")))

  (add-hook 'org-capture-mode-hook
            (lambda ()
              (xah-fly-insert-mode-activate)))
  )

(use-package org-journal
  :ensure t
  :mode (".*/[0-9]*-[0-9]*-[0-9]*$" . org-journal-mode)
  :init
  (progn

    ;; (evil-leader/set-key
    ;;   "+" '(lambda ()
    ;;          (interactive)
    ;;          (org-journal-new-entry nil)
    ;;          (evil-insert-state)
    ;;          )
    ;;   "=" '(lambda () (interactive) (org-journal-new-entry t)))
    (setq org-journal-dir "~/journal/")
    (setq org-journal-date-format "Journal Entry- %Y-%b-%d (%A)")
    (setq org-journal-time-format "")
    (setq org-journal-file-format "%Y-%m-%d.org"
          org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format)
          org-journal-hide-entries-p nil)

    ;; (evil-leader/set-key-for-mode 'calendar-mode
    ;;   "m j j" 'org-journal-read-entry
    ;;   "m j i" 'org-journal-new-date-entry
    ;;   "m j [" 'org-journal-previous-entry
    ;;   "m j ]" 'org-journal-next-entry
    ;;   "m j f f" 'org-journal-search-forever
    ;;   "m j f m" 'org-journal-search-calendar-month
    ;;   "m j f w" 'org-journal-search-calender-week
    ;;   "m j f y" 'org-journal-search-calendar-year)
    ;; (evil-leader/set-key-for-mode 'org-journal-mode
    ;;   "m j [" 'org-journal-open-previous-entry
    ;;   "m j ]" 'org-journal-open-next-entry)

    )

  (defun journal-file-insert ()
    "Insert's the journal heading based on the file's name."
    (interactive)
    (when (string-match "\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)" (buffer-name))
      (let ((year  (string-to-number (match-string 1 (buffer-name))))
            (month (string-to-number (match-string 2 (buffer-name))))
            (day   (string-to-number (match-string 3 (buffer-name))))
            (datim nil))
        (setq datim (encode-time 0 0 0 day month year))
        (insert (format-time-string org-journal-date-format datim))
        (insert "\n\n"))))  ; Start with a blank separating line

  (setq auto-insert-alist (append '(
                                    (".*/[0-9]*$" . journal-file-insert)
                                    )
                                  auto-insert-alist))

  (add-to-list 'org-agenda-files (expand-file-name "~/journal"))

  (defun get-journal-file-today ()
    "Return filename for today's journal entry."
    (let ((daily-name (format-time-string "%Y-%m-%d")))
      (expand-file-name (concat org-journal-dir daily-name ".org"))))

  (defun journal-file-today ()
    "Create and load a journal file based on today's date."
    (interactive)
    (find-file (get-journal-file-today)))

  (defun get-journal-file-yesterday ()
    "Return filename for yesterday's journal entry."
    (let ((daily-name (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1)))))
      (expand-file-name (concat org-journal-dir daily-name ".org"))))

  (defun journal-file-yesterday ()
    "Creates and load a file based on yesterday's date."
    (interactive)
    (find-file (get-journal-file-yesterday)))
  )

(use-package org-research
  :disabled t
  :config
  (progn
    (setq org-research-root "~/research")
    ))

(use-package org-pomodoro
  :commands (org-pomodoro)
  :ensure t
  :config
  (progn
    (setq org-pomodoro-length 2)
    (setq org-pomodoro-long-break-length 1)
    (setq org-pomodoro-short-break-length 1)
    (when *is-a-mac*
      (setq org-pomodoro-audio-player "/usr/bin/afplay")))
  )

(use-package notifications
  :config
  (defun my-appt-disp-window-function (min-to-app new-time msg)
    (notifications-notify :title (format "Appointment in %s min" min-to-app) :body msg))
  (setq appt-disp-window-function 'my-appt-disp-window-function)
  (setq appt-delete-window-function (lambda (&rest args))))

;; org-clock
(use-package org-clock
  :config
  (setq org-clock-idle-time 15)
  (setq org-clock-in-resume t)
  (setq org-clock-persist t)
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-clocked-in-display 'both)
  (setq org-clock-frame-title-format
        (append '((t org-mode-line-string)) '(" ") frame-title-format))
  (setq org-clock-history-length 23
        ;; Resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;; Separate drawers for clocking and logs
        org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "HIDDEN")
        ;; Save clock data and state changes and notes in the LOGBOOK drawer
        org-clock-into-drawer t
        ;; Sometimes I change tasks I'm clocking quickly -
        ;; this removes clocked tasks with 0:00 duration
        org-clock-out-remove-zero-time-clocks t
        ;; Clock out when moving task to a done state
        org-clock-out-when-done t
        ;; Save the running clock and all clock history when exiting Emacs, load it on startup
        org-clock-persist t
        ;; Prompt to resume an active clock
        org-clock-persist-query-resume t
        ;; Enable auto clock resolution for finding open clocks
        org-clock-auto-clock-resolution #'when-no-clock-is-running
        ;; Include current clocking task in clock reports
        org-clock-report-include-clocking-task t
        ;; don't use pretty things for the clocktable
        org-pretty-entities nil
        ;; some default parameters for the clock report
        org-agenda-clockreport-parameter-plist
        '(:maxlevel 10 :fileskip0 t :score agenda :block thismonth :compact t :narrow 60))
  (when (executable-find "xprintidle")
    (setq org-x11idle-exists-p t)
    (setq org-clock-x11idle-program-name "xprintidle"))
  (org-clock-persistence-insinuate))

;; ob-clojure
(use-package ob-clojure
  :config
  (setq org-babel-clojure-backend 'cider)
  ;; Clojure-specific org-babel stuff
  (defvar org-babel-default-header-args:clojure
    '((:results . "silent")))
  (defun org-babel-execute:clojure (body params)
    "Execute a block of Clojure code with Babel."
    (let ((result-plist
           (nrepl-send-string-sync
            (org-babel-expand-body:clojure body params) nrepl-buffer-ns))
          (result-type  (cdr (assoc :result-type params))))
      (org-babel-script-escape
       (cond ((eq result-type 'value) (plist-get result-plist :value))
             ((eq result-type 'output) (plist-get result-plist :value))
             (t (message "Unknown :results type!"))))))
  )

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
     (plantuml   . t)))

  ;;thisiswhereFedorainstallsit,YMMV
  (setq org-plantuml-jar-path "/usr/share/java/plantuml.jar")
  ;;ensurethisvariableisdefined
  (unless (boundp 'org-babel-default-header-args:sh)
    (setq org-babel-default-header-args:sh '()))

  ;;addadefaultshebangheaderargumentshellscripts
  (add-to-list 'org-babel-default-header-args:sh
               '(:shebang . "#!/usr/bin/env bash"))

  ;;addadefaultshebangheaderargumentforpython
  (add-to-list 'org-babel-default-header-args:python
               '(:shebang . "#!/usr/bin/env python")))

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

(use-package org-mobile
  :disabled t
  :config
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
  :config
  (progn
    ;; GPG key to use for encryption
    ;; Either the Key ID or set to nil to use symmetric encryption.
    (setq org-crypt-key "C6FC9277")
    ;; (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    )
  )

;; Generate unique IDs for all entries
(use-package org-id
  :init
  (progn
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

    (defun my/org-custom-id-get (&optional pom create prefix)
      "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
      (interactive)
      (org-with-point-at pom
        (let ((id (org-entry-get nil "CUSTOM_ID")))
          (cond
           ((and id (stringp id) (string-match "\\S-" id))
            id)
           (create
            (setq id (org-id-new (concat prefix "h")))
            (org-entry-put pom "CUSTOM_ID" id)
            (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
            id)))))

    (defun my/org-add-ids-to-headlines-in-file ()
      "Add CUSTOM_ID properties to all headlines in the
   current file which do not already have one."
      (interactive)
      (org-map-entries (lambda () (my/org-custom-id-get (point) 'create))))

    ;; automatically add ids to captured headlines
    (add-hook 'org-capture-prepare-finalize-hook
              (lambda () (my/org-custom-id-get (point) 'create)))
    ))

(use-package org-cliplink :ensure t)

(use-package org-protocol)

(use-package toc-org
  :defer t
  :init
  (progn
    (setq toc-org-max-depth 10)
    ;; (add-hook 'org-mode-hook 'toc-org-enable)
    ))

(provide 'init-org-2)
