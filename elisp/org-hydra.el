(defhydra hydra-calendar
  (:color pink
          :hint nil
          :columns 4
          :body-pre (calendar)
          )
  "
 ^Calendar^     ^Journal^   ^ ^           ^Quit^
  _._: today    _n_ext      _e_: edit     _b_: journal
  _?_: date     _p_revious  _C-e_: view   _q_: quit
  ^ ^           _N_ew
"
  ("." calendar-goto-today)
  ("?" calendar-goto-date)
  ("n" org-journal-next-entry)
  ("p" org-journal-previous-entry)
  ("e" org-journal-read-entry :color blue)
  ("C-e" org-journal-display-entry)
  ("N" org-journal-new-date-entry)
  ("b" hydra-journal/body :color blue :exit-function (calendar-exit))
  ("q" nil "quit" :color blue :exit-function (calendar-exit)))

(defhydra hydra-journal (:color pink :hint nil :columns 4)
  "
Journal
"
  ("n" org-journal-new-entry "new" :color blue)
  ("/" org-journal-search-forever "search" :color blue)
  ("c" hydra-calendar/body "calendar" :color blue)
  ("q" nil "quit" :color blue))

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

(provide 'org-hydra)
