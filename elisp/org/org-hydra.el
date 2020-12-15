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

(defhydra hydra-org-organize (:color red
                                     :hint nil)
  "
 ^Meta^    ^Shift^   ^Shift-Meta^ ^Shift-Ctrl^  ^Move^        ^Item^
^^^^^^^^^^^^^--------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   ^ ^ _p_ ^ ^      ^ ^ _P_ ^ ^       _<_: promote  _u_: up     _q_: quit
 _h_ ^+^ _l_   _H_ ^+^ _L_   _b_ ^+^ _f_      _B_ ^+^ _F_       _>_: demote   _d_: down
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   ^ ^ _n_ ^ ^      ^ ^ _N_ ^ ^
"
  ("h" org-metaleft)
  ("l" org-metaright)
  ("j" org-metadown)
  ("k" org-metaup)
  ("H" org-shiftleft)
  ("L" org-shiftright)
  ("J" org-shiftdown)
  ("K" org-shiftup)
  ("b" org-shiftmetaleft)
  ("f" org-shiftmetaright)
  ("n" org-shiftmetadown)
  ("p" org-shiftmetaup)
  ("B" org-shiftcontrolleft)
  ("F" org-shiftcontrolright)
  ("P" org-shiftcontroldown)
  ("N" org-shiftcontrolup)
  ("<" org-promote)
  (">" org-demote)
  ("d" org-move-item-down)
  ("u" org-move-item-up)
  ("q" nil :color blue))

(defhydra sk/hydra-org-todo (:color red
                                    :hint nil)
  "
 _d_: deadline    _o_: over    _s_: schedule   _c_: check   _q_: quit
"
  ("d" org-deadline :color blue)
  ("o" org-deadline-close :color blue)
  ("s" org-schedule :color blue)
  ("c" org-check-deadlines)
  ("q" nil :color blue))


(defhydra sk/hydra-org-checkbox (:color pink
                                        :hint nil)
  "
 _t_: toggle   _s_: stats    _r_: reset    _c_: count    _q_: quit
"
  ("t" org-toggle-checkbox)
  ("c" org-update-checkbox-count-maybe)
  ("r" org-reset-checkbox-state-subtree)
  ("s" org-update-statistics-cookies)
  ("q" nil :color blue))

(defhydra sk/hydra-org-property (:color red
                                        :hint nil)
  "
 _i_: insert  _p_: property   _s_: set    _d_: delete    _t_: toggle    _q_: quit
"
  ("i" org-insert-drawer)
  ("p" org-insert-property-drawer)
  ("s" org-set-property)
  ("d" org-delete-property)
  ("t" org-toggle-ordered-property)
  ("q" nil :color blue))

(defhydra sk/hydra-org-clock (:color red
                                     :hint nil)
  "
 ^Clock^                     ^Timer^     ^Stamp^
^^^^^^^^^^-------------------------------------------------
 _i_: in       _z_: resolve    _b_: begin  _t_: stamp       _q_: quit
 _o_: out      _l_: last       _e_: end    _u_: inactive
 _r_: report   _c_: cancel     _m_: timer
 _d_: display  _g_: goto       _s_: set
"
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("r" org-clock-report)
  ("z" org-resolve-clocks)
  ("c" org-clock-cancel)
  ("d" org-clock-display)
  ("l" org-clock-in-last)
  ("g" org-clock-goto)
  ("m" org-timer)
  ("s" org-timer-set-timer)
  ("b" org-timer-start)
  ("e" org-timer-stop)
  ("t" org-time-stamp)
  ("u" org-time-stamp-inactive)
  ("q" nil :color blue))

(defhydra sk/hydra-org-tables (:color red
                                      :hint nil)
  "
 ^Field^   ^Shift^   ^Insert^      ^Delete^         ^Field^     ^Table^      ^Formula^
^^^^^^^^^^^^------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   _r_: row      _dr_: del row    _e_: edit   _a_: align   _+_: sum    _q_: quit
 _h_ ^+^ _l_   _H_ ^+^ _L_   _c_: column   _dc_: del col    _b_: blank  _|_: create  _=_: eval
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   _-_: hline                     _i_: info   _f_: edit
"
  ("a" org-table-align)
  ("l" org-table-next-field)
  ("h" org-table-previous-field)
  ("j" org-table-end-of-field)
  ("k" org-table-beginning-of-field)
  ("r" org-table-insert-row)
  ("c" org-table-insert-column)
  ("-" org-table-insert-hline)
  ("J" org-table-move-row-down)
  ("K" org-table-move-row-up)
  ("H" org-table-move-column-left)
  ("L" org-table-move-column-right)
  ("dr" org-table-kill-row)
  ("dc" org-table-delete-column)
  ("b" org-table-blank-field)
  ("e" org-table-edit-field)
  ("i" org-table-field-info)
  ("+" org-table-sum)
  ("=" org-table-eval-formula)
  ("f" org-table-edit-formulas)
  ("|" org-table-create-or-convert-from-region)
  ("q" nil :color blue))

(defhydra sk/hydra-org-jump (:color pink
                                    :hint nil)
  "
 ^Outline^          ^Item^   ^Table^   ^Block^   ^Link^
 ^^^^^^^^^^^-------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   ^ ^ _u_ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ _p_ ^ ^   ^ ^ _P_ ^ ^    _q_ quit
 _h_ ^+^ _l_   ^ ^ ^+^ ^ ^   ^ ^ ^+^ ^ ^   _b_ ^+^ _f_   ^ ^ ^+^ ^ ^   ^ ^ ^+^ ^ ^
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   ^ ^ _d_ ^ ^   ^ ^ ^ ^ ^ ^   ^ ^ _n_ ^ ^   ^ ^ _N_ ^ ^
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("l" org-down-element)
  ("h" org-up-element)
  ("J" org-forward-heading-same-level)
  ("K" org-backward-heading-same-level)
  ("u" org-next-item)
  ("d" org-previous-item)
  ("f" org-table-next-field)
  ("b" org-table-previous-field)
  ("n" org-next-block)
  ("p" org-previous-block)
  ("N" org-next-link)
  ("P" org-previous-link)
  ("q" nil :color blue))

(defhydra sk/hydra-org-agenda-view (:color red
                                           :hint nil)
  "
 _d_: day        _g_: time grid    _a_: arch-trees    _L_: log closed clock
 _w_: week       _i_: inactive     _A_: arch-files    _c_: log clock check
 _t_: fortnight  _f_: follow       _r_: report        _l_: log mode toggle
 _m_: month      _e_: entry        _D_: diary         _q_: quit
 _y_: year       _!_: deadlines    _R_: reset
"
  ("R" org-agenda-reset-view)
  ("d" org-agenda-day-view)
  ("w" org-agenda-week-view)
  ("t" org-agenda-fortnight-view)
  ("m" org-agenda-month-view)
  ("y" org-agenda-year-view)
  ("l" org-agenda-log-mode)
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode)
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode)
  ("e" org-agenda-entry-text-mode)
  ("g" org-agenda-toggle-time-grid)
  ("D" org-agenda-toggle-diary)
  ("!" org-agenda-toggle-deadlines)
  ("i"
   (let ((org-agenda-include-inactive-timestamps t))
     (org-agenda-check-type t 'timeline 'agenda)
     (org-agenda-redo)))
  ("q" nil :color blue))

(defhydra sk/hydra-org-template (:color blue
                                        :hint nil)
  "
 ^One liners^                                        ^Blocks^                                      ^Properties^
--------------------------------------------------------------------------------------------------------------------------------------------------------
 _a_: author        _i_: interleave  _D_: description    _C_: center      _p_: python src    _n_: notes    _d_: defaults   _r_: properties        _<_: insert '<'
 _A_: date          _l_: label       _S_: subtitle       _e_: elisp src   _Q_: quote                     _L_: latex      _I_: interleave        _q_: quit
 _c_: caption       _N_: name        _k_: keywords       _E_: example     _s_: src                       _x_: export     _T_: drill two-sided
 _f_: file tags     _o_: options     _M_: minted         _h_: html        _v_: verbatim                  _X_: noexport
 _H_: latex header  _t_: title       _P_: publish        _m_: matlab src  _V_: verse
 "
  ("a" (hot-expand "<a"))
  ("A" (hot-expand "<A"))
  ("c" (hot-expand "<c"))
  ("f" (hot-expand "<f"))
  ("H" (hot-expand "<H"))
  ("i" (hot-expand "<i"))
  ("I" (hot-expand "<I"))
  ("l" (hot-expand "<l"))
  ("n" (hot-expand "<n"))
  ("N" (hot-expand "<N"))
  ("P" (hot-expand "<P"))
  ("o" (hot-expand "<o"))
  ("t" (hot-expand "<t"))
  ("C" (hot-expand "<C"))
  ("D" (hot-expand "<D"))
  ("e" (hot-expand "<e"))
  ("E" (hot-expand "<E"))
  ("h" (hot-expand "<h"))
  ("k" (hot-expand "<k"))
  ("M" (hot-expand "<M"))
  ("m" (hot-expand "<m"))
  ("p" (hot-expand "<p"))
  ("Q" (hot-expand "<q"))
  ("s" (hot-expand "<s"))
  ("S" (hot-expand "<S"))
  ("v" (hot-expand "<v"))
  ("V" (hot-expand "<V"))
  ("x" (hot-expand "<x"))
  ("X" (hot-expand "<X"))
  ("d" (hot-expand "<d"))
  ("L" (hot-expand "<L"))
  ("r" (hot-expand "<r"))
  ("I" (hot-expand "<I"))
  ("T" (hot-expand "<T"))
  ("b" (hot-expand "<b"))
  ("<" self-insert-command)
  ("q" nil :color blue))

(defhydra sk/hydra-org-drill (:color blue
                                     :hint nil)
  "
 _f_: file        _r_: resume   _q_: quit
 _h_: heading     _a_: again
 _d_: directory   _c_: cram
"
  ("f" org-drill)
  ("h" org-drill-tree)
  ("d" org-drill-directory)
  ("r" org-drill-resume)
  ("a" org-drill-again)
  ("c" org-drill-cram)
  ("q" nil :color blue))

(defhydra sk/org-ref-bibtex-file (:color blue
                                         :hint nil)
  "
_v_: validate     _s_: sort     _r_: reformat     _c_: count     _p_: PDF      _q_: quit
  "
  ("v" bibtex-validate)
  ("s" bibtex-sort-buffer)
  ("r" bibtex-reformat)
  ("c" bibtex-count-entries)
  ("p" org-ref-build-full-bibliography)
  ("q" nil :color blue))

(defhydra sk/hydra-org-ref (:color blue
                                   :hint nil)
  "
 _e_: bib new entry     _r_: ref link     _b_: bib file options    _q_: quit
 _t_: crossref entry    _l_: label link   _f_: file format
 _d_: doi entry         _c_: cite link    _k_: keyword set
  "
  ("e" sk/org-ref-bibtex-new-entry/body)
  ("t" crossref-add-bibtex-entry)
  ("d" doi-add-bibtex-entry)
  ("b" sk/org-ref-bibtex-hydra/body)
  ("k" org-ref-set-bibtex-keywords)
  ("r" org-ref-ivy-insert-ref-link)
  ("l" org-ref-ivy-insert-label-link)
  ("c" org-ref-ivy-insert-cite-link)
  ("f" sk/org-ref-bibtex-file/body)
  ("q" nil :color blue))

(defhydra sk/org-ref-bibtex-hydra (:color blue
                                          :hint nil)
  "
_p_: Open pdf     _y_: Copy key               _n_: New entry     _w_: WOS
_b_: Open url     _f_: Copy formatted entry   _o_: Copy entry    _c_: WOS citing
_r_: Refile entry _k_: Add keywords           _d_: delete entry  _a_: WOS related
_e_: Email entry  _K_: Edit keywords          _L_: clean entry   _P_: Pubmed
_U_: Update entry _N_: Open notes             _R_: Crossref      _g_: Google Scholar
_s_: Sort entry   _A_: Remove nonascii        _C_: Cite entry    _q_: quit
_u_: Update field _F_: file funcs
"
  ("p" org-ref-open-bibtex-pdf)
  ("b" org-ref-open-in-browser)
  ("r" (lambda () (interactive)
         (bibtex-beginning-of-entry)
         (bibtex-kill-entry)
         (find-file (ido-completing-read
                     "Bibtex file: "
                     (f-entries "." (lambda (f) (f-ext? f "bib")))))
         (goto-char (point-max))
         (bibtex-yank)
         (save-buffer)
         (kill-buffer)))
  ("e" org-ref-email-bibtex-entry)
  ("U" (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi)))
  ("s" org-ref-sort-bibtex-entry)
  ("u" doi-utils-update-field)
  ("y" (kill-new  (bibtex-autokey-get-field "=key=")))
  ("f" bibtex-copy-summary-as-kill)
  ("k" helm-tag-bibtex-entry)
  ("K" (lambda ()
         (interactive)
         (org-ref-set-bibtex-keywords
          (read-string "Keywords: "
                       (bibtex-autokey-get-field "keywords"))
          t)))
  ("N" org-ref-open-bibtex-notes)
  ("A" org-ref-replace-nonascii)
  ("F" sk/org-ref-bibtex-file/body)
  ("n" sk/org-ref-bibtex-new-entry/body)
  ("o" bibtex-copy-entry-as-kill)
  ("d" bibtex-kill-entry)
  ("L" org-ref-clean-bibtex-entry)
  ("R" org-ref-bibtex-crossref)
  ("w" org-ref-bibtex-wos)
  ("c" org-ref-bibtex-wos-citing)
  ("a" org-ref-bibtex-wos-related)
  ("P" org-ref-bibtex-pubmed)
  ("g" org-ref-bibtex-google-scholar)
  ("C" sk/org-ref-cite-hydra/body)
  ("q" nil :color blue))

;; https://fuco1.github.io/2019-02-10-Refiling-hydra-with-pre-defined-targets.html
(defmacro my-org-make-refile-command (fn-suffix refile-targets)
  "Generate a command to call `org-refile' with modified targets."
  `(defun ,(intern (concat "my-org-refile-" (symbol-name fn-suffix))) ()
     ,(format "`org-refile' to %S" refile-targets)
     (interactive)
     (org-refile-cache-clear)
     (let ((org-refile-target-verify-function nil)
           (org-refile-targets ,refile-targets))
       (call-interactively 'org-refile))))

(my-org-make-refile-command someday '(("~/org/someday.org" :maxlevel . 9)))
(my-org-make-refile-command agenda '(("~/org/agenda.org" :maxlevel . 9)))
(my-org-make-refile-command this-file `((,(buffer-file-name) :maxlevel . 9)))

(defhydra my-org-refile-hydra (:color blue :hint nil)
  "
_t_his file

Special files:
---------------------
_s_omeday.org    _a_genda.org"
  ("a" my-org-refile-agenda)
  ("s" my-org-refile-someday)
  ("t" my-org-refile-this-file))

;; (bind-key "C-c r" #'my-org-refile-hydra/body org-mode-map)


(provide 'org-hydra)
