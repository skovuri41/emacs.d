;;; org-calendar.el --- org calendar                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  shyam

;; Author: shyam <shyam@hanuman>
;; Keywords: abbrev

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


(defvar american-holiday
  '((holiday-fixed 1 1 "New Year")
    (holiday-fixed 6 4 "US Independence Day")))

(setq calendar-date-style 'american
      calendar-holidays american-holiday
      calendar-mark-holidays-flag t)

;; org-clock
(use-package org-clock
  :config
  ;; Export TODO items in iCal too
  (validate-setq org-icalendar-include-todo t)
  (validate-setq org-clock-idle-time 15)
  (validate-setq org-clock-in-resume t)
  (validate-setq org-clock-persist t)
  (validate-setq org-clock-persist-query-resume nil)
  (validate-setq org-clock-clocked-in-display 'both)
  (validate-setq org-clock-frame-title-format
                 (append '((t org-mode-line-string)) '(" ") frame-title-format))
  (validate-setq org-clock-history-length 23
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
    (validate-setq org-x11idle-exists-p t)
    (validate-setq org-clock-x11idle-program-name "xprintidle"))
  (org-clock-persistence-insinuate))



(provide 'org-calendar)
;;; org-calendar.el ends here

