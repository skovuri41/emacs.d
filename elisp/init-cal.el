(use-package calfw
  :ensure t
  :ensure calfw-gcal
  :config
  (require 'calfw-ical)
  (require 'calfw-org)
  ;; (require 'calfw-gcal)
  (require 'calfw-cal)
  (defun keymaps/calfw ()
    "Keymaps for calfw."
    (evil-set-initial-state 'cfw:calendar-mode 'emacs)

    (define-key cfw:calendar-mode-map "h" 'cfw:navi-previous-day-command)
    (define-key cfw:calendar-mode-map "j" 'cfw:navi-next-week-command)
    (define-key cfw:calendar-mode-map "k" 'cfw:navi-previous-week-command)
    (define-key cfw:calendar-mode-map "l" 'cfw:navi-next-day-command)
    (define-key cfw:calendar-mode-map "q" 'bury-buffer)
    (define-key cfw:calendar-mode-map "r" 'cfw:refresh-calendar-buffer)
    (define-key cfw:calendar-mode-map "t" 'cfw:navi-goto-today-command)
    (define-key cfw:calendar-mode-map (kbd "TAB") 'cfw:show-details-command)
    (define-key cfw:calendar-mode-map "$" 'cfw:navi-goto-week-end-command)
    (define-key cfw:calendar-mode-map "0" 'cfw:navi-goto-week-begin-command)
    (define-key cfw:calendar-mode-map ">" 'cfw:navi-next-month-command)
    (define-key cfw:calendar-mode-map "<" 'cfw:navi-previous-month-command)
    (define-key cfw:calendar-mode-map "D" 'cfw:change-view-day)
    (define-key cfw:calendar-mode-map "M" 'cfw:change-view-month)
    (define-key cfw:calendar-mode-map "W" 'cfw:change-view-week)
    (define-key cfw:calendar-mode-map "o" 'cfw:navi-goto-date-command)
    )
  (add-hook 'cfw:calendar-mode-hook 'keymaps/calfw)
  ;; (setq cfw:org-agenda-schedule-args '(:timestamp))
  (add-hook 'cfw:calendar-mode-hook #'(lambda () (visual-line-mode -1)))
  (add-hook 'cfw:calendar-mode-hook #'(lambda ()
                                        (setq-local global-hl-line-mode nil)))
  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (when (boundp 'gcal-url)
        ;;googlecalendarICS
        (cfw:ical-create-source "gcal" gcal-url "IndianRed"))
      ;; (cfw:open-ical-calendar gcal-url)

      ;; orgmode source
      (cfw:org-create-source "Green")

      ;; diary source
      ;; (cfw:cal-create-source "Orange")
      )))

  ;; (setq cfw:fchar-junction ?╋
  ;;       cfw:fchar-vertical-line ?┃
  ;;       cfw:fchar-horizontal-line ?━
  ;;       cfw:fchar-left-junction ?┣
  ;;       cfw:fchar-right-junction ?┫
  ;;       cfw:fchar-top-junction ?┯
  ;;       cfw:fchar-top-left-corner ?┏
  ;;       cfw:fchar-top-right-corner ?┓)

  (setq cfw:fchar-junction ?╬
        cfw:fchar-vertical-line ?║
        cfw:fchar-horizontal-line ?═
        cfw:fchar-left-junction ?╠
        cfw:fchar-right-junction ?╣
        cfw:fchar-top-junction ?╦
        cfw:fchar-top-left-corner ?╔
        cfw:fchar-top-right-corner ?╗)

  (when (and (boundp 'gcal-client-id) (boundp 'gcal-client-secret) (boundp 'gcal-email))
    (use-package org-gcal
      :ensure t
      :config
      (setq org-gcal-client-id gcal-client-id
            org-gcal-client-secret gcal-client-secret
            org-gcal-file-alist `((,gcal-email .  "~/org/gcal.org")))))
  (setq org-gcal-up-days 30)
  (setq org-gcal-down-days 180)

  )
(provide 'init-cal)
