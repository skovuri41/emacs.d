;;; org-extra.el --- org extras                      -*- lexical-binding: t; -*-

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

(require 'ox-org)
(require 'ox-md)
(require 'org-habit)

(use-package org-bullets
  :ensure t
  :init
  ;; (setq org-bullets-bullet-list
  ;;       '("►" "◉" "★" "○" "◇" "◉" "○" ))
  (setq org-bullets-bullet-list '("■" "►" "◆" "▶"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package worf
  :ensure t
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (auto-fill-mode)
              (worf-mode)
              (org-indent-mode)))
  :config
  (progn
    (bind-keys :map worf-mode-map
               ("M-j" . nil))))

(add-to-list 'org-modules 'org-habit)

(setq org-habit-graph-column 60)
(setq org-habit-show-habits-only-for-today t)
(setq org-habit-graph-column 40)
(setq org-habit-preceding-days 7)
(setq org-habit-following-days 1)

;; Notifications
(appt-activate t)
(setq appt-display-format 'window)
(setq appt-disp-window-function #'appt-disp-window)
(setq appt-display-mode-line t)
(defun my-org-check-appt ()
  (org-agenda-to-appt t `(:deadline
			  :scheduled
			  (headline ,og-org-agenda-appt-headline))))

(my-org-check-appt)
(run-at-time nil 600 #'my-org-check-appt)

(use-package org-pomodoro
  :commands (org-pomodoro)
  :ensure t
  :config
  (progn
    (setq org-pomodoro-length 2)
    (setq org-pomodoro-long-break-length 1)
    (setq org-pomodoro-short-break-length 1)
    (when *is-a-mac*
      (setq org-pomodoro-audio-player "/usr/bin/afplay"))))


(use-package org-mobile
  :disabled t
  :config
  (progn
    (setq org-mobile-directory "~/Documents/mobileorg"
          org-mobile-files (list
                            "~/Documents/org"
                            "~/Documents/Work/org")
          org-mobile-inbox-for-pull "~/Documents/org/inbox.org")))

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
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))))

(use-package org-cliplink :ensure t)

(use-package org-download
  :ensure t
  :commands (org-download-enable
             org-download-yank
             org-download-screenshot)
  :init
  (progn
    (add-hook 'org-mode-hook 'org-download-enable)))

(provide 'org-extra)
;;; org-extra.el ends here

