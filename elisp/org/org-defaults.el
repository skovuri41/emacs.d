(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  )
(require 'org)
(require 'org-element)
;; (require 'org-pomodoro)
;;(require 'org-drill)
;;(require 'org-jira-protocol)
;;(require 'org-gogs-protocol)
;;(require 'org-sip-protocol)
;;(require 'org-debbugs-protocol)
;; (require 'org-bullets)
;;(require 'ob-js)
;;(require 'ox-md)
(require 'avy)
(require 'ivy)
(require 's)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode)) ;; Journal entries
(add-hook 'org-mode-hook #'hl-line-mode)
(defun my/org-mode-hook ()
  (interactive)
  (turn-on-auto-fill)
  (turn-on-flyspell)
  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode 1)))
(add-hook 'org-mode-hook #'my/org-mode-hook)
(add-hook 'org-mode-hook 'yas-minor-mode-on)
(add-hook 'org-mode-hook 'company-mode)
(add-hook 'org-mode-hook 'set-org-mode-app-defaults)
;; (add-hook 'org-mode-hook #'my/org-mode-key-bindings)

(validate-setq org-directory (file-truename "~/org"))
(validate-setq org-default-notes-file (concat org-directory "/inbox.org"))
(validate-setq org-replace-disputed-keys t)
(validate-setq org-startup-folded t)
(validate-setq org-startup-indented t)
(validate-setq org-startup-with-inline-images t)
(validate-setq org-startup-truncated t)

;; follow links by pressing ENTER on them
(validate-setq org-return-follows-link t)
;; don't adapt indentation
(validate-setq  org-adapt-indentation nil)
;; Imenu should use 3 depth instead of 2
(validate-setq  org-imenu-depth 3)
;; special begin/end of line to skip tags and stars
(validate-setq  org-special-ctrl-a/e t)
;; special keys for killing a headline
(validate-setq  org-special-ctrl-k t)
;; don't adjust subtrees that I copy
(validate-setq  org-yank-adjusted-subtrees nil)
;; try to be smart when editing hidden things
(validate-setq  org-catch-invisible-edits 'smart)
(validate-setq org-hide-leading-stars t)
(validate-setq org-odd-levels-only nil)
(validate-setq org-list-allow-alphabetical t)
(validate-setq org-cycle-include-plain-lists t)
(validate-setq org-cycle-separator-lines 0)
(validate-setq org-cycle-include-plain-lists t)
(validate-setq org-blank-before-new-entry (quote ((heading)
                                                  (plain-list-item . auto))))
(validate-setq org-hide-emphasis-markers t)
(validate-setq org-reverse-note-order nil)
;; (validate-setq org-tags-column 80)

(add-hook 'focus-in-hook
          (lambda () (progn
                  (setq org-tags-column (- 5 (window-body-width)))) (org-align-all-tags)))

(add-hook 'focus-out-hook
          (lambda () (progn
                  (setq org-tags-column (- 5 (window-body-width)))) (org-align-all-tags)))

;; Block entries from changing state to DONE while they have children
;; that are not DONE
(validate-setq org-enforce-todo-dependencies t)
(setq org-use-fast-todo-selection t)
;; put state change log messages into a drawer
(validate-setq org-log-into-drawer t)
(validate-setq org-log-done (quote time))
(validate-setq org-log-redeadline (quote time))
(validate-setq org-log-reschedule (quote time))
(validate-setq org-ellipsis "⤵")

(validate-setq org-use-speed-commands t)




(provide 'org-defaults)
