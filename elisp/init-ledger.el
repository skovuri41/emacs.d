;;; init-ledger.el --- init-ledger                   -*- lexical-binding: t; -*-

(require-package 'ledger-mode)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(when (maybe-require-package 'flycheck-ledger)
  (after-load 'flycheck
    (after-load 'ledger-mode
      (require 'flycheck-ledger))))

(after-load 'ledger-mode
  (define-key ledger-mode-map (kbd "RET") 'newline)
  (define-key ledger-mode-map (kbd "C-o") 'open-line))

(setq ledger-highlight-xact-under-point nil
      ledger-use-iso-dates nil)

(after-load 'ledger-mode
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "LEDGER_FILE")))

(add-hook 'ledger-mode-hook 'goto-address-prog-mode)

;;; Customize ledger appearance
(set-face-foreground 'ledger-font-periodic-xact-face "aquamarine4")
(set-face-foreground 'ledger-font-posting-account-face "gray65")
(set-face-foreground 'ledger-font-payee-uncleared-face "#859900")
(set-face-bold-p 'ledger-font-payee-uncleared-face nil)
(set-face-foreground 'ledger-font-posting-date-face "#859900")

;;; Provide a couple of customized reports
(add-to-list 'ledger-report-format-specifiers '("start-of-month" . (lambda () (format-time-string "%Y-%m-01"))))

(add-to-list 'ledger-reports '("budgeted" "ledger -f %(ledger-file) budget --begin %(start-of-month)"))
(add-to-list 'ledger-reports '("unbudgeted" "ledger -f %(ledger-file) budget --unbudgeted --begin %(start-of-month)"))


(provide 'init-ledger)
