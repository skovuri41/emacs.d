(use-package company
  :init (global-company-mode)
  :config
  (progn
    (defun indent-or-complete ()
      (interactive)
      (if (looking-at "\\_>")
          (company-complete-common)
      (indent-according-to-mode)))
      (global-set-key "\t" 'indent-or-complete)
      (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-require-match nil
        company-minimum-prefix-length 2
        company-idle-delay 0.25
        company-show-numbers t
        company-occurrence-weight-function #'company-occurrence-prefer-any-closest
        company-continue-commands
        (append company-continue-commands
                '(comint-previous-matching-input-from-input
                  comint-next-matching-input-from-input)))))

(provide 'init-company)
