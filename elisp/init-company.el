(use-package company
  :init (global-company-mode)
  :diminish company-mode
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
                    comint-next-matching-input-from-input)))
    (define-key company-active-map (kbd "\C-n") 'company-select-next)
    (define-key company-active-map (kbd "\C-p") 'company-select-previous)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "<tab>") 'company-complete)
    (define-key company-active-map [escape] 'company-abort)
    (define-key company-active-map (kbd "j") 'company-select-next)
    (define-key company-active-map (kbd "k") 'company-select-previous)))

(use-package company-quickhelp
  :ensure t
  :init
  (progn
    ;; (company-quickhelp-mode 1)
    (eval-after-load 'company
      '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))
    ))

(provide 'init-company)
