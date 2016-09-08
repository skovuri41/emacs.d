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
          company-minimum-prefix-length 3
          company-idle-delay 0.10
          company-show-numbers t
          company-occurrence-weight-function #'company-occurrence-prefer-any-closest
          company-continue-commands
          (append company-continue-commands
                  '(comint-previous-matching-input-from-input
                    comint-next-matching-input-from-input)))
    (progn
      (add-to-list 'company-backends 'company-yasnippet)
      (add-to-list 'company-backends 'company-dabbrev)
      (add-to-list 'company-backends 'company-nxml)
      (add-to-list 'company-backends 'company-files)
      (add-to-list 'company-backends 'company-keywords)
      (add-to-list 'company-backends 'company-capf)
      (add-to-list 'company-backends 'company-ispell t)
      ;; (add-to-list 'company-backends 'company-web-html)
      )

    (define-key company-active-map (kbd "\C-n") 'company-select-next)
    (define-key company-active-map (kbd "\C-p") 'company-select-previous)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "<tab>") 'company-complete)
    (define-key company-active-map [escape] 'company-abort)
    (define-key company-active-map (kbd "j") 'company-select-next)
    (define-key company-active-map (kbd "k") 'company-select-previous)
    ;; org-mode completions
    (defun my-pcomplete-capf ()
      (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
    (add-hook 'org-mode-hook #'my-pcomplete-capf)
    ))

(use-package company-quickhelp
  :ensure t
  :init
  (progn
    ;; (company-quickhelp-mode 1)
    (eval-after-load 'company
      '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))
    ))

(use-package company-statistics
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'after-init-hook 'company-statistics-mode)))

(provide 'init-company)
