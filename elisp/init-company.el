(use-package company
  :ensure t
  :diminish company-mode
  :config
  (progn
    (validate-setq company-auto-commit nil)
    (validate-setq company-require-match nil)
    (validate-setq company-minimum-prefix-length 2)
    (setq company-show-numbers t)
    ;; If I actually get the point, this variable `company-begin-commands` controls
    ;; what commands of emacs can triger the starting of company.
    ;; `self-insert-command` means typing IO.
    (validate-setq company-begin-commands '(self-insert-command))
    (validate-setq company-idle-delay 0.4)
    (validate-setq company-tooltip-align-annotations t
                   company-tooltip-flip-when-above t
                   company-occurrence-weight-function #'company-occurrence-prefer-any-closest)
    (validate-setq company-frontends
                   '(company-pseudo-tooltip-unless-just-one-frontend
                     company-preview-if-just-one-frontend))

    (let ((map company-active-map))
      (mapc
       (lambda (x)
         (define-key map (format "%d" x) 'ora-company-number))
       (number-sequence 0 9))
      (define-key map " " (lambda ()
                            (interactive)
                            (company-abort)
                            (self-insert-command 1)))
      (define-key map (kbd "<return>") nil))

    (defun ora-company-number ()
      "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
      (interactive)
      (let* ((k (this-command-keys))
             (re (concat "^" company-prefix k)))
        (if (cl-find-if (lambda (s) (string-match re s))
                        company-candidates)
            (self-insert-command 1)
          (company-complete-number (string-to-number k)))))

    (define-key company-active-map (kbd "\C-n") 'company-select-next)
    (define-key company-active-map (kbd "\C-p") 'company-select-previous)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "C-<SPC>") 'company-complete)
    (define-key company-active-map [escape] 'company-abort)
    ;; (define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "C-o") #'company-other-backend)
    (define-key company-active-map (kbd "C-s") #'company-filter-candidates)
    (define-key company-active-map (kbd "C-M-s") #'company-search-candidates)
    (define-key company-active-map (kbd "<tab>") #'company-other-backend)
    (define-key company-active-map (kbd "C-.") #'company-try-hard)

    (use-package company-projectile-cd)
    (use-package company-statistics
      :commands (company-statistics-mode)
      :disabled t)
    (use-package company-elisp
      :commands (company-elisp))
    (use-package company-capf
      :commands (company-capf))
    (use-package company-files
      :commands (company-files))
    (use-package company-try-hard
      :commands company-try-hard
      :ensure t
      :after company
      :bind ("C-\\" . company-try-hard)
      :config
      (bind-keys :map company-active-map
                 ("C-\\" . company-try-hard)))
    (use-package company-quickhelp
      :ensure t
      :after company
      :config
      (progn
        (require #'company-quickhelp)
        (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)))
    (use-package company-dabbrev
      :commands (company-dabbrev)
      :after company
      :config
      (progn
        (validate-setq company-dabbrev-ignore-case nil)
        (validate-setq company-dabbrev-downcase nil)
        (validate-setq company-dabbrev-minimum-length 2)))
    (use-package company-emoji
      :after company
      :ensure t)
    (use-package company-dabbrev-code
      :after company
      :init (require #'company-dabbrev-code)
      :config
      (validate-setq company-dabbrev-code-everywhere t))
    (use-package company-math
      :init (require #'company-math)
      :after company
      :disabled t
      :config
      (progn
        (add-to-list 'company-backends 'company-math-symbols-unicode)
        (add-to-list 'company-backends 'company-math-symbols-latex)))

    (defun my-pcomplete-capf ()
      (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
    ;; (add-hook 'org-mode-hook #'my-pcomplete-capf)
    ))

(provide 'init-company)
