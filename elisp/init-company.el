(use-package company
  :diminish company-mode
  :config
  (progn
    (validate-setq company-auto-complete nil)
    (validate-setq company-require-match nil)
    (validate-setq company-minimum-prefix-length 3)
    (validate-setq company-show-numbers t)
    ;; If I actually get the point, this variable `company-begin-commands` controls
    ;; what commands of emacs can triger the starting of company.
    ;; `self-insert-command` means typing IO.
    (validate-setq company-begin-commands '(self-insert-command))
    (validate-setq company-idle-delay 0)
    (validate-setq company-tooltip-align-annotations t
                   company-tooltip-flip-when-above t
                   company-occurrence-weight-function #'company-occurrence-prefer-any-closest)
    (validate-setq company-frontends
                   '(company-pseudo-tooltip-unless-just-one-frontend
                     company-preview-if-just-one-frontend))

    (let ((map company-active-map))
      (mapc (lambda (x) (define-key map (format "%d" x)
                     `(lambda () (interactive) (company-complete-number ,x))))
            (number-sequence 0 9))
      (define-key map " " (lambda () (interactive)
                            (company-abort)
                            (self-insert-command 1)))
      (define-key map (kbd "<return>") nil))

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

    (use-package company-statistics
      :commands (company-statistics-mode))
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
      :bind ("C-'" . company-try-hard)
      :config
      (bind-keys :map company-active-map
                 ("C-'" . company-try-hard)))
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
      :ensure t
      :config
      (progn
        (add-to-list 'company-backends 'company-math-symbols-unicode)))
    (use-package company-dabbrev-code
      :after company
      :init (require #'company-dabbrev-code)
      :config
      (validate-setq company-dabbrev-code-everywhere t))
    (use-package company-math
      :init (require #'company-math)
      :after company
      :defer t
      :config
      (progn
        (add-to-list 'company-backends 'company-math-symbols-unicode)
        (add-to-list 'company-backends 'company-math-symbols-latex)))

    (defun my-pcomplete-capf ()
      (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
    (add-hook 'org-mode-hook #'my-pcomplete-capf)

    ;; use company-statistics to arrange the order of candidates, show more probably selected one to the first
    (defun setup-company-mode (backends)
      "turn-on company-mode, then make variable company-backends to buffer local, and set it to BACKENDS.
     Example: for elisp, (setup-company-mode '(company-elisp))"
      (company-mode 1)
      (company-statistics-mode)
      (make-local-variable 'company-backends)
      (validate-setq company-backends backends))

    (add-hook 'emacs-lisp-mode-hook
              '(lambda () (setup-company-mode '((company-dabbrev-code
                                            company-gtags
                                            company-etags
                                            company-keywords)
                                           company-elisp
                                           company-capf
                                           company-files
                                           ))))

    (add-hook 'clojure-mode-hook
              '(lambda () (setup-company-mode '((company-dabbrev-code
                                            company-gtags
                                            company-etags
                                            company-keywords)
                                           company-capf
                                           company-files
                                           ))))

    (add-hook 'cider-mode-hook
              '(lambda () (setup-company-mode '((company-dabbrev-code
                                            company-gtags
                                            company-etags
                                            company-keywords)
                                           company-capf
                                           company-files
                                           ))))

    (add-hook 'java-mode-hook
              '(lambda () (setup-company-mode '(company-eclim
                                           (company-dabbrev-code
                                            company-gtags
                                            company-etags
                                            company-keywords)
                                           company-capf
                                           company-files))))

    (add-hook 'org-mode-hook
              '(lambda () (setup-company-mode '(company-dabbrev
                                           (company-dabbrev-code
                                            company-gtags
                                            company-etags
                                            company-keywords)
                                           company-capf
                                           company-files company-ispell))))

    ))

;;; useful company-backend
;;  company-c-headers
;;  company-elisp
;;  company-bbdb ;; BBDB stands for The Insidious Big Brother Database – an address book that you can hook into your mail- and newsreader, sync with your mobile device, etc.
;;  company-nxml
;;  company-css
;;  company-eclim
;;  company-semantic ;; completion backend using CEDET Semantic
;;  company-clang
;;  company-xcode
;;  company-cmake
;;  company-capf
;;  (company-dabbrev-code company-gtags company-etags company-keywords)
;;  company-oddmuse
;;  company-files
;;  company-dabbrev ;; this is very useful!

(provide 'init-company)
