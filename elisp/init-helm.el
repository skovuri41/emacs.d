(use-package helm-flx
  ;; :ensure t
  :disabled t
  :init
  (helm-flx-mode +1))

(use-package helm
  :ensure t
  ;; :defer t
  :diminish helm-mode
  :init
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match nil ;; locate fuzzy is worthless
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-semantic-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-completion-in-region-fuzzy-match t)

  :config
  (progn
    (setq helm-autoresize-mode t)
    (setq helm-buffer-max-length 40)
    (helm-mode 1)
    (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
    (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)
    (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
    (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil))
    (add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
    (add-to-list 'helm-completing-read-handlers-alist '(helm-c-yas-complete . nil))
    (add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy . nil))
    (add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename . nil))
    (add-to-list 'helm-completing-read-handlers-alist '(nameframe-switch-frame . nil))
    (add-to-list 'helm-completing-read-handlers-alist '(eyebrowse-switch-to-window-config . nil))
    (add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory . nil)))

  (use-package helm-descbinds )
  ;; (use-package helm-mu)
  (use-package helm-gtags )
  (use-package helm-projectile)
  (use-package helm-swoop)
  (use-package helm-company
    :ensure t
    :config
    (eval-after-load 'company
      '(progn
         (define-key company-mode-map (kbd "C-:") 'helm-company)
         (define-key company-active-map (kbd "C-:") 'helm-company)))
    )

  (use-package helm-ag
    :ensure helm-ag
    :commands (helm-ag helm-projectile-ag))

  (use-package helm-backup
    ;; :bind ("C-c b" . helm-backup)
    :config
    (add-hook 'after-save-hook 'helm-backup-versioning))

  (use-package helm-org-rifle
    :ensure t
    :init
    (require 'helm-org-rifle))

  (use-package helm-hunks
    :ensure t
    :commands helm-hunks))

(provide 'init-helm)
