(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))
(use-package json-mode
  :defer t)

(use-package scratch
  :ensure t)

;;;; Modes ;;;;
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(use-package with-editor
  :ensure t
  :init
  (progn
    (add-hook 'shell-mode-hook  'with-editor-export-editor)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

(use-package smartscan
  :ensure t
  :init
  (add-hook #'prog-mode-hook #'smartscan-mode)
  :config
  (define-key smartscan-map (kbd "M-n") nil)
  (define-key smartscan-map (kbd "M-p") nil)
  (define-key smartscan-map (kbd "M-'") nil))

;;; Visible mark
(use-package visible-mark
  :ensure t
  :init
  (defface face2
    '((((type tty) (class mono)))
      (t (:foreground "#FF1D00" :background "#E593C3")))
    "Face for flycheck error feedback in the modeline."
    :group 'visible-mark)
  :config
  (setq visible-mark-max 1)
  (setq visible-mark-faces `(face2 visible-mark-face2))
  (global-visible-mark-mode 1))

(use-package bug-hunter                            ; Search init file for bugs
  :ensure t)

(use-package shrink-whitespace
  :ensure t
  )

(use-package paren-face
  :ensure t
  :init (global-paren-face-mode))

(use-package find-file-in-project
  :ensure t
  ;; :config (setq ffip-prefer-ido-mode t)
  )

(use-package outshine
  :diminish outline-minor-mode
  :commands outshine-hook-function
  :init
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'picolisp-mode-hook 'outline-minor-mode)
  (add-hook 'clojure-mode-hook 'outline-minor-mode)
  (add-hook 'ess-mode-hook 'outline-minor-mode)
  (add-hook 'ledger-mode-hook 'outline-minor-mode)
  (add-hook 'message-mode-hook 'outline-minor-mode)
  )

(use-package paradox
  :ensure t
  :init
  (setq paradox-github-token t)
  (setq paradox-column-width-version 14)
  (setq paradox-column-width-package 30)
  :config
  (progn
    (define-key package-menu-mode-map "j" 'next-line)
    (define-key package-menu-mode-map "k" 'previous-line)

;;;###autoload
    (defun ora-package-menu-hook ()
      (setq tabulated-list-format
            [("Package" 28 package-menu--name-predicate)
             ("Version" 18 nil)
             ("Status" 10 package-menu--status-predicate)
             ("Description" 50 nil)])
      (tabulated-list-init-header))))

;; https://www.emacswiki.org/emacs/sequential-command-config.el
(use-package sequential-command
  :ensure t
  )

(use-package bm
  :ensure t
  :commands (bm-buffer-restore bm-buffer-save bm-toggle bm-next bm-previous)
  :init
  (setq bm-repository-file "~/.emacs.d/.bm-repository")
  (setq-default bm-buffer-persistence t)
  (setq bm-restore-repository-on-load t)
  (setq bm-marker 'bm-marker-left)
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  (setq bm-cycle-all-buffers t)
  (defface bm-fringe-persistent-face
    '((((class grayscale)
        (background light)) (:background "DimGray"))
      (((class grayscale)
        (background dark))  (:background "LightGray"))
      (((class color)
        (background light)) (:foreground "White" :background "#E593C3"))
      (((class color)
        (background dark))  (:foreground "White" :background "#E593C3")))
    "Face used to highlight current line if bookmark is persistent."
    :group 'bm)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  (defadvice bm-show-mode
      (around bm-show-mode-with-linum activate)
    ad-do-it
    (linum-mode))

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  )

(provide 'init-minor-modes)
