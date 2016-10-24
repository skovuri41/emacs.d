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
  (progn
    (setq smartscan-symbol-selector "symbol")
    (define-key smartscan-map (kbd "M-n") nil)
    (define-key smartscan-map (kbd "M-p") nil)
    (define-key smartscan-map (kbd "M-'") nil)))

;;; Visible mark
(use-package visible-mark
  :ensure t
  :disabled t
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

(use-package bug-hunter    ; Search init file for bugs
  :ensure t)

(use-package shrink-whitespace
  :ensure t
  )

(use-package paren-face
  :ensure t
  :config (global-paren-face-mode))

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
  :config
  (progn
    (define-sequential-command my-quit-dwim quit-window
      popwin:close-popup-window)))

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
        (background dark)) (:background "LightGray"))
      (((class color)
        (background light)) (:foreground "White" :background "#E593C3"))
      (((class color)
        (background dark)) (:foreground "White" :background "#E593C3")))
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
                                (bm-repository-save))))

(use-package volatile-highlights
  :ensure t
  :diminish (volatile-highlights-mode . " ")
  :config
  (volatile-highlights-mode 1)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

(use-package diff-hl
  :ensure t
  :disabled t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package vkill
  :defer t
  :disabled t
  :commands vkill
  :bind ("C-x L" . vkill-and-helm-occur)
  :init
  (defun vkill-and-helm-occur ()
    (interactive)
    (vkill)
    (my/turn-on-hl-line-mode)
    (call-interactively #'helm-occur)))

(use-package view
  :ensure t
  :defer t
  :bind
  (("C-M-n" . View-scroll-half-page-forward)
   ("C-M-p" . View-scroll-half-page-backward))
  :config
  (progn
    ;; When in view-mode, the buffer is read-only:
    (setq view-read-only t)

    (defun View-goto-line-last (&optional line)
      "goto last line"
      (interactive "P")
      (goto-line (line-number-at-pos (point-max))))

    ;; less like
    (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
    (define-key view-mode-map (kbd "?") 'View-search-regexp-backward?)
    (define-key view-mode-map (kbd "g") 'View-goto-line)
    (define-key view-mode-map (kbd "G") 'View-goto-line-last)
    ;; vi/w3m like
    (define-key view-mode-map (kbd "h") 'backward-char)
    (define-key view-mode-map (kbd "j") 'next-line)
    (define-key view-mode-map (kbd "k") 'previous-line)
    (define-key view-mode-map (kbd "l") 'forward-char)))

(use-package doc-view
  :config
  (define-key doc-view-mode-map (kbd "j")
    #'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "k")
    #'doc-view-previous-line-or-previous-page)
  ;; use 'q' to kill the buffer, not just hide it
  (define-key doc-view-mode-map (kbd "q")
    #'kill-this-buffer))

(use-package fullframe
  :ensure t
  :config
  (progn
    (after 'magit
      (fullframe magit-log-all magit-mode-quit-window)
      (fullframe magit-log-current magit-mode-quit-window)
      (fullframe magit-status magit-mode-quit-window))
    (fullframe list-packages quit-window) ))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(eval-after-load 'grep
  '(define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
(eval-after-load 'wgrep
  '(define-key grep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit))

(use-package graphviz-dot-mode
  :mode (("\\.diag\\'" . graphviz-dot-mode)
         ("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :ensure t
  :config (setq graphviz-dot-indent-width 4))

(use-package region-bindings-mode
  :ensure t
  :after xah-fly-keys
  :disabled t
  :config
  (progn
    ;; Do not activate `region-bindings-mode' in Special modes like `dired' and
    ;; `ibuffer'. Single-key bindings like 'm' are useful in those modes even
    ;; when a region is selected.
    (setq region-bindings-mode-disabled-modes '(dired-mode
                                                ibuffer-mode))
    (region-bindings-mode-enable)

    (defun modi/disable-rbm-deactivate-mark ()
      "Disable `region-bindings-mode' and deactivate mark."
      (interactive)
      (region-bindings-mode -1)
      (deactivate-mark)
      (message "Mark deactivated"))

    (bind-keys
     :map region-bindings-mode-map
     ("x" . xah-cut-line-or-region)
     ("y" . xah-copy-line-or-region)
     ("(" . lispy-parens)
     ("{" . lispy-braces)
     ("'" . lispy-quotes)
     ("\"" . lispy-doublequote)
     ("c" . duplicate-current-line-or-region)
     ("n" . fancy-narrow-to-region)
     ("E" . eval-region)
     ("/" . swiper-the-thing)
     ("q" . anzu-query-replace)
     ("<C-SPC>" . modi/disable-rbm-deactivate-mark))))

(use-package selected
  :ensure t
  :after xah-fly-keys
  :disabled t
  :commands selected-minor-mode
  :init
  (setq selected-org-mode-map (make-sparse-keymap))
  (selected-global-mode)
  :bind (:map selected-keymap
              ("x" . xah-cut-line-or-region)
              ("y" . xah-copy-line-or-region)
              ("(" . lispy-parens)
              ("{" . lispy-braces)
              ("'" . lispy-quotes)
              ("\"" . lispy-doublequote)
              ("c" . duplicate-current-line-or-region)
              ("n" . fancy-narrow-to-region)
              ("E" . eval-region)
              ("/" . swiper-the-thing)
              ("q" . anzu-query-replace)
              ("<C-SPC>" . modi/disable-rbm-deactivate-mark)
              ("l" . xah-toggle-letter-case)
              ("w" . count-words-region)
              ("m" . apply-macro-to-region-lines)
              :map selected-org-mode-map
              ("t" . org-table-convert-region)))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))

(use-package log4j-mode
  :ensure t
  :disabled t
  :init
  (autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
    (add-hook 'log4j-mode-hook
              (lambda ()
                (setq truncate-lines t)
                (text-scale-set -1)
                (toggle-read-only t)
                (buffer-disable-undo)
                (end-of-buffer)))))

(use-package fill-column-indicator
  :ensure t
  :commands fci-mode
  :config
  (setq fci-rule-column 79)
  (fci-mode))

(use-package logview
  :ensure t
  :mode ("log$" . logview-mode)
  :init
  (add-hook
   'logview-mode-hook
   (lambda ()
     (linum-mode -1)
     (toggle-truncate-lines 1)))
  :config
  (setq logview-auto-revert-mode 'auto-revert-tail-mode))

(provide 'init-minor-modes)
