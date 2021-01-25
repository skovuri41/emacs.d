(use-package scratch
  :ensure t)

;;;; Modes ;;;;
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
;; go-to-address-mode
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
(push '("\\.epub\\'" . nov-mode) auto-mode-alist)

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
  :ensure t)

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode "w"
  :config (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package paren-face
  :ensure t
  :config (global-paren-face-mode))

(use-package find-file-in-project
  :ensure t)

(use-package outshine
  :ensure t
  :diminish outline-minor-mode
  :init
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'picolisp-mode-hook 'outline-minor-mode)
  (add-hook 'clojure-mode-hook 'outline-minor-mode)
  (add-hook 'ess-mode-hook 'outline-minor-mode)
  (add-hook 'ledger-mode-hook 'outline-minor-mode)
  (add-hook 'message-mode-hook 'outline-minor-mode))

(use-package paradox
  :ensure t
  :init
  (setq paradox-github-token t)
  (setq paradox-column-width-version 14)
  (setq paradox-column-width-package 30)
  :config
  (progn
    (define-key paradox-menu-mode-map "." 'hydra-paradox-filter/body)
    (define-key package-menu-mode-map "j" 'next-line)
    (define-key package-menu-mode-map "k" 'previous-line)
    (ora-move-key "k" "K" paradox-menu-mode-map)
    (ora-move-key "j" "J" paradox-menu-mode-map)
    (define-key paradox-menu-mode-map "j" 'next-line)
    (define-key paradox-menu-mode-map "k" 'previous-line)

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
    (define-sequential-command my-quit-dwim xah-quit-window popwin:close-popup-window)
    (define-sequential-command my-lispy-hungry-delete lispy-delete-backward-or-splice-or-slurp hungry-delete-backward)))

(use-package smartrep
  :ensure t)

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
                                (bm-repository-save)))
  :config
  (progn
    ;; Make a more bookmarky symbol for a 'mark':
    (define-fringe-bitmap 'bm-marker-left [254 254 254 254 254 238 198 130] 8 8 'center)

    (defun bm-bookmark-defun ()
      "Drops a temporary breadcrumb/bookmark at the beginning of the current defun."
      (interactive)
      (save-excursion
        (beginning-of-defun)
        (bm-toggle)))

    (defun my-add-bookmark (name)
      (interactive
       (list (let* ((filename (file-name-base (buffer-file-name)))
                    (project (projectile-project-name))
                    (func-name (which-function))
                    (initial (format "%s::%s:%s " project filename func-name)))
               (read-string "Bookmark: " initial))))
      (bookmark-set name))))

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
  ;; :ensure t
  :disabled t
  :config
  (progn
    (after 'magit
      (fullframe magit-log-all magit-mode-quit-window)
      (fullframe magit-log-current magit-mode-quit-window)
      (fullframe magit-status magit-mode-quit-window))
    (fullframe dired-jump quit-window)
    (fullframe ibuffer ibuffer-quit)
    ;; (fullframe find-file quit-window true)
    (fullframe dired-up-directory quit-window)
    (fullframe org-capture quit-window)

    ;; (fullframe projectile-switch-project quit-window)
    ;; (fullframe list-packages quit-window)
    ))

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

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :init
  (page-break-lines-mode)
  (global-page-break-lines-mode))

(use-package log4j-mode
  :ensure t
  :disabled t
  (define-key xah-fly-key-map (kbd "<C-f11>") 'xah-previous-user-buffer)
  (define-key xah-fly-key-map (kbd "<C-f12>") 'xah-next-user-buffer)  :init
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

(use-package evil-matchit
  :ensure t
  :init
  (global-evil-matchit-mode 1))

(use-package jump-char
  :disabled t
  :config
  (progn
    (bind-keys :map jump-char-base-map
               ("C-n" . jump-char-repeat-forward)
               ("C-p" . jump-char-repeat-backward)
               ("C-'" . jump-char-switch-to-ace))))

(use-package vi-tilde-fringe
  :ensure t
  :defer t
  :hook (prog-mode . vi-tilde-fringe-mode)
  :config
  (global-vi-tilde-fringe-mode 1))

(use-package simpleclip
  :ensure t
  :config
  (simpleclip-mode 1))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

(use-package visual-fill-column
  :diminish visual-fill-column-mode
  :disabled t
  :config
  (add-hook 'prog-mode-hook #'visual-line-mode)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))


;; (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; TODO figure out handling long line mode, still problematic


(use-package point-history
  :load-path "elisp/"
  :config
  (progn
    (point-history-mode t)))

(use-package literate-calc-mode
  :ensure t)


(use-package buffer-flip
  :ensure t
  :bind  (("M-<tab>" . buffer-flip)
          :map buffer-flip-map
          ( "M-<tab>" .   buffer-flip-forward)
          ( "M-S-<tab>" . buffer-flip-backward)
          ( "M-S-<iso-lefttab>" . buffer-flip-backward)
          ( "M-<iso-lefttab>" . buffer-flip-backward)
          ( "M-ESC" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$")))

(use-package major-mode-hydra
  :ensure t)

(provide 'init-minor-modes)
