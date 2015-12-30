(use-package evil
  :init
  (progn
    (evil-mode 1)
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader "SPC")
        (evil-leader/set-key "wd" 'delete-window)
        (evil-leader/set-key "wo" 'delete-other-windows)
        (evil-leader/set-key "ws" 'split-window-below)
        (evil-leader/set-key "wh" 'split-window-horizontally)
        (evil-leader/set-key "wv" 'split-window-vertically)
        (evil-leader/set-key "ww" 'other-window)
        (evil-leader/set-key
        "hf" 'helm-for-files
        "hl" 'helm-locate
        "hy" 'helm-show-kill-ring
        "ht" 'helm-top
        "hm" 'helm-man-woman
        "ho" 'helm-occur
        "hx" 'helm-M-x
        "he" 'helm-find-files
        "hb" 'helm-buffers-list
        "hh" 'helm-projectile-find-file
        "hr" 'helm-recentf
        "hp" 'helm-projectile)))
    (use-package evil-org
      :init (add-hook 'org-mode-hook 'evil-org-mode))
    (use-package evil-surround
      :init (global-evil-surround-mode 1)
      :config
      (progn
        (add-to-list 'evil-surround-operator-alist '(evil-paredit-change . change))
        (add-to-list 'evil-surround-operator-alist '(evil-paredit-delete . delete)))))
    (use-package evil-escape
      :ensure t
      :diminish evil-escape-mode
      :config
      (evil-escape-mode)
      (setq-default evil-escape-key-sequence "kj")
      (setq-default evil-escape-delay 0.2))
  :config
  (progn
    (setq evil-cross-lines t)
    (setq evil-move-cursor-back nil)
    ;; gui mode
    (when (window-system)
     (setq evil-emacs-state-cursor '("red" box))
     (setq evil-normal-state-cursor '("green" box))
     (setq evil-visual-state-cursor '("orange" box))
     (setq evil-insert-state-cursor '("red" bar))
     (setq evil-replace-state-cursor '("red" bar))
     (setq evil-operator-state-cursor '("red" hollow)))

    ;;; esc quits
    (defun minibuffer-keyboard-quit ()
        "Abort recursive edit.
         In Delete Selection mode, if the mark is active, just deactivate it;
         then it takes a second \\[keyboard-quit] to abort the minibuffer."
        (interactive)
        (if (and delete-selection-mode transient-mark-mode mark-active)
            (setq deactivate-mark  t)
          (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
          (abort-recursive-edit)))
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    ;;(define-key helm-map [escape] 'helm-keyboard-quit)

    ;;(define-key evil-normal-state-map "\C-j"  'evil-window-down)
    ;;(define-key evil-normal-state-map "\C-k"  'evil-window-up)
    ;;(define-key evil-normal-state-map "\C-h"  'evil-window-left)
    ;;(define-key evil-normal-state-map "\C-l"  'evil-window-right)

    ;; Split and move the cursor to the new split
    (define-key evil-normal-state-map (kbd "-")
      (lambda ()
        (interactive)
        (split-window-vertically)
        (other-window 1)))
    (define-key evil-normal-state-map (kbd "|")
      (lambda ()
        (interactive)
        (split-window-horizontally)
            (other-window 1)))

    ;; Set the initial evil state that certain major modes will be in.
    (evil-set-initial-state 'magit-log-edit-mode 'emacs)
    (evil-set-initial-state 'nav-mode 'emacs)
    (evil-set-initial-state 'grep-mode 'emacs)
    (evil-set-initial-state 'ibuffer-mode 'normal)
    (evil-set-initial-state 'eshell-mode 'emacs)
    (evil-set-initial-state 'shell-mode 'emacs)
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-diff-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)))
(provide 'init-evil)

