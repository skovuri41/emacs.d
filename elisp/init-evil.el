(use-package evil
  :init
  (progn
    (setq evil-move-beyond-eol t)
    (evil-mode 1)
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader "SPC")
        (setq evil-leader/in-all-states 1) 
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
          "hp" 'helm-projectile
          "h'" 'helm-all-mark-rings
          "hs" 'helm-swoop
          "ha" 'helm-do-ag 
          "hA" 'helm-ag-project-root
          "hi" 'helm-imenu
          "hI" 'helm-imenu-anywhere
          ))) 
    (use-package evil-org
      :init (add-hook 'org-mode-hook 'evil-org-mode))
    (use-package evil-surround
      :init (global-evil-surround-mode 1)
      :config
      (progn
        (add-to-list 'evil-surround-operator-alist '(evil-paredit-change . change))
        (add-to-list 'evil-surround-operator-alist '(evil-paredit-delete . delete))))
    (use-package evil-escape
      :ensure t
      :diminish evil-escape-mode
      :config
      (evil-escape-mode)
      (setq-default evil-escape-key-sequence "kj")
      (setq-default evil-escape-delay 0.2))
    (use-package evil-cleverparens
      :diminish evil-cleverparens-mode
      :init
      (progn
        (defun set-cleverparens-key-bindings ()
          (progn  (evil-define-key 'normal evil-cleverparens-mode-map (kbd "M-w") nil)
                  (evil-define-key 'normal evil-cleverparens-mode-map (kbd "M-[") nil)
                  (evil-define-key 'normal evil-cleverparens-mode-map (kbd "M-z") nil)
                  (setq evil-move-beyond-eol nil)
                  ))
        (add-hook 'evil-cleverparens-enabled-hook 'set-cleverparens-key-bindings)
        (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode)
        (add-hook 'clojure-mode-hook 'evil-cleverparens-mode)
        )
      )
    )
  :config
  (progn
    (setq evil-cross-lines t)
    (setq evil-move-cursor-back nil)
    ;; colorful mode line
    (defface my-evil-state-emacs-face
      '((t (:background "Orange" :foreground "White")))
      "Evil Mode Emacs State Face")
    (defface my-evil-state-insert-face
      '((t (:background "DodgerBlue1" :foreground "White")))
      "Evil Mode Insert State Face")
    (defface my-evil-state-normal-face
      '((t (:background "Red" :foreground "White")))
      "Evil Mode Normal Stace Face")
    (defface my-evil-state-visual-face
      '((t (:background "Green" :foreground "White")))
      "Evil Mode Visual Stace Face")

    (defun evil-generate-mode-line-tag (&optional state)
      "Generate the evil mode-line tag for STATE."
      (let ((tag (evil-state-property state :tag t)))
        ;; prepare mode-line: add tooltip
        (if (stringp tag)
            (propertize tag
                        'face (cond
                               ((string= "normal" state)
                                'my-evil-state-normal-face)
                               ((string= "insert" state)
                                'my-evil-state-insert-face)
                               ((string= "visual" state)
                                'my-evil-state-visual-face)
                               ((string= "emacs" state)
                                'my-evil-state-emacs-face))
                        'help-echo (evil-state-property state :name)
                        'mouse-face 'mode-line-highlight)
          tag)))
    
    ;; gui mode
    (defun evil-set-cursor-by-state ()
      (message "evil-set-cursor-by-state")
      (when (display-graphic-p)
        (setq evil-emacs-state-cursor '("red" box)
              evil-normal-state-cursor '("green" box)
              evil-visual-state-cursor '("orange" box)
              evil-insert-state-cursor '("red" bar)
              evil-replace-state-cursor '("red" bar)
              evil-operator-state-cursor '("red" hollow))))

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

    (add-hook 'evil-visual-state-entry-hook 'evil-set-cursor-by-state)
    ;;(add-hook 'global-evil-leader-mode-hook 'evil-set-cursor-by-state)
    ;;(add-hook 'evil-leader-mode-hook 'evil-set-cursor-by-state)
    ;; (eval-after-load 'turn-on-evil-mode 'evil-set-cursor-by-state)

    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    (define-key evil-normal-state-map ";" 'evil-ex)
    (define-key evil-visual-state-map ";" 'evil-ex)

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
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (evil-set-initial-state 'magit-log-mode 'normal)))
(provide 'init-evil)
