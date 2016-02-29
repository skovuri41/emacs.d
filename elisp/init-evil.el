(use-package evil
  :init
  (progn
    (setq evil-move-beyond-eol nil)
    (evil-mode 1)
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader "SPC")
        (setq evil-leader/in-all-states 1)
        (setq evil-leader/non-normal-prefix "S-")
        (evil-leader/set-key "wd" 'delete-window)
        (evil-leader/set-key "wo" 'delete-other-windows)
        (evil-leader/set-key "ws" 'split-window-below)
        (evil-leader/set-key "wh" 'split-window-horizontally)
        (evil-leader/set-key "wv" 'split-window-vertically)
        (evil-leader/set-key "bS" 'save-some-buffers)
        (evil-leader/set-key "bs" 'save-buffer)
        (evil-leader/set-key "ww" 'ace-window)
        (evil-leader/set-key
          "hf" 'helm-for-files
          "ff" 'helm-for-files
          "bn" 'xah-new-empty-buffer
          "by" 'bury-buffer
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
          "fr" 'helm-recentf
          "hp" 'helm-projectile
          "fp" 'helm-projectile
          "h'" 'helm-all-mark-rings
          "hs" 'helm-swoop
          "ha" 'helm-do-ag
          "hA" 'helm-ag-project-root
          "hi" 'helm-imenu
          "hI" 'helm-imenu-anywhere
          )
        ;; Elisp Editing Bindings
        (evil-leader/set-key-for-mode 'emacs-lisp-mode
          "m e b" 'eval-buffer
          "m e r" 'eval-region
          "m e c" 'eval-sexp-fu-eval-sexp-inner-list
          "m e e" 'eval-sexp-fu-eval-sexp-inner-sexp)
        (define-key evil-normal-state-map (kbd "q") nil)
        (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
        (define-key evil-normal-state-map (kbd "q") 'popwin:close-popup-window )
        ;;(evil-leader/set-key "q" 'popwin:close-popup-window)
        ))
    (use-package evil-org
      :init (add-hook 'org-mode-hook 'evil-org-mode))
    (use-package evil-visualstar
      :ensure t
      :init
      (global-evil-visualstar-mode))
    (use-package evil-iedit-state
      :ensure t
      :config
      (setq evil-iedit-state-tag  "R+")
      (custom-set-faces
       '(iedit-occurrence ((t (:inherit lazy-highlight)))))
      (require 'evil-iedit-state)
      (define-key evil-motion-state-map (kbd ";") nil)
      (define-key evil-motion-state-map ";" 'evil-iedit-state/iedit-mode))
    (use-package evil-matchit
      :ensure t
      :init
      (global-evil-matchit-mode 1)
      :config
      (progn
        (define-key evil-normal-state-map (kbd "go") 'evilmi-jump-items)))
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
        ))
    (use-package evil-nerd-commenter
      :bind
      ("M-;" . evilnc-comment-or-uncomment-lines)
      :init
      (evil-leader/set-key
        "//" 'evilnc-comment-or-uncomment-lines
        "/l" 'evilnc-quick-comment-or-uncomment-to-the-line
        "/y" 'evilnc-copy-and-comment-lines
        "/p" 'evilnc-comment-or-uncomment-paragraphs
        "/r" 'comment-or-uncomment-region
        "/v" 'evilnc-toggle-invert-comment-line-by-line))
    (use-package evil-numbers
      :ensure t
      :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
      :init
      (progn
        (defhydra init/hydra-numbers ()
          "numbers"
          ("i" evil-numbers/inc-at-pt "increase")
          ("d" evil-numbers/dec-at-pt "decrease"))
        (evil-leader/set-key
          "ni" 'init/hydra-numbers/evil-numbers/inc-at-pt
          "nd" ' init/hydra-numbers/evil-numbers/dec-at-pt)))
    (use-package evil-snipe
      :ensure t
      :diminish evil-snipe-mode
      :diminish evil-snipe-local-mode
      :init
      (setq-default
       evil-snipe-smart-case t
       evil-snipe-repeat-keys nil ; using space to repeat
       ;; evil-snipe-scope 'line
       evil-snipe-scope 'whole-buffer
       evil-snipe-repeat-scope 'visible
       evil-snipe-override-evil-repeat-keys nil ; causes problems with remapped ;
       evil-snipe-symbol-groups '((?\[ "[[{(]")
                                  (?\] "[]})]")
                                  (?\; "[;:]")))
      :config
      (evil-snipe-mode 1)
      (evil-snipe-override-mode 1))
    (use-package evil-terminal-cursor-changer
      ;; Change the cursor face when switching evil states
      ;; Homepage: https://github.com/7696122/evil-terminal-cursor-changer
      :ensure t
      :config
      (progn
        (setq evil-normal-state-cursor '("white" box)
              evil-insert-state-cursor '("orange" bar)
              evil-replace-state-cursor '("orange" hbar)
              evil-visual-state-cursor '("yellow" box)
              evil-motion-state-cursor '("violet" box)
              evil-operator-state-cursor '("magenta" hollow)
              evil-iedit-state-cursor '("pink" box)
              evil-emacs-state-cursor '("red" bar))))
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

    ;; esc quits
    (defun minibuffer-keyboard-quit ()
      "Abort recursive edit.
         In Delete Selection mode, if the mark is active, just deactivate it;
         then it takes a second \\[keyboard-quit] to abort the minibuffer."
      (interactive)
      (if (and delete-selection-mode transient-mark-mode mark-active)
          (setq deactivate-mark  t)
        (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
        (abort-recursive-edit)))

    (defadvice keyboard-quit (before evil-insert-to-nornal-state activate)
      "C-g back to normal state"
      (when  (evil-insert-state-p)
        (cond
         ((equal (evil-initial-state major-mode) 'normal)
          (evil-normal-state))
         ((equal (evil-initial-state major-mode) 'insert)
          (evil-normal-state))
         ((equal (evil-initial-state major-mode) 'motion)
          (evil-motion-state))
         (t
          (if (equal last-command 'keyboard-quit)
              (evil-normal-state)
            (evil-change-to-initial-state))
          ))))

    (with-eval-after-load 'diff-mode
      (evil-add-hjkl-bindings diff-mode-map 'insert
        (kbd "SPC") evil-leader--default-map
        "t" 'toggle-diff-whitespace-eol
        ))

    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    ;; (define-key evil-normal-state-map ";" 'evil-ex)
    ;; (define-key evil-visual-state-map ";" 'evil-ex)
    (define-key evil-normal-state-map "\C-n" nil)
    (define-key evil-normal-state-map "\C-p" nil)
    (define-key evil-normal-state-map "\C-v" nil)
    (define-key evil-motion-state-map "\C-v" nil)
    (define-key evil-normal-state-map "\C-e" nil)
    ;; (define-key evil-motion-state-map (kbd "C-w") nil)
    ;; (define-key evil-motion-state-map (kbd "C-i") nil)
    ;; (define-key evil-motion-state-map (kbd "C-b") nil)
    ;; (define-key evil-motion-state-map (kbd "C-d") nil)
    ;; (define-key evil-motion-state-map (kbd "C-e") nil)
    ;; (define-key evil-motion-state-map (kbd "C-f") nil)
    ;; (define-key evil-motion-state-map (kbd "C-y") nil)

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
    (evil-set-initial-state 'eshell-mode 'emacs)
    (evil-set-initial-state 'shell-mode 'emacs)
    (evil-set-initial-state 'ibuffer-mode 'normal)
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-diff-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)
    ;; modes to map to different default states
    (dolist (mode-map '((compilation-mode       . normal)
                        (help-mode              . normal)
                        (message-mode           . normal)
                        (debugger-mode          . normal)
                        (profile-report-mode    . emacs)
                        (Info-mode              . emacs)
                        (view-mode              . emacs)
                        (comint-mode            . emacs)
                        (cider-repl-mode        . emacs)
                        (term-mode              . emacs)
                        (calendar-mode          . emacs)
                        (Man-mode               . emacs)
                        (grep-mode              . emacs)
                        (image-mode             . normal)
                        ))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    ))
(provide 'init-evil)
