(use-package evil
  :init
  (progn
    (setq evil-move-beyond-eol nil)
    (use-package evil-leader
      :init
      (global-evil-leader-mode)
      (setq evil-leader/in-all-states 1)
      :config
      (progn
        (evil-leader/set-leader "SPC")
        (setq evil-leader/non-normal-prefix "S-")
        (evil-leader/set-key
          "wd" 'delete-window
          "wo" 'delete-other-windows
          "ws" 'split-window-below
          "wh" 'split-window-horizontally
          "wv" 'split-window-vertically
          "wu" 'winner-undo
          "wU" 'winner-redo
          "bS" 'save-some-buffers
          "bb" 'ivy-switch-buffer
          "bs" 'save-buffer
          "bk" 'ido-kill-buffer
          "bp" 'psw-switch-projectile-files
          "br" 'psw-switch-recentf
          "ff" 'counsel-find-file
          "ww" 'ace-window
          "hf" 'counsel-find-file
          "ff" 'counsel-find-file
          "fw" 'toggle-full-window
          "bn" 'xah-new-empty-buffer
          "by" 'bury-buffer
          "hl" 'helm-locate
          "hy" 'helm-show-kill-ring
          "ht" 'helm-top
          "ho" 'helm-occur
          "hx" 'helm-M-x
          "he" 'helm-find-files
          "hb" 'helm-buffers-list
          "hh" 'helm-projectile-find-file
          "hr" 'helm-recentf
          "fr" 'ivy-recentf
          "hp" 'helm-projectile
          "fp" 'helm-projectile
          "h'" 'helm-all-mark-rings
          "hs" 'helm-swoop
          "ha" 'helm-do-ag
          "hA" 'helm-ag-project-root
          "hi" 'helm-imenu
          "hI" 'helm-imenu-anywhere
          "el" 'flycheck-list-errors
          )
        ;; Elisp Editing Bindings
        (evil-leader/set-key-for-mode 'emacs-lisp-mode
          "m e b" 'eval-buffer
          "m e r" 'eval-region
          "m e c" 'eval-sexp-fu-eval-sexp-inner-list
          "m e e" 'eval-sexp-fu-eval-sexp-inner-sexp)
        (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
        (define-key evil-normal-state-map (kbd "m") 'push-mark-no-activate)
        (define-key evil-normal-state-map (kbd "gm") 'helm-all-mark-rings)
        ;; (define-key evil-normal-state-map (kbd "`") 'helm-all-mark-rings)
        (define-key evil-normal-state-map (kbd "q") 'popwin:close-popup-window )

        (defun my/evil-org ()
          (progn
            (evil-leader/set-key-for-mode 'org-mode
              "mt"  'org-show-todo-tree
              "ma"  'org-agenda
              "mc"  'org-archive-subtree
              "mo"  org-mode-map
              )
            (defun clever-insert-item ()
              "Clever insertion of org item."
              (if (not (org-in-item-p))
                  (insert "\n")
                (org-insert-item))
              )
            (defun evil-org-eol-call (fun)
              "Go to end of line and call provided function.
               FUN function callback"
              (end-of-line)
              (funcall fun)
              (evil-append nil)
              )
            ;; normal state shortcuts
            (evil-define-key 'normal org-mode-map
              "gh" 'outline-up-heading
              "gp" 'outline-previous-heading
              "gj" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
                       'org-forward-same-level
                     'org-forward-heading-same-level)
              "gk" (if (fboundp 'org-backward-same-level)
                       'org-backward-same-level
                     'org-backward-heading-same-level)
              "gl" 'outline-next-visible-heading
              "t" 'org-todo
              "T" '(lambda () (interactive) (evil-org-eol-call (lambda() (org-insert-todo-heading nil))))
              "H" 'org-shiftleft
              "J" 'org-shiftdown
              "K" 'org-shiftup
              "L" 'org-shiftright
              "o" '(lambda () (interactive) (evil-org-eol-call 'clever-insert-item))
              "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
              "$" 'org-end-of-line
              "^" 'org-beginning-of-line
              "<" 'org-metaleft
              ">" 'org-metaright
              "-" 'org-cycle-list-bullet
              (kbd "<tab>") 'org-cycle)
            ;;normal & insert state shortcuts.
            (mapc (lambda (state)
                    (evil-define-key state org-mode-map
                      (kbd "M-l") 'org-metaright
                      (kbd "M-h") 'org-metaleft
                      (kbd "M-k") 'org-metaup
                      (kbd "M-j") 'org-metadown
                      (kbd "M-L") 'org-shiftmetaright
                      (kbd "M-H") 'org-shiftmetaleft
                      (kbd "M-K") 'org-shiftmetaup
                      (kbd "M-J") 'org-shiftmetadown
                      (kbd "M-o") '(lambda () (interactive)
                                     (evil-org-eol-call
                                      '(lambda()
                                         (org-insert-heading)
                                         (org-metaright))))
                      (kbd "M-t") '(lambda () (interactive)
                                     (evil-org-eol-call
                                      '(lambda()
                                         (org-insert-todo-heading nil)
                                         (org-metaright))))
                      ))
                  '(normal insert))

            (defun org-goto-refile-target ()
              (interactive)
              (find-file org-default-notes-file))

            ))
        (add-hook 'org-mode-hook #'my/evil-org)
        (evil-leader/set-key
          "oa" 'org-agenda
          "og" 'helm-org-agenda-files-headings
          "oo" 'org-clock-out
          "oi" 'org-clock-in
          "oc" 'org-capture
          "oC" 'helm-org-capture-templates
          "or" 'org-goto-refile-target
          "op" 'org-pomodoro
          "oL" 'org-insert-link
          "ol" 'org-store-link)
        ))
    (use-package evil-visualstar
      :ensure t
      :init
      (setq evil-visualstar/persistent t)
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
        (setq evil-normal-state-cursor '("white" bar )
              evil-insert-state-cursor '("orange" bar)
              evil-replace-state-cursor '("orange" hbar)
              evil-visual-state-cursor '("yellow" box)
              evil-motion-state-cursor '("violet" box)
              evil-operator-state-cursor '("magenta" hollow)
              evil-iedit-state-cursor '("pink" box)
              evil-emacs-state-cursor '("red" bar))))
    )
  (evil-mode 1)
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


    (evil-set-initial-state 'nav-mode 'emacs)
    (evil-set-initial-state 'grep-mode 'emacs)
    (evil-set-initial-state 'eshell-mode 'emacs)
    (evil-set-initial-state 'shell-mode 'emacs)
    (evil-set-initial-state 'ibuffer-mode 'normal)
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-diff-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)
    (evil-set-initial-state 'git-gutter+-commit-mode 'insert)
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
                        (cfw:calendar-mode      . emacs)
                        (cfw:details-mode       . emacs)
                        (image-mode             . normal)
                        ))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))
    (add-hook 'git-commit-mode-hook 'evil-insert-state)

    (eval-after-load 'calendar
      '(progn
         (evil-set-initial-state 'calendar-mode 'emacs)
         (add-hook 'calendar-mode-hook
                   (lambda ()
                     (define-key calendar-mode-map "l" 'calendar-forward-day)
                     (define-key calendar-mode-map "h" 'calendar-backward-day)
                     (define-key calendar-mode-map "j" 'calendar-forward-week)
                     (define-key calendar-mode-map "k" 'calendar-backward-week)
                     (define-key calendar-mode-map "{" 'calendar-forward-month)
                     (define-key calendar-mode-map "}" 'calendar-backward-month)
                     (define-key calendar-mode-map "0" 'calendar-beginning-of-week)
                     (define-key calendar-mode-map "$" 'calendar-end-of-week)
                     (define-key calendar-mode-map "[" 'calendar-beginning-of-month)
                     (define-key calendar-mode-map "]" 'calendar-end-of-month)
                     (define-key calendar-mode-map (kbd "s") 'avy-goto-char)
                     (define-key calendar-mode-map "gg" 'calendar-beginning-of-year)
                     (define-key calendar-mode-map "G" 'calendar-end-of-year)))))

    ))

(provide 'init-evil)
