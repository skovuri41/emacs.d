(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
(define-key isearch-mode-map [(control return)]
  #'isearch-exit-other-end)
;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

(global-set-key '[(f1)] 'call-last-kbd-macro)
(global-set-key '[(shift f1)]  'toggle-kbd-macro-recording-on)
(global-set-key [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "<f7>") 'repeat-complex-command)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c E")  'erase-buffer)
(global-set-key (kbd "C-c m") 'counsel-osx-app)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c g") 'counsel-git-grep)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(global-set-key (kbd "C-c I") ' modi/imenu-list-display-toggle)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x r N") 'number-rectangle)
(global-set-key (kbd "C-c r") 'ora-occur)
(global-set-key (kbd "M-k") 'my/kill-sentence-dwim)
(global-set-key (kbd "M-`") 'helm-all-mark-rings)
(global-set-key (kbd "M-i") 'iedit-mode)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-g d") 'dumb-jump-go)
(global-set-key (kbd "M-g b") 'dumb-jump-back)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "C-<return>") 'company-complete-common-or-cycle)
(global-set-key (kbd "C-.") 'company-try-hard)
(define-key outline-minor-mode-map (kbd "TAB") 'org-cycle)
(define-key outline-mode-map "\t" 'org-cycle)
(bind-key "C-x p" 'my-pop-to-mark-command)
(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)
(global-set-key (kbd "C-x o") 'ace-window)
;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
(global-set-key [(meta return)] 'toggle-frame-fullscreen)

;; (global-set-key (kbd "C-;") 'iedit-mode)

;; Move more quickly
(global-set-key (kbd "C-S-j")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-k")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(define-key occur-mode-map "k" 'previous-line)
(define-key occur-mode-map "j" 'next-line)
(define-key occur-mode-map "l" 'occur-mode-goto-occurrence)
(define-key occur-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
(define-key occur-mode-map (kbd "C-c C-c") 'wgrep-finish-edit)

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
(define-key Buffer-menu-mode-map "k" 'previous-line)
(define-key Buffer-menu-mode-map "j" 'next-line)

(defun my-bookmark-bmenu-mode-hook-fun ()
  (define-key bookmark-bmenu-mode-map "k" 'previous-line)
  (define-key bookmark-bmenu-mode-map "j" 'next-line)
  (define-key bookmark-bmenu-mode-map "s" 'bookmark-bmenu-save))
(add-hook 'bookmark-bmenu-mode-hook 'my-bookmark-bmenu-mode-hook-fun)

(require 'info)
;; (setq Info-additional-directory-list (list (expand-file-name "etc/info/" emacs-d)))

(define-key Info-mode-map "j" 'ora-para-down)
(define-key Info-mode-map "k" 'ora-para-up)
(define-key Info-mode-map "v" 'recenter-top-bottom)
(define-key Info-mode-map "h" 'backward-char)
(define-key Info-mode-map "l" 'forward-char)
(define-key Info-mode-map "w" 'forward-word)
(define-key Info-mode-map "b" 'backward-word)
(define-key Info-mode-map "a" 'beginning-of-line)
(define-key Info-mode-map "e" 'end-of-line)
(define-key Info-mode-map "A" 'beginning-of-buffer)
(define-key Info-mode-map "E" 'end-of-buffer)
(define-key Info-mode-map "t" 'hydra-info-to/body)
(define-key Info-mode-map "u" 'Info-history-back)
(define-key Info-mode-map "c" 'counsel-ace-link)

(defun ora-open-info (topic bname)
  "Open info on TOPIC in BNAME."
  (if (get-buffer bname)
      (progn
        (switch-to-buffer bname)
        (unless (string-match topic Info-current-file)
          (Info-goto-node (format "(%s)" topic))))
    (info topic bname)))

(defun ora-para-down (arg)
  (interactive "p")
  (if (bolp)
      (progn
        (forward-paragraph arg)
        (forward-line 1))
    (line-move arg)))

(defun ora-para-up (arg)
  (interactive "p")
  (if (bolp)
      (progn
        (forward-line -1)
        (backward-paragraph arg)
        (forward-line 1))
    (line-move (- arg))))

(defhydra hydra-info-to (:hint nil :color teal)
  "
_o_rg e_l_isp _e_macs _h_yperspec"
  ("o" (ora-open-info "org" "*org info*"))
  ("l" (ora-open-info "elisp" "*elisp info*"))
  ("e" (ora-open-info "emacs" "*emacs info*"))
  ("h" (ora-open-info "gcl" "*hyperspec*")))

;; swith meta and mac command key for mac port emacs build
(setq mac-option-modifier 'meta)
;; mac switch meta key
(defun mac-switch-meta ()
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
        (message "switching meta and command")
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'hyper))
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta))))

;; (setq mac-option-key-is-meta nil)
;; (setq mac-command-key-is-meta t)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'alt)

;;;;; aliases

;; make frequently used commands short
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'sl 'sort-lines)
(defalias 'rr 'reverse-region)
(defalias 'rs 'replace-string)
(defalias 'dup 'duplicate-thing)
;; (defalias 'max 'maximize-frame)

(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)
(defalias 'wd 'wdired-change-to-wdired-mode)

(defalias 'rb 'revert-buffer)

(defalias 'sh 'shell)
(defalias 'sbc 'set-background-color)
(defalias 'rof 'recentf-open-files)
(defalias 'lcd 'list-colors-display)
(defalias 'cc 'calc)
(defalias 'cal 'calendar)
(defalias 'wtime 'display-time-world)

(defalias 'fold 'hydra-folding/body)
(defalias 'of 'other-frame)

;; Macro
(defalias 'sm 'start-kbd-macro)
(defalias 'em 'end-kbd-macro)
(defalias 'lm 'call-last-kbd-macro)

;; ; elisp
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'eis 'elisp-index-search)
(defalias 'lf 'load-file)

;; ; major modes
(defalias 'hm 'html-mode)
(defalias 'tm 'text-mode)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'om 'org-mode)
(defalias 'ssm 'shell-script-mode)

;; ; minor modes
(defalias 'wsm 'whitespace-mode)
(defalias 'gwsm 'global-whitespace-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'glm 'global-linum-mode)
(defalias 'fm 'focus-mode)
(defalias 'xi 'xah-fly-insert-mode-activate)
(defalias 'xc 'xah-fly-command-mode-activate)

(defalias 'dv 'describe-variable)
(defalias 'df 'describe-function)
(defalias 'dk 'describe-key)
(defalias 'db 'describe-buffer)
(defalias 'dm 'describe-keymap)

(defalias 'db 'counsel-descbinds)
(defalias 'fnd 'find-name-dired)
(defalias 'ne 'next-error)
(defalias 'pe 'previous-error)
(defalias 'pd 'projectile-dired)
(defalias 'pff 'projectile-find-file)
(defalias 'pfd 'projectile-find-dir)
(defalias 'psp 'projectile-switch-project)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
