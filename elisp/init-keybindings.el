(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key '[(f1)] 'call-last-kbd-macro)
(global-set-key '[(shift f1)]  'toggle-kbd-macro-recording-on)
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)
(global-set-key (kbd "M-`") 'helm-all-mark-rings)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key (kbd "<f7>") 'repeat-complex-command)
(global-set-key "\C-ca" 'org-agenda)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(global-set-key (kbd "C-c I") 'find-user-init-file)
(global-set-key (kbd "C-c E")  'erase-buffer)
(global-set-key (kbd "C-x r N") 'number-rectangle)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "M-k") 'my/kill-sentence-dwim)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "M-TAB") 'yas-expand)
(define-key outline-minor-mode-map (kbd "TAB") 'org-cycle)
(define-key outline-mode-map "\t" 'org-cycle)
(global-set-key (kbd "C-.") 'company-try-hard)
;; (global-set-key (kbd "M-g") 'company-try-hard)
(bind-key "C-x p" 'my-pop-to-mark-command)

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
(define-key occur-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
(define-key occur-mode-map (kbd "C-c C-c") 'wgrep-finish-edit)


(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
(define-key Buffer-menu-mode-map "k" 'previous-line)
(define-key Buffer-menu-mode-map "j" 'next-line)
(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
(define-key ibuffer-mode-map "k" 'ibuffer-backward-line)
(define-key ibuffer-mode-map "j" 'ibuffer-forward-line)
(define-key ibuffer-mode-map "l" (lambda () (interactive) (progn
                                                       (ibuffer-visit-buffer)
                                                       (xah-fly-command-mode-activate))))
(define-key bookmark-bmenu-mode-map "k" 'previous-line)
(define-key bookmark-bmenu-mode-map "j" 'next-line)
(define-key bookmark-bmenu-mode-map "s" 'bookmark-bmenu-save)

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

(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)

(defalias 'rb 'revert-buffer)

(defalias 'sh 'shell)
(defalias 'sbc 'set-background-color)
(defalias 'rof 'recentf-open-files)
(defalias 'lcd 'list-colors-display)
(defalias 'cc 'calc)

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

(defalias 'dv 'describe-variable)
(defalias 'df 'describe-function)
(defalias 'dk 'describe-key)
(defalias 'db 'counsel-descbinds)
(defalias 'fnd 'find-name-dired)
(defalias 'ne 'next-error)
(defalias 'pe 'previous-error)
(defalias 'pd 'projectile-dired)
(defalias 'pff 'projectile-find-file)
(defalias 'pfd 'projectile-find-dir)
(defalias 'psp 'projectile-switch-project)



(provide 'init-keybindings)
