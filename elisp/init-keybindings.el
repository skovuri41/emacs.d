(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;;(global-set-key (kbd "M-%") 'query-replace-regexp)
;;(global-set-key (kbd "C-M-%") 'query-replace)
(global-set-key '[(f1)] 'call-last-kbd-macro)
(global-set-key '[(shift f1)]  'toggle-kbd-macro-recording-on)
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)
(global-set-key (kbd "M-`") 'helm-all-mark-rings)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
;;(global-set-key (kbd "C-[ [ a a") 'push-mark-no-activate)
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
;; (global-set-key (kbd "C-f") 'golden-ratio-scroll-screen-up)
;; (global-set-key (kbd "C-b") 'golden-ratio-scroll-screen-down)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "M-k") 'my/kill-sentence-dwim)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "M-TAB") 'yas-expand)
(define-key outline-minor-mode-map (kbd "TAB") 'org-cycle)
(define-key outline-mode-map "\t" 'org-cycle)
(global-set-key (kbd "M-/") 'smart-tab)
;; (global-set-key (kbd "TAB") 'indent-or-complete)

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
