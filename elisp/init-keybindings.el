(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;;(global-set-key (kbd "M-%") 'query-replace-regexp)
;;(global-set-key (kbd "C-M-%") 'query-replace)
;;switch prev nex user buffers

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

(provide 'init-keybindings)
