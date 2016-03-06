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
(global-set-key (kbd "<f11>") 'xah-previous-user-buffer)
(global-set-key (kbd "<f12>") 'xah-next-user-buffer)
(global-set-key (kbd "<S-f11>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<S-f12>") 'xah-next-emacs-buffer)
(global-set-key (kbd "<home>") 'xah-backward-left-bracket)
(global-set-key (kbd "<end>") 'xah-forward-right-bracket)
(global-set-key (kbd "<prior>") 'xah-backward-block) ; page up key
(global-set-key (kbd "<next>") 'xah-forward-block) ; page down key
(global-set-key (kbd "<f8>") 'xah-search-current-word)
;;(global-set-key (kbd "M-n") 'xah-new-empty-buffer) ; new empty buffer
(global-set-key (kbd "M-n") 'new-scratch-buffer) ; new empty buffer
(global-set-key (kbd "<f2>") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "<f4>") 'yank) ; paste
(global-set-key '[(f1)] 'call-last-kbd-macro)
(global-set-key '[(shift f1)]  'toggle-kbd-macro-recording-on)
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)
(global-set-key (kbd "M-`") 'helm-all-mark-rings)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "C-[ [ a a") 'push-mark-no-activate)
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key (kbd "<f7>") 'repeat-complex-command)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c I") 'find-user-init-file)
(global-set-key (kbd "C-c E")  'erase-buffer)


(provide 'init-keybindings)
