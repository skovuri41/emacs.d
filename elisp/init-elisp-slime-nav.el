(use-package elisp-slime-nav
  :commands (elisp-slime-nav-mode)
  :ensure elisp-slime-nav
  :demand elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :config
  (progn
    (after 'evil
           (evil-define-key 'normal emacs-lisp-mode-map (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point)
           (defun my-lisp-hook ()
             (elisp-slime-nav-mode)
             (eldoc-mode))
           (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
           (evil-leader/set-key-for-mode 'emacs-lisp-mode "." 'elisp-slime-nav-find-elisp-thing-at-point)
           (defun my-jump-to-elisp-docs (sym-name)
             "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point"
             (interactive (list (elisp-slime-nav--read-symbol-at-point)))
             (help-xref-interned (intern sym-name))
             (switch-to-buffer-other-window "*Help*" t))
           (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
             'my-jump-to-elisp-docs)
           )))

(provide 'init-elisp-slime-nav)
