(use-package elisp-slime-nav
  :commands (elisp-slime-nav-mode)
  :ensure elisp-slime-nav
  :demand elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :config
  (progn
    (defun my-lisp-hook ()
      (elisp-slime-nav-mode)
      (eldoc-mode))
    (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
    (defun my-jump-to-elisp-docs (sym-name)
      "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point"
      (interactive (list (elisp-slime-nav--read-symbol-at-point)))
      (help-xref-interned (intern sym-name))
      ;; (switch-to-buffer-other-window "*Help*" t)
      )
    ))

(provide 'init-elisp-slime-nav)
