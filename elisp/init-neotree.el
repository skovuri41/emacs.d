(use-package neotree
  :ensure t
  :config
  (progn
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (setq neo-create-file-auto-open t
          neo-dont-be-alone t
          neo-banner-message "File Tree browser"
          neo-smart-open t
          neo-persist-show nil)
    (defun neo-buffer--insert-header ()
      (let ((start (point)))
        (set-text-properties start (point) '(face neo-header-face)))
      (neo-buffer--newline-and-begin))
    (after 'evil
      (evil-leader/set-key "nt" 'neotree-toggle)
      (evil-leader/set-key "ns" 'neotree-find)
      (evil-set-initial-state 'neotree-mode 'normal)
      (add-to-list 'evil-motion-state-modes 'neotree-mode)
      (evil-define-key 'normal neotree-mode-map
        (kbd "RET") 'neotree-enter
        (kbd "c")   'neotree-create-node
        (kbd "r")   'neotree-rename-node
        (kbd "d")   'neotree-delete-node
        (kbd "j")   'neotree-next-line
        (kbd "k")   'neotree-previous-line
        (kbd "SPC") 'neotree-change-root
        (kbd "q")   'neotree-hide
        (kbd "H")   'neotree-hidden-file-toggle
        (kbd "a")   'neotree-stretch-toggle
        (kbd "|")   'neotree-enter-vertical-split
        (kbd "-")   'neotree-enter-horizontal-split
        (kbd "H")   'neotree-select-previous-sibling-node
        (kbd "J")   'neotree-select-next-sibling-node
        (kbd "K")   'neotree-select-up-node
        (kbd "l")   'neotree-enter
        )
      )
    ))


(provide 'init-neotree)
