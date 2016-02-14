(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-create-file-auto-open t
          neo-dont-be-alone t
          neo-banner-message "File Tree browser"
          neo-smart-open t
          neo-persist-show nil)
    (setq neo-theme 'nerd) ; 'classic, 'nerd, 'ascii, 'arrow
    (defun neo-buffer--insert-header ()
      (let ((start (point)))
        (set-text-properties start (point) '(face neo-header-face)))
      (neo-buffer--newline-and-begin))
    ;; http://emacs.stackexchange.com/a/12156/115
    (defun find-file-next-in-dir (&optional prev)
      "Open the next file in the directory.
       When PREV is non-nil, open the previous file in the directory."
      (interactive "P")
      (let ((neo-init-state (neo-global--window-exists-p)))
        (if (null neo-init-state)
            (neotree-show))
        (neo-global--select-window)
        (if (if prev
                (neotree-previous-line)
              (neotree-next-line))
            (progn
              (neo-buffer--execute nil
                                   (quote neo-open-file)
                                   (lambda (full-path &optional arg)
                                     (message "Reached dir: %s/" full-path)
                                     (if prev
                                         (neotree-next-line)
                                       (neotree-previous-line)))))
          (progn
            (if prev
                (message "You are already on the first file in the directory.")
              (message "You are already on the last file in the directory."))))
        (if (null neo-init-state)
            (neotree-hide))))

    (defun find-file-prev-in-dir ()
      "Open the next file in the directory."
      (interactive)
      (find-file-next-in-dir :prev))

    
    (after 'evil
      (evil-leader/set-key "nt" 'neotree-toggle)
      (evil-leader/set-key "ne" 'neotree-find)
      (evil-leader/set-key "ns" 'neotree-show)
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
        (kbd ".")   'neotree-hidden-file-toggle
        (kbd "a")   'neotree-stretch-toggle
        (kbd "|")   'neotree-enter-vertical-split
        (kbd "-")   'neotree-enter-horizontal-split
        (kbd "H")   'neotree-select-previous-sibling-node
        (kbd "J")   'neotree-select-next-sibling-node
        (kbd "K")   'neotree-select-up-node
        (kbd "o")   'find-file-next-in-dir
        (kbd "O")   'find-file-prev-in-dir
        (kbd "l")   'neotree-enter
        )
      )
    ))

(provide 'init-neotree)
