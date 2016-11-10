(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-create-file-auto-open t
          neo-dont-be-alone t
          neo-auto-indent-point t
          neo-banner-message "File Tree browser"
          neo-smart-open t
          neo-persist-show nil)
    (setq neo-theme 'nerd)           ; 'classic, 'nerd, 'ascii, 'arrow
    (validate-setq neo-vc-integration '(face char))
    (validate-setq neo-toggle-window-keep-p t)
    
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

    (defun neotree-project-dir ()
      "Open NeoTree using the git root."
      (interactive)
      (let ((project-dir (ffip-project-root))
            (file-name (buffer-file-name)))
        (if project-dir
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name))
          (message "Could not find git project root."))))
    
    ;; (setq projectile-switch-project-action 'neotree-projectile-action)

    (defun my/neotree-goto-entry ()
      "enter"
      (interactive)
      (progn
        ;; (neotree-enter)
        (neotree-enter-ace-window)
        (if (equal (buffer-name (current-buffer))
                   neo-buffer-name)
            (xah-fly-insert-mode-activate)
          (xah-fly-command-mode-activate))))
    
    (defun my/neotree-toggle ()
      "insert mode activate for mode"
      (interactive)
      (progn
        (neotree-toggle)
        (if (equal (buffer-name (current-buffer))
                   neo-buffer-name)
            (xah-fly-insert-mode-activate)
          (xah-fly-command-mode-activate))))
    
    (defun my/neotree-show ()
      "insert mode activate for mode"
      (interactive)
      (progn
        (neotree-show)
        (neo-global--select-window)
        (neo-point-auto-indent)
        (xah-fly-insert-mode-activate)))

    (defun my/neotree-hide ()
      "insert mode activate for mode"
      (interactive)
      (progn
        (neotree-hide)
        (xah-fly-command-mode-activate)))

    (defun my/neotree-switch-window ()
      "insert mode activate for mode"
      (interactive)
      (progn
        (call-interactively 'ace-window)
        (xah-fly-command-mode-activate)))
    
    (defun nt-mode-keys-setup ()
      "for 'neo tree mode'"
      (local-set-key (kbd "RET") 'neotree-enter)
      (local-set-key (kbd "i") 'neotree-enter)
      (local-set-key (kbd "l") 'my/neotree-goto-entry)
      (local-set-key (kbd "+") 'neotree-create-node)
      (local-set-key (kbd "r") 'neotree-rename-node)
      (local-set-key (kbd "d") 'neotree-delete-node)
      (local-set-key (kbd "j") 'next-line)
      (local-set-key (kbd "k") 'previous-line)
      (local-set-key (kbd "q") 'my/neotree-hide)
      (local-set-key (kbd ".") 'neotree-hidden-file-toggle)
      (local-set-key (kbd "TAB") 'neotree-stretch-toggle)
      (local-set-key (kbd "t") 'neotree-stretch-toggle)
      (local-set-key (kbd "|") 'neotree-enter-vertical-split)
      (local-set-key (kbd "-") 'neotree-enter-horizontal-split)
      (local-set-key (kbd "<") 'neotree-select-previous-sibling-node)
      (local-set-key (kbd ">") 'neotree-select-next-sibling-node)
      (local-set-key (kbd "h") 'neotree-select-up-node)
      (local-set-key (kbd "o") 'find-file-next-in-dir)
      (local-set-key (kbd "O") 'find-file-prev-in-dir)
      (local-set-key (kbd "ww") 'my/neotree-switch-window)
      (local-set-key (kbd "SPC") '(lambda () (interactive)
                                    (save-selected-window (neotree-enter)))))
    (add-hook 'neotree-mode-hook #'nt-mode-keys-setup)


    (defun neotree-project-root (&optional directory)
      "Open a NeoTree browser for a project DIRECTORY."
      (interactive)
      (let ((default-directory (or directory default-directory)))
        (if (and (fboundp 'neo-global--window-exists-p)
                 (neo-global--window-exists-p))
            (neotree-hide)
          (neotree-find (projectile-project-root)))))))

(provide 'init-neotree)
