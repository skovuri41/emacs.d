(use-package eyebrowse
  :ensure t
  :diminish eyebrowse-mode
  :commands (eyebrowse-create-window-config
             eyebrowse-last-window-config
             eyebrowse-next-window-config
             eyebrowse-prev-window-config
             eyebrowse-rename-window-config
             eyebrowse-switch-to-window-config)
  ;; :init
  :config
  (progn

    (defun eos/create-eyebrowse-setup ()
      (interactive)
      "Create a default window config, if none is present"
      (when (not (eyebrowse--window-config-present-p 3))
        ;; there's probably a better way to do this, creating three workspaces
        (eyebrowse-switch-to-window-config-2)
        (eyebrowse-switch-to-window-config-3)
        (eyebrowse-switch-to-window-config-1)
        (message "eyebrowse-ssetup")))

    ;; (add-hook 'after-init-hook #'eos/create-eyebrowse-setup)

    (defun my-move-buffer-to-next-free-slot (&optional buffer)
      (interactive "b")
      (call-interactively 'eyebrowse-create-window-config)
      (switch-to-buffer buffer))

    (defun my-projectile-eyebrowse (p)
      (interactive)
      (eyebrowse-create-window-config)
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) (projectile-default-project-name p))
      (projectile-switch-project-by-name p))

    (defun my-eyebrowse-rename-1 ()
      (interactive)
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) (buffer-name)))

    (defun my-eyebrowse-rename-2 ()
      (interactive)
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) (projectile-project-name)))

    (defun my-eyebrowse-current-tag ()
      (interactive)
      (let ((current-slot (eyebrowse--get 'current-slot))
            (window-configs (eyebrowse--get 'window-configs)))
        (nth 2 (assoc current-slot window-configs))))

    (defun ivy-switch-project-with-eyebrowse ()
      (interactive)
      (ivy-read
       "Switch to project: "
       (if (projectile-project-p)
           (cons (abbreviate-file-name (projectile-project-root))
                 (projectile-relevant-known-projects))
         projectile-known-projects)
       :action  #'my-projectile-eyebrowse))

    (defun my-close-all-other-slots ()
      (interactive)
      (let ((all-slots (mapcar 'car (eyebrowse--get 'window-configs)))
            (current-slot (eyebrowse--get 'current-slot)))
        (dolist (slot all-slots)
          (unless (= slot current-slot)
            (eyebrowse--delete-window-config slot)))))

    (defun my-close-slots-to-the-right ()
      (interactive)
      (let ((all-slots (mapcar 'car (eyebrowse--get 'window-configs)))
            (current-slot (eyebrowse--get 'current-slot)))
        (dolist (slot all-slots)
          (unless (<= slot current-slot)
            (eyebrowse--delete-window-config slot)))))

    (bind-keys :prefix-map my-eyebrowse-prefix-map
               :prefix "C-c w"
               ("c" . eyebrowse-create-window-config)
               ("ko" . my-close-all-other-slots)
               ("kr" . my-close-slots-to-the-right)
               ("n" . eyebrowse-next-window-config)
               ("p" . eyebrowse-prev-window-config)
               ("l" . eyebrowse-last-window-config)
               ("kk" . eyebrowse-close-window-config)
               ("w" . eyebrowse-switch-to-window-config)
               ("r" . eyebrowse-rename-window-config)

               ("0" . eyebrowse-switch-to-window-config-0)
               ("1" . eyebrowse-switch-to-window-config-1)
               ("2" . eyebrowse-switch-to-window-config-2)
               ("3" . eyebrowse-switch-to-window-config-3)
               ("4" . eyebrowse-switch-to-window-config-4)
               ("5" . eyebrowse-switch-to-window-config-5)
               ("6" . eyebrowse-switch-to-window-config-6)
               ("7" . eyebrowse-switch-to-window-config-7)
               ("8" . eyebrowse-switch-to-window-config-8)
               ("9" . eyebrowse-switch-to-window-config-9))

    (setq eyebrowse-mode-line-style 'smart)
    (setq eyebrowse-close-window-config-prompt t)
    ;; (setq eyebrowse-new-workspace '(lambda () (progn (jethro/insert-startupify-lists)
    ;;                                             (switch-to-buffer "*startscreen*"))))
    (setq eyebrowse-keymap-prefix (kbd "C-c w"))
    (setq eyebrowse-wrap-around t)

    (eyebrowse-mode t)))

(use-package nameframe
  :ensure t
  :config
  (progn
    (use-package nameframe-projectile
      :ensure t)
    (use-package nameframe-eyebrowse
      :load-path "elisp/"
      ;; :load-path ".cask/25.1/elpa/nameframe-eyebrowse"
      )
    (nameframe-projectile-mode t)
    (nameframe-eyebrowse-mode t)))

(provide 'init-perspective)
