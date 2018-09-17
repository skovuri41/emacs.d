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
      ;; :ensure t
      :load-path "elisp/"
      )
    (nameframe-projectile-mode t)
    (nameframe-eyebrowse-mode t)))



(use-package shackle
  :ensure t
  :commands shackle-mode
  :init
  ;; one of below, above, left, right
  (setq shackle-default-alignment 'below)
  ;; Don't reuse windows by default.
  (setq shackle-select-reused-windows nil)
  ;; Define shackle rules!
  (setq shackle-rules
        ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
        '((compilation-mode :select nil)
          ("*undo-tree*" :size 0.25 :align right)
          ("*eshell*" :select t :other t)
          ("*Shell Command Output*" :select nil)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          (occur-mode :select nil :align t)
          ("*Help*" :select t :inhibit-window-quit nil :other t)
          ("*Completions*" :size 0.3 :align t)
          ("*Messages*" :select nil :inhibit-window-quit t :other t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :inhibit-window-quit t :other t)
          ("\\*poporg.*\\*" :regexp t :select t :other t)
          ("\\`\\*helm.*?\\*\\'" :regexp t :size 0.3 :align t)
          ("*Calendar*" :select t :size 0.3 :align below)
          ("*info*" :select t :inhibit-window-quit t :same t)
          (magit-status-mode :select t :inhibit-window-quit t :same t)
          (magit-log-mode :select t :inhibit-window-quit t :same t)))

  ;; A bit of trickery to have the final layout be adjusted by golden ratio.
  (defun adjust-ratio-golden-ratio (buffer alist plist)
    (progn
      (golden-ratio)))

  (add-function :after (symbol-function 'shackle-display-buffer) #'adjust-ratio-golden-ratio)

  (shackle-mode 1)

  ;; (setq shackle-default-rule '(:select t))
  )

(use-package perspeen
  ;; :ensure t
  :disabled t
  :init
  (setq perspeen-use-tab t
        perspeen-modestring-dividers '("[" "]" "/"))
  :config
  ;; (custom-set-faces
  ;;  '(perspeen-selected-face ((t (:foreground "#fcfcfc" :height 150))))
  ;;  '(perspeen-tab--header-line-active ((t (:background "#cfcfcf" :foreground "#262626" :height 150))))
  ;;  '(perspeen-tab--header-line-inactive ((t (:background "#404040" :foreground "#bfbfbf" :height 150))))
  ;;  '(perspeen-tab--powerline-inactive1 ((t (:inherit mode-line :height 150)))))
  (defun start-perspeen-mode ()
    "Enable perspeen-mode."
    (interactive)
    (perspeen-mode +1)
    (perspeen-rename-ws "default"))
  (add-hook 'after-init-hook #'start-perspeen-mode)
  (unbind-key "t" perspeen-command-map)
  (unbind-key "n" perspeen-command-map)
  (unbind-key "p" perspeen-command-map)
  (unbind-key "k" perspeen-command-map)
  (unbind-key "c" perspeen-command-map)
  (bind-keys :map perspeen-command-map
             ;; tab
             ("j" . perspeen-tab-next)
             ("k" . perspeen-tab-prev)
             ("c" . perspeen-tab-create-tab)
             ("x" . perspeen-tab-del)
             ;; workspace
             ("l" . perspeen-next-ws)
             ("h" . perspeen-previous-ws)
             ("w c" . perspeen-create-ws)
             ("w k" . perspeen-delete-ws)
             ("w r" . perspeen-rename-ws)
             ("w /" . perspeen-change-root-dir)))

(provide 'init-perspective)
