(defun my/setup-semantic-mode ()
  (interactive)
  (use-package semantic)
  (require 'semantic/ia)
  (require 'semantic/wisent)
  (setq semantic-default-submodes
        '(global-semantic-idle-scheduler-mode
          global-semanticdb-minor-mode
          global-semantic-idle-summary-mode
          global-semantic-stickyfunc-mode))
  (semantic-mode t)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c?" 'semantic-analyze-proto-impl-toggle))

(add-hook 'c-mode-hook #'my/setup-semantic-mode)
(add-hook 'java-mode-hook #'my/setup-semantic-mode)

(defun my:setup-imenu-for-use-package ()
  "Recognize `use-package` in imenu"
  (add-to-list
   'imenu-generic-expression
   '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)))

(add-hook 'emacs-lisp-mode-hook #'my:setup-imenu-for-use-package)

(use-package imenu-anywhere
  :ensure t
  :config
  (progn
    (setq imenu-auto-rescan t)
    (defun my-merge-imenu ()
      (interactive)
      (let ((mode-imenu (imenu-default-create-index-function))
            (custom-imenu (imenu--generic-function imenu-generic-expression)))
        (append mode-imenu custom-imenu)))
    (add-hook 'python-mode-hook
              (lambda ()
                (add-to-list
                 'imenu-generic-expression
                 '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))
                (imenu-add-to-menubar "Position")
                (setq imenu-create-index-function 'my-merge-imenu)))))

(use-package imenu-list
  :ensure t
  :commands (modi/imenu-list-display-toggle)
  :init
  (defface imenu-list-entry-face-0
    '((((class color) (background light))
       :inherit imenu-list-entry-face
       :foreground "maroon")
      (((class color) (background dark))
       :inherit imenu-list-entry-face
       :foreground "#4f97d7"))
    "Face for outermost imenu-list entries (depth 0)."
    :group 'imenu-list)

  (defface imenu-list-entry-face-1
    '((((class color) (background light))
       :inherit imenu-list-entry-face
       :foreground "dark green")
      (((class color) (background dark))
       :inherit imenu-list-entry-face
       :foreground "#67b11d"))
    "Face for imenu-list entries with depth 1."
    :group 'imenu-list)
  :config
  (progn
    ;; (setq imenu-list-size     0.2)
    (setq imenu-list-position 'right)
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize t)

    (defun modi/imenu-list-hide ()
      (interactive)
      (switch-to-buffer-other-window imenu-list-buffer-name)
      (quit-window))

    (defun modi/imenu-list-visible-p ()
      "Returns `t' if the `imenu-list' buffer is visible."
      (catch 'break
        (dolist (win (window-list))
          (when (string= imenu-list-buffer-name (buffer-name (window-buffer win)))
            (throw 'break t)))))

    (defun modi/imenu-list-display-toggle (noselect)
      "Toggle the display of Imenu-list buffer.
       If NOSELECT is non-nil, do not select the imenu-list buffer."
      (interactive "P")
      (if (modi/imenu-list-visible-p)
          (modi/imenu-list-hide)
        (if noselect
            (imenu-list-noselect)
          (imenu-list))))

    (defun modi/imenu-list-goto-entry-and-hide ()
      "Execute `imenu-list-goto-entry' and hide the imenu-list buffer."
      (interactive)
      (imenu-list-goto-entry)
      (modi/imenu-list-hide))
    (bind-key "C-<return>"
              #'modi/imenu-list-goto-entry-and-hide
              imenu-list-major-mode-map)

    (bind-keys
     :map imenu-list-major-mode-map
     ("j" . next-line)
     ("k" . previous-line)
     ("g" . beginning-of-buffer)
     ("G" . end-of-buffer)
     ("O" . ace-window)
     ("L" . avy-goto-line)
     ("i" . hs-toggle-hiding))

    (defun my/imenu-list-goto-entry ()
      "Enable lispy mode for selected major modes only"
      (interactive)
      (imenu-list-goto-entry)
      (let ((buffer-major-mode
             (format "%s" (get-buffer-mode))))
        (if (equal "pdf-view-mode" buffer-major-mode)
            (xah-fly-insert-mode-activate)
          (xah-fly-command-mode-activate))))

    (bind-key "l"
              #'my/imenu-list-goto-entry imenu-list-major-mode-map)

    (defun modi/imenu-auto-update (orig-fun &rest args)
      "Auto update the *Ilist* buffer if visible."
      (prog1 ; Return value of the advising fn needs to be the same as ORIG-FUN
          (apply orig-fun args)
        (when (modi/imenu-list-visible-p)
          (progn
            (imenu-list-update-safe)
            (when (string-match "^\\*Ilist\\*" (buffer-name))
              (xah-fly-insert-mode-activate)))))) ; update `imenu-list' buffer

    (advice-add 'switch-to-buffer :around #'modi/imenu-auto-update)
    (advice-add 'xah-previous-user-buffer :around #'modi/imenu-auto-update)
    (advice-add 'xah-next-user-buffer :around #'modi/imenu-auto-update)
    (advice-add 'xah-close-current-buffer :around #'modi/imenu-auto-update)
    (advice-add 'modi/imenu-list-display-toggle :around #'modi/imenu-auto-update)
    (advice-add 'ace-window :around #'modi/imenu-auto-update)
    (advice-add 'ivy-done :around #'modi/imenu-auto-update)
    (advice-add 'revert-buffer    :around #'modi/imenu-auto-update)))

  (provide 'init-imenu-anywhere)
