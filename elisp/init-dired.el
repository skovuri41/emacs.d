;;;; dired
;; directory-browsing commands
(use-package dired
  :init
  (setq dired-auto-revert-buffer t)
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown copy delete load move symlink))
  (setq dired-deletion-confirmer (lambda (x) t))
  ;; dired - reuse current buffer by pressing 'a'
  ;;(put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)
  :config
  (use-package dired-x
    :init
    (progn
      (defun my-load-dired-x ()
        "Load dired-x. For use on dired-load-hook"
        (load "dired-x"))
      (add-hook 'dired-load-hook 'my-load-dired-x)))

  (define-key dired-mode-map (kbd "`") 'dired-toggle-read-only)
  ;; make rename use ido and not helm
  (put 'dired-do-rename 'ido 'find-file)
  ;; make copy use ido and not helm
  (put 'dired-do-copy 'ido 'find-file)

  (defun my/dotfiles-toggle ()
    "Show/hide dot-files"
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (message "h")
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        (progn (revert-buffer) ; otherwise just revert to re-show
               (set (make-local-variable 'dired-dotfiles-show-p) t)))))

  (defun my/back-to-top ()
    "Move to first file"
    (interactive)
    (beginning-of-buffer)
    (dired-next-line 1))

  (defun my/jump-to-bottom ()
    "Move to last file"
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))

  (defun my/move-up ()
    "Move to previous file"
    (interactive)
    (dired-previous-line 1)
    (if (bobp)
        (dired-next-line 1)))

  (defun my/move-down ()
    "Move to next file"
    (interactive)
    (dired-next-line 1)
    (if (eobp)
        (dired-next-line -1)))

  (evil-define-key 'normal dired-mode-map "k" 'dired-previous-line)
  (evil-define-key 'normal dired-mode-map "j" 'dired-next-line)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-file)
  (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "C-j") 'dired-next-subdir)
  (evil-define-key 'normal dired-mode-map (kbd "C-k") 'dired-prev-subdir)
  (evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
  (evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
  (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
  (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
  (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
  (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
  (evil-define-key 'normal dired-mode-map "gg" 'my/back-to-top)
  (evil-define-key 'normal dired-mode-map "G" 'my/jump-to-bottom)
  (evil-define-key 'normal dired-mode-map "." 'my/dotfiles-toggle)

  (use-package wdired
    :init
    ;; allow changing of file permissions
    (setq wdired-allow-to-change-permissions t))

  (use-package dired+
    ;; dired+ adds some features to standard dired (like reusing buffers)
    :bind ("C-x C-j" . dired-jump)
    :defer 1
    :init
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag nil)
    (evil-leader/set-key "jd" 'dired-jump)
    :config
    (diredp-toggle-find-file-reuse-dir 1))

  (use-package dired-narrow
    :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow)))
  )

(use-package ranger
  :ensure t
  :init
  (setq ranger-cleanup-on-disable t
        ranger-cleanup-eagerly t
        ranger-show-dotfiles t
        ranger-parent-depth 1
        ranger-show-literal nil
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-hide-cursor nil)
  ;; section width ratios
  (setq ranger-width-parents 0.25)
  (setq ranger-max-parent-width 0.25)
  (setq ranger-width-preview 0.50)
  (setq ranger-override-dired t)
  (setq ranger-preview-file t)
  (setq ranger-max-preview-size 10)
  (setq ranger-dont-show-binary t)
  (setq ranger-cleanup-eagerly t)
  (setq ranger-cleanup-on-disable t)

  ;; hide irrelevant info
  (defun my/ranger-hooks ()
    "Hooks to run in ranger buffers."
    (interactive)
    ;; (setq mode-line-format nil)
    (linum-mode -1)
    )
  (add-hook 'ranger-mode-hook 'my/ranger-hooks)
  (add-hook 'ranger-parent-dir-hook 'my/ranger-hooks)

  (setq ranger-preview-header-func (lambda () (interactive)
                                     (last (s-split "/" (buffer-file-name) t))))
  :bind ("<f10>" . ranger)
  :bind (:map ranger-mode-map
              ("/" . dired-narrow)))

(provide 'init-dired)
