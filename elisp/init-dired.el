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

  (defun my/dired-mode-hook ()
    (toggle-truncate-lines 1))

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

  (defun my/dired-diff ()
    "Ediff marked files in dired or selected files in separate window"
    (interactive)
    (let* ((marked-files (dired-get-marked-files nil nil))
           (other-win (get-window-with-predicate
                       (lambda (window)
                         (with-current-buffer (window-buffer window)
                           (and (not (eq window (selected-window)))
                                (eq major-mode 'dired-mode))))))
           (other-marked-files (and other-win
                                    (with-current-buffer (window-buffer other-win)
                                      (dired-get-marked-files nil)))))
      (cond ((= (length marked-files) 2)
             (ediff-files (nth 0 marked-files)
                          (nth 1 marked-files)))
            ((= (length marked-files) 3)
             (ediff-files3 (nth 0 marked-files)
                           (nth 1 marked-files)
                           (nth 2 marked-files)
                           ))
            ((and (= (length marked-files) 1)
                  (= (length other-marked-files) 1))
             (ediff-files (nth 0 marked-files)
                          (nth 0 other-marked-files)))
            ((= (length marked-files) 1)
             (dired-diff))
            (t (error "mark exactly 2 files, at least 1 locally")))))

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
  (evil-define-key 'normal dired-mode-map "r" 'revert-buffer)
  (evil-define-key 'normal dired-mode-map "gg" 'my/back-to-top)
  (evil-define-key 'normal dired-mode-map "G" 'my/jump-to-bottom)
  (evil-define-key 'normal dired-mode-map "." 'my/dotfiles-toggle)
  (evil-define-key 'normal dired-mode-map "=" 'my/dired-diff)
  (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
  (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
  (evil-define-key 'normal dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'my/dired-mode-hook)

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
    (evil-leader/set-key "dj" 'dired-jump)
    :config
    (diredp-toggle-find-file-reuse-dir 1))

  (use-package dired-narrow
    :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow)))

  ;;preview files in dired
  (use-package peep-dired
    :ensure t
    :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
    :bind (:map dired-mode-map
                ("P" . peep-dired)))
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
  (setq ranger-override-dired nil)
  (setq ranger-preview-file t)
  (setq ranger-max-preview-size 10)
  (setq ranger-dont-show-binary t)
  (setq ranger-cleanup-eagerly t)
  (setq ranger-cleanup-on-disable t)

  ;; hide irrelevant info
  (defun my/ranger-hooks ()
    "Hooks to run in ranger buffers."
    (interactive)
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
