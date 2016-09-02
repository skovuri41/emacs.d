;;;; dired
;; directory-browsing commands
(use-package dired
  :init
  (setq dired-auto-revert-buffer t)
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown copy delete load move symlink))
  (setq dired-deletion-confirmer (lambda (x) t))
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  ;; (setq dired-listing-switches "-ABhl --si --group-directories-first")
  (setq dired-listing-switches "-Al --si --time-style long-iso")
  (setq directory-free-space-args "-Pmh")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-omit-files "\\(?:.*\\.\\(?:aux\\|log\\|synctex\\.gz\\|run\\.xml\\|bcf\\|am\\|in\\)\\'\\)\\|^\\.\\|-blx\\.bib")
  (setq dired-garbage-files-regexp
        "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'")
  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; (setq dired-guess-shell-alist-user
  ;;       '(("\\.pdf\\'" "evince" "okular")
  ;;         ("\\.\\(?:djvu\\|eps\\)\\'" "evince")
  ;;         ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\|bmp\\)\\'" "eog")
  ;;         ("\\.\\(?:xcf\\)\\'" "gimp")
  ;;         ("\\.csv\\'" "libreoffice")
  ;;         ("\\.tex\\'" "pdflatex" "latex")
  ;;         ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\|ifo\\|m4v\\|wmv\\)\\(?:\\.part\\)?\\'"
  ;;          "vlc")
  ;;         ("\\.\\(?:mp3\\|flac\\|wv\\)\\'" "rhythmbox")
  ;;         ("\\.html?\\'" "firefox")
  ;;         ("\\.cue?\\'" "audacious")
  ;;         ("\\.pptx\\'" "libreoffice")))

  :config
  (use-package dired-x
    :init
    (progn
      (defun my-load-dired-x ()
        "Load dired-x. For use on dired-load-hook"
        (load "dired-x"))
      (add-hook 'dired-load-hook 'my-load-dired-x)))

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

  (defun my/dired-find-file (&optional arg)
    "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
    (interactive "P")
    (let* ((fn-list (dired-get-marked-files nil arg)))
      (mapc 'find-file fn-list)))

  (defun ora-dired-other-window ()
    (interactive)
    (save-selected-window
      (dired-find-file-other-window)))

  (defun ora-dired-up-directory ()
    (interactive)
    (let ((buffer (current-buffer)))
      (dired-up-directory)
      (unless (equal buffer (current-buffer))
        (kill-buffer buffer))))

  (defun ora-dired-get-size ()
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message
         "Size of all marked files: %s"
         (progn
           (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
           (match-string 1))))))

  ;;* advice
  (defadvice dired-advertised-find-file (around ora-dired-subst-directory activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let* ((orig (current-buffer))
           (filename (dired-get-filename t t))
           (bye-p (file-directory-p filename)))
      ad-do-it
      (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
        (kill-buffer orig))))

  (defadvice dired-delete-entry (before ora-force-clean-up-buffers (file) activate)
    (let ((buffer (get-file-buffer file)))
      (when buffer
        (kill-buffer buffer))))


  (require 'hydra)
  (defhydra hydra-marked-items (dired-mode-map "")
    "
   Number of marked items: %(length (dired-get-marked-files))
    "
    ("m" dired-mark "mark"))

  ;; (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'my/dired-mode-hook)

  (defun xah-dired-sort ()
    "Sort dired dir listing in different ways.
     Prompt for a choice.
     URL `http://ergoemacs.org/emacs/dired_sort.html'
     Version 2015-07-30"
    (interactive)
    (let (-sort-by -arg)
      (setq -sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" "dir")))
      (cond
       ((equal -sort-by "name") (setq -arg "-Al --si --time-style long-iso "))
       ((equal -sort-by "date") (setq -arg "-Al --si --time-style long-iso -t"))
       ((equal -sort-by "size") (setq -arg "-Al --si --time-style long-iso -S"))
       ((equal -sort-by "dir") (setq -arg "-Al --si --time-style long-iso --group-directories-first"))
       (t (error "logic error 09535" )))
      (dired-sort-other -arg )))

  (defun my/dired-toggle-hide-details ()
    "Toggle details and hidden files in dired."
    (interactive)
    (when (not (string-equal major-mode "dired-mode"))
      (error "Major mode is not dired."))
    (let* ((b (or dired-omit-mode
                  dired-hide-details-mode))
           (v (if b -1 1)))
      (progn
        (dired-omit-mode v)
        (dired-hide-details-mode v))))

  ;;* bind and hook
  (define-key dired-mode-map (kbd "e") 'my/dired-diff)
  (define-key dired-mode-map (kbd "E") 'dired-toggle-read-only)
  (define-key dired-mode-map (kbd "C-t") nil)
  (define-key dired-mode-map (kbd "h") 'ora-dired-up-directory)
  (define-key dired-mode-map (kbd "i") 'counsel-find-file)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "l") 'diredp-find-file-reuse-dir-buffer)
  (define-key dired-mode-map (kbd "%^") 'dired-flag-garbage-files)
  (define-key dired-mode-map (kbd "z") 'ora-dired-get-size)
  (define-key dired-mode-map (kbd "F") 'find-name-dired)
  (define-key dired-mode-map (kbd "f") 'dired-goto-file)
  (define-key dired-mode-map (kbd "'") 'eshell-this-dir)
  (define-key dired-mode-map (kbd "u") 'dired-undo)
  (define-key dired-mode-map (kbd "U") 'dired-unmark-all-marks)
  (define-key dired-mode-map (kbd "!") 'sudired)
  (define-key dired-mode-map (kbd "O") 'ora-dired-other-window)
  (define-key dired-mode-map (kbd "`") 'dired-toggle-read-only)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd ".") 'my/dired-toggle-hide-details)

  (unbind-key "SPC" dired-mode-map)

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
    :config
    (diredp-toggle-find-file-reuse-dir 1))

  (use-package dired-quick-sort
    :ensure t
    :bind (:map dired-mode-map
                ("s" . hydra-dired-quick-sort/body)))

  (use-package dired-narrow
    :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow-fuzzy)))

  (defconst media-files-extensions
    '("mp3" "mp4" "avi" "mpg" "flv" "ogg" "mkv" "mpeg" "wmv" "wav" "mov" "flv" "ogm")
    "Media files.")
  (defconst time-style-space (string ?\u2008) "Punctuation space Unicode char.")

  (use-package dired-rainbow
    :disabled t
    :ensure t
    :init
    (setq dired-rainbow-date-regexp
	  (concat "\\(?:[0-3][0-9]/[0-1][0-9]/[0-9][0-9]"
		  time-style-space "[0-2][0-9]:[0-5][0-9]\\)"))
    ;; highlight executable files, but not directories
    (dired-rainbow-define-chmod executable-unix "#2aa198" "-.*x.*")
    (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
    (dired-rainbow-define media "#ce5c00" media-files-extensions))

  ;;preview files in dired
  (use-package peep-dired
    :ensure t
    :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
    :bind (:map dired-mode-map
                ("P" . peep-dired))))

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
  ;; :bind ("<f10>" . ranger)
  :bind (:map ranger-mode-map
              ("/" . dired-narrow)))

(provide 'init-dired)
