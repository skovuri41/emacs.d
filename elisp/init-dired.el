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
        (progn (revert-buffer)      ; otherwise just revert to re-show
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

  (defun ora-ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "no more than 2 files should be marked"))))

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

  (defun my-diredp-find-file-reuse-dir-buffer ()
    (interactive)
    (diredp-find-file-reuse-dir-buffer)
    (delete-other-windows))

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

  (defun my/dired-split-directories (dir-a dir-b)
    (delete-other-windows)
    (split-window-right)
    (find-file dir-a)
    (find-file-other-window dir-b)
    (other-window 1))

  (defun my/dired-split-downloads-to-ebooks ()
    "Split window with downloads to active directory."
    (interactive)
    (ar/dired-split-directories "~/Downloads"
                                "~/Dropbox/ebooks"))

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

  (defun xah-wrapper-dired-commands ()
    "enable command mode when dired commands are called from"
    (interactive)
    (progn
      (let ((buffer-major-mode
             (format "%s" (get-buffer-mode))))
        (if (equal "dired-mode" buffer-major-mode)
            (xah-fly-insert-mode-activate)
          (xah-fly-command-mode-activate)))))

  (defun dired-do-find-marked-files-and-select-in-ibuffer ()
    "Open marked files in ibuffer and select them."
    (interactive)
    (let ((current (current-buffer)))
      (dired-map-over-marks
       (let* ((filename (dired-get-file-for-visit))
              (buffer (find-file-noselect filename)))
         ;; Select buffer in ibuffer
         (ibuffer)
         (ibuffer-mark-on-buffer #'(lambda (buf)
                                     (eq buf buffer)))
         ;; Go back to dired
         (switch-to-buffer current))
       nil)

      ;; Remove other buffers from ibuffer listing
      (ibuffer)
      (ibuffer-toggle-marks)
      (ibuffer-do-kill-lines)
      (ibuffer-toggle-marks))
    (xah-fly-insert-mode-activate))

  (defun dired-2unix-eol-marked-files ()
    "Change marked file's newline convention to unix,
or file under cursor if no file is marked."
    (interactive)
    (mapc
     (lambda (ff) (change-file-newline ff 'unix))
     (dired-get-marked-files)))

  (defun dired-utf-8-unix-marked-files ()
    "Change marked file's newline convention to unix,
or file under cursor if no file is marked."
    (interactive)
    (mapc
     (lambda (ff) (change-file-newline ff 'utf-8-unix))
     (dired-get-marked-files)))

  (defadvice dired-find-file-other-window (after xah-wrapper-dired-commands activate)
    (xah-wrapper-dired-commands))

  (defadvice ora-dired-other-window (after xah-wrapper-dired-commands activate)
    (xah-wrapper-dired-commands))

  (defadvice diredp-find-file-reuse-dir-buffer (after xah-wrapper-dired-commands activate)
    (xah-wrapper-dired-commands))

  (defadvice projectile-find-file-dwim (after xah-wrapper-dired-commands activate)
    (xah-wrapper-dired-commands))

  ;;* bind and hook
  ;; (define-key dired-mode-map (kbd "e") 'my/dired-diff)
  (define-key dired-mode-map (kbd "e") 'ora-ediff-files)
  (define-key dired-mode-map (kbd "E") 'dired-toggle-read-only)
  (define-key dired-mode-map (kbd "C-t") nil)
  (define-key dired-mode-map (kbd "h") 'ora-dired-up-directory)
  (define-key dired-mode-map (kbd "i") 'counsel-find-file)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "n") 'dired-next-marked-file)
  (define-key dired-mode-map (kbd "p") 'dired-prev-marked-file)
  (define-key dired-mode-map (kbd "q") '(lambda () (interactive)
                                          (progn
                                            (xah-fly-command-mode-activate)
                                            (quit-window))) )
  (define-key dired-mode-map (kbd "%^") 'dired-flag-garbage-files)
  (define-key dired-mode-map (kbd "z") 'ora-dired-get-size)
  ;; (define-key dired-mode-map (kbd "F") 'find-name-dired)
  (define-key dired-mode-map (kbd "f") 'dired-goto-file)
  (define-key dired-mode-map (kbd "'") 'eshell-this-dir)
  (define-key dired-mode-map (kbd "u") 'dired-undo)
  (define-key dired-mode-map (kbd "U") 'dired-unmark-all-marks)
  (define-key dired-mode-map (kbd "!") 'sudired)
  (define-key dired-mode-map (kbd "O") 'ora-dired-other-window)
  (define-key dired-mode-map (kbd "`") 'dired-toggle-read-only)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd ".") 'my/dired-toggle-hide-details)
  (define-key dired-mode-map (kbd "F") 'dired-do-find-marked-files-and-select-in-ibuffer)

  (unbind-key "SPC" dired-mode-map)

  (use-package wdired
    :init
    ;; allow changing of file permissions
    (setq wdired-allow-to-change-permissions t))

  (use-package dired-collapse             ; Collapse unique nested paths
    ;; :ensure t
    :disabled t
    :config (add-hook 'dired-mode-hook #'dired-collapse-mode))

  (use-package dired+
    :defer 1
    :load-path "elisp/"
    :bind ("C-x C-j" . dired-jump)
    :init
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag nil)
    :config
    (progn
      ;; Privilege indicator faces
      (defun modi/dired-update-privilege-faces ()
        (set-face-attribute 'diredp-dir-priv nil
                            :foreground "#7474FFFFFFFF"
                            :background (face-background 'default))
        (set-face-attribute 'diredp-exec-priv nil
                            :foreground "dodger blue"
                            :background (face-background 'default))
        (set-face-attribute 'diredp-other-priv nil
                            :background (face-background 'default))
        (set-face-attribute 'diredp-write-priv nil
                            :foreground "#25258F8F2929"
                            :background (face-background 'default))
        (set-face-attribute 'diredp-read-priv nil
                            :foreground "#999932325555"
                            :background (face-background 'default))
        (set-face-attribute 'diredp-no-priv nil
                            :foreground "#2C2C2C2C2C2C"
                            :background (face-background 'default))
        (set-face-attribute 'diredp-rare-priv nil
                            :foreground "Green"
                            :background (face-background 'default))
        (set-face-attribute 'diredp-link-priv nil
                            :foreground "#00007373FFFF"))
      (add-hook 'dired-mode-hook #'modi/dired-update-privilege-faces)

      (diredp-toggle-find-file-reuse-dir 1)
      (define-key dired-mode-map (kbd "b") 'xah-make-backup-and-save)
      (define-key dired-mode-map (kbd "f") 'projectile-find-file-dwim)
      (define-key dired-mode-map (kbd "C-o") 'xah-open-in-external-app)
      (define-key dired-mode-map (kbd "l") 'my-diredp-find-file-reuse-dir-buffer)))


  (use-package dired-narrow
    :ensure t
    :config
    (bind-keys :map dired-mode-map
               ("N" . dired-narrow-fuzzy)))

  (use-package dired-filter
    :ensure t
    :init
    ;; (define-key dired-mode-map (kbd "F") dired-filter-map)
    (define-key dired-mode-map (kbd "/") dired-filter-map)
    :bind (:map dired-mode-map
                ("/P" . dired-filter-pop-all))
    :config
    (setq dired-filter-verbose nil)
    (setq dired-filter-group-saved-groups
          '(("default"
             ("Archives"
              (extension "zip" "rar" "gz" "bz2" "tar"))
             ("Docs"
              (extension "pdf" "epub"))
             ("Directory"
              (directory))
             ("Media"
              (extension "mp3" "mp4" "avi" "rm" "mkv"))
             ("Image"
              (extension "bmp" "gif" "jpg" "png" "jpeg")))))
    (add-hook 'dired-mode-hook
              (lambda ()
                (dired-filter-mode t)
                (dired-filter-group-mode t))))

  (use-package dired-quick-sort
    :ensure t
    :config
    (bind-keys :map dired-mode-map
               ("s" . hydra-dired-quick-sort/body)))

  (use-package dired-subtree
    :ensure t
    :config
    (bind-keys :map dired-mode-map
               ("i" . dired-subtree-toggle)
               ("<tab>" . dired-subtree-cycle)))

  (use-package dired-launch
    :ensure t)

  (use-package dired-du
    :ensure t)

  (defconst media-files-extensions
    '("mp3" "mp4" "avi" "mpg" "flv" "ogg" "mkv" "mpeg" "wmv" "wav" "mov" "flv" "ogm")
    "Media files.")
  (defconst time-style-space (string ?\u2008) "Punctuation space Unicode char.")

  (use-package dired-rainbow
    :ensure t
    :config
    (progn
      (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
      (dired-rainbow-define xml "#b4fa70" ("xml" "xsd" "xsl" "xslt" "wsdl"))
      (dired-rainbow-define document "#fce94f" ("doc" "docx" "odt" "pdb" "pdf"
                                                "ps" "rtf" "djvu" "epub"))
      (dired-rainbow-define excel "#3465a4" ("xlsx"))
      (dired-rainbow-define media "#ce5c00" media-files-extensions)
      (dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))
      (dired-rainbow-define log "#c17d11" ("log"))
      (dired-rainbow-define sourcefile "#fcaf3e" ("py" "c" "cc" "h" "java" "pl"
                                                  "rb" "R" "php"))
      (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
      (dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz"
                                                  "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
      (dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
      (dired-rainbow-define encrypted "LightBlue" ("gpg" "pgp"))
      (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")))

  ;;preview files in dired
  (use-package peep-dired
    :ensure t
    :disabled t
    :defer t ;; don't access `dired-mode-map' until `peep-dired' is loaded
    :bind (:map dired-mode-map
                ("P" . peep-dired))))

(use-package ranger
  :ensure t
  :disabled t
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
  (setq ranger-width-preview 0.5)
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
    (linum-mode -1))
  (add-hook 'ranger-mode-hook 'my/ranger-hooks)
  (add-hook 'ranger-parent-dir-hook 'my/ranger-hooks)

  (setq ranger-preview-header-func (lambda () (interactive)
                                     (last (s-split "/" (buffer-file-name) t)))))

(use-package all-the-icons-dired
  ;; :if (display-graphic-p)
  :ensure t
  :diminish all-the-icons-dired-mode
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  ;; :if (display-graphic-p)
  :ensure t
  :init
  (progn
    (all-the-icons-ivy-setup)
    (ivy-set-display-transformer 'counsel-recentf 'all-the-icons-ivy-file-transformer)
    (ivy-set-display-transformer 'counsel-projectile-switch-to-buffer 'all-the-icons-ivy-file-transformer)))

(provide 'init-dired)
