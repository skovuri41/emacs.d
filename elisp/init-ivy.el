(unless (executable-find "ag")
  (message "%s" "executable: ag not found!, counsel-ag will not work."))

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume))
  :init
  (progn
    (setq projectile-completion-system 'ivy)
    (setq swiper-completion-method 'ivy)
    (setq magit-completing-read-function 'ivy-completing-read)
    (global-set-key (kbd "C-s") 'swiper)
    (with-eval-after-load "ivy"
      (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-insert-current)
      (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-backward-kill-word)
      (define-key ivy-minibuffer-map [escape] (kbd "C-g"))
      (key-chord-define ivy-minibuffer-map "kj" (kbd "C-g"))
      ;; (key-chord-define ivy-minibuffer-map "kj" 'keyboard-quit)
      )
    (setq ivy-use-virtual-buffers t
          ivy-display-style 'fancy)
    (setq ivy-count-format "(%d/%d) ")
    ;; (ivy-mode 1)
    (setq ivy-initial-inputs-alist nil)
    ;; (setq ivy-re-builders-alist
    ;;       '((t . ivy--regex-fuzzy)))
    ;; (setq ivy-re-builders-alist
    ;;       '((t . ivy--regex-plus)))
    (setq ivy-re-builders-alist
          '((read-file-name-internal . ivy--regex-fuzzy)
            (ivy-switch-buffer . ivy--regex-fuzzy)
            (counsel-M-x . ivy--regex-fuzzy)
            (t . ivy--regex-plus)))
    )
  :config
  (progn
    (define-key swiper-map (kbd "C-.")
      (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window
                                                       (thing-at-point 'symbol))))))
    (defun swiper-the-thing ()
      (interactive)
      (swiper (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    ))

(use-package ivy
  :ensure swiper
  :diminish ivy-mode
  :config
  (progn
    ;; partial complete without exiting
    (define-key ivy-minibuffer-map
      (kbd "TAB") #'ivy-partial)
    ;;advise swiper to recenter on exit
    (defun bjm-swiper-recenter (&rest args)
      "recenter display after swiper"
      (recenter))
    (advice-add 'swiper :after #'bjm-swiper-recenter)
    ;; fancier colors
    (setq ivy-display-style 'fancy)
    ;;ivy-wrap
    (setq ivy-wrap t)

    (ivy-set-actions
     t
     '(("i" (lambda (x) (with-ivy-window
                     (insert x))) "insert candidate")
       (" " (lambda (x) (ivy-resume)) "resume")
       ("?" (lambda (x)
              (interactive)
              (describe-keymap ivy-minibuffer-map)) "Describe keys")))

    ;; ** Find file actions
    (ivy-add-actions
     'counsel-find-file
     '(("a" (lambda (x)
              (unless (memq major-mode '(mu4e-compose-mode message-mode))
                (compose-mail))
              (mml-attach-file x)) "Attach to email")
       ("c" (lambda (x) (kill-new (f-relative x))) "Copy relative path")
       ("4" (lambda (x) (find-file-other-window x)) "Open in new window")
       ("5" (lambda (x) (find-file-other-frame x)) "Open in new frame")
       ("C" (lambda (x) (kill-new x)) "Copy absolute path")
       ("d" (lambda (x) (dired x)) "Open in dired")
       ("D" (lambda (x) (delete-file x)) "Delete file")
       ("e" (lambda (x) (shell-command (format "open %s" x)))
        "Open in external program")
       ("f" (lambda (x)
              "Open X in another frame."
              (find-file-other-frame x))
        "Open in new frame")
       ("p" (lambda (path)
              (with-ivy-window
                (insert (f-relative path))))
        "Insert relative path")
       ("P" (lambda (path)
              (with-ivy-window
                (insert path)))
        "Insert absolute path")
       ("l" (lambda (path)
              "Insert org-link with relative path"
              (with-ivy-window
                (insert (format "[[file:%s]]" (f-relative path)))))
        "Insert org-link (rel. path)")
       ("L" (lambda (path)
              "Insert org-link with absolute path"
              (with-ivy-window
                (insert (format "[[file:%s]]" path))))
        "Insert org-link (abs. path)")
       ("r" (lambda (path)
              (rename-file path (read-string "New name: ")))
        "Rename")))

    ;; open recent directory, requires ivy (part of swiper)
    ;; borrows from http://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
    (defun bjm/ivy-dired-recent-dirs ()
      "Present a list of recently used directories and open the selected one in dired"
      (interactive)
      (let ((recent-dirs
             (delete-dups
              (mapcar (lambda (file)
                        (if (file-directory-p file) file (file-name-directory file)))
                      recentf-list))))

        (let ((dir (ivy-read "Directory: "
                             recent-dirs
                             :re-builder #'ivy--regex-fuzzy
                             ;;ivy--regex
                             :sort nil
                             :initial-input nil)))
          (dired dir)))))
  :init
  (progn
    (ivy-mode 1)))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-ag)
         ("C-c y" . counsel-yank-pop)
         ("C-c g" . counsel-git)
         ("C-x l" . counsel-locate))
  :config
  (progn
    (defun counsel-ag-projectile ()
      "Counsel version of projectile-ag."
      (interactive)
      (counsel-ag "" (projectile-project-root)))

    (defun counsel-ag-project-symbol ()
      (interactive)
      (counsel-ag (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (thing-at-point 'symbol))
                  (projectile-project-root)))

    )
  )

(use-package counsel-projectile       ; Ivy integration for Projectile
  :ensure t
  :bind (:map projectile-command-map
              ("p" . counsel-projectile)))


(use-package tiny
  :commands tiny-expand)



(provide 'init-ivy)
