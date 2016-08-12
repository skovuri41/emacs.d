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
      (define-key ivy-minibuffer-map (kbd "C-o") 'hydra-ivy/body)
      (key-chord-define ivy-minibuffer-map "kj" (kbd "C-g") )
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
            (t . ivy--regex-plus)))
    )
  )

(use-package ivy
  :ensure swiper
  :diminish ivy-mode
  :bind (("C-x b" . nil))
  :config
  (progn
    ;; partial complete without exiting
    (define-key ivy-minibuffer-map
      (kbd "TAB") #'ivy-partial)
    ;;advise swiper to recenter on exit
    (defun bjm-swiper-recenter (&rest args)
      "recenter display after swiper"
      (recenter)
      )
    (advice-add 'swiper :after #'bjm-swiper-recenter)
    ;; fancier colors
    (setq ivy-display-style 'fancy)
    ;;ivy-wrap
    (setq ivy-wrap t)

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
          (dired dir))))
    )
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
         ("C-x l" . counsel-locate)))

(use-package counsel-projectile         ; Ivy integration for Projectile
  :ensure t
  :bind (:map projectile-command-map
              ("p" . counsel-projectile))
  )

(provide 'init-ivy)
