(unless (executable-find "ag")
  (message "%s" "executable: ag not found!, counsel-ag will not work."))

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume))
  :init
  (progn
    (setq projectile-completion-system 'ivy)
    (global-set-key (kbd "C-s") 'swiper)
    (with-eval-after-load "ivy"
      (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-insert-current)
      (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-backward-delete-char)
      (define-key ivy-minibuffer-map [escape] (kbd "C-g")))
    (setq ivy-use-virtual-buffers t
          ivy-display-style 'fancy)
    (setq ivy-count-format "(%d/%d) ")
    (ivy-mode 1)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-re-builders-alist
          '((t . ivy--regex-fuzzy)))))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ;;("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-ag)
         ("C-c g" . counsel-git)
         ("C-x l" . counsel-locate)))

(provide 'init-ivy)
