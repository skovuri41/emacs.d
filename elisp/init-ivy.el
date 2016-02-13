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
    ;; (ivy-mode 1)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-re-builders-alist
          '((t . ivy--regex-fuzzy))))
  (progn
    (evil-global-set-key 'normal [remap evil-search-forward] #'swiper)
    (evil-global-set-key 'normal [remap evil-search-backward] #'swiper)
    (evil-global-set-key 'motion [remap evil-search-forward] #'swiper)
    (evil-global-set-key 'motion [remap evil-search-backward] #'swiper))
  )

(use-package ivy
  :ensure swiper
  :diminish ivy-mode
  :bind
  ("C-x b" . nil)
  :bind
  (:map evil-leader--default-map
        ("bs" . ivy-switch-buffer))
  :config (progn
            ;; partial complete without exiting
            (define-key ivy-minibuffer-map
              (kbd "TAB") #'ivy-partial)

            ;; fancier colors
            (setq ivy-display-style 'fancy))
  :init (progn
          (ivy-mode 1)))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ;;("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-ag)
         ("C-c g" . counsel-git)
         ("C-x l" . counsel-locate))
  :bind
  (:map evil-leader--default-map
        ("ff" . counsel-find-file)))

(provide 'init-ivy)
