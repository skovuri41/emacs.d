(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package web-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-enable-css-colorization t)
                        (setq web-mode-markup-indent-offset 2)
                        (setq web-mode-style-padding 2)
                        (setq web-mode-script-padding 2)))
            (setq web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-enable-auto-pairing t
                  web-mode-enable-engine-detection t
                  web-mode-enable-current-element-highlight t)))

(use-package css-mode
  :config (setq css-indent-offset 2))

(use-package js-mode
  :mode ("\\.json$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))))

(use-package json-navigator
  :ensure t)

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :defer t
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))))

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package macrostep
      :bind ("C-c e" . macrostep-expand))
    (use-package ert
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)))
  :config
  (progn
    (setq tab-always-indent 'complete)
    (add-to-list 'completion-styles 'initials t))
  :bind (("M-&" . complete-symbol))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package csv-mode
  :ensure t
  :mode (("\\.csv\\'" . csv-mode))
  :mode ("\\.tsv\\'" . csv-mode)
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))

(use-package json-mode
  :defer t)

(use-package csv-nav
  :ensure t
  :config
  (autoload 'csv-nav-mode "csv-nav" "Major mode for navigating comma-separated value files." t)
  (setq csv-separators '("," ";" "|" " ")))

(use-package scratch
  :ensure t)

(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; whitespace
(use-package whitespace
  :commands (whitespace-mode)
  :config
  (progn
    (setq show-trailing-whitespace t)
    ;;(add-hook'before-save-hook'delete-trailing-whitespace)
    (setq whitespace-style '(face tabs spaces newline empty
                                  trailing tab-mark newline-mark))))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode))


(use-package abbrev
  :diminish ""
  :config
  (progn
    ;; abbrev file name
    (setq abbrev-file-name (expand-file-name "elisp/emacs_abbrev.el" user-emacs-directory))
    (unless (file-exists-p abbrev-file-name)
      (with-temp-buffer (write-file abbrev-file-name)))
    (setq save-abbrevs 'silently) ; Silently save abbrevs on quitting emacs

    (defconst my/abbrev-hooks '( emacs-lisp-mode-hook
                                 org-mode-hook)
      "List of hooks of major modes in which abbrev should be enabled.")

    (defun my/turn-on-abbrev-mode ()
      "Turn on abbrev only for specific modes."
      (interactive)
      (dolist (hook my/abbrev-hooks)
        (add-hook hook #'abbrev-mode)))

    (defun my/turn-off-abbrev-mode ()
      "Turn off abbrev only for specific modes."
      (interactive)
      (dolist (hook my/abbrev-hooks)
        (remove-hook hook #'abbrev-mode)))

    (my/turn-on-abbrev-mode)
    (quietly-read-abbrev-file)))


;; Origami code folding
(use-package origami
  :ensure t
  :commands (origami-mode)
  :init
  (global-origami-mode t)
  :config
  (progn
    (with-eval-after-load 'hydra
      (defhydra hydra-folding (:color red :hint nil)
        "
_o_pen node    _j_: next fold       toggle _f_orward    _F_ill column: %`fill-column
_c_lose node   _k_: previous fold   toggle _a_ll        _q_: exit
"
        ("o" origami-open-node)
        ("c" origami-close-node)
        ("j" origami-next-fold)
        ("k" origami-previous-fold)
        ("f" origami-forward-toggle-node)
        ("a" origami-toggle-all-nodes)
        ("F" fill-column)
        ("q" nil :color blue))
      ))

  (setq vc-handled-backends '(git svn)))

(use-package vimish-fold
  :ensure t
  :config
  (progn
    (with-eval-after-load 'hydra
      (defhydra hydra-vimish-fold (:color blue
                                          :columns 3)
        "fold"
        ("a" vimish-fold-avy "avy")
        ("d" vimish-fold-delete "del")
        ("D" vimish-fold-delete-all "del-all")
        ("u" vimish-fold-unfold "undo")
        ("U" vimish-fold-unfold-all "undo-all")
        ("f" vimish-fold "fold")
        ("r" vimish-fold-refold "refold")
        ("R" vimish-fold-refold-all "refold-all")
        ("t" vimish-fold-toggle "toggle" :exit nil)
        ("T" vimish-fold-toggle-all "toggle-all" :exit nil)
        ("j" vimish-fold-next-fold "down" :exit nil)
        ("k" vimish-fold-previous-fold "up" :exit nil)
        ("q" nil "quit")))))

(use-package dumb-jump
  :ensure t
  :init (dumb-jump-mode)
  :config
  (setq dumb-jump-selector 'ivy))

(use-package groovy-mode
  :ensure t
  :mode (("\\.gvy\\'" . groovy-mode)
         ("\\.groovy\\'" . groovy-mode)))

(use-package drools-mode
  ;; :quelpa (drools-mode :fetcher github :repo "suryaaditya/rules-editing-mode")
  :load-path "elisp/"
  ;;:ensure t
  ;; :disabled t
  :init
  (progn
    (defun set-extension-mode (extension mode)
      (setq auto-mode-alist
            (cons (cons (concat "\\" extension "\\'") mode)
                  auto-mode-alist)))

    (set-extension-mode ".drl" 'drools-mode)
    (set-extension-mode ".dslr" 'drools-mode)

    (add-hook 'drools-mode-hook 'my-drools-hook)

    (defun drools-return-and-indent ()
      (interactive)
      (newline) (indent-for-tab-command))

    (defun my-drools-hook ()
      (setq indent-tabs-mode nil)
      (local-set-key [?\C-m] 'drools-return-and-indent))))

(provide 'init-programming)
