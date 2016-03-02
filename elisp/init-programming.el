(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word))
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
  :bind (("M-." . find-function-at-point)
         ("M-&" . complete-symbol))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package csv-mode
  :ensure t
  :mode  (("\\.csv\\'" . csv-mode))
  :mode ("\\.tsv\\'" . csv-mode)
  :mode ("\\.[Cc][Ss][Vv]\\'")
  :config
  (evil-leader/set-key-for-mode 'csv-mode
    "mt"  'csv-transpose
    "ma"  'csv-align-fields
    "mu"  'csv-unalign-fields
    "msf" 'csv-sort-fields
    "msn" 'csv-sort-numeric-fields
    "mso" 'csv-toggle-descending
    "mn"  'csv-forward-field
    "mp"  'csv-backward-field
    "mr"  'csv-reverse-region
    "md"  'csv-kill-fields
    "mi"  'csv-toggle-invisibility
    "mvf" 'csv-yank-fields
    "mvt" 'csv-yank-as-new-table)
  )
(use-package csv-nav
  :ensure t
  :config
  (autoload 'csv-nav-mode "csv-nav" "Major mode for navigating comma-separated value files." t)
  (setq csv-separators '("," ";" "|" " "))
  )

(use-package scratch
  :ensure t)

(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  )

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
_o_pen node    _n_ext fold       toggle _f_orward    _F_ill column: %`fill-column
_c_lose node   _p_revious fold   toggle _a_ll        e_x_it
"
        ("o" origami-open-node)
        ("c" origami-close-node)
        ("n" origami-next-fold)
        ("p" origami-previous-fold)
        ("f" origami-forward-toggle-node)
        ("a" origami-toggle-all-nodes)
        ("F" fill-column)
        ("x" nil :color blue))
      (evil-leader/set-key "no" 'hydra-folding/body)
      ))

  (setq vc-handled-backends '(git svn)))

(provide 'init-programming)
