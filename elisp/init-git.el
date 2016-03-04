(use-package git-timemachine
  :ensure t
  :config
  (progn
    (evil-make-overriding-map git-timemachine-mode-map 'normal)
    ;; force update evil keymaps after git-timemachine-mode loaded
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))
  )

(use-package git-gutter+
  :commands global-git-gutter+-mode
  :init
  (progn
    (setq git-gutter+-hide-gutter t)
    (add-hook 'magit-pre-refresh-hook 'git-gutter+-refresh))
  )

(use-package git-gutter-fringe+
  :init
  (progn
    (when (display-graphic-p)
      (with-eval-after-load 'git-gutter+
        (require 'git-gutter-fringe+)))
    (global-git-gutter+-mode)
    ;; (setq git-gutter-fr+-side 'right-fringe))
    :config
    (progn
      ;; custom graphics that works nice with half-width fringes
      (fringe-helper-define 'git-gutter-fr+-added nil
        "..X...."
        "..X...."
        "XXXXX.."
        "..X...."
        "..X...."
        )
      (fringe-helper-define 'git-gutter-fr+-deleted nil
        "......."
        "......."
        "XXXXX.."
        "......."
        "......."
        )
      (fringe-helper-define 'git-gutter-fr+-modified nil
        "..X...."
        ".XXX..."
        "XX.XX.."
        ".XXX..."
        "..X...."
        ))))


(provide 'init-git)
