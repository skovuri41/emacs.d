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
  :diminish ""
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
    )
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
      )
    ;; hydra git-gutter
    (defhydra hydra-git-gutter (:body-pre (global-git-gutter+-mode 1)
                                          :hint nil)
      "
   Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk   
  _m_: git-gutter+-mode _c_ommit 
  _b_: stage & commit  _B_: stage & commit whole buffer
"
      ("j" git-gutter+-next-hunk)
      ("k" git-gutter+-previous-hunk)
      ("s" git-gutter+-stage-hunks)
      ("r" git-gutter+-unstage-whole-buffer)
      ("m" global-git-gutter+-mode)
      ("c" git-gutter+-commit)
      ("b" git-gutter+-stage-and-commit)
      ("B" git-gutter+-stage-and-commit-whole-buffer)
      ("q" nil :color blue))
    ))


(provide 'init-git)
