(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window)
  :config
  (setq aw-ignore-on t)
  (add-to-list 'aw-ignored-buffers " *NeoTree*")
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package transpose-frame
  :ensure t
  ;; :disabled t
  :commands (transpose-frame)
  :init
  (progn 
    (with-eval-after-load 'hydra
      (defhydra hydra-transpose-frame(:color red :hint nil)
        "
      _t_ranspose  _f_lip    f_l_op 
      _r_otate  _c_lockwise  _a_nticlockwise e_x_it
     "
        ("t" transpose-frame)
        ("f" flip-frame)
        ("l" flop-frame)
        ("r" rotate-frame)
        ("c" rotate-frame-clockwise)
        ("a" rotate-frame-anticlockwise)
        ("x" nil :color blue)
        )
      (evil-leader/set-key "wt" 'hydra-transpose-frame/body)
      ))
  )



(provide 'init-ace-window)
