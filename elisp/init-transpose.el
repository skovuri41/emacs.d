(use-package transpose-frame
  :ensure t
  :init
  (progn 
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
    )
  )
                         
(provide 'init-transpose)














