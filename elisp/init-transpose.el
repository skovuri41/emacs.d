(use-package transpose-frame
  :ensure t
  :init
  (progn
    (defhydra hydra-transpose-frame(:color blue :hint nil)
      "
      _t_ranspose  _f_lip    f_l_op 
      _r_otate  _c_lockwise  _a_nticlockwise _q_uit
     "
      ("t" transpose-frame :exit nil)
      ("f" flip-frame :exit nil)
      ("l" flop-frame :exit nil)
      ("r" rotate-frame :exit nil)
      ("c" rotate-frame-clockwise :exit nil)
      ("a" rotate-frame-anticlockwise :exit nil)
      ("q" nil :color red))))
                         
(provide 'init-transpose)














