(use-package ztree-diff
  :ensure ztree
  :config
  (set-face-attribute 'ztreep-diff-model-add-face  nil :foreground "deep sky blue")
  (setq ztree-draw-unicode-lines t)
  (bind-keys :map ztreediff-mode-map
             ("p" . previous-line)
             ("k" . previous-line)
             ("j" . next-line)
             ("n" . next-line))

  (when (package-installed-p 'hydra)
    (bind-keys :map ztreediff-mode-map
               ("\\" . hydra-ztree/body))
    (defhydra hydra-ztree (:color blue :hint nil)
      "
                                                                      ╭────────────┐
       Move      File                 Do                              │ Ztree diff │
    ╭─────────────────────────────────────────────────────────────────┴────────────╯
      _k_/_p_   [_C_] copy                  [_h_] toggle equal files
      ^ ^↑^ ^   [_D_] delete                [_x_] toggle subtree
      ^_TAB_^   [_v_] view                  [_r_] partial rescan
      ^ ^↓^ ^   [_d_] simple diff           [_R_] full rescan
      _j_/_n_   [_RET_] diff/expand         [_g_] refresh
      ^ ^ ^ ^   [_SPC_] simple diff/expand
    --------------------------------------------------------------------------------
          "
      ("\\" hydra-master/body "back")
      ("<ESC>" nil "quit")
      ("p" previous-line)
      ("k" previous-line)
      ("j" next-line)
      ("n" next-line)
      ("C" ztree-diff-copy)
      ("h" ztree-diff-toggle-show-equal-files)
      ("D" ztree-diff-delete-file)
      ("v" ztree-diff-view-file)
      ("d" ztree-diff-simple-diff-files)
      ("r" ztree-diff-partial-rescan)
      ("R" ztree-diff-full-rescan)
      ("RET" ztree-perform-action)
      ("SPC" ztree-perform-soft-action)
      ("TAB" ztree-jump-side)
      ("g" ztree-refresh-buffer)
      ("x" ztree-toggle-expand-subtree))))
