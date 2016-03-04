(use-package hydra
  :ensure t
  :config
  (progn
    ;; hydra zoom
    (defhydra hydra-zoom (global-map "<f5>")
      "zoom"
      ("+" text-scale-increase "in")
      ("-" text-scale-decrease "out"))
    (global-set-key (kbd "<f5>") 'hydra-zoom/body)

    ;; hydra rectangle
    (defun my-ex-point-mark ()
      (interactive)
      (if rectangle-mark-mode
          (exchange-point-and-mark)
        (let ((mk (mark)))
          (rectangle-mark-mode 1)
          (goto-char mk))))

    (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                         :color pink
                                         :post (deactivate-mark))
      "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _q_uit        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
      ("h" backward-char nil)
      ("l" forward-char nil)
      ("k" previous-line nil)
      ("j" next-line nil)
      ("e" my-ex-point-mark nil)
      ("n" copy-rectangle-as-kill nil)
      ("d" delete-rectangle nil)
      ("r" (if (region-active-p)
               (deactivate-mark)
             (rectangle-mark-mode 1)) nil)
      ("y" yank-rectangle nil)
      ("u" undo nil)
      ("s" string-rectangle nil)
      ("p" kill-rectangle nil)
      ("q" nil nil))
    (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

    ;; YASnippet
    (defhydra hydra-yasnippet (:color blue :hint nil)
      "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:
 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
      ("d" yas-load-directory)
      ("e" yas-activate-extra-mode)
      ("i" yas-insert-snippet)
      ("f" yas-visit-snippet-file :color blue)
      ("n" yas-new-snippet)
      ("t" yas-tryout-snippet)
      ("l" yas-describe-tables)
      ("g" yas/global-mode)
      ("m" yas/minor-mode)
      ("a" yas-reload-all))
    (global-set-key (kbd "C-c C-y") 'hydra-yasnippet/body)
    ))


(provide 'init-hydra)
