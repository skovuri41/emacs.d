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

    (defhydra hydra-toggle-map nil
      "
     Toggle^
     ^^^^^^^--------------------
     d_: debug-on-error
     D_: debug-on-quit
     f_: auto-fill-mode
     l_: toggle-truncate-lines
     h_: hl-line-mode
     r_: read-only-mode
     v_: viewing-mode
     n_: narrow-or-widen-dwim
     g_: golden-ratio-mode
     q_: quit
    "
      ("d" toggle-debug-on-error :exit t)
      ("D" toggle-debug-on-quit :exit t)
      ("g" golden-ratio-mode :exit t)
      ("f" auto-fill-mode :exit t)
      ("l" toggle-truncate-lines :exit t)
      ("r" read-only-mode :exit t)
      ("h" hl-line-mode :exit t)
      ("v" my/turn-on-viewing-mode :exit t)
      ("n" my/narrow-or-widen-dwim :exit t)
      ("q" nil :exit t))

    (global-set-key (kbd "C-x t") 'hydra-toggle-map/body)

    (defun hydra/smart-copy (f)
      "Invoke exprand region function F before call `smart-copy-add`"
      (funcall
       `(lambda () (interactive)
          (save-excursion
            (,f)
            (kill-new (buffer-substring (region-beginning) (region-end)))
            (keyboard-quit)))))

    (defhydra hydra-smart-copy (:color pink :hint nil)
      "
^Mark^                    | ^Files^        | ^Pairs^            | ^Quotes^
------------------------+--------------^+^------------------^+^-----------------------
_w_: word    _._: sentence  | _d_: file dir  | _p_: Inside Pairs  | _o_:
Inside Quotes
_s_: symbol  _h_: paragraph | _f_: file name | _P_: Outside Pairs | _O_:
Outside Quotes
_d_: defun   _u_: url       | _F_: full path
                        | _b_: buffer
"
      ("w" (hydra/smart-copy 'er/mark-word) :exit t :color teal)
      ("s" (hydra/smart-copy 'er/mark-symbol) :exit t :color teal)
      ("d" (hydra/smart-copy 'er/mark-defun) :exit t :color teal)
      ("p" (hydra/smart-copy 'er/mark-inside-pairs) :exit t :color teal)
      ("P" (hydra/smart-copy 'er/mark-outside-pairs) :exit t :color teal)
      ("o" (hydra/smart-copy 'er/mark-inside-quotes) :exit t :color teal)
      ("O" (hydra/smart-copy 'er/mark-outside-quotes) :exit t :color teal)
      ("." (hydra/smart-copy 'er/mark-text-sentence) :exit t :color teal)
      ("h" (hydra/smart-copy 'er/mark-text-paragraph) :exit t :color teal)
      ("b" (kill-new (buffer-name)) :exit t :color teal)
      ("F" (kill-new (buffer-file-name)) :exit t :color teal)
      ("f" (kill-new (file-name-nondirectory (buffer-file-name))) :exit t :color
       teal)
      ("d" (kill-new (file-name-directory (buffer-file-name))) :exit t :color
       teal)
      ("u" (hydra/smart-copy 'er/mark-url) :exit t :color teal)
      ("m" avy-goto-char "move")
      ("q" nil "quit"))


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
