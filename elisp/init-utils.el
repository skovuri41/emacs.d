(use-package symon
  :ensure t
  :init
  (setq symon-refresh-rate 2
        symon-delay 5)
  (symon-mode 1)
  :config
  (setq symon-sparkline-type 'bounded))

(use-package vlf-setup)

(use-package typit
  :ensure t)

(use-package define-word
  :ensure t)

(use-package chess
  :ensure t
  :commands chess
  :config
  (setq chess-images-default-size 70
        chess-images-separate-frame nil))

(use-package lorem-ipsum
  :ensure t
  :commands lorem-ipsum-insert-paragraphs)

(use-package boxquote
  :ensure t
  :defer t
  :config
  (setq-default  boxquote-bottom-corner "╰"      ; U+2570
                 boxquote-side          "│ "     ; U+2572 + space
                 boxquote-top-and-tail  "────"   ; U+2500 (×4)
                 boxquote-top-corner    "╭")     ; U+256F
  (when (package-installed-p 'hydra)
    (defhydra hydra-boxquote (:color blue :hint nil)
      "
                                                                    ╭──────────┐
  Text           External           Apropos         Do              │ Boxquote │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_r_] region        [_f_] file      [_K_] describe-key        [_t_] title
  [_p_] paragraph     [_b_] buffer    [_F_] describe-function   [_u_] unbox
  [_a_] buffer        [_s_] shell     [_V_] describe-variable   [_w_] fill-paragraph
  [_e_] text           ^ ^            [_W_] where-is            [_n_] narrow
  [_d_] defun         [_y_] yank       ^ ^                      [_c_] narrow to content
  [_q_] boxquote      [_Y_] yanked     ^ ^                      [_x_] kill
--------------------------------------------------------------------------------
       "
      ("<esc>" nil "quit")
      ("x" boxquote-kill)
      ("Y" boxquote-yank)
      ("e" boxquote-text)
      ("u" boxquote-unbox)
      ("d" boxquote-defun)
      ("t" boxquote-title)
      ("r" boxquote-region)
      ("a" boxquote-buffer)
      ("q" boxquote-boxquote)
      ("W" boxquote-where-is)
      ("p" boxquote-paragraph)
      ("f" boxquote-insert-file)
      ("K" boxquote-describe-key)
      ("s" boxquote-shell-command)
      ("b" boxquote-insert-buffer)
      ("y" boxquote-kill-ring-save)
      ("w" boxquote-fill-paragraph)
      ("F" boxquote-describe-function)
      ("V" boxquote-describe-variable)
      ("n" boxquote-narrow-to-boxquote)
      ("c" boxquote-narrow-to-boxquote-content))))

(provide 'init-utils)
