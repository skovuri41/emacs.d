(use-package expand-region
  :ensure t
  :config
  (progn
    (setq expand-region-contract-fast-key "-")

    (defun er/mark-line()
      (interactive)
      (back-to-indentation)
      (set-mark (point))
      (move-end-of-line nil)
      (re-search-backward "^\\|[^[:space:]]")
      (when (eq last-command 'er/expand-region)
        (forward-char)))

    (defhydra hydra-expand-region (:color pink :hint nil)
      "
^Mark^                     ^Pairs^             ^Quotes^
^^^^^^-------------------------^-------------------^--------^---------------------------
_w_: word    _._: sentence      _p_: Inside Pairs   _O_: Inside Quotes
_s_: symbol  _h_: paragraph     _l_: Outside Pairs  _o_: Outside Quotes
_d_: defun   _=_: expand-region
"
      ("w" (er/mark-word) :exit t :color teal)
      ("s" (er/mark-symbol) :exit t :color teal)
      ("d" (er/mark-defun) :exit t :color teal)
      ("p" (er/mark-inside-pairs) :exit t :color teal)
      ("l" (er/mark-outside-pairs) :exit t :color teal)
      ("O" (er/mark-inside-quotes) :exit t :color teal)
      ("o" (er/mark-outside-quotes) :exit t :color teal)
      ("." (er/mark-text-sentence) :exit t :color teal)
      ("h" (er/mark-text-paragraph) :exit t :color teal)
      ("=" er/expand-region :exit t :color teal)
      ("q" nil "quit" :color blue)))
  )
(provide 'init-expand-region)
