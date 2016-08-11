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

    (defhydra hydra-expand-region (:color pink :hint nil :exit t)
      "
^Mark^                     ^Pairs^             ^Quotes^
^^^^^^-------------------------^-------------------^--------^---------------------------
_w_: word    _._: sentence      _p_: Inside Pairs   _O_: Inside Quotes
_s_: symbol  _h_: paragraph     _l_: Outside Pairs  _o_: Outside Quotes
_d_: defun   _=_: expand-region
"
      ("w" er/mark-word )
      ("s" er/mark-symbol  )
      ("d" er/mark-defun  )
      ("p" er/mark-inside-pairs)
      ("l" er/mark-outside-pairs)
      ("O" er/mark-inside-quotes)
      ("o" er/mark-outside-quotes )
      ("." er/mark-text-sentence )
      ("h" er/mark-text-paragraph)
      ("=" er/expand-region)
      ("q" nil "quit")))
  )
(provide 'init-expand-region)
