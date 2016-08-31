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
_w_: word    _._: sentence      _P_: Inside Pairs   _q_: Inside Quotes
_s_: symbol  _b_: paragraph     _p_: Outside Pairs  _Q_: Outside Quotes
_d_: defun   _l_: line          _=_: expand-region
"
      ("w" er/mark-word )
      ("s" er/mark-symbol)
      ("d" er/mark-defun)
      ("P" er/mark-inside-pairs)
      ("p" er/mark-outside-pairs)
      ("q" er/mark-inside-quotes)
      ("Q" er/mark-outside-quotes)
      ("." er/mark-text-sentence)
      ("b" er/mark-text-paragraph)
      ("l" er/mark-line)
      ("=" er/expand-region)
      ("q" nil "quit")))
  )
(provide 'init-expand-region)
