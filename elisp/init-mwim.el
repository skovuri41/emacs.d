(use-package mwim
  :load-path "elisp/"
  :config
  (progn
    (autoload 'mwim-beginning-of-code-or-line "mwim" nil t)
    (autoload 'mwim-beginning-of-line-or-code "mwim" nil t)
    (autoload 'mwim-end-of-code-or-line "mwim" nil t)
    (autoload 'mwim-end-of-line-or-code "mwim" nil t)
    ))
