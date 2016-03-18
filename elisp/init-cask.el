(let ((cask-el "/usr/local/share/emacs/site-lisp/cask/cask.el"))
  (when (file-readable-p cask-el)
    (require 'cask cask-el)
    (cask-initialize)))

(provide 'init-cask)
