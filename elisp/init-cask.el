(when (file-readable-p "/usr/local/share/emacs/site-lisp/cask/cask.el")
  (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
  (cask-initialize))

(provide 'init-cask)
