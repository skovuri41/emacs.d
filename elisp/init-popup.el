(use-package popup
  :ensure t
  :init
  (autoload 'which-function "which-func")
  (autoload 'popup-tip "popup")
  :config
  (progn
    (defun copy-yank-str (msg)
      (kill-new msg)
      (with-temp-buffer
        (insert msg)
        (shell-command-on-region (point-min) (point-max)
                                 (cond
                                  ((eq system-type 'cygwin) "putclip")
                                  ((eq system-type 'darwin) "pbcopy")
                                  (t "xsel -ib")
                                  ))))
    (defun popup-which-function ()
      (interactive)
      (let ((msg (which-function)))
        (popup-tip msg)
        (copy-yank-str msg)
        ))))


(provide 'init-popup)
