(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :defer t
  :init
  (progn

    (defun restclient-http-send-current-raw-stay-in-window ()
      (interactive)
      (restclient-http-send-current t t))
    )
  )


(provide 'init-restclient)
