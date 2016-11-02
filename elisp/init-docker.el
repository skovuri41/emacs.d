;;; init-docker.el --- docker configuration          -*- lexical-binding: t; -*-

;; docker
(use-package docker
  :ensure t
  :commands docker-mode)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile.*\\'")


(provide 'init-docker)
