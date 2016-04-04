;;; init-docker.el --- docker configuration          -*- lexical-binding: t; -*-

;; docker
(use-package docker
  :commands docker-mode)

(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")
