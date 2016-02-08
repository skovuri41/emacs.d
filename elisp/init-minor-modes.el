(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))
(use-package json-mode
  :defer t)

(use-package js2-mode
  :defer t)

(use-package scratch
	  :ensure t)
	

(provide 'init-minor-modes)
