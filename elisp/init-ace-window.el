(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window)
  :config
  (setq aw-ignore-on t)
  (add-to-list 'aw-ignored-buffers " *NeoTree*")
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'init-ace-window)
