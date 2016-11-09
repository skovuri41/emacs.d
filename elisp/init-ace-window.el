(use-package ace-window
  :ensure t
  :config
  (progn
    (validate-setq aw-ignore-on t)
    ;; (add-to-list 'aw-ignored-buffers " *NeoTree*")
    (validate-setq aw-swap-invert t)
    (validate-setq aw-dispatch-always nil)
    (validate-setq aw-scope 'frame)
    (validate-setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package ace-link
  :config (ace-link-setup-default))

(provide 'init-ace-window)
