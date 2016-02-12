(use-package link-hint
  :ensure t
  :bind
  (:map evil-leader--default-map
        ("ll" . link-hint-open-link)
        ("la" . link-hint-open-all-links)))

(provide 'init-link-hint)
