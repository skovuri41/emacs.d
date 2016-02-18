(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (setq hl-paren-delay 0.2)
  (setq hl-paren-colors '("Springgreen3"
                          "DarkOrange1"
                          "magenta1"
                          "RoyalBlue1"
                          "DeepPink1"
                          "chartreuse1"
                          "gold1"))
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

(provide 'init-highlight-parentheses)
