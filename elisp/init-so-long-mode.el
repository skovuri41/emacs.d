;;; init-so-long-mode --- configure so-long support
;;; Commentary:
;; Configure so-long-mode to my liking.

;;; Code:
(require 'so-long)
(defvar so-long-minor-modes)
(defvar so-long-target-modes)

;; configure so-long-mode
(use-package "so-long"
  ;; :init
  :config
  (add-to-list 'so-long-minor-modes 'rainbow-delimiters-mode)
  (add-to-list 'so-long-minor-modes 'rainbow-mode)
  ;; (add-to-list 'so-long-minor-modes 'color-identifiers-mode)
  (add-to-list 'so-long-minor-modes 'rainbow-identifiers-mode)
  (add-to-list 'so-long-minor-modes 'flycheck-mode)
  (add-to-list 'so-long-target-modes 'json-mode)
  (so-long-enable)
  (setq-default so-long-threshold 500)
  (message "so-long watching enabled"))

(provide 'init-so-long-mode)
