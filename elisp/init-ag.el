(use-package ag
  :ensure ag
  :commands (ag ag-mode ag-files ag-regexp)
  :init
  (progn
    (setq ag-highlight-search t)
    (setq ag-reuse-buffers t)
    (defun setup-ag ()
      "Function called to set my ag stuff up."
      (toggle-truncate-lines t)
      (linum-mode 0)
      (switch-to-buffer-other-window "*ag search*")
      )
    (add-hook 'ag-mode-hook 'setup-ag)
    (after 'evil
      (evil-set-initial-state 'ag-mode 'normal)
      ;; unbind kill window
      )
    )
  :config
  (progn
    (define-key ag-mode-map (kbd "k") 'nil)
    (evil-define-key 'normal ag-mode-map (kbd "k") 'nil)
    (evil-leader/set-key
      "agg" 'ag
      "agf" 'ag-files
      "agr" 'ag-regexp
      "agp" 'ag-project
      "agP" 'ag-project-files
      "agR" 'ag-project-regexp
      "agd" 'ag-project-dired
      "agD" 'ag-project-dired-regexp
      "agk" 'ag-kill-buffers
      )
    (add-hook 'ag-mode-hook
              (lambda ()
                (define-key ag-mode-map "j" 'evil-next-line)
                (define-key ag-mode-map "k" 'evil-previous-line)
                (define-key ag-mode-map "n" 'evil-ex-search-next)
                (define-key ag-mode-map "N" 'evil-ex-search-previous)))))
(provide 'init-ag)
