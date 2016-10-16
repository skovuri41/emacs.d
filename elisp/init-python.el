(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-hook 'python-mode-hook
            (lambda () (aggressive-indent-mode -1)))
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4))))

(use-package elpy
  :ensure t
  :init (elpy-enable))

(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(use-package company-anaconda
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends
                 '(company-anaconda :with company-capf))))

(use-package pip-requirements
  :defer t
  :init
  (progn
    ;; company support
    (push 'company-capf company-backends-pip-requirements-mode)))
