(setq ditaa-jar-path (expand-file-name "extra/ditaa0_9.jar" user-emacs-directory))
(setq org-ditaa-jar-path (expand-file-name "extra/ditaa0_9.jar" user-emacs-directory))
(setq plantuml-jar-path (expand-file-name "extra/plantuml.jar" user-emacs-directory))
(setq org-plantuml-jar-path (expand-file-name "extra/plantuml.jar" user-emacs-directory))

(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (shut-up (load local-config))))

(provide 'init-local)

