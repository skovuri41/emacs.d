(use-package mu4e
  :commands (mu4e)
  :config
  (progn
    (require 'mu4e-contrib)
    ;; (require 'w3m)
    (setq mu4e-attachment-dir "~/Downloads")
    (setq mu4e-mu-binary "/usr/local/bin/mu")
    (setq mu4e-maildir (expand-file-name "~/Mail"))
    (setq mu4e-get-mail-command "offlineimap")
    ;; (setq mu4e-get-mail-command "offlineimap -q")
    ;; allow for updating mail using 'U' in the main view:
    ;; (setq mu4e-get-mail-command "offlineimap -q -u Basic")
    ;; (setq mu4e-update-interval 300)
    (setq mu4e-sent-messages-behavior 'delete)

    ;; (setq mu4e-html2text-command "w3m -dump -T text/html")
    ;; (setq mu4e-view-prefer-html t)
    ;; Html rendering
    (setq mu4e-view-prefer-html t)
    (setq mu4e-html2text-command (cond ((fboundp 'w3m)
                                        (lambda ()          ; Use emacs-w3m
                                          (w3m-region (point-min) (point-max))))
                                       ((executable-find "w3m")
                                        "w3m -T text/html") ; Use w3m shell-command
                                       (t (lambda ()        ; Use shr (slow)
                                            (let ((shr-color-visible-luminance-min 75)
                                                  shr-width)
                                              (shr-render-region (point-min) (point-max)))))))

    (setq mu4e-use-fancy-chars t)
    (setq mu4e-headers-skip-duplicates t)
    ;; (setq mu4e-view-show-images t)
    ;; Attempt to show images when viewing messages
    (setq mu4e-view-show-images t
          mu4e-view-image-max-width 800)
    (setq message-kill-buffer-on-exit t)
    (setq mu4e-hide-index-messages t)
    (setq mu4e-change-filenames-when-moving t)
    (setq mu4e-completing-read-function 'ivy-completing-read)
    (setq mu4e-context-policy 'pick-first)
    (setq mu4e-compose-context-policy 'ask)
    (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

    (setq mu4e-maildir-shortcuts
          '(("/INBOX"             . ?i)
            ("/[Gmail].Sent Mail" . ?s)
            ("/[Gmail].Trash"     . ?t)))

    (setq mu4e-headers-fields
          '( (:date          .  25)    ;; alternatively, use :human-date
             (:flags         .   6)
             (:from          .  22)
             (:subject       .  nil))) ;; alternatively, use :thread-subject

    (setq mu4e-user-mail-address-list
          (delq nil
                (mapcar (lambda (context)
                          (when (mu4e-context-vars context)
                            (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
                        mu4e-contexts)))
    (add-to-list 'mu4e-view-actions
                 '("ViewInBrowser" . mu4e-action-view-in-browser) t)
;;; Decorate mu main view
    (defun mu4e-main-mode-font-lock-rules ()
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\[\\([a-zA-Z]\\{1,2\\}\\)\\]" nil t)
          (add-text-properties (match-beginning 1) (match-end 1) '(face font-lock-variable-name-face)))))
    (add-hook 'mu4e-main-mode-hook 'mu4e-main-mode-font-lock-rules)


    (use-package evil-mu4e
      :ensure t
      :init
      (require 'evil-mu4e)
      ;; :config
      ;; (evil-set-initial-state 'mu4e-mode 'normal)
      ;; (evil-set-initial-state 'mu4e-main-mode 'normal)
      ;; (evil-set-initial-state 'mu4e-headers-mode 'normal)
      ;; (evil-set-initial-state 'mu4e-view-mode 'normal)
      ;; (evil-set-initial-state 'mu4e-compose-mode 'normal)
      )
    ))

(provide 'init-mu4e)
