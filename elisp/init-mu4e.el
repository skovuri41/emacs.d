(use-package mu4e
  :commands (mu4e)
  :config
  (progn
    (require 'mu4e-contrib)
    (setq mu4e-attachment-dir "~/Downloads")
    (setq mu4e-mu-binary "/usr/local/bin/mu")
    (setq mu4e-maildir (expand-file-name "~/Mail"))
    (setq mu4e-get-mail-command "offlineimap")
    ;; (setq mu4e-get-mail-command "offlineimap -q")
    (setq mu4e-update-interval 300)
    (setq mu4e-sent-messages-behavior 'delete)
    (setq mu4e-html2text-command "w3m -dump -T text/html")
    (setq mu4e-view-prefer-html t)
    (setq mu4e-use-fancy-chars t)
    (setq mu4e-headers-skip-duplicates t)
    (setq mu4e-view-show-images t)
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

    ;; Vimify Key Bindings
    (with-eval-after-load "mu4e"
      (evil-make-overriding-map mu4e-main-mode-map 'normal t)
      (evil-define-key 'normal mu4e-main-mode-map
        "<SPC>" nil
        "j" nil
        "J" 'mu4e~headers-jump-to-maildir
        "/" 'mu4e-headers-search)

      (evil-make-overriding-map mu4e-headers-mode-map 'normal t)
      (evil-define-key 'normal mu4e-headers-mode-map
        "J" 'mu4e~headers-jump-to-maildir
        "j" 'evil-next-line
        "k" 'evil-previous-line)

      (evil-make-overriding-map mu4e-view-mode-map 'normal t)
      (evil-define-key 'normal mu4e-view-mode-map
        "j" 'evil-next-line
        "k" 'evil-previous-line))
    (evil-set-initial-state 'mu4e-mode 'normal)
    (evil-set-initial-state 'mu4e-main-mode 'normal)
    (evil-set-initial-state 'mu4e-headers-mode 'normal)
    (evil-set-initial-state 'mu4e-view-mode 'normal)
    (evil-set-initial-state 'mu4e-compose-mode 'normal)))

(provide 'init-mu4e)
