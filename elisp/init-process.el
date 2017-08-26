;;; init-process.el --- Process support.

;;; Commentary:
;; Process helpers.


;;; Code:

(require 'cl)

(defun my/process-call (program &rest arguments)
  "Call PROGRAM with ARGUMENTS.  Look out for *PROGRAM* buffer output."
  (apply #'call-process (append (list program
                                      nil
                                      (format "*%s*"
                                              (file-name-nondirectory program))
                                      nil)
                                arguments)))

(defun my/process-binary-installed-p (binary-name)
  "Return t if BINARY-NAME is found in PATH.  Nil otherwise."
  (> (length (shell-command-to-string (format "which %s"
                                              binary-name)))
     0))

(defun my/process-assert-binary-installed (binary-name &optional install-message)
  "Assert BINARY-NAME is in PATH.  Show INSTALL-MESSAGE for instructions."
  (assert (my/process-binary-installed-p binary-name) nil (concat (format "%s not found. "
                                                                          binary-name)
                                                                  install-message)))

;; eg usage:
;; (my/process-call "mogrify" "-auto-orient" (buffer-file-name))
;; (my/process-assert-binary-installed "mogrify" "Install with: brew install imagemagick or apt-get imagemagick")

(provide 'init-process)

;;; init-process.el ends here
