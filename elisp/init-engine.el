(use-package engine-mode
  :ensure t
  :config
  (progn
    (engine-mode 1)
    (defengine google
      "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
      :keybinding "g")
    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      :keybinding "t")
    (defengine bing
      "http://www.bing.com/search?q=%s"
      :keybinding "b")
    (defengine stackoverflow
      "http://stackoverflow.com/search?q=%s"
      :keybinding "s")
    (defengine wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      :keybinding "w")
    
    (use-package osx-browse
      :ensure t
      :if (eq system-type 'darwin)
      :config
      (osx-browse-mode 1))

    ;; (setq engine/browser-function 'eww-browse-url)
    
    ))
(provide 'init-engine)
