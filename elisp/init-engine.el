(use-package engine-mode
  :ensure t
  :config
  (progn
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
    (defengine google-images
      "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
    (defengine google-maps
      "http://maps.google.com/maps?q=%s"
      :docstring "Mappin' it up.")
    (defengine word
      "http://wordnik.com/words/%s"
      :term-transformation-hook downcase
      :keybinding "d")
    (defengine duckduckgo
      "https://duckduckgo.com/html/?q=%s"
      :keybinding "k")
    
    (eval-after-load 'hydra
      `(defhydra hydra-engine (:color blue)
         ("w" engine/search-wikipedia     "wikipedia")
         ("k" engine/search-duckduckgo    "duckduckgo")
         ("w" engine/search-word          "word")
         ("h" engine/search-github        "github")
         ("s" engine/search-stackoverflow "stackoverflow")
         ("g" engine/search-google        "google")
         ("i" engine/search-google-images "google-images")
         ("m" engine/search-google-maps   "google-maps")))
    
    (use-package osx-browse
      :ensure t
      :if (eq system-type 'darwin)
      :config
      (osx-browse-mode 1))

    ;; (setq engine/browser-function 'eww-browse-url)
    
    ))
(provide 'init-engine)
