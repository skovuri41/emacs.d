;; Weather Forecast

;; Sunshine
;; https://github.com/aaronbieber/sunshine.el
(use-package sunshine
  :commands (sunshine-quick-forecast
             sunshine-forecast)
  :config
  (progn
    ;; The "openweathermap-api" file is supposed to contain this line:
    ;;     (setq sunshine-appid "<YOUR_API>")
    ;; Sign up at http://openweathermap.org/ to get your API KEY.
    ;; (load (locate-user-emacs-file "openweathermap-api") :noerror :nomessage)
    (setq sunshine-location "07751,USA")
    (setq sunshine-show-icons t)))

;; Forecast
;; https://github.com/cadadr/forecast.el
(use-package forecast
  ;; deferring not needed as the package is set to autoload on M-x forecast
  ;; :defer 1 ; Wait for at least a second after emacs has loaded.
  ;;          ; The emacs frame needs to be set up properly before `find-font' call.
  :commands (forecast)
  :config
  (progn
    ;; Use Quivira font for moon phases
    (when (find-font (font-spec :name "Quivira"))
      (set-face-attribute 'forecast-moon-phase nil :font "Quivira"))

    (set-face-attribute 'forecast-upcoming-temperature nil
                        :inherit font-lock-function-name-face)

    ;; The "forecast-api" file is supposed to contain this line:
    ;;     (setq forecast-api-key "<YOUR_API>")
    ;; Register at https://developer.forecast.io/ to get your API KEY.
    ;; (load (locate-user-emacs-file "forecast-api") :noerror :nomessage)
    (setq forecast-latitude  40.3616)
    (setq forecast-longitude  -74.2630)
    (setq forecast-city      "Morganville, NJ")
    (setq forecast-country   "USA")
    (setq forecast-units     'us)))

(use-package wttrin
  :ensure t
  :commands (wttrin wttrin-query)
  :init
  (setq wttrin-default-cities '("hyd"
                                "07751"
                                "newyork"
                                "london")))

(use-package yahoo-weather
  :if *is-a-mac*
  :ensure t
  :defer 5
  :config
  (validate-setq yahoo-weather-location "07751")
  ;; (validate-setq yahoo-weather-location "Hyderabad,India")
  (validate-setq yahoo-weather-use-F t)
  (validate-setq yahoo-weather-format "")
  (validate-setq yahoo-weather-mode-line nil)
  (yahoo-weather-mode))


(provide 'init-weather)
