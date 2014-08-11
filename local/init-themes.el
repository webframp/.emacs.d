;;; package --- color theme setup
;;; Commentary:
;;; Code:

(when *is-windows*
  (load-theme 'base16-solarized t))

;; use tomorrow theme on osx
(when *is-osx*
  (load-theme 'base16-tomorrow t))

(provide'init-themes)

;;; init-themes.el ends here
