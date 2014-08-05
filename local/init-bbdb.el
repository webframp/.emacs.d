;;; bbdb
(require 'bbdb)
;;(require 'bbdb-autoloads)
(bbdb-initialize 'gnus 'message) ;;'reportmail)
(load "bbdb-com" t)
(bbdb-insinuate-message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; basics
(setq
 bbdb-file "~/.bbdb"
 bbdb-offer-save 'auto
 bbdb-notice-auto-save-file t
 bbdb-expand-mail-aliases t
 bbdb-canonicalize-redundant-nets-p t
 bbdb-always-add-addresses t
 bbdb-complete-name-allow-cycling t
 )


;; extra
;;(bbdb-insinuate-reportmail)
(setq bbdb-north-american-phone-numbers nil)
(setq bbdb-auto-notes-ignore (quote (("Organization" . "^Gatewayed from\\\\|^Source only"))))
(setq bbdb-auto-notes-ignore-all nil)
(setq bbdb-check-zip-codes-p nil)
(setq bbdb-default-area-code 718)
(setq bbdb-default-country "United States")
(setq bbdb-notice-hook (quote (bbdb-auto-notes-hook)))
(setq bbdb/mail-auto-create-p t)
(setq bbdb/news-auto-create-p (quote bbdb-ignore-some-messages-hook))

;; (setq bbdb-auto-notes-alist
;;       (quote (("To"
;;                ("w3o" . "w3o")
;;                ("plug" . "plug")
;;                ("linux" . "linux")
;;                ("emacs-commit" . "emacs commit")
;;                ("emacs" . "emacs")
;;                ("pinoyjug" . "pinoyjug")
;;                ("digitalfilipino" . "digitalfilipino")
;;                ("sacha" . "personal mail"))
;;               ("From"
;;                ("admu" company "Ateneo de Manila University")
;;                ("Organization" (".*" company 0 nil))
;;                ))))
