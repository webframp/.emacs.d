;;;; init-mail.el --- Custom settings for notmuch and mail handling
;;; Commentary:

;; this configures mail and message-mode

;;; Code:
(autoload 'mailto-compose-mail "mailto-compose-mail" nil t)

;; mail sending
(setq message-send-mail-function 'message-send-mail-with-sendmail)

(when *is-windows*  (setq sendmail-program "c:/src/tools/bin/msmtp.exe"))
(when *is-osx*  (setq sendmail-program "/usr/local/bin/msmtp"))
(setq message-directory "~/.mail")
(setq mail-specify-envelope-from 't)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)

(setq message-kill-buffer-on-exit 't)
(setq mail-host-address "webframp.com")
(setq message-default-mail-headers "Cc: \nBcc: \n")

(add-hook 'message-mode-hook 'flyspell-mode)

(provide 'init-mail)
;;; init-mail.el ends here
