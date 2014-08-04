;;;; init-mail.el --- Custom settings for notmuch and mail handling
;;; Commentary:

;; this configures mail and message-mode

;;; Code:
(autoload 'mailto-compose-mail "mailto-compose-mail" nil t)

;; mail sending
(setq message-send-mail-function 'message-send-mail-with-sendmail)

(when *is-windows*  (setq sendmail-program "c:/src/tools/bin/msmtp.exe"))
(when *is-osx*  (setq sendmail-program "/usr/local/bin/msmtp"))

(setq mail-specify-envelope-from 't)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)

(setq message-kill-buffer-on-exit 't)
(setq mail-host-address "webframp.com")
(setq message-default-mail-headers "Cc: \nBcc: \n")

;; mail viewing and composing
;; fix for possible weirdness with notmuch-show-mode and emacs 24.x
;; still needed?
(require 'gnus-art)

;; support multiple email accounts
(require 'gnus-alias)
(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)

;; Define Identities
;; Define three identities, "home", "work", "jw"
(setq gnus-alias-identity-alist
      '(("personal"
         nil ;; Does not refer to any other identity
         "Sean Escriva <sean.escriva@gmail.com>" ;; Sender address
         nil ;; No organization header
         nil ;; No extra headers
         nil ;; No extra body text
         "~/.signature"
         )
        ("hw"
         nil ;; Not referencing another identity
         "Sean Escriva <sean@hw-ops.com>"
         "Heavywater"
         nil ;; No extra headers
         nil ;; No extra body text
         "~/.signature.hw")
        ("jw"
         nil ;; Doesn't refer to another id
         "Sean Escriva <sescriva@jw.org>"
         nil ;; No org header
         nil ;; No extra headers
         nil ;; No extra body text
         "~/.signature.jw")
        ))

;; Use "jw" identity by default
(setq gnus-alias-default-identity "jw")
;; Define rules to match work identity
(setq gnus-alias-identity-rules
      '(("hw" ("any" "<\\(.+\\)\\@hw-ops\\.com" both) "hw")
        ("jw" ("any" "<\\(.+\\)\\@jw\\.org" both) "jw")))

(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

(setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-20,20n%]%) (%c) %s\n")

;;(setq gnus-use-cache t)
;; (when *is-windows*
;;   (setq gnus-cache-directory "c:/src/_mail/cache/")
;;   (setq gnus-cache-directory "~/.mail/cache/"))
;; (setq gnus-cache-enter-articles '(ticked dormant read unread))
;; (setq gnus-cache-remove-articles nil)
;; (setq gnus-cacheable-groups "^nnimap")

(require 'gnus-desktop-notify)
(gnus-desktop-notify-mode)
(gnus-demon-add-handler 'gnus-demon-scan-mail 15 1)

(provide 'init-mail)
;;; init-mail.el ends here
