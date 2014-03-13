;;;; init-notmuch.el --- Custom settings for notmuch and mail handling
;;; Commentary:

;; this configures both notmuch and message-mode

;;; Code:
(autoload 'mailto-compose-mail "mailto-compose-mail" nil t)

;; mail sending
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtp")
(setq mail-specify-envelope-from 't)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)

(setq message-kill-buffer-on-exit 't)
(setq mail-host-address "webframp.com")
(setq message-default-mail-headers "Cc: \nBcc: \n")

;; mail contacts
(require 'notmuch-address)
(setq notmuch-address-command "~/bin/notmuch-contacts.sh")
(notmuch-address-message-insinuate)

(setq notmuch-poll-script "~/bin/notmuch-touch")

;; mail folders
(setq notmuch-saved-searches '(("unread" .  "tag:unread and not (tag:lists or tag:to-read or tag:notifications)")
                               ("chef" . "tag:chef and tag:unread and not tag:github")
                               ("chef-dev" . "tag:chef-dev and tag:unread and not tag:github")
                               ("work" . "tag:work and tag:unread")
                               ("to-me" . "tag:to-me and tag:unread")
                               ("to-read" . "tag:to-read and tag:unread")
                               ("org-mode" . "tag:org-mode and tag:unread")
                               ("org-trello" . "tag:org-trello and tag:unread")
                               ("devops" . "tag:devops and tag:unread")
                               ("devops-toolchain" . "tag:devops-toolchain and tag:unread")
                               ("pdxruby" . "tag:pdxruby and tag:unread")
                               ("pdxdevops" . "tag:pdxdevops and tag:unread")
                               ("pdxfunc" . "tag:pdxfunc and tag:unread")
                               ("pdxpython" . "tag:pdxpython and tag:unread")
                               ("ptp-general" . "tag:ptp-general and tag:unread")
                               ("ptp-ops" . "tag:ptp-ops and tag:unread")
                               ("arch-announce" . "tag:arch-announce and tag:unread")
                               ("arch-haskell" . "tag:arch-haskell and tag:unread")
                               ("sole-flounder" . "tag:sole-flounder and tag:unread")
                               ("work-today" . "tag:work and date:today..today")
                               ("xmonad" . "tag:xmonad and tag:unread")))

(setq notmuch-archive-tags '("-unread" "+archive"))

(setq message-directory "~/.mail")
(setq notmuch-fcc-dirs '(("sean@hw-ops.com" . "hw/sent")
                         ("sean.escriva@gmail.com" . "gmail/sent")
                         (".*" . "sent")))

;; mail viewing and composing
;; fix for possible weirdness with notmuch-show-mode and emacs 24.x
;; still needed?
(require 'gnus-art)

;; support multiple email accounts
(require 'gnus-alias)
(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)

;; Define Identities
;; Define two identities, "home" and "work"
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
         "~/.signature.hw")))

;; Use "personal" identity by default
(setq gnus-alias-default-identity "personal")
;; Define rules to match work identity
(setq gnus-alias-identity-rules
      '(
        ("hw"
         ("any" "<\\(.+\\)\\@hw-ops\\.com" both) "hw")
        )
      )

(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

(provide 'init-notmuch)
;;; init-notmuch.el ends here
