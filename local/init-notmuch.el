;;;; init-notmuch.el --- Custom settings for notmuch and mail handling
;;; Commentary:

;; notmuch specific configuration

;;; Code:
;; load general mail settings
(require 'init-mail)

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


(setq notmuch-fcc-dirs '(("sean@hw-ops.com" . "hw/sent")
                         ("sean.escriva@gmail.com" . "gmail/sent")
                         (".*" . "sent")))

(provide 'init-notmuch)
;;; init-notmuch.el ends here
