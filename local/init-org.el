;;; init-org.el --- org mode settings
;;; Commentary:
;;  Various local settings for org-mode

;;; Code:

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "todo.org"))
(setq org-mobile-inbox-for-pull "~/org/inbox.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-src-fontify-natively t)
(setq org-log-done t)
(setq org-export-backends '(md ascii html icalendar latex))

(setq org-agenda-files (list "~/todo.org"
                             "~/links.org"))

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

(setq deft-directory org-directory
      deft-extension "org"
      deft-text-mode 'org-mode
      deft-use-filename-as-title t)

(require 'ox-reveal)

;; org-contacts
(require 'org-contacts)
(setq
 org-contacts-files (list "~/org/contacts.org")
 org-contacts-birthday-property "ANNIVERSARY"
 org-contacts-birthday-format "Anniversary: %l (%Y)")

;;; org-babel
(when *is-osx*
  (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar"))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (latex . t))))

(setq org-confirm-babel-evaluate nil) ; can be dangerous

;;;; org-mobile
(setq org-mobile-use-encryption nil)
(setq org-mobile-encryption-password nil)

;;;; org-open
(add-hook 'org-mode-hook
          '(lambda ()
             (setq org-file-apps
                   (append '(
                             ("\\.png\\'" . default)
                             ) org-file-apps ))))


;;;; org-capture
(setq org-capture-templates
      `(
        ("a" "Log Activity" entry (file+datetree (concat org-directory "timelog.org")) "** %U - %^{Activity} :TIME:")
        ("k" "Task" entry (file+headline org-default-notes-file "Tasks"))
        ("l" "Reading List" item (file+headline org-default-notes-file "Links") "- [[%^{URL}][%^{Description}]]")
        ("n" "Note item" entry (file+headline org-default-notes-file "Notes")
         "* %^{Description} %^g
 %?")
        ("s" "Shopping List" checkitem (file+headline org-default-notes-file "Shopping List"))

        ;; Templates with a prefix key
        ("t" "Template for Theocratic items") ;; one
        ("ta" "Meeting Assignments" entry (file+headline (concat org-directory "theocratic.org") "Assignments")
         "* %^{Meeting Part} %^t")
        ("tb" "Bethel" entry (file+headline (concat org-directory "theocratic.org") "Bethel")
         "* %^{Description}  %^g
 %?
Added: %U")
        ("c" "Add Contact" entry (file "~/org/contacts.org")
         "* %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(org-contacts-template-email)
  :PHONE:
  :ALIAS:
  :NICKNAME:
  :IGNORE:
  :ICON:
  :NOTE:
  :ADDRESS:
  :ANNIVERSARY:
  :END:")))

;;       ("c" "Coffee related") ;; three
;;       ("ca" "Aeropress" entry (file+headline (concat org-directory "coffee.org") "Aeropress") "** %^{Heading}
;; - grinder: %^{grinder setting}
;; - water: %^{water amount}
;; - coffee: %^{coffee amout}
;; - steep: %^{steep time}
;; - press: %^{press time}")
;;       ("cc" "Chemex" entry (file+headline (concat org-directory "coffee.org") "Chemex"))
;;       ("cf" "French Press" entry (file+headline (concat org-directory "coffee.org") "French Press"))

(provide 'init-org)
;;; init-org.el ends here
