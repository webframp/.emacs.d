;;; org-config.el --- org mode settings
;;; Commentary:
;;  Various local settings for org-mode

;;; Code:

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "todo.org"))
(setq org-mobile-inbox-for-pull "~/org/inbox.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-src-fontify-natively t)
(setq org-log-done t)
(setq org-export-backends '(md odt ascii html icalendar latex))

(setq org-agenda-files (list "~/org/todo.org"
                             "~/org/inbox.org"
                             "~/org/theocratic.org"
                             "~/org/work.org"))

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

(setq deft-directory org-directory
      deft-extension "org"
      deft-text-mode 'org-mode
      deft-use-filename-as-title t)

(require 'org-odt)
(require 'ox-reveal)

;;; org-babel
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")

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
;; FIX a lot of boilerplate here
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
        ("tr" "RBC SDP" entry (file+headline (concat org-directory "theocratic.org") "RBCSDP")
         "* %^{Description} %^g
 %?
Added: %U")
        ("tb" "Bethel" entry (file+headline (concat org-directory "theocratic.org") "Bethel")
         "* %^{Description}  %^g
 %?
Added: %U")
        ("tq" "Quick Draw" entry (file+headline (concat org-directory "theocratic.org") "Quick Draw")
         "* %^{Description}  %^g
 %?
Added: %U")
        ("tw" "Woodburn" entry (file+headline (concat org-directory "theocratic.org") "Woodburn")
         "* %^{Description}  %^g
 %?
Added: %U")

        ("w" "Templates for work or client related items") ;; two
        ("wa" "Analog Analtics" entry (file+headline (concat org-directory "work.org") "Analog") "* %^{Description} %^g
 %?
 Added: %U")
        ("wb" "AboutUs" entry (file+headline (concat org-directory "work.org") "AboutUs") "* %^{Description} %^g
 %?
 Added: %U")
        ("wg" "General work items" entry (file+headline (concat org-directory "work.org") "General") "* %^{Description} %^g
 %?
 Added: %U")
        ("wp" "Product" entry (file+headline (concat org-directory "work.org") "Product") "* %^{Description} %^g
 %?
 Added: %U")
        ("ws" "SweetSpot" entry (file+headline (concat org-directory "work.org") "Sweetspot") "* %^{Description} %^g
 %?
 Added: %U")

        ("c" "Coffee related") ;; three
        ("ca" "Aeropress" entry (file+headline (concat org-directory "coffee.org") "Aeropress") "** %^{Heading}
  - grinder: %^{grinder setting}
  - water: %^{water amount}
  - coffee: %^{coffee amout}
  - steep: %^{steep time}
  - press: %^{press time}")
        ("cc" "Chemex" entry (file+headline (concat org-directory "coffee.org") "Chemex"))
        ("cf" "French Press" entry (file+headline (concat org-directory "coffee.org") "French Press"))
        ))

(provide 'org-config)
;;; org-config.el ends here
