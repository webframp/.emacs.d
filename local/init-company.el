;;; init-company.el --- customization for company mode
;;; Commentary:

;; settings for company-mode autocomplete framework

;;; Code:

(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-limit 20)                      ; bigger popup
(setq company-minimum-prefix-length 2)               ; autocomplete
(setq company-idle-delay .5)                         ; shorter delay
(setq company-echo-delay 0)                          ; removes blink
(setq company-begin-commands '(self-insert-command)) ; start after typing

(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))

(after-load 'company
  '(add-to-list 'company-backends 'company-inf-ruby)
  '(add-to-list 'company-backends 'company-dabbrev)
  '(add-to-list 'company-backends 'company-ispell)
  '(add-to-list 'company-backends 'company-ghc)
  '(add-to-list 'company-backends 'company-go)
  '(add-to-list 'company-backends 'company-files))

(provide 'init-company)
;;; init-company.el ends here
