;;; package -- init-ruby.el - ruby specific settings
;;; Commentary:
;;; Code:

(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'robe-mode)

(add-to-list 'auto-mode-alist '("Gemfile.*" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile.*" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile.*" . ruby-mode))

;; https://github.com/technomancy/emacs-starter-kit/pull/145
;;(defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

;; Chris' ruby indent
(setq ruby-deep-indent-paren nil)

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

(provide 'init-ruby)
;;; init-ruby.el ends here
