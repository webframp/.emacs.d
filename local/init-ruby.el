;;; package -- init-ruby.el - ruby specific settings
;;; Commentary:
;;; Code:

(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'robe-mode)

(add-to-list 'auto-mode-alist '("Gemfile.*" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile.*" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile.*" . ruby-mode))

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

(unless *is-windows*
  (require 'chruby)
  (chruby "ruby-2.1.1"))

(require 'bundler)

;;;###autoload
(defun run-bundled-command (cmd &rest args)
  "Run bundle exec for the given command, optionally with args"
  (interactive)
  (let (command)
    (setq command
          (if args
              (concat "bundle exec " cmd " "(mapconcat 'identity args " "))
            (concat "bundle exec " cmd)))
    (bundle-command command)))

(defun bundle-exec (command)
  (interactive "sBundle Exec: ")
  (run-bundled-command command))

(provide 'init-ruby)
;;; init-ruby.el ends here
