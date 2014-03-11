;;; init-sme.el --- Sean Escriva - emacs init customizations
;;; Commentary:
;;; Code:

;; Thanks https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el#L1
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defun set-exec-path-from-shell-PATH ()
  "Make sure our exe path is set correctly."
  (let ((path-from-shell
         (replace-regexp-in-string "[[:space:]\n]*$" ""
                                   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

;; don't clutter fs with backups and set some options
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))

(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".yauto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "o" 'open-with)))

;; some file associations
(setq auto-mode-alist (cons '("\\.bat$" . ntcmd-mode) auto-mode-alist))

;; side by side diff mode
(setq ediff-split-window-function 'split-window-horizontally)

;; spelling settings
(eval-after-load "ispell"
  (progn
    (setq ispell-dictionary "en"
          ispell-silently-savep t)))
(if (eq system-type 'gnu/linux)
    (setq-default ispell-program-name "/usr/bin/aspell")
  (setq-default ispell-program-name "/usr/local/bin/aspell"))

(setenv "ASPELL_CONF" nil)

(defalias 'qrr 'query-replace-regexp)

;; easy-pg
(if (eq system-type 'gnu/linux)
    (setq epg-gpg-program "/usr/bin/gpg")
  (setq epg-gpg-program "/usr/local/bin/gpg"))

;; ghc-mod
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) )) ;; REMOVE FLYMAKE?
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; go-mode
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

;; dash-at-point
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)

;; ghc
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; lice - auto license heades
(setq lice:default-license "apache-2.0")

;; powerline config
(powerline-default-theme)

(provide 'init-sme)
;;; inti-sme.el ends here
