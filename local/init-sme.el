;;; init-sme.el --- Sean Escriva - emacs init customizations
;;; Commentary:
;;; Code:

;; don't let libraries ever turn this on
(setq debug-on-error nil)
(setq-default debug-on-error nil)

;; tabs vs spaces
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
(setq-default tab-width 2)
(setq powershell-indent 4)

;; use sRGB colorspace
(when *is-osx*
  (setq ns-use-srgb-colorspace t))

;; side by side diff mode
(setq ediff-split-window-function 'split-window-horizontally)

;; spelling settings
(after-load 'ispell
  (progn
    (setq ispell-dictionary "english"
          ispell-silently-savep t)))

(when *is-windows*
  (add-to-list 'exec-path "C:/src/tools/Aspell/bin/"))

(setq-default ispell-program-name (executable-find "aspell"))
(setenv "ASPELL_CONF" nil)

(defalias 'qrr 'query-replace-regexp)

;; remove a few uneeded decorations
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; ido settings: vertical, fuzzy and show matching
(ido-mode t)
(ido-everywhere t)
(ido-vertical-mode t)
(flx-ido-mode t)
(setq ido-use-faces nil)

;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-completion-system 'ido)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;hate this thing
(after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(custom-set-faces
 '(flycheck-warning
   ((t (:foreground "orange" :underline)))))

;; fix ag mode for windows
(after-load 'ag
  (when *is-windows*
    (setq-default ag-arguments (cons "--line-numbers" ag-arguments))))

;; easy-pg
(setq epg-gpg-program (executable-find "gpg"))

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
(add-to-list 'safe-local-variable-values '(lice:default-license . "apache-2.0"))

;; powerline config
(powerline-default-theme)

;; powershell mode
(when *is-windows*
  (autoload 'powershell-mode "powershell-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.ps.*1$" . powershell-mode)))

;; batch files
(add-to-list 'auto-mode-alist '("\\.bat$" . ntcmd-mode))

;; projectile
(projectile-global-mode)

;; git-gutter
(global-git-gutter-mode t)
(setq git-gutter:lighter " GG")

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; json-mode
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

;; smartparens
(smartparens-global-mode)
(show-smartparens-global-mode t)

(custom-set-faces
 '(sp-pair-overlay-face
   ((t (:background "yellow" :foreground "black"))))
 '(sp-wrap-overlay-face
   ((t (:background "green" :foreground "black"))))
 '(sp-wrap-tag-overlay-face
   ((t (:background "magenta" :foreground "black"))))
 )

;; ido history
(setq ido-save-directory-list-file (concat user-emacs-directory ".ido.last"))

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

(provide 'init-sme)
;;; init-sme.el ends here
