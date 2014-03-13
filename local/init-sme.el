;;; init-sme.el --- Sean Escriva - emacs init customizations
;;; Commentary:
;;; Code:

;; use sRGB colorspace
(when *is-a-mac*
  (setq ns-use-srgb-colorspace t))

;; some file associations
(setq auto-mode-alist (cons '("\\.bat$" . ntcmd-mode) auto-mode-alist))

;; side by side diff mode
(setq ediff-split-window-function 'split-window-horizontally)

;; spelling settings
(after-load 'ispell
  (progn
    (setq ispell-dictionary "en"
          ispell-silently-savep t)))
(if (eq system-type 'gnu/linux)
    (setq-default ispell-program-name "/usr/bin/aspell")
  (setq-default ispell-program-name "/usr/local/bin/aspell"))

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
(add-to-list 'safe-local-variable-values '(lice:default-license . "apache-2.0"))

;; powerline config
(powerline-default-theme)

;; projectile
(projectile-global-mode)

;; git-gutter
(global-git-gutter-mode t)
(set-face-foreground 'git-gutter:added "blue")
(set-face-foreground 'git-gutter:deleted "yellow")
(setq git-gutter:lighter " GG")

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
