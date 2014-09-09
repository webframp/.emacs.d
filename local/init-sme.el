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

(setq visible-bell t
      screen-vertical-padding 0
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

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

;;; haskell setup
;; cabal path
;; make sure cabal/bin is in path
(setenv "PATH" (concat "~/.cabal/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.cabal/bin")

;; haskell indent
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; hasktags
(setq haskell-tags-on-save 't)
(add-hook 'haskell-mode-hook
          (lambda () (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)))

;; inf-haskell
(setq haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-process-type 'cabal-repl)

;; ghc-mod
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(setq company-ghc-show-info t)

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
(add-to-list 'auto-mode-alist '("\\.arclint$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.arcconfig$" . json-mode))

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

;; smartparens
(smartparens-global-mode)
(show-smartparens-global-mode t)
(require 'smartparens-config)
(sp-local-pair '(message-mode org-mode git-commit-mode) "`" "'")

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

;; dired
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "o" 'open-with)))

(require 'dired-efap)
(define-key dired-mode-map [f2] 'dired-efap)

;; python
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; Seed the random-number generator
(random t)

;; try to keep init.el clean
(setq custom-file (concat user-emacs-directory ".custom.el"))
(when (not (file-exists-p custom-file))
  (write-file custom-file))
(load custom-file)

(provide 'init-sme)
;;; init-sme.el ends here
