;;; init.el --- Sean Escriva - emacs init customizations
;;; Commentary:
;;   Config using Cask and Pallet for package management

;;; Code:
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; add local config files to path
;; more portable init, instead of username dir based starter-kit
(defconst *is-a-mac* (eq system-type 'darwin))
(add-to-list 'load-path (concat user-emacs-directory "local"))

;; startup profiling
(require 'init-benchmarking)

;; my customizations
(require 'init-themes)
(require 'init-sme)
(require 'init-bindings)
(require 'init-company)
(require 'init-ruby)
(require 'org-config)
(require 'notmuch-config)
(require 'user-functions)
(require 'chef-mode)

(when *is-a-mac*
  (require 'init-osx))

;; remove a few uneeded decorations
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; company mode for auto complete
(add-hook 'after-init-hook 'global-company-mode)

;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-completion-system 'ido)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

;; ido settings: vertical, fuzzy and show matching
(ido-mode t)
(ido-everywhere t)
(ido-vertical-mode t)
(flx-ido-mode t)
(setq ido-use-faces nil)

(message "init completed in %.2fms"
         (sme/time-subtract-millis (current-time) before-init-time))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t

(provide 'init)
;;; init.el ends here
