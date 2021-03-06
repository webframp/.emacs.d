;; -*- coding: utf-8;  no-byte-compile: t -*-
;;; init.el --- Sean Escriva - emacs init customizations
;;; Commentary:
;;   Config using Cask and Pallet for package management

;;; Code:
(defconst *is-osx* (eq system-type 'darwin))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))

(require 'cl)
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(when *is-windows*
  (setq url-proxy-services '(("http" . "localhost:3128"))))

;; add local config files to path
(add-to-list 'load-path (concat user-emacs-directory "local"))

;; startup profiling
(require 'init-benchmarking)

;; more portable init, instead of username dir based starter-kit
(require 'user-defuns)
(require 'init-themes)
(require 'init-sme)
(require 'init-bindings)
(require 'init-company)
(require 'init-ruby)
(require 'init-org)
(when *is-windows* (require 'init-gnus))
(when *is-osx*
  (require 'init-osx)
  (require 'init-notmuch)
  )

(message "init completed in %.2fms"
         (sme/time-subtract-millis (current-time) before-init-time))

(provide 'init)
;;; init.el ends here
