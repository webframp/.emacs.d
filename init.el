; -*- coding: utf-8;  no-byte-compile: t -*-
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
(require 'user-functions)
(require 'init-themes)
(require 'init-sme)
(require 'init-bindings)
(require 'init-company)
(require 'init-ruby)
(require 'init-org)
(require 'init-notmuch)

(require 'chef-mode)

(when *is-a-mac*
  (require 'init-osx))

(message "init completed in %.2fms"
         (sme/time-subtract-millis (current-time) before-init-time))

(provide 'init)
;;; init.el ends here
