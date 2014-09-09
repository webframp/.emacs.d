;;; init-bindings.el --- Sean Escriva - emacs init customizations
;;; Commentary:
;;; Code:

;; custom keybindings
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

(global-set-key "\C-xj" 'eval-print-last-sexp)
(global-set-key "\C-x\C-j" 'eval-print-last-sexp)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; basically esk
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-c n") 'esk-cleanup-buffer)

;; search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

 ;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'esk-eval-and-replace)
(global-set-key (kbd "C-c v") 'eval-buffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; So good!
(global-set-key (kbd "C-c g") 'magit-status)

;; org-mode bindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; deft
(global-set-key (kbd "C-x z") 'deft)
(global-set-key (kbd "C-x Z") 'deft-new-file)

;; custom bindings for functions defined in user.el
(global-set-key (kbd "C-c t") 'rotate-windows)
(global-set-key (kbd "C-c C-r") 'rename-file-and-buffer)
(global-set-key (kbd "C-x TAB") 'indent-rigidly)

;; rebind mail, since I don't use eshell
(global-set-key (kbd "C-x m") 'compose-mail)

;; rebind ruby-tools functions
(global-set-key (kbd "C-c '") 'ruby-tools-to-single-quote-string)
(global-set-key (kbd "C-c \"") 'ruby-tools-to-double-quote-string)
(global-set-key (kbd "C-c :") 'ruby-tools-to-symbol)
(global-set-key (kbd "C-c ;") 'ruby-tools-clear-string)

;; gitgutter
(global-set-key (kbd "C-c s s") 'git-gutter:stage-hunk)
(global-set-key (kbd "C-c s r") 'git-gutter:revert-hunk)

;; magit
(global-set-key (kbd "C-c l") 'magit-log)

;; haskell binds
(add-hook 'haskell-mode-hook (lambda ()
                               (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                               (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
                               (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                               (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                               (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                               (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
                               (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(add-hook 'cabal-mode-hook (lambda ()
                             (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
                             (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
                             (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                             (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;; smartparens
(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
(define-key sp-keymap (kbd "C-M-[") 'sp-select-previous-thing)

(provide 'init-bindings)
;;; init-bindings.el ends here
