;;; init-bindings.el --- Sean Escriva - emacs init customizations
;;; Commentary:
;;; Code:

;; custom keybindings
(global-set-key (kbd "M-z") 'undo)
(global-set-key [f5] 'split-window-horizontally)
(global-set-key [f6] 'split-window-vertically)
(global-set-key (kbd "C-c J") 'join-line)

(global-set-key (kbd "C-x c") 'comment-or-uncomment-region)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; org-mode bindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; deft
(global-set-key (kbd "C-x z") 'deft)
(global-set-key (kbd "C-x Z") 'deft-new-file)

;; custom bindings for functions defined in user.el
(global-set-key (kbd "C-c t") 'rotate-windows)
(global-set-key "\C-c\C-r" 'rename-file-and-buffer)
(global-set-key (kbd "C-x TAB") 'indent-rigidly)

;; rebind mail, since I don't use eshell
(global-set-key (kbd "C-x m") 'compose-mail)

;; sunrise
(global-set-key (kbd "C-c x") 'sunrise)
(global-set-key (kbd "C-c X") 'sunrise-cd)

;; rebind ruby-tools functions
(global-set-key (kbd "C-c '") 'ruby-tools-to-single-quote-string)
(global-set-key (kbd "C-c \"") 'ruby-tools-to-double-quote-string)
(global-set-key (kbd "C-c :") 'ruby-tools-to-symbol)
(global-set-key (kbd "C-c ;") 'ruby-tools-clear-string)

;; gitgutter
(global-set-key (kbd "C-c v s") 'git-gutter:stage-hunk)
(global-set-key (kbd "C-c v r") 'git-gutter:revert-hunk)

(provide 'init-bindings)
;;; init-bindings.el ends here
