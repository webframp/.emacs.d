;;; package -- init-python.el - ruby specific settings
;;; Commentary:
;;; Code:

(require 'python-mode)

(push "~/.virtualenvs/default/bin" exec-path)
(setenv "PATH"
        (concat
         "~/.virtualenvs/default/bin" ":"
         (getenv "PATH")))

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;; don't split windows
(setq py-split-windows-on-execute-p nil)

;; try to automagically figure out indentation
(setq py-smart-indentation t)

(provide 'init-python)
;;; init-python.el ends here
