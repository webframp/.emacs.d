;;;; user-defuns.el --- stuff that can't go anywhere else
;;; Commentary:
;; Various bit I like from other places

;;; Code:

;; esk-* from:
;; https://github.com/technomancy/emacs-starter-kit/blob/v2/starter-kit-defuns.el
;; A lot of starter-kit v2 is very nice. I really hate the paredit
;; dependency of the whole esk v2, so just using most of what I like.

(defun esk-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun esk-turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

;; (add-hook 'prog-mode-hook 'esk-local-column-number-mode)
;; (add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
;; (add-hook 'prog-mode-hook 'esk-turn-on-save-place-mode)
;; (add-hook 'prog-mode-hook 'esk-pretty-lambdas)
;; (add-hook 'prog-mode-hook 'esk-add-watchwords)

;; (defun esk-prog-mode-hook ()
;;   (run-hooks 'prog-mode-hook))

(defun esk-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun esk-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun esk-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (esk-indent-buffer)
  (esk-untabify-buffer)
  (delete-trailing-whitespace))

;; basic, dumb window movement
(defun rotate-windows ()
  "Rotate your windows" (interactive)
  (cond ((not (> (count-windows) 1)) (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))



(defun screen-size-as-string ()
  (let ((width (frame-pixel-width))
        (height (frame-pixel-height)))
    (concat (number-to-string (if (oddp width) (+ 1 width) width))
            "x"
            (number-to-string (+ screen-vertical-padding (if (oddp height) (+ 1 height) height))))))

;; http://emacsredux.com/blog/2013/03/27/open-file-in-external-program/
(defun open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

;; Cleaner eval-after-load
;; Thanks to:  https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el#L1
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;; smarter tab
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (sme/company-complete-or-select)
        (indent-according-to-mode)))))

;; WIP
(defun sme/company-complete-or-select ()
  (if 'company-candidates
      (company-select-next))
  (company-complete-selection))

;; split out cause it's huge
(require 'mailto-compose-mail)

(provide 'user-defuns)
;;; user-functions.el ends here
