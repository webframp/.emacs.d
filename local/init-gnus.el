;;;; init-gnus.el --- Custom settings for notmuch and mail handling
;;; Commentary:

;; this configures gnus

;;; Code:

;; load general mail settings
(require 'init-mail)

;; mail viewing and composing
(require 'gnus-art)

;; support multiple email accounts
(require 'gnus-alias)
(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
(setq gnus-inhibit-startup-message t)
;; Define Identities
;; Define three identities, "home", "work", "jw"
(setq gnus-alias-identity-alist
      '(("personal"
         nil ;; Does not refer to any other identity
         "Sean Escriva <sean.escriva@gmail.com>" ;; Sender address
         nil ;; No organization header
         nil ;; No extra headers
         nil ;; No extra body text
         "~/.signature"
         )
        ("hw"
         nil ;; Not referencing another identity
         "Sean Escriva <sean@hw-ops.com>"
         "Heavywater"
         nil ;; No extra headers
         nil ;; No extra body text
         "~/.signature.hw")
        ("jw"
         nil ;; Doesn't refer to another id
         "Sean Escriva <sescriva@jw.org>"
         nil ;; No org header
         nil ;; No extra headers
         nil ;; No extra body text
         "~/.signature.jw")
        ))

;; Use "jw" identity by default
(setq gnus-alias-default-identity "jw")
;; Define rules to match work identity
(setq gnus-alias-identity-rules
      '(("hw" ("any" "<\\(.+\\)\\@hw-ops\\.com" both) "hw")
        ("jw" ("any" "<\\(.+\\)\\@jw\\.org" both) "jw")))

(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
;;; threading display
(defun gnus-user-format-function-@ (header)
  "Display @ for message with attachment in summary line.

You need to add `Content-Type' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'."
  (let ((case-fold-search t)
        (ctype (or (cdr (assq 'Content-Type (mail-header-extra header)))
                   "text/plain"))
        indicator)
    (when (string-match "^multipart/mixed" ctype)
      (setq indicator "@"))
    (if indicator
        indicator
      " ")))

(defalias 'gnus-user-format-function-score 'rs-gnus-summary-line-score)
(setq gnus-face-9 'font-lock-warning-face)
(setq gnus-face-10 'shadow)

(defun rs-gnus-summary-line-score (head)
  "Return pretty-printed version of article score.
See (info \"(gnus)Group Line Specification\")."
  (let ((c (gnus-summary-article-score (mail-header-number head))))
    ;; (gnus-message 9 "c=%s chars in article %s" c (mail-header-number head))
    (cond ((< c -1000)     "vv")
          ((< c  -100)     " v")
          ((< c   -10)     "--")
          ((< c     0)     " -")
          ((= c     0)     "  ")
          ((< c    10)     " +")
          ((< c   100)     "++")
          ((< c  1000)     " ^")
          (t               "^^"))))

(defun sdl-gnus-summary-line-format-ascii nil
  (interactive)
  (setq gnus-summary-line-format
        (concat
         "%0{%U%R%z%}" "%10{|%}" "%1{%d%}" "%10{|%}"
         "%9{%u&@;%}" "%(%-15,15f %)" "%10{|%}" "%4k" "%10{|%}"
         "%2u&score;" "%10{|%}" "%10{%B%}" "%s\n"))
  (setq
   gnus-sum-thread-tree-single-indent   "o "
   gnus-sum-thread-tree-false-root      "x "
   gnus-sum-thread-tree-root            "* "
   gnus-sum-thread-tree-vertical        "| "
   gnus-sum-thread-tree-leaf-with-other "|-> "
   gnus-sum-thread-tree-single-leaf     "+-> " ;; "\\" is _one_ char
   gnus-sum-thread-tree-indent          "  ")
  (gnus-message 5 "Using ascii tree layout."))

(defun sdl-gnus-summary-line-format-unicode nil
  (interactive)
  (setq gnus-summary-line-format
        (concat
         "%0{%U%R%z%}" "%10{│%}" "%1{%d%}" "%10{│%}"
         "%9{%u&@;%}" "%(%-15,15f %)" "%10{│%}" "%4k" "%10{│%}"
         "%2u&score;" "%10{│%}" "%10{%B%}" "%s\n"))
  (setq
   gnus-sum-thread-tree-single-indent   "◎ "
   gnus-sum-thread-tree-false-root      "  "
   gnus-sum-thread-tree-root            "┌ "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-leaf-with-other "├─>"
   gnus-sum-thread-tree-single-leaf     "└─>"
   gnus-sum-thread-tree-indent          "  ")
  (gnus-message 5 "Using ascii tree layout with unicode chars."))

(defun sdl-gnus-summary-line-format-unicode-alt nil
  "Alternate version of unicode arrow message display.
Format"
  (interactive)
  (setq gnus-summary-line-format
        (concat
         "%0{%U%R%z%}"
         "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
         "  "
         "%4{%-20,20f%}"               ;; name
         "  "
         "%3{│%}"
         " "
         "%1{%B%}"
         "%s\n"))
  (setq
   gnus-sum-thread-tree-indent          "  "
   gnus-sum-thread-tree-root            "" ;; "● ")
   gnus-sum-thread-tree-false-root      "" ;; "◯ ")
   gnus-sum-thread-tree-single-indent   "" ;; "◎ ")
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-leaf-with-other "├─► "
   gnus-sum-thread-tree-single-leaf     "╰─► "
   gnus-summary-display-arrow t )
  (gnus-message 5 "Using alternate ascii tree layout with unicode chars."))

;; decide which layout to use
(sdl-gnus-summary-line-format-unicode)

;; no idea how to test this, but need to
(require 'gnus-desktop-notify)
(gnus-desktop-notify-mode)
(gnus-demon-add-handler 'gnus-demon-scan-mail 2 nil)


(provide 'init-gnus)
;;; init-gnus.el ends here
