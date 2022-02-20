;;; -*- lexical-binding: t; -*-
;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
;; (kill-buffer "*Messages*")

;; Disable unhelpful mesages in minibuffer.
;; https://superuser.com/a/1025827/1114552
;; https://www.reddit.com/r/emacs/comments/df3kko/suppress_some_message_in_minibuffer/

(defun suppress-messages (func &rest args)
  (cl-letf (((symbol-function 'message)
              (lambda (&rest args) nil)))
     (apply func args)))

(advice-add 'load :around #'suppress-messages)
(advice-add 'org-babel-load-file :around #'suppress-messages)
(advice-add 'write-region :around #'suppress-messages)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
(set 'ad-redefinition-action 'accept)

;; https://www.reddit.com/r/emacs/comments/4q4ixw
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
