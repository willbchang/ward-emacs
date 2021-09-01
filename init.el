;;; -*- lexical-binding: t; -*-

(org-babel-load-file "~/.config/emacs/config.org")
(set 'ad-redefinition-action 'accept)

;; https://www.reddit.com/r/emacs/comments/4q4ixw
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
