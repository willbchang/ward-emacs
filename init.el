;;; -*- lexical-binding: t; -*-

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
(set 'ad-redefinition-action 'accept)

;; https://www.reddit.com/r/emacs/comments/4q4ixw
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)


(setq default-frame-alist
      (append (list
	           '(min-height . 1)
               '(height     . 45)
	           '(min-width  . 1)
               '(width      . 80)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 18)
               '(left-fringe    . 2)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))
