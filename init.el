;;; -*- lexical-binding: t; -*-

;; Increase GC to 100MB to speed up start time.
(setq gc-cons-threshold (* 100 1024 1024))
;; Make Option and Command work normal in Emacs Mac Port.
(setq mac-option-key-is-meta t
      x-select-enable-clipboard 't
      mac-command-modifier 'super
      mac-option-modifier 'meta)
(org-babel-load-file "~/.config/emacs/config.org")
(set 'ad-redefinition-action 'accept)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-line-numbers-type 'relative)
 '(git-gutter:update-interval 2)
 '(package-selected-packages
   '(org-appear sis auto-package-update evil-pinyin valign undo-fu markdown-mode exec-path-from-shell vterm evil use-package))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:weight light)))))
