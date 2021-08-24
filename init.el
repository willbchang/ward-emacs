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
 '(package-selected-packages
   '(amx evil-anzu evil-better-visual-line ace-pinyin avy org-download restart-emacs gitignore-mode gitconfig-mode gitattributes-mode ivy-rich evil-goggles diff-hl evil-snipe ivy-prescient ivy org-appear sis auto-package-update evil-pinyin valign undo-fu markdown-mode exec-path-from-shell vterm evil use-package))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#8fe9e3"))))
 '(diff-hl-delete ((t (:background "#f5cce1"))))
 '(diff-hl-insert ((t (:background "#80f1a4"))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
