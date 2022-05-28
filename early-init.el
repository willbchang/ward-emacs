;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
;; PATCH: UX
(setq package-enable-at-startup nil)
(setq package-quickstart t)

;; PATCH: Native Comp
;; Make libgccjit able to use gcc which installed from homebrew
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin21/11")

(setq native-comp-async-report-warnings-errors nil)
(setq package-native-compile t)

