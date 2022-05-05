;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;; PATCH: UX
(setq file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
;; PATCH: UX
(setq package-enable-at-startup nil)
(setq package-quickstart t)

;; PATCH: Native Comp
;; Make libgccjit able to use gcc which installed from homebrew
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin21/11")

(setq warning-minimum-level :error)

;;-------------------------Frame-----------------------------------------------
;; PATCH: UI
;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Set the frame parameters before it's drawing. Save times for redrawing.
(setq default-frame-alist '((tool-bar-lines . 0)
                            (font . "Roboto Mono 18")
                            (internal-border-width . 18)
                            (left-fringe    . 3)
                            (right-fringe   . 1)
                            (vertical-scroll-bars . nil)))

;; PATCH: Key Binding
;;-------------------------Key Bindings----------------------------------------
(global-set-key (kbd "s-,") 'open-config-file)
(global-set-key (kbd "s-.") 'reload-init-file)

;; TODO: param/fallbacks for file.
(defun open-config-file ()
  (interactive)
  (find-file "~/.config/emacs/config.org"))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))
