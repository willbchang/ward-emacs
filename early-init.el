;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
;; https://github.com/casouri/lunarymacs/blob/6ce1a6da38d5e5c261d71a495ee2fdbd051303f9/early-init.el#L3-L26
(add-hook 'emacs-startup-hook
          (let ((old-list file-name-handler-alist)
                ;; If x10, half of cpu time is spent on gc when
                ;; scrolling.
                (threshold (* 100 gc-cons-threshold))
                (percentage gc-cons-percentage))
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)
              (setq file-name-handler-alist old-list
                    gc-cons-threshold threshold
                    gc-cons-percentage percentage)
              (garbage-collect)))
          t)

(setq file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(setq package-quickstart t)

;; Make libgccjit able to use gcc which installed from homebrew
;;(setenv "LIBGCCJITIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin20/10.2.0")

;;(setq warning-minimum-level :error)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;;-------------------------Frame-----------------------------------------------
;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Set the frame parameters before it's drawing. Save times for redrawing.
(setq default-frame-alist '((tool-bar-lines . 0)
                            (font . "Roboto Mono 18")
                            (internal-border-width . 18)
                            (left-fringe    . 3)
                            (right-fringe   . 0)
                            (vertical-scroll-bars . nil)))

;;-------------------------Key Bindings----------------------------------------
;; Make the macOS like keybinding work even Emacs's init file has bug, so I don't need to use the default keybinding which makes me like a dumb.
;; Make Option and Command work normal in Emacs Mac Port.
(setq mac-option-key-is-meta t
      x-select-enable-clipboard 't
      mac-command-modifier 'super
      mac-option-modifier 'meta)

;; Frame shortcuts
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-W") 'delete-frame)
(global-set-key (kbd "s-`") 'other-frame)
(global-set-key (kbd "M-`") 'other-window)
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

;; Buffer shortcuts
(global-set-key (kbd "s-w") 'kill-buffer-and-window)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-,") 'open-config-file)
(global-set-key (kbd "s-.") 'reload-init-file)

(defun open-config-file ()
  (interactive)
  (find-file "~/.config/emacs/config.org"))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

;; Moving Cursor
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)

;; Selecting Text
(global-set-key (kbd "s-a") 'mark-whole-buffer)

;; Editing Text
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "<s-return>") 'newline)
(global-set-key (kbd "s-<backspace>") 'backward-kill-line)
(global-set-key (kbd "s-S-<backspace>") 'kill-whole-line)

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
