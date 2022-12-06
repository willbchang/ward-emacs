;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Adjust garbage collection thresholds during startup, and thereafter.
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Set the frame parameters before it's drawing. Save times for redrawing.
(setq default-frame-alist '((ns-transparent-titlebar . t)
                            (ns-appearance . light)
                            (tool-bar-lines . 0)
                            (font . "Roboto Mono-18")
                            (internal-border-width . 18)
                            (left-fringe    . 3)
                            (right-fringe   . 1)
                            (vertical-scroll-bars . nil)))


;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(setq package-quickstart t)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(fset 'display-startup-echo-area-message 'ignore)

(setq native-comp-async-report-warnings-errors nil)
(setq package-native-compile t)

;; Here are some Nextstep-like bindings for command key sequences.

;; Make Option and Command work normal in Emacs Mac Port.
(setq mac-command-modifier 'super
      mac-option-modifier 'meta)

;; Frame
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-N") 'new-empty-frame)
(global-set-key (kbd "s-W") 'delete-frame)
(global-set-key (kbd "s-`") 'other-frame)
(global-set-key (kbd "M-`") 'other-window)
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

;; Buffer
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-n") 'new-empty-buffer)
(global-set-key (kbd "s-r") 'revert-buffer-no-confirm)
(global-set-key (kbd "s-p") 'project-find-file)
(global-set-key (kbd "s-,") 'open-config-file)
(global-set-key (kbd "s-.") 'reload-init-file)



;; Moving Cursor
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)
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

;; Text Scale
(global-set-key (kbd "s-+") 'text-scale-adjust)
(global-set-key (kbd "s-=") 'text-scale-adjust)
(global-set-key (kbd "s--") 'text-scale-adjust)
(global-set-key (kbd "s-0") 'text-scale-adjust)

;; Use Return to act Yes
(setq use-short-answers t)
(define-key y-or-n-p-map (kbd "<return>") 'y-or-n-p-insert-y)

;; Translation ESC keybinding
;; Cancel partially typed or accidental command.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Translation Chinese Keybindings
(define-key key-translation-map (kbd "s-【") (kbd "s-["))
(define-key key-translation-map (kbd "s-】") (kbd "s-]"))
(define-key key-translation-map (kbd "s-，") (kbd "s-,"))
(define-key key-translation-map (kbd "s-。") (kbd "s-."))
(define-key key-translation-map (kbd "C-·") (kbd "C-`"))
;; FIX: Key sequence M-· starts with non-prefix key ESC
;; (define-key key-translation-map (kbd "M-·") (kbd "M-`"))

;; Cusmotized Keybindings' Functions
(defun open-config-file ()
  (interactive)
  (find-file "~/.config/emacs/config.org"))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defun new-empty-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer buffer)
    (funcall initial-major-mode)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))


;; http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
(defun new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc."
  (interactive)
  (let (($buffer (generate-new-buffer "untitled")))
    (switch-to-buffer $buffer)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buffer))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (save-buffer t)
  (revert-buffer t t)
  (message "Reverted `%s'" (buffer-name)))


(defun reveal-in-finder ()
  "Open current file in Finder."
  (interactive)
  (shell-command "open -R ."))

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

;; https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
