;; ------------------- Font -------------------------------------------
;; Set Font Family and Font Size
(set-face-attribute 'default nil
                    :font "Inconsolata-18")

;; ------------------- Color Theme --------------------------------------
;; Set Color Theme
(load-theme 'idea-darkula t)

;; Set Cursor Color
(set-cursor-color "#e2416c")

;; Set Titlebar Appearance
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


;; ------------------- Text Editing --------------------------------------
;; Set Command + ArrowKey to behave like MacOS
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)

;; Delete current line and put cursor in the beginning
(global-set-key (kbd "s-<backspace>") 'backward-kill-line)
;; Kill whole line entirely
(global-set-key (kbd "s-S-<backspace>") 'kill-whole-line)

;; Set shortcut to comment/uncomment a region/line
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)


(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
 (kill-line (- 1 arg)))
;; TODO:
;; 1. Comment on empty line, it adds ;; (e.g.) and put the cursor behind
;; 2. Comment one line, it adds ;; before and forward one line
;; 3. Comment on region, it add ;; and move to the next line of the region
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))


;; ------------------- Window Management ---------------------------------
;; Set frame full screen with Ctrl + Command + F
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)
;; Create new frame with Command + N, default with org-mode and evil-mode
(global-set-key (kbd "s-n") 'new-empty-frame)

;; Close current buffer
(global-set-key (kbd "s-w") 'kill-this-buffer)
;; Close current frame with Shift + Command + W
(global-set-key (kbd "s-W") 'delete-frame)

;; TODO: Make the buffers independent in each frame.
(defun new-empty-frame ()
  "Create a new frame with a new empty buffer. With org-mode and evil-mode enabled."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer buffer)
    (org-mode)
    (evil-mode 1)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))


;; ------------------- Behaviors ------------------------------------
;; Save cursor position for each file
(save-place-mode t)

;; Overwrite selection on pasting
(delete-selection-mode)

;; Highlight urls and make them clickable
(goto-address-mode)

;; Highlight paired brackets
(show-paren-mode 1)
(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#fad56e")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; Delete trailing whitespace on saving a buffer
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Disable the ring bell when scroll beyond the document
(setq ring-bell-function 'ignore)

;; ------------------- Proxy ----------------------------------------
;; Start server for opening file/folder from emacsclient
(server-start)
