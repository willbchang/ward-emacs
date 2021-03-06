;; Switch to English input method when switching to normal mode and switch back when entering insert/replace modes.
;; im-select is needed https://github.com/daipeihust/im-select
(defvar default-input-method "com.apple.keylayout.ABC" "Default ascii-only input method")
(defvar previous-input-method (substring (shell-command-to-string "/usr/local/bin/im-select") 0 -1)
  "INPUT-METHOD that I use when starting Emacs and exiting insert mode")

(defun input-method-use-english ()
  "Switch to English input method.
im-select is needed https://github.com/daipeihust/im-select"
  (interactive)
  (cond ((eq system-type 'darwin)
         (call-process-shell-command (concat "/usr/local/bin/im-select " default-input-method)))))

(defun input-method-remember ()
  "Remember the input method being used in insert mode,
so we can switch to it in other modes."
  (interactive)
  (cond ((eq system-type 'darwin)
         (setq previous-input-method (substring (shell-command-to-string "/usr/local/bin/im-select") 0 -1)))))

(defun input-method-use-previous ()
  "Use previous input method.
If previous input method is not defined, use default method"
  (interactive)
  (cond ((eq system-type 'darwin)
         (if previous-input-method
             (call-process-shell-command (concat "/usr/local/bin/im-select " previous-input-method))
           (call-process-shell-command (concat "/usr/local/bin/im-select " default-input-method))))))

(add-hook 'evil-normal-state-entry-hook 'input-method-use-english)
(add-hook 'evil-insert-state-entry-hook 'input-method-use-previous)
(add-hook 'evil-insert-state-exit-hook 'input-method-remember)
(add-hook 'evil-replace-state-entry-hook 'input-method-use-previous)
(add-hook 'evil-replace-state-exit-hook 'input-method-remember)
(add-hook 'evil-emacs-state-entry-hook 'input-method-use-english)

(provide 'evil-switch-input-method)
;;;evil-switch-input-method.el ends here
