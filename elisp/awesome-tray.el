;;; awesome-tray.el ---  Modular tray bar

;; Filename: awesome-tray.el
;; Description: Modular tray bar
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-10-07 07:30:16
;; Version: 4.2
;; Last-Updated: 2020-06-18 21:02:39
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/awesome-tray.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `cl-lib'
;; `subr-x'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Require
(require 'cl-lib)
(require 'subr-x)

;;; Code:
(defgroup awesome-tray nil
  "Modular tray bar."
  :group 'awesome-tray)

(defcustom awesome-tray-active-modules
  '("location")
  "Default active modules."
  :type 'list
  :group 'awesome-tray)

(defcustom awesome-tray-refresh-idle-delay 0.05
  "Update idle delay of awesome tray, in seconds."
  :type 'double
  :group 'awesome-tray)

;;;###autoload
(define-minor-mode awesome-tray-mode
  "Modular tray bar."
  :require 'awesome-tray-mode
  :global t
  (if awesome-tray-mode
      (awesome-tray-enable)))

(defvar awesome-tray-info-padding-right 1)

(defvar awesome-tray-timer nil)

(defvar awesome-tray-active-p nil)

(defvar awesome-tray-last-tray-info nil)

(defvar awesome-tray-module-alist
  '(("location" . (awesome-tray-module-location-info default))))

(defun awesome-tray-enable ()
  ;; Disable mode line.
  (set-face-attribute 'mode-line nil
                      :background nil
                      :height 0.1
                      :underline t
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :background nil
                      :box nil
                      :inherit 'mode-line)
  (set-face-attribute 'header-line nil
                      :inherit nil)

  ;; Add update timer.
  (setq awesome-tray-timer
        (run-with-timer 0 awesome-tray-refresh-idle-delay 'awesome-tray-show-info))
  (add-hook 'focus-in-hook 'awesome-tray-show-info)
  (setq awesome-tray-active-p t))

(defun awesome-tray-build-info ()
  (condition-case nil
      (mapconcat 'identity (cl-remove-if #'(lambda (n) (equal (length n) 0))
                                         (mapcar 'awesome-tray-get-module-info awesome-tray-active-modules)) " ")
    (format "Awesome Tray broken.")))

(defun awesome-tray-get-module-info (module-name)
  (let* ((func (ignore-errors (cadr (assoc module-name awesome-tray-module-alist))))
         (face-param (ignore-errors (caddr (assoc module-name awesome-tray-module-alist))))
         (face (cond ((functionp face-param) (funcall face-param))
                     ((facep face-param) face-param)
                     (t nil)))
         (raw-info (ignore-errors (funcall func)))
         (info (ignore-errors (if face (propertize raw-info 'face face) raw-info))))
    (if func
        (if info
            info
          (propertize "" 'face face))
      (propertize module-name 'face 'default))))


(defun awesome-tray-module-location-info ()
  (format "%s:%s"
          (format-mode-line "%l")
          (format-mode-line "%C")))

(defun awesome-tray-show-info ()
  ;; Only flush tray info when current message is empty.
  (unless (current-message)
    (awesome-tray-flush-info)))

(defun awesome-tray-get-frame-width ()
  "Only calculating a main Frame width, to avoid wrong width when new frame, such as `snails'."
  (with-selected-frame (car (last (frame-list)))
    (frame-width)))

(defun awesome-tray-flush-info ()
  (let* ((tray-info (awesome-tray-build-info)))
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert (concat (make-string (max 0 (- (awesome-tray-get-frame-width) (string-width tray-info) awesome-tray-info-padding-right)) ?\ ) tray-info)))))

(defun awesome-tray-get-echo-format-string (message-string)
  (let* ((tray-info (awesome-tray-build-info))
         (blank-length (- (awesome-tray-get-frame-width) (string-width tray-info) (string-width message-string) awesome-tray-info-padding-right))
         (empty-fill-string (make-string (max 0 (- (awesome-tray-get-frame-width) (string-width tray-info) awesome-tray-info-padding-right)) ?\ ))
         (message-fill-string (make-string (max 0 (- (awesome-tray-get-frame-width) (string-width message-string) (string-width tray-info) awesome-tray-info-padding-right)) ?\ )))
    (prog1
        (cond
         ;; Fill message's end with whitespace to keep tray info at right of minibuffer.
         ((> blank-length 0)
          (concat message-string message-fill-string tray-info))
         ;; Fill empty whitespace if new message contain duplicate tray-info (cause by move mouse on minibuffer window).
         ((and awesome-tray-last-tray-info
               message-string
               (string-suffix-p awesome-tray-last-tray-info message-string))
          (concat empty-fill-string tray-info))
         ;; Don't fill whitepsace at end of message if new message is very long.
         (t
          (concat message-string "\n" empty-fill-string tray-info)))
      ;; Record last tray information.
      (setq awesome-tray-last-tray-info tray-info))))

(defun awesome-tray-process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

;; Wrap `message' make tray information visible always
;; even other plugins call `message' to flush minibufer.
(defun awesome-tray-message-advice (old-message &rest arguments)
  (if (ignore-errors
        (cond
         ;; Don't wrap tray info if `awesome-tray-active-p' is nil.
         ((not awesome-tray-active-p)
          (apply old-message arguments))

         ;; Don't wrap awesome-tray info if variable `inhibit-message' is non-nil.
         (inhibit-message
          (apply old-message arguments))

         ;; Just flush tray info if message string is empty.
         ((not (car arguments))
          (apply old-message arguments)
          (awesome-tray-flush-info))

         ;; Otherwise, wrap message string with tray info and show it in echo area,
         ;; logging origin message at `*Messages*' buffer if allowed.
         (t
          (if message-log-max
              (let ((inhibit-message t))
                (apply old-message arguments)))
          (let ((message-log-max nil))
            (apply old-message "%s" (cons (awesome-tray-get-echo-format-string (apply 'format arguments)) '())))))

        ;; Return t if everything is okay.
        t)
      ;; Return origin message string. if not, `message' function will always return `nil'.
      (if (car arguments)
          (apply 'format arguments))
    (apply old-message arguments)))

(advice-add #'message :around #'awesome-tray-message-advice)

(defun awesome-tray-current-message-advice (old-func &rest arguments)
  (let ((message-string (apply old-func arguments)))
    (if (and message-string awesome-tray-last-tray-info)
        (string-trim-right (replace-regexp-in-string awesome-tray-last-tray-info "" message-string))
      message-string)))

(advice-add #'current-message :around #'awesome-tray-current-message-advice)

(defun awesome-tray-end-of-buffer-advice (old-func &rest arguments)
  (apply old-func arguments)
  (message ""))

(advice-add #'end-of-buffer :around #'awesome-tray-end-of-buffer-advice)

(defun awesome-tray-beginning-of-buffer-advice (old-func &rest arguments)
  (apply old-func arguments)
  (message ""))

(advice-add #'beginning-of-buffer :around #'awesome-tray-beginning-of-buffer-advice)

(provide 'awesome-tray)

;;; awesome-tray.el ends here
