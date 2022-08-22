;;; detached-shell.el --- Detached integration for shell -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a `detached' extension which provides integration for `shell'.

;;; Code:

;;;; Requirements

(require 'detached)

;;;; Variables

(defcustom detached-shell-session-action
  '(:attach detached-shell-command-attach-session
            :view detached-view-dwim
            :run detached-shell-command)
  "Actions for a session created with `detached-shell'."
  :group 'detached
  :type 'plist)

;;;; Functions

;;;###autoload
(defun detached-shell-override-history (orig-fun &rest args)
  "Override history in ORIG-FUN with ARGS.

This function also makes sure that the HISTFILE is disabled for local shells."
  (cl-letf (((getenv "HISTFILE") ""))
    (advice-add 'comint-read-input-ring :around #'detached-shell--comint-read-input-ring-advice)
    (apply (if (called-interactively-p 'any)
               #'funcall-interactively #'funcall)
           orig-fun
           args)))

;;;###autoload
(defun detached-shell-save-history-on-kill ()
  "Add hook to save history when killing `shell' buffer."
  (add-hook 'kill-buffer-hook #'detached-shell--save-history 0 t))

;;;; Commands

;;;###autoload
(defun detached-shell-send-input (&optional detach)
  "Create a session and attach to it unless DETACH."
  (interactive "P")
  (let* ((detached-session-origin 'shell)
         (detached-session-action detached-shell-session-action)
         (detached-session-mode (if detach 'create 'create-and-attach))
         (comint-input-sender #'detached-shell--create-input-sender))
    (comint-send-input)))

;;;###autoload
(defun detached-shell-attach-session (session)
  "Attach to SESSION.

`comint-add-to-input-history' is temporarily disabled to avoid
cluttering the `comint-history' with dtach commands."
  (interactive
   (list (detached-shell--select-session)))
  (when (detached-valid-session session)
    (if (eq 'active (detached--determine-session-state session))
        (cl-letf ((detached--current-session session)
                  (comint-input-sender #'detached-shell--attach-input-sender)
                  ((symbol-function 'comint-add-to-input-history) (lambda (_) t)))
          (setq detached--buffer-session session)
          (let ((kill-ring nil))
            (comint-kill-input))
          (insert "[attached]")
          (comint-send-input))
      (detached-open-session session))))

;;;; Support functions

(defun detached-shell--select-session ()
  "Return selected session."
  (let* ((host-name (car (detached--host)))
         (sessions
          (thread-last (detached-get-sessions)
                       (seq-filter (lambda (it)
                                     (string= (car (detached--session-host it)) host-name)))
                       (seq-filter (lambda (it) (eq 'active (detached--determine-session-state it)))))))
    (detached-completing-read sessions)))

(defun detached-shell--attach-input-sender (proc _string)
  "Attach to `detached--session' and send the attach command to PROC."
  (let* ((detached-session-mode 'attach)
         (input
          (detached--shell-command detached--current-session t)))
    (comint-simple-send proc input)))

(defun detached-shell--create-input-sender (proc string)
  "Create a detached session based on STRING and send to PROC."
  (with-connection-local-variables
   (let* ((command (detached--shell-command
                    (substring-no-properties string) t)))
     (comint-simple-send proc command))))

(defun detached-shell--comint-read-input-ring-advice (orig-fun &rest args)
  "Set `comint-input-ring-file-name' before calling ORIG-FUN with ARGS."
  (with-connection-local-variables
   (let* ((history-file (cond ((string= shell--start-prog "bash") "~/.bash_history")
                              ((string= shell--start-prog "ksh") "~/.sh_history")
                              ((string= shell--start-prog "zsh") "~/.zsh_history")
                              (t nil))))
     (setq comint-input-ring-file-name
           (if history-file
               (concat
                (file-remote-p default-directory)
                history-file)
             comint-input-ring-file-name))
     (apply orig-fun args)
     (advice-remove 'comint-read-input-ring #'detached-shell--comint-read-input-ring-advice))))

(defun detached-shell--save-history ()
  "Save `shell' history."
  (unless (string-prefix-p detached--shell-command-buffer (buffer-name))
    (let* ((inhibit-message t))
      (comint-write-input-ring))))

;;;; Minor mode

(let ((map detached-shell-mode-map))
  (define-key map (kbd "<S-return>") #'detached-shell-send-input)
  (define-key map (kbd "<C-return>") #'detached-shell-attach-session))

(provide 'detached-shell)

;;; detached-shell.el ends here
