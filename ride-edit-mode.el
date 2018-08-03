;;; -*- coding: utf-8 -*-

(defvar ride-edit-mode-hook nil
  "List of function to be executed when the mode is invoked.")

(defvar ride-edit-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c '") #'ride-edit-exit)
    (define-key map (kbd "C-c C-k") #'ride-edit-abort)
    (define-key map (kbd "C-x C-s") #'ride-edit-save) map)
  "Keymap for ride edit minor mode.")

(define-minor-mode ride-edit-mode
  "Minor mode for editing files and functions from ride."
  nil
  " RideEdit"
  nil
  (add-hook 'kill-buffer-hook #'ride-edit-kill nil t)
  (add-hook 'kill-buffer-query-functions #'ride-edit-kill-query-function nil t)
  (setq-local safe-to-kill nil)
  :group 'RIDE)

(defun ride-edit-save ()
  "Sends a SaveChanges message to the interpreter."
  (interactive)
  (ride-send ride-client `("SaveChanges"
                         (:win ,win-id
                               :text ,(split-string
                                       (buffer-substring-no-properties
                                        (point-min)
                                        (point-max))
                                       "\n" t)))))
(defun ride-edit-exit ()
  "Saves any changes before exiting."
  (interactive)
  (ride-edit-save)
  (ride-send ride-client `("CloseWindow"
                         (:win ,win-id))))

(defun ride-edit-abort ()
  "Exits without saving."
  (interactive)
  (ride-send ride-client `("CloseWindow"
                           (:win ,win-id))))

(defun ride-edit-kill ()
  "Exits without saving."
  (ride-edit-abort))

(defun ride-edit-kill-query-function ()
  "Verify user wants to exit before exiting."
  (if safe-to-kill t (y-or-n-p "This is an edit window. Any changes will be unsaved. Kill it? ")))

(provide 'ride-edit-mode)
