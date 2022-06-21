;;; -*- coding: utf-8 -*-

(defvar ride-debug-mode-hook nil
  "List of function to be executed when the mode is invoked.")

(defvar ride-debug-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "n") #'ride-debug-TraceForward)
    (define-key map (kbd "p") #'ride-debug-TraceBackward)
    (define-key map (kbd "RET") #'ride-debug-RunCurrentLine)
    (define-key map (kbd "SPC") #'ride-debug-RunCurrentLine)
    (define-key map (kbd "i") #'ride-debug-StepInto)
    (define-key map (kbd "C-RET") #'ride-debug-StepInto)
    (define-key map (kbd "c") #'ride-debug-Continue)
    (define-key map (kbd "u") #'ride-debug-Cutback)
    (define-key map (kbd "b") #'ride-debug-toggleBreakpoint)
    ;; (define-key map (kbd "C-c C-k") #'ride-edit-abort)
    ;; (define-key map (kbd "C-x C-s") #'ride-edit-save)
    map)
  "Keymap for ride edit minor mode.")

(add-to-list 'overlay-arrow-variable-list 'ridedbg-overlay-arrow-position)

(define-fringe-bitmap 'breakpoint
  "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")

(define-minor-mode ride-debug-mode
  "Minor mode for editing files and functions from ride."
  nil
  " RideDbg"
  nil
  ;; (add-hook 'kill-buffer-hook #'ride-edit-kill nil t)
  ;; (add-hook 'kill-buffer-query-functions #'ride-edit-kill-query-function nil t)
  (setq-local safe-to-kill nil)
  :group 'RIDE)

(defun ride-debug-TraceBackward ()
  (interactive)
  (ride-send ride-client `("TraceBackward"
                           (:win ,win-id))))

(defun ride-debug-TraceForward ()
  (interactive)
  (ride-send ride-client `("TraceForward"
                           (:win ,win-id))))

(defun ride-debug-Continue ()
  (interactive)
  (ride-send ride-client `("Continue"
                           (:win ,win-id))))

(defun ride-debug-ContinueTrace ()
  (interactive)
  (ride-send ride-client `("ContinueTrace"
                           (:win ,win-id))))

(defun ride-debug-ContinueTrace ()
  (interactive)
  (ride-send ride-client `("ContinueTrace"
                           (:win ,win-id))))

(defun ride-debug-Cutback ()
  (interactive)
  (ride-send ride-client `("Cutback"
                           (:win ,win-id))))

(defun ride-debug-RunCurrentLine ()
  (interactive)
  (ride-send ride-client `("RunCurrentLine"
                           (:win ,win-id))))

(defun ride-debug-StepInto ()
  (interactive)
  (ride-send ride-client `("StepInto"
                           (:win ,win-id))))

(defun ride-debug-toggleBreakpoint ()
  (interactive)
  (let ((l (1- (line-number-at-pos))))
    (if (member l breakpoints)
        (setq-local breakpoints (remove l breakpoints))
      (add-to-list 'breakpoints l)))
  (ride-send ride-client `("SetLineAttributes"
                           (:win ,win-id
                                 :stop ,(if breakpoints breakpoints []))))
  (ride-debug-drawBreakpoints))

(defun ride-debug-drawBreakpoints ()
  (remove-overlays)
  (save-excursion
    (mapcar (lambda (b)
              (goto-line (1+ b))
              (overlay-put
               (make-overlay (point) (point))
               'before-string (propertize
                               "x" 'display
                               `(left-fringe breakpoint error))))
            breakpoints)))


;; (defun ride-edit-save ()
;;   "Sends a SaveChanges message to the interpreter."
;;   (interactive)
;;   (ride-send ride-client `("SaveChanges"
;;                            (:win ,win-id
;;                                  :text ,(split-string
;;                                          (buffer-substring-no-properties
;;                                           (point-min)
;;                                           (point-max))
;;                                          "\n" t)))))
;; (defun ride-edit-exit ()
;;   "Saves any changes before exiting."
;;   (interactive)
;;   (ride-edit-save)
;;   (ride-send ride-client `("CloseWindow"
;;                          (:win ,win-id))))

;; (defun ride-edit-abort ()
;;   "Exits without saving."
;;   (interactive)
;;   (ride-send ride-client `("CloseWindow"
;;                            (:win ,win-id))))

;; (defun ride-edit-kill ()
;;   "Exits without saving."
;;   (ride-edit-abort))

;; (defun ride-edit-kill-query-function ()
;;   "Verify user wants to exit before exiting."
;;   (if safe-to-kill t (y-or-n-p "This is an edit window. Any changes will be unsaved. Kill it? ")))

(provide 'ride-debug-mode)
