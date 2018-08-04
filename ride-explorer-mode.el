;;; -*- coding: utf-8 -*-

(defvar ride-explorer-mode-hook
  '(ride-explorer-init read-only-mode)
  "List of functions to be executed on the envocation of ride explorer mode.")

(defvar ride-explorer-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-i") #'ride-explorer-expand/collapse)
    (define-key map (kbd "RET") #'ride-explorer-edit-line)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "e") #'ride-explorer-edit-line)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
    map)
  "Keymap for ride explorer mode.")

(defface ride-explorer-variable-face '((t
                                        (:inherit font-lock-variable-name-face)))
  "Face for variables in ws explorer.")

(defface ride-explorer-function-face '((t
                                        (:inherit font-lock-function-name-face)))
  "Face for function in ws explorer.")

(defface ride-explorer-object-face '((t
                                      (:inherit font-lock-type-face)))
  "Face for objects in ws explorer.")

(defface ride-explorer-operator-face '((t
                                        (:inherit font-lock-keyword-face)))
  "Face for operators in ws explorer.")

(defface ride-explorer-event-face '((t
                                     (:inherit font-lock-string-face)))
  "Face for events in ws explorer.")

(define-derived-mode ride-explorer-mode fundamental-mode "RideExplorer"
  "Major mode for the ride workspace explorer."
  (setq-local tree-list-response nil))

(defun ride-explorer-init ()
  (ride-send ride-client '("TreeList" (:nodeId 0)))
  (accept-process-output ride-client nil 2)
  (let ((inhibit-read-only t))
    (apply #'ride-explorer-insert (cons 0 tree-list-response))
    (newline)
    (goto-char (point-min))))

(defun ride-explorer-insert (indent parent ids classes names)
  (mapcar* (lambda (id class name)
             (let* ((expandable (/= 0 id))
                    (name (if (not expandable) (concat (ride-explorer-get-char class) " " name)
                            (concat "+" (ride-explorer-get-char class) " " name))))
               (insert (propertize name
                                   'parent parent
                                   'id id
                                   'expandable expandable
                                   'expanded nil
                                   'face (ride-explorer-get-face class)
                                   'rear-nonsticky t
                                   'front-sticky t))
               (indent-line-to indent)
               (end-of-line)
               (newline)))
           ids classes names)
  (delete-backward-char 1)
  (length names))

(defun ride-explorer-get-face (x)
  (let ((x (floor x)))
    (cond
     ((= 2 x)
      'ride-explorer-variable-face)
     ((= 3 x)
      'ride-explorer-function-face)
     ((= 4 x)
      'ride-explorer-operator-face)
     ((= 8 x)
      'ride-explorer-event-face)
     (t 'ride-explorer-object-face))))

(defun ride-explorer-get-char (x)
  (cond
   ((= 2 (floor x))
    "V")
   ((= 3 (floor x))
    "F")
   ((= 4 (floor x))
    "O")
   ((= 8 (floor x))
    "E")
   ((= 9.4 x)
    "C")
   (t "N")))

(defun ride-explorer-expand/collapse ()
  (interactive)
  (let* ((inhibit-read-only t)
         (start-pos (prog1 (point) (beginning-of-line)))
         (parent (get-text-property (point) 'parent))
         (id (get-text-property (point) 'id))
         (expandable (get-text-property (point) 'expandable))
         (expanded (get-text-property (point) 'expanded))
         (face (get-text-property (point) 'face))
         (indent (current-indentation)))
    (when expandable
      (if expanded
          (progn
            (search-forward "-")
            (replace-match (propertize "+"
                                       'parent parent
                                       'id id
                                       'expandable t
                                       'expanded nil
                                       'face face
                                       'front-sticky t
                                       'rear-nonsticky t))
            (beginning-of-line)
            (add-text-properties (point) (progn (end-of-line) (point)) `(expanded ,nil))
            (forward-line 1)
            (while (not (or (= (point) (point-max)) (equal parent (get-text-property (point) 'parent))))
              (delete-region (point) (progn (end-of-line) (point)))
              (delete-forward-char 1)))
        (progn
          (search-forward "+")
          (replace-match (propertize "-"
                                     'parent parent
                                     'id id
                                     'expandable t
                                     'expanded t
                                     'face face
                                     'front-sticky t
                                     'rear-nonsticky t))
          (beginning-of-line)
          (add-text-properties (point) (progn (end-of-line) (point)) `(expanded ,t))
          (ride-send ride-client `("TreeList" (:nodeId ,id)))
          (accept-process-output ride-client nil 2)
          (newline)
          (apply #'ride-explorer-insert (cons (+ 2 indent) tree-list-response)))))
    (goto-char start-pos)))

(defun ride-explorer-edit-line ()
  (interactive)
  (let ((start-pos (point))
        (name (ride-explorer-resolve-name)))
    (goto-char start-pos)
    (ride-edit name)))

(defun ride-explorer-resolve-name ()
  (end-of-line)
  (backward-char 1)
  (let ((parent (get-text-property (point) 'parent))
        (word (substring (string-trim (thing-at-point 'line t) "[ +-]+") 2)))
    (if (= parent 0) word
      (while (/= parent (get-text-property (point) 'id))
        (previous-line)
        (end-of-line)
        (backward-char 1))
      (concat (ride-explorer-resolve-name) "." word))))
(provide 'ride-explorer-mode)
