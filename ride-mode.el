;;; -*- coding: utf-8 -*-

(require 'json)
(require 'ride-edit-mode)
(require 'ride-explorer-mode)
(require 'cl-lib)
(require 'dash)
(require 'dyalog-input)

(defgroup RIDE nil
  "Major mode `ride-mode' for a Dyalog APL repl."
  :group 'dyalog)

(defcustom ride-server-port 35353
  "Port number the RIDE server listens on."
  :group 'RIDE)

(defface ride-idiom-face '((t
                            (:inherit font-lock-type-face)))
  "Face used for idioms."
  :group 'RIDE)

(defface ride-output-face '((t
                             (:inherit font-lock-string-face)))
  "Face used for output."
  :group 'RIDE)

(defcustom idioms
  '("⍴⍴" "/⍳" "/⍳⍴" "⊃¨⊂" "{}" "{⍺}" "{⍵}" "{⍺⍵}" "{0}" "{0}¨" ",/" "⊃⌽" "↑⌽" "⊃⌽," "↑⌽," "0=⍴"
    "0=⍴⍴" "0=≡" "⎕AV⍳" "↓⍉↑" "↓⍉⊃" "+/∧\\" "+/∧\\' '=" "{(↓⍺)⍳↓⍵}" "~∘' '¨↓" "{(+/∨\\' '≠⌽⍵)↑¨↓⍵}"
    "∧\\' '=" "{(∨\\' '≠⍵)/⍵}" "{(+/∧\\' '=⍵)↓⍵}" "1=≡" "1=≡," "0∊⍴" "~0∊⍴" "⊃∘⍴¨" "↑∘⍴¨" ",←"
    "{⍵[⍋⍵]}" "{⍵[⍒⍵]}" "{⍵[⍋⍵;]}" "{⍵[⍒⍵;]}" "⍪←" "⍪/" "*○" "⊣/" "⊢/" "⊣⌿" "⊢⌿" "0=⊃⍴" "0≠⊃⍴"
    "⌊0.5+" "≢⍴" "↓⍨←" "{(⊂⍋⍵)⌷⍵}" "{(⊂⍒⍵)⌷⍵}")
  "List of idioms that are recognised and highlighted in the repl. \\ must be quoted to \\\\."
  :group 'RIDE)

(defcustom ride-enable-debugging
  nil
  "when non-nil this associates a buffer \"*ride-dbg*\" with the session that prints messages from the interpreter.")

(defcustom mapl-environment
  (list (format "RIDE_INIT=CONNECT:127.0.0.1:%d" ride-server-port)
        "CLASSICMODE=1" "SINGLETRACE=1" "RIDE_SPAWNED=1")
  "List of environment variables with their values."
  :group 'RIDE)

(defcustom mapl-command
  '("mapl" "+s" "-q")
  "Command to spawn mapl. list of (Executable &rest args)."
  :group 'RIDE)

(defvar ride-client nil
  "Has value of client process for sendingn messages to.")
(defvar msg-acc ""
  "Used when messages are split into multiple tcp messages.")
(defvar pending-msg nil
  "Used when messages are split into multiple tcp messages.")
(defvar ride-mode-hook nil
  "List of functions to be executed on startup of ride.")
(defvar ride-completion-res nil
  "Variable that holds completion results.")
(defvar ride-completion-skip 0
  "Variable that holds information about where completion begins.")
(defvar ride-open-buffer-alist nil
  "Association list with elements of the form (win-id . buffer).
This keeps track of edit windows.")
(defvar ride-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") #'ride-send-input)
    (define-key map (kbd "M-p") #'ride-previous-input)
    (define-key map (kbd "M-n") #'ride-next-input)
    (define-key map (kbd "C-c C-k") #'ride-session-clear)
    (define-key map (kbd "C-c C-c") #'ride-send-interrupt)
    (define-key map (kbd "C-c C-d") #'dyalog-help-for-symbol-at-point)
    (define-key map (kbd "C-i") #'ride-add-prompt-when-empty)
    (define-key map (kbd "C-c C-w") #'ride-workspace-explorer)
    (define-key map (kbd "C-c C-e") #'ride-edit-thing-at-point)
    map)
  "Keymap for ride-mode.")

(defvar idiom-regexps (sort (mapcar
                             (lambda (x)
                               (concat (apply #'concat (mapcar
                                                        (lambda (x)
                                                          (concat " *" (string x)))
                                                        x)) " *"))
                             idioms)
                            (lambda (x y)
                              (> (length x)
                                 (length y))))
  "List of regular expressions for the idioms")

(defvar ride-font-lock-keywords (append (list `(,(replace-regexp-in-string
                                                  "\\(\\[\\|\\]\\|\\\\ \\|\\+\\)" "\\\\\\1"
                                                  (concat "\\(" (apply #'concat (-interpose
                                                                                 "\\|"
                                                                                 idiom-regexps))
                                                          "\\)"))
                                                1 'ride-idiom-face append))
                                        dyalog-font-lock-keywords)
  "Font lock keywords for dyalog text")

(defvar ride-prompt
  (let ((p (propertize "      " 'field 'prompt 'rear-nonsticky t)))
    (concat (substring p 0 1)
            (propertize (substring p 1) 'front-sticky t)))
  "Propertized prompt so (beginning-of-line) and others won't go further than the end of it.")
(defvar ride-history-ring (let ((ring (make-ring 1)))
                            (ring-insert ring ride-prompt) ring)
  "History of inputs that have been sent.")
(defvar ride-history-ring-index nil
  "Index in history ring. Used when scrolling through history.")

(define-derived-mode ride-mode fundamental-mode "Ride"
  "Major mode for a Dyalog APL repl.

\\{ride-mode-map}"
  :group 'RIDE
  (setq-local font-lock-defaults '(ride-font-lock-keywords))
  (add-hook 'kill-buffer-hook #'ride-kill nil t)
  (add-hook 'completion-at-point-functions #'ride-completion-at-point nil t)
  (font-lock-mode -1)
  (add-hook 'after-change-functions (lambda (&rest args) (let* ((pos (point))
                                                           (inhibit-field-text-motion t)
                                                           (text (buffer-substring (point-at-bol) (point-at-eol))))
                                                      (beginning-of-line)
                                                      (delete-region (point) (progn (end-of-line) (point)))
                                                      (insert (with-temp-buffer
                                                                (dyalog-mode)
                                                                (setq-local font-lock-defaults '(ride-font-lock-keywords))
                                                                (insert text)
                                                                (font-lock-fontify-region (point-min) (point-max))
                                                                (buffer-substring (point-min) (point-max))))
                                                      (goto-char pos)))
            nil t))

(defun dyalog-start ()
  "Starts the dyalog interpreter to connect to the session."
  (let ((process-environment (append process-environment mapl-environment)))
    (make-process :name "mapl"
                  :command mapl-command
                  :connection-type 'pipe
                  :coding 'utf-8)))

(defun ride-server-init ()
  "Starts the RIDE server."
  (make-network-process :name "ride"
                        :buffer "*ride*"
                        :service ride-server-port
                        :family 'ipv4
                        :filter 'ride-server-filter
                        :server t
                        :coding 'utf-8))

(defun ride-server-filter (proc msg)
  "Parses, respond to, and handles messages from the interpreter."
  (setq ride-client proc)
  (cond ((string= msg "   RIDESupportedProtocols=2")
         (process-send-string proc msg))
        ((string= msg "   RIDEUsingProtocol=2")
         (progn (process-send-string proc msg)
                (ride-send proc '("Identify"
                                (:indentity 1)))))
        (t (progn (dolist (payload (unpack msg))
                    (when ride-enable-debugging
                      (ride-debug payload))
                    (ride-payload-parse payload))))))


(defun ride-session-clear ()
  "Remove all text from the session window. This is to reduce lag when the output is 1000s of lines long."
  (interactive)
  (delete-region (point-min)
                 (point-max))
  (ride-handle--SetPromptType :type 1))

(defun ride-add-prompt-when-empty ()
  "This adds the prompt for the user. Useful for when the interpreter neglects to set it (e.g. after a ]demo)."
  (interactive)
  (let ((inhibit-field-text-motion t))
    (when (string-empty-p (string-trim-right (thing-at-point 'line t)))
      (ride-handle--SetPromptType :type 1))))

(defun ride-payload-parse (payload)
  "Parses an individual message from the interpreter and acts accordingly."
  (let* ((decoded (json-parse-string payload
                                     :object-type 'plist))
         (command (aref decoded 0))
         (args (aref decoded 1)))
    (cond ((string= command "SetPromptType")
           (apply #'ride-handle--SetPromptType args))
          ((string= command "AppendSessionOutput")
           (apply #'ride-handle--AppendSessionOutput args))
          ((string= command "OpenWindow")
           (apply #'ride-handle--OpenWindow args))
          ((string= command "UpdateWindow")
           (apply #'ride-handle--UpdateWindow args))
          ((string= command "GotoWindow")
           (apply #'ride-handle--GotoWindow args))
          ((string= command "CloseWindow")
           (apply #'ride-handle--CloseWindow args))
          ((string= command "ReplyGetAutocomplete")
           (apply #'ride-handle--ReplyGetAutocomplete args))
          ((string= command "OptionsDialog")
           (apply #'ride-handle--OptionsDialog args))
          ((string= command "Disconnect")
           (progn (ride-kill)
                  (kill-buffer "*ride*")))
          ((string= command "ReplyTreeList")
           (apply #'ride-handle--ReplyTreeList args))
          (t nil))))

(cl-defun ride-handle--ReplyTreeList (&key nodeId nodeIds names classes err)
  (with-current-buffer "*ws-explorer*"
    (setq-local tree-list-response (cons nodeId
                                         (mapcar #'seq--into-list (list nodeIds classes names))))))

(cl-defun ride-handle--OptionsDialog (&key type token options title text)
  "Currently just replys automatically. As a user I don't want to deal with it."
  (ride-send ride-client `("ReplyOptionsDialog"
                         (:index 0
                                 :token ,token))))

(defun ride-handle--CloseWindow (&key win)
  "Kills the buffer associated with :win."
  (let* ((a (assoc win ride-open-buffer-alist))
         (buffer (cdr a)))
    (when buffer (progn
                   (setq ride-open-buffer-alist (delete* a ride-open-buffer-alist))
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (setq-local safe-to-kill t)
                       (when (get-buffer buffer)
                         (kill-buffer buffer))))))))

(cl-defun ride-handle--OpenWindow (&key currentRow debugger entityType name
                                        offset readOnly size stop text tid tname token)
  "Opens the buffer adds the text and switches to it."
  (let ((buffer (generate-new-buffer name)))
    (add-to-list 'ride-open-buffer-alist (cons token buffer))
    (with-current-buffer buffer
      (if (member entityType '(2 4 8 16 128))
          (dyalog-array-mode)
        (dyalog-mode))
      (ride-edit-mode)
      (mapcar
       (lambda (x)
         (insert x)
         (newline))
       text)
      (setq-local win-id token)
      (goto-line currentRow)
      (when (= 1 readOnly) (read-only-mode t)))
    (switch-to-buffer-other-window buffer)))

(cl-defun ride-handle--UpdateWindow (&key currentRow debugger entityType name
                                          offset readOnly size stop text tid tname token)
  "Modifies contents of buffer associated with :token"
  (let ((inhibit-read-only t)
        (buffer (cdr (assoc token ride-open-buffer-alist))))
    (when buffer (with-current-buffer buffer
                   (delete-region (point-min) (point-max))
                   (mapcar
                    (lambda (x)
                      (insert x)
                      (newline))
                    text)
                   (goto-line currentRow)))))

(defun ride-handle--GotoWindow (&key win)
  "Focus window associated with :win."
  (let ((buffer (cdr (assoc win ride-open-buffer-alist))))
    (switch-to-buffer-other-window buffer)))

(cl-defun ride-handle--ReplyGetAutocomplete (&key options skip token)
  "Sets completion variables."
  (setq ride-completion-res options
        ride-completion-skip skip))

(defun ride-handle--AppendSessionOutput (&key result)
  "Adds text to ouput. It font-locks the text so stray apostrophes don't cause issues."
  (let ((inhibit-modification-hooks (string= "\n" (substring result (1- (length result))))))
    (ride-buffer-append (propertize result 'face 'ride-output-face))))

(defun ride-handle--SetPromptType (&key type)
  "Sets the prompt of 6 spaces or no prompt based on :type."
  (if (member type '(1 2))
      (with-current-buffer "*ride*"
        (let ((inhibit-field-text-motion t))
          (goto-char (point-max))
          (beginning-of-line)
          (delete-region (point)
                         (progn (end-of-line)
                                (point)))
          (insert ride-prompt))))
  (with-current-buffer "*ride*"
    (let ((w (get-buffer-window "*ride*")))
      (when w (goto-char (point-max))
            (set-window-point w (point))))))

(define-thing-chars dyalog-thing ".⎕#_∆[:alnum:]")

(defun ride-edit-thing-at-point ()
  (interactive)
  (ride-edit (thing-at-point 'dyalog-thing t)))

(defun ride-edit (val)
  "Request the interpreter to edit val."
  (ride-send ride-client `("Edit" (:win 0 :pos 0 :text ,val))))

(defun ride-debug (msg)
  "Adds msg to debug buffer."
  (with-current-buffer "*ride-dbg*"
    (goto-char (point-max))
    (insert msg)
    (newline)))

(defun ride-previous-input (arg)
  "Controls scrolling through history."
  (interactive "*p")
  (cond ((not (= (line-number-at-pos)
                 (line-number-at-pos (point-max))))
         (user-error
          "Not at prompt"))
        (t (progn (if ride-history-ring-index
                      (setq ride-history-ring-index (mod (+ arg ride-history-ring-index)
                                                         (ring-size ride-history-ring)))
                    (setq ride-history-ring-index (mod (1- arg)
                                                       (ring-size ride-history-ring))))
                  (let* ((inhibit-field-text-motion t)
                         (input (ring-ref ride-history-ring ride-history-ring-index)))
                    (unless (= ride-history-ring-index (1- (ring-size ride-history-ring)))
                      (message "History item: %d" (1+ ride-history-ring-index)))
                    (beginning-of-line)
                    (delete-region (point)
                                   (progn (end-of-line)
                                          (point)))
                    (insert input))))))

(defun ride-workspace-explorer ()
  (interactive)
  (if (get-buffer "*ws-explorer*")
      (switch-to-buffer-other-window "*ws-explorer*")
    (progn
      (switch-to-buffer-other-window "*ws-explorer*")
      (ride-explorer-mode)
      (font-lock-mode 0))))

(defun ride-next-input (arg)
  "Calls ride-previous-input with negative argument."
  (interactive "*p")
  (ride-previous-input (- arg)))

(defun ride-send-input ()
  "Sends what ever is on the current line to the interpreter and adds it to the history."
  (interactive)
  (ride-send ride-client `("SetPW"
                         (:pw ,(window-width))))
  (let* ((inhibit-field-text-motion t)
         (input (string-trim-right (thing-at-point 'line)))
         (linum (line-number-at-pos)))
    (ring-insert+extend ride-history-ring input t)
    (setq ride-history-ring-index nil)
    (ride-send ride-client `("Execute"
                           (:text ,(format "%s\n" (substring-no-properties input))
                                  :trace nil)))
    (goto-char (point-max))
    (beginning-of-line)
    (delete-region (point)
                   (progn (end-of-line)
                          (point)))
    (insert (concat input "\n"))))

(defun ride-send-interrupt ()
  "Sends a strong interrupt message to the interpreter."
  (interactive)
  (ride-send ride-client '("StrongInterrupt" ())))

(defun ride-buffer-append (string)
  "Adds text to the session buffer"
  (let ((inhibit-field-text-motion t))
    (with-current-buffer "*ride*"
      (goto-char (point-max))
      (insert string)
      (end-of-line))))

(defun ride-send (proc msg)
  "Converts msg from lisp object into json and sends that to the interpreter."
  (process-send-string proc (pack (json-encode msg))))

(defun unpack (message)
  (if (string-empty-p msg-acc)
      (if (string-empty-p message) nil
        (if (< (length message) 8) (prog1 nil (setq msg-acc message))
         (let* ((s (mapcar #'identity (encode-coding-string (substring message 0 4) 'us-ascii)))
                (len (apply (lambda (a b c d) (+ (lsh a 24) (lsh b 16) (lsh c 8) d)) s)))
           (if (>= (string-bytes message) len)
               (let ((msg (string-as-multibyte (substring (string-as-unibyte message) 8 len))))
                 (cons msg
                       (unpack (substring message (+ 8 (length msg))))))
             (setq msg-acc message)
             nil))))
    (let ((msg (concat msg-acc message)))
      (setq msg-acc "")
      (unpack msg))))

(defun pack (message)
  (let* ((len (+ 8 (string-bytes message)))
         (a (logand (lsh len -24) #xff))
         (b (logand (lsh len -16) #xff))
         (c (logand (lsh len -8) #xff))
         (d (logand len #xff)))
    (concat (apply #'concat (mapcar #'byte-to-string(list a b c d))) "RIDE" message)))


(defun ride-start ()
  "Starts ride session unless it already is running."
  (interactive)
  (if (get-buffer "*ride*")
      (switch-to-buffer "*ride*")
    (progn (when ride-enable-debugging
             (generate-new-buffer "*ride-dbg*"))
           (ride-server-init)
           (dyalog-start)
           (switch-to-buffer "*ride*")
           (ride-mode))))

(defun ride-kill ()
  "Kills associated buffers and processes before killing the buffer"
  (mapcar
   (lambda (x)
     (with-current-buffer (cdr x)
       (setq-local safe-to-kill t)
       (kill-buffer (cdr x))))
   ride-open-buffer-alist)
  (setq ride-open-buffer-alist nil)
  (when (member "mapl" (mapcar #'process-name (process-list)))
    (kill-process "mapl"))
  (when (member "ride" (mapcar #'process-name (process-list)))
    (delete-process "ride"))
  (when (get-buffer "*ride-dbg*")
    (kill-buffer "*ride-dbg*"))
  (when (get-buffer "*ws-explorer*")
    (kill-buffer "*ws-explorer*"))
  (setq msg-acc ""))

(defun ride-completion-at-point ()
  "Completion at point function for ride-mode."
  (let* ((inhibit-field-text-motion t)
         (line (thing-at-point 'line t))
         (pos (current-column)))
    (ride-send ride-client `("GetAutocomplete"
                           (:line ,line
                                  :pos ,pos
                                  :token 1)))
    (accept-process-output ride-client nil 2)
    (let ((completion-list (mapcar #'identity ride-completion-res)))
      (list (- (point) ride-completion-skip)
            (point) completion-list))))

(provide 'ride-mode)
