(require 'epc)

(defvar chatgpt-process nil
  "The ChatGPT process.")

;;;###autoload
(defun chatgpt-init ()
  "Initialize the ChatGPT server.

This function creates the ChatGPT process and starts it. It also
initializes the ChatGPT buffer, enabling visual line mode in it. A
message is displayed to indicate that the initialization was
successful.

If ChatGPT server is not initialized, chatgpt-query calls this
function."
  (interactive)
  (setq chatgpt-process (epc:start-epc "python" '("chatgpt-emacs.py")))
  (with-current-buffer (get-buffer-create "*ChatGPT*")
    (visual-line-mode 1))
  (message "ChatGPT initialized."))

;;; `chatgpt-enable-loading-ellipsis'
;;;
;;; This variable controls whether the loading ellipsis animation is displayed
;;; in the ChatGPT buffer while waiting for a response from the ChatGPT
;;; process. The default value is `t`, which enables the animation. To disable
;;; the animation, set this variable to `nil`.
(defvar chatgpt-enable-loading-ellipsis t
  "Controls whether the loading ellipsis animation is displayed in the ChatGPT buffer.")

;;; `chatgpt-waiting-dot-timer'
;;;
;;; This variable holds the timer used to update the waiting message in the
;;; ChatGPT buffer with a dot (.) every 0.5 seconds. This creates an
;;; animation that indicates to the user that the ChatGPT process is still
;;; waiting for a response. The default value of this variable is `nil`,
;;; which means that the timer is not running. The timer is started when the
;;; `chatgpt-query` function is called, and is stopped when a response is
;;; received from the ChatGPT process.
(defvar chatgpt-waiting-dot-timer nil
  "Timer to update the waiting message in the ChatGPT buffer.")

;;;###autoload
(defun chatgpt-display ()
  "Displays the ChatGPT buffer and centers the max-point if it is not in the current view."
  (display-buffer "*ChatGPT*")
  (when-let ((saved-win (get-buffer-window (current-buffer)))
             (win (get-buffer-window "*ChatGPT*")))
    (unless (equal (current-buffer) (get-buffer "*ChatGPT*"))
      (select-window win)
      (goto-char (point-max))
      (unless (pos-visible-in-window-p (point-max) win)
        (goto-char (point-max))
        (recenter))
      (select-window saved-win))))

;;;###autoload
(defun chatgpt--newline-twice ()
  (newline)
  (newline))

;;;###autoload
(defun chatgpt--delete-line ()
  (beginning-of-line)
  (kill-line)
  (setq kill-ring (cdr kill-ring)))

;;;###autoload
(defun chatgpt-query (query)
  "Query ChatGPT with the provided string.

The user will be prompted to enter a query if none is provided. If
there is an active region, the user will be prompted to select the
type of query to perform.

Supported query types are:

* doc: Ask for documentation in query
* bug: Find bug in query
* improve: Suggestions for improving code
* understand: Query for understanding code or behavior"
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Query: "))))
  ;; add support for region here, based on modes
  (if (region-active-p)
      (let ((query-type (completing-read "Type of Query: " '(doc bug improve understand))))
        (chatgpt-query-by-type query query-type))
    (chatgpt--query query)))

;;;###autoload
(defun chatgpt--query (query)
  "Send a query to the ChatGPT process.

The query is inserted into the *ChatGPT* buffer with bold text,
and the response from the ChatGPT process is appended to the
buffer. If chatgpt-enable-loading-ellipsis is non-nil, a loading
ellipsis is displayed in the buffer while waiting for the
response.

This function is intended to be called internally by the
chatgpt-query function, and should not be called directly by
users."
  (unless chatgpt-process
    (chatgpt-init))
  (with-current-buffer (get-buffer-create "*ChatGPT*")
    (save-excursion
      (goto-char (point-max))
      (unless (equal (thing-at-point 'line) "\n")
        (chatgpt--delete-line))
      (insert (propertize query 'face 'bold))
      (chatgpt--newline-twice)
      (unless chatgpt-enable-loading-ellipsis
        (insert (concat "Waiting for ChatGPT...")))))
  (when chatgpt-enable-loading-ellipsis
    (setq chatgpt-waiting-dot-timer
          (run-with-timer 0.5 0.5
                          (lambda ()
                            (with-current-buffer (get-buffer-create "*ChatGPT*")
                              (save-excursion
                                (goto-char (point-max))
                                (let ((line (thing-at-point 'line)))
                                  (when (>= (length line) 3)
                                    (chatgpt--delete-line))
                                  (insert "."))))))))
  (chatgpt-display)
  (deferred:$
   (epc:call-deferred chatgpt-process 'query (list query))
   (deferred:nextc it
     (lambda (response)
       (let ((message (plist-get response :message)))
         (when chatgpt-enable-loading-ellipsis
           (cancel-timer chatgpt-waiting-dot-timer))
         (with-current-buffer (get-buffer-create "*ChatGPT*")
           (save-excursion
             (goto-char (point-max))
             (chatgpt--delete-line)
             (insert message)
             (chatgpt--newline-twice)))
         (chatgpt-display))))))

;;;###autoload
(defun chatgpt-stop ()
  (interactive)
  (cancel-timer chatgpt-waiting-dot-timer)
  (epc:stop-epc chatgpt-process)
  (setq chatgpt-process nil)
  (message "Stop ChatGPT process."))
