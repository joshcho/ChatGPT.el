;;; chatgpt.el --- ChatGPT in Emacs

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Jungmin "Josh" Cho <joshchonpc@gmail.com>
;; Version: 0.1
;; Package-Requires: ((epc "0.1.1") (deferred "0.5.1"))
;; Keywords: ai
;; URL: https://github.com/joshcho/ChatGPT.el

;;; Commentary:

;; This package provides an interactive interface with ChatGPT API.

(require 'epc)
(require 'deferred)

;;; Code:

(defgroup chatgpt nil
  "Configuration for chatgpt."
  :prefix "chatgpt-"
  :group 'ai)

(defcustom chatgpt-query-format-string-map
  '(("doc" . "Please write the documentation for the following function.\n\n%s")
    ("bug" . "There is a bug in the following function, please help me fix it.\n\n%s")
    ("understand" . "What is the following?\n\n%s")
    ("improve" . "Please improve the following.\n\n%s"))
  "An association list that maps query types to their corresponding format strings."
  :type '(alist :key-type (string :tag "Query Type")
                :value-type (string :tag "Format String"))
  :group 'chatgpt)

(defcustom chatgpt-enable-loading-ellipsis t
  "Whether the ellipsis animation is displayed in *ChatGPT*."
  :type 'boolean
  :group 'chatgpt)

(defcustom chatgpt-display-on-query t
  "Whether *ChatGPT* is displayed when a query is sent."
  :type 'boolean
  :group 'chatgpt)

(defcustom chatgpt-display-on-response t
  "Whether *ChatGPT* is displayed when a response is received."
  :type 'boolean
  :group 'chatgpt)

(defcustom chatgpt-python-interpreter "python"
  "The Python interpreter used for ChatGPT."
  :type 'string
  :group 'chatgpt)

(defvar chatgpt-process nil
  "The ChatGPT process.")

(defcustom chatgpt-repo-path "~/.emacs.d/straight/repos/ChatGPT.el/"
  "The path of ChatGPT.el repository."
  :type 'string
  :group 'chatgpt)

;;;###autoload
(defun chatgpt-login ()
  "Log in to ChatGPT."
  (interactive)
  (shell-command "chatgpt install &"))

;;;###autoload
(defun chatgpt-init ()
  "Initialize the ChatGPT server.

This function creates the ChatGPT process and starts it. It also
initializes the ChatGPT buffer, enabling visual line mode in it. A
message is displayed to indicate that the initialization was
successful.

If ChatGPT server is not initialized, `chatgpt-query' calls this
function."
  (interactive)
  (when (equal (shell-command-to-string "pip list | grep chatGPT") "")
    (shell-command "pip install git+https://github.com/mmabrouk/chatgpt-wrapper")
    (message "chatgpt-wrapper installed through pip.")
    (chatgpt-login))
  (setq chatgpt-process (epc:start-epc chatgpt-python-interpreter (list (expand-file-name (format "%schatgpt.py" chatgpt-repo-path)))))
  (with-current-buffer (get-buffer-create "*ChatGPT*")
    (visual-line-mode 1))
  (message "ChatGPT initialized."))

(defvar chatgpt-waiting-dot-timer nil
  )

(defvar chatgpt-wait-timers (make-hash-table)
  "Timers to update the waiting message in the ChatGPT buffer.")

;;;###autoload
(defun chatgpt-stop ()
  "Stops the ChatGPT server."
  (interactive)
  (chatgpt--stop-wait)
  (epc:stop-epc chatgpt-process)
  (setq chatgpt-process nil)
  (message "Stop ChatGPT process."))

;;;###autoload
(defun chatgpt-reset ()
  "Reset the ChatGPT server. The same session is maintained."
  (interactive)
  (chatgpt-stop)
  (chatgpt-init))

;;;###autoload
(defun chatgpt-display ()
  "Displays *ChatGPT*."
  (interactive)
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

(defun chatgpt--clear-line ()
  "Clear line in *ChatGPT*."
  (assert (equal (current-buffer) (get-buffer "*ChatGPT*")))
  (delete-region (save-excursion (beginning-of-line)
                                 (point))
                 (save-excursion (end-of-line)
                                 (point))))

(defun chatgpt--identifier-string (id)
  "Identifier string corresponding to ID."
  (format "[%s]" id))

(defun chatgpt--regex-string (id)
  "Regex corresponding to ID."
  (format "\\[%s\\]" id))

(defun chatgpt--goto-identifier (id)
  "Go to response of ID."
  (assert (equal (current-buffer) (get-buffer "*ChatGPT*")))
  (goto-char (point-min))
  (re-search-forward (chatgpt--regex-string id))
  (next-line))

(defun chatgpt--insert-query (query id)
  "Insert QUERY with ID into *ChatGPT*."
  (with-current-buffer (get-buffer-create "*ChatGPT*")
    (save-excursion
      (goto-char (point-max))
      (insert (format "\n\n%s\n%s\n%s"
                      (propertize query 'face 'bold)
                      (chatgpt--identifier-string id)
                      (if chatgpt-enable-loading-ellipsis
                          ""
                        (concat "Waiting for ChatGPT...")))))))

(defun chatgpt--insert-response (response id)
  "Insert RESPONSE into *ChatGPT* for ID."
  (with-current-buffer (get-buffer-create "*ChatGPT*")
    (save-excursion
      (chatgpt--goto-identifier id)
      (chatgpt--clear-line)
      (insert response))))

(defun chatgpt--stop-wait ()
  "Stop waiting for a response."
  (when chatgpt-waiting-dot-timer
    (cancel-timer chatgpt-waiting-dot-timer)
    (setq chatgpt-waiting-dot-timer nil)))

(defvar chatgpt-id 0
  "Tracks responses in the background.")

(defun chatgpt--query (query)
  "Send QUERY to the ChatGPT process.

The query is inserted into the *ChatGPT* buffer with bold text,
and the response from the ChatGPT process is appended to the
buffer. If `chatgpt-enable-loading-ellipsis' is non-nil, a loading
ellipsis is displayed in the buffer while waiting for the
response.

This function is intended to be called internally by the
`chatgpt-query' function, and should not be called directly by
users."
  (unless chatgpt-process
    (chatgpt-init))
  (let ((saved-id (incf chatgpt-id)))
    (chatgpt--insert-query query saved-id)
    (when chatgpt-enable-loading-ellipsis
      (setq chatgpt-waiting-dot-timer
            (run-with-timer 0.5 0.5
                            (lambda ()
                              (with-current-buffer (get-buffer-create "*ChatGPT*")
                                (save-excursion
                                  (chatgpt--goto-identifier saved-id)
                                  (let ((line (thing-at-point 'line)))
                                    (when (>= (length line) 3)
                                      (chatgpt--clear-line))
                                    (insert "."))))))))
    (when chatgpt-display-on-query
      (chatgpt-display))
    (deferred:$
     (epc:call-deferred chatgpt-process 'query (list query))
     (eval `(deferred:nextc it
              (lambda (response)
                (chatgpt--stop-wait)
                (chatgpt--insert-response response ,saved-id)
                (when chatgpt-display-on-response
                  (chatgpt-display))))))))

(defun chatgpt--query-by-type (query query-type)
  "Query ChatGPT with a given QUERY and QUERY-TYPE.

QUERY is the text to be passed to ChatGPT.

QUERY-TYPE is the type of query, which determines the format string
used to generate the final query sent to ChatGPT. The format string
is looked up in 'chatgpt-query-format-string-map', which is an
alist of QUERY-TYPE and format string pairs. If no matching
format string is found, an error is raised.

The format string is expected to contain a %s placeholder, which
will be replaced with QUERY. The resulting string is then passed
to ChatGPT.

For example, if QUERY is \"(defun square (x) (* x x))\" and
QUERY-TYPE is \"doc\", the final query sent to ChatGPT would be
\"Please write the documentation for the following function.
\(defun square (x) (* x x))\""
  (if (equal query-type "custom")
      (chatgpt--query
       (format "%s\n\n%s" (read-from-minibuffer "ChatGPT Custom Prompt: ") query))
    (if-let (format-string (cdr (assoc query-type chatgpt-query-format-string-map)))
        (chatgpt--query
         (format format-string query))
      (error "No format string associated with 'query-type' %s. Please customize 'chatgpt-query-format-string-map'" query-type))))

;;;###autoload
(defun chatgpt-query-by-type (query)
  "Query ChatGPT with from QUERY and interactively chosen 'query-type'.

The function uses the 'completing-read' function to prompt the
user to select the type of query to use. The selected query type
is passed to the 'chatgpt--query-by-type' function along with the
'query' argument, which sends the query to the ChatGPT model and
returns the response."
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Query: "))))
  (let* ((query-type (completing-read "Type of Query: " (cons "custom" (mapcar #'car chatgpt-query-format-string-map)))))
    (chatgpt--query-by-type query query-type)))

;;;###autoload
(defun chatgpt-query (query)
  "Query ChatGPT with QUERY.

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
  ;; (if chatgpt-waiting-dot-timer
  ;;     (message "Already waiting on a ChatGPT query. If there was an error with your previous query, try M-x chatgpt-reset")
  ;;   (if (region-active-p)
  ;;       (chatgpt-query-by-type query)
  ;;     (chatgpt--query query)))
  (if (region-active-p)
      (chatgpt-query-by-type query)
    (chatgpt--query query)))

(provide 'chatgpt)
;;; chatgpt.el ends here
