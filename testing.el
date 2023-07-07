;;; chatgpt.el --- ChatGPT in Emacs

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Jungmin "Josh" Cho <joshchonpc@gmail.com>
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: ai
;; URL: https://github.com/joshcho/ChatGPT.el

;;; Commentary:

;; This package provides an interactive interface with ChatGPT API.

;;; Code:

(require 'cl-lib)
(require 'comint)
(defgroup chatgpt nil
  "Configuration for chatgpt."
  :prefix "chatgpt-"
  :group 'ai)

(defvar chatgpt-cli-file-path (replace-regexp-in-string
                               "\n$" "" (shell-command-to-string "which chatgpt")))
(defvar chatgpt-cli-arguments '())
(defvar chatgpt-prompt-regexp "^[^@]+@[^@]+>"
  "Prompt for `run-chatgpt'.")

(defconst chatgpt-cmds '("ask" "chat" "config" "context" "copy" "delete" "echo" "editor" "exit" "file" "functions" "help" "history" "log" "login" "logout" "max-submission-tokens" "model" "nav" "new" "preset-delete" "preset-edit" "preset-load" "preset-save" "preset-show" "presets" "provider" "providers" "quit" "read" "stream" "switch" "system-message" "template" "template-copy" "template-delete" "template-edit" "template-edit-run" "template-prompt-edit-run" "template-prompt-run" "template-run" "templates" "title" "user" "user-delete" "user-edit" "user-login" "user-logout" "user-register" "users" "workflow-delete" "workflow-edit" "workflow-run" "workflow-show" "workflows"))

(defun chatgpt-completion-at-point ()
  "Completes / commands interactively"
  (interactive)
  (let ((line
         (buffer-substring (line-beginning-position) (point))))
    (if-let (filtered-cmds
             (when (string-match "\\s-*/\\([^ ]*\\)$" line)
               (cl-remove-if-not (lambda (str)
                                   (or
                                    (string-prefix-p
                                     (match-string 1 line)
                                     str)
                                    (equal str
                                           (match-string 1 line))))
                                 chatgpt-cmds)))
        (insert
         (substring
          (if (= (length filtered-cmds) 1)
              (car filtered-cmds)
            (completing-read "Select cmd: " filtered-cmds))
          (length (match-string 1 line))))
      (call-interactively #'completion-at-point))))

(defvar chatgpt-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" #'chatgpt-completion-at-point)
    map)
  "Basic mode map for `run-chatgpt'.")
(defvar chatgpt-buffer-name "*ChatGPT*")

;;;###autoload
(defun run-chatgpt ()
  "Run an inferior instance of `chatgpt-cli' inside Emacs."
  (interactive)
  (let* ((buffer (get-buffer-create chatgpt-buffer-name))
         (proc-alive (comint-check-proc buffer)))
    ;; if process is dead, recreate buffer and reset mode
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "ChatGPT" buffer
               chatgpt-cli-file-path nil chatgpt-cli-arguments)
        (chatgpt-mode)))
    ;; Regardless, provided we have a valid buffer, we pop to it.
    (when buffer
      (pop-to-buffer buffer))))

(defun chatgpt--initialize ()
  "Helper function to initialize ChatGPT."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode chatgpt-mode comint-mode "ChatGPT"
  "Major mode for `run-chatgpt'.

\\<chatgpt-mode-map>"
  (setq comint-prompt-regexp chatgpt-prompt-regexp)
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(chatgpt-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) chatgpt-prompt-regexp))

(add-hook 'chatgpt-mode-hook 'chatgpt--initialize)

(defvar chatgpt-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "/" (regexp-opt chatgpt-cmds)) . font-lock-keyword-face))
  "Additional expressions to highlight in `chatgpt-mode'.")

(defcustom chatgpt-query-format-string-map
  '(("doc" . "Please write the documentation for the following function.\n\n%s")
    ("bug" . "There is a bug in the following function, please help me fix it.\n\n%s")
    ("understand" . "What is the following?\n\n%s")
    ("improve" . "Please improve the following.\n\n%s"))
  "An association list that maps query types to their corresponding format strings."
  :type '(alist :key-type (string :tag "Query Type")
                :value-type (string :tag "Format String"))
  :group 'chatgpt)

(defcustom chatgpt-display-on-query t
  "Whether *ChatGPT* is displayed when a query is sent."
  :type 'boolean
  :group 'chatgpt)

;; (defcustom chatgpt-repo-path nil
;;   "The path of ChatGPT.el repository."
;;   :type 'string
;;   :group 'chatgpt)

;;;###autoload
(defun chatgpt-display ()
  "Displays *ChatGPT*."
  (interactive)
  (display-buffer chatgpt-buffer-name)
  (when-let ((saved-win (get-buffer-window (current-buffer)))
             (win (get-buffer-window chatgpt-buffer-name)))
    (unless (equal (current-buffer) (get-buffer chatgpt-buffer-name))
      (select-window win)
      (goto-char (point-max))
      (unless (pos-visible-in-window-p (point-max) win)
        (goto-char (point-max))
        (recenter))
      (select-window saved-win))))

(defun chatgpt--query (query)
  ;; face error
  (with-current-buffer (get-buffer chatgpt-buffer-name)
    (comint-kill-input)
    (if (string-match "\n" query)
        (progn
          ;; shouldn't need the read-only hack after chatgpt-wrapper fork
          (setq comint-prompt-read-only nil)
          (with-current-buffer chatgpt-buffer-name
            (insert "/read")
            (call-interactively #'comint-send-input)
            (insert query)
            (insert "\n/end")
            (call-interactively #'comint-send-input))
          (setq comint-prompt-read-only t))
      (comint-simple-send
       (get-buffer-process chatgpt-buffer-name)
       query))))

(defun chatgpt--query-by-type (query query-type)
  (if (equal query-type "custom")
      (chatgpt--query
       (format "%s\n\n%s" (read-from-minibuffer "ChatGPT Custom Prompt: ") query))
    (if-let (format-string (cdr (assoc query-type chatgpt-query-format-string-map)))
        (chatgpt--query
         (format format-string query))
      (error "No format string associated with 'query-type' %s. Please customize 'chatgpt-query-format-string-map'" query-type))))

;;;###autoload
(defun chatgpt-query-by-type (query)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Query: "))))
  (let* ((query-type (completing-read "Type of Query: " (cons "custom" (mapcar #'car chatgpt-query-format-string-map)))))
    (if (or (assoc query-type chatgpt-query-format-string-map)
            (equal query-type "custom"))
        (chatgpt--query-by-type query query-type)
      (chatgpt--query (format "%s\n\n%s" query-type query)))))

;;;###autoload
(defun chatgpt-query (query)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Query: "))))
  (if (region-active-p)
      (chatgpt-query-by-type query)
    (chatgpt--query query))
  ;; (chatgpt-display)
  )

(defvar in-code-block nil
  "Variable to track if we are currently in a code block.")

(defvar finished-code-blocks 0
  "Variable to track the number of finished code blocks.")

(defun chatgpt-comint-process-filter (proc string)
  "Custom comint process filter.
PROC is the current process."
  (let ((buffer-substring (buffer-string)))
    (let ((start-blocks (length (string-match-positions "^```[a-z-]+" buffer-substring)))
          (end-blocks (length (string-match-positions "^```\n" buffer-substring))))
      (when (and (> start-blocks finished-code-blocks)
                 (not in-code-block))
        (setq in-code-block t)
        (message "In a code block"))
      (when (and in-code-block (= start-blocks end-blocks))
        (setq in-code-block nil)
        (setq finished-code-blocks start-blocks)
        (message "Finished code block"))))
  (comint-output-filter proc string))

(defun string-match-positions (regexp str)
  "Find positions of all matches of REGEXP in STR."
  (let ((pos 0)
        matches)
    (while (string-match regexp str pos)
      (setq matches (cons (match-beginning 0) matches))
      (setq pos (match-end 0)))
    (nreverse matches)))

(add-hook 'chatgpt-mode-hook
          (lambda ()
            (set-process-filter (get-buffer-process (current-buffer))
                                'chatgpt-comint-process-filter)))

(provide 'chatgpt)
;;; chatgpt.el ends here
