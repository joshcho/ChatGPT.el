;;; chatgpt.el --- ChatGPT in Emacs

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Jungmin "Josh" Cho <joshchonpc@gmail.com>
;; Version: 0.2
;; Package-Requires: ((polymode "0.2.2"))
;; Keywords: ai, openai, chatgpt, assistant
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
    (define-key map (kbd "RET") #'chatgpt-send-input)
    map)
  "Basic mode map for `run-chatgpt'.")

(defun get-chatgpt-buffer ()
  "Find and return the buffer with name matching '*ChatGPT*[lang]' or '*ChatGPT*'."
  (let ((buffers (buffer-list)))
    (while (and buffers
                (not (string-match "\\*ChatGPT\\*\\(\\[.*\\]\\)?"
                                   (buffer-name (car buffers)))))
      (setq buffers (cdr buffers)))
    (car buffers)))

;;;###autoload
(defun chatgpt-run ()
  "Run an inferior instance of `chatgpt-cli' inside Emacs."
  (interactive)
  (unless (getenv "OPENAI_API_KEY")
    (error "Please set the environment variable \"OPENAI_API_KEY\" to your API key"))
  (let* ((buffer
          (or (get-chatgpt-buffer)
              (get-buffer-create "*ChatGPT*")))
         (proc-alive (comint-check-proc buffer)))
    ;; if process is dead, recreate buffer and reset mode
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "ChatGPT" buffer
               chatgpt-cli-file-path nil chatgpt-cli-arguments)
        (chatgpt-mode)
        (let ((continue-loop t)
              (end-time (+ (float-time (current-time))
                           chatgpt--load-wait-time-in-secs)))
          ;; block until ready
          (while (and continue-loop (< (float-time (current-time)) end-time))
            (with-current-buffer (get-chatgpt-buffer)
              (sleep-for 0.1)
              (when (string-match-p (concat chatgpt-prompt-regexp
                                            "[[:space:]]*$")
                                    (buffer-string))
                ;; output response from /read command received
                (setq continue-loop nil))))
          (when continue-loop
            (message
             (format
              "No response from chatgpt-wrapper after %d seconds"
              chatgpt--load-wait-time-in-secs))))))
    ;; Regardless, provided we have a valid buffer, we pop to it.
    (pop-to-buffer buffer)))

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
   `(,(concat "\\_</" (regexp-opt chatgpt-cmds) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `chatgpt-mode'.")

(defcustom chatgpt-code-query-map
  '(("bug" . "There is a bug in the following, please help me fix it.")
    ("doc" . "Please write the documentation for the following.")
    ("improve" . "Please improve the following.")
    ("understand" . "What is the following?")
    ("refactor" . "Please refactor the following.")
    ("suggest" . "Please make suggestions for the following."))
  "An association list that maps query types to their corresponding query."
  :type '(alist :key-type (string :tag "Query Type")
                :value-type (string :tag "Query String"))
  :group 'chatgpt)

(defcustom chatgpt-display-on-query t
  "Whether *ChatGPT* is displayed when a query is sent."
  :type 'boolean
  :group 'chatgpt)

(defvar chatgpt--load-wait-time-in-secs 10)

;; (mapc 'cancel-timer timer-list)
(defun chatgpt--query (query &optional code)
  (let ((mode major-mode))
    (with-current-buffer (get-chatgpt-buffer)
      (comint-kill-input)
      (if (or code (string-match "\n" query))
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "/read")
            (call-interactively #'comint-send-input)
            (let ((continue-loop t)
                  (end-time (+ (float-time (current-time))
                               chatgpt--load-wait-time-in-secs)))
              (while (and continue-loop (< (float-time (current-time)) end-time))
                (with-current-buffer (get-chatgpt-buffer)
                  (sleep-for 0.1)
                  (when (string-match-p "/end. *\n\n$" (buffer-string))
                    ;; output response from /read command received
                    (setq continue-loop nil)
                    (let ((inhibit-read-only t))
                      (insert query)
                      (if (not code)
                          (progn
                            (insert "\n/end")
                            (call-interactively #'comint-send-input))
                        (let ((saved-point (point)))
                          ;; if possible, don't refactor this. very fragile,
                          ;; and probably won't change in the way that you expect
                          (insert code)
                          (insert "\n/end")
                          (call-interactively #'comint-send-input)
                          (save-excursion
                            (goto-char saved-point)
                            (remove-list-of-text-properties (point) (+ (point)
                                                                       (length code))
                                                            '(font-lock-face))
                            (chatgpt--insert-syntax-highlight
                             (buffer-substring saved-point(+ saved-point (length code)))
                             mode)
                            (delete-region (point)
                                           (+ (point) (length code))))))))))
              (when continue-loop
                (message
                 (format
                  "No response from chatgpt-wrapper after %d seconds"
                  chatgpt--load-wait-time-in-secs)))))
        ;; Else, send the query directly
        (comint-simple-send
         (get-buffer-process (get-chatgpt-buffer))
         query)))
    ;; Display the chatgpt buffer if necessary
    (when chatgpt-display-on-query
      (pop-to-buffer (get-chatgpt-buffer))
      ;; (goto-char (point-max))
      ;; (unless (pos-visible-in-window-p (point))
      ;;   (recenter))
      )))

;;;###autoload
(defun chatgpt--code-query (code)
  ;; assumes *ChatGPT* is alive
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-from-minibuffer "ChatGPT Query: "))))
  (let* ((query-type (completing-read "Type of Query: "
                                      (cons "custom"
                                            (mapcar #'car chatgpt-code-query-map))))
         (query
          (format "%s\n\n"
                  (cond ((assoc query-type chatgpt-code-query-map)
                         (cdr (assoc query-type chatgpt-code-query-map)))
                        ((equal query-type "custom")
                         (read-from-minibuffer "ChatGPT Custom Prompt: "))
                        (t query-type)))))
    (chatgpt--query query code)))

(defvar chatgpt--in-code-block nil)

;;;###autoload
(defun chatgpt-send-input ()
  (interactive)
  (call-interactively #'comint-send-input))

;;;###autoload
(defun chatgpt-query ()
  (interactive)
  (save-window-excursion
    (chatgpt-run))
  (if (region-active-p)
      (chatgpt--code-query
       (buffer-substring-no-properties (region-beginning) (region-end)))
    (chatgpt--query (read-from-minibuffer "ChatGPT Query: ")))
  (when chatgpt-display-on-query
    (pop-to-buffer (get-chatgpt-buffer))))

(defun chatgpt--insert-syntax-highlight (text mode)
  (cl-flet ((fontify-using-faces
              (text)
              (let ((pos 0)
                    next)
                (while (setq next (next-single-property-change pos 'face text))
                  (put-text-property pos next 'font-lock-face
                                     (get-text-property pos 'face text) text)
                  (setq pos next))
                (add-text-properties 0  (length text) '(fontified t) text)
                text)))
    (insert
     (fontify-using-faces
      (with-temp-buffer
        (insert text)
        (funcall mode)
        (font-lock-ensure)
        (buffer-string))))))

(defun chatgpt--string-match-positions (regexp str)
  "Find positions of all matches of REGEXP in STR."
  (let ((pos 0)
        matches)
    (while (string-match regexp str pos)
      (setq matches (cons (match-beginning 0) matches))
      (setq pos (match-end 0)))
    (nreverse matches)))

(require 'polymode)
(define-hostmode poly-chatgpt-hostmode
  :mode 'chatgpt-mode)

(define-auto-innermode poly-chatgpt-fenced-code-innermode
  :head-matcher (cons "^[ \t]*\\(```{?[[:alpha:]].*\n\\)" 1)
  :tail-matcher (cons "\\(^[ \t]*\\(```\\)\\|Request to interrupt streaming\\)[ \t]*$" 1)
  :mode-matcher (cons "```[ \t]*{?\\(?:lang *= *\\)?\\([^ \t\n;=,}]+\\)" 1)
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-chatgpt-mode
  :hostmode 'poly-chatgpt-hostmode
  :innermodes '(poly-chatgpt-fenced-code-innermode))

(add-hook 'chatgpt-mode-hook #'poly-chatgpt-mode)

(provide 'chatgpt)
;;; chatgpt.el ends here
