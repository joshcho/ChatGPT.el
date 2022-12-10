;;; chatgpt-emacs.el --- Emacs interface for ChatGPT  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jungmin "Josh" Cho

;; Author: Jungmin "Josh" Cho
;; Keywords: chat, chatgpt

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The chatgpt-emacs package provides an Emacs interface for the ChatGPT large
;; language model. The package allows users to initialize the ChatGPT system and
;; query it for responses to natural language queries. The package also provides
;; keybindings and other convenience features for a seamless integration with
;; Emacs.

;;; Code:

(require 'lispy)

(defvar chatgpt-config nil
  "The configuration for the ChatGPT system.")

(defun chatgpt-init ()
  (interactive)
  (lispy--eval-python (string-join '("from revChatGPT.revChatGPT import Chatbot"
                                     "path = os.path.expanduser(CONFIG_PATH)"
                                     "with open(path, 'r') as file:
    config = json.load(file)")
                                   "\n"))
  (message "Initialize ChatGPT."))

(defun chatgpt-query (query)
  (interactive "sChatGPT Query: ")
  (let ((json-response
         (lispy--eval-python (format "chatbot.get_chat_response(\"%s\")" query))))
    (string-match "'message': \"\\([^\"]*\\)" json-response)
    (message (match-string 1 json-response))))

(provide 'chatgpt-emacs)
;;; chatgpt-emacs.el ends here
