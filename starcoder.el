;;; starcoder.el --- Starcoder completion

;; Copyright (C) 2023 Sergey Kostyaev, all rights reserved.

;; Author: Sergey Kostyaev <feo.me@ya.ru>
;; Keywords: languages
;; Url: https://github.com/s-kostyaev/starcoder.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (websocket "1.13.1"))

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

;; This package provides autocompletion for different programming languages
;; based on https://github.com/bigcode-project/starcoder and
;; https://github.com/oobabooga/text-generation-webui streaming api.

;;; Code:
(require 'websocket)
(require 'json)

(defcustom starcoder-streaming-server-uri "ws://127.0.0.1:5005/api/v1/stream"
  "Text-generation-webui streaming server uri.")

(setq starcoder--ws nil)

(defun starcoder-connect ()
  "Connect to text-generation-webui."
  (if (and starcoder--ws
	   (websocket-p starcoder--ws)
	   (websocket-openp starcoder--ws))
      nil
    (setq starcoder--ws
	  (websocket-open
	   starcoder-streaming-server-uri
	   :on-message (lambda (_websocket frame)
			 (let ((text
				(alist-get
				 'text
				 (json-read-from-string
				  (websocket-frame-text frame)))))
			   (if text (insert text)
			     (message "starcoder completion done"))))
	   :on-close (lambda (_websocket)
		       (message "starcoder webskocket closed"))))))

(defun starcoder--get-prompt-python ()
  "Create prompt for starcoder completion for python files."
  (concat
   (if (buffer-file-name)
       (concat
	"<filename>"
	(file-name-nondirectory (buffer-file-name))
	"\n")
     "")
   "<fim_prefix>"
   (buffer-substring-no-properties (point-min) (point))
   "<fim_suffix>"
   (buffer-substring-no-properties (point) (point-max))
   "<fim_middle>"))

(defun starcoder--get-prompt-base ()
  "Create prompt for starcoder completion for non-python code."
  (buffer-substring-no-properties (point-min) (point)))

(defun starcoder--get-prompt ()
  "Create prompt for starcoder completion."
  (if (equal major-mode 'python-mode)
      (starcoder--get-prompt-python)
    (starcoder--get-prompt-base)))

(defun starcoder-complete ()
  "Complete code at point with starcoder."
  (interactive)
  (starcoder-connect)
  (websocket-send-text
   starcoder--ws
   (json-encode (list (cons 'prompt (starcoder--get-prompt))))))

(provide 'starcoder)
;;; starcoder.el ends here.
