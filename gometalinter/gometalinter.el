;; Modified golint.el to provide gometalinter

(require 'compile)

(defun go-lint-buffer-name (mode)
  "*Gometalinter*")

(defun gometalinter-process-setup ()
  "Setup compilation variables and buffer for `gometalinter'."
  (run-hooks 'gometalinter-setup-hook))

(define-compilation-mode gometalinter-mode "gometalinter"
  "Gometalinter is a linter for Go source code."
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-process-setup-function)
       'gometalinter-process-setup)
  )

;;;###autoload
(defun gometalinter ()
  "Run gometalinter on the current file and populate the fix list. Pressing C-x ` will jump directly to the line in your code which caused the first message."
  (interactive)
  (compilation-start
   (mapconcat #'shell-quote-argument
	      (list "gometalinter" "--cyclo-over=15") " ")
   'gometalinter-mode))

(provide 'gometalinter)
