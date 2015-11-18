;; Modified golint.el to provide gobuild

(require 'compile)

(defun go-build-buffer-name (mode)
  "*Gobuild*")

(defun gobuild-process-setup ()
  "Setup compilation variables and buffer for `gobuild'."
  (run-hooks 'gobuild-setup-hook))

(define-compilation-mode gobuild-mode "gobuild"
  "Gobuild is a builder for Go source code."
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-process-setup-function)
       'gobuild-process-setup)
  )

;;;###autoload
(defun gobuild ()
  "Run gobuild on the current directory and populate the fix list. Pressing C-x ` will jump directly to the line in your code which caused the first message."
  (interactive)
  (compilation-start
   (mapconcat #'shell-quote-argument
	      (list "go" "build") " ")
   'gobuild-mode))

(provide 'gobuild)
