;;; counsel-erl --- Erlang completion at point using ivy.

;;; Commentary:

;;; Code:
(require 'counsel)

(defvar counsel-erl-erlang-root "/usr/lib/erlang"
  "Path to erlang root.")

(defvar counsel-erl-project-root)

(defvar-local counsel-erl-candidates nil
  "Candidates for completion.")

(defvar-local counsel-erl-predicate nil
  "Completion predicate.")

(defun counsel-erl--find-functions (module)
  "Find functions in MODULE."
  (shell-command-to-string
   (concat
    "find "
    counsel-erl-project-root
    " "
    counsel-erl-erlang-root
    " -name "
    module
    ".erl | xargs sed -n '/-export(/,/)./p' | sed -e '/%/d' | sed -e 's/ //g' | sed -e 's/\\t//g' | sed -e 's/-export.*(\\\[//g' | sed -e 's/\\\]).//g' | sed 's/\\\,/\\\n/g' | sed '/^$/d'")))

(defun counsel-erl--find-modules ()
  "Find functions in MODULE."
  (shell-command-to-string
   (concat
    "find "
    counsel-erl-project-root
    " "
    counsel-erl-erlang-root
    " -iname '*.erl' | xargs basename -a | sed -e 's/\\.erl//g'")))

(defun counsel-erl-at-point ()
  "Return the erlang thing at point, or nil if none is found."
  (when (thing-at-point-looking-at "[^=^+^-^ ^(^)^,^.]+")
    (match-string-no-properties 0)))

;;;###autoload
(defun counsel-erl-set-project-root ()
  "Set root for current project."
  (interactive)
  (let
      ((dir
        (expand-file-name (read-directory-name
                           "Select project directory:" default-directory))))
    (setq counsel-erl-project-root dir)))


;;;###autoload
(defun counsel-erl ()
  "Erlang completion at point."
  (interactive)
  (let ((thing (counsel-erl-at-point)))
   (if (string-match "\\([^\:]+\\)\:\\([^\:]+\\)" thing)
       (progn
         (setq-local counsel-erl-candidates
                     (counsel-erl--find-functions
                      (substring thing (match-beginning 1) (match-end 1))))
         (setq-local counsel-erl-predicate
                     (substring thing (match-beginning 2) (match-end 2))))
     (progn
       (setq-local counsel-erl-candidates (counsel-erl--find-modules))
       (setq-local counsel-erl-predicate thing))))
  (when (looking-back counsel-erl-predicate (line-beginning-position))
    (setq ivy-completion-beg (match-beginning 0))
    (setq ivy-completion-end (match-end 0)))
  (ivy-read "Counsel-erl cand:" (split-string counsel-erl-candidates "\n")
            :initial-input counsel-erl-predicate
            :action (lambda (x)
                        (ivy-completion-in-region-action
                         (replace-regexp-in-string "/[0-9]" "" x)))))



(provide 'counsel-erl)
;;; counsel-erl.el ends here
