;;; counsel-erl --- Erlang completion at point using ivy.

;;; Commentary:

;;; Code:
(require 'counsel)
(require 'subr-x)

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
  (when (thing-at-point-looking-at "[^=^+^-^ ^(^)^,^.^\n]+")
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

(defun counsel-erl--insert-candidate (candidate)
  "Insert CANDIDATE at point."
  (ivy-completion-in-region-action
   (replace-regexp-in-string "/[0-9]" "" candidate)))

;;;###autoload
(defun counsel-erl ()
  "Erlang completion at point."
  (interactive)
  (let ((thing (counsel-erl-at-point)))
   (if (and thing (string-match "\\([^\:]+\\)\:\\([^\:]*\\)" thing))
       (let ((erl-prefix (substring thing (match-beginning 1) (match-end 1))))
         (progn
           (setq counsel-erl-candidates
                 (counsel-erl--find-functions
                  erl-prefix))
           (setq counsel-erl-predicate
                 (string-remove-prefix (concat erl-prefix ":") thing))))
     (progn
       (setq counsel-erl-candidates (counsel-erl--find-modules))
       (setq counsel-erl-predicate thing))))
  (when (looking-back counsel-erl-predicate (line-beginning-position))
    (setq ivy-completion-beg (match-beginning 0))
    (setq ivy-completion-end (match-end 0)))
  (ivy-read "Counsel-erl cand:" (split-string counsel-erl-candidates "\n")
            :initial-input counsel-erl-predicate
            :action #'counsel-erl--insert-candidate))



(provide 'counsel-erl)
;;; counsel-erl.el ends here