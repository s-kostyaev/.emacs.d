;;; counsel-erl --- Erlang completion at point using ivy.

;;; Commentary:
;; Deps:
;;  - coreutils
;;  - findutils
;;  - grep
;;  - sed


;;; Code:
(require 'counsel)
(require 'subr-x)
(require 'dash)

(defvar counsel-erl-erlang-root "/usr/lib/erlang"
  "Path to erlang root.")

(defvar counsel-erl-project-root)

(defvar-local counsel-erl-candidates nil
  "Candidates for completion.")

(defvar-local counsel-erl-predicate nil
  "Completion predicate.")

(defun counsel-erl--find-functions (module)
  "Find functions in MODULE."
  (-remove #'string-empty-p
   (split-string
    (shell-command-to-string
     (concat
      "find "
      counsel-erl-project-root
      " "
      counsel-erl-erlang-root
      " -name "
      module
      ".erl | xargs sed -n '/-export(/,/)./p' | sed -e '/%/d' | sed -e 's/ //g' | sed -e 's/\\t//g' | sed -e 's/-export.*(\\\[//g' | sed -e 's/\\\]).//g' | sed 's/\\\,/\\\n/g' | sed '/^$/d'"))
    "\n")))

(defun counsel-erl--find-modules ()
  "Find functions in MODULE."
  (-remove #'string-empty-p
   (split-string
    (shell-command-to-string
     (concat
      "find "
      counsel-erl-project-root
      " "
      counsel-erl-erlang-root
      " -iname '*.erl' | xargs basename -a | sed -e 's/\\.erl//g'"))
    "\n")))

(defun counsel-erl--extract-functions (file)
  "Extract all functions from FILE."
  (-remove #'string-empty-p
   (split-string
    (shell-command-to-string
     (concat
      "sed -n '/^[a-z][a-zA-Z0-9_]*(.*)/,/[[:space:]]*->/p' "
      file
      " | sed -e '/%/d' | sed -e '/^\\\-/d' | sed -e '/^[[:space:]]/d' | sed '/^$/d' | sed -e 's/).*/)/g'"))
    "\n")))

(defun counsel-erl--set-arity (erl-function)
  "Set arity to ERL-FUNCTION instead of arglist."
  (let ((arity
         (string-trim
          (shell-command-to-string
           (concat
            "echo '" erl-function
            "' | sed -e 's/.*(//g' | sed -e 's/)//g' | sed -e 's/,/\\n/g' | sed -e '/^$/d' | wc -l")))))
    (when
        (string-match "[^(]+" erl-function)
        (concat (substring erl-function (match-beginning 0) (match-end 0)) "/" arity))))

(defun counsel-erl--find-local-functions ()
  "Find all local functions."
  (mapcar #'counsel-erl--set-arity
          (counsel-erl--extract-functions (buffer-file-name))))

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
  (if (string-match "\\([^/]+\\)/\\([0-9]+\\)" candidate)
      (let ((arity (string-to-number
                    (substring candidate
                               (match-beginning 2) (match-end 2)))))
        (ivy-completion-in-region-action
         (concat (replace-regexp-in-string "/[0-9]+" "" candidate)
                 "("
                 (make-string (if (= 0 arity) arity (- arity 1)) ?,)
                 ")"))
        (goto-char (- (point) arity)))
    (ivy-completion-in-region-action (concat candidate ":"))))

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
       (setq counsel-erl-candidates (append (counsel-erl--find-local-functions)
                                            (counsel-erl--find-modules)
                                            (counsel-erl--find-functions "erlang")))
       (setq counsel-erl-predicate thing))))
  (when (looking-back counsel-erl-predicate (line-beginning-position))
    (setq ivy-completion-beg (match-beginning 0))
    (setq ivy-completion-end (match-end 0)))
  (ivy-read "Counsel-erl cand:" counsel-erl-candidates
            :initial-input counsel-erl-predicate
            :action #'counsel-erl--insert-candidate))



(provide 'counsel-erl)
;;; counsel-erl.el ends here
