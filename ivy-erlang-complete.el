;;; ivy-erlang-complete --- Erlang completion at point using ivy.

;;; Commentary:
;; Deps:
;;  - coreutils
;;  - findutils
;;  - grep
;;  - sed


;;; Code:
(require 'ivy)
(require 'subr-x)
(require 'dash)
(require 's)

(defvar ivy-erlang-complete-erlang-root "/usr/lib/erlang"
  "Path to erlang root.")

(defvar ivy-erlang-complete-project-root)

(defvar-local ivy-erlang-complete-candidates nil
  "Candidates for completion.")

(defvar-local ivy-erlang-complete-predicate nil
  "Completion predicate.")

(defvar-local ivy-erlang-complete-records
  (make-hash-table)
  "Records accessible in current buffer.")

(defun ivy-erlang-complete--find-functions (module)
  "Find functions in MODULE."
  (s-split "\n"
   (shell-command-to-string
    (s-join " "
     (list
      "find" ivy-erlang-complete-project-root ivy-erlang-complete-erlang-root
      "-name" (concat module ".erl") "| xargs sed -n '/-export(/,/)./p'"
      "| sed -e '/%/d' | sed -e 's/ //g' | sed -e 's/\\t//g'"
      "| sed -e '/^$/d' | sed -e '/-export(\\[.*\\])./{ n ; d }'"
      "| sed -e 's/-export.*(\\\[//g' | sed -e 's/\\\]).//g'"
      "| sed 's/\\\,/\\\n/g' | sed '/^$/d'")))
    t))

(defun ivy-erlang-complete--find-modules ()
  "Find functions in MODULE."
  (s-split "\n"
    (shell-command-to-string
     (s-join " "
      (list
       "find" ivy-erlang-complete-project-root ivy-erlang-complete-erlang-root
       "-iname '*.erl' | xargs basename -a | sed -e 's/\\.erl//g'")))
    t))

(defun ivy-erlang-complete--extract-records (file)
  "Extract all records from FILE."
  (-map (lambda (s) (concat s "})."))
   (s-split
    "})\\."
    (shell-command-to-string
     (s-join " "
             (list "find" ivy-erlang-complete-project-root "-name" file "|" "xargs"
                   "sed" "-n" "'/-record(/,/})./p'" "|" "sed -e 's/%.*//g'"
                   "|" "sed -e 's/::.*/,/g'" "|" "sed -e 's/=.*/,/g'")))
    t)))

(defun ivy-erlang-complete--parse-record (record)
  "Parse RECORD and set it accessible in current buffer."
  (if (not ivy-erlang-complete-records)
      (setq ivy-erlang-complete-records (make-hash-table :test 'equal)))
  (let ((matched
         (-map 's-trim
               (-drop 1 (s-match "-record(\\([^,]+\\),[^{]*{\\(.*\\)}."
                                 (s-replace "\n" "" record))))))
    (if matched
        (puthash (car matched) (-map 's-trim (s-split "," (car (cdr matched))))
                   ivy-erlang-complete-records)
        )))

(defun ivy-erlang-complete--extract-functions (file)
  "Extract all functions from FILE."
  (s-split "\n"
    (shell-command-to-string
     (s-join " "
      (list
       "sed -n '/^[a-z][a-zA-Z0-9_]*(.*)/,/[[:space:]]*->/p' " file
       " | sed -e '/%/d' | sed -e '/^\\\-/d' | sed -e '/^[[:space:]]/d'"
       "| sed '/^$/d' | sed -e 's/).*/)/g'")))
    t))

(defun ivy-erlang-complete--set-arity (erl-function)
  "Set arity to ERL-FUNCTION instead of arglist."
  (let ((arity
         (s-trim
          (shell-command-to-string
           (s-join " "
            (list
             "echo" (concat "'" erl-function "'") "| sed -e 's/.*(//g'"
             "| sed -e 's/)//g' | sed -e 's/,/\\n/g' | sed -e '/^$/d'"
             "| wc -l"))))))
    (when
        (string-match "[^(]+" erl-function)
      (concat (substring erl-function (match-beginning 0) (match-end 0))
              "/" arity))))

(defun ivy-erlang-complete--find-local-functions ()
  "Find all local functions."
  (-map #'ivy-erlang-complete--set-arity
          (ivy-erlang-complete--extract-functions (buffer-file-name))))

(defun ivy-erlang-complete-at-point ()
  "Return the erlang thing at point, or nil if none is found."
  (when (thing-at-point-looking-at "[A-Za-z0-9_#?:]+")
    (match-string-no-properties 0)))

;;;###autoload
(defun ivy-erlang-complete-set-project-root ()
  "Set root for current project."
  (interactive)
  (let
      ((dir
        (expand-file-name (read-directory-name
                           "Select project directory:" default-directory))))
    (setq ivy-erlang-complete-project-root dir)))

(defun ivy-erlang-complete--insert-candidate (candidate)
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
(defun ivy-erlang-complete ()
  "Erlang completion at point."
  (interactive)
  (let ((thing (ivy-erlang-complete-at-point)))
   (if (and thing (string-match "\\([^\:]+\\)\:\\([^\:]*\\)" thing))
       (let ((erl-prefix (substring thing (match-beginning 1) (match-end 1))))
         (progn
           (setq ivy-erlang-complete-candidates
                 (ivy-erlang-complete--find-functions
                  erl-prefix))
           (setq ivy-erlang-complete-predicate
                 (string-remove-prefix (concat erl-prefix ":") thing))))
     (progn
       (setq ivy-erlang-complete-candidates (append (ivy-erlang-complete--find-local-functions)
                                            (ivy-erlang-complete--find-modules)
                                            (ivy-erlang-complete--find-functions "erlang")))
       (setq ivy-erlang-complete-predicate thing))))
  (when (looking-back ivy-erlang-complete-predicate (line-beginning-position))
    (setq ivy-completion-beg (match-beginning 0))
    (setq ivy-completion-end (match-end 0)))
  (ivy-read "Counsel-erl cand:" ivy-erlang-complete-candidates
            :initial-input ivy-erlang-complete-predicate
            :action #'ivy-erlang-complete--insert-candidate))



(provide 'ivy-erlang-complete)
;;; ivy-erlang-complete.el ends here
