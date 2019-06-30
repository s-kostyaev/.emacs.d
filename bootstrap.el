(setq gc-cons-threshold (* 80 1024 1024))
(setq gc-cons-percentage 0.5)

(defvar network-security-level)
(defvar tls-program)
(defvar gnutls-verify-error)
(defvar gnutls-trustfiles)
(if (> emacs-major-version 24)
    (progn
      (setq network-security-level 'high)
      (setq gnutls-verify-error t))
  (let ((trustfile
         (replace-regexp-in-string
          "\\\\" "/"
          (replace-regexp-in-string
           "\n" ""
           (shell-command-to-string "python -m certifi")))))
    (setq tls-program
          (list
           (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                   (if (eq window-system 'w32) ".exe" "") trustfile)))
    (setq gnutls-verify-error t)
    (setq gnutls-trustfiles (list trustfile))))
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(defvar package--init-file-ensured)
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(require 'package)
(package-initialize)

(setq custom-file "~/.emacs.d/emacs-customizations.el")

(defun my-bootstrap()
  "Install all needed packages."
  (interactive)
  (ignore-errors (load-file custom-file))
  (package-refresh-contents)
  (package-install-selected-packages))

(if (not (fboundp 'use-package))
    (progn
      (require 'cl-lib)
      (cl-flet ((always-yes (&rest _) t))
	(defun no-confirm (fun &rest args)
	  "Apply FUN to ARGS, skipping user confirmations."
	  (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
                    ((symbol-function 'yes-or-no-p) #'always-yes))
            (apply fun args)))
	(no-confirm 'my-bootstrap))))

(kill-emacs)
(provide 'my-bootstrap)
