(setq gc-cons-threshold (* 80 1024 1024))
(setq gc-cons-percentage 0.5)

(defvar gnutls-trustfiles)
(with-eval-after-load 'gnutls
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))

(eval-and-compile
  (defvar network-security-level)
  (defvar tls-program)
  (defvar gnutls-verify-error)
  (if (> emacs-major-version 24)
      (progn
        (setq network-security-level 'high)
        (setq gnutls-verify-error t))))

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
