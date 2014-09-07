;;; parscope.el --- Minor mode for showing the current scope in Lisp-like languages.

;; Copyright (C) 2013 Nick Pascucci

;; Author: Nick Pascucci <npascut1@gmail.com>
;; Created: 22 Jun 2013
;; Keywords: tools
;; Version: 0.1.0
;; URL: https://github.com/nickpascucci/ParScope

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Commentary:

;; parscope-mode highlights the current scope by creating an overlay with parscope-overlay-face
;; that extends to cover the body of the current S-expression. As you might imagine, this works
;; pretty well in Lisp-like languages (Common Lisp, Emacs Lisp, Clojure, etc) and even works
;; somewhat-well for Algol-based languages.

;; Inspired by https://www.youtube.com/watch?feature=player_embedded&v=wBl0rYXQdGg

;;; Code:

(defvar-local ps-overlay nil "Overlay highlighting the current scope.")

(define-minor-mode parscope-mode
  "Minor mode that highlights the current sexp."
  nil "ParScope" nil
  (ps/enable-disable))

(defface parscope-overlay-face
  '((default :background "#07272D"))
  "Face used to highlight the current scope."
  :group 'parscope)

(defun ps/backward-up-sexp (&optional arg)
  "Move backwards up to the start of the current S-expression or string."
  (interactive "p")
  (let ((ppss (syntax-ppss))
        (arg (or arg 1)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (ps/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(defun ps/enable-disable ()
  (if parscope-mode
      (ps/init)
    (ps/detach-hooks)
    (delete-overlay ps-overlay)))

(defun ps/init ()
  (when (not (null ps-overlay))
    (delete-overlay ps-overlay))
  (setq ps-overlay (make-overlay 3 3 (current-buffer)))
  (overlay-put ps-overlay 'face 'ps-overlay-face)
  (ps/set-scope-overlay)
  (ps/attach-hooks))

(defun ps/set-scope-overlay ()
  (interactive)
  (condition-case ps-error
      (save-excursion
        (ps/backward-up-sexp)
        (let ((start (point)))
          (forward-sexp)
          (move-overlay ps-overlay start (point))
          (list start (point))))
    ('error (move-overlay ps-overlay (point) (point)))))

(defun ps/update-scope ()
  (when parscope-mode
   (ps/set-scope-overlay)))

(defun ps/attach-hooks ()
  (add-hook 'post-command-hook #'ps/update-scope nil t))

(defun ps/detach-hooks ()
  (remove-hook 'post-command-hook #'ps/update-scope t))

(provide 'parscope)

;;; parscope.el ends here
