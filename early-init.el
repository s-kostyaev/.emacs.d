;;; early-init.el --- early init                     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq gc-cons-threshold (* 80 1024 1024))
(setq gc-cons-percentage 0.5)

(defun my-reset-gc-settings ()
  "Reset gc settings."
  (setq gc-cons-threshold (* 8 1024 1024)))

(add-hook 'after-init-hook #'my-reset-gc-settings)
(setopt package-enable-at-startup nil)

(provide 'early-init)

;;; early-init.el ends here
