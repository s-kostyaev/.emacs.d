(require 'package)
;; load autoload files and populate load-path’s
(package-initialize)
;; (package-initialize) doens’t require each package, we need to load
;; those we want manually
(setq luna-dumped-load-path load-path)
(setq luna-dumped t)

(dolist (package '(use-package company helm helm-files helm-config helm-command helm-make
                    which-key aggressive-indent ample-light-theme spacemacs-dark-theme
                    etags expand-region lsp-mode prescient
                    company-capf company-cmake company-clang company-eclim
                    company-dabbrev-code reverse-im quail smie diff-mode flymake avy comment-tags))
  (require package))
;; dump image
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
