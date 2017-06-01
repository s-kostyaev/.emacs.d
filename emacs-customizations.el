;;; emacs-customizations --- File for store emacs customize.

;;; Commentary:

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(arch-packer-default-command "pacaur")
 '(browse-url-browser-function (quote browse-url-firefox))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(electric-indent-mode t)
 '(flycheck-disabled-checkers (quote (erlang-rebar3)))
 '(global-aggressive-indent-mode t)
 '(global-flycheck-mode t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (electric-operator hungry-delete aggressive-indent dtrt-indent all-the-icons-dired all-the-icons-ivy symbol-overlay aurel arch-packer org-brain spaceline counsel-dash ivy-historian ibuffer-vc rainbow-mode mu4e-alert pass password-store paradox use-package use-package-chords async zenburn-theme xref-js2 xah-lookup which-key wgrep web-mode web-beautify timp tagedit sublime-themes speed-type spacemacs-theme solarized-theme smex smart-mode-line-powerline-theme slime-company rtags rjsx-mode restclient react-snippets quelpa pandoc-mode pacmacs package-lint noflet nlinum monokai-theme markdown-mode lua-mode link-hint key-chord json-rpc js2-refactor jquery-doc ivy-rich ivy-hydra ido-vertical-mode header2 go-impl go-eldoc go-autocomplete geiser fuzzy fsm flymd flycheck-gometalinter flycheck-dialyzer flx fill-column-indicator esup eopengrok ensime embrace edit-indirect darkokai-theme counsel-projectile composable company-tern company-statistics company-quickhelp company-irony-c-headers company-irony company-go company-erlang company-c-headers company-anaconda column-marker column-enforce-mode color-theme-solarized cmake-ide cmake-font-lock cask-mode camcorder auto-complete-clang ace-window ace-mc ace-link ac-js2 ac-emmet ac-cider)))
 '(paradox-automatically-star nil)
 '(paradox-github-token (password-store-get "paradox-token"))
 '(send-mail-function (quote smtpmail-send-it))
 '(sml/theme (quote respectful))
 '(smtpmail-smtp-server (password-store-get "smtp-server"))
 '(smtpmail-smtp-service 25))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
