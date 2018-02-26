;;; emacs-customizations --- File for store emacs customize.

;;; Commentary:

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :width normal :family "Go Mono")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(arch-packer-default-command "pacaur")
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-chromium)
 '(byte-compile-warnings
   '(redefine callargs unresolved obsolete interactive-only make-local mapcar constants suspicious lexical))
 '(column-number-mode t)
 '(counsel-rg-base-command
   "rg -i -u --no-heading --line-number --max-columns 150 %s .")
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(debug-on-error nil)
 '(dtrt-indent-active-mode-line-info "")
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(electric-indent-mode t)
 '(flycheck-disabled-checkers '(erlang-rebar3))
 '(flycheck-gometalinter-deadline "30s")
 '(flycheck-gometalinter-disable-linters '("gotype"))
 '(flycheck-vale-modes '(text-mode markdown-mode rst-mode gfm-mode web-mode))
 '(global-aggressive-indent-mode t)
 '(global-flycheck-mode t)
 '(go-add-tags-style 'lower-camel-case)
 '(go-packages-function 'go-packages-go-list)
 '(go-test-verbose t)
 '(godoc-at-point-function 'godoc-gogetdoc)
 '(godoc-command "godoc")
 '(godoc-use-completing-read t)
 '(gofmt-command "goimports")
 '(helm-M-x-always-save-history t)
 '(helm-M-x-fuzzy-match t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-codesearch-global-csearchindex "/home/feofan/.csearchindex")
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-echo-input-in-header-line t)
 '(helm-file-cache-fuzzy-match t)
 '(helm-grep-file-path-style 'relative)
 '(helm-lisp-fuzzy-completion t)
 '(helm-locate-command "locate %s -e -A --regex %s")
 '(helm-mode t)
 '(helm-mode-fuzzy-match t)
 '(helm-recentf-fuzzy-match t)
 '(helm-session-fuzzy-match t)
 '(helm-swoop-pre-input-function (lambda nil nil))
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-flycheck-enable nil)
 '(mail-envelope-from 'header)
 '(mail-specify-envelope-from t)
 '(message-sendmail-envelope-from 'header)
 '(notmuch-search-oldest-first nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(gif-screencast go-snippets yasnippet-snippets smart-shift highlight-indentation swiper-helm lsp-go lsp-ui go-fill-struct edit-server ace-jump-helm-line helm-codesearch company-ycmd flycheck-ycmd python-mode go-tag sicp go-gen-test company-lsp meghanada paradox go-add-tags go-rename helm-dash circadian comment-tags flycheck-plantuml plantuml-mode org-mind-map ace-isearch evalator helm-swoop helm-fuzzier helm-flx helm-flycheck helm-company indium json-snatcher docker-compose-mode flycheck-vale go-direx go-dlv godoctor go-playground color-theme lsp-mode company-racer cargo flycheck-rust racer rust-mode rust-playground flycheck-clang-analyzer feature-mode reverse-im flycheck-irony flycheck-rtags json-navigator zygospore hungry-delete aggressive-indent dtrt-indent all-the-icons-dired all-the-icons-ivy symbol-overlay aurel arch-packer ibuffer-vc rainbow-mode pass password-store use-package use-package-chords async zenburn-theme xref-js2 xah-lookup which-key wgrep web-mode web-beautify timp tagedit sublime-themes speed-type spacemacs-theme solarized-theme smex slime-company rtags rjsx-mode restclient react-snippets pandoc-mode pacmacs package-lint noflet nlinum monokai-theme markdown-mode lua-mode link-hint key-chord json-rpc js2-refactor jquery-doc ivy-hydra ido-vertical-mode header2 go-impl go-eldoc go-autocomplete geiser fuzzy fsm flymd flycheck-gometalinter flycheck-dialyzer flx fill-column-indicator esup eopengrok ensime embrace edit-indirect darkokai-theme counsel-projectile composable company-tern company-statistics company-quickhelp company-irony-c-headers company-irony company-go company-erlang company-c-headers company-anaconda column-marker column-enforce-mode cmake-ide cmake-font-lock cask-mode camcorder auto-complete-clang ace-window ace-mc ace-link ac-js2 ac-emmet ac-cider))
 '(paradox-automatically-star nil)
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(paradox-spinner-type 'progress-bar)
 '(plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml/plantuml.jar"))
 '(racer-rust-src-path "/usr/src/rust/src")
 '(safe-local-variable-values '((eval c-set-offset 'innamespace 0)))
 '(send-mail-function 'sendmail-send-it)
 '(sendmail-program "/usr/bin/msmtp")
 '(sml/theme 'respectful)
 '(smtpmail-smtp-server (password-store-get "smtp-server"))
 '(smtpmail-smtp-service 25)
 '(symbol-overlay-global-mode t)
 '(tls-checktrust t))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
