;;; emacs-customizations --- File for store emacs customize.

;;; Commentary:

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :width normal :family "Go Mono")))))
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
   "/opt/local/bin/rg -i -u --no-heading --line-number --max-columns 150 %s .")
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(debug-on-error nil)
 '(debug-on-quit nil)
 '(dtrt-indent-active-mode-line-info "")
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(electric-indent-mode t)
 '(flycheck-disabled-checkers
   '(erlang-rebar3 go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert))
 '(flycheck-gometalinter-deadline "30s")
 '(flycheck-gometalinter-disable-linters '("gotype"))
 '(flycheck-vale-modes '(text-mode markdown-mode rst-mode gfm-mode web-mode) t)
 '(global-aggressive-indent-mode t)
 '(global-flycheck-mode t)
 '(go-add-tags-style 'lower-camel-case)
 '(go-packages-function 'go-packages-go-list)
 '(go-tag-args '("-transform" "snakecase") t)
 '(go-test-verbose t)
 '(godoc-at-point-function 'godoc-gogetdoc)
 '(godoc-command "godoc")
 '(godoc-use-completing-read t)
 '(gofmt-command "goimports")
 '(helm-M-x-always-save-history t)
 '(helm-M-x-fuzzy-match t)
 '(helm-buffers-fuzzy-matching t)
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
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")
     ("???" . "#dc752f")))
 '(lsp-clients-go-server "gopls")
 '(lsp-enable-on-type-formatting nil)
 '(lsp-prefer-flymake t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-sideline-enable t)
 '(mail-envelope-from 'header)
 '(mail-specify-envelope-from t)
 '(message-sendmail-envelope-from 'header)
 '(notmuch-search-oldest-first nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(frog-jump-buffer deadgrep company-lsp lsp-mode lsp-ui magit-todos pcre2el hl-todo quelpa docker ace-window package-lint rust-mode markdown-mode magit python-mode eglot ht docker-tramp exec-path-from-shell json-mode pyenv-mode virtualenvwrapper define-word auto-yasnippet gif-screencast go-snippets yasnippet-snippets smart-shift highlight-indentation swiper-helm go-fill-struct edit-server ace-jump-helm-line helm-codesearch go-tag sicp go-gen-test go-add-tags helm-dash comment-tags plantuml-mode org-mind-map ace-isearch evalator helm-fuzzier helm-flx helm-company indium json-snatcher docker-compose-mode go-direx go-dlv go-playground rust-playground feature-mode reverse-im json-navigator zygospore hungry-delete aggressive-indent dtrt-indent symbol-overlay ibuffer-vc rainbow-mode pass password-store use-package use-package-chords async zenburn-theme xref-js2 xah-lookup which-key wgrep web-beautify timp tagedit sublime-themes speed-type spacemacs-theme solarized-theme smex slime-company rjsx-mode restclient react-snippets pandoc-mode pacmacs noflet nlinum monokai-theme lua-mode link-hint key-chord json-rpc js2-refactor jquery-doc ivy-hydra ido-vertical-mode header2 go-impl geiser fuzzy fsm flymd flx esup ensime embrace edit-indirect darkokai-theme composable company-statistics company-quickhelp column-marker cmake-font-lock cask-mode camcorder ace-mc ace-link ac-js2 ac-emmet ac-cider))
 '(paradox-automatically-star nil)
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(paradox-spinner-type 'progress-bar)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml/plantuml.jar"))
 '(pyenv-mode t)
 '(racer-rust-src-path "/usr/src/rust/src")
 '(safe-local-variable-values
   '((checkdoc-minor-mode . 1)
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (package-build-minor-mode)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (eval c-set-offset 'innamespace 0)))
 '(send-mail-function 'sendmail-send-it)
 '(sendmail-program "/usr/bin/msmtp")
 '(sml/theme 'respectful)
 '(smtpmail-smtp-server (password-store-get "smtp-server"))
 '(smtpmail-smtp-service 25)
 '(symbol-overlay-global-mode t)
 '(tls-checktrust t))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
