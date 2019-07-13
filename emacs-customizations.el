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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(arch-packer-default-command "pacaur")
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-default-browser)
 '(byte-compile-verbose nil)
 '(byte-compile-warnings
   '(redefine callargs obsolete interactive-only make-local mapcar constants suspicious lexical))
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(counsel-rg-base-command
   "/opt/local/bin/rg -i -u --no-heading --line-number --max-columns 150 %s .")
 '(custom-safe-themes
   '("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "37ba833442e0c5155a46df21446cadbe623440ccb6bbd61382eb869a2b9e9bf9" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(debug-on-error nil)
 '(debug-on-quit nil)
 '(dtrt-indent-active-mode-line-info "")
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(electric-spacing-double-space-docs nil)
 '(flycheck-disabled-checkers
   '(erlang-rebar3 go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert))
 '(flycheck-gometalinter-deadline "30s")
 '(flycheck-gometalinter-disable-linters '("gotype"))
 '(flycheck-vale-modes '(text-mode markdown-mode rst-mode gfm-mode web-mode) t)
 '(global-aggressive-indent-mode t)
 '(global-flycheck-mode t)
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
 '(lsp-auto-guess-root t)
 '(lsp-clients-go-server "gopls")
 '(lsp-enable-on-type-formatting nil)
 '(lsp-prefer-flymake t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-sideline-enable t)
 '(mac-frame-tabbing nil)
 '(mac-pass-command-to-system nil)
 '(magit-diff-use-overlays nil)
 '(mail-envelope-from 'header)
 '(mail-specify-envelope-from t)
 '(message-sendmail-envelope-from 'header)
 '(notmuch-search-oldest-first nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(darktooth-theme birds-of-paradise-plus-theme nova-theme reason-mode zenburn-theme magit-libgit solarized-theme use-package flymake libgit ample-theme lsp-ui company-lsp flymake-go-staticcheck package-lint auto-yasnippet pass password-store ace-window ace-link exec-path-from-shell symbol-overlay composable multiple-cursors helm-make go-playground gotest dtrt-indent magit-todos aggressive-indent reverse-im deadgrep wgrep-helm moe-theme bash-completion lsp-mode pcre2el eglot docker-tramp pyenv-mode go-snippets smart-shift highlight-indentation go-fill-struct edit-server ace-jump-helm-line go-tag go-gen-test comment-tags org-mind-map ace-isearch helm-fuzzier helm-flx helm-company json-snatcher docker-compose-mode go-direx feature-mode zygospore hungry-delete ibuffer-vc use-package-chords xah-lookup which-key wgrep web-beautify timp tagedit speed-type spacemacs-theme smex pandoc-mode noflet monokai-theme key-chord json-rpc jquery-doc ido-vertical-mode header2 go-impl geiser fuzzy flymd flx esup embrace edit-indirect darkokai-theme company-quickhelp column-marker cask-mode))
 '(paradox-automatically-star nil)
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(paradox-spinner-type 'progress-bar)
 '(plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml/plantuml.jar"))
 '(pyenv-mode t)
 '(racer-rust-src-path "/usr/src/rust/src")
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((eval progn
           (make-local-variable 'process-environment)
           (setq process-environment
                 (copy-sequence process-environment))
           (setenv "GOOS" "js")
           (setenv "GOARCH" "wasm"))
     (checkdoc-minor-mode . 1)
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
 '(tls-checktrust t)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
