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
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(counsel-rg-base-command
   "/opt/local/bin/rg -i -u --no-heading --line-number --max-columns 150 %s .")
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(debug-on-error nil)
 '(debug-on-quit nil)
 '(dtrt-indent-active-mode-line-info "")
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(fci-rule-color "#eee8d5")
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
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
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
 '(magit-diff-use-overlays nil)
 '(mail-envelope-from 'header)
 '(mail-specify-envelope-from t)
 '(message-sendmail-envelope-from 'header)
 '(notmuch-search-oldest-first nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(flymake-go-staticcheck package-lint auto-yasnippet pass password-store ace-window ace-link exec-path-from-shell symbol-overlay composable multiple-cursors helm-make go-playground gotest dtrt-indent magit-todos aggressive-indent reverse-im deadgrep wgrep-helm moe-theme bash-completion lsp-mode pcre2el eglot docker-tramp pyenv-mode go-snippets smart-shift highlight-indentation go-fill-struct edit-server ace-jump-helm-line go-tag go-gen-test comment-tags org-mind-map ace-isearch helm-fuzzier helm-flx helm-company json-snatcher docker-compose-mode go-direx feature-mode zygospore hungry-delete ibuffer-vc use-package-chords xah-lookup which-key wgrep web-beautify timp tagedit speed-type spacemacs-theme smex react-snippets pandoc-mode noflet monokai-theme key-chord json-rpc jquery-doc ido-vertical-mode header2 go-impl geiser fuzzy flymd flx esup embrace edit-indirect darkokai-theme company-quickhelp column-marker cask-mode))
 '(paradox-automatically-star nil)
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(paradox-spinner-type 'progress-bar)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml/plantuml.jar"))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
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
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/theme 'respectful)
 '(smtpmail-smtp-server (password-store-get "smtp-server"))
 '(smtpmail-smtp-service 25)
 '(symbol-overlay-global-mode t)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tls-checktrust t)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#c9485ddd1797")
     (60 . "#bf7e73b30bcb")
     (80 . "#b58900")
     (100 . "#a5a58ee30000")
     (120 . "#9d9d91910000")
     (140 . "#9595943e0000")
     (160 . "#8d8d96eb0000")
     (180 . "#859900")
     (200 . "#67119c4632dd")
     (220 . "#57d79d9d4c4c")
     (240 . "#489d9ef365ba")
     (260 . "#3963a04a7f29")
     (280 . "#2aa198")
     (300 . "#288e98cbafe2")
     (320 . "#27c19460bb87")
     (340 . "#26f38ff5c72c")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
