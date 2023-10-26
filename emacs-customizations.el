;;; emacs-customizations --- File for store emacs customize.

;;; Commentary:

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:size 13.0 :family "PT Mono" :foundry "PARA" :slant normal :weight normal :height 143 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(arch-packer-default-command "pacaur")
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-default-browser)
 '(byte-compile-verbose nil)
 '(byte-compile-warnings
   '(redefine callargs interactive-only make-local mapcar constants suspicious lexical))
 '(c-basic-offset 'set-from-style)
 '(c-default-style
   '((c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(column-number-mode t)
 '(company-backends
   '(company-capf company-cmake company-files
		  (company-dabbrev-code company-keywords)
		  company-oddmuse company-dabbrev))
 '(compilation-message-face 'default)
 '(conda-anaconda-home "/opt/homebrew/anaconda3")
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("96c56bd2aab87fd92f2795df76c3582d762a88da5c0e54d30c71562b7bf9c605" "7ea491e912d419e6d4be9a339876293fff5c8d13f6e84e9f75388063b5f794d6" "11e0bc5e71825b88527e973b80a84483a2cfa1568592230a32aedac2a32426c1" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "cdb4ffdecc682978da78700a461cdc77456c3a6df1c1803ae2dd55c59fa703e3" "e62b66040cb90a4171aa7368aced4ab9d8663956a62a5590252b0bc19adde6bd" "621595cbf6c622556432e881945dda779528e48bb57107b65d428e61a8bb7955" "22a3867a1643196673bdf6a11b8b797c382c9b7b6462e088f33891a0c64f38d8" "33af2d5cb040182b798c3a4ee6d16210e700a2fabaa409231e1c4a003cafd1c2" "b3697d12fb7c087e1337432be92026b5fd218e7e43277918c0fce680d573a90c" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "37ba833442e0c5155a46df21446cadbe623440ccb6bbd61382eb869a2b9e9bf9" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(dap-auto-configure-mode t)
 '(dap-internal-terminal 'dap-internal-terminal-vterm)
 '(dap-mode t nil (dap-mode))
 '(dap-tooltip-mode t)
 '(dap-ui-controls-mode t nil (dap-ui))
 '(dap-ui-many-windows-mode t)
 '(dap-ui-mode t nil (dap-ui))
 '(dash-docs-enable-debugging nil)
 '(debug-on-error nil)
 '(debug-on-quit nil)
 '(ein:output-area-inlined-images t)
 '(electric-spacing-double-space-docs nil)
 '(enable-recursive-minibuffers t)
 '(flymake-mode-line-format '(" " flymake-mode-line-counters))
 '(fringe-mode 0 nil (fringe))
 '(global-aggressive-indent-mode t)
 '(go-gen-test-use-testify t)
 '(go-packages-function 'go-packages-go-list)
 '(go-tag-args '("-transform" "camelcase") t)
 '(go-test-verbose nil)
 '(godoc-at-point-function 'godoc-gogetdoc)
 '(godoc-command "godoc")
 '(godoc-use-completing-read t)
 '(gofmt-args '("--shorten-comments"))
 '(gofmt-command "golines")
 '(hippie-expand-try-functions-list
   '(try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill))
 '(imenu-max-item-length 230)
 '(keycast-insert-after "    ")
 '(leetcode-prefer-language "golang")
 '(leetcode-save-solutions t)
 '(lsp-auto-guess-root t)
 '(lsp-eldoc-render-all t)
 '(lsp-file-watch-threshold 5000)
 '(lsp-go-use-gofumpt nil)
 '(lsp-rust-server 'rust-analyzer)
 '(mac-frame-tabbing nil)
 '(mac-pass-command-to-system nil)
 '(magit-clone-default-directory "~/projects")
 '(magit-diff-use-overlays nil)
 '(mail-envelope-from 'header)
 '(mail-specify-envelope-from t)
 '(message-sendmail-envelope-from 'header)
 '(mouse-wheel-progressive-speed nil)
 '(notmuch-search-oldest-first nil)
 '(orderless-matching-styles '(orderless-regexp orderless-initialism orderless-flex))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(dape llm ellama plz websocket conda lsp-jedi treesit-auto eat xeft dune dash-functional consult-dash embark-consult company-restclient restclient-jq leetcode denote ef-themes jsonian pulsar merlin ocp-indent dotnet dash-docs frimacs vterm vterm-toggle fb2-reader csharp-mode origami lsp-origami ein lsp-haskell haskell-mode haskell-snippets dap-mode affe eglot-fsharp fsharp-mode company-maxima posframe speechd-el apparmor-mode visual-fill-column justify-kp nov spray embark marginalia consult consult-lsp visual-regexp cl-libify auctex maxima flx expand-region flymake-proselint flymake-quickdef package-lint-flymake rainbow-mode go-translate lsp-mode lsp-pyright vterm elisp-benchmarks protobuf-mode dumb-jump vmd-mode dockerfile-mode pkgbuild-mode rg cargo rust-playground rust-mode string-inflection prism poly-markdown gif-screencast keycast hercules white-sand-theme yasnippet magit kaolin-themes company-prescient chocolate-theme git-timemachine dart-mode nova-theme zenburn-theme solarized-theme flymake ample-theme flymake-go-staticcheck package-lint auto-yasnippet pass password-store ace-link symbol-overlay composable multiple-cursors go-playground gotest dtrt-indent magit-todos aggressive-indent reverse-im bash-completion pcre2el eglot pyenv-mode go-snippets smart-shift highlight-indentation go-fill-struct edit-server go-tag go-gen-test comment-tags org-mind-map json-snatcher docker-compose-mode feature-mode hungry-delete ibuffer-vc xah-lookup which-key wgrep web-beautify timp tagedit speed-type spacemacs-theme pandoc-mode noflet monokai-theme key-chord json-rpc jquery-doc go-impl edit-indirect company-quickhelp cask-mode))
 '(package-vc-selected-packages
   '((dape :vc-backend Git :url "https://www.github.com/svaante/dape")
     (ellama :vc-backend Git :url "https://www.github.com/s-kostyaev/ellama")))
 '(racer-rust-src-path "/usr/src/rust/src")
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((eval conda-env-activate "assistant")
     (eval conda-env-activate "playground")
     (eval conda-env-activate "langchain")
     (eval conda-env-activate "textgen")
     (eval progn
	   (make-local-variable 'process-environment)
	   (setq process-environment
		 (copy-sequence process-environment))
	   (setenv "CGO_LDFLAGS" "-no-pie"))
     (eval when
	   (and
	    (buffer-file-name)
	    (not
	     (file-directory-p
	      (buffer-file-name)))
	    (string-match-p "^[^.]"
			    (buffer-file-name)))
	   (unless
	       (featurep 'package-build)
	     (let
		 ((load-path
		   (cons "../package-build" load-path)))
	       (require 'package-build)))
	   (unless
	       (derived-mode-p 'emacs-lisp-mode)
	     (emacs-lisp-mode))
	   (package-build-minor-mode)
	   (setq-local flycheck-checkers nil)
	   (set
	    (make-local-variable 'package-build-working-dir)
	    (expand-file-name "../working/"))
	   (set
	    (make-local-variable 'package-build-archive-dir)
	    (expand-file-name "../packages/"))
	   (set
	    (make-local-variable 'package-build-recipes-dir)
	    default-directory))
     (flycheck-disabled-checkers emacs-lisp-checkdoc)
     (eval progn
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
 '(speechd-out-active-drivers '(ssip))
 '(straight-package-neutering-mode t)
 '(straight-use-package-mode t)
 '(symbol-overlay-global-mode t)
 '(tls-checktrust t)
 '(tuareg-opam-insinuate t)
 '(vterm-max-scrollback 10000)
 '(warning-suppress-log-types '((comp) (comp) (frameset) (bytecomp)))
 '(warning-suppress-types '(((tar link)) (comp) (comp) (comp) (frameset) (bytecomp))))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
