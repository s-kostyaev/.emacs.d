;;; emacs-customizations --- File for store emacs customize.

;;; Commentary:

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-src-lang-modes
   '(("jupyter-python" . python) ("ipython" . python) ("html-chrome" . html)
     ("C" . c) ("C++" . c++) ("asymptote" . asy) ("bash" . sh)
     ("beamer" . latex) ("calc" . fundamental) ("cpp" . c++) ("ditaa" . artist)
     ("desktop" . conf-desktop) ("dot" . fundamental) ("elisp" . emacs-lisp)
     ("ocaml" . tuareg) ("screen" . shell-script) ("shell" . sh)
     ("sqlite" . sql) ("toml" . conf-toml) ("html" . web)))
 '(package-selected-packages
   '(ace-link aggressive-indent aider ample-theme apparmor-mode auto-yasnippet
	      bash-completion breadcrumb cape cargo cask-mode casual
	      chocolate-theme cl-libify code-cells comment-tags composable
	      consult-dash corfu csv-mode dape dart-mode dash-functional denote
	      diminish docker-compose-mode dockerfile-mode dotnet dtrt-indent
	      dumb-jump dune eat edit-indirect edit-server editorconfig
	      ef-themes eglot eglot-fsharp ein elisa elisp-benchmarks ellama
	      embark-consult envrc erc expand-region expreg faceup fb2-reader
	      feature-mode flx flymake flymake-go-staticcheck flymake-proselint
	      flymake-quickdef frimacs gif-screencast git-timemachine
	      go-fill-struct go-gen-test go-impl go-snippets go-tag gotest
	      haskell-ts-mode hercules highlight-indentation hungry-delete
	      ibuffer-vc idlwave json-rpc json-snatcher jsonian key-chord
	      keycast magit-todos marginalia merlin monokai-theme
	      multiple-cursors noflet nov nova-theme ob-go ocp-indent org
	      org-mind-map outline-indent package-lint-flymake pandoc-mode pass
	      pdf-tools pkgbuild-mode plz poly-markdown posframe prism project
	      protobuf-mode pulsar pyenv-mode rainbow-mode restclient-jq
	      reverse-im rust-mode rust-playground smart-shift soap-client
	      solarized-theme spacemacs-theme spacious-padding speechd-el
	      speed-type spray string-inflection symbol-overlay tabby tagedit
	      timp tramp transient treesit-auto treesit-fold ultra-scroll
	      use-package verilog-mode visual-regexp vmd-mode vterm-toggle
	      web-beautify web-mode wgrep which-key white-sand-theme
	      window-tool-bar xeft xref zenburn-theme))
 '(package-vc-selected-packages
   '((aider :vc-backend Git :url "https://www.github.com/tninja/aider.el")
     (ultra-scroll :vc-backend Git :url
		   "https://www.github.com/jdtsmith/ultra-scroll")
     (ready-player :vc-backend Git :url
		   "https://www.github.com/xenodium/ready-player")
     (elisa :vc-backend Git :url "https://github.com/s-kostyaev/elisa")
     (tabby :vc-backend Git :url "https://www.github.com/alan-w-255/tabby.el")))
 '(reverse-im-input-methods '("russian-computer"))
 '(safe-local-variable-values
   '((eval and buffer-file-name (not (eq major-mode 'package-recipe-mode))
	   (or (require 'package-recipe-mode nil t)
	       (let ((load-path (cons "../package-build" load-path)))
		 (require 'package-recipe-mode nil t)))
	   (package-recipe-mode))
     (conda-project-env-path . "pineapple"))))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
