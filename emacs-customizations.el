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
   '(("jupyter-python" . python)
     ("ipython" . python)
     ("html-chrome" . html)
     ("C" . c)
     ("C++" . c++)
     ("asymptote" . asy)
     ("bash" . sh)
     ("beamer" . latex)
     ("calc" . fundamental)
     ("cpp" . c++)
     ("ditaa" . artist)
     ("desktop" . conf-desktop)
     ("dot" . fundamental)
     ("elisp" . emacs-lisp)
     ("ocaml" . tuareg)
     ("screen" . shell-script)
     ("shell" . sh)
     ("sqlite" . sql)
     ("toml" . conf-toml)
     ("html" . web)))
 '(package-selected-packages
   '(posframe aider erc faceup flymake idlwave org project soap-client tramp use-package verilog-mode xref ultra-scroll outline-indent haskell-ts-mode expreg elisa web-mode gotest ob-go pdf-tools casual code-cells eglot cape tabby ellama dape envrc wgrep breadcrumb zenburn-theme xeft white-sand-theme web-beautify vterm-toggle vmd-mode visual-regexp treesit-auto timp tagedit symbol-overlay string-inflection spray speed-type speechd-el spacious-padding spacemacs-theme solarized-theme smart-shift rust-playground rust-mode reverse-im restclient-jq rainbow-mode pyenv-mode pulsar protobuf-mode prism poly-markdown plz pkgbuild-mode pass pandoc-mode package-lint-flymake org-mind-map ocp-indent nova-theme nov noflet multiple-cursors monokai-theme merlin marginalia magit-todos keycast key-chord jsonian json-snatcher json-rpc ibuffer-vc hungry-delete highlight-indentation hercules go-tag go-snippets go-impl go-gen-test go-fill-struct git-timemachine gif-screencast frimacs flymake-quickdef flymake-proselint flymake-go-staticcheck flx feature-mode fb2-reader expand-region embark-consult elisp-benchmarks ein eglot-fsharp ef-themes edit-server edit-indirect eat dune dumb-jump dtrt-indent dotnet dockerfile-mode docker-compose-mode denote dash-functional dart-mode corfu consult-dash composable comment-tags cl-libify chocolate-theme cask-mode cargo bash-completion auto-yasnippet apparmor-mode ample-theme aggressive-indent ace-link))
 '(package-vc-selected-packages
   '((aider :vc-backend Git :url "https://www.github.com/tninja/aider.el")
     (ultra-scroll :vc-backend Git :url "https://www.github.com/jdtsmith/ultra-scroll")
     (ready-player :vc-backend Git :url "https://www.github.com/xenodium/ready-player")
     (elisa :vc-backend Git :url "https://github.com/s-kostyaev/elisa")
     (tabby :vc-backend Git :url "https://www.github.com/alan-w-255/tabby.el")))
 '(reverse-im-input-methods '("russian-computer"))
 '(safe-local-variable-values
   '((eval and buffer-file-name
	   (not
	    (eq major-mode 'package-recipe-mode))
	   (or
	    (require 'package-recipe-mode nil t)
	    (let
		((load-path
		  (cons "../package-build" load-path)))
	      (require 'package-recipe-mode nil t)))
	   (package-recipe-mode))
     (conda-project-env-path . "pineapple"))))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
