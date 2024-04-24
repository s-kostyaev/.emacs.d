;;; emacs-customizations --- File for store emacs customize.

;;; Commentary:

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "unspecified-bg")))
 '(header-line ((t :box (:line-width 4 :color "unspecified-bg" :style nil))))
 '(header-line-highlight ((t :box (:color "unspecified-fg"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "unspecified-bg")))
 '(mode-line ((t :box (:line-width 6 :color nil :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color nil :style nil))))
 '(mode-line-highlight ((t :box (:color "unspecified-fg"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color nil :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "grey" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey" :style nil))))
 '(window-divider ((t :background "unspecified-bg" :foreground "unspecified-bg")))
 '(window-divider-first-pixel ((t :background "unspecified-bg" :foreground "unspecified-bg")))
 '(window-divider-last-pixel ((t :background "unspecified-bg" :foreground "unspecified-bg"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ob-go pdf-tools casual code-cells elisa eglot cape tabby ellama dape envrc haskell-mode wgrep breadcrumb zenburn-theme xeft white-sand-theme web-beautify vterm-toggle vmd-mode visual-regexp treesit-auto timp tagedit symbol-overlay string-inflection spray speed-type speechd-el spacious-padding spacemacs-theme solarized-theme smart-shift rust-playground rust-mode reverse-im restclient-jq rainbow-mode pyenv-mode pulsar protobuf-mode prism poly-markdown plz pkgbuild-mode pass pandoc-mode package-lint-flymake org-mind-map ocp-indent nova-theme nov noflet multiple-cursors monokai-theme merlin marginalia magit-todos keycast key-chord kaolin-themes jsonian json-snatcher json-rpc ibuffer-vc hungry-delete highlight-indentation hercules haskell-snippets go-tag go-snippets go-impl go-gen-test go-fill-struct git-timemachine gif-screencast frimacs flymake-quickdef flymake-proselint flymake-go-staticcheck flx feature-mode fb2-reader expand-region embark-consult elisp-benchmarks ein eglot-fsharp ef-themes edit-server edit-indirect eat dune dumb-jump dtrt-indent dotnet dockerfile-mode docker-compose-mode denote dash-functional dart-mode corfu consult-dash conda composable comment-tags cl-libify chocolate-theme cask-mode cargo bash-completion auto-yasnippet apparmor-mode ample-theme aggressive-indent ace-link))
 '(package-vc-selected-packages
   '((elisa :vc-backend Git :url "https://github.com/s-kostyaev/elisa")
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
