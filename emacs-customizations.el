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
 '(mode-line ((t :box (:line-width 6 :color nil :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color nil :style nil))))
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
   '(breadcrumb ellama zenburn-theme xeft white-sand-theme web-beautify vterm-toggle vmd-mode visual-regexp treesit-auto timp tagedit symbol-overlay string-inflection spray speed-type speechd-el spacious-padding spacemacs-theme solarized-theme smart-shift rust-playground rust-mode rg reverse-im restclient-jq rainbow-mode pyenv-mode pulsar protobuf-mode prism poly-markdown plz pkgbuild-mode pass pandoc-mode package-lint-flymake org-mind-map ocp-indent nova-theme nov noflet multiple-cursors monokai-theme merlin marginalia magit-todos lsp-pyright lsp-origami lsp-jedi lsp-haskell leetcode keycast key-chord kaolin-themes jsonian json-snatcher json-rpc ibuffer-vc hungry-delete highlight-indentation hercules haskell-snippets go-translate go-tag go-snippets go-playground go-impl go-gen-test go-fill-struct git-timemachine gif-screencast frimacs flymake-quickdef flymake-proselint flymake-go-staticcheck flx feature-mode fb2-reader expand-region embark-consult elisp-benchmarks ein eglot-fsharp ef-themes edit-server edit-indirect eat dune dumb-jump dtrt-indent dotnet dockerfile-mode docker-compose-mode denote dash-functional dart-mode dape dap-mode corfu consult-lsp consult-dash conda composable company-restclient company-quickhelp company-prescient company-maxima comment-tags cl-libify chocolate-theme cask-mode cargo bash-completion auto-yasnippet auctex apparmor-mode ample-theme aggressive-indent affe ace-link))
 '(reverse-im-input-methods '("russian-computer")))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
