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
 '(column-number-mode t)
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode haskell-mode jde-mode lua-mode python-mode)))
 '(company-gtags-modes
   (quote
    (prog-mode jde-mode web-mode js-mode js3-mode erlang-mode)))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("1e3b2c9e7e84bb886739604eae91a9afbdfb2e269936ec5dd4a9d3b7a943af7f" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "1a85b8ade3d7cf76897b338ff3b20409cb5a5fbed4e45c6f38c98eee7b025ad4" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "f0b0710b7e1260ead8f7808b3ee13c3bb38d45564e369cbe15fc6d312f0cd7a0" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(display-time-mode t)
 '(flycheck-emacs-lisp-initialize-packages t)
 '(flycheck-emacs-lisp-load-path (quote inherit))
 '(frame-background-mode (quote light))
 '(geiser-default-implementation (quote guile))
 '(global-flycheck-mode t)
 '(helm-external-programs-associations (quote (("htm" . "inox") ("pdf" . "zathura"))))
 '(helm-gtags-auto-update t)
 '(helm-gtags-path-style (quote relative))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (company-erlang package-lint cask-mode header2 rjsx-mode slime geiser web-mode smartparens projectile magit ivy flycheck dash erlang counsel flymd edit-indirect esup timp signal fifo-class s flycheck-gometalinter go-impl color-theme pacmacs dash-functional which-key ace-link viking-mode cmake-font-lock cmake-mode xah-lookup flycheck-dialyzer eopengrok smooth-scrolling sublimity xref-js2 emmet-mode flx wgrep ivy-hydra darkokai-theme smart-mode-line monokai-theme clojure-mode cider cousel consel-ivy consel powerline smooth-scroll link-hint avy ace-mc company-irony-c-headers company-irony tern irony paredit-menu zenburn-theme tagedit sublime-themes speed-type solarized-theme smex smart-mode-line-powerline-theme slime-company restclient react-snippets paredit pandoc-mode noflet nlinum multiple-cursors markdown-mode lua-mode key-chord json-rpc js3-mode jquery-doc ido-vertical-mode go-autocomplete fuzzy fsm fill-column-indicator company-tern company-quickhelp company-c-headers company-anaconda column-marker column-enforce-mode color-theme-solarized cmake-ide auto-complete-clang ace-jump-mode ac-js2 ac-emmet ac-cider)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules")))
 '(scroll-bar-mode nil)
 '(sml/no-confirm-load-theme t)
 '(sml/theme (quote respectful))
 '(starttls-extra-arguments (quote ("--insecure")))
 '(starttls-use-gnutls t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
