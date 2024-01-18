;;; init.el --- Emacs init file. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar gnutls-trustfiles)
(if (eq system-type 'darwin)
    (with-eval-after-load 'gnutls
      (add-to-list 'gnutls-trustfiles "/opt/homebrew/opt/libressl/.bottle/etc/libressl/cert.pem")))

(eval-and-compile
  (defvar network-security-level)
  (defvar gnutls-verify-error)
  (if (> emacs-major-version 24)
      (progn
        (setq network-security-level 'high)
        (setq gnutls-verify-error t))))

(require 'cl-lib)
(cl-defun my-vc-install (&key url host repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to
make non-interactive usage more ergonomic.  Takes the following
named arguments:

- HOST the remote where to get the package (e.g., \"gitlab\").

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- URL, NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((uri (or url (format "https://www.%s.com/%s" host repo)))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install uri rev backend))))

(setopt use-package-always-defer t)

(setopt native-comp-deferred-compilation t)
(setopt native-compile-prune-cache t)

(setopt visible-bell t)

(setq custom-file "~/.emacs.d/emacs-customizations.el")

(defun my-load-custom-file ()
  "Load my custom file."
  (load-file custom-file))

(my-load-custom-file)

(use-package package
  :demand t
  :preface
  (setopt package-quickstart nil)
  :config
  (defvar package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))


(progn ; my-themes
  (defvar my-font)
  (defvar my-light-theme)
  (defvar my-dark-theme)
  (defvar my-need-theme-reload)
  (if (eq system-type 'darwin)
      (setq my-font (font-spec :size 15.0 :family "Go Mono"))
    (setq my-font (font-spec :size 13.0 :family "PT Mono")))
  (setq my-light-theme 'ef-cyprus
	my-dark-theme 'ef-cherie
	my-need-theme-reload nil)

  (defun my-enable-light-theme ()
    "Enable light theme."
    (interactive)
    (mapc #'disable-theme custom-enabled-themes)
    (progn
      (load-theme my-light-theme t)))

  (defun my-enable-dark-theme ()
    "Enable dark theme."
    (interactive)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme my-dark-theme t))

  (defun my-set-themes ()
    "Function for setting themes after init."
    (interactive)
    (mapc #'disable-theme custom-enabled-themes)
    (if (ignore-errors
	  (string-suffix-p "-dark"
			   (string-replace "'" "" (car
						   (process-lines
						    "gsettings" "get"
						    "org.gnome.desktop.interface"
						    "gtk-theme")))))
	(load-theme my-dark-theme t)
      (load-theme my-light-theme t)))

  (defun my-toggle-themes ()
    "Toggle light and dark themes."
    (interactive)
    (let ((cur-theme (if (equal
			  (car custom-enabled-themes)
			  my-light-theme)
			 'light 'dark)))
      (if (equal cur-theme 'light)
	  (my-enable-dark-theme)
	(my-enable-light-theme))))

  (defun my-reload-theme ()
    "Reload current theme."
    (interactive)
    (set-frame-font my-font nil t)
    (if my-need-theme-reload
	(mapc
	 (lambda (theme)
	   (disable-theme theme)
	   (load-theme theme t))
	 custom-enabled-themes)))

  (defun my-reload-theme--frame (_frame)
    "Reload theme after make frame."
    (my-reload-theme))

  (defun my-load-theme ()
    "Load theme."
    (interactive)
    (let ((theme (intern
		  (completing-read "Load custom theme: "
				   (mapcar #'symbol-name
					   (custom-available-themes))))))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t nil)))

  (defun my-load-builtin-theme ()
    "Load theme."
    (interactive)
    (let ((theme (intern
		  (completing-read "Load custom theme: "
				   (cl-delete-if
				    (lambda (theme)
				      (file-in-directory-p
				       (locate-file
					(concat theme "-theme.el")
					(custom-theme--load-path)
					'("" "c"))
				       (getenv "HOME")))
				    (mapcar #'symbol-name
					    (custom-available-themes)))))))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t nil)))

  (add-to-list 'after-make-frame-functions #'my-reload-theme--frame)
  (add-hook 'server-after-make-frame-hook #'my-reload-theme)
  (add-hook 'after-init-hook #'my-reload-theme)
  (with-eval-after-load 'emacs-customizations #'my-set-themes)

  (global-set-key (kbd "<f6>") 'my-toggle-themes)
  (add-hook 'after-init-hook 'my-set-themes)
  (add-hook 'desktop-after-read-hook 'my-set-themes)
  ;; (add-hook 'after-init-hook 'my-load-custom-file)
  )

;; (while (not (eq system-type 'darwin)) ; my-gnome-night-light-light
;;   (defvar my-gnome-night-light-light-change-callback)
;;   (require 'dbus)
;;   (defun my-gnome-night-light-internal-prop-change-listener (_name changed-props _)
;;     (let* ((prop (car changed-props))
;; 	   (name (car prop))
;; 	   (value (car (cadr prop))))
;;       (when (string-equal name "NightLightActive")
;; 	(when (functionp my-gnome-night-light-light-change-callback)
;; 	  (funcall my-gnome-night-light-light-change-callback value)))))

;;   (defun my-gnome-night-light ()
;;     "Load and enable my-gnome-night-light."
;;     (dbus-register-signal
;;      :session
;;      "org.gnome.SettingsDaemon.Color"
;;      "/org/gnome/SettingsDaemon/Color"
;;      "org.freedesktop.DBus.Properties"
;;      "PropertiesChanged"
;;      #'my-gnome-night-light-internal-prop-change-listener)
;;     (let ((value (dbus-get-property
;; 		  :session
;; 		  "org.gnome.SettingsDaemon.Color"
;; 		  "/org/gnome/SettingsDaemon/Color"
;; 		  "org.gnome.SettingsDaemon.Color"
;; 		  "NightLightActive")))
;;       (when (functionp my-gnome-night-light-light-change-callback)
;; 	(funcall my-gnome-night-light-light-change-callback value))))

;;   (defun my-theme-changer (state)
;;     "My callback for gnome-night-light.\nChanges theme according to STATE."
;;     (mapc #'disable-theme custom-enabled-themes)
;;     (if state
;; 	(load-theme my-dark-theme t nil)
;;       (load-theme my-light-theme t nil)))

;;   (setq my-gnome-night-light-light-change-callback 'my-theme-changer)
;;   (defvar my-gnome-night-light-light-change-callback nil
;;     "The callback function called on Night Light state change.
;; It takes one parameter, which is t when the Night Light is active
;; (e.g.  it's night) and nil when it's day.")
;;   (my-gnome-night-light))

(progn ; my-mac-themes
  (defun my-mac-apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme my-light-theme t))
      ('dark (load-theme my-dark-theme t))))

  (if (eq system-type 'darwin)
      (add-hook 'ns-system-appearance-change-functions #'my-mac-apply-theme)))

(use-package key-chord
  :commands key-chord-define-global
  :bind (([f9] . key-chord-mode))
  :config
  (run-with-idle-timer 2 nil #'require 'key-chord nil t)
  (with-eval-after-load 'key-chord
    (key-chord-mode 1)))

(use-package flymake
  :hook ((prog-mode-hook . flymake-mode)
         (emacs-lisp-mode-hook . flymake-mode))
  :bind (("C-x `" . flymake-goto-next-error)
         ("C-c r" . consult-flymake)))

(setq flymake-mode-line-format '(" " flymake-mode-line-counters))
(setq-default mode-line-format
	      (list
	       "["
	       ;; was this buffer modified since the last save?
	       '(:eval (when (buffer-modified-p)
			 (propertize "*"
				     'face 'font-lock-warning-face
				     'help-echo "Buffer has been modified")))

	       ;; is this buffer read-only?
	       '(:eval (when buffer-read-only
			 (propertize "RO"
				     'face 'font-lock-type-face
				     'help-echo "Buffer is read-only")))
	       "] "

	       ;; the buffer name; the file name as a tool tip
	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
				   'help-echo (buffer-file-name)))

	       ;; line and column
	       ;; "(" ;; '%02' to set to 2 chars at least; prevents flickering
	       (propertize "%02l" 'face 'font-lock-constant-face) ":"
	       (propertize "%02c" 'face 'font-lock-constant-face)
	       ;; ") "

	       "    "
	       '(:eval (when (stringp vc-mode)
			 vc-mode))
	       ;; the current major mode for the buffer.
	       "    ["

	       '(:eval (propertize "%m" 'face 'font-lock-string-face
				   'help-echo buffer-file-coding-system))
	       "] "

	       '("" mode-line-process)

	       ;; flymake errors
	       '(:eval (when flymake--state flymake-mode-line-format))

	       ;; relative position, size of file
	       "    ["
	       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
	       "/"
	       (propertize "%I" 'face 'font-lock-constant-face) ;; size
	       "] "))

(use-package vc-mode
  :preface
  (defun my-update-vc-mode ()
    "Update variable `vc-mode' for modeline."
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s"
                             (vc-backend buffer-file-name))
                     " " vc-mode)))
        (setq vc-mode (propertize vc-mode 'face
                                  (cond
                                   ((string-match "^ -" noback)
                                    'font-lock-keyword-face)
                                   ((string-match "^ [:@]" noback)
                                    'font-lock-warning-face)
                                   ((string-match "^ [!\\?]" noback)
                                    'font-lock-warning-face)))))))

  :hook ((after-revert-hook . my-update-vc-mode)
         (after-find-file . my-update-vc-mode)))

(use-package text-defaults
  :bind (("M-J" . scroll-up-line)
         ("M-K" . scroll-down-line))
  :hook ((after-init-hook . global-font-lock-mode))
  :preface
  (setq inhibit-startup-message t
	frame-title-format "emacs - %b"
	auto-window-vscroll nil
	require-final-newline t
	next-line-add-newlines nil)
  (mouse-wheel-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'downcase-region 'disabled nil)
  (delete-selection-mode)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (setq sentence-end-double-space nil))

(defun my-toggle-window-split ()
  "Toggle window split vertically or horizontally."
  (interactive)
  (if (=
       (count-windows)
       2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer
                               (next-window)))
             (this-win-edges (window-edges
                              (selected-window)))
             (next-win-edges (window-edges
                              (next-window)))
             (this-win-2nd (not (and
                                 (<=
                                  (car this-win-edges)
                                  (car next-win-edges))
                                 (<=
                                  (cadr this-win-edges)
                                  (cadr next-win-edges)))))
             (splitter (if (=
                            (car this-win-edges)
                            (car (window-edges
                                  (next-window))))
                           'split-window-horizontally 'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd
              (other-window 1))
          (set-window-buffer
           (selected-window)
           this-win-buffer)
          (set-window-buffer
           (next-window)
           next-win-buffer)
          (select-window first-win)
          (if this-win-2nd
              (other-window 1))))))

(bind-key (kbd "C-x t") 'my-toggle-window-split)

(use-package hydra
  :disabled t
  :bind (("C-x t" . my-toggle-window-split)
         ;; ("C-x o" . hydra-cycle-windows/body)
	 )
  :config
  ;; (defhydra hydra-cycle-windows
  ;;   (:body-pre
  ;;    (other-window 1))
  ;;   "Windows"
  ;;   ("o"
  ;;    (other-window 1)
  ;;    "Next")
  ;;   ("O"
  ;;    (other-window -1)
  ;;    "Previous")
  ;;   ("t" my-toggle-window-split "Toggle split")
  ;;   ("]" enlarge-window-horizontally "Enlarge horizontal")
  ;;   ("[" shrink-window-horizontally "Shrink horizontal")
  ;;   ("=" enlarge-window "Enlarge vertival")
  ;;   ("-" shrink-window "Shrink vertical")
  ;;   ("b" balance-windows "Balance windows")
  ;;   ("m" delete-other-windows "Maximize window")
  ;;   ("n" split-window-below "New window")
  ;;   ("c" delete-window "Close window")
  ;;   ("q" nil "quit"))
  )

(use-package eglot
  :commands eglot-code-actions
  :demand t
  :preface
  (setq eglot-strict-mode nil)
  (setq-default eglot-workspace-configuration '((:gopls :usePlaceholders t :staticcheck t :completeUnimported t)))
  (setq-default eglot-confirm-server-initiated-edits nil)
  :config
  (fset 'jsonrpc--log-event #'ignore)
  (defun my-eglot-organize-imports ()
    (interactive)
    (ignore-errors
      (eglot-code-actions nil nil "source.organizeImports" t)))
  (defun my-eglot-setup ()
    (interactive)
    (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
    (add-hook 'before-save-hook 'eglot-format-buffer nil t))

  (define-key eglot-mode-map (kbd "C-x l h h") 'eldoc)
  (define-key eglot-mode-map (kbd "C-x l w q") 'eglot-shutdown)
  (define-key eglot-mode-map (kbd "C-x l w r") 'eglot-reconnect)
  (define-key eglot-mode-map (kbd "C-x l r r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-x l r o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-x l a a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-x l g i") 'eglot-find-implementation)

  (add-hook 'eglot-managed-mode-hook 'my-eglot-setup))

(progn ; exec-path
  (setq exec-path (append
		   (mapcar 'expand-file-name
			   '("~/.opam/default/bin"
			     "~/go/bin"
			     "/opt/local/bin"
			     "/usr/local/bin"
			     "~/.cargo/bin"
			     "/usr/local/opt/llvm/bin"
			     "~/.local/bin"
			     "~/.dotnet/tools"
			     "/opt/homebrew/bin"
			     "/opt/homebrew/Cellar/libpq/15.2/bin"
			     "/opt/homebrew/anaconda3/bin"))
		   exec-path))
  (setenv "PATH"
	  (string-join exec-path ":")))

(setq-default project-vc-extra-root-markers
	      '("TAGS" "GTAGS"                                          ; tags
		"configure.ac" "configure.in"                           ; autoconf
		"cscope.out"                                            ; cscope
		"SConstruct"                                            ; scons
		"meson.build"                                           ; meson
		"default.nix" "flake.nix"                               ; nix
		"WORKSPACE"                                             ; bazel
		"debian/control"                                        ; debian
		"Makefile" "GNUMakefile" "CMakeLists.txt"               ; Make & CMake
		"composer.json"                                         ; PHP
		"rebar.config" "mix.exs"                                ; Erlang & Elixir
		"Gruntfile.js" "gulpfile.js" "package.json" "angular.json"
					; JavaScript
		"manage.py" "requirements.txt" "setup.py" "tox.ini" "Pipfile" "poetry.lock"
					; Python
		"pom.xml" "build.gradle" "gradlew" "application.yml"    ; Java & friends
		"build.sbt" "build.sc"                                  ; Scala
		"project.clj" "build.boot" "deps.edn" ".bloop"          ; Clojure
		"Gemfile"                                               ; Ruby
		"shard.yml"                                             ; Crystal
		"Cask" "Eldev" "Keg" "Eask"                             ; Emacs
		"DESCRIPTION"                                           ; R
		"bower.json" "psc-package.json" "spago.dhall"           ; PureScript
		"stack.yaml" "*.cabal"                                  ; Haskell
		"Cargo.toml"                                            ; Rust
		"info.rkt"                                              ; Racket
		"pubspec.yaml"                                          ; Dart
		"dune-project"                                          ; OCaml
		"go.mod"                                                ; Go
		"*.cproj"                                               ; c#
		"*.fsproj"                                              ; f#
		".project"
		))

(use-package treesit-auto
  :init
  (my-vc-install :name "treesit-auto" :host "github" :repo "renzmann/treesit-auto")
  :disabled t
  :demand t
  :config
  (defun my-install-language-grammar (lang)
    (when (not (file-exists-p
		(expand-file-name
		 (format "tree-sitter/libtree-sitter-%s.so" lang)
		 user-emacs-directory)))
      (treesit-install-language-grammar lang)))

  (mapc 'my-install-language-grammar
	'(go gomod elisp c cpp js python rust markdown typescript tsx yaml make
	     json csharp css cmake html bash haskell))

  (treesit-auto-install-all))

(use-package go-mode
  :init
  (add-hook 'go-mode-hook 'go-ts-mode))

(progn ; go
  (progn
    (defun my-extract-go-module-name ()
      (let* ((go-mod-file (expand-file-name
			   "go.mod"
			   (if (project-current)
			       (project-root (project-current))
			     default-directory)))
	     (name
	      (with-temp-buffer
		(find-file-noselect-1 (current-buffer) go-mod-file t t go-mod-file 2)
		(string-remove-prefix "module "
				      (buffer-substring-no-properties (point-min)
								      (progn
									(goto-char (point-min))
									(end-of-line)
									(point)))))))
	name)))
  (defun my-convert-go-time-ext ()
    "Convert go time ext to datetime."
    (interactive)
    (let ((num (number-at-point)))
      (message
       "%s"
       (format-time-string
	"%Y-%m-%d %H:%M:%S"
	(seconds-to-time
	 (+ num (time-to-seconds
		 (encode-time '(0 0 0 1 1 1 nil nil nil)))))))))

  (use-package go-ts-mode
    :functions (go-test-current-project
		go-goto-imports
		my-go-test
		go-tag-add
		go-tag-remove
		go-gen-test-dwim
		symbol-overlay-mode
		flymake-start
		my-go-mode-hook)
    :mode ("\\.go\\'"
	   ("go.mod$" . go-mod-ts-mode))
    :config
    (with-eval-after-load 'go-ts-mode
      (progn
        (setenv "GOPATH"
                (concat
                 (getenv "HOME")
                 "/go"))
        (setenv "PATH"
                (concat
                 (getenv "PATH")
                 ":/usr/local/bin:"
                 (getenv "GOPATH")
                 "/bin"))
        (defun my-go-mode-hook ()
	  "Setup for go."
	  (if (eq system-type 'darwin)
	      (setenv "GOROOT"
		      (string-trim
		       (shell-command-to-string "find /opt/homebrew/Cellar/go -type 'd' -name 'libexec'"))))
	  (require 'go-impl)
	  (require 'gotest)
	  (defun my-go-test (arg)
	    (interactive "P")
	    (if arg
                (pcase (completing-read "go test "
                                        '("current project" "current directory"))
		  ("current project"
		   (let ((default-directory (project-root
					     (project-current)))
                         (current-prefix-arg nil))
		     (go-test-current-project)))
		  (_
		   (go-test-current-project)))
	      (go-test-current-project)))

	  (setopt go-tag-args (list "-transform" "snakecase"))
	  (local-set-key
	   (kbd "C-c i")
	   #'go-goto-imports)
	  (local-set-key
	   (kbd "C-c C-t")
	   #'my-go-test)
	  (local-set-key
	   (kbd "C-c t")
	   #'go-tag-add)
	  (local-set-key
	   (kbd "C-c T")
	   #'go-tag-remove)
	  (local-set-key
	   (kbd "C-c g")
	   #'go-gen-test-dwim)
	  (local-set-key
	   (kbd "C-c C-c")
	   #'my-make)
	  (symbol-overlay-mode -1)
	  (eglot-ensure)
	  (flymake-start t))

        (add-hook 'go-ts-mode-hook #'my-go-mode-hook)))))

(setq make-backup-files nil
      text-mode-hook 'turn-on-auto-fill)

(use-package corfu
  :functions corfu-echo-mode
  :commands (global-corfu-mode
	     corfu-history-mode)
  :preface
  (setopt corfu-auto t)
  (setopt corfu-auto-delay 0.1)
  (setopt corfu-auto-prefix 1)
  ;; (global-set-key (kbd "<tab>") 'indent-for-tab-command)
  (setopt tab-always-indent 'complete)
  (setopt tab-first-completion nil)
  :init
  (global-corfu-mode t)
  (corfu-echo-mode t)
  (corfu-history-mode t))

(use-package bash-dynamic-completion
  :commands bash-completion-dynamic-complete
  :hook ((shell-dynamic-complete-functions . bash-completion-dynamic-complete)))

(use-package eldoc
  :hook ((emacs-lisp-mode-hook . eldoc-mode)
         (lisp-interaction-mode-hook . eldoc-mode)
         (ielm-mode-hook . eldoc-mode)))

(use-package dumb-jump
  :hook ((xref-backend-functions . dumb-jump-xref-activate)))

(use-package browse-url
  :preface
  (defun my-browse-url-chromium-wayland (url &optional _ignored)
    "Pass the specified URL to the \"chromium\" command.
The optional argument IGNORED is not used."
    (interactive (if (fboundp 'browse-url-interactive-arg)
		     (browse-url-interactive-arg "URL: ")))
    (let ((cmd
           (concat
            "DISPLAY=:0 HOME=" (getenv "HOME")
            " PATH=" (getenv "PATH")
            " XAUTHORITY=" (getenv "XAUTHORITY")
            " WAYLAND_DISPLAY=" (getenv "WAYLAND_DISPLAY")
            " USER=" (getenv "USER")
            " GDK_BACKEND=x11 /usr/bin/setsid -w chromium " url)))
      (start-process-shell-command "browser" "*chromium-open-url*" cmd)))

  (setq browse-url-browser-function 'my-browse-url-chromium-wayland))

(use-package my-open-multiple-files
  :hook ((window-setup-hook . delete-other-windows)))

(use-package pkgbuild-mode
  :when (file-exists-p "/etc/pacman.conf")
  :mode ("/PKGBUILD$"))

(use-package poly-markdown
  :mode ("\\.text\\'"
         ("\\.md$" . poly-gfm-mode)
         "\\.markdown$"))

(use-package flymake-proselint
  :hook ((markdown-mode-hook . flymake-proselint-setup)
         (gfm-mode-hook . flymake-proselint-setup)))

(use-package vmd-mode
  :disabled t
  :functions vmd-mode--update-emojis-file
  :preface
  (defun my-github-emojis-complete-at-point ()
    "My function for complete github emoji at point."
    (let ((start (or
                  (car (bounds-of-thing-at-point 'symbol))
                  (point))))
      (if (char-equal
           (char-before start)
           58)
          (list start
                (point)
                vmd-mode-github-emojis-list :exit-function
                (lambda (_candidate _status)
                  (insert ":")))
        nil)))

  (defun my-enable-emojis-completion ()
    "Enable completion github emojis."
    (cl-pushnew 'my-github-emojis-complete-at-point completion-at-point-functions))

  (defun my-bind-md-preview-key ()
    "Rebind markdown preview."
    (define-key markdown-mode-command-map
		(kbd "p")
		'vmd-mode))

  :after markdown-mode
  :bind ((:map markdown-mode-command-map
               ("p" . vmd-mode)))
  :hook ((markdown-mode-hook . my-enable-emojis-completion)
         (markdown-mode-hook . my-bind-md-preview-key))
  :config
  (setq vmd-mode--emojis-file (expand-file-name "~/.emacs.d/.github-emojis"))
  (unless (file-exists-p vmd-mode--emojis-file)
    (vmd-mode--update-emojis-file))
  (setq vmd-mode-github-emojis-list (and
                                     (file-exists-p vmd-mode--emojis-file)
                                     (with-temp-buffer
                                       (insert-file-contents vmd-mode--emojis-file)
                                       (split-string
                                        (buffer-string)
                                        "\n" t)))))

(use-package emmet-mode
  :disabled t
  :hook (sgml-mode-hook web-mode-hook rjsx-mode css-mode-hook)
  (setq emmet-move-cursor-between-quotes t))

(use-package avy
  :commands avy-isearch
  :preface
  (key-chord-define-global "fj" 'avy-goto-word-1)
  (key-chord-define-global "f'" 'avy-pop-mark)
  :config
  (define-key isearch-mode-map (kbd "C-'") #'avy-isearch))


(use-package expand-region
  :preface
  (key-chord-define-global "zj" 'er/expand-region)
  (key-chord-define-global "zk" 'er/contract-region))

(use-package multiple-cursors
  :commands (multiple-cursors-hydra/body
	     mc/edit-lines
	     mc/mark-all-like-this
	     mc/mark-all-words-like-this
	     mc/mark-next-like-this
	     mc/skip-to-next-like-this
	     mc/unmark-next-like-this
	     mc/mark-previous-like-this
	     mc/skip-to-previous-like-this
	     mc/unmark-previous-like-this
	     mc/mark-all-in-region-regexp
	     mc/mark-previous-word-like-this
	     mc/mark-next-word-like-this
	     mc-hide-unmatched-lines-mode
	     ace-mc-add-multiple-cursors
	     ace-mc-add-single-cursor)
  :init
  (defvar my-mc-map
    (define-keymap
      "l" #'mc/edit-lines
      "a" #'mc/mark-all-like-this
      "A" #'mc/mark-all-words-like-this
      "n" #'mc/mark-next-like-this
      "N" #'mc/skip-to-next-like-this
      "M-n" #'mc/unmark-next-like-this
      "p" #'mc/mark-previous-like-this
      "P" #'mc/skip-to-previous-like-this
      "M-p" #'mc/unmark-previous-like-this
      "r" #'mc/mark-all-in-region-regexp
      "W" #'mc/mark-previous-word-like-this
      "w" #'mc/mark-next-word-like-this
      "h" #'mc-hide-unmatched-lines-mode
      "j" #'ace-mc-add-multiple-cursors
      "k" #'ace-mc-add-single-cursor))
  (define-key global-map (kbd "C-c m") my-mc-map)
  (hercules-def
   :transient t
   :keymap 'my-mc-map
   :show-funs '(mc/edit-lines
	     mc/mark-all-like-this
	     mc/mark-all-words-like-this
	     mc/mark-next-like-this
	     mc/skip-to-next-like-this
	     mc/unmark-next-like-this
	     mc/mark-previous-like-this
	     mc/skip-to-previous-like-this
	     mc/unmark-previous-like-this
	     mc/mark-previous-word-like-this
	     mc/mark-next-word-like-this
	     mc-hide-unmatched-lines-mode)
   :hide-funs '(mc/mark-all-in-region-regexp
		ace-mc-add-multiple-cursors
		ace-mc-add-single-cursor)))

(use-package yasnippet
  :defines (yas-inhibit-overlay-modification-protection)
  :commands yas-global-mode
  ;; :bind (([tab]
  ;;         . tab-indent-or-complete)
  ;;        ("TAB" . tab-indent-or-complete))
  :config
  (setq yas-inhibit-overlay-modification-protection t)
  (run-with-idle-timer 3 nil #'require 'yasnippet nil t)
  (with-eval-after-load 'yasnippet
    (progn
      (yas-global-mode 1))))

(setopt x-hyper-keysym 'meta
	mac-option-modifier 'none
	mac-command-modifier 'meta
	mac-command-key-is-meta 't
	mac-option-key-is-meta nil)

(use-package wgrep
  :bind (("C-c C-p" . wgrep-change-to-wgrep-mode))
  :config
  (with-eval-after-load 'wgrep
    (setq wgrep-auto-save-buffer t)))

(setq select-enable-primary t
      select-enable-clipboard  t)

(use-package mouse
  :bind (([drag-mouse-0]
          . mouse-set-region))
  :preface
  (setq mouse-drag-copy-region t)
  :config
  (xterm-mouse-mode t))

(use-package imenu
  :bind (("M-i" . imenu)))

(use-package magit
  :commands diff-refine-hunk
  :functions my-magit-diff-hook
  :bind* (("C-x g" . magit-status))
  :config
  (require 'diff-mode)
  (with-eval-after-load 'magit
    (progn
      (defun my-magit-diff-hook ()
        "My hook for improve magit diff."
        (local-set-key
         (kbd "h")
         #'diff-refine-hunk))

      (add-hook 'magit-diff-mode-hook #'my-magit-diff-hook)
      (setopt auto-revert-check-vc-info t))))



(use-package edit-indirect
  :preface
  (key-chord-define-global ";r" 'edit-indirect-region))

(use-package pandoc
  :hook ((markdown-mode-hook . pandoc-mode)
         (pandoc-mode-hook . pandoc-load-default-settings)))

(use-package open-urls
  :bind (("C-x u" . link-hint-open-multiple-links)))

(use-package xml-mode
  :mode (("\\.xsd\\'" . xml-mode)
         ("\\.xslt\\'" . xml-mode)))

(use-package hungry-delete
  :commands global-hungry-delete-mode
  :demand t
  :config
  (run-with-idle-timer 0.5 nil #'require 'hungry-delete nil t)
  (with-eval-after-load 'hungry-delete
    (global-hungry-delete-mode)))

(use-package ace-link
  :disabled t
  :functions ace-link-setup-default
  :preface
  (run-with-idle-timer 0.1 nil #'require 'ace-link nil t)
  (with-eval-after-load 'ace-link
    (ace-link-setup-default)))

(use-package which-key
  :commands which-key-mode
  :demand t
  :preface
  (setq which-key-show-transient-maps 't)
  :config
  (which-key-mode t))

(use-package ibuffer-vc
  :commands ibuffer-vc-set-filter-groups-by-vc-root
  :bind* (("C-x C-b" . ibuffer))
  :init
  (run-with-idle-timer 3 nil #'require 'ibuffer-vc nil t)
  (with-eval-after-load 'ibuffer-vc
    (add-hook 'ibuffer-hook
              (lambda nil
                (ibuffer-vc-set-filter-groups-by-vc-root)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))))

(use-package symbol-overlay
  :commands symbol-overlay-mode
  :config
  (run-with-idle-timer 2 nil #'require 'symbol-overlay nil t)
  (with-eval-after-load 'symbol-overlay
    (setq-default symbol-overlay-temp-in-scope t)
    (add-hook 'prog-mode-hook #'symbol-overlay-mode)
    (add-hook 'emacs-lisp-mode-hook #'symbol-overlay-mode)))

(use-package reverse-im
  :commands (reverse-im-add-input-method reverse-im-mode)
  :demand t
  :config
  (reverse-im-add-input-method "russian-computer")
  (reverse-im-mode t))

(use-package aggressive-indent
  :demand t
  :commands (aggressive-indent-global-mode aggressive-indent-mode)
  :preface
  (defun my-disable-aggressive-indent ()
    "Disable aggressive indent mode in current buffer."
    (interactive)
    (aggressive-indent-mode -1))

  :hook ((fsharp-mode-hook . my-disable-aggressive-indent))
  :config
  (aggressive-indent-global-mode))

(use-package password-store
  :commands password-store-get)

(use-package pass
  :commands pass)

(use-package dash
  :commands global-dash-fontify-mode
  :after t
  :config
  (global-dash-fontify-mode t))

(use-package grep
  :bind* (("C-c C-s" . my-lgrep))
  :bind (:map grep-mode-map
	      ("e" . wgrep-change-to-wgrep-mode))
  :functions (grep-apply-setting grep-read-regexp grep-read-files)
  :demand t
  :config
  (defun my-lgrep ()
    "Recursive grep in current project"
    (interactive)
    (let* ((regexp (grep-read-regexp))
	   (files (grep-read-files regexp))
	   (dir (read-directory-name "In directory: "
				     (if (project-current)
					 (project-root (project-current))
				       default-directory)))
	   (confirm (equal current-prefix-arg '(4))))
      (lgrep regexp files dir confirm)))
  
  (grep-apply-setting 'grep-template "rg --no-heading -H -S -g '<F>' -e <R> <D>"))

(use-package isearch
  :commands consult-line
  :preface
  (defun my-isearch-next (arg)
    "Isearch symbol at point or next isearch history item."
    (interactive "p")
    (if (string= isearch-string "")
        (isearch-yank-string
         (format "%s"
                 (or
                  (symbol-at-point)
                  "")))
      (next-history-element arg)))

  (defun my-consult-line-from-isearch ()
    (interactive)
    (consult-line (or isearch-string (thing-at-point 'symbol) "")))

  :bind ((:map isearch-mode-map
               ("M-i" . my-consult-line-from-isearch)
               ("M-n" . my-isearch-next)))
  :config
  (setq search-whitespace-regexp ".*")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil))

(put 'upcase-region 'disabled nil)

(use-package comment-tags
  :hook (prog-mode-hook))

(use-package eww-more-readable
  :preface
  (defun eww-more-readable ()
    "Better eww.  Run it after eww buffer is loaded."
    (interactive)
    (set-window-margins
     (get-buffer-window)
     20 20)
    (if (fboundp 'eww-reload)
	(eww-reload 'local)))

  :hook ((eww-after-render-hook . eww-more-readable)))

(use-package yaml-ts-mode
  :commands (global-smart-shift-mode prism-mode)
  :preface
  (defun my-disable-auto-fill ()
    "Disable `auto-fill-mode'."
    (auto-fill-mode -1))

  (defun my-enable-prism ()
    "Enable `prism-mode'."
    (prism-mode t))

  (key-chord-define-global "<<" 'smart-shift-left)
  (key-chord-define-global ">>" 'smart-shift-right)

  :hook ((yaml-ts-mode-hook . highlight-indentation-mode)
	 (yaml-ts-mode-hook . highlight-indentation-current-column-mode)
	 (yaml-ts-mode-hook . my-disable-auto-fill)
	 (yaml-ts-mode-hook . my-enable-prism))
  :config
  (global-smart-shift-mode 1))

(use-package auto-yasnippet
  :bind (("C-c y" . aya-create)
         ("C-." . aya-expand))
  :config
  (with-eval-after-load 'auto-yasnippet
    (setq aya-field-regex "\\sw\\|\\s_\\|\\*\\|\\&")))

(use-package toggle-fullscreen
  :bind (("ff" . toggle-frame-fullscreen)))


(use-package open-file-as-root
  :preface
  (defun open-this-file-as-root ()
    "Edit current file as root, using `tramp' and `sudo'.
If the current buffer is not visiting a file, prompt for a file name."
    (interactive)
    (let* ((filename (or buffer-file-name
                         (read-file-name "Find file (as root): ")))
           (tramp-path (concat "/sudo:root@localhost:" filename)))
      (if buffer-file-name
          (find-alternate-file tramp-path)
        (find-file tramp-path))))

  :bind (("C-c C-r" . open-this-file-as-root)))

(use-package my-go-home
  :preface
  (defun my-go-home ()
    "Go to home dir."
    (cd
     (getenv "HOME")))

  :hook ((after-init-hook . my-go-home)))

(use-package so-long
  :when (>= emacs-major-version 27)
  :hook ((after-init-hook . global-so-long-mode))
  :preface
  (setq bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t))

(use-package hippie-expand
  :bind (("C-;" . hippie-expand)))

(use-package dtrt-indent
  :hook (prog-mode-hook protobuf-mode-hook))

;;; Screencasts

(use-package gif-screencast
  :functions (my-fix-screencast-hidpi
	      gif-screencast--cropping-region)
  :bind (("<f7>" . gif-screencast-toggle-pause)
         ("<f8>" . gif-screencast-start-or-stop))
  :config
  (with-eval-after-load 'gif-screencast
    (if (eq system-type 'darwin)
        (progn
          (setq gif-screencast-args '("-x" "-o"))
          (setq gif-screencast-cropping-program "mogrify")
          (setq gif-screencast-capture-format "ppm")
          (defun my-fix-screencast-hidpi (oldfun &rest r)
            (apply #'format "%dx%d+%d+%d"
                   (mapcar
                    (lambda (x)
                      (* 2
                         (string-to-number x)))
                    (split-string
                     (apply oldfun r)
                     "[+x]"))))

          (if (fboundp 'gif-screencast--cropping-region)
	      (advice-add #'gif-screencast--cropping-region :around #'my-fix-screencast-hidpi)))

      (setq gif-screencast-program "gnome-screenshot"
            gif-screencast-args '("-w" "-f")
            gif-screencast-capture-format "png"))
    (setq gif-screencast-capture-prefer-internal t)))

(use-package c-cpp-mode
  :hook ((c-mode-hook . eglot-ensure)
         (c++-mode-hook . eglot-ensure)))

(use-package icomplete
  :functions my-directory-tidy
  :bind ((:map icomplete-minibuffer-map
               ("<down>" . icomplete-forward-completions)
               ("C-n" . icomplete-forward-completions)
               ("<up>" . icomplete-backward-completions)
               ("C-p" . icomplete-backward-completions)
               ("C-v" . icomplete-vertical-toggle)
               ("<backspace>" . icomplete-fido-backward-updir)
               ("C-j" . icomplete-force-complete)
               ("C-M-j" . exit-minibuffer)
               ("<RET>" . icomplete-force-complete-and-exit)))
  :preface
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        icomplete-show-matches-on-no-input t
        icomplete-prospects-height 10)
  :init
  (defun my-directory-tidy () ;; thanks minad - this part from vertico
    "Tidy shadowed file name, see `rfn-eshadow-overlay'."
    (when (and (eq this-command #'self-insert-command)
               (bound-and-true-p rfn-eshadow-overlay)
               (overlay-buffer rfn-eshadow-overlay)
               (= (point) (point-max))
               (or (>= (- (point) (overlay-end rfn-eshadow-overlay)) 2)
                   (eq ?/ (char-before (- (point) 2)))))
      (delete-region (overlay-start rfn-eshadow-overlay) (overlay-end rfn-eshadow-overlay))))

  (add-hook 'rfn-eshadow-update-overlay-hook #'my-directory-tidy)

  (setq-default completion-styles '(basic partial-completion emacs22 initials flex))
  (setq-default completion-category-overrides '((file (styles basic substring))
						(consult-location (styles basic substring))))
  (icomplete-mode)
  (icomplete-vertical-mode)
  (setq completion-ignore-case t))

(use-package consult
  :preface
  (setq completion-in-region-function 'consult-completion-in-region)

  (defun my-consult-project-rg ()
    (interactive)
    (let ((xref-search-program 'ripgrep)
          (xref-show-xrefs-function 'xref--show-defs-minibuffer))
      (call-interactively 'project-find-regexp)))

  (defun my-advices-inhibit-if-non-essential (oldfun &rest args)
    "An around advice that inhibit OLDFUN if `non-essential' is non-nil."
    (unless non-essential
      (apply oldfun args)))
  
  :bind (("C-x b" . consult-buffer)
         ("M-i" . consult-imenu)
         ("M-y" . consult-yank-pop)))

(use-package marginalia
  :commands marginalia-mode
  :init
  (marginalia-mode +1)
  (setopt marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package embark
  :disabled t
  :bind ((minibuffer-local-completion-map
          ("M-o" . embark-act-noexit)
          ("C-o" . embark-act)
          ("M-r" . embark-become))))


(setq read-process-output-max (* 1024 1024))

(use-package string-inflection
  :bind (("C-c C-u" . string-inflection-all-cycle)))

(use-package rust
  :hook ((rust-mode-hook . eglot-ensure))
  :preface
  (setq rust-format-on-save t))

(progn ; indent-region-workaround
  (defun my-suppress-messages (old-fun &rest args)
    (cl-flet ((silence
		(&rest _args1)
		(ignore)))
      (advice-add 'message :around #'silence)
      (advice-add 'make-progress-reporter :around #'silence)
      (advice-add 'progress-reporter-done :around #'silence)
      (advice-add 'progress-reporter-update :around #'silence)
      (advice-add 'progress-reporter-do-update :around #'silence)
      (advice-add 'progress-reporter-force-update :around #'silence)
      (unwind-protect
	  (apply old-fun args)
        (advice-remove 'message #'silence)
        (advice-remove 'make-progress-reporter #'silence)
        (advice-remove 'progress-reporter-done #'silence)
        (advice-remove 'progress-reporter-update #'silence)
        (advice-remove 'progress-reporter-do-update #'silence)
        (advice-remove 'progress-reporter-force-update #'silence))))

  (advice-add 'indent-region :around #'my-suppress-messages))

(use-package savehist
  :hook (after-init-hook-hook)
  :config
  (with-eval-after-load 'savehist
    (setq savehist-file "~/.emacs.d/savehist")
    (setq history-length 1000)
    (setq history-delete-duplicates t)
    (setq savehist-save-minibuffer-history t)))

(use-package saveplace
  :preface
  (setq save-place-file "~/.emacs.d/saveplace"
	save-place-forget-unreadable-files t)
  (save-place-mode 1))

(use-package recentf
  :preface
  (recentf-mode 1)
  (setq recentf-max-menu-items 300)
  (setq recentf-max-saved-items 300))

(use-package package-lint-flymake
  :commands package-lint-flymake-setup
  :preface
  (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup))

(progn ; my-make
  (defun my--make-target-list (makefile)
    "Return the target list for MAKEFILE by parsing it."
    (let (targets)
      (with-temp-buffer
	(insert-file-contents makefile)
	(goto-char (point-min))
	(while (re-search-forward "^\\([^: \n]+\\) *:\\(?: \\|$\\)" nil t)
	  (let ((str (match-string 1)))
	    (unless (string-match "^\\." str)
	      (push str targets))))
	(when (or (eq major-mode 'go-mode)
		  (eq major-mode 'go-ts-mode))
	  (push "go build" targets)))
      (nreverse targets)))

  (defun my-make (arg)
    "Make current project with targets selection.
Use project root as default directory if universal ARG is not set.
Select it interactively otherwise."
    (interactive "p")
    (let* ((project (project-current))
	   (default-directory (if (= arg 4)
				  (read-directory-name "select directory ")
				(if project
				    (project-root project)
				  default-directory)))
	   (makefile (expand-file-name "Makefile" default-directory)))
      (if (file-exists-p makefile)
	  (let* ((targets (my--make-target-list makefile))
		 (target (completing-read "make " targets)))
	    (compile
	     (if (string= target "go build")
		 "go build ./..."
	       (format "make %s" target))))
	(compile "go build ./...")))))

(use-package smerge-mode
  :preface
  (require 'hercules)
  (hercules-def :toggle-funs #'smerge-mode
                :keymap 'smerge-basic-map
                :show-funs '(smerge-next smerge-prev)
                :transient t))

(use-package imaxima
  :disabled t
  :init (require 'cl)
  :commands (imaxima))

(use-package frimacs
  :disabled t
  :preface
  (defun my-setup-frimacs ()
    "Setup frimacs."
    (aggressive-indent-mode -1))

  (setq frimacs-process-enable-pretty-print t)
  (setq frimacs-process-embed-gnu-draw t)
  (setq frimacs-process-show-svg t)
  :hook ((frimacs-process-mode-hook . my-setup-frimacs)))

;;; doesn't work on mac, see https://github.com/tecosaur/screenshot/issues/13
;; (progn ; my-screenshots
;;   (defun my-screenshot-svg ()
;;     "Save a screenshot of the current frame as an SVG image.
;; Saves to a temp file."
;;     (interactive)
;;     (require 'dired)
;;     (let* ((filename (make-temp-file "Emacs" nil ".svg"))
;; 	   (data (x-export-frames nil 'svg)))
;;       (with-temp-file filename
;;         (insert data))
;;       (dired-rename-file filename (expand-file-name (file-name-nondirectory filename)
;; 						    (expand-file-name "~/Pictures")) 1)))
;;   (defun my-screenshot-png ()
;;     "Save a screenshot of the current frame as an PNG image.
;; Saves to a temp file."
;;     (interactive)
;;     (require 'dired)
;;     (let* ((filename (make-temp-file "Emacs" nil ".png"))
;; 	   (data (x-export-frames nil 'png)))
;;       (with-temp-file filename
;;         (insert data))
;;       (dired-rename-file filename
;; 			 (expand-file-name (file-name-nondirectory filename)
;; 					   (expand-file-name "~/Pictures"))
;; 			 1))))

(use-package nov
  :commands spray-mode
  :mode (("\\.epub\\'" . nov-mode))
  :preface
  (use-package speechd
    :init
    (defun my-read-buffer ()
      (interactive)
      (if (fboundp 'speechd-stop)
	  (speechd-stop t))
      (if (fboundp 'speechd-set-language)
	  (speechd-set-language "ru"))
      (if (fboundp 'speechd-set-rate)
	  (speechd-set-rate 100 t))
      (if (fboundp 'speechd-say-text)
	  (speechd-say-text
	   (buffer-substring-no-properties (point) (point-max)))))

    (defun my-stop-reading ()
      (interactive)
      (if (fboundp 'speechd-stop)
	  (speechd-stop t))))

  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.6)
    (local-set-key (kbd "f") (lambda ()
                               (interactive)
                               (spray-mode)))
    (local-set-key (kbd "y") #'my-read-buffer)
    (local-set-key (kbd "s") #'my-stop-reading))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
  (setq nov-text-width 140)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)

  :config
  (use-package justify-kp
    :init
    (my-vc-install :name "justify-kp" :host "github" :repo "Fuco1/justify-kp"))
  (setq nov-text-width t)

  (defun my-nov-window-configuration-change-hook ()
    ;; (my-nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
                 'my-nov-window-configuration-change-hook
                 t))

  ;; (defun my-nov-post-html-render-hook ()
  ;;   (if (get-buffer-window)
  ;;       (let ((max-width (pj-line-width))
  ;;             buffer-read-only)
  ;;         (save-excursion
  ;;           (goto-char (point-min))
  ;;           (while (not (eobp))
  ;;             (when (not (looking-at "^[[:space:]]*$"))
  ;;               (goto-char (line-end-position))
  ;;               (when (> (shr-pixel-column) max-width)
  ;;                 (goto-char (line-beginning-position))
  ;;                 (pj-justify)))
  ;;             (forward-line 1))))
  ;;     (add-hook 'window-configuration-change-hook
  ;;               'my-nov-window-configuration-change-hook
  ;;               nil t)))

  ;; (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)
  )

(use-package fsharp-mode
  :hook ((fsharp-mode-hook . eglot-ensure)
         (fsharp-mode-hook . my-set-fsharp-compile-command))

  :init
  (require 'eglot-fsharp)
  (setq auto-mode-alist (cons '("\\.fsproj\\'" . nxml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.csproj\\'" . nxml-mode) auto-mode-alist))
  (setq inferior-fsharp-program "dotnet fsi")
  (defun my-set-fsharp-compile-command ()
    (interactive)
    (setq-local compile-command "dotnet build")))

(use-package csharp-mode
  :hook ((csharp-mode-hook . eglot-ensure)
	 (csharp-mode-hook . my-set-fsharp-compile-command)))

(use-package dotnet
  :disabled t
  :preface
  (add-hook 'csharp-mode-hook 'dotnet-mode)
  (add-hook 'fsharp-mode-hook 'dotnet-mode)

  (require 'project)
  (require 'cl-lib)

  (defun my-locate-dominating-file-regexp (regexp &optional directory)
    "Locate dominating file with REGEXP in DIRECTORY."
    (locate-dominating-file
     (or directory default-directory)
     (lambda (dir)
       (cl-first
	(cl-remove-if-not
	 (lambda (name) (string-match regexp name))
	 (directory-files dir))))))
  
  (defun my-try-dotnet (dir)
    "Find dotnet project root for DIR."
    (or
     (my-locate-dominating-file-regexp "\.sln$" dir)
     (my-locate-dominating-file-regexp "\..proj" dir)))

  (defun my-dotnet-project ()
    "Enable alternative project root find function."
    (setq-local project-find-functions
		(list #'my-try-dotnet #'project-try-vc)))

  (add-hook 'csharp-mode-hook 'my-dotnet-project)
  (add-hook 'fsharp-mode-hook 'my-dotnet-project))

(use-package dune
  :ensure t)

(use-package gopcaml-mode
  :preface
  (setq gopcaml-messaging-level 'none)
  (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      ;; Register Gopcaml mode
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'gopcaml-mode "gopcaml-mode" nil t nil)
      (autoload 'tuareg-mode "tuareg" nil t nil)
      (autoload 'merlin-mode "merlin" "Merlin mode" t)
      ;; Automatically start it in OCaml buffers
      (setq auto-mode-alist
	    (append '(("\\.ml[ily]?$" . gopcaml-mode)
		      ("\\.topml$" . gopcaml-mode))
		    auto-mode-alist))))

  (defun my-opam-env ()
    (interactive nil)
    (dolist (var (car (read-from-string
		       (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var))))

  (add-hook 'tuareg-mode-hook
	    #'(lambda ()
		(set (make-local-variable 'compile-command)
		     (concat "dune build"))
		(set (make-local-variable 'compilation-read-command)
		     nil)
		(my-opam-env)
		(eglot-ensure))))

(use-package haskell-mode
  :disabled t
  :preface
  (require 'haskell-interactive-mode)
  (defun my-haskell-setup ()
    "Setup haskell mode by hook."
    (defun my-send-region-to-haskell-interactive ()
      "Send region to haskell interactive."
      (interactive)
      (save-mark-and-excursion
	(when (not (region-active-p))
	  (move-end-of-line nil)
	  (set-mark-command nil)
	  (backward-sentence))
	(let ((content
	       (buffer-substring-no-properties
		(region-beginning) (region-end))))
	  (if (fboundp 'haskell-interactive-switch)
	      (haskell-interactive-switch))
	  (goto-char (point-max))
	  (insert content)
	  (if (fboundp 'haskell-interactive-mode-return)
	      (haskell-interactive-mode-return))
	  (if (fboundp 'haskell-interactive-switch-back)
	      (haskell-interactive-switch-back)))))
    (eglot-ensure))
  (add-hook 'haskell-mode-hook 'my-haskell-setup)
  :bind ((:map haskell-mode-map
	       ("C-c C-e" . my-send-region-to-haskell-interactive))))

(use-package denote
  :commands denote-link-buttonize-buffer
  :commands denote-dired-mode
  :demand t
  :bind
  (("C-c n n" . denote)
   ("C-c n d" . my-denote-dired)
   ("C-c n l" . denote-link)
   ("C-c n f" . denote-find-link))
  :config
  (require 'org)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (setq denote-file-type 'markdown-yaml)
  (defun my-denote-dired ()
    "Open dired buffer with denote notes."
    (interactive)
    (dired denote-directory)
    (denote-dired-mode)))

(use-package xeft
  :ensure t
  :preface
  (require 'denote)
  (setq xeft-directory (denote-directory))
  :bind
  (("C-c n x" . xeft))
  :config
  (bind-key (kbd "RET") 'denote xeft-mode-map))

(use-package consult-dash
  :commands dash-docs-activate-docset
  :bind
  (("M-s d" . consult-dash))
  :init
  (my-vc-install :name "consult-dash" :url "https://codeberg.org/ravi/consult-dash.git")
  :config
  (dash-docs-activate-docset "Go")
  (dash-docs-activate-docset "NET Framework"))

(use-package my-reopen-file
  :bind (("<f5>" . my-reopen-file))
  :init
  (defun my-reopen-file ()
    "Reopen current file."
    (interactive)
    (let ((filename (buffer-file-name)))
      (kill-buffer)
      (find-file filename))))

(use-package pulsar
  :commands pulsar-global-mode
  :init
  (pulsar-global-mode))

(when (and (boundp pixel-scroll-precision-mode)
	   ;; (not (eq system-type 'darwin))
	   )
  (pixel-scroll-precision-mode +1))

(use-package narrow
  :bind (("C-x n n" . my-narrow-dwim))
  :init
  (defun my-narrow-dwim ()
    "Toggle narrowing."
    (interactive)
    (cond ((region-active-p)
           ;; If region is highlighted, narrow to that
           (call-interactively #'narrow-to-region)
           (deactivate-mark t))
          ((buffer-narrowed-p)
           ;; Otherwise widen if narrowed
           (widen))
          (t
           (call-interactively #'narrow-to-defun)))))

(use-package restclient
  :mode ((".rest$" . restclient-mode)
	 (".http$" . restclient-mode)))

(bind-key* (kbd "C-o") 'other-window)

(use-package ellama
  :demand t
  :functions (make-llm-ollama)
  :init
  (setopt ellama-language "Russian")
  (require 'llm-ollama)
  (setopt ellama-fill-paragraphs nil)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model "mistral:7b-instruct-v0.2-q6_K" :embedding-model "mistral:7b-instruct-v0.2-q6_K")))

(use-package org-mode
  :init
  (setopt org-startup-truncated nil)
  (add-hook 'org-mode-hook 'toggle-word-wrap))

(use-package tabby
  :bind (("C-'" . tabby-complete))
  :commands (tabby-accept-completion)
  :defines (taby-mode-map tabby--ongoing-request-id)
  :init
  (my-vc-install :name "tabby" :host "github" :repo "alan-w-255/tabby.el")
  (add-hook 'go-ts-mode-hook 'tabby-mode)
  (add-hook 'tuareg-mode-hook 'tabby-mode)
  :config
  (bind-key (kbd "M-<RET>") #'tabby-accept-completion tabby-mode-map))

(setopt elisp-flymake-byte-compile-load-path load-path)

(use-package dape
  :commands (go-func-name-at-point dape-read-pid)
  :preface
  (hercules-def
   :keymap 'dape-global-map
   :show-funs '(dape dape-step-in dape-step-out dape-next)
   :hide-funs '(dape-disconnect-quit dape-quit dape-info)
   :transient t)
  :config
  ;; Use n for next etc. in REPL
  (setq dape-repl-use-shorthand t)

  ;; (setq treesit-go-tests-query
  ;; 	(treesit-query-compile
  ;; 	 'go
  ;;        '((function_declaration
  ;;           name: (identifier) @testname
  ;;           parameters: (parameter_list
  ;; 			 :anchor (parameter_declaration type: (pointer_type) @type :anchor))
  ;;           (:match "*testing.\\(T\\|M\\)" @type) (:match "^Test.+$" @testname)) @parent)))
  ;; (defun my-query-go-test-nodes ()
  ;;   (when (treesit-ready-p 'go)
  ;;     (treesit-query-capture (treesit-buffer-root-node) treesit-go-tests-query)))

  ;; (defun my-completing-read-go-tests ()
  ;;   (let* ((test-matches (my-query-go-test-nodes))
  ;;          (test-name-matches
  ;; 	    (cl-remove-if-not (lambda (match) (eq (car match) 'testname)) test-matches))
  ;;          (test-names
  ;; 	    (mapcar (lambda (match) (treesit-node-text (cdr match))) test-name-matches)))
  ;;     (completing-read "Test:" test-names nil t)))


  ;; (defun my-dape--select-go-test-args ()
  ;;   (when-let* ((test-name (my-completing-read-go-tests))
  ;;               (test-regexp (concat "^" test-name "$")))
  ;;     (if test-name
  ;;         `["-test.run" ,test-regexp]
  ;;       (error "No test selected"))))

  ;; (defun my-file-relative-dir ()
  ;;   "Return the file directory relative to dape's cwd. This is used by Delve debugger."
  ;;   (concat "./" (file-relative-name default-directory (funcall dape-cwd-fn))))

  ;; ;; inside your dape-config
  ;; (add-to-list 'dape-configs
  ;;              `(delve-test
  ;;                modes (go-mode go-ts-mode)
  ;;                command "dlv"
  ;;                command-cwd dape-cwd-fn
  ;;                command-args ("dap" "--listen" "127.0.0.1:55878")
  ;;                host "127.0.0.1"
  ;;                port 55878
  ;;                :type "go"
  ;;                :name "debug test"
  ;;                :request "launch"
  ;;                :mode "test"
  ;;                :cwd dape-cwd-fn
  ;;                :program my-file-relative-dir
  ;;                :args my-dape--select-go-test-args))

  (defun go-func-name-at-point ()
    (interactive)
    (save-excursion
      (end-of-line)
      (beginning-of-defun)
      (when (re-search-forward "^func[[:space:]]+\\([[:alnum:]_]+\\)" nil t)
        (match-string 1))))

  (defun my-dape--select-go-args ()
    (if (string-suffix-p "_test.go"   (buffer-name))
        (when-let* ((test-name (go-func-name-at-point))
                    (test-regexp (concat "^" test-name "$")))
          (if test-name
              `["-test.run" ,test-regexp]
            (error "No test selected")))
      (if  current-prefix-arg
          (vconcat (split-string (read-shell-command "args: " nil
                                                     (if (equal (car compile-history) "")
                                                         '(compile-history . 1)
                                                       'compile-history))))
        [])))

  ;; https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
  (defun my-dape-test-p ()
    (if (string-suffix-p "_test.go"   (buffer-name))
        "test" "debug"))

  (defun my-dape-relative-dir ()
    "Return the file directory relative to dape's cwd.
This is used by Delve debugger."
    (if (string-suffix-p "_test.go"   (buffer-name))
        (concat "./" (file-relative-name
                      default-directory (funcall dape-cwd-fn)))
      (funcall dape-cwd-fn)))

  (defvar my-dape-debug-pid nil)

  (defun my-dlv-attach-args ()
    (list "dap" "--listen" "127.0.0.1:55878" "attach" (format "%d" (setq my-dape-debug-pid (dape-read-pid)))))

  (defun my-dlv-remote-args ()
    (list "dap" "--listen" "127.0.0.1:55878" "connect"
	  (format "%s:%s"
		  (read-string "host: ")
		  (read-string "port: "))))

  ;; inside your dape-config
  (add-to-list 'dape-configs
               `(delve
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-cwd dape-cwd-fn
                 command-args ("dap" "--listen" "127.0.0.1:55878")
                 host "127.0.0.1"
                 port 55878
                 :type "go"
                 :name "go-debug"
                 :request "launch"
                 :mode my-dape-test-p
                 :cwd dape-cwd-fn
                 :program my-dape-relative-dir
                 :args my-dape--select-go-args))

  (add-to-list 'dape-configs
               `(delve-attach
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-cwd dape-cwd-fn
                 command-args my-dlv-attach-args
		 host "127.0.0.1"
		 port 55878
		 :type "go"
		 :name "go-debug"
		 :request "attach"
		 :mode "local"
		 :processId my-dape-debug-pid
		 :cwd dape-cwd-fn))

  (add-to-list 'dape-configs
               `(delve-remote
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-cwd dape-cwd-fn
                 command-args my-dlv-remote-args
		 host "127.0.0.1"
		 port 55878
		 :type "go"
		 :name "go-debug"
		 :request "attach"
		 :mode "remote"
		 :cwd dape-cwd-fn)))

(use-package spacious-padding
  :commands spacious-padding-mode
  :init
  (spacious-padding-mode t))

(use-package breadcrumb
  :commands breadcrumb-mode
  :demand t
  :init
  (breadcrumb-mode))

(use-package conda
  :disabled t
  :commands (conda-env-autoactivate-mode conda-env-activate-for-buffer)
  :demand t
  :preface
  (setopt conda-anaconda-home "/opt/homebrew/anaconda3")
  :config
  (conda-env-autoactivate-mode t)
  (add-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
                                         (conda-env-activate-for-buffer)))))

(use-package python-mode
  :defines (python-mode-map python-ts-mode-map)
  :preface
  (add-hook 'python-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  (with-eval-after-load "python"
    (bind-key
     (kbd "C-c C-c")
     #'my-make python-ts-mode-map)
    (bind-key
     (kbd "C-c C-c")
     #'my-make python-mode-map)))

(use-package envrc
  :commands envrc-global-mode
  :demand t
  :init
  (envrc-global-mode))

(setopt warning-suppress-types '((undo discard-info)))

(provide 'init)
;;; init.el ends here
