;; -*- lexical-binding: t -*-
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-font-lock-mode 1)
(show-paren-mode 1)

(electric-pair-mode 1)
(electric-indent-mode t)

(setq mouse-wheel-progressive-speed nil)

(require 'icomplete)
(fido-mode -1)
(icomplete-mode 1)

(setq-default icomplete-delay-completions-threshold 0)
(setq-default icomplete-max-delay-chars 0)
(setq-default icomplete-compute-delay 0)
(setq-default icomplete-with-completion-tables t)
(setq-default icomplete-in-buffer t)
(setq-default icomplete--in-region-buffer t)
(setq-default completion-styles '(basic partial-completion emacs22 initials flex))
(setq-default completion-category-overrides '((file (styles basic substring))))
(setq-default read-file-name-completion-ignore-case t)
(setq-default read-buffer-completion-ignore-case t)
(setq-default completion-ignore-case t)
(setq-default icomplete-hide-common-prefix nil)
(setq-default icomplete-show-matches-on-no-input t)
(setq-default resize-mini-windows 'grow-only)
(setq-default icomplete-prospects-height 10)
(icomplete-vertical-mode +1)

(define-key icomplete-minibuffer-map (kbd "<down>")  #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-n")  #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<up>")  #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p")  #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<backspace>")  #'icomplete-fido-backward-updir)
(define-key icomplete-minibuffer-map (kbd "C-j")  #'icomplete-force-complete)
(define-key icomplete-minibuffer-map (kbd "C-M-j") #'exit-minibuffer)
(define-key icomplete-minibuffer-map (kbd "<RET>")  #'icomplete-force-complete-and-exit)

(defun my-icomplete-yank-kill-ring ()
  "Insert the selected `kill-ring' item directly at point."
  (interactive)
  (let ((icomplete-separator
         (concat "\n" (propertize "..................." 'face 'shadow) "\n ")))
    (insert
     (completing-read "paste from kill ring: " kill-ring nil t))))

(global-set-key (kbd "M-y") 'my-icomplete-yank-kill-ring)

(global-set-key (kbd "C-;") #'hippie-expand)
(global-set-key (kbd "M-i") #'imenu)
(global-set-key "\C-cff" #'toggle-frame-fullscreen)
(fset 'yes-or-no-p 'y-or-n-p)

(flymake-mode 1)
(global-set-key (kbd "C-x `") #'flymake-goto-next-error)
(global-set-key (kbd "C-c r") #'flymake-show-diagnostics-buffer)

(global-set-key (kbd "M-p") #'other-window)

(setq my-light-theme
      ;; 'leuven
      ;; 'adwaita
      ;; 'dichromacy
      ;; 'tsdh-light
      'modus-operandi
      my-dark-theme 'misterioso
      ;; 'tsdh-dark
      )

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
  (let ((cur-theme (if (equal (car custom-enabled-themes)
                              my-light-theme)
                       'light
                     'dark)))
    (mapc #'disable-theme custom-enabled-themes)
    (if (equal cur-theme 'light)
        (load-theme my-dark-theme t)
      (load-theme my-light-theme t))))

(progn ; my-gnome-night-light-light
  (require 'dbus)
  (defun my-gnome-night-light-internal-prop-change-listener (_name changed-props _)
    (let* ((prop (car changed-props))
	   (name (car prop))
	   (value (car (cadr prop))))
      (when (string-equal name "NightLightActive")
	(when (functionp my-gnome-night-light-light-change-callback)
	  (funcall my-gnome-night-light-light-change-callback value)))))

  (defun my-gnome-night-light ()
    "Load and enable my-gnome-night-light."
    (dbus-register-signal
     :session
     "org.gnome.SettingsDaemon.Color"
     "/org/gnome/SettingsDaemon/Color"
     "org.freedesktop.DBus.Properties"
     "PropertiesChanged"
     #'my-gnome-night-light-internal-prop-change-listener)
    (let ((value (dbus-get-property
		  :session
		  "org.gnome.SettingsDaemon.Color"
		  "/org/gnome/SettingsDaemon/Color"
		  "org.gnome.SettingsDaemon.Color"
		  "NightLightActive")))
      (when (functionp my-gnome-night-light-light-change-callback)
	(funcall my-gnome-night-light-light-change-callback value))))

  (defun my-theme-changer (state)
    "My callback for gnome-night-light.\nChanges theme according to STATE."
    (mapc #'disable-theme custom-enabled-themes)
    (if state
	(load-theme my-dark-theme t nil)
      (load-theme my-light-theme t nil)))

  (setq my-gnome-night-light-light-change-callback 'my-theme-changer)
  (defvar my-gnome-night-light-light-change-callback nil
    "The callback function called on Night Light state change.
It takes one parameter, which is t when the Night Light is active
(e.g.  it's night) and nil when it's day.")
  (my-gnome-night-light))

(global-set-key (kbd "<f6>") #'my-toggle-themes)


;; from helm-make
(defun my--make-target-list (makefile)
  "Return the target list for MAKEFILE by parsing it."
  (let (targets)
    (with-temp-buffer
      (insert-file-contents makefile)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^: \n]+\\) *:\\(?: \\|$\\)" nil t)
        (let ((str (match-string 1)))
          (unless (string-match "^\\." str)
            (push str targets)))))
    (nreverse targets)))


(defun my-make (arg)
  "Make current project with targets selection.
Use project root as default directory if universal ARG is not set.
Select it interactively otherwise."
  (interactive "p")
  (let* ((project (project-current))
         (default-directory (if (= arg 4)
                                (read-directory-name "select directory ")
                              (if project (project-root project)
                                default-directory)))
         (makefile (expand-file-name "Makefile" default-directory))
         (targets (my--make-target-list makefile))
         (target (completing-read "make " targets)))
    (compile (format "make %s" target))))

(defun my-isearch-next (arg)
  "Isearch symbol at point or next isearch history item."
  (interactive "p")
  (if (string= isearch-string "")
      (isearch-yank-string (format "%s" (or (symbol-at-point) "")))
    (next-history-element arg)))

(define-key isearch-mode-map (kbd "M-n") #'my-isearch-next)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(gomod "https://github.com/syntacti/tree-sitter-go-mod")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(lua "https://github.com/Azganoth/tree-sitter-lua")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(r "https://github.com/r-lib/tree-sitter-r")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
	(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")
	(haskell "https://github.com/tree-sitter/tree-sitter-haskell")))

(defun my-install-language-grammar (lang)
  (when (not (file-exists-p
	       (expand-file-name
		(format "tree-sitter/libtree-sitter-%s.so" lang)
		user-emacs-directory)))
    (treesit-install-language-grammar lang)))

(mapc 'my-install-language-grammar
      '(go gomod elisp c cpp js python rust markdown typescript tsx yaml make
	   json csharp css cmake html bash haskell))

(add-hook 'go-ts-mode-hook 'eglot-ensure)
(with-eval-after-load 'go-ts-mode
  (add-to-list 'load-path (expand-file-name "~/elisp/go-gen-test"))
  (require 'go-gen-test)
  (progn
    (define-key go-ts-mode-map (kbd "C-c C-c") 'my-make)
    (define-key go-ts-mode-map (kbd "C-c C-t") (lambda ()
						 (interactive)
						 (compile "go test .")))
    (define-key go-ts-mode-map (kbd "C-c g") 'go-gen-test-dwim)))
(require 'go-ts-mode)

(defun my-go-playground ()
  "Create go snippet for play around."
  (interactive)
  (let* ((dir (expand-file-name
	       (concat "at-" (format-time-string "%Y-%m-%d-%H%M%S")) "~/go/src/playground/"))
	 (file (expand-file-name "snippet.go" dir)))
    (dired-create-directory dir)
    (dired-copy-file (expand-file-name "~/.emacs.d/snippet.go") file t)
    (find-file file)
    (local-set-key (kbd "C-<return>") (lambda () (interactive)
					(compile "go run snippet.go")))))

(defun my-go-playground-rm ()
  "Remove current go snippet."
  (interactive)
  (let ((filename (concat
		   (file-name-base (or (buffer-file-name) "")) "."
		   (file-name-extension (or (buffer-file-name) "")))))
    (when (and
	   (not (string= default-directory
			 (expand-file-name user-emacs-directory)))
	   (string= filename
		    "snippet.go"))
      (delete-directory default-directory t nil)
      (kill-buffer))))

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
      (autoload 'dune-mode "dune" nil t)
      (add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\)?\\'" . dune-mode))
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
		(eglot-ensure)))
  :config
  (unbind-key "TAB" gopcaml-mode-map)
  (define-key gopcaml-mode-map (kbd "C-c C-j") 'gopcaml-move-to-hole))

(setq tab-always-indent 'complete)
(setq completion-auto-help 'visible)
(setq completion-auto-select 'second-tab)
(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 20)
(define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)

(defun my-minibuffer-choose-completion (&optional no-exit no-quit)
  (interactive "P")
  (with-minibuffer-completions-window
    (let ((completion-use-base-affixes nil))
      (choose-completion nil no-exit no-quit))))

(define-key completion-in-region-mode-map (kbd "M-RET") 'my-minibuffer-choose-completion)
(define-key completion-in-region-mode-map (kbd "C-<return>") 'my-minibuffer-choose-completion)
(define-key completion-in-region-mode-map (kbd "RET") 'my-minibuffer-choose-completion)

(global-set-key (kbd "C-c C-s") 'rgrep)

(when (boundp pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode +1))

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

	       ;; flymake errors
	       '(:eval (when flymake--state flymake-mode-line-format))

	       ;; relative position, size of file
	       "    ["
	       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
	       "/"
	       (propertize "%I" 'face 'font-lock-constant-face) ;; size
	       "] "))

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

(add-hook 'after-revert-hook 'my-update-vc-mode)
(add-hook 'after-find-file 'my-update-vc-mode)

(setq-default eglot-confirm-server-initiated-edits nil)
(eval-after-load 'eglot
  (lambda nil
    (defun my-eglot-organize-imports () (interactive)
	   (eglot-code-actions nil nil "source.organizeImports" t))
    (defun my-eglot-setup ()
      (interactive)
      (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
      (add-hook 'before-save-hook 'eglot-format-buffer nil t))
    (add-hook 'eglot-managed-mode-hook 'my-eglot-setup)
    (define-key eglot-mode-map (kbd "C-x l h h") 'eldoc)
    (define-key eglot-mode-map (kbd "C-x l w q") 'eglot-shutdown)
    (define-key eglot-mode-map (kbd "C-x l w r") 'eglot-reconnect)
    (define-key eglot-mode-map (kbd "C-x l r r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-x l r o") 'eglot-code-action-organize-imports)
    (define-key eglot-mode-map (kbd "C-x l a a") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-x l g i") 'eglot-find-implementation)))

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
		"stack.yaml"                                            ; Haskell
		"Cargo.toml"                                            ; Rust
		"info.rkt"                                              ; Racket
		"pubspec.yaml"                                          ; Dart
		"dune-project"                                          ; OCaml
		"go.mod"                                                ; Go
		"*.cproj"                                               ; c#
		"*.fsproj"                                              ; f#
		".project"
		))

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
  :config
  (save-place-mode 1))

(use-package recentf
  :preface
  (recentf-mode 1)
  (setq recentf-max-menu-items 300)
  (setq recentf-max-saved-items 300))

(bind-key* (kbd "C-o") 'other-window)
(bind-key* (kbd "C-x C-p") 'recentf)

(setq make-backup-files nil
      text-mode-hook 'turn-on-auto-fill)

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

(progn ; my-screenshots
  (defun my-screenshot-svg ()
    "Save a screenshot of the current frame as an SVG image.
Saves to a temp file."
    (interactive)
    (require 'dired)
    (let* ((filename (make-temp-file "Emacs" nil ".svg"))
	   (data (x-export-frames nil 'svg)))
      (with-temp-file filename
        (insert data))
      (dired-rename-file filename (expand-file-name (file-name-nondirectory filename)
						    (expand-file-name "~/Pictures")) 1)))
  (defun my-screenshot-png ()
    "Save a screenshot of the current frame as an PNG image.
Saves to a temp file."
    (interactive)
    (require 'dired)
    (let* ((filename (make-temp-file "Emacs" nil ".png"))
	   (data (x-export-frames nil 'png)))
      (with-temp-file filename
        (insert data))
      (dired-rename-file filename
			 (expand-file-name (file-name-nondirectory filename)
					   (expand-file-name "~/Pictures"))
			 1))))

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

(setq ring-bell-function 'ignore)

(my-set-themes)
