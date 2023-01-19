;; -*- lexical-binding: t -*-
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(load-theme 'misterioso)
(global-font-lock-mode 1)
(show-paren-mode 1)

(electric-pair-mode 1)

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

(setq my-light-theme 'dichromacy
      ;; 'adwaita
      ;; 'tsdh-light
      my-dark-theme 'misterioso
      ;; 'tsdh-dark
      )

(defun my-set-themes ()
  "Function for setting themes after init."
  (interactive)
  (let ((cur-hour (nth 2 (decode-time))))
    (mapc #'disable-theme custom-enabled-themes)
    (if (and (>  cur-hour 7)
             (<  cur-hour 20))
        (load-theme my-light-theme t)
      (load-theme my-dark-theme t))))

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

(setq go-ts-mode-indent-offset 8)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(with-eval-after-load 'go-ts-mode
  (progn
    (define-key go-ts-mode-map (kbd "C-c C-c") 'my-make)
    (define-key go-ts-mode-map (kbd "C-c C-t") (lambda ()
						 (interactive)
						 (compile "go test .")))))


(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 20)
(setq completion-auto-select nil)
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
    (defun my-eglot-organize-imports () (call-interactively 'eglot-code-action-organize-imports))
    (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
    (define-key eglot-mode-map (kbd "C-x l h h") 'eldoc)
    (define-key eglot-mode-map (kbd "C-x l w s") 'eglot-shutdown)
    (define-key eglot-mode-map (kbd "C-x l w r") 'eglot-reconnect)
    (define-key eglot-mode-map (kbd "C-x l r r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-x l r o") 'eglot-code-action-organize-imports)
    (define-key eglot-mode-map (kbd "C-x l a a") 'eglot-code-actions)))
(add-hook 'before-save-hook 'eglot-format-buffer)

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
		))

(my-set-themes)
