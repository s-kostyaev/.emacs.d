;;; init.el --- Emacs init file. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar gnutls-trustfiles)
(if (eq system-type 'darwin)
    (with-eval-after-load 'gnutls
      (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem")))

(eval-and-compile
  (defvar network-security-level)
  (defvar gnutls-verify-error)
  (if (> emacs-major-version 24)
      (progn
        (setq network-security-level 'high)
        (setq gnutls-verify-error t))))

(defvar bootstrap-version)
(defvar straight-check-for-modifications)
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(package-initialize)

(leaf benchmark-init
  :disabled t
  :hook ((after-init-hook . benchmark-init/deactivate))
  :require benchmark-init)

(global-set-key (kbd "C-M-r") (lambda () (interactive)
                                (byte-recompile-file "~/.emacs.d/init.el" t 0)
				(native-compile "~/.emacs.d/init.el")
				(package-quickstart-refresh)
				(byte-recompile-file "~/.emacs.d/package-quickstart.el" t 0)
				(native-compile "~/.emacs.d/package-quickstart.el")))

(setq custom-file "~/.emacs.d/emacs-customizations.el")

(leaf-keywords-init)

(leaf my-themes
  :preface
  (setq calendar-location-name "Novosibirsk, Russia")
  (setq calendar-latitude 55.05)
  (setq calendar-longitude 82.93)

  (defun my-switch-themes (sun-event &optional first-run)
    "Switch themes on sunrise and sunset."
    (if first-run
	(cond ((memq sun-event '(sunrise midday))
	       (mapc #'disable-theme custom-enabled-themes)
	       (load-theme my-light-theme t))
	      ((memq sun-event '(sunset midnight))
	       (mapc #'disable-theme custom-enabled-themes)
	       (load-theme my-dark-theme t)))
      (cond ((eq sun-event 'sunrise)
	     (mapc #'disable-theme custom-enabled-themes)
	     (load-theme my-light-theme t))
	    ((eq sun-event 'sunset)
	     (mapc #'disable-theme custom-enabled-themes)
	     (load-theme my-dark-theme t)))))

  ;; sign this function to be invoked on sun events
  (add-hook 'rase-functions 'my-switch-themes)

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
    (rase-start t))

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
				      (f-ancestor-of\?
				       (getenv "HOME")
				       (locate-file
					(concat theme "-theme.el")
					(custom-theme--load-path)
					'("" "c"))))
				    (mapcar #'symbol-name
					    (custom-available-themes)))))))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t nil)))

  (defun my-load-custom-file ()
    "Load my custom file."
    (load-file custom-file))

  (add-to-list 'after-make-frame-functions #'my-reload-theme--frame)
  (add-hook 'server-after-make-frame-hook #'my-reload-theme)
  (add-hook 'after-init-hook #'my-reload-theme)
  (with-eval-after-load 'emacs-customizations #'my-set-themes)

  :pre-setq ((my-light-theme quote solarized-light)
	     (my-dark-theme quote solarized-dark)
	     (my-need-theme-reload))

  :preface
  (setq my-font (font-spec :size 14.0 :family "PT Mono"))
  :bind
  (("<f6>" . my-toggle-themes))
  :hook
  ((after-init-hook . my-set-themes)
   (desktop-after-read-hook . my-set-themes)
   (after-init-hook . my-load-custom-file)))

(leaf my-gnome-night-light-light
  :preface
  (defun my-gnome-night-light-internal-prop-change-listener (_name changed-props _)
    (let* ((prop (car changed-props))
	   (name (car prop))
	   (value (car (cadr prop))))
      (when (string-equal name "NightLightActive")
	(when (functionp my-gnome-night-light-light-change-callback)
	  (funcall my-gnome-night-light-light-change-callback value)))))

  (defun my-gnome-night-light ()
    "Load and enable my-gnome-night-light."
    (dbus-register-signal :session "org.gnome.SettingsDaemon.Color" "/org/gnome/SettingsDaemon/Color" "org.freedesktop.DBus.Properties" "PropertiesChanged" #'my-gnome-night-light-internal-prop-change-listener)
    (let ((value (dbus-get-property :session "org.gnome.SettingsDaemon.Color" "/org/gnome/SettingsDaemon/Color" "org.gnome.SettingsDaemon.Color" "NightLightActive")))
      (when (functionp my-gnome-night-light-light-change-callback)
	(funcall my-gnome-night-light-light-change-callback value))))

  (defun my-theme-changer (state)
    "My callback for gnome-night-light.\nChanges theme according to STATE."
    (mapc #'disable-theme custom-enabled-themes)
    (if state
	(load-theme my-dark-theme t nil)
      (load-theme my-light-theme t nil)))

  :require dbus
  :setq ((my-gnome-night-light-light-change-callback function my-theme-changer))
  :config
  (defvar my-gnome-night-light-light-change-callback nil "The callback function called on Night Light state change.\nIt takes one parameter, which is t when the Night Light is active\n(e.g.  it's night) and nil when it's day.")
  (my-gnome-night-light))

(leaf my-mac-themes
  :preface
  (defun my-mac-apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme my-light-theme t))
      ('dark (load-theme my-dark-theme t))))

  (if (eq (window-system) 'ns)
      (add-hook 'ns-system-appearance-change-functions #'my-mac-apply-theme)))

(leaf key-chord
  :bind (([f9] . key-chord-mode))
  :config
  (run-with-idle-timer 2 nil #'require 'key-chord nil t)
  (with-eval-after-load 'key-chord
    (key-chord-mode 1)))

(leaf flymake
  :preface
  (defun my-flymake--mode-line-format ()
    "Produce a pretty minor mode indicator."
    (if flymake--backend-state
	(let* ((known (hash-table-keys flymake--backend-state))
	       (running (flymake-running-backends))
	       (disabled (flymake-disabled-backends))
	       (diags-by-type (make-hash-table))
	       (all-disabled (and disabled
				  (null running))))
	  (maphash
	   (lambda (_b state)
	     (mapc
	      (lambda (diag)
		(push diag
		      (gethash
		       (flymake--diag-type diag)
		       diags-by-type)))
	      (flymake--backend-state-diags state)))
	   flymake--backend-state)
	  `(,@(unless (or all-disabled
			  (null known))
		(cl-loop with types =
			 (hash-table-keys diags-by-type)
			 with _augmented =
			 (cl-loop for extra in
				  '(:error :warning)
				  do
				  (cl-pushnew extra types :key #'flymake--severity))
			 for type in
			 (cl-sort types #'> :key #'flymake--severity)
			 for diags =
			 (gethash type diags-by-type)
			 for face =
			 (flymake--lookup-type-property type 'mode-line-face 'compilation-error)
			 when (or diags
				  (cond
				   ((eq flymake-suppress-zero-counters t)
				    nil)
				   (flymake-suppress-zero-counters
				    (>=
				     (flymake--severity type)
				     (warning-numeric-level flymake-suppress-zero-counters)))
				   (t t)))
			 collect
			 `(:propertize ,(format "%d"
						(length diags))
				       face ,face mouse-face mode-line-highlight keymap ,(let ((map (make-sparse-keymap))
											       (type type))
											   (define-key map
											     (vector 'mode-line mouse-wheel-down-event)
											     (lambda (event)
											       (interactive "e")
											       (with-selected-window (posn-window
														      (event-start event))
												 (flymake-goto-prev-error 1
															  (list type)
															  t))))
											   (define-key map
											     (vector 'mode-line mouse-wheel-up-event)
											     (lambda (event)
											       (interactive "e")
											       (with-selected-window (posn-window
														      (event-start event))
												 (flymake-goto-next-error 1
															  (list type)
															  t))))
											   map)
				       help-echo ,(concat
						   (format "%s diagnostics of type %s\n"
							   (propertize
							    (format "%d"
								    (length diags))
							    'face face)
							   (propertize
							    (format "%s" type)
							    'face face))
						   (format "%s/%s: previous/next of this type" mouse-wheel-down-event mouse-wheel-up-event)))
			 into forms finally return
			 `((:propertize "[")
			   ,@(cl-loop for
				      (a . rest)
				      on forms by #'cdr collect a when rest collect
				      '(:propertize " "))
			   (:propertize "]"))))))
      nil))

  :hook ((prog-mode-hook . flymake-mode)
	 (emacs-lisp-mode-hook . flymake-mode))
  :init
  (leaf flymake
    :bind (("C-x `" . flymake-goto-next-error)
	   ;; ("C-c r" . flymake-show-diagnostics-buffer)
	   ("C-c r" . consult-flymake)))

  :require flymake)

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
               (propertize "%02l" 'face 'font-lock-type-face) ":"
               (propertize "%02c" 'face 'font-lock-type-face)
               ;; ") "

               "        "
               '(:eval (when (stringp vc-mode)
                         vc-mode))

               ;; the current major mode for the buffer.
               "        ["

               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               "] "

               ;; Flycheck errors

               ;; '(:eval (flycheck-mode-line-status-text))

               ;; flymake errors
               '(:eval (my-flymake--mode-line-format))

               ;; relative position, size of file
               "    ["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ))

(leaf vc-mode
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

(leaf text-defaults
  :bind (("M-J" . scroll-up-line)
	 ("M-K" . scroll-down-line))
  :hook ((after-init-hook . global-font-lock-mode))
  :pre-setq ((inhibit-startup-message . t)
	     (frame-title-format . "emacs - %b")
	     (auto-window-vscroll)
	     (require-final-newline . t)
	     (next-line-add-newlines))
  :preface
  (mouse-wheel-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'downcase-region 'disabled nil)
  (delete-selection-mode)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (setq sentence-end-double-space nil))

(leaf my-align-region
  :defvar delimit-columns-extra delimit-columns-format delimit-columns-str-separator delimit-columns-separator
  :preface
  (defun my-align-region-by (&optional delimiter)
    "Align current region by DELIMITER."
    (interactive)
    (let* ((delim (or delimiter
		      (read-string "delimiter: ")))
	   (delimit-columns-separator delim)
	   (delimit-columns-str-separator delim)
	   (delimit-columns-format 'separator)
	   (delimit-columns-extra nil)
	   (beg (region-beginning)))
      (delimit-columns-region beg
			      (region-end))
      (goto-char beg)
      (ignore-errors
	(er/expand-region 1))
      (let ((new-end (region-end)))
	(goto-char new-end)
	(whitespace-cleanup-region beg
				   (line-end-position)))))

  :bind (("C-c a" . my-align-region-by)))

(leaf hydra
  :preface
  (defun toggle-window-split ()
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

  :bind (("C-x t" . toggle-window-split)
	 ("C-x o" . hydra-cycle-windows/body))
  :config
  (defhydra hydra-cycle-windows
    (:body-pre
     (other-window 1))
    "Windows"
    ("o"
     (other-window 1)
     "Next")
    ("O"
     (other-window -1)
     "Previous")
    ("t" toggle-window-split "Toggle split")
    ("]" enlarge-window-horizontally "Enlarge horizontal")
    ("[" shrink-window-horizontally "Shrink horizontal")
    ("=" enlarge-window "Enlarge vertival")
    ("-" shrink-window "Shrink vertical")
    ("b" balance-windows "Balance windows")
    ("m" delete-other-windows "Maximize window")
    ("n" split-window-below "New window")
    ("c" delete-window "Close window")
    ("q" nil "quit")))

(leaf eglot
  :defvar eglot-workspace-configuration eglot-strict-mode
  :setq ((eglot-strict-mode))
  :setq-default ((eglot-workspace-configuration quote
						((:gopls :usePlaceholders t :staticcheck t :completeUnimported t))))
  :config
  (eval-after-load 'eglot
    (lambda nil
      (define-key eglot-mode-map (kbd "C-x l h h") 'eldoc)
      (define-key eglot-mode-map (kbd "C-x l w s") 'eglot-shutdown)
      (define-key eglot-mode-map (kbd "C-x l w r") 'eglot-reconnect)
      (define-key eglot-mode-map (kbd "C-x l r r") 'eglot-rename)
      (define-key eglot-mode-map (kbd "C-x l r o") 'eglot-code-action-organize-imports)
      (define-key eglot-mode-map (kbd "C-x l a a") 'eglot-code-actions))))

(leaf exec-path
  :preface
  (setq exec-path (append exec-path
			  '("~/go/bin" "/opt/local/bin" "/usr/local/bin" "~/.cargo/bin" "/usr/local/opt/llvm/bin" "~/.local/bin")))
  (require 's)
  (setenv "PATH"
	  (s-join ":" exec-path)))

(leaf go
  :preface
  (defun my-try-go-mod (dir)
    "Find go project root for DIR."
    (if (and dir
	     (not (f-descendant-of-p dir
				     (or
				      (getenv "GOPATH")
				      (concat
				       (getenv "HOME")
				       "/go")))))
	(let ((result (locate-dominating-file dir "go.mod")))
	  (if result
	      (cons 'transient
		    (expand-file-name result))
	    (cons 'transient dir)))
      (when dir
	(cons 'transient dir))))
  (defun my-extract-go-module-name ()
    (let* ((go-mod-file (f-join (project-root (project-current)) "go.mod"))
	   (name
	    (with-temp-buffer
	      (find-file-noselect-1 (current-buffer) go-mod-file t t go-mod-file 2)
	      (string-remove-prefix "module "
				    (buffer-substring-no-properties (point-min)
								    (progn
								      (goto-char (point-min))
								      (end-of-line)
								      (point)))))))
      name))
  :require s
  :config
  (require 'ht)
  (defvar my-go-packages-by-project (ht-create))
  (leaf go-mode
    :defvar company-backends go-tag-args
    :mode ("\\.go\\'"
	   "\\.go2\\'"
	   ("go.mod$" . go-dot-mod-mode))
    :config
    (with-eval-after-load 'go-mode
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
		      (s-trim
		       (shell-command-to-string "find /usr/local/Cellar/go -type 'd' -name 'libexec'"))))
	  (require 'go-impl)
	  (require 'gotest)
	  (require 'go-playground)
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

	  (setq go-tag-args (list "-transform" "snakecase"))
	  (add-hook 'before-save-hook #'gofmt-before-save)
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
	   (kbd "C-c C-i")
	   #'go-fill-struct)
	  (local-set-key
	   (kbd "M-?")
	   #'lsp-find-references)
	  (local-set-key
	   (kbd "C-c C-c")
	   #'my-make)
	  (require 'lsp-mode)
	  (lsp-register-custom-settings
	   '(("gopls.completeUnimported" t)))
	  (lsp-register-custom-settings
	   '(("gopls.staticcheck" t)))
	  (setq-local project-find-functions
		      (list #'my-try-go-mod #'project-try-vc))
	  (setq-local flymake-start-on-save-buffer nil)
	  (setq-local flymake-no-changes-timeout nil)
	  (setq-local lsp-go-goimports-local (my-extract-go-module-name))
	  (require 'lsp-go)
	  (symbol-overlay-mode -1)
	  (company-prescient-mode -1)
	  (setq-local lsp-completion-filter-on-incomplete nil)
	  (lsp-deferred)
	  ;; (eglot-ensure)
	  (defun my-go-packages-go-list ()
	    (ht-get my-go-packages-by-project (project-current)))

	  (setq go-packages-function 'my-go-packages-go-list)
	  (require 'cl-lib)
	  (defun my-refresh-go-packages-list (&optional force)
	    "Refresh go packages list.
If `force' refresh even if package list already exists."
	    (if (string-equal "go-mode" major-mode)
		(let ((cur-buf (buffer-name))
		      (cur-project (project-current)))
		  (if (not
		       (f-descendant-of-p (project-root cur-project)
					  (or
					   (getenv "GOPATH")
					   (concat
					    (getenv "HOME")
					    "/go/pkg/mod"))))
		      (if (or (not (cl-find cur-project (ht-keys my-go-packages-by-project)))
			      force)
			  (progn
			    (if (not (cl-find cur-project (ht-keys my-go-packages-by-project)))
				(ht-set my-go-packages-by-project cur-project '("in progress")))
			    (async-start
			     (lambda nil
			       (process-lines "go" "list" "-e" "all"))
			     (lambda (res)
			       (with-current-buffer cur-buf
				 (ht-set my-go-packages-by-project cur-project (cl-mapcar
										(lambda (s)
										  (replace-regexp-in-string "@[^/]*" ""
													    (string-remove-prefix "mod/"
																  (string-remove-prefix "vendor/" s))))
										(cl-remove-if
										 (lambda (s)
										   (string-prefix-p "warning:" s))
										 res))))))))))))

	  (my-refresh-go-packages-list)
	  (add-hook 'after-save-hook #'my-refresh-go-packages-list t))

	(add-hook 'go-mode-hook #'my-go-mode-hook)))))

(leaf default-keybindings
  :bind (("s" . save-buffer)
	 ("v" . quoted-insert)
	 ("f" . search-forward)
	 ("c" . compile)
	 ("0" . overwrite-mode)
	 ("" . previous-line)
	 ("" . forward-word)
	 ("" . not-modified)))

(leaf default-modes
  :setq ((make-backup-files quote nil)
	 (text-mode-hook quote turn-on-auto-fill))
  :config
  (setq auto-mode-alist (cons
			 '("\\.cxx$" . c++-mode)
			 auto-mode-alist))
  (setq auto-mode-alist (cons
			 '("\\.hpp$" . c++-mode)
			 auto-mode-alist))
  (setq auto-mode-alist (cons
			 '("\\.tex$" . latex-mode)
			 auto-mode-alist)))


(leaf company
  :defvar company-dabbrev-ignore-case company-dabbrev-code-ignore-case company-tooltip-limit company-idle-delay company-echo-delay company-minimum-prefix-length
  :commands global-company-mode
  :hook ((after-init-hook . global-company-mode))
  :config
  (add-hook 'company-mode-hook #'company-prescient-mode)
  (with-eval-after-load 'company
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-code-ignore-case nil)
    (setq company-tooltip-limit 20)
    (setq company-idle-delay 0.1)
    (setq company-echo-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq defvar nil)
    (setq company-tooltip-align-annotations nil)
    (setq setq nil)
    (setq company-tooltip-align-annotations nil)))

(leaf bash-dynamic-completion
  :commands bash-completion-dynamic-complete
  :hook ((shell-dynamic-complete-functions . bash-completion-dynamic-complete)))

(leaf eldoc
  :hook ((emacs-lisp-mode-hook . eldoc-mode)
	 (lisp-interaction-mode-hook . eldoc-mode)
	 (ielm-mode-hook . eldoc-mode)))

(leaf dumb-jump
  :hook ((xref-backend-functions . dumb-jump-xref-activate)))

(leaf browse-url
  :require t
  :preface
  (defun my-browse-url-chromium-wayland (url &optional ignored)
    "Pass the specified URL to the \"chromium\" command.
The optional argument IGNORED is not used."
    (interactive (browse-url-interactive-arg "URL: "))
    (let ((cmd
	   (concat
	    "DISPLAY=:0 HOME=" (getenv "HOME")
	    " PATH=" (getenv "PATH")
	    " XAUTHORITY=" (getenv "XAUTHORITY")
	    " WAYLAND_DISPLAY=" (getenv "WAYLAND_DISPLAY")
	    " USER=" (getenv "USER")
	    " GDK_BACKEND=x11 /usr/bin/setsid -w chromium " url)))
      (start-process-shell-command "browser" "*chromium-open-url*" cmd)))

  :setq ((browse-url-browser-function function my-browse-url-chromium-wayland)))

(leaf my-open-multiple-files
  :hook ((window-setup-hook . delete-other-windows)))

(leaf pkgbuild-mode
  :when (file-exists-p "/etc/pacman.conf")
  :mode ("/PKGBUILD$"))

(leaf poly-markdown
  :mode ("\\.text\\'"
	 ("\\.md$" . poly-gfm-mode)
	 "\\.markdown$")
  :bind ((markdown-mode-map
	  ("M-p" . ace-window))))

(leaf flymake-proselint
  :hook ((markdown-mode-hook . flymake-proselint-setup)
	 (gfm-mode-hook . flymake-proselint-setup)))

(leaf vmd-mode
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
  :bind ((markdown-mode-command-map
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

(leaf emmet-mode
  :disabled t
  :hook (sgml-mode-hook web-mode-hook rjsx-mode css-mode-hook)
  :require t
  :setq ((emmet-move-cursor-between-quotes . t)))

(leaf avy
  :chord (("fj" . avy-goto-word-1)
          ("f'" . avy-pop-mark))
  :config
  (define-key isearch-mode-map (kbd "C-'") #'avy-isearch))


(leaf expand-region
  :chord (("zj" . er/expand-region)
          ("zk" . er/contract-region)))

(leaf multiple-cursors
  :commands (multiple-cursors-hydra/body)
  :chord (("mf" . multiple-cursors-hydra/body))
  :config
  (progn
    (require 'hydra)
    (defhydra multiple-cursors-hydra (:hint nil)
      "
     ^Up^            ^Down^            ^Other^
--------------------------------------------------
[_p_]   Next      [_n_]   Next      [_l_] Edit lines
[_P_]   Skip      [_N_]   Skip      [_a_] Mark all
[_M-p_] Unmark    [_M-n_] Unmark    [_A_] Mark all words
[_W_]   Up word   [_w_]   Down word [_r_] Mark
^ ^               ^ ^              [_q_] Quit
^ ^               ^ ^              [_h_] Toggle hide unmatched
^ ^               ^ ^              [_j_] Jump for add or remove cursors
^ ^               ^ ^              [_k_] Jump for single cursor
"
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this)
      ("A" mc/mark-all-words-like-this)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("r" mc/mark-all-in-region-regexp :exit t)
      ("W" mc/mark-previous-word-like-this)
      ("w" mc/mark-next-word-like-this)
      ("h" mc-hide-unmatched-lines-mode)
      ("j" ace-mc-add-multiple-cursors :exit t)
      ("k" ace-mc-add-single-cursor :exit t)
      ("q" nil))))

(leaf yasnippet
  :bind (([tab]
	  . tab-indent-or-complete)
	 ("TAB" . tab-indent-or-complete))
  :config
  (setq yas-inhibit-overlay-modification-protection t)
  (run-with-idle-timer 3 nil #'require 'yasnippet nil t)
  (with-eval-after-load 'yasnippet
    (progn
      (yas-global-mode 1)
      (defun check-expansion ()
	"Check yasnippet expansion."
	(save-excursion
	  (if (looking-at "\\_>")
	      t
	    (backward-char 1)
	    (if (looking-at "\\.")
		t
	      (backward-char 1)
	      (if (looking-at "->")
		  t nil)))))

      (defvar yas-minor-mode)
      (defun tab-indent-or-complete ()
	"Smart tab function."
	(interactive)
	(if (minibufferp)
	    (minibuffer-complete)
	  (if (or
	       (not yas-minor-mode)
	       (null (yas-expand)))
	      (if (check-expansion)
		  (company-complete-common)
		(indent-for-tab-command))))))))

(leaf x-hyper-keysym
  :defvar x-hyper-keysym
  :setq ((x-hyper-keysym quote meta)
	 (mac-option-modifier quote none)
	 (mac-command-modifier quote meta)
	 (mac-command-key-is-meta quote t)
	 (mac-option-key-is-meta nil)))

(leaf fzf
  :bind* (("C-c C-f" . my-fzf-project))
  :config
  (with-eval-after-load 'fzf
    (defun my-fzf-project ()
      "Fzf in project directory."
      (interactive)
      (fzf/start
       (or
	(if (project-current)
	    (project-root (project-current)))
	default-directory)))))

(leaf wgrep
  :bind (("C-c C-p" . wgrep-change-to-wgrep-mode))
  :config
  (with-eval-after-load 'wgrep
    (setq wgrep-auto-save-buffer t)))

(leaf keyboard-selection
  :setq ((select-enable-primary . t)
	 (select-enable-clipboard . t)))

(leaf mouse
  :bind (([drag-mouse-0]
	  . mouse-set-region))
  :require mouse
  :setq ((mouse-drag-copy-region . t))
  :config
  (xterm-mouse-mode t))

(leaf imenu
  :bind (("M-i" . imenu)))

(leaf magit
  :defvar auto-revert-check-vc-info
  :bind (("C-x C-j" . my-magit-find-file-other-frame))
  :bind* (("C-x g" . magit-status))
  :config
  (leaf diff-mode
    :require t)
  (with-eval-after-load 'magit
    (progn
      (defun my-magit-diff-hook ()
	"My hook for improve magit diff."
	(local-set-key
	 (kbd "h")
	 #'diff-refine-hunk))

      (add-hook 'magit-diff-mode-hook #'my-magit-diff-hook)
      (setq auto-revert-check-vc-info t)
      (defun my-magit-find-file-other-frame (file)
	"View FILE from worktree, in another frame.\nSwitch to a buffer visiting blob FILE, creating one if none\nalready exists.  If prior to calling this command the current\nbuffer and/or cursor position is about the same file, then go to\nthe line and column corresponding to that location."
	(interactive
	 (my-magit-find-file-read-args "Find file in other frame"))
	(find-file-other-frame
	 (f-join
	  (vc-root-dir)
	  file)))

      (defun my-magit-find-file (file)
	"View FILE from worktree.\nSwitch to a buffer visiting blob FILE, creating one if none\nalready exists.  If prior to calling this command the current\nbuffer and/or cursor position is about the same file, then go\nto the line and column corresponding to that location."
	(interactive
	 (my-magit-find-file-read-args "Find file"))
	(find-file
	 (f-join
	  (vc-root-dir)
	  file)))

      (defun my-magit-find-file-read-args (prompt)
	(list
	 (magit-read-file-from-rev "HEAD" prompt))))))



(leaf edit-indirect
  :chord ((";r" . edit-indirect-region)))

(leaf pandoc
  :defun (pandoc-load-default-settings . ext:pandoc)
  :hook ((markdown-mode-hook . pandoc-mode)
	 (pandoc-mode-hook . pandoc-load-default-settings)))

(leaf erlang
  :disabled t
  :defvar erlang-extended-mode-map
  :mode ("\\.erl$" "\\.hrl$" "rebar\\.config$" "relx\\.config$" "system\\.config$" "\\.app\\.src$")
  :hook ((erlang-mode-hook . my-erlang-hook)
	 (erlang-mode-hook . company-erlang-init))
  :config
  (with-eval-after-load 'erlang
    (progn
      (leaf company-erlang
	:config
	(load "company-erlang-autoloads"))

      (leaf ivy-erlang-complete)

      (add-to-list 'load-path "/usr/lib/erlang/lib/wrangler-1.2.0/elisp")
      (defun my-format-erlang-record ()
	"Format erlang record."
	(interactive)
	(let ((from (line-beginning-position)))
	  (goto-char from)
	  (search-forward "-record")
	  (search-forward "{")
	  (goto-char (-
		      (point)
		      1))
	  (ignore-errors
	    (er/expand-region 1))
	  (my-align-region-by "=")
	  (goto-char from)
	  (search-forward "-record")
	  (search-forward "{")
	  (goto-char (-
		      (point)
		      1))
	  (ignore-errors
	    (er/expand-region 1))
	  (my-align-region-by "::")))

      (defun my-erlang-hook ()
	"Setup for erlang."
	(ignore-errors
	  (require 'wrangler))
	(ivy-erlang-complete-init)
	(define-key erlang-extended-mode-map
	  (kbd "M-.")
	  nil)
	(define-key erlang-extended-mode-map
	  (kbd "M-,")
	  nil)
	(define-key erlang-extended-mode-map
	  (kbd "M-?")
	  nil)
	(define-key erlang-extended-mode-map
	  (kbd "(")
	  nil)
	(define-key erlang-extended-mode-map
	  (kbd "C-M-i")
	  nil)
	(local-set-key
	 (kbd "C-c C-p")
	 #'my-format-erlang-record)
	(local-set-key
	 (kbd "C-M-i")
	 #'ivy-erlang-complete))

      (add-hook 'after-save-hook #'ivy-erlang-complete-reparse))))

(leaf open-urls
  :bind (("C-x u" . link-hint-open-multiple-links)))

(leaf xml-mode
  :mode (("\\.xsd\\'" . xml-mode)
	 ("\\.xslt\\'" . xml-mode)))

(leaf hungry-delete
  :config
  (run-with-idle-timer 0.5 nil #'require 'hungry-delete nil t)
  (with-eval-after-load 'hungry-delete
    (global-hungry-delete-mode)))

(leaf ace-link
  :config
  (run-with-idle-timer 0.1 nil #'require 'ace-link nil t)
  (with-eval-after-load 'ace-link
    (ace-link-setup-default)))

(leaf ace-window
  :bind (("M-p" . ace-window))
  :config
  (with-eval-after-load 'ace-window
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))


(leaf which-key
  :hook (after-init-hook)
  :require t
  :setq ((which-key-show-transient-maps . 't)))

(leaf composable
  :disabled t
  :straight
  (composable :type git :host github :repo "paldepind/composable.el"
	      :fork (:host github
			   :repo "Ergus/composable.el"))
  :config
  (run-with-idle-timer 0.1 nil #'require 'composable nil t)
  (with-eval-after-load 'composable
    (progn
      (composable-mode)
      (composable-mark-mode))))

(leaf ibuffer-vc
  :bind* (("C-x C-b" . ibuffer))
  :config
  (run-with-idle-timer 3 nil #'require 'ibuffer-vc nil t)
  (with-eval-after-load 'ibuffer-vc
    (add-hook 'ibuffer-hook
	      (lambda nil
		(ibuffer-vc-set-filter-groups-by-vc-root)
		(unless (eq ibuffer-sorting-mode 'alphabetic)
		  (ibuffer-do-sort-by-alphabetic))))))

(leaf symbol-overlay
  :config
  (run-with-idle-timer 2 nil #'require 'symbol-overlay nil t)
  (with-eval-after-load 'symbol-overlay
    (setq-default symbol-overlay-temp-in-scope t)
    (add-hook 'prog-mode-hook #'symbol-overlay-mode)
    (add-hook 'emacs-lisp-mode-hook #'symbol-overlay-mode)))

(leaf zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

(leaf reverse-im
  :require t
  :config
  (reverse-im-activate "russian-computer"))

(leaf aggressive-indent
  :preface
  (defun my-disable-aggressive-indent ()
    "Disable aggressive indent mode in current buffer."
    (interactive)
    (aggressive-indent-mode -1))

  :hook ((lsp-mode-hook . my-disable-aggressive-indent))
  :require t
  :config
  (aggressive-indent-global-mode))

(leaf password-store
  :commands password-store-get)

(leaf pass
  :commands pass)

(leaf dash
  :after t
  :config
  (dash-enable-font-lock))

(leaf rg
  :bind* (("C-c C-s" . my-grep-vc-or-dir))
  :bind (:rg-mode-map
         ("f" . next-error-follow-minor-mode)
         ("s" . my-rg-save-search-as-name)
         ("n" . next-line)
         ("p" . previous-line)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file))
  :config
  (with-eval-after-load 'rg
    (require 'wgrep)
    (setq rg-group-result t)
    (setq rg-hide-command t)
    (setq rg-show-columns nil)
    (setq rg-show-header t)
    (setq rg-custom-type-aliases nil)
    (setq rg-default-alias-fallback "all")
    (rg-define-search my-grep-vc-or-dir :query ask :format regexp :files "everything" :case-fold-search smart :dir
      (let ((vc (vc-root-dir)))
	(if vc
	    vc default-directory))
      :confirm prefix :flags
      ("--hidden -g !.git"))
    (defun my-rg-save-search-as-name ()
      "Save `rg' buffer, naming it after the current search query.\n\nThis function is meant to be mapped to a key in `rg-mode-map'."
      (interactive)
      (let ((pattern (car rg-pattern-history)))
	(rg-save-search-as-name
	 (concat "«" pattern "»"))))

    (defun my-next-window (_)
      (other-window 1))

    (advice-add 'my-grep-vc-or-dir :after 'my-next-window)))


(leaf my-yank-kill-ring
  :disabled t
  :preface
  (defun my-icomplete-yank-kill-ring ()
    "Insert the selected `kill-ring' item directly at point."
    (interactive)
    (let ((icomplete-separator (concat "\n"
				       (propertize "..................." 'face 'shadow)
				       "\n ")))
      (insert
       (completing-read "paste from kill ring:" kill-ring nil t))))

  :bind (("M-y" . my-icomplete-yank-kill-ring)))

(leaf isearch
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
    (minibuffer-with-setup-hook
	(lambda ()
	  (setq orderless-transient-matching-styles '(orderless-prefixes)))
      (consult-line-from-isearch)))

  :bind ((isearch-mode-map
	  ("M-i" . my-consult-line-from-isearch)
	  ("M-n" . my-isearch-next)))
  :config
  (setq search-whitespace-regexp ".*")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil))

(leaf upcase-region
  :config
  (put 'upcase-region 'disabled nil))

(leaf comment-tags
  :hook (prog-mode-hook)
  :require t)

(leaf eww-more-readable
  :preface
  (defun eww-more-readable ()
    "Better eww.  Run it after eww buffer is loaded."
    (interactive)
    (set-window-margins
     (get-buffer-window)
     20 20)
    (eww-reload 'local))

  :hook ((eww-after-render-hook . eww-more-readable)))

(leaf yaml
  :preface
  (defun my-disable-auto-fill ()
    "Disable `auto-fill-mode'."
    (auto-fill-mode -1))

  (defun my-enable-prism ()
    "Enable `prism-mode'."
    (prism-mode 1))

  :hook ((yaml-mode-hook . highlight-indentation-mode)
	 (yaml-mode-hook . highlight-indentation-current-column-mode)
	 (yaml-mode-hook . my-disable-auto-fill)
	 (yaml-mode-hook . my-enable-prism))
  :require company-dabbrev-code
  :chord (("<<" . smart-shift-left)
	  (">>" . smart-shift-right))
  :config
  (add-to-list 'company-dabbrev-code-modes 'yaml-mode)
  (add-to-list 'company-dabbrev-code-modes 'protobuf-mode)
  (global-smart-shift-mode 1))

(leaf auto-yasnippet
  :bind (("C-c y" . aya-create)
	 ("C-." . aya-expand))
  :config
  (with-eval-after-load 'auto-yasnippet
    (setq aya-field-regex "\\sw\\|\\s_\\|\\*\\|\\&")))

(leaf toggle-fullscreen
  :bind (("ff" . toggle-frame-fullscreen)))


(leaf open-file-as-root
  :preface
  (defun open-this-file-as-root ()
    "Edit current file as root, using `tramp' and `sudo'.\nIf the current buffer is not visiting a file, prompt for a file name."
    (interactive)
    (let* ((filename (or buffer-file-name
			 (read-file-name "Find file (as root): ")))
	   (tramp-path (concat "/sudo:root@localhost:" filename)))
      (if buffer-file-name
	  (find-alternate-file tramp-path)
	(find-file tramp-path))))

  :bind (("C-c C-r" . open-this-file-as-root)))

(leaf helm-make
  :disabled t
  :after t
  :setq ((helm-make-directory-functions-list
	  '(helm-make-project-directory helm-make-current-directory))))

(leaf lsp-mode
  :preface
  (setq lsp-keymap-prefix (kbd "C-x l"))

  (defun my-lsp-before-save ()
    (interactive)
    (when lsp-mode
      (lsp-organize-imports)
      (lsp-format-buffer)))

  :after t
  :require yasnippet
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'lsp-mode-hook #'lsp-completion--enable)
  (add-hook 'before-save-hook #'my-lsp-before-save))

(leaf my-go-home
  :preface
  (defun my-go-home ()
    "Go to home dir."
    (cd
     (getenv "HOME")))

  :hook ((after-init-hook . my-go-home)))

(leaf libgit
  :straight (libgit :fetcher github
		    :repo "magit/libegit2"
		    :fork "s-kostyaev/libegit2"
		    :files ("CMakeLists.txt"
			    ("libgit2" "libgit2/cmake")
			    ("libgit2" "libgit2/CMakeLists.txt")
			    ("libgit2" "libgit2/COPYING")
			    ("libgit2" "libgit2/deps")
			    ("libgit2" "libgit2/.HEADER")
			    ("libgit2" "libgit2/include")
			    ("libgit2" "libgit2/libgit2_clar.supp")
			    ("libgit2" "libgit2/libgit2.pc.in")
			    ("libgit2" "libgit2/script")
			    ("libgit2" "libgit2/src")
			    "libgit.el"
			    "Makefile"
			    "src"
			    "uthash")))

(leaf so-long
  :when (>= emacs-major-version 27)
  :hook ((after-init-hook . global-so-long-mode)))

(leaf hippie-expand
  :bind (("C-;" . hippie-expand)))

(leaf dtrt-indent
  :hook (prog-mode-hook protobuf-mode-hook))

;;; Screencasts

(leaf gif-screencast
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

	  (advice-add #'gif-screencast--cropping-region :around #'my-fix-screencast-hidpi))

      (setq gif-screencast-program "gnome-screenshot"
	    gif-screencast-args '("-w" "-f")
	    gif-screencast-capture-format "png"))
    (setq gif-screencast-capture-prefer-internal t)))

(leaf c-cpp-mode
  :hook ((c-mode-hook . lsp-deferred)
	 (c++-mode-hook . lsp-deferred)))

(leaf icomplete
  :bind ((icomplete-minibuffer-map
	  ("<down>" . icomplete-forward-completions)
	  ("C-n" . icomplete-forward-completions)
	  ("<up>" . icomplete-backward-completions)
	  ("C-p" . icomplete-backward-completions)
	  ("C-v" . icomplete-vertical-toggle)
	  ("<backspace>" . icomplete-fido-backward-updir)
	  ("C-j" . icomplete-force-complete)
	  ("C-M-j" . exit-minibuffer)
	  ("<RET>" . icomplete-force-complete-and-exit)))
  :setq ((completion-styles quote
			    (orderless))
	 (completion-category-overrides quote
					((file
					  (styles basic substring))))
	 (read-file-name-completion-ignore-case . t)
	 (read-buffer-completion-ignore-case . t)
	 (completion-ignore-case . t)
	 (icomplete-show-matches-on-no-input . t)
	 (icomplete-prospects-height . 10))
  :require orderless
  :init
  (icomplete-mode)
  (icomplete-vertical-mode)
  (setq completion-ignore-case t))

(leaf orderless
  :preface
  (defun my-orderless-dispatch (pattern _index _total)
    (cond
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (setq orderless-style-dispatchers '(my-orderless-dispatch))
  :hook ((minibuffer-exit-hook . orderless-remove-transient-configuration)))

(leaf consult
  :preface
  (defun my-consult-project-rg ()
    (interactive)
    (let ((xref-search-program 'ripgrep)
	  (xref-show-xrefs-function 'xref--show-defs-minibuffer))
      (call-interactively 'project-find-regexp)))

  :bind (("C-x b" . consult-buffer)
	 ("<help> a" . consult-apropos)
	 ("M-i" . consult-imenu)
	 ("M-y" . consult-yank-pop))
  ;; :init
  ;; (consult-preview-mode +1)
  :setq ((completion-in-region-function . 'consult-completion-in-region)))

(leaf marginalia-mode
  :init
  (marginalia-mode +1)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(leaf embark
  :disabled t
  :bind ((minibuffer-local-completion-map
	  ("M-o" . embark-act-noexit)
	  ("C-o" . embark-act)
	  ("M-r" . embark-become))))


(leaf external-process-improvements
  :preface
  (setq read-process-output-max (* 1024 1024)))

(leaf string-inflection
  :bind (("C-c C-u" . string-inflection-all-cycle)))

(leaf rust
  :hook ((rust-mode-hook . lsp-deferred))
  :setq ((rust-format-on-save . t)))

(leaf tree-sitter
  :preface
  (require 'tree-sitter-langs)

  (defun my-enable-tree-sitter ()
    "Enable tree-sitter."
    (tree-sitter-hl-mode +1))

  :hook ((tree-sitter-after-on-hook . my-enable-tree-sitter))
  :config
  (global-tree-sitter-mode))

(leaf comby
  :bind (("C-c C-e" . comby)))

(leaf indent-region-workaround
  :preface
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

  :config
  (advice-add 'indent-region :around #'my-suppress-messages))

(leaf lsp-pyright
  :setq ((lsp-pyright-auto-search-paths . t)
	 (lsp-pyright-venv-path . "/home/feofan/.local/share/virtualenvs/"))
  :config
  (add-hook 'python-mode-hook
	    #'(lambda nil
		(require 'lsp-pyright)
		(lsp-deferred))))

(leaf go-translate
  :bind (("C-c t" . go-translate)
	 ("C-c T" . go-translate-popup))
  :config
  (with-eval-after-load 'go-translate
    (setq go-translate-token-current (cons 430675 2721866130))
    (setq go-translate-target-language "ru")
    (setq go-translate-local-language "en")
    (setq go-translate-extra-directions '(("en" . "ru")
					  ("de" . "ru")
					  ("ru" . "en")
					  ("ru" . "de")))))

(leaf savehist
  :hook (after-init-hook-hook)
  :config
  (with-eval-after-load 'savehist
    (setq savehist-file "~/.emacs.d/savehist")
    (setq history-length 1000)
    (setq history-delete-duplicates t)
    (setq savehist-save-minibuffer-history t)))

(leaf saveplace
  :require t
  :setq ((save-place-file . "~/.emacs.d/saveplace")
	 (save-place-forget-unreadable-files . t))
  :config
  (save-place-mode 1))

(leaf recentf
  :preface
  (recentf-mode 1)
  (setq recentf-max-menu-items 300)
  (setq recentf-max-saved-items 300))

(leaf package-lint-flymake
  :preface
  (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup))

(leaf my-make
  :preface
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
    "Make current project with targets selection.\nUse project root as default directory if universal ARG is not set.\nSelect it interactively otherwise."
    (interactive "p")
    (let* ((project (project-current))
	   (default-directory (if (= arg 4)
				  (read-directory-name "select directory ")
				(if project
				    (project-root project)
				  default-directory)))
	   (makefile (expand-file-name "Makefile" default-directory))
	   (targets (my--make-target-list makefile))
	   (target (completing-read "make " targets)))
      (compile
       (format "make %s" target)))))

(leaf smerge-mode
  :preface
  (hercules-def :toggle-funs #'smerge-mode
		:keymap 'smerge-basic-map
		:show-funs '(smerge-next smerge-prev)
		:transient t))

(leaf imaxima
  :init (require 'cl)
  :commands (imaxima))

(leaf fricas
  :preface
  (add-to-list 'load-path "~/projects/fricas/contrib/emacs")
  ;; (add-to-list 'load-path "/usr/lib/fricas/emacs")
  (defun my-fricas-setup()
    "Configure fricas."
    (process-send-string fricas-process
                         (concat
			  ")set output tex on\n"
			  ")set output algebra off\n")))
  :commands fricas
  :config
  (add-hook 'fricas-mode-hook #'my-fricas-setup))

(leaf axiom
  :preface
  (defun my-setup-axiom ()
    "Setup axiom."
    (setq-local company-backends '(company-axiom-backend))
    (aggressive-indent-mode -1))

  (setq axiom-process-enable-pretty-print t)
  (setq axiom-process-embed-gnu-draw t)
  (setq axiom-process-show-svg t)
  :hook ((axiom-process-mode-hook . my-setup-axiom)))

(leaf my-screenshots
  :preface
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
      (dired-rename-file filename (expand-file-name (file-name-nondirectory filename)
						    (expand-file-name "~/Pictures")) 1))))

(leaf languagetool
  :disabled t
  :setq ((languagetool-default-language . "en-US")))

(leaf nov
  :mode (("\\.epub\\'" . nov-mode))
  :preface
  (leaf speechd
    :preface
    (defun my-read-buffer ()
      (interactive)
      (speechd-stop t)
      (speechd-set-language "ru")
      (speechd-set-rate 100 t)
      (speechd-say-text (buffer-substring-no-properties (point) (point-max))))

    (defun my-stop-reading ()
      (interactive)
      (speechd-stop t)))

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
  (leaf justify-kp
    :straight (justify-kp :type git :host github :repo "Fuco1/justify-kp")
    :require t)
  (setq nov-text-width t)

  (defun my-nov-window-configuration-change-hook ()
    (my-nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
		 'my-nov-window-configuration-change-hook
		 t))

  (defun my-nov-post-html-render-hook ()
    (if (get-buffer-window)
	(let ((max-width (pj-line-width))
              buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (not (looking-at "^[[:space:]]*$"))
		(goto-char (line-end-position))
		(when (> (shr-pixel-column) max-width)
                  (goto-char (line-beginning-position))
                  (pj-justify)))
              (forward-line 1))))
      (add-hook 'window-configuration-change-hook
		'my-nov-window-configuration-change-hook
		nil t)))

  (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook))

(leaf golden-ratio-mode
  :disabled t
  :init
  (golden-ratio-mode +1))

(leaf golden
  :straight (golden :type git :repo "https://git.sr.ht/~wklew/golden")
  :init
  (global-golden-mode +1))

(leaf fsharp-mode
  :hook (;; (fsharp-mode-hook . lsp)
	 (fsharp-mode-hook . eglot-ensure))
  :init
  (require 'eglot-fsharp)
  (defun my-fix-fsac ()
    (interactive)
    (dolist  (file (directory-files-recursively (expand-file-name "~/.emacs.d/.cache/lsp/fsautocomplete") "." t))
      (if (file-directory-p file)
	  (chmod file #o755)
	(chmod file #o644)))))

(provide 'init)
;;; init.el ends here
