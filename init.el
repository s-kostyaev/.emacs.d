;;; init.el --- Emacs init file. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-and-compile
  (if (< emacs-major-version 27)
      (progn
	(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
	(require 'early-init))))


(defvar gnutls-trustfiles)
(with-eval-after-load 'gnutls
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))

(eval-and-compile
  (defvar network-security-level)
  (defvar gnutls-verify-error)
  (if (> emacs-major-version 24)
      (progn
        (setq network-security-level 'high)
        (setq gnutls-verify-error t))))

(defvar luna-dumped nil
  "Non-nil when a dump file is loaded (because dump.el sets this variable).")

(defmacro luna-if-dump (then &rest else)
  "Evaluate THEN if running with a dump file, else evaluate ELSE."
  (declare (indent 1))
  `(if luna-dumped
       ,then
     ,@else))

(defvar luna-dumped-load-path nil)
(luna-if-dump
    (progn
      (setq load-path luna-dumped-load-path)
      (global-font-lock-mode)
      (transient-mark-mode)
      (defun my-fix-scratch ()
        (save-excursion
          (switch-to-buffer "*scratch*")
          (lisp-interaction-mode)))
      (add-hook 'after-init-hook 'my-fix-scratch))
  ;; add load-path’s and load autoload files
  (require 'package)
  (package-initialize))

(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

(global-set-key (kbd "C-M-r") #'(lambda () (interactive)
                                  (dolist (elt (package--alist))
                                    (condition-case err
                                        (package-activate (car elt) t)
                                      ;; Don't let failure of activation of a package arbitrarily stop
                                      ;; activation of further packages.
                                      (error (message "%s" (error-message-string err)))))
                                  (byte-recompile-file "~/.emacs.d/init.el" t 0 t)
                                  (my-recompile-installed-packages)
                                  (if (eq system-type 'darwin)
                                      (start-process-shell-command
                                       "dump" "*dump*"
                                       "emacs --batch -q -l ~/.emacs.d/dump.el"))))

(setq custom-file "~/.emacs.d/emacs-customizations.el")

(setq my-light-theme ;; 'kaolin-valley-light
      ;; 'dichromacy
      ;; 'adwaita
      ;; 'spacemacs-light
      ;; 'tsdh-light
      'ample-light
      ;; 'moe-light
      ;; 'solarized-light
      my-dark-theme ;; 'misterioso
      ;; 'zenburn
      ;; 'spacemacs-dark
      'chocolate
      ;; 'monokai
      my-need-fix-bg nil)

(defun my-toggle-themes ()
  "Toggle light and dark themes."
  (interactive)
  (let ((cur-theme (if (equal (car custom-enabled-themes)
                              my-light-theme)
                       'light
                     'dark)))
    (mapc #'disable-theme custom-enabled-themes)
    (if (equal cur-theme 'light)
        (progn
          (load-theme my-dark-theme t)
          (if my-need-fix-bg
              (custom-set-faces
               '(default ((t (:height 130 :width normal :family "Go Mono")))))))
      (load-theme my-light-theme t)
      (if my-need-fix-bg
          (custom-set-faces
           '(default ((t (:background "#fdf6e3" :height 130 :width normal :family "Go Mono")))))))))

(global-set-key (kbd "<f6>") #'my-toggle-themes)

(defun my-set-themes ()
  "Function for setting themes after init."
  (interactive)
  (let ((cur-hour (nth 2 (decode-time))))
    (mapc #'disable-theme custom-enabled-themes)
    (if (and (>  cur-hour 7)
             (<  cur-hour 20))
        (progn
          (load-theme my-light-theme t)
          (if my-need-fix-bg
              (custom-set-faces
               '(default ((t (:background "#fdf6e3":height 130 :width normal :foundry "nil" :family "Go Mono")))))))
      (load-theme my-dark-theme t)
      (if my-need-fix-bg
          (custom-set-faces
           '(default ((t (:height 130 :width normal :family "Go Mono")))))))))

(add-hook 'after-init-hook #'my-set-themes)
(add-hook 'desktop-after-read-hook #'my-set-themes)
(with-eval-after-load 'emacs-customizations #'my-set-themes)

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
                                    (f-ancestor-of?
                                     (getenv "HOME")
                                     (locate-file (concat theme "-theme.el")
                                                  (custom-theme--load-path)
                                                  '("" "c"))))
                                  (mapcar #'symbol-name
			                  (custom-available-themes)))))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t nil)))

(defun my-load-custom-file ()
  "Load my custom file."
  (load-file custom-file))

(add-hook 'after-init-hook #'my-load-custom-file)


(eval-when-compile
  (require 'use-package)
  (require 'use-package-chords))
(require 'bind-key)

(use-package key-chord
  :defer 2
  :config
  (key-chord-mode 1))

(use-package flymake
  :bind (("C-x `" . flymake-goto-next-error)
         ("C-c r" . flymake-show-diagnostics-buffer)))

(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(defun my-flymake--mode-line-format ()
  "Produce a pretty minor mode indicator."
  (if flymake--backend-state
      (let* ((known (hash-table-keys flymake--backend-state))
             (running (flymake-running-backends))
             (disabled (flymake-disabled-backends))
             (diags-by-type (make-hash-table))
             (all-disabled (and disabled (null running))))
        (maphash (lambda (_b state)
                   (mapc (lambda (diag)
                           (push diag
                                 (gethash (flymake--diag-type diag)
                                          diags-by-type)))
                         (flymake--backend-state-diags state)))
                 flymake--backend-state)
        `(,@(unless (or all-disabled
                        (null known))
              (cl-loop
               with types = (hash-table-keys diags-by-type)
               with _augmented = (cl-loop for extra in '(:error :warning)
                                          do (cl-pushnew extra types
                                                         :key #'flymake--severity))
               for type in (cl-sort types #'> :key #'flymake--severity)
               for diags = (gethash type diags-by-type)
               for face = (flymake--lookup-type-property type
                                                         'mode-line-face
                                                         'compilation-error)
               when (or diags
                        (cond ((eq flymake-suppress-zero-counters t)
                               nil)
                              (flymake-suppress-zero-counters
                               (>= (flymake--severity type)
                                   (warning-numeric-level
                                    flymake-suppress-zero-counters)))
                              (t t)))
               collect `(:propertize
                         ,(format "%d" (length diags))
                         face ,face
                         mouse-face mode-line-highlight
                         keymap
                         ,(let ((map (make-sparse-keymap))
                                (type type))
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-down-event)
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (flymake-goto-prev-error 1 (list type) t))))
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-up-event)
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (flymake-goto-next-error 1 (list type) t))))
                            map)
                         help-echo
                         ,(concat (format "%s diagnostics of type %s\n"
                                          (propertize (format "%d"
                                                              (length diags))
                                                      'face face)
                                          (propertize (format "%s" type)
                                                      'face face))
                                  (format "%s/%s: previous/next of this type"
                                          mouse-wheel-down-event
                                          mouse-wheel-up-event)))
               into forms
               finally return
               `((:propertize "[")
                 ,@(cl-loop for (a . rest) on forms by #'cdr
                            collect a when rest collect
                            '(:propertize " "))
                 (:propertize "]"))))))
    nil))

(require 'flymake)
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

(defun my-update-vc-mode ()
  "Update variable `vc-mode' for modeline."
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode)))
      (setq vc-mode
            (propertize vc-mode
                        'face  (cond ((string-match "^ -" noback)    'font-lock-keyword-face)
                                     ((string-match "^ [:@]" noback) 'font-lock-warning-face)
                                     ((string-match "^ [!\\?]" noback) 'font-lock-warning-face)))))))

(add-hook 'after-revert-hook #'my-update-vc-mode)
(add-hook 'after-find-file #'my-update-vc-mode)

;; to setup tabs
;; (defvar c-basic-indent)
;; (setq c-basic-indent 4)
;; (setq tab-width 4)
;; (setq tab-stop-list (number-sequence 4 200 4))
;; (setq-default indent-tabs-mode nil)

;; Text and the such
;; Use colors to highlight commands, etc.
(add-hook 'after-init-hook #'global-font-lock-mode)
;; Disable the welcome message
(setq inhibit-startup-message t)
;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")
;; Make the mouse wheel scroll Emacs
(mouse-wheel-mode t)
(setq auto-window-vscroll nil)
(global-set-key (kbd "M-J") #'scroll-up-line)
(global-set-key (kbd "M-K") #'scroll-down-line)
(global-set-key (kbd "<mouse-5>") #'scroll-up-line)
(global-set-key (kbd "<mouse-4>") #'scroll-down-line)
;; (defun my-scroll-hook(_)
;;   "Increase gc-threshold before scroll and set it back after."
;;   (setq gc-cons-threshold most-positive-fixnum)
;;   ;; (internal-show-cursor nil nil)
;;   (run-with-idle-timer 1 nil (lambda ()
;;                                ;; (internal-show-cursor nil t)
;;                                (setq gc-cons-threshold (* 8 1024 1024)))))

;; (advice-add 'scroll-up-line :before 'my-scroll-hook)
;; (advice-add 'scroll-down-line :before 'my-scroll-hook)
;; (declare-function pixel-scroll-mode "ext:pixel-scroll")
;; (defvar pixel-resolution-fine-flag)
;; (if (> emacs-major-version 25)
;;     (progn
;;       (pixel-scroll-mode)
;;       (setq pixel-resolution-fine-flag t)
;;       (setq mouse-wheel-progressive-speed nil)
;;       (setq mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control))))
;;       ;; (advice-add 'pixel-scroll-down :before 'my-scroll-hook)
;;       ;; (advice-add 'pixel-scroll-up :before 'my-scroll-hook)
;;       ))

;; Always end a file with a newline
(setq require-final-newline t)
;; Stop emacs from arbitrarily adding lines to the end of a file when the
;; cursor is moved past the end of it:
(setq next-line-add-newlines nil)
;; Remove icons toolbar
;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)


(defvar delimit-columns-extra)
(defvar delimit-columns-format)
(defvar delimit-columns-str-separator)
(defvar delimit-columns-separator)
(defun my-align-region-by (&optional delimiter)
  "Align current region by DELIMITER."
  (interactive)
  (let* ((delim (or delimiter (read-string "delimiter: ")))
         (delimit-columns-separator delim)
         (delimit-columns-str-separator delim)
         (delimit-columns-format 'separator)
         (delimit-columns-extra nil)
         (beg (region-beginning)))
    (delimit-columns-region beg (region-end))
    (goto-char beg)
    (ignore-errors (er/expand-region 1))
    (let ((new-end (region-end)))
      (goto-char new-end)
      (whitespace-cleanup-region beg (line-end-position)))))
(global-set-key (kbd "C-c a") #'my-align-region-by)

(use-package hydra
  :defer t
  :config (defhydra hydra-cycle-windows
            (:body-pre (other-window 1))
            "Windows"
            ("o" (other-window 1) "Next")
            ("O" (other-window -1) "Previous")
            ("t" toggle-window-split "Toggle split")
            ("]" enlarge-window-horizontally "Enlarge horizontal")
            ("[" shrink-window-horizontally "Shrink horizontal")
            ("=" enlarge-window "Enlarge vertival")
            ("-" shrink-window "Shrink vertical")
            ("b" balance-windows "Balance windows")
            ("m" delete-other-windows "Maximize window")
            ("n" split-window-below "New window")
            ("c" delete-window "Close window")
            ("q" nil "quit"))
  :bind ("C-x o" . hydra-cycle-windows/body))

(global-set-key (kbd "C-x t") #'toggle-window-split)

;; (windmove-default-keybindings)

(defun toggle-window-split ()
  "Toggle window split vertically or horizontally."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(use-package flycheck
  :disabled t
  :defer 3
  :bind ("C-c r" . flymake-show-diagnostics-buffer)
  :config
  (global-flycheck-mode))

;; (require 'eglot)
;; (defvar eglot-connect-timeout)
;; (defvar eglot-server-programs)
;; (setq eglot-connect-timeout 300)
;; (setq eglot-put-doc-in-help-buffer (lambda (s) (> (length s) 250)))
;; (map-put! eglot-server-programs 'go-mode '("gopls"))
(defvar eglot-workspace-configuration)
(setq-default
 eglot-workspace-configuration
 '((:gopls . (:usePlaceholders t :staticcheck t :completeUnimported t))))
(defvar eglot-strict-mode)
(setq eglot-strict-mode nil)
(eval-after-load 'eglot (lambda ()
                          (bind-key (kbd "C-c C-h") 'eglot-help-at-point eglot-mode-map)))


(defun my-disable-aggressive-indent ()
  "Disable aggressive indent mode in current buffer."
  (interactive)
  (aggressive-indent-mode -1))

;; aggressive-indent-mode breaks interaction with lsp-server
(add-hook 'lsp-mode-hook #'my-disable-aggressive-indent)

(defun my-try-go-mod (dir)
  "Find go project root for DIR."
  (if (and dir
           (not (f-descendant-of-p dir (or (getenv "GOPATH")
                                           (concat (getenv "HOME") "/go")))))
      (let ((result (locate-dominating-file dir "go.mod")))
        (if result
            (cons 'transient (expand-file-name result))
          (cons 'transient dir)))
    (when dir
      (cons 'transient dir))))

(setq exec-path (append exec-path '("~/go/bin" "/opt/local/bin" "/usr/local/bin" "~/.cargo/bin" "/usr/local/opt/llvm/bin")))
(require 's)
(setenv "PATH" (s-join ":" exec-path))
(defvar-local my-go-packages nil)
(use-package go-mode
  :mode (("\\.go\\'" . go-mode)
         ("go.mod$" . go-dot-mod-mode))
  :functions (my-go-mode-hook go-goto-imports
                              godoc-at-point goimports)
  :defines (company-backends go-tag-args)
  :config
  (progn
    (setenv "GOPATH" (concat (getenv "HOME") "/go"))
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:" (getenv "GOPATH") "/bin"))

    (defun my-go-mode-hook ()
      "Setup for go."
      (if (eq system-type 'darwin)
	  (setenv "GOROOT"
                  (s-trim
                   (shell-command-to-string
                    "echo /usr/local/Cellar/go/`GOROOT='' go version | awk '{print $3}' | sed -e 's/go//g'`/libexec"))))
      (require 'go-impl)
      (use-package go-direx
        :config
        (defun my-kill-prev-buf (_)
          "Kill current buffer."
          (interactive)
          (kill-buffer (caar (window-prev-buffers))))
        (defun my-direx-hook()
          "My hook for direx."
          (interactive)
          (local-set-key (kbd "s") #'swiper-helm)
          (local-set-key (kbd "q") (lambda () (interactive)(kill-buffer (buffer-name))))
          (advice-add 'direx:find-item :after 'my-kill-prev-buf))
        (add-hook 'direx:direx-mode-hook 'my-direx-hook))
      (require 'gotest)
      (require 'go-playground)
      (defun my-go-test (arg)
        (interactive "P")
        (if arg
            (pcase (completing-read "go test " '("current project" "current directory"))
              ("current project"
               (let ((default-directory (project-root (project-current)))
                     (current-prefix-arg nil))
                 (go-test-current-project)))
              (_ (go-test-current-project)))
          (go-test-current-project)))
      (setq go-tag-args (list "-transform" "snakecase"))
      (add-hook 'before-save-hook #'gofmt-before-save)
      (local-set-key (kbd "C-c i") #'go-goto-imports)
      (local-set-key (kbd "C-c C-t") #'my-go-test)
      (local-set-key (kbd "C-c t") #'go-tag-add)
      (local-set-key (kbd "C-c T") #'go-tag-remove)
      (local-set-key (kbd "C-c g") #'go-gen-test-dwim)
      (local-set-key (kbd "C-c C-i") #'go-fill-struct)
      (local-set-key (kbd "M-i") #'go-direx-switch-to-buffer)
      (local-set-key (kbd "M-?") #'lsp-find-references)
      (local-set-key (kbd "C-c C-c") #'helm-make)
      (require 'lsp-mode)
      (lsp-register-custom-settings '(("gopls.completeUnimported" t)))
      (lsp-register-custom-settings '(("gopls.staticcheck" t)))
      (lsp-register-custom-settings '(("gopls.experimentalDisabledAnalyses" ["ST1000"])))
      ;; (lsp-deferred)
      ;; (require 'yasnippet)
      ;; (eglot-ensure)
      ;; (setq-local company-backends '(company-capf))

      (setq-local project-find-functions (list #'my-try-go-mod #'project-try-vc))
      (setq-local flymake-start-on-save-buffer nil)
      ;; (setq-local lsp-auto-guess-root t)
      ;; (setq lsp-ui-sideline-ignore-duplicate t)
      (require 'lsp-go)
      (lsp-deferred)
      (symbol-overlay-mode -1)

      (defun my-go-packages-go-list ()
        my-go-packages)
      (setq go-packages-function 'my-go-packages-go-list)
      (require 'cl-lib)
      (defun my-refresh-go-packages-list ()
        "Refresh go packages list."
        (if (string-equal "go-mode" major-mode)
            (let ((cur-buf (buffer-name)))
              (async-start (lambda ()
                             (process-lines "go" "list" "-e" "all"))
                           (lambda (res)
                             (with-current-buffer cur-buf
                               (setq my-go-packages
                                     (cl-mapcar
                                      (lambda (s)
                                        (replace-regexp-in-string
                                         "@[^/]*" ""
                                         (string-remove-prefix
                                          "mod/"
                                          (string-remove-prefix
                                           "vendor/"
                                           s))))
                                      (cl-remove-if
                                       (lambda (s) (string-prefix-p "warning:" s)) res)))))))))
      (my-refresh-go-packages-list)
      (add-hook 'after-save-hook #'my-refresh-go-packages-list))

    (add-hook 'go-mode-hook #'my-go-mode-hook)))

(global-set-key "\C-xs" #'save-buffer)
(global-set-key "\C-xv" #'quoted-insert)
(global-set-key "\C-xf" #'search-forward)
(global-set-key "\C-xc" #'compile)
(global-set-key "\C-xr" #'replace-string)
(global-set-key "\C-xa" #'repeat-complex-command)
(global-set-key "\C-xm" #'manual-entry)
(global-set-key "\C-xw" #'what-line)
(global-set-key "\C-x\C-u" #'shell)
(global-set-key "\C-x0" #'overwrite-mode)
(global-set-key "\C-x\C-r" #'read-only-mode)
(global-set-key "\C-t" #'kill-word)
(global-set-key "\C-p" #'previous-line)
(global-set-key "\C-o" #'forward-word)
(global-set-key "\C-x\C-m" #'not-modified)
(setq make-backup-files 'nil)
(setq text-mode-hook 'turn-on-auto-fill)
(setq auto-mode-alist (cons '("\\.cxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hpp$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))

                                        ; http://nex-3.com/posts/45-efficient-window-switching-in-emacs#comments
;; (global-set-key [M-left] #'windmove-left)          ; move to left windnow
;; (global-set-key [M-right] #'windmove-right)        ; move to right window
;; (global-set-key [M-up] #'windmove-up)              ; move to upper window
;; (global-set-key [M-down] #'windmove-down)          ; move to downer window

(put 'downcase-region 'disabled nil)

(use-package octave
  :disabled t
  :mode ("\\.m$" . octave-mode)
  :defines (octave-mode-map)
  :functions (octave-mode)
  :init
  (defun my-octave-hook ()
    "My hook for octave."
    (interactive)
    (local-set-key (kbd "C-c C-z") #'run-octave))
  (add-hook 'octave-mode-hook 'my-octave-hook))

;;; Auto-complete
;; (require 'company)
(add-hook 'after-init-hook #'global-company-mode)
(defvar company-etags-ignore-case)
(setq company-etags-ignore-case nil)
(defvar company-dabbrev-ignore-case)
(setq company-dabbrev-ignore-case nil)
(defvar company-dabbrev-code-ignore-case)
(setq company-dabbrev-code-ignore-case nil)
(defvar company-tooltip-limit)
(setq company-tooltip-limit 20)                      ; bigger popup window
(defvar company-idle-delay)
(setq company-idle-delay 0.1)                         ; decrease delay before autocompletion popup shows
(defvar company-echo-delay)
(setq company-echo-delay 0)                          ; remove annoying blinking
(defvar company-minimum-prefix-length)
(setq company-minimum-prefix-length 3)

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

;;; ElDoc
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)


;;; Commenting
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.  ARG is selected region.
If no region is selected and current line is not blank and we are not at
the end of the line, then comment current line.  Replaces default behaviour of
`comment-dwim', when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" #'comment-dwim-line)

(use-package browse-url
  :functions (browse-url-default-browser))
(setq browse-url-browser-function #'browse-url-default-browser)

(kill-buffer "*Messages*")

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook #'delete-other-windows)

(use-package pkgbuild-mode
  :if (file-exists-p "/etc/pacman.conf")
  :functions (pkgbuild-mode)
  :mode ("/PKGBUILD$" . pkgbuild-mode))

(use-package poly-markdown
  :defer t
  :commands (poly-markdown-mode poly-gfm-mode)
  :mode (("\\.text\\'" . poly-markdown-mode)
         ("\\.md$" . poly-gfm-mode)
         ("\\.markdown$" . poly-markdown-mode)))

(use-package vmd-mode
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("p" . vmd-mode))
  :init
  (setq vmd-mode--emojis-file (expand-file-name "~/.emacs.d/.github-emojis"))

  (if (not (file-exists-p vmd-mode--emojis-file))
      (vmd-mode--update-emojis-file))

  (setq vmd-mode-github-emojis-list
        (and (file-exists-p vmd-mode--emojis-file)
             (with-temp-buffer
               (insert-file-contents vmd-mode--emojis-file)
               (split-string (buffer-string) "\n" t))))
  
  (defun my-github-emojis-complete-at-point ()
    "My function for complete github emoji at point."
    (let ((start (or (car (bounds-of-thing-at-point 'symbol)) (point))))
      (if (char-equal (char-before start) ?:)
          (list start
                (point)
                vmd-mode-github-emojis-list
                :exit-function (lambda (_candidate _status)
                                 (insert ":")))
        nil)))

  (defun my-enable-emojis-completion ()
    "Enable completion github emojis."
    (cl-pushnew 'my-github-emojis-complete-at-point completion-at-point-functions))

  (defun my-bind-md-preview-key ()
    "Rebind markdown preview."
    (define-key markdown-mode-command-map (kbd "p") 'vmd-mode))

  (add-hook 'markdown-mode-hook #'my-enable-emojis-completion)
  (add-hook 'markdown-mode-hook #'my-bind-md-preview-key))

;; for over-80-chars line highlightning
;; (add-hook 'prog-mode-hook 'column-enforce-mode)
;; (require 'fill-column-indicator)
(use-package fill-column-indicator
  :disabled t
  :after company
  :defer 0.1
  :config
  (progn
    (defun my-disable-fci () "Disable fci mode." (fci-mode -1))

    (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
    (set 'fci-rule-column 80)
    (global-fci-mode 1)
    ;; Disable fci if needed.
    (defvar company-fci-mode-on-p)
    (defun disable-fci (&rest ignore)
      "Disable fci with IGNORE arg."
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))
    (add-hook 'company-completion-started-hook #'disable-fci)
    ;; Re-enable fci if needed.
    (defun reenable-fci (&rest ignore)
      "Reenable fci with IGNORE arg."
      (when company-fci-mode-on-p (fci-mode 1)))
    (add-hook 'company-completion-finished-hook #'reenable-fci)
    ;; Re-enable fci if needed.
    (add-hook 'company-completion-cancelled-hook #'reenable-fci)

    ;; fix for infinite eating RAM
    (add-hook 'rjsx-mode-hook #'my-disable-fci)))

(defvar company-tooltip-align-annotations)
(setq company-tooltip-align-annotations t)

(use-package emmet-mode
  :disabled t
  :functions (emmet-mode)
  :init
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'rjsx-mode #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t))

(use-package avy
  :chords (("fj" . avy-goto-word-1)
           ("f'" . avy-pop-mark))
  :config
  (define-key isearch-mode-map (kbd "C-'") #'avy-isearch))


;;
;; expand region
;;
(use-package expand-region
  :chords (("zj" . er/expand-region)
           ("zk" . er/contract-region)))
(delete-selection-mode)

(use-package multiple-cursors
  :defer 3
  :chords (("mf" . multiple-cursors-hydra/body))
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

(declare-function check-expansion "ext:config")
(declare-function company-complete-common "ext:company")
(declare-function tab-indent-or-complete "ext:config")
(use-package yasnippet
  :functions (tab-indent-or-complete)
  :bind (([tab] . tab-indent-or-complete)
         ("TAB" . tab-indent-or-complete))
  :defer 3
  :config
  (progn
    (yas-global-mode 1)
    (defun check-expansion ()
      "Check yasnippet expansion."
      (save-excursion
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil)))))

    (defvar yas-minor-mode)
    (defun tab-indent-or-complete ()
      "Smart tab function."
      (interactive)
      (if (minibufferp)
          (minibuffer-complete)
        (if (or (not yas-minor-mode)
                (null (yas-expand)))
            (if (check-expansion)
                (company-complete-common)
              (indent-for-tab-command)))))))

;;for faster toggle key-chord-mode
(global-set-key [f9] #'key-chord-mode)

(defvar x-hyper-keysym)
(setq x-hyper-keysym 'meta)

(use-package ivy
  :disabled t
  :bind* ("C-c s k" . ivy-resume)
  :defines (ivy-completion-beg ivy-completion-end)
  :functions (ivy-completion-in-region-action)
  :config
  (progn
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-re-builders-alist
          '((counsel-M-x . ivy--regex-fuzzy)
            (swiper . ivy--regex-plus)
            (t . ivy--regex-fuzzy)))))

(use-package swiper
  :disabled t
  :defer t)

(use-package helm-files
  :functions (helm-find-files-up-one-level))

(use-package helm
  :defines (helm-grep-ag-command
            helm-read-file-map
            helm-find-files-map)
  :functions (helm-ido-like-higher-gc
              helm-ido-like-lower-gc
              helm-ido-like-find-files-navigate-forward
              helm-ido-like-load-file-nav)
  :bind*
  (("C-x l" . helm-locate)
   ("C-x b" . helm-mini)
   ;; ("C-c C-s" . my-helm-rg-repo)
   )
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)
   ("M-y". helm-show-kill-ring)
   :map helm-map
   ([tab] . helm-select-action))
  :init
  (progn
    (setq helm-source-grep (helm-build-dummy-source "init_grep" :follow 1))
    (add-hook 'helm-before-initialize-hook
              (lambda () (helm-attrset 'follow 1 helm-source-grep))))
  :config
  (use-package ace-jump-helm-line
    :bind
    (:map helm-map
          ("C-'" . ace-jump-helm-line)))
  (require 'helm-config)
  (helm-mode +1)
  (defvar helm-ido-like-user-gc-setting nil)

  (defun helm-ido-like-higher-gc ()
    (setq helm-ido-like-user-gc-setting gc-cons-threshold)
    (setq gc-cons-threshold most-positive-fixnum))

  (defun helm-ido-like-lower-gc ()
    (setq gc-cons-threshold helm-ido-like-user-gc-setting))

  (defun helm-ido-like-helm-make-source (f &rest args)
    (let ((source-type (cadr args)))
      (unless (or (memq source-type '(helm-source-async helm-source-ffiles helm-grep-ag-class helm-grep-class helm-mac-spotlight-source helm-moccur-class))
                  (eq (plist-get args :filtered-candidate-transformer)
                      'helm-ff-sort-candidates)
                  (eq (plist-get args :persistent-action)
                      'helm-find-files-persistent-action))
        (nconc args '(:fuzzy-match t))))
    (apply f args))

  (defun helm-ido-like-load-fuzzy-enhancements ()
    (add-hook 'minibuffer-setup-hook #'helm-ido-like-higher-gc)
    (add-hook 'minibuffer-exit-hook #'helm-ido-like-lower-gc)
    (advice-add 'helm-make-source :around 'helm-ido-like-helm-make-source))
  
  (setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number -M 170 --max-columns-preview %s %s %s")
  (defun helm-ido-like-find-files-up-one-level-maybe ()
    (interactive)
    (if (looking-back "/" 1)
        (helm-find-files-up-one-level 1)
      (delete-char -1)))

  (defun helm-ido-like-load-file-nav ()
    (with-eval-after-load 'helm-files
      (define-key helm-read-file-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)
      (define-key helm-find-files-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)))

  (defun my--helm-do-grep-ag-repo (arg)
    "My grepping implementation."
    (message "helm-grep-ag-pipe-cmd-switches: %s" helm-grep-ag-pipe-cmd-switches)
    (require 'helm-files)
    (helm-grep-ag (expand-file-name (if (vc-root-dir)
                                        (vc-root-dir)
                                      default-directory)) arg))

  (defun my--helm-do-grep-ag-project (arg)
    "My grepping implementation."
    (message "helm-grep-ag-pipe-cmd-switches: %s" helm-grep-ag-pipe-cmd-switches)
    (require 'helm-files)
    (helm-grep-ag (expand-file-name (if (project-current)
                                        (car (project-roots (project-current)))
                                      default-directory)) arg))


  (defun my-helm-rg-repo (arg)
    "Preconfigured helm for grepping with AG in `default-directory'.
With prefix-arg prompt for type if available with your AG version."
    (interactive "P")
    (if arg
        (progn
          (require 's)
          (unwind-protect
              (progn
                (setq
                 helm-grep-ag-command (s-concat helm-grep-ag-command " -u"))
                (my--helm-do-grep-ag-repo arg))
            (setq
             helm-grep-ag-command (s-chop-suffix " -u" helm-grep-ag-command))))
      (my--helm-do-grep-ag-repo arg)))

  (defun my-helm-rg-project (arg)
    "Preconfigured helm for grepping with AG in `default-directory'.
With prefix-arg prompt for type if available with your AG version."
    (interactive "P")
    (if arg
        (progn
          (require 's)
          (unwind-protect
              (progn
                (setq
                 helm-grep-ag-command (s-concat helm-grep-ag-command " -u"))
                (my--helm-do-grep-ag-project arg))
            (setq
             helm-grep-ag-command (s-chop-suffix " -u" helm-grep-ag-command))))
      (my--helm-do-grep-ag-project arg)))

  (helm-ido-like-load-fuzzy-enhancements)
  (helm-ido-like-load-file-nav))

(use-package helm-fd
  :bind* ("C-x C-p" . helm-fd-project))

(use-package fzf
  :bind* ("C-c C-f" . my-fzf-project)
  :config
  (defun my-fzf-project ()
    "Fzf in project directory."
    (interactive)
    (fzf/start (or (if (project-current)
                       (car (project-roots (project-current))))
                   default-directory))))

(use-package wgrep
  :bind ("C-c C-p" . wgrep-change-to-wgrep-mode)
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

;;
;; keyboard selection
;;
(setq select-enable-primary t)
(setq select-enable-clipboard t)
;; (when (getenv "DISPLAY")
;;   (defun xclip-cut-function (text &optional _push)
;;     (with-temp-buffer
;;       (insert text)
;;       (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))
;;   (defun xclip-paste-function()
;;     (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
;;       (unless (string= (car kill-ring) xclip-output)
;;         xclip-output )))
;;   (setq interprogram-cut-function 'xclip-cut-function)
;;   (setq interprogram-paste-function 'xclip-paste-function))

(require 'mouse)
(xterm-mouse-mode t)
(setq mouse-drag-copy-region t)
(global-set-key [drag-mouse-0]	'mouse-set-region)

(use-package imenu
  :defer t
  :bind ("M-i" . imenu))

;; magit
(use-package exec-path-from-shell
  :disabled t
  :if (memq window-system '(mac ns))
  :defer 15
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package
  magit
  :defines (auto-revert-check-vc-info)
  :functions (my-magit-diff-hook my-magit-find-file-other-frame my-magit-find-file)
  :defer t
  :bind ("C-x C-j" . my-magit-find-file-other-frame)
  :bind* ("C-x g" . magit-status)
  :init
  (if (eq system-type 'darwin)
      (setq magit-git-executable "/opt/local/bin/git"))
  :config
  (progn
    (defun my-magit-diff-hook ()
      "My hook for improve magit diff."
      (local-set-key (kbd "h") #'diff-refine-hunk))
    (add-hook 'magit-diff-mode-hook #'my-magit-diff-hook)
    (setq auto-revert-check-vc-info t)
    (defun my-magit-find-file-other-frame (file)
      "View FILE from worktree, in another frame.
Switch to a buffer visiting blob FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location."
      (interactive (my-magit-find-file-read-args "Find file in other frame"))
      (find-file-other-frame (f-join (vc-root-dir) file)))

    (defun my-magit-find-file (file)
      "View FILE from worktree.
Switch to a buffer visiting blob FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location."
      (interactive (my-magit-find-file-read-args "Find file"))
      (find-file (f-join (vc-root-dir) file)))


    (defun my-magit-find-file-read-args (prompt)
      (list (magit-read-file-from-rev "HEAD" prompt)))))

(use-package diff-mode
  :functions
  (diff-refine-hunk))

;; org-mode
;; (define-key global-map "\C-cl" 'org-store-link)
;; ;; (define-key global-map "\C-ca" 'org-agenda)
;; (defvar org-log-done)
;; (setq org-log-done t)
;; ;; ditaa
;; (defun my-org-hook ()
;;   "My hook for `org-mode'."
;;   (require 'org-mind-map)
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((ditaa . t) (dot . t))))
;; (add-hook 'org-mode-hook #'my-org-hook)


(use-package edit-indirect
  :chords ((";r" . edit-indirect-region)))

;; pandoc
;; (require 'pandoc-mode)
(add-hook 'markdown-mode-hook #'pandoc-mode)
(declare-function pandoc-load-default-settings "ext:pandoc")
(add-hook 'pandoc-mode-hook #'pandoc-load-default-settings)

(use-package erlang
  :disabled t
  :functions
  (my-erlang-hook my-format-erlang-record)
  :mode (("\\.erl$" . erlang-mode)
         ("\\.hrl$" . erlang-mode)
         ("rebar\\.config$" . erlang-mode)
         ("relx\\.config$" . erlang-mode)
         ("system\\.config$" . erlang-mode)
         ("\\.app\\.src$" . erlang-mode))
  :defines (erlang-extended-mode-map)
  :init
  (add-hook 'erlang-mode-hook #'my-erlang-hook)
  (add-hook 'erlang-mode-hook #'company-erlang-init)
  :config
  (progn
    (use-package company-erlang
      :defer t
      :init
      (load "company-erlang-autoloads"))

    (use-package ivy-erlang-complete
      :defer t)

    (add-to-list 'load-path "/usr/lib/erlang/lib/wrangler-1.2.0/elisp")
    (defun my-format-erlang-record ()
      "Format erlang record."
      (interactive)
      (let ((from (line-beginning-position)))
        (goto-char from)
        (search-forward "-record" )
        (search-forward "{")
        (goto-char (- (point) 1))
        (ignore-errors (er/expand-region 1))
        (my-align-region-by "=")
        (goto-char from)
        (search-forward "-record" )
        (search-forward "{")
        (goto-char (- (point) 1))
        (ignore-errors (er/expand-region 1))
        (my-align-region-by "::")))
    (defun my-erlang-hook ()
      "Setup for erlang."
      (ignore-errors (require 'wrangler))
      (ivy-erlang-complete-init)
      (define-key erlang-extended-mode-map (kbd "M-.") nil)
      (define-key erlang-extended-mode-map (kbd "M-,") nil)
      (define-key erlang-extended-mode-map (kbd "M-?") nil)
      (define-key erlang-extended-mode-map (kbd "(") nil)
      (define-key erlang-extended-mode-map (kbd "C-M-i") nil)
      (local-set-key (kbd "C-c C-p") #'my-format-erlang-record)
      (local-set-key (kbd "C-M-i") #'ivy-erlang-complete))

    (add-hook 'after-save-hook #'ivy-erlang-complete-reparse)))

;; fast open url
(global-set-key (kbd "C-x u") #'link-hint-open-multiple-links)

;; nXML mode customization
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))

;; hungry deletion
(use-package hungry-delete
  :defer 0.5
  :config
  (global-hungry-delete-mode))

(show-paren-mode 1)

(electric-pair-mode 1)
;; (add-hook 'prog-mode-hook #'electric-spacing-mode)
;; (add-hook 'emacs-lisp-mode-hook #'electric-spacing-mode)

;;; Ace link
(use-package ace-link
  :defer 0.1
  :config
  (ace-link-setup-default))

;;; Ace window
(use-package ace-window
  :after markdown-mode
  :bind (("M-p" . ace-window)
         :map markdown-mode-map
         ("M-p" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;; Which key
(use-package which-key
  :functions (which-key-mode)
  :init
  (add-hook 'after-init-hook #'which-key-mode))

;;; embrace
(global-set-key (kbd "C-,") #'embrace-commander)

(use-package composable
  :defer 0.1
  :config
  (progn
    (composable-mode)
    (composable-mark-mode)))

(use-package ibuffer-vc
  :defer 3
  :config (add-hook 'ibuffer-hook
                    (lambda ()
                      (ibuffer-vc-set-filter-groups-by-vc-root)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))))
  :bind* ("C-x C-b" . ibuffer))

(use-package symbol-overlay
  :defer 2
  :config
  (setq-default symbol-overlay-temp-in-scope t)
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'emacs-lisp-mode-hook #'symbol-overlay-mode))

;; auto configure indent with SMIE
(use-package smie
  :functions (smie-auto-guess)
  :init
  (add-hook 'prog-mode-hook 'smie-auto-guess)
  :config
  (defun smie-auto-guess ()
    "Autoindentation with SMIE."
    (when (featurep 'smie)
      (unless (eq smie-grammar 'unset)
        (smie-config-guess)))))

(use-package zygospore
  :bind
  ("C-x 1" . zygospore-toggle-delete-other-windows))

(declare-function reverse-im-activate "ext:reverse-im")
(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

;; (use-package notmuch
;;   :disabled t
;;   :functions (notmuch-tag-completions notmuch-logged-error)
;;   :defines (notmuch-hello-thousands-separator
;;             notmuch-command
;;             notmuch-search-oldest-first
;;             notmuch-hello-sections
;;             notmuch-saved-searches)
;;   :commands notmuch)

;; (use-package notmuch-hello
;;   :disabled t
;;   :functions (my-count-query
;;               my-notmuch-hello-query-insert
;;               notmuch-hello-nice-number
;;               notmuch-hello-widget-search
;;               my-gen-notmuch-ss
;;               my-notmuch-update-ss)
;;   :after notmuch
;;   :config
;;   (defun my-gen-notmuch-ss (tag)
;;     (let ((key (downcase (or (and (let ((case-fold-search nil))
;;                                     (string-match "[[:upper:]]" tag))
;;                                   (match-string 0 tag))
;;                              (substring tag 0 1)))))
;;       (list :key key :name tag :query (format "tag:%s" tag)
;;             :search-type 'tree :sort-order 'newest-first)))
;;   (defun my-notmuch-update-ss ()
;;     (setq notmuch-saved-searches
;;           (cons '(:key "e" :name "licEnse" :query "to:sergey.kostyaev@eltex.loc and lic"
;;                        :search-type 'tree :sort-order 'newest-first)
;;                 (seq-map #'my-gen-notmuch-ss (notmuch-tag-completions)))))
;;   (my-notmuch-update-ss)
;;   (advice-add 'notmuch-jump-search :before 'my-notmuch-update-ss)
;;   (setq notmuch-hello-thousands-separator ".")

;;   (defun my-count-query (query)
;;     (with-temp-buffer
;;       (insert query "\n")
;;       (unless (= (call-process-region (point-min) (point-max) notmuch-command
;;                                       t t nil "count" "--batch") 0)
;;         (notmuch-logged-error "Command \"notmuch count --batch\" failed"
;;                               "Please check that the notmuch CLI is new enough to support `count
;; --batch'. In general we recommend running matching versions of
;; the CLI and emacs interface."))

;;       (goto-char (point-min))
;;       (let ((n (read (current-buffer))))
;;         (if (= n 0)
;;             nil
;;           (notmuch-hello-nice-number n)))))

;;   (defun my-notmuch-hello-query-insert (cnt query elem)
;;     (if cnt
;;         (let* ((str (format "%s" cnt))
;;                (widget-push-button-prefix "")
;;                (widget-push-button-suffix "")
;;                (oldest-first (case (plist-get elem :sort-order)
;;                                (newest-first nil)
;;                                (oldest-first t)
;;                                (otherwise notmuch-search-oldest-first))))
;;           (widget-create 'push-button
;;                          :notify #'notmuch-hello-widget-search
;;                          :notmuch-search-terms query
;;                          :notmuch-search-oldest-first oldest-first
;;                          :notmuch-search-type 'tree
;;                          str)
;;           (widget-insert (make-string (- 8 (length str)) ? )))
;;       (widget-insert "        ")))

;;   (defun my-notmuch-hello-insert-searches ()
;;     "Insert the saved-searches section."
;;     (widget-insert (propertize "New     Total      Key  List\n" 'face 'my-notmuch-hello-header-face))
;;     (mapc (lambda (elem)
;;             (when elem
;;               (let* ((q_tot (plist-get elem :query))
;;                      (q_new (concat q_tot " AND tag:unread"))
;;                      (n_tot (my-count-query q_tot))
;;                      (n_new (my-count-query q_new)))
;;                 (my-notmuch-hello-query-insert n_new q_new elem)
;;                 (my-notmuch-hello-query-insert n_tot q_tot elem)
;;                 (widget-insert "   ")
;;                 (widget-insert (plist-get elem :key))
;;                 (widget-insert "    ")
;;                 (widget-insert (plist-get elem :name))
;;                 (widget-insert "\n")
;;                 ))
;;             )
;;           notmuch-saved-searches))

;;   (setq notmuch-hello-sections '(notmuch-hello-insert-header my-notmuch-hello-insert-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)))

;;; Prose linting
(use-package flycheck-vale
  :disabled t
  :after flycheck
  :config (flycheck-vale-setup))

(use-package aggressive-indent
  :demand t
  :config
  (aggressive-indent-global-mode))

(use-package password-store
  :commands (password-store-get))

(use-package pass
  :commands (pass))

(eval-after-load 'dash '(dash-enable-font-lock))

(use-package rg
  :after wgrep
  :config
  (setq rg-group-result t)
  (setq rg-hide-command t)
  (setq rg-show-columns nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")

  (rg-define-search my-grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :case-fold-search smart
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun my-rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.

This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  (defun my-next-window (_)
    (other-window 1))

  (advice-add 'my-grep-vc-or-dir :after 'my-next-window)

  :bind* (("C-c C-s" . my-grep-vc-or-dir)
          :map rg-mode-map
          ("f" . next-error-follow-minor-mode)
          ("s" . my-rg-save-search-as-name)
          ("n" . next-line)
          ("p" . previous-line)
          ("C-n" . next-line)
          ("C-p" . previous-line)
          ("M-n" . rg-next-file)
          ("M-p" . rg-prev-file)))


(use-package counsel
  :disabled t
  :bind (("C-c g" . my-counsel-git-grep)
         :map isearch-mode-map
         ("M-i" . swiper-from-isearch))
  :bind*
  (("C-x l" . counsel-locate))
  :functions (swiper-from-isearch)
  :config
  (defun swiper-from-isearch ()
    "Invoke `swiper' from isearch."
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-exit)
      (counsel-grep-or-swiper query)))
  (defun my-counsel-git-grep ()
    "My git grep."
    (interactive)
    (counsel-git-grep
     nil
     (let ((symb (symbol-at-point)))
       (if symb
           (prin1-to-string symb)
         ""))))
  (setq ivy-height 20))

(use-package ace-isearch
  :bind (:map isearch-mode-map
              ("M-n" . my-isearch-next)
              ("M-i" . swiper-helm-from-isearch))
  :init
  (defun my-isearch-next ()
    "Isearch symbol at point or next isearch history item."
    (interactive)
    (isearch-yank-string (format "%s" (or (symbol-at-point) ""))))
  (global-ace-isearch-mode +1)
  :config
  (setq ace-isearch-function 'avy-goto-word-1)
  (setq ace-isearch-function-from-isearch 'swiper-helm-from-isearch)
  (setq ace-isearch-use-jump nil))

(use-package swiper-helm
  :functions (swiper-helm-from-isearch))

;;; plantuml
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((plantuml . t)))
;; (defvar org-plantuml-jar-path)
;; (setq org-plantuml-jar-path
;;       (expand-file-name "~/.emacs.d/plantuml/plantuml.jar"))

(put 'upcase-region 'disabled nil)

;; (defun my-format-decimal ()
;;   "Replace java decimals to regular floats."
;;   (interactive)
;;   (goto-char (point-min))
;;   (while (search-forward-regexp "[0-9.]+E[+-][0-9]+" (point-max) t)
;;     (shell-command-on-region
;;      (match-beginning 0) (match-end 0)
;;      (concat "LC_NUMERIC=C printf \"%'.3f\" " (match-string 0)) (buffer-file-name) t)))

;; (defun my-toggle-camelcase-underscores ()
;;   "Toggle between camelcase and underscore notation for the symbol at point."
;;   (interactive)
;;   (save-excursion
;;     (ignore-errors
;;       (let* ((bounds (bounds-of-thing-at-point 'symbol))
;;              (start (car bounds))
;;              (end (cdr bounds))
;;              (currently-using-underscores-p (progn (goto-char start)
;;                                                    (re-search-forward "_" end t))))
;;         (if currently-using-underscores-p
;;             (let ((res (mapconcat 'capitalize (split-string (buffer-substring-no-properties start end) "_") "")))
;;               (progn
;;                 (kill-region start end)
;;                 (insert res)))
;;           (funcall-interactively 'replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
;;           (downcase-region start (cdr (bounds-of-thing-at-point 'symbol))))))))

(use-package comment-tags
  :config
  (add-hook 'prog-mode-hook #'comment-tags-mode))

(declare-function eww-reload "ext:eww")
(defun eww-more-readable ()
  "Better eww.  Run it after eww buffer is loaded."
  (interactive)
  ;; (setq eww-header-line-format nil)               ;; removes page title
  (set-window-margins (get-buffer-window) 20 20)  ;; increases size of margins
  (eww-reload 'local))
(add-hook 'eww-after-render-hook #'eww-more-readable)

(use-package helm-codesearch
  :disabled t
  :bind (("C-c h f" . helm-codesearch-find-file)
         ("C-c h s" . helm-codesearch-find-pattern)
         ("C-c h c" . helm-codesearch-create-csearchindex))
  :config
  (setq helm-codesearch-global-csearchindex (concat (getenv "HOME") "/.csearchindex")))

;; helm charts support
(defun my-disable-auto-fill ()
  "Disable `auto-fill-mode'."
  (auto-fill-mode -1))
(defun my-enable-prism ()
  "Enable `prism-mode'."
  (prism-mode 1))
(require 'company-dabbrev-code)
(add-to-list 'company-dabbrev-code-modes 'yaml-mode)
(add-to-list 'company-dabbrev-code-modes 'protobuf-mode)
(add-hook 'yaml-mode-hook #'highlight-indentation-mode)
(add-hook 'yaml-mode-hook #'highlight-indentation-current-column-mode)
(add-hook 'yaml-mode-hook #'my-disable-auto-fill)
(add-hook 'yaml-mode-hook #'my-enable-prism)
(global-smart-shift-mode 1)
(key-chord-define-global "<<" 'smart-shift-left)
(key-chord-define-global ">>" 'smart-shift-right)

;; auto-yasnippet
(use-package auto-yasnippet
  :bind (("C-c y" . aya-create)
         ("C-." . aya-expand))
  :config
  (setq aya-field-regex "\\sw\\|\\s_\\|\\*\\|\\&"))

(global-set-key "\C-cff" 'toggle-frame-fullscreen)


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
(global-set-key (kbd "C-c C-r") 'open-this-file-as-root)

(use-package helm-make
  :config
  (setq helm-make-directory-functions-list
        '(helm-make-project-directory helm-make-current-directory)))

(use-package lsp-mode
  :defer t
  :config
  (require 'yasnippet))

(add-hook 'dart-mode-hook #'lsp)

(use-package reason-mode
  :disabled t
  :config
  (add-hook 'before-save-hook #'refmt-before-save)
  (add-hook 'reason-mode-hook #'lsp))

;; (magit-todos-mode 1)

(defun my-go-home()
  "Go to home dir."
  (cd (getenv "HOME")))

(add-hook 'after-init-hook #'my-go-home)

(if (package-installed-p 'libgit)
    (require 'libgit))

(if (>= emacs-major-version 27)
    (add-hook 'after-init-hook #'global-so-long-mode))

(global-set-key (kbd "C-;") #'hippie-expand)

(add-hook 'company-mode-hook #'company-prescient-mode)

(add-hook 'prog-mode-hook #'dtrt-indent-mode)

;;; Screencasts

(use-package gif-screencast
  :bind (("<f7>" . gif-screencast-toggle-pause)
         ("<f8>" . gif-screencast-start-or-stop))
  :config
  (setq gif-screencast-args '("-x" "-o")) ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
  (setq gif-screencast-cropping-program "mogrify") ;; Optional: Used to crop the capture to the Emacs frame.
  (setq gif-screencast-capture-format "ppm") ;; Optional: Required to crop captured images.
  ;; HiDPI display patch
  (defun my-fix-screencast-hidpi (oldfun &rest r)
    (apply #'format "%dx%d+%d+%d"
           (mapcar
            (lambda (x) (* 2 (string-to-number x)))
            (split-string (apply oldfun r) "[+x]"))))
  (advice-add
   #'gif-screencast--cropping-region
   :around
   #'my-fix-screencast-hidpi))

(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

(setq completion-styles `(basic partial-completion emacs22 initials
                                ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))
(setq completion-ignore-case t)
(setq yas-inhibit-overlay-modification-protection t)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(defun my-hash-intersection-dedup (l1 l2)
  "Fast hash intersection between L1 and L2."
  (let ((ht (make-hash-table :test #'equal))
        (acc nil))
    (dolist (l l1)
      (puthash l t ht))
    (dolist (l l2)
      (when (gethash l ht nil)
        (puthash l nil ht)
        (push l acc)))
    acc))

(use-package string-inflection
  :bind ("C-c C-u" . #'string-inflection-all-cycle))

(add-hook 'rust-mode-hook #'lsp-deferred)
(setq rust-format-on-save t)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(defun my-get-env-var-from-shell (variable)
  "Get `VARIABLE' from shell and set it inside Emacs."
  (setenv variable
	  (string-trim
	   (shell-command-to-string
	    (format "zsh -c 'source ~/.profile; echo $%s'" variable)))))

(defun my-recompile-installed-packages ()
  "Native recompile installed packages."
  (interactive)
  (if (>= emacs-major-version 28)
      (progn (my-get-env-var-from-shell "LD_LIBRARY_PATH")
             (my-get-env-var-from-shell "LIBRARY_PATH")
             (native-compile-async "~/.emacs.d/elpa" t))))

(defun my-enable-tree-sitter ()
  "Enable tree-sitter."
  (require 'tree-sitter-langs)
  (tree-sitter-hl-mode))

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'my-enable-tree-sitter)

(global-set-key (kbd "C-c C-e") #'comby)

(provide 'init)
;;; init.el ends here
