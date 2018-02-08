;;; init.el --- Emacs init file. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq gc-cons-threshold (* 80 1024 1024))
(setq gc-cons-percentage 0.5)

(defvar network-security-level)
(defvar tls-program)
(defvar gnutls-verify-error)
(defvar gnutls-trustfiles)
(if (> emacs-major-version 24)
    (progn
      (setq network-security-level 'high)
      (setq gnutls-verify-error t))
  (let ((trustfile
         (replace-regexp-in-string
          "\\\\" "/"
          (replace-regexp-in-string
           "\n" ""
           (shell-command-to-string "python -m certifi")))))
    (setq tls-program
          (list
           (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                   (if (eq window-system 'w32) ".exe" "") trustfile)))
    (setq gnutls-verify-error t)
    (setq gnutls-trustfiles (list trustfile))))
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
;; (mapc #'(lambda (add) (add-to-list 'load-path add))
;;       (eval-when-compile
;;         (require 'package)
;;         (package-initialize)
;;         ;; Install use-package if not installed yet.
;;         (unless (package-installed-p 'use-package)
;;           (package-refresh-contents)
;;           (package-install 'use-package))
;;         ;; (require 'use-package)
;;         (let ((package-user-dir-real (file-truename package-user-dir)))
;;           ;; The reverse is necessary, because outside we mapc
;;           ;; add-to-list element-by-element, which reverses.
;;           (nreverse (apply #'nconc
;;                            ;; Only keep package.el provided loadpaths.
;;                            (mapcar #'(lambda (path)
;;                                        (if (string-prefix-p package-user-dir-real path)
;;                                            (list path)
;;                                          nil))
;;                                    load-path))))))

;; (require 'package)
;; (package-initialize)
(require 'package)
(package-initialize)

(global-set-key (kbd "C-M-r") #'(lambda () (interactive)
                                  (byte-recompile-file "~/.emacs.d/init.el" t 0 t)))

(eval-when-compile
  (require 'package)
  (if (not (package-installed-p 'async))
      (progn
        (package-refresh-contents)
        (package-install 'async))))

(setq custom-file "~/.emacs.d/emacs-customizations.el")

(defun my-set-themes-hook ()
  "Hook for setting themes after init."
  (interactive)
  ;; (load-theme 'spacemacs-dark t)
  ;; (require 'solarized)
  ;; (load-theme 'solarized-light t)
  ;; (require 'spacemacs-light-theme)
  ;; (load-theme 'spacemacs-light t)
  (require 'circadian)
  (circadian-activate-latest-theme)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  ;; (seq-doseq (frame (frame-list)) (my-solarized-dark-workaround frame))
  )

(eval-when-compile
  (defun my-bootstrap ()
    "Async install all needed packages."
    (interactive)
    (require 'async)
    (async-start
     (lambda ()
       ;; Melpa
       (require 'package)
       (setq custom-file "~/.emacs.d/emacs-customizations.el")
       (package-initialize)
       (ignore-errors (load custom-file 'noerror))
       (require 'cl-lib)
       (cl-flet ((always-yes (&rest _) t))
         (defun no-confirm (fun &rest args)
           "Apply FUN to ARGS, skipping user confirmations."
           (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
                     ((symbol-function 'yes-or-no-p) #'always-yes))
             (apply fun args)))
         (no-confirm 'package-refresh-contents)
         (no-confirm 'package-install-selected-packages)))
     (lambda (res)
       (seq-do #'load-file
               (split-string
                (shell-command-to-string
                 "find ~/.emacs.d/elpa -name '*autoloads.el' -newermt $(date +%Y-%m-%d -d '1 hour ago')") "\n" t))
       (seq-do (lambda (filename)
                 (let* ((pac (file-name-base filename))
                        (pac-sym (intern pac)))
                   (if (featurep pac-sym)
                       (progn
                         (message "reloading %s" pac)
                         (load-file filename)))))
               (split-string
                (shell-command-to-string
                 "find ~/.emacs.d/elpa -name '*.elc' -newermt $(date +%Y-%m-%d -d '1 hour ago')") "\n" t))
       (if (featurep 'yasnippet)
           (yas-reload-all))
       (my-set-themes-hook)
       (message "packages bootstrap success: %s" res))))

  (my-bootstrap))

(eval-when-compile
  (require 'use-package)
  (require 'use-package-chords))
(require 'bind-key)

(use-package key-chord
  :defer 2
  :config
  (key-chord-mode 1))

(use-package color-theme
  :defer 0.1
  :config
  (progn
    (my-set-themes-hook)))

;; (defun my-solarized-dark-workaround (frame)
;;   "Fix solarized-dark theme for terminal FRAME."
;;   (with-selected-frame frame
;;     (if (and (featurep 'color-theme)
;;              (not window-system))
;;         (set-face-background 'default "none" frame))))

;; (defun my-solarized-dark-on-focus ()
;;   "Fix solarized-dark theme for terminal on focus."
;;   (my-solarized-dark-workaround (selected-frame)))

;; (add-hook 'focus-in-hook #'my-solarized-dark-on-focus)

;; (add-hook 'after-make-frame-functions #'my-solarized-dark-workaround)

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
                         (let ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode)))
                           (setq vc-mode
                                 (propertize vc-mode
                                             'face  (cond ((string-match "^ -" noback)    'font-lock-keyword-face)
                                                          ((string-match "^ [:@]" noback) 'font-lock-warning-face)
                                                          ((string-match "^ [!\\?]" noback) 'font-lock-warning-face)))))))

               ;; the current major mode for the buffer.
               "        ["

               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               "] "

               ;; Flycheck errors

               '(:eval (flycheck-mode-line-status-text))

               ;; relative position, size of file
               "    ["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ))

;; to setup tabs
(defvar c-basic-indent)
(setq c-basic-indent 4)
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode nil)

;; Text and the such
;; Use colors to highlight commands, etc.
(global-font-lock-mode t)
;; Disable the welcome message
(setq inhibit-startup-message t)
;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")
;; Make the mouse wheel scroll Emacs
(mouse-wheel-mode t)
(global-set-key (kbd "M-J") #'scroll-up-line)
(global-set-key (kbd "M-K") #'scroll-down-line)
(defun my-scroll-hook(_)
  "Increase gc-threshold before scroll and set it back after."
  (setq gc-cons-threshold most-positive-fixnum)
  (internal-show-cursor nil nil)
  (run-with-idle-timer 1 nil (lambda ()
                               (internal-show-cursor nil t)
                               (setq gc-cons-threshold (* 8 1024 1024)))))

(advice-add 'scroll-up-line :before 'my-scroll-hook)
(advice-add 'scroll-down-line :before 'my-scroll-hook)
(declare-function pixel-scroll-mode "ext:pixel-scroll")
(defvar pixel-resolution-fine-flag)
(if (> emacs-major-version 25)
    (progn
      (pixel-scroll-mode)
      (setq pixel-resolution-fine-flag t)
      (setq mouse-wheel-progressive-speed nil)
      (setq mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control))))
      (advice-add 'pixel-scroll-down :before 'my-scroll-hook)
      (advice-add 'pixel-scroll-up :before 'my-scroll-hook)))

;; Always end a file with a newline
(setq require-final-newline t)
;; Stop emacs from arbitrarily adding lines to the end of a file when the
;; cursor is moved past the end of it:
(setq next-line-add-newlines nil)
;; Flash instead of that annoying bell
(setq visible-bell t)
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

(windmove-default-keybindings)

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
  :defer 3
  :bind ("C-c r" . flycheck-list-errors)
  :config
  (global-flycheck-mode))

(use-package go-mode
  :mode "\\.go\\'"
  :functions (my-go-mode-hook go-goto-imports lsp-define-stdio-client
                              godoc-at-point goimports lsp-go-enable lsp-ui-mode)
  :defines (company-begin-commands company-backends flycheck-gometalinter-deadline go-tag-args lsp-ui-peek-mode-map)
  :config
  (progn
    (setenv "GOPATH" "/home/feofan/go")
    (setq exec-path (append exec-path '("~/go/bin")))

    (defun my-go-mode-hook ()
      "Setup for go."
      (require 'company-go)
      (require 'go-impl)
      (require 'lsp-mode)
      (require 'lsp-ui)
      (require 'flycheck-gometalinter)
      (use-package go-direx
        :config
        (defun my-kill-prev-buf (_)
          "Kill current buffer."
          (interactive)
          (kill-buffer (caar (window-prev-buffers))))
        (defun my-direx-hook()
          "My hook for direx."
          (interactive)
          (local-set-key (kbd "s") #'swiper)
          (local-set-key (kbd "q") (lambda () (interactive)(kill-buffer (buffer-name))))
          (advice-add 'direx:find-item :after 'my-kill-prev-buf))
        (add-hook 'direx:direx-mode-hook 'my-direx-hook))
      (require 'gotest)
      (require 'go-playground)
      (require 'go-eldoc)
      (require 'godoctor)
      (require 'go-rename)
      (require 'go-add-tags)
      (require 'lsp-go)
      (setq go-tag-args (list "-transform" "camelcase"))
      (if (buffer-file-name)
          (lsp-go-enable))
      (if (not (featurep 'expanderr))
          (load "~/go/src/github.com/stapelberg/expanderr/expanderr.el"))
      (flycheck-gometalinter-setup)
      (setq flycheck-gometalinter-deadline "30s")
      (add-hook 'before-save-hook #'gofmt-before-save)
      (local-set-key (kbd "C-c i") #'go-goto-imports)
      (local-set-key (kbd "C-c C-t") #'go-test-current-project)
      (local-set-key (kbd "C-c t") #'go-tag-add)
      (local-set-key (kbd "C-c T") #'go-tag-remove)
      (local-set-key (kbd "C-c C-g") #'go-gen-test-dwim)
      (local-set-key (kbd "C-c C-i") #'go-fill-struct)
      (local-set-key (kbd "M-i") #'go-direx-switch-to-buffer)
      (if (buffer-file-name) (lsp-mode))
      (go-eldoc-setup)
      (local-set-key (kbd "C-h C-d") #'godoc-at-point)
      (setq-local company-backends '(company-go))
      (lsp-ui-mode 1)
      (define-key lsp-ui-peek-mode-map (kbd "C-n") 'lsp-ui-peek--select-next)
      (define-key lsp-ui-peek-mode-map (kbd "C-p") 'lsp-ui-peek--select-prev))

    (add-hook 'go-mode-hook #'my-go-mode-hook)))

(defun my-lsp-hook ()
  "Hook for lsp-mode."
  (interactive)
  (require 'company-lsp)
  (add-to-list 'company-backends 'company-lsp))
(add-hook 'lsp-after-initialize-hook #'my-lsp-hook)

(global-font-lock-mode t)
(global-set-key "\C-xs" #'save-buffer)
(global-set-key "\C-xv" #'quoted-insert)
(global-set-key "\C-xg" #'goto-line)
(global-set-key "\C-xf" #'search-forward)
(global-set-key "\C-xc" #'compile)
;; (global-set-key "\C-xt" #'text-mode)
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
(global-set-key [M-left] #'windmove-left)          ; move to left windnow
(global-set-key [M-right] #'windmove-right)        ; move to right window
(global-set-key [M-up] #'windmove-up)              ; move to upper window
(global-set-key [M-down] #'windmove-down)          ; move to downer window

(put 'downcase-region 'disabled nil)

;;; Python mode
(add-hook 'python-mode-hook #'anaconda-mode)
(add-hook 'python-mode-hook #'eldoc-mode)

(setenv "PYMACS_PYTHON" "python2")
(setenv "PYTHONPATH" "/usr/bin/python2")
(autoload 'python-mode "python-mode.el" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(defvar python-indent)
(declare-function py-shift-right "ext:python-mode")
(declare-function py-shift-left "ext:python-mode")
(defvar python-indent-offset)
(defun my-python-hook ()
  "Setup for python."
  (add-to-list 'company-backends 'company-anaconda)
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (setq tab-width 8)
  (local-set-key (kbd "<M-iso-lefttab>") #'py-shift-right)
  (local-set-key (kbd "<backtab>") #'py-shift-left))
(add-hook 'python-mode-hook #'my-python-hook)

(use-package octave
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

(use-package company-statistics
  :functions (company-statistics-mode)
  :init
  (add-hook 'after-init-hook 'company-statistics-mode))

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


;; Clojure

;; (declare-function cider-turn-on-eldoc-mode "ext:cider")
;; (add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode)
;; (defvar nrepl-hide-special-buffers)
;; (setq nrepl-hide-special-buffers t)
;; (defvar cider-repl-print-length)
;; (setq cider-repl-print-length 100) ; the default is nil, no limit
;; ;; (set cider-repl-result-prefix ";; => ")
;; ;; (set cider-interactive-eval-result-prefix ";; => ")
;; (defvar cider-repl-wrap-history)
;; (setq cider-repl-wrap-history t)
;; (defvar cider-repl-history-size)
;; (setq cider-repl-history-size 1000) ; the default is 500

(setq browse-url-browser-function #'browse-url-chromium)

(kill-buffer "*Messages*")

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook #'delete-other-windows)

(use-package pkgbuild-mode
  :functions (pkgbuild-mode)
  :mode ("/PKGBUILD$" . pkgbuild-mode))

(use-package markdown-mode
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

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

(setq eval-expression-debug-on-error t)

;;;; Web developement
(use-package js2-mode
  :mode
  (("\\.js$" . js2-mode)
   ("\\.json$" . js-mode))
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer))
  :config
  ;; extra features for imenu
  (add-hook 'js2-mode-hook (lambda ()
                             (js2-imenu-extras-mode)))

  (defvar js2-highlight-level)
  (setq js2-highlight-level 3)
  (defvar js2-idle-timer-delay)
  (setq js2-idle-timer-delay 2)
  (setq blink-matching-paren nil)
  (use-package js2-refactor
    :functions
    (js2r-expand-node-at-point
     js2r-contract-node-at-point
     js2r-extract-function js2r-extract-method
     js2r-toggle-function-expression-and-declaration
     js2r-toggle-arrow-function-and-expression
     js2r-introduce-parameter js2r-localize-parameter
     js2r-wrap-buffer-in-iife js2r-inject-global-in-iife
     js2r-add-to-globals-annotation js2r-inline-var js2r-rename-var
     js2r-var-to-this js2r-arguments-to-object js2r-ternary-to-if
     js2r-split-var-declaration js2r-split-string js2r-unwrap
     js2r-log-this js2r-debug-this js2r-forward-slurp
     js2r-forward-barf js2r-kill)
    :bind
    (:map js2-mode-map
          ("C-c h r" . js2-refactor-hydra/body))
    :config (js2r-add-keybindings-with-prefix "C-c C-r")

    (defhydra js2-refactor-hydra (:color blue :hint nil)
      "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
      ("ee" js2r-expand-node-at-point)
      ("cc" js2r-contract-node-at-point)
      ("ef" js2r-extract-function)
      ("em" js2r-extract-method)
      ("tf" js2r-toggle-function-expression-and-declaration)
      ("ta" js2r-toggle-arrow-function-and-expression)
      ("ip" js2r-introduce-parameter)
      ("lp" js2r-localize-parameter)
      ("wi" js2r-wrap-buffer-in-iife)
      ("ig" js2r-inject-global-in-iife)
      ("ag" js2r-add-to-globals-annotation)
      ("ev" js2r-extract-var)
      ("iv" js2r-inline-var)
      ("rv" js2r-rename-var)
      ("vt" js2r-var-to-this)
      ("ao" js2r-arguments-to-object)
      ("ti" js2r-ternary-to-if)
      ("sv" js2r-split-var-declaration)
      ("ss" js2r-split-string)
      ("uw" js2r-unwrap)
      ("lt" js2r-log-this)
      ("dt" js2r-debug-this)
      ("sl" js2r-forward-slurp)
      ("ba" js2r-forward-barf)
      ("k" js2r-kill)
      ("q" nil)
      ))
  (use-package json-snatcher
    :config
    (defun js-mode-bindings ()
      "Sets a hotkey for using the json-snatcher plugin"
      (when (string-match  "\\.json$" (buffer-name))
        (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
    (add-hook 'js2-mode-hook 'js-mode-bindings))
  (use-package xref-js2
    :config

    ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
    ;; unbind it.
    (define-key js-mode-map (kbd "M-.") nil)

    (add-hook 'js2-mode-hook (lambda ()
                               (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

  ;; indium: javascript awesome development environment
  ;; https://github.com/NicolasPetton/indium
  (use-package indium
    :config (add-hook 'js2-mode-hook 'indium-interaction-mode))
  (use-package tern
    :init
    (add-hook 'js2-mode-hook #'my-js-mode-hook)
    (add-hook 'js-mode-hook #'my-js-mode-hook)
    (add-hook 'web-mode-hook #'my-js-mode-hook)
    :functions
    (my-js-mode-hook)
    :config
    (defun my-js-mode-hook ()
      "Hook for `js-mode'."
      (tern-mode)
      (define-key tern-mode-keymap (kbd "M-.") nil)
      (define-key tern-mode-keymap (kbd "M-,") nil)
      (set (make-local-variable 'company-backends)
           '((company-tern company-files))))
    (add-hook 'js2-mode-hook 'my-js-mode-hook))

  ;; turn off all warnings in js2-mode
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)

  (use-package company-tern))

(use-package rjsx-mode
  :mode ("\\.jsx$" . rjsx-mode))

(use-package web-mode
  :init
  (add-hook 'web-mode-hook  #'my-web-mode-hook)
  :mode (("\\.htm[l]?$" . web-mode)
         ("\\.dtl$" . web-mode))
  :functions (my-web-mode-hook)
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode.  Adjust `indent's."
    ;; http://web-mode.org/
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (fci-mode -1)))

(defvar company-tooltip-align-annotations)
(setq company-tooltip-align-annotations t)

(use-package emmet-mode
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
  :after ivy
  :chords (("fm" . multiple-cursors-hydra/body)
           ("mf" . multiple-cursors-hydra/body))
  ;; :functions
  ;; (my-mc-prompt-once my-mc-prompt-once-advice)
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
      ("q" nil))
    ;; (defun my-mc-prompt-once-advice (fn &rest args) ; needs lexical-binding!
    ;;   "Make FN prompt only once with ARGS and multiple cursors."
    ;;   (setq mc--this-command (lambda () (interactive) (apply fn args)))
    ;;   (apply fn args))

    ;; (defun my-mc-prompt-once (&rest fns)
    ;;   "Make FNS prompt only once with multiple cursors."
    ;;   (dolist (fn fns)
    ;;     (advice-add fn :around #'my-mc-prompt-once-advice)))

    ;; (defvar ivy-completion-beg)
    ;; (defvar ivy-completion-end)
    ;; (defun my-counsel-company ()
    ;;   "Complete using `company-candidates'."
    ;;   (interactive)
    ;;   (company-mode 1)
    ;;   (unless company-candidates
    ;;     (company-other-backend))
    ;;   (when company-point
    ;;     (when (looking-back company-prefix (line-beginning-position))
    ;;       (setq ivy-completion-beg (match-beginning 0))
    ;;       (setq ivy-completion-end (match-end 0)))
    ;;     (ivy-read "company cand: " company-candidates
    ;;               :action #'ivy-completion-in-region-action)))

    ;; (my-mc-prompt-once 'my-counsel-company #'helm-company)
    ))

(declare-function check-expansion "ext:config")
(declare-function company-complete-common "ext:company")
(declare-function tab-indent-or-complete "ext:config")
(use-package yasnippet
  :defer 3
  :after company
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
              (indent-for-tab-command)))))

    (global-set-key [tab] #'tab-indent-or-complete)))

;;for faster toggle key-chord-mode
(global-set-key [f9] #'key-chord-mode)

(setq x-hyper-keysym 'meta)

(use-package ivy
  :bind* ("C-c s k" . ivy-resume)
  :defines (ivy-completion-beg ivy-completion-end)
  :functions (ivy-completion-in-region-action)
  :config
  (progn
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-re-builders-alist
          '((counsel-M-x . ivy--regex-fuzzy)
            (swiper . ivy--regex-plus)
            ;; (t . ivy--regex-ignore-order)
            (t . ivy--regex-fuzzy)
            ))
    ;; (setq completion-in-region-function 'ivy-completion-in-region)
    ;; (ivy-mode 1)
    ))

(use-package swiper
  :disabled t
  :defer t)

(defvar company-candidates)
(defvar company-point)
(defvar company-prefix)
(declare-function company-other-backend "ext:company")
;; (use-package counsel
;;   :disabled t
;;   :init
;;   (define-key lisp-interaction-mode-map (kbd "C-M-i") #'my-counsel-company)
;;   :bind*
;;   (("C-c C-s" . counsel-rg)
;;    ("C-x l" . counsel-locate))
;;   :bind
;;   (("C-s" . counsel-grep-or-swiper)
;;    ("C-M-i" . my-counsel-company))
;;   :config
;;   (counsel-mode t))

(use-package helm-files
  :functions (helm-find-files-up-one-level))

(use-package helm
  :defines (helm-grep-ag-command
            helm-source-occur
            helm-read-file-map
            helm-find-files-map)
  :functions (helm-ido-like-higher-gc
              helm-ido-like-lower-gc
              helm-ido-like-find-files-navigate-forward
              helm-ido-like-load-file-nav
              helm-ido-like-load-fuzzy-enhancements
              helm-ido-like-fuzzier-deactivate
              helm-ido-like-fuzzier-activate
              helm-ido-like-fix-fuzzy-files
              my-helm-rg)
  :bind*
  (("C-c C-s" . my-helm-rg)
   ("C-x l" . helm-locate)
   ("C-x b" . helm-mini))
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
              (lambda () (helm-attrset 'follow 1 helm-source-grep)))
    (load "helm-autoloads"))
  :config
  (use-package ace-jump-helm-line
    :bind
    (:map helm-map
          ("C-'" . ace-jump-helm-line)))
  (use-package helm-swoop
    :disabled t
    :defines helm-swoop-map
    :functions (helm-swoop)
    :config
    (add-hook 'after-revert-hook #'helm-swoop--clear-cache)
    (defvar my-compressed-file-regex
      (progn
        (require 'jka-compr nil t)
        (jka-compr-build-file-regexp))
      "Store the regex for compressed file names.")
    (defvar my-swoop-limit 300000
      "When the buffer is larger than this, use `my-helm-rg' instead of `helm-swoop'.")
    (require 'helm-grep)
    (defun helm-swoop-or-grep ()
      "Call `helm-swoop' for small buffers and `helm-do-grep' for large ones."
      (interactive)
      (let ((fname (buffer-file-name)))
        (if (and fname
                 (not (buffer-narrowed-p))
                 (not (ignore-errors
                        (file-remote-p fname)))
                 (not (string-match
                       my-compressed-file-regex
                       fname))
                 (> (buffer-size)
                    (if (eq major-mode 'org-mode)
                        (/ my-swoop-limit 4)
                      my-swoop-limit)))
            (progn
              (when (file-writable-p fname)
                (save-buffer))
              (let ((input isearch-string))
                (helm-do-grep-1 (list (buffer-file-name)) nil nil nil nil input)))
          (helm-swoop :$query isearch-string)))))
  (require 'helm-config)
  (helm-mode +1)
  (require 'helm-fuzzier)
  (helm-fuzzier-mode 1)
  (require 'helm-flx)
  (helm-flx-mode +1)
  (defvar helm-ido-like-user-gc-setting nil)

  (defun helm-ido-like-higher-gc ()
    (setq helm-ido-like-user-gc-setting gc-cons-threshold)
    (setq gc-cons-threshold most-positive-fixnum))

  (defun helm-ido-like-lower-gc ()
    (setq gc-cons-threshold helm-ido-like-user-gc-setting))

  (defun helm-ido-like-helm-make-source (f &rest args)
    (let ((source-type (cadr args)))
      (unless (or (memq source-type '(helm-source-async helm-source-ffiles helm-grep-ag-class helm-grep-class helm-mac-spotlight-source))
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

  (with-eval-after-load 'helm-regexp
    (setq helm-source-occur
          (helm-make-source "Occur" 'helm-source-multi-occur
            :follow 1)))

  (setq helm-grep-ag-command "rg -uu --smart-case --no-heading --line-number -M 150 %s %s %s")
  (defun helm-ido-like-find-files-up-one-level-maybe ()
    (interactive)
    (if (looking-back "/" 1)
        (helm-find-files-up-one-level 1)
      (delete-char -1)))

  (defun helm-ido-like-load-file-nav ()
    (with-eval-after-load 'helm-files
      (define-key helm-read-file-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)
      (define-key helm-find-files-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)))

  (defun helm-ido-like-fuzzier-deactivate (&rest _)
    (helm-fuzzier-mode -1))


  (defun helm-ido-like-fuzzier-activate (&rest _)
    (unless helm-fuzzier-mode
      (helm-fuzzier-mode 1)))


  (defun helm-ido-like-fix-fuzzy-files ()
    (add-hook 'helm-find-files-before-init-hook #'helm-ido-like-fuzzier-deactivate)
    (advice-add 'helm--generic-read-file-name :before #'helm-ido-like-fuzzier-deactivate)
    (add-hook 'helm-exit-minibuffer-hook #'helm-ido-like-fuzzier-activate)
    (add-hook 'helm-cleanup-hook #'helm-ido-like-fuzzier-activate)
    (advice-add 'helm-keyboard-quit :before #'helm-ido-like-fuzzier-activate))

  (helm-ido-like-load-fuzzy-enhancements)
  (helm-ido-like-load-file-nav)
  (helm-ido-like-fix-fuzzy-files)

  (defun my-helm-rg ()
    (interactive)
    (let ((dir (if current-prefix-arg
                   (read-directory-name "select directory for search: " default-directory)
                 default-directory)))
      (helm-grep-ag dir (if (and current-prefix-arg
                                 (> (car current-prefix-arg) 4))
                            t
                          nil)))))

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

;;
;; ash integration
;;
;; (require 'helm-ash)
;; (global-set-key (kbd "C-x c C-r") 'helm-ash-inbox)

;; disable italic
(mapc
 #'(lambda (face)
     (set-face-attribute face nil :slant 'normal))
 (face-list))

;;
;; keyboard selection
;;
(setq select-enable-primary t)
(setq select-enable-clipboard t)
(when (getenv "DISPLAY")
  (defun xclip-cut-function (text &optional _push)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-i" "-selection" "clipboard")))
  (defun xclip-paste-function()
    (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
      (unless (string= (car kill-ring) xclip-output)
        xclip-output )))
  (setq interprogram-cut-function 'xclip-cut-function)
  (setq interprogram-paste-function 'xclip-paste-function))

(require 'mouse)
(xterm-mouse-mode t)
(setq mouse-drag-copy-region t)
(global-set-key [drag-mouse-0]	'mouse-set-region)

(use-package imenu
  :defer t
  :bind ("M-i" . imenu))

(use-package projectile
  :disabled t
  :defer 1
  :config
  (progn
    (setq projectile-completion-system 'ivy)
    (projectile-mode 1)))

;;;; Gnu global

;; key bindings
;; (defvar helm-gtags-mode-map)
;; (eval-after-load "helm-gtags"
;;   '(progn
;;      (define-key helm-gtags-mode-map (kbd "M-t d") #'helm-gtags-dwim)
;;      (define-key helm-gtags-mode-map (kbd "M-t t") #'helm-gtags-find-tag)
;;      (define-key helm-gtags-mode-map (kbd "M-t r") #'helm-gtags-find-rtag)
;;      (define-key helm-gtags-mode-map (kbd "M-t s") #'helm-gtags-find-symbol)
;;      (define-key helm-gtags-mode-map (kbd "M-t u") #'helm-gtags-update-tags)
;;      (define-key helm-gtags-mode-map (kbd "M-t c") #'helm-gtags-create-tags)
;;      (define-key helm-gtags-mode-map (kbd "M-g M-p") #'helm-gtags-parse-file)
;;      (define-key helm-gtags-mode-map (kbd "C-c <") #'helm-gtags-previous-history)
;;      (define-key helm-gtags-mode-map (kbd "C-c >") #'helm-gtags-next-history)
;;      (define-key helm-gtags-mode-map (kbd "M-,") #'helm-gtags-pop-stack)))

;;;; OpenGrok
(defvar eopengrok-jar)
(setq eopengrok-jar
      (expand-file-name "~/.emacs.d/opengrok/clj-opengrok-0.3.0-standalone.jar"))
(defvar eopengrok-ctags)
(setq eopengrok-ctags "/usr/bin/ctags")
;; (require 'eopengrok)
(defun my-opengrok-hook ()
  "Hook for eopengrok."
  (local-set-key (kbd "o") #'(lambda ()
                               (interactive)
                               (other-window 1))))
(add-hook 'eopengrok-mode-hook #'my-opengrok-hook)

;; speed-typing
;; (require 'speed-type)

;; magit
(use-package
  magit
  :defines (auto-revert-check-vc-info)
  :functions (my-magit-diff-hook)
  :defer t
  :init
  (load "magit-autoloads")
  :config
  (progn
    (defun my-magit-diff-hook ()
      "My hook for improve magit diff."
      (local-set-key (kbd "h") #'diff-refine-hunk))
    (add-hook 'magit-diff-mode-hook #'my-magit-diff-hook)
    (setq auto-revert-check-vc-info t)))

(use-package diff-mode
  :functions
  (diff-refine-hunk))

;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
(defvar org-log-done)
(setq org-log-done t)
;; ditaa
(defun my-org-hook ()
  "My hook for `org-mode'."
  (require 'org-mind-map)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t) (dot . t))))
(add-hook 'org-mode-hook #'my-org-hook)


(use-package edit-indirect
  :chords ((";r" . edit-indirect-region)))

;; pandoc
;; (require 'pandoc-mode)
(add-hook 'markdown-mode-hook #'pandoc-mode)
(declare-function pandoc-load-default-settings "ext:pandoc")
(add-hook 'pandoc-mode-hook #'pandoc-load-default-settings)

;; guile support
(defvar geiser-chez-binary)
(setq geiser-chez-binary "scheme")
;; (require 'geiser-impl)
(defvar geiser-active-implementations)
(eval-after-load "geiser-impl"
  (lambda ()
    (add-to-list 'geiser-active-implementations 'chez)))

;; slime
(use-package slime
  :disabled t
  :defer 2
  :init
  (progn
    (defvar swank-kawa-jar "")
    (defvar swank-kawa-cp "")
    (require 'subr-x)
    (setq swank-kawa-jar (concat
                          (string-trim
                           (shell-command-to-string
                            (concat "dirname " (locate-library "slime.el"))))
                          "/contrib/swank-kawa.jar"))
    (setq swank-kawa-cp (concat "/usr/share/kawa/lib/kawa.jar:"
                                swank-kawa-jar
                                ":/usr/lib/jvm/java-8-openjdk/lib/tools.jar"))

    (defmacro setup-slime-implementations ()
      "Setup slime Lisp implementations."
      `(setq slime-lisp-implementations
             '((kawa
                ("java"
                 ;; needed jar files
                 "-cp"
                 ,(prin1-to-string swank-kawa-cp 't)
                 ;; use eval-print-last-sexp on it
                 ;; channel for debugger
                 "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"
                 ;; depending on JVM, compiler may need more stack
                 "-Xss2M"
                 ;; kawa without GUI
                 "kawa.repl" "-s")
                :init kawa-slime-init)))))
  :config
  (progn
    (slime-setup '(slime-repl slime-company))

    (if (not (file-exists-p swank-kawa-jar))
        (start-process-shell-command "swank-kawa compilation"
                                     "*swank-kawa-compilation*"
                                     (concat "cd `dirname " swank-kawa-jar
                                             "` && java -cp /usr/share/kawa/lib/kawa.jar:/usr/lib/jvm/java-8-openjdk/lib/tools.jar -Xss2M kawa.repl --r7rs -d classes -C swank-kawa.scm &&  jar cf swank-kawa.jar -C classes .")))

    (defvar slime-lisp-implementations)

    (setup-slime-implementations)

    (defvar slime-protocol-version)
    (defun kawa-slime-init (file ignore)
      "Init kawa-slime for `FILE' and IGNORE second arg."
      (setq slime-protocol-version 'ignore)
      (format "%S\n"
              `(begin (import (swank-kawa))
                      (start-swank ,file)
                      ;; Optionally add source paths of your code so
                      ;; that M-. works better:
                      ;; (set! swank-java-source-path
                      ;;  (append
                      ;;   '(,(expand-file-name "~/.emacs.d/elpa/slime-20160113.630/contrib/")
                      ;;     "")
                      ;;   swank-java-source-path))
                      )))

    ;; Optionally define a command to start it.
    (defun kawa ()
      "Run kawa repl."
      (interactive)
      (slime 'kawa))))

(use-package erlang
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
;; (require 'flycheck-dialyzer)

;; fast open url
(global-set-key (kbd "C-x u") #'link-hint-open-multiple-links)

;;;; Lua
;; (require 'lua-mode)

;; nXML mode customization
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))


;;;; C, C++ Development
(use-package flycheck-clang-analyzer
  :after flycheck-irony
  :config
  (flycheck-clang-analyzer-setup))

(use-package xah-lookup
  :bind (:map c++-mode-map
              ("C-c b" . xah-lookup-boost)
              ("C-c d" . xah-lookup-cppreference))
  :functions (xah-lookup-word-on-internet xah-lookup-cppreference xah-lookup-boost)
  :defines (xah-lookup-browser-function)
  :config
  (defun xah-lookup-cppreference (&optional word)
    "Lookup definition of current WORD or text selection in URL."
    (interactive)
    (xah-lookup-word-on-internet
     word
     ;; Use � as a placeholder in the query URL.
     "http://en.cppreference.com/mwiki/index.php?search=�"
     xah-lookup-browser-function))

  (defun xah-lookup-boost (&optional word)
    "Lookup definition of current WORD or text selection in URL."
    (interactive)
    (xah-lookup-word-on-internet
     word
     "https://cse.google.com/cse?cx=011577717147771266991:jigzgqluebe&q=�"
     xah-lookup-browser-function)))

(use-package cmake-ide
  :defer 3
  :disabled t
  :config
  (use-package rtags
    :functions (rtags-eldoc
                rtags-call-rc
                rtags-symbol-type
                rtags-print-symbol-info
                rtags-find-symbol-at-point
                rtags-find-references-at-point)
    :config
    (require 'package)
    (require 'pkg-info)
    (setq rtags-completions-enabled nil))

  (use-package irony
    :functions (irony-cdb-json-add-compile-commands-path my-flycheck-irony-setup)
    :config
    (use-package irony-cdb)
    (use-package irony-cdb-json)
    (defun my-flycheck-irony-setup ()
      "Setup irony checker."
      (use-package flycheck-irony)
      (flycheck-select-checker 'irony))

    (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))

  (cmake-ide-setup))

(set-variable 'ycmd-server-command '("python" "/home/feofan/ycmd/ycmd"))
(declare-function ycmd-goto "ext:ycmd")
(declare-function ycmd-goto-references "ext:ycmd")
(defun my-cc-mode-hook ()
  "My hook for c & c++ modes."
  (require 'cc-mode)
  (require 'ycmd)
  (require 'company-ycmd)
  (require 'flycheck-ycmd)
  (ycmd-mode 1)
  ;; (local-set-key (kbd "C-c C-t") #'rtags-symbol-type)
  ;; (local-set-key (kbd "C-c C-d") #'rtags-print-symbol-info)
  (local-set-key (kbd "M-.") #'ycmd-goto
                 ;; (lambda ()
                 ;;   (interactive)
                 ;;   (xref-push-marker-stack)
                 ;;   (rtags-find-symbol-at-point))
                 )
  (local-set-key (kbd "M-?") #'ycmd-goto-references
                 ;; (lambda ()
                 ;;   (interactive)
                 ;;   (xref-push-marker-stack)
                 ;;   (rtags-find-references-at-point))
                 )
  ;; (local-set-key (kbd "C-'") #'company-irony-c-headers)
  ;; (rtags-start-process-unless-running)
  ;; (setq-local eldoc-documentation-function #'rtags-eldoc)
  ;; (eldoc-mode +1)
  (ycmd-eldoc-mode 1)
  (flycheck-ycmd-setup)
  ;; (irony-mode)
  ;; (my-flycheck-irony-setup)
  (add-to-list 'company-backends ;; '(company-irony company-irony-c-headers)
               '(company-ycmd)
               ))
(add-hook 'c++-mode-hook #'my-cc-mode-hook)
(add-hook 'c-mode-hook #'my-cc-mode-hook)

(use-package cmake-mode
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

(use-package cmake-font-lock
  :functions (cmake-font-lock-activate))

(defun my-cmake-font-lock ()
  "Activate font lock for cmake."
  (cmake-font-lock-activate))
(add-hook 'cmake-mode-hook #'my-cmake-font-lock)

;; Semantic
;; (require 'cc-mode)
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)
;; (global-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
;; (global-semantic-idle-summary-mode 1)

;; hungry deletion
(use-package hungry-delete
  :defer 0.5
  :config
  (global-hungry-delete-mode))

(show-paren-mode 1)

(electric-pair-mode 1)

;;;; Scala & Java development
;; (require 'ensime)
(defvar ensime-startup-notification)
(setq ensime-startup-notification nil)

(declare-function meghanada-reference "ext:meghanada")
(defun my-java-hook ()
  "My java hook."
  (interactive)
  (require 'meghanada)
  (meghanada-mode t)
  (local-set-key (kbd "M-?") #'meghanada-reference)
  (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))

(add-hook 'java-mode-hook #'my-java-hook)

;;; Ace link
(use-package ace-link
  :defer 0.1
  :config
  (ace-link-setup-default))

;;; Ace window
(use-package ace-window
  :bind ("M-p" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;; Which key
(use-package which-key
  :functions (which-key-mode)
  :init
  (add-hook 'after-init-hook #'which-key-mode))

;;; On the fly markdown preview
(defvar flymd-browser-open-function)
(defun my-flymd-browser-function (url)
  "See URL in firefox for flymd."
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url url)))
(setq flymd-browser-open-function 'my-flymd-browser-function)

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

(use-package counsel-dash
  :disabled t
  :defer 2
  :config
  (defun url-copy-file (url newname &optional _ok-if-already-exists
                            _keep-time _preserve-uid-gid)
    "Copy URL to NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
Fifth arg PRESERVE-UID-GID is ignored.
A prefix arg makes KEEP-TIME non-nil."
    (shell-command-to-string
     (format "curl -L -o \"%s\" \"%s\"" (expand-file-name newname) url))))

(use-package all-the-icons-ivy
  :disabled t
  :defer 2
  :config
  (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :disabled t
  :defer 2
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package symbol-overlay
  :defer 2
  :config
  (setq-default symbol-overlay-temp-in-scope t)
  (add-hook 'prog-mode-hook #'symbol-overlay-mode))

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

(use-package notmuch
  :functions (notmuch-tag-completions notmuch-logged-error)
  :defines (notmuch-hello-thousands-separator
            notmuch-command
            notmuch-search-oldest-first
            notmuch-hello-sections
            notmuch-saved-searches)
  :commands notmuch)

(use-package notmuch-hello
  :functions (my-count-query
              my-notmuch-hello-query-insert
              notmuch-hello-nice-number
              notmuch-hello-widget-search
              my-gen-notmuch-ss
              my-notmuch-update-ss)
  :after notmuch
  :config
  (defun my-gen-notmuch-ss (tag)
    (let ((key (downcase (or (and (let ((case-fold-search nil))
                                    (string-match "[[:upper:]]" tag))
                                  (match-string 0 tag))
                             (substring tag 0 1)))))
      (list :key key :name tag :query (format "tag:%s" tag)
            :search-type 'tree :sort-order 'newest-first)))
  (defun my-notmuch-update-ss ()
    (setq notmuch-saved-searches
          (cons '(:key "e" :name "licEnse" :query "to:sergey.kostyaev@eltex.loc and lic"
                       :search-type 'tree :sort-order 'newest-first)
                (seq-map #'my-gen-notmuch-ss (notmuch-tag-completions)))))
  (my-notmuch-update-ss)
  (advice-add 'notmuch-jump-search :before 'my-notmuch-update-ss)
  (setq notmuch-hello-thousands-separator ".")

  (defun my-count-query (query)
    (with-temp-buffer
      (insert query "\n")
      (unless (= (call-process-region (point-min) (point-max) notmuch-command
                                      t t nil "count" "--batch") 0)
        (notmuch-logged-error "Command \"notmuch count --batch\" failed"
                              "Please check that the notmuch CLI is new enough to support `count
--batch'. In general we recommend running matching versions of
the CLI and emacs interface."))

      (goto-char (point-min))
      (let ((n (read (current-buffer))))
        (if (= n 0)
            nil
          (notmuch-hello-nice-number n)))))

  (defun my-notmuch-hello-query-insert (cnt query elem)
    (if cnt
        (let* ((str (format "%s" cnt))
               (widget-push-button-prefix "")
               (widget-push-button-suffix "")
               (oldest-first (case (plist-get elem :sort-order)
                               (newest-first nil)
                               (oldest-first t)
                               (otherwise notmuch-search-oldest-first))))
          (widget-create 'push-button
                         :notify #'notmuch-hello-widget-search
                         :notmuch-search-terms query
                         :notmuch-search-oldest-first oldest-first
                         :notmuch-search-type 'tree
                         str)
          (widget-insert (make-string (- 8 (length str)) ? )))
      (widget-insert "        ")))

  (defun my-notmuch-hello-insert-searches ()
    "Insert the saved-searches section."
    (widget-insert (propertize "New     Total      Key  List\n" 'face 'my-notmuch-hello-header-face))
    (mapc (lambda (elem)
            (when elem
              (let* ((q_tot (plist-get elem :query))
                     (q_new (concat q_tot " AND tag:unread"))
                     (n_tot (my-count-query q_tot))
                     (n_new (my-count-query q_new)))
                (my-notmuch-hello-query-insert n_new q_new elem)
                (my-notmuch-hello-query-insert n_tot q_tot elem)
                (widget-insert "   ")
                (widget-insert (plist-get elem :key))
                (widget-insert "    ")
                (widget-insert (plist-get elem :name))
                (widget-insert "\n")
                ))
            )
          notmuch-saved-searches))

  (setq notmuch-hello-sections '(notmuch-hello-insert-header my-notmuch-hello-insert-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)))

(use-package rust-mode
  :init
  (load "rust-mode-autoloads")
  :config
  (use-package racer
    :functions (racer-mode))

  (use-package flycheck-rust
    :functions (flycheck-rust-setup))

  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'racer-mode))

;;; Prose linting
(use-package flycheck-vale
  :after flycheck
  :config (flycheck-vale-setup))

(use-package esup
  :functions (esup))

(use-package aggressive-indent
  :demand t
  :config
  (aggressive-indent-global-mode))

(use-package password-store
  :commands (password-store-get))

(use-package rich-minority
  :disabled t
  :demand t
  :config
  (progn
    (setq rm-whitelist "FlyC.*")
    (rich-minority-mode 1)))

(use-package pass
  :commands (pass))

(use-package paradox
  :disabled t
  :commands (paradox-list-packages))

(eval-after-load 'dash '(dash-enable-font-lock))

(use-package evalator
  :bind (("C-c e e" . evalator)
         ("C-c e x" . evalator-explicit)
         ("C-c e r" . evalator-resume)
         ("C-c e i" . evalator-insert-equiv-expr)))

(use-package counsel
  :bind (:map isearch-mode-map
              ("M-i" . swiper-from-isearch))
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
  (setq ivy-height 20))

(use-package ace-isearch
  :after counsel
  :bind (;; :map helm-swoop-map
         ;;      ("C-s" . swoop-action-goto-line-next)
         ;;      ("C-r" . swoop-action-goto-line-prev)
         ;;      ("C-w" . nil)
         ;;      ("M-n" . my-swoop-next)
         :map isearch-mode-map
         ("M-n" . my-isearch-next))
  :config
  (defun my-isearch-next ()
    "Isearch symbol at point or next isearch history item."
    (interactive)
    (isearch-yank-string (format "%s" (or (symbol-at-point) ""))))
  ;; (defun my-swoop-next ()
  ;;   "Swoop symbol at point or next isearch history item."
  ;;   (interactive)
  ;;   (if (s-equals-p "" helm-pattern)
  ;;       (helm-swoop-yank-thing-at-point)
  ;;     (next-history-element 1)))
  (global-ace-isearch-mode +1)
  (setq ace-isearch-function 'avy-goto-word-1)
  (setq ace-isearch-function-from-isearch 'counsel-grep-or-swiper)
  (setq ace-isearch-use-jump nil)
  ;; (define-key helm-swoop-map (kbd "C-s") 'swoop-action-goto-line-next)
  ;; (define-key helm-swoop-map (kbd "C-r") 'swoop-action-goto-line-prev)
  )

;;; plantuml
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))
(defvar org-plantuml-jar-path)
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/plantuml/plantuml.jar"))

(put 'upcase-region 'disabled nil)

(use-package circadian
  :config
  (require 'spacemacs-light-theme)
  (require 'spacemacs-dark-theme)
  (setq circadian-themes '(("8:00" . spacemacs-light)
                           ("21:00" . spacemacs-dark)))
  (circadian-setup))

(defun my-format-decimal ()
  "Replace java decimals to regular floats."
  (interactive)
  (goto-char (point-min))
  (while (search-forward-regexp "[0-9.]+E[+-][0-9]+" (point-max) t)
    (shell-command-on-region
     (match-beginning 0) (match-end 0)
     (concat "LC_NUMERIC=C printf \"%'.3f\" " (match-string 0)) (buffer-file-name) t)))

(defun my-toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (ignore-errors
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds))
             (currently-using-underscores-p (progn (goto-char start)
                                                   (re-search-forward "_" end t))))
        (if currently-using-underscores-p
            (let ((res (mapconcat 'capitalize (split-string (buffer-substring-no-properties start end) "_") "")))
              (progn
                (kill-region start end)
                (insert res)))
          (funcall-interactively 'replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
          (downcase-region start (cdr (bounds-of-thing-at-point 'symbol))))))))

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
  :bind (("C-c h f" . helm-codesearch-find-file)
         ("C-c h s" . helm-codesearch-find-pattern)
         ("C-c h c" . helm-codesearch-create-csearchindex)))

;; helm charts support
(defvar company-dabbrev-code-modes)
(add-to-list 'company-dabbrev-code-modes 'yaml-mode)
(add-hook 'yaml-mode-hook #'highlight-indentation-mode)
(add-hook 'yaml-mode-hook #'highlight-indentation-current-column-mode)
(global-smart-shift-mode 1)
(key-chord-define-global "<<" 'smart-shift-left)
(key-chord-define-global ">>" 'smart-shift-right)

(load custom-file 'noerror)
(setq gc-cons-threshold (* 8 1024 1024))

(provide 'init)
;;; init.el ends here
