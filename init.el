;;; init.el --- Emacs init file.

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa"))
(require 'seq)
(seq-do (lambda (dir) (add-to-list 'load-path (expand-file-name dir)))
 (split-string (shell-command-to-string "ls -1 ~/.emacs.d/elpa/")))
(seq-do (lambda (dir) (add-to-list 'load-path (expand-file-name dir)))
 (split-string (shell-command-to-string "ls -1 ~/.emacs.d/lisp")))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(defvar personal-keybindings nil)

;; all keybindings must work with russian (key-chord still doesn't work)
;; system language must be set to en
;; use C-\ for change language in emacs instead
(setq default-input-method "cyrillic-jis-russian")

(defun my-set-font ()
  "Set my font."
  (set-frame-font "-gohu-gohufont-medium-r-normal--14-*-100-100-c-80-iso10646-1" nil t)
  ;; (set-frame-font "-pyrs-Roboto Mono Light for Powerline-light-normal-normal-*-13-*-*-*-*-0-iso10646-1")
  ;; (set-frame-font "-FBI -Input Mono Compressed-light-normal-extracondensed-*-14-*-*-*-m-0-iso10646-1")
  ;; (set-frame-font "-PfEd-Iosevka-light-normal-normal-*-14-*-*-*-d-0-iso10646-1")
  ;; (set-frame-font "-unci-monofur for Powerline-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
  )
(my-set-font)
(add-hook 'after-change-major-mode-hook #'my-set-font)

;; Melpa
(require 'package)
(package-initialize)
(defvar quelpa-update-melpa-p)
(setq quelpa-update-melpa-p nil)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; async
(quelpa 'async)
(async-bytecomp-package-mode 1)


(quelpa 'color-theme)
(require 'color-theme)
(setq color-theme-is-global t)
;; (color-theme-initialize)
(quelpa 'color-theme-solarized)

;; (quelpa 'color-theme-sanityinc-solarized)
;; (require 'color-theme-sanityinc-solarized)
;; (color-theme-sanityinc-solarized-light)
;; (quelpa 'zenburn-theme)
;; (load-theme 'zenburn t)

;; (quelpa 'sublime-themes)


;; powerline
(quelpa 'smart-mode-line)
(require 'smart-mode-line)
;; (quelpa 'smart-mode-line-powerline-theme)

;; (sml/apply-theme "powerline")
;; (sml/apply-theme 'respectful)
;; (load-theme 'smart-mode-line-respectful t)
;; (powerline-default-theme)
;; (sml/setup)

(quelpa 'monokai-theme)
(defun my-set-themes-hook ()
  "Hook for setting themes after init."
  (sml/setup)
  (require 's)
  ;; (if (s-equals? "probook" (s-trim (shell-command-to-string "hostname")))
  ;;   (load-theme 'monokai t)
  ;; (load-theme 'solarized t))
  (load-theme 'solarized t)
  (load-theme 'smart-mode-line-respectful t))

(add-hook 'after-init-hook #'my-set-themes-hook)
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
;; Display time
;(display-time)
;; Make the mouse wheel scroll Emacs
(mouse-wheel-mode t)
;; (quelpa 'smooth-scroll)
;; (require 'smooth-scroll)
;; (smooth-scroll-mode t)
;; (setq smooth-scroll/vscroll-step-size 1)
(setq gc-cons-threshold (* 80 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
;; (quelpa 'smooth-scrolling)
;; (require 'smooth-scrolling)
;; (smooth-scrolling-mode 1)
;; (global-set-key [(control down)] #'(lambda () (interactive) (scroll-up-1 4)))
;; (global-set-key [(control up)]   #'(lambda () (interactive) (scroll-down-1 4)))
;; (global-set-key (kbd "C-v") #'(lambda () (interactive) (smooth-scroll/orig-scroll-up)))
;; (global-set-key (kbd "M-v") #'(lambda () (interactive) (smooth-scroll/orig-scroll-down)))
;; (global-set-key (kbd "<next>") #'(lambda () (interactive) (smooth-scroll/orig-scroll-up)))
;; (global-set-key (kbd "<prior>") #'(lambda () (interactive) (smooth-scroll/orig-scroll-down)))
(global-set-key (kbd "M-J") #'scroll-up-line)
(global-set-key (kbd "M-K") #'scroll-down-line)

;; Always end a file with a newline
(setq require-final-newline t)
;; Stop emacs from arbitrarily adding lines to the end of a file when the
;; cursor is moved past the end of it:
(setq next-line-add-newlines nil)
;; Flash instead of that annoying bell
(setq visible-bell t)
;; Remove icons toolbar
;(if (> emacs-major-version 20)
;(tool-bar-mode -1))
;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)


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
    (ignore-errors (er/expand-region))
    (let ((new-end (region-end)))
     (goto-char new-end)
     (whitespace-cleanup-region beg (line-end-position)))))
(global-set-key (kbd "C-c a") #'my-align-region-by)

;;
;; hydra
;;
(quelpa 'hydra)
(require 'hydra)

(global-set-key (kbd "C-x o")
                (defhydra hydra-cycle-windows
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
                  ("q" nil "quit")))
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

;; Flycheck
(quelpa 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (global-set-key (kbd "C-c r") #'helm-flycheck)
(global-set-key (kbd "C-c r") #'flycheck-list-errors)

;;;; Go mode
(setenv "GOPATH" "/home/feofan/go")
(setq exec-path (append exec-path '("~/go/bin")))
(quelpa 'go-mode)
(require 'go-mode-autoloads)
(defun goimports ()
  "Running goimports on go files."
  (interactive)
  (if (s-equals-p mode-name "Go")
      (progn
        (shell-command "goimports -w *.go")
        (revert-buffer t t))))

(quelpa 'company-go)

(declare-function go-remove-unused-imports "ext:go-mode")
(declare-function go-goto-imports "ext:go-mode")
(declare-function gobuild "ext:gobuild")
(declare-function gometalinter "ext:gometalinter")
(defvar company-begin-commands)
(defvar company-backends)

(quelpa 'go-impl)
(quelpa 'flycheck-gometalinter)
(defun my-go-mode-hook ()
  "Setup for go."
  (require 'gobuild)
  (require 'company-go)
  (require 'go-impl)
  (require 'gometalinter)
  (require 'go-flycheck)
  ; start autocompletion only after typing
  (setq company-begin-commands '(self-insert-command))
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'after-save-hook #'goimports)
  (local-set-key (kbd "C-c C-r") #'go-remove-unused-imports)
  (local-set-key (kbd "C-c i") #'go-goto-imports)
  (local-set-key (kbd "C-c C-c") #'(lambda ()
                                     (interactive)
                                     (gobuild)))
  (local-set-key (kbd "C-c C-t") #'(lambda ()
                                     (interactive)
                                     (shell-command "go test")))
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (local-set-key (kbd "C-c C-l") #'(lambda ()
                                     (interactive)
                                     (gometalinter))))

(add-hook 'go-mode-hook #'my-go-mode-hook)
;; Flycheck
(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
;; doc
(quelpa 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook #'go-eldoc-setup)


; for compiling C/C++
(global-font-lock-mode t)
(global-set-key "\C-xs" #'save-buffer)
(global-set-key "\C-xv" #'quoted-insert)
(global-set-key "\C-xg" #'goto-line)
(global-set-key "\C-xf" #'search-forward)
(global-set-key "\C-xc" #'compile)
(global-set-key "\C-xt" #'text-mode);
(global-set-key "\C-xr" #'replace-string);
(global-set-key "\C-xa" #'repeat-complex-command);
(global-set-key "\C-xm" #'manual-entry);
(global-set-key "\C-xw" #'what-line);
(global-set-key "\C-x\C-u" #'shell);
(global-set-key "\C-x0" #'overwrite-mode);
(global-set-key "\C-x\C-r" #'read-only-mode);
(global-set-key "\C-t" #'kill-word);
(global-set-key "\C-p" #'previous-line);
(global-set-key "\C-o" #'forward-word);
;(global-set-key "\C-h" 'backward-delete-char-untabify);
(global-set-key "\C-x\C-m" #'not-modified);
(setq make-backup-files 'nil);
(setq text-mode-hook 'turn-on-auto-fill)
(setq auto-mode-alist (cons '("\\.cxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hpp$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))

; http://nex-3.com/posts/45-efficient-window-switching-in-emacs#comments
(global-set-key [M-left] #'windmove-left)          ; move to left windnow
(global-set-key [M-right] #'windmove-right)        ; move to right window
(global-set-key [M-up] #'windmove-up)              ; move to upper window
(global-set-key [M-down] #'windmove-down)          ; move to downer window


;; http://emacs-fu.blogspot.com/2008/12/cycling-through-your-buffers-with-ctrl.html
;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") #'bury-buffer)



; http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun iwb ()
    "Indent whole buffer."
    (interactive)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max)))

(put 'downcase-region 'disabled nil)

;;; Python mode
(quelpa 'anaconda-mode)
(add-hook 'python-mode-hook #'anaconda-mode)
(add-hook 'python-mode-hook #'eldoc-mode)
(quelpa 'company-anaconda)

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

;;; Octave mode
;; (autoload 'octave-mode "octave-mod" nil t)
;;           ;(setq auto-mode-alist
;;           ;      (cons '("\\.m$" . octave-mode) auto-mode-alist))
;; (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
;; ;; to turn on the abbrevs, auto-fill and font-lock features automatically
;;        (add-hook 'octave-mode-hook
;;          (lambda ()
;;          (abbrev-mode 1)
;;          (auto-fill-mode 1)
;;          (if (eq window-system 'x)
;;           (font-lock-mode 1))))

;; ;; And finally, inferior-octave-mode-hook is run after starting the process
;;     ;; and putting its buffer into Inferior Octave mode. Hence, if you like
;;     ;; the up and down arrow keys to behave in the interaction buffer as in
;;     ;; the shell, and you want this buffer to use nice colors:
    
;;        (add-hook 'inferior-octave-mode-hook
;;          (lambda ()
;;            (turn-on-font-lock)
;;            (define-key inferior-octave-mode-map [up]
;;              'comint-previous-input)
;;            (define-key inferior-octave-mode-map [down]
;;              'comint-next-input)))
;; ;; run an inferior Octave process in a special Emacs buffer
;;        (autoload 'run-octave "octave-inf" nil t)


(defvar fsm-debug)
(setq fsm-debug nil)


;;; Auto-complete
(quelpa 'company)
(require 'company)
(global-company-mode)
(setq company-global-modes '(not erlang-mode))
(defvar company-etags-ignore-case)
(setq company-etags-ignore-case nil)
(defvar company-dabbrev-ignore-case)
(setq company-dabbrev-ignore-case nil)
(defvar company-dabbrev-code-ignore-case)
(setq company-dabbrev-code-ignore-case nil)
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
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay 0.1)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-minimum-prefix-length 3)
(defun check-expansion ()
  "Check yasnippet expansion."
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  "Do yasnippet expansion."
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defvar yas-minor-mode)
(defun tab-indent-or-complete ()
  "Smart tab function."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [tab] #'tab-indent-or-complete)



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

(declare-function cider-turn-on-eldoc-mode "ext:cider")
(add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode)
(defvar nrepl-hide-special-buffers)
(setq nrepl-hide-special-buffers t)
(defvar cider-repl-print-length)
(setq cider-repl-print-length 100) ; the default is nil, no limit
;; (set cider-repl-result-prefix ";; => ")
;; (set cider-interactive-eval-result-prefix ";; => ")
(defvar cider-repl-wrap-history)
(setq cider-repl-wrap-history t)
(defvar cider-repl-history-size)
(setq cider-repl-history-size 1000) ; the default is 500

(setq browse-url-browser-function #'browse-url-chromium)

;;;; Paredit
;; (quelpa 'paredit)
;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; (add-hook 'clojure-mode-hook          #'enable-paredit-mode)

(require 'eldoc) ; if not already loaded

;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
;(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook #'delete-other-windows)

;; (setq scroll-margin 12)
;; (setq scroll-step 1)

;; PKGBUILDs
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; Markdown
(quelpa 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; for over-80-chars line highlightning
;; (quelpa 'column-enforce-mode)
;; (add-hook 'prog-mode-hook 'column-enforce-mode)
(quelpa 'fill-column-indicator)
(require 'fill-column-indicator)
(set 'fci-rule-column 80)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(setq eval-expression-debug-on-error t)

;;;; Web developement
(quelpa 'web-mode)
(quelpa 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; (quelpa 'react-snippets)
(defvar js2-highlight-level)
(setq js2-highlight-level 3)
(defvar js2-idle-timer-delay)
(setq js2-idle-timer-delay 2)
(setq blink-matching-paren nil)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; for templates
(add-to-list 'auto-mode-alist '("\\.dtl$" . web-mode))

(quelpa 'ac-js2)
(require 'ac-js2)
(add-hook 'js2-mode-hook #'ac-js2-mode)
(defvar ac-js2-evaluate-calls)
(setq ac-js2-evaluate-calls t)
(defvar ac-js2-add-browser-externs)
(setq ac-js2-add-browser-externs t)
(defvar ac-js2-add-prototype-completions)
(setq ac-js2-add-prototype-completions t)
(add-hook 'ac-js2-mode-hook #'skewer-run-phantomjs)
;; default port conflicts with other soft
(setq httpd-port 18080)

;; xref-js2
(quelpa 'xref-js2)
(define-key js2-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; tern
;; (quelpa 'tern)
;; (quelpa 'company-tern)
;; (add-to-list 'company-backends 'company-tern)
;; (defvar company-tern-meta-as-single-line)
;; (setq company-tern-meta-as-single-line t)
;; (defvar company-tooltip-align-annotations)
;; (setq company-tooltip-align-annotations t)

;; (add-hook 'js2-mode-hook #'tern-mode)
;; (add-hook 'js-mode-hook #'tern-mode)
;; (add-hook 'web-mode-hook #'tern-mode)

(quelpa 'js2-refactor)
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

(quelpa 'skewer-mode)
(require 'skewer-mode)
(skewer-setup)

;; adjust indents for web-mode to 2 spaces
(defvar web-mode-markup-indent-offset)
(defvar web-mode-css-indent-offset)
(defvar web-mode-code-indent-offset)
(defun my-web-mode-hook ()
  "Hooks for Web mode.  Adjust `indent's."
  ;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  #'my-web-mode-hook)

;;
;; emmet mode
;;
(quelpa 'emmet-mode)
(require 'emmet-mode)
(add-hook 'sgml-mode-hook #'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook #'emmet-mode)
(add-hook 'css-mode-hook #'emmet-mode) ;; enable Emmet's css abbreviation.
(setq emmet-move-cursor-between-quotes t) ;; default nil


;;
;; key chord
;;
(quelpa 'key-chord)
(require 'key-chord)
(key-chord-mode 1)

;;
;; ace jump mode major function
;; 
;; (quelpa 'ace-jump-mode)
;; (autoload
;;   'ace-jump-mode
;;   "ace-jump-mode"
;;   "Emacs quick move minor mode"
;;   t)
;; ;; you can select the key you prefer to
;; (key-chord-define-global "fk"
;;                          (defhydra hydra-ace-jump (:exit t) "Ace jump mode"
;;                            ("j" ace-jump-mode "jump")
;;                            ("l" ace-jump-line-mode "line")
;;                            ("w" ace-jump-word-mode "word")
;;                            ("c" ace-jump-char-mode "char")
;;                            ("p" ace-jump-mode-pop-mark "pop mark")
;;                            ("q" nil "quit")))
;; avy
(quelpa 'avy)
(key-chord-define-global "fj" 'avy-goto-word-1)
(key-chord-define-global "f'" 'avy-pop-mark)
(define-key isearch-mode-map (kbd "C-'") #'avy-isearch)

;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
;; (declare-function ace-jump-mode-enable-mark-sync "ext:ace-jump-mode")
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))



;;
;; expand region
;;
(quelpa 'expand-region)
(require 'expand-region)
(key-chord-define-global "zj" 'er/expand-region)
(key-chord-define-global "zk" 'er/contract-region)
(delete-selection-mode)

;;
;; multiple cursors
;;
(quelpa 'multiple-cursors)
(require 'multiple-cursors)
(quelpa 'ace-mc)

(key-chord-define-global "fm"
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
  ("q" nil)))

;;
;; tagedit
;;
;; (quelpa 'tagedit)
;; (eval-after-load "sgml-mode"
;;   '(progn
;;      (require 'tagedit)
;;      (tagedit-add-paredit-like-keybindings)
;;      (add-hook 'html-mode-hook #'tagedit-mode)))

;;
;; yasnippet
;;
(quelpa 'yasnippet)
(yas-global-mode 1)

;;
;; tramp mode for fast open files with sudo
;;
(require 'tramp)

;;
;; C-w like in readline
;;
(global-set-key (kbd "C-w") #'backward-kill-word)
(global-set-key (kbd "C-c C-w") #'kill-region)

(global-set-key (kbd "C-c C-n") #'goto-line)

;;for faste toggle key-chord-mode
(global-set-key [f9] #'key-chord-mode)

;helm
;; (quelpa 'helm)
;; (require 'helm-config)
;; (global-set-key (kbd "C-x C-x") #'helm-M-x)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-r") #'(lambda () (interactive)
                                  (byte-recompile-file "~/.emacs.d/init.el" t 0 t)))
(setq x-hyper-keysym 'meta)
;; ivy
(quelpa 'ivy)
(quelpa 'ivy-hydra)
(ivy-mode 1)
(defvar ivy-initial-inputs-alist)
(setq ivy-initial-inputs-alist nil)
(defvar ivy-re-builders-alist)
(setq ivy-re-builders-alist
      '((counsel-M-x . ivy--regex-fuzzy)
        (swiper . ivy--regex-plus)
        ;; (t . ivy--regex-ignore-order)
        (t . ivy--regex-fuzzy)
        ))
(setq completion-in-region-function 'ivy-completion-in-region)
(quelpa 'swiper)
(global-set-key "\C-s" #'swiper)
(global-set-key (kbd "C-c s k") #'ivy-resume)
(quelpa 'counsel)
(quelpa 'smex)
(quelpa 'flx)
(quelpa 'wgrep)
(require 'wgrep)
(setq wgrep-auto-save-buffer t)
(counsel-mode t)
(global-set-key "\C-s" #'counsel-grep-or-swiper)
(global-set-key (kbd "s-x") #'counsel-M-x)
(global-set-key (kbd "s-y") #'counsel-yank-pop)
(global-set-key (kbd "s-w") #'kill-ring-save)
(global-set-key (kbd "s-v") #'scroll-down-command)
(global-set-key (kbd "s-;") #'comment-dwim-line)
(global-set-key (kbd "M-;") #'comment-dwim-line)
(global-set-key (kbd "C-c C-s") #'counsel-ag)
(global-set-key (kbd "C-x l") #'counsel-locate)
(quelpa 'counsel-projectile)

;; (quelpa 'helm-descbinds)
;; (require 'helm-descbinds)
;; (helm-descbinds-mode 1)

(defvar ivy-mode-map)

(defvar-local my-counsel-company-prefix nil
  "Company prefix for use counsel-company with multiple-cursors.")
(defvar ivy-completion-beg)
(defvar ivy-completion-end)
(defun my-counsel-company ()
  "Complete using `company-candidates'."
  (interactive)
  (company-mode 1)
  (company-cancel)
  (unless company-candidates
    (company-complete))
  (when company-point
    (setq my-counsel-company-prefix company-prefix)
    (company--fetch-candidates my-counsel-company-prefix)
    (when (looking-back company-common (line-beginning-position))
      (setq ivy-completion-beg (match-beginning 0))
      (setq ivy-completion-end (match-end 0)))
    (ivy-read "company cand: " (mapcar #'(lambda (x)
                                           (let ((annotation
                                                  (company-call-backend 'annotation x)))
                                             (if (> (length annotation) 0)
                                                 (progn
                                                   (set-text-properties
                                                    0 (length annotation)
                                                    '(face success) annotation)
                                                   (concat x "\t\t" annotation))
                                               x)))
                                       company-candidates)
              :action #'(lambda (x)
                          (company-cancel)
                          (ivy-completion-in-region-action
                           (replace-regexp-in-string "\t\t\.*" "" x))
                          (let
                              ((insertion (s-chop-prefix my-counsel-company-prefix
                                                       (replace-regexp-in-string "\t\t\.*" "" x))))
                            (ignore-errors
                             (mc/execute-command-for-all-fake-cursors
                              #'(lambda ()
                                  (interactive)
                                  (insert insertion)))))))))

(global-set-key (kbd "C-:") #'counsel-company)

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

;; rebind F1 for xterm
;(global-set-key (kbd "M-o p") 'help)

(global-set-key (kbd "M-i") #'imenu)

;;;; Projectile
(quelpa 'projectile)
;; (quelpa 'helm-projectile)
(projectile-mode 1)
(defvar projectile-completion-system)
(setq projectile-completion-system 'ivy)
;; (helm-projectile-on)


;;;; Gnu global
;; (quelpa 'helm-gtags)

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
(quelpa 'eopengrok)
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
(quelpa 'speed-type)
(require 'speed-type)

;; magit
(quelpa 'magit)

;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
(defvar org-log-done)
(setq org-log-done t)
; ditaa
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t) (dot . t)))

;; indirect region
(quelpa 'edit-indirect)
(key-chord-define-global (kbd ";r") #'edit-indirect-region)

;; helm flycheck
;; (quelpa 'helm-flycheck)

;; pandoc
(quelpa 'pandoc-mode)
(require 'pandoc-mode)
(add-hook 'markdown-mode-hook #'pandoc-mode)
(declare-function pandoc-load-default-settings "ext:pandoc")
(add-hook 'pandoc-mode-hook #'pandoc-load-default-settings)

;; guile support
(quelpa 'geiser)
;; (add-hook 'geiser-repl-mode-hook #'paredit-mode)
;; (add-hook 'geiser-mode-hook #'paredit-mode)
(defvar geiser-chez-binary)
(setq geiser-chez-binary "chez-scheme")
(require 'geiser-impl)
(add-to-list 'geiser-active-implementations 'chez)

;; slime
(quelpa 'slime)
(quelpa 'slime-company)
(slime-setup '(slime-repl slime-company))

(defvar swank-kawa-jar "")
(defvar swank-kawa-cp "")
(require 'subr-x)
(setq swank-kawa-jar (concat
                        (string-trim
                         (shell-command-to-string
                          (concat "dirname " (locate-library "slime.el"))))
                        "/contrib/swank-kawa.jar"))

(if (not (file-exists-p swank-kawa-jar))
    (start-process-shell-command "swank-kawa compilation"
                                 "*swank-kawa-compilation*"
                                 (concat "cd `dirname " swank-kawa-jar
                                         "` && java -cp /usr/share/java/kawa.jar:/usr/lib/jvm/java-8-openjdk/lib/tools.jar -Xss2M kawa.repl --r7rs -d classes -C swank-kawa.scm &&  jar cf swank-kawa.jar -C classes .")))

(setq swank-kawa-cp (concat "/usr/share/java/kawa.jar:"
                        swank-kawa-jar
                        ":/usr/lib/jvm/java-8-openjdk/lib/tools.jar"))

(defvar slime-lisp-implementations)
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
           :init kawa-slime-init))))

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
  (slime 'kawa))

;;;; Erlang
(quelpa 'erlang)
(setq flycheck-erlang-include-path '("../include" "../deps"))

(defun fix-erlang-project-includes (project-root)
  "Find erlang include paths for PROJECT-ROOT with project deps."
  (setq-local flycheck-erlang-include-path
              (append
               (s-split
                "\n"
                (shell-command-to-string
                 (concat "find "
                         project-root
                         "/*"
                         " -type d -name include"))
                t)
               (list project-root
                     (concat project-root "/include")
                     (concat project-root "/deps")
                     default-directory
                     (concat
                      (locate-dominating-file
                       default-directory
                       "src") "include")
                     (concat
                      (locate-dominating-file
                       default-directory
                       "src") "deps")))))

(defun fix-erlang-project-code-path (project-root)
  "Find erlang include paths for PROJECT-ROOT with project deps."
  (let ((code-path
           (split-string (shell-command-to-string
                        (concat "find " project-root " -type d -name ebin")))
         ))
    (setq-local flycheck-erlang-library-path code-path)))
(quelpa 'ivy-erlang-complete)
(require 'ivy-erlang-complete)

;; (defun force-reindex-erlang-project ()
;;   "Force update erlang project index."
;;   (interactive)
;;   (message "%s" (shell-command-to-string
;;                  (concat "find " ivy-erlang-complete-project-root
;;                          " -type d -name .opengrok -exec rm -fr {} +")))
;;   (eopengrok-make-index-with-enable-projects ivy-erlang-complete-project-root))

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
  (require 'wrangler)
  (let ((project-root (ivy-erlang-complete-autosetup-project-root)))
    (fix-erlang-project-code-path project-root)
    (fix-erlang-project-includes project-root))
  (ivy-erlang-complete-init)
  (defvar erlang-extended-mode-map)
  (define-key erlang-extended-mode-map (kbd "M-.") nil)
  (define-key erlang-extended-mode-map (kbd "M-,") nil)
  (define-key erlang-extended-mode-map (kbd "M-?") nil)
  (define-key erlang-extended-mode-map (kbd "(") nil)
  (local-set-key (kbd "C-c C-p") #'my-format-erlang-record))
(add-hook 'erlang-mode-hook #'my-erlang-hook)
(add-hook 'after-save-hook #'ivy-erlang-complete-reparse)
(eval-after-load 'erlang (define-key erlang-mode-map (kbd "C-c C-s") nil))

(add-to-list 'auto-mode-alist '("rebar\\.config$" . erlang-mode))
(add-to-list 'auto-mode-alist '("relx\\.config$" . erlang-mode))
(add-to-list 'auto-mode-alist '("system\\.config$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.app\\.src$" . erlang-mode))

(quelpa 'flycheck-dialyzer)
(require 'flycheck-dialyzer)

;;; wrangler
(add-to-list 'load-path "/usr/lib/erlang/lib/wrangler-1.2.0/elisp")
; Some code inspection functionalities of Wrangler generate .dot
; files, which can be compiled and previewed in Emacs if the
; Graphviz-dot mode for Emacs is enabled.
;; (load-library "graphviz-dot-mode")

;; distel
;; (add-to-list 'load-path "/usr/share/distel/elisp")
;; (require 'distel)
;; (distel-setup)

;; A number of the erlang-extended-mode key bindings are useful in the shell too
;; (defconst distel-shell-keys
;;   '(("\M-/"      erl-complete)
;;     ("\M-."      erl-find-source-under-point)
;;     ("\M-,"      erl-find-source-unwind)
;;     )
;;   "Additional keys to bind when in Erlang shell.")

;; (add-hook 'erlang-shell-mode-hook
;;           (lambda ()
;;             ;; add some Distel bindings to the Erlang shell
;;             (dolist (spec distel-shell-keys)
;;               (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;; (defun distel-start-local ()
;;   "Start local erlang node for distel."
;;   (interactive)
;;   (start-process-shell-command
;;    "erlang-local-node" "*erlang-local*" "erl -sname local"))


;; fast open url
(quelpa 'link-hint)
(global-set-key (kbd "C-x u") #'link-hint-open-multiple-links)

;;;; Lua
(quelpa 'lua-mode)
(require 'lua-mode)

;; nXML mode customization
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))


;;;; C, C++ Development
;; Rtags
(quelpa 'rtags)
(require 'rtags)
(setq rtags-completions-enabled nil)
;; (setq rtags-use-helm t)
;; completion
(quelpa 'irony)
(quelpa 'company-irony)
(quelpa 'company-irony-c-headers)
;; (quelpa 'company-c-headers)
(add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

;; show docs at point
(quelpa 'xah-lookup)
(require 'xah-lookup)
;; Uncomment the below line to use eww (Emacs Web Wowser)
;; (setq xah-lookup-browser-function 'eww)

(defun xah-lookup-cppreference (&optional word)
  "Lookup definition of current WORD or text selection in URL."
  (interactive)
  (xah-lookup-word-on-internet
   word
   ;; Use � as a placeholder in the query URL.
   "http://en.cppreference.com/mwiki/index.php?search=�"
   xah-lookup-browser-function))

(require 'cc-mode)

;; Add shortcut for c++-mode
(define-key c++-mode-map (kbd "C-c d") #'xah-lookup-cppreference)

;; Another example with http://www.boost.org
(defun xah-lookup-boost (&optional word)
  "Lookup definition of current WORD or text selection in URL."
  (interactive)
  (xah-lookup-word-on-internet
   word
   "https://cse.google.com/cse?cx=011577717147771266991:jigzgqluebe&q=�"
   xah-lookup-browser-function))
(define-key c++-mode-map (kbd "C-c b") #'xah-lookup-boost)

(defun my-cc-mode-hook ()
  "My hook for c & c++ modes."
  (local-set-key (kbd "C-c C-t") #'rtags-symbol-type)
  (local-set-key (kbd "C-c C-d") #'rtags-print-symbol-info)
  (local-set-key (kbd "M-.") (lambda ()
                               (interactive)
                               (xref-push-marker-stack)
                               (rtags-find-symbol-at-point)))
  (local-set-key (kbd "M-?") (lambda ()
                               (interactive)
                               (xref-push-marker-stack)
                               (rtags-find-references-at-point)))
  (local-set-key (kbd "C-'") #'company-irony-c-headers)
  (rtags-start-process-unless-running)
  (irony-mode)
  (add-to-list 'company-backends '(company-irony company-irony-c-headers)))
(add-hook 'c++-mode-hook #'my-cc-mode-hook)
(add-hook 'c-mode-hook #'my-cc-mode-hook)
(eval-after-load 'cc-mode (define-key c++-mode-map (kbd "C-c C-s") nil))
(eval-after-load 'cc-mode (define-key c-mode-map (kbd "C-c C-s") nil))
;; Semantic
;; (require 'cc-mode)
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)
;; (global-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
;; (global-semantic-idle-summary-mode 1)

(quelpa 'cmake-ide)
(cmake-ide-setup)

(quelpa 'cmake-mode)
; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))
(quelpa 'cmake-font-lock)
(defun my-cmake-font-lock ()
  "Activate font lock for cmake."
  (require 'cmake-font-lock)
  (cmake-font-lock-activate))
(add-hook 'cmake-mode-hook #'my-cmake-font-lock)

(defvar company-c-headers-path-user)
(defun company-set-c-headers-user-path ()
  "Set path for selected directory with project headers."
  (interactive)
  (let
      ((dir
        (expand-file-name (read-directory-name
                           "Select project directory:" default-directory))))
    (setq company-c-headers-path-user (list (concat dir "/include")))))

;;; Viking mode - Kill first, ask later
(quelpa 'viking-mode)
(require 'viking-mode)
(viking-global-mode)

(show-paren-mode 1)

;;; Smartparens
(quelpa 'smartparens)
(require 'smartparens-config)

(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'erlang-mode-hook #'smartparens-mode)

;;;; Scala & Java development
(quelpa 'ensime)
(require 'ensime)
(setq ensime-startup-snapshot-notification nil)

;;; Ace link
(quelpa 'ace-link)
(ace-link-setup-default)

;;; Ace window
(quelpa 'ace-window)
(global-set-key (kbd "M-p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;; Which key
(quelpa 'which-key)
(require 'which-key)
(which-key-mode)


(setq custom-file "~/.emacs.d/emacs-customizations.el")
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
