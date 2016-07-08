;;; init.el --- Emacs init file.

;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d")
(progn (cd "~/.emacs.d")
       (normal-top-level-add-subdirs-to-load-path))

(defvar personal-keybindings nil)

;; all keybindings must work with russian (key-chord still doesn't work)
;; system language must be set to en
;; use C-\ for change language in emacs instead
(setq default-input-method "cyrillic-jis-russian")

(defun my-set-font ()
  "Set my font."
  (set-frame-font "-gohu-gohufont-medium-r-normal--14-*-100-100-c-80-iso10646-1" nil t))
(my-set-font)
(add-hook 'after-change-major-mode-hook #'my-set-font)

;; Melpa
(require 'package) ;; You might already have this line
;; (add-to-list 'package-archives
;; 			              '("melpa" . "http://melpa.org/packages/") t)
;; (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;;     (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(setq  package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
(package-initialize) ;; You might already have this line

										; for automatic download required packages
(defun need-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
	   t
	 (if (or (assoc package package-archive-contents) no-refresh)
		 (package-install package)
	   (progn
		 (package-refresh-contents)
		 (need-package package min-version t)))))

;; async
(need-package 'async)
(async-bytecomp-package-mode 1)


(need-package 'color-theme)
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(need-package 'color-theme-solarized)

;; (need-package 'color-theme-sanityinc-solarized)
;; (require 'color-theme-sanityinc-solarized)
;; (color-theme-sanityinc-solarized-light)
;; (need-package 'zenburn-theme)
;; (load-theme 'zenburn t)

(need-package 'sublime-themes)


;; powerline
(need-package 'smart-mode-line)
(require 'smart-mode-line)
;; (need-package 'smart-mode-line-powerline-theme)

;; (sml/apply-theme "powerline")
;; (sml/apply-theme 'respectful)
;; (load-theme 'smart-mode-line-respectful t)
;; (powerline-default-theme)
;; (sml/setup)

(defun my-set-themes-hook ()
  "Hook for setting themes after init."
  (sml/setup)
  (require 's)
  (if (s-equals? "probook" (s-trim (shell-command-to-string "hostname")))
    (load-theme 'monokai t)
  (load-theme 'solarized t))
  (load-theme 'smart-mode-line-respectful t))

(add-hook 'after-init-hook #'my-set-themes-hook)
;; to setup tabs
(defvar c-basic-indent)
(setq c-basic-indent 4)
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode nil)

(defun my-insert-tabs ()
    "Insert tab for `tab' in text mode."
  (setq indent-line-function 'insert-tab))
(add-hook 'text-mode-hook #'my-insert-tabs)

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
(need-package 'smooth-scroll)
(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq smooth-scroll/vscroll-step-size 4)
(setq gc-cons-threshold 100000000)
(global-set-key [(control down)] #'(lambda () (interactive) (scroll-up-1 4)))
(global-set-key [(control up)]   #'(lambda () (interactive) (scroll-down-1 4)))
(global-set-key (kbd "C-v") #'(lambda () (interactive) (smooth-scroll/orig-scroll-up)))
(global-set-key (kbd "M-v") #'(lambda () (interactive) (smooth-scroll/orig-scroll-down)))
(global-set-key (kbd "<next>") #'(lambda () (interactive) (smooth-scroll/orig-scroll-up)))
(global-set-key (kbd "<prior>") #'(lambda () (interactive) (smooth-scroll/orig-scroll-down)))

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

;;
;; hydra
;;
(need-package 'hydra)
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
(need-package 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "C-c r") #'helm-flycheck)

;;;; Go mode
(setenv "GOPATH" "/home/feofan/go")
(setq exec-path (append exec-path '("~/go/bin")))
(need-package 'go-mode)
(require 'go-mode-autoloads)
(defun goimports ()
  "Running goimports on go files."
  (interactive)
  (if (s-equals-p mode-name "Go")
      (progn
        (shell-command "goimports -w *.go")
        (revert-buffer t t))))

(need-package 'company-go)

(declare-function go-remove-unused-imports "ext:go-mode")
(declare-function go-goto-imports "ext:go-mode")
(declare-function gobuild "ext:gobuild")
(declare-function gometalinter "ext:gometalinter")
(defvar company-begin-commands)
(defvar company-backends)

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
(need-package 'go-eldoc) ;; Don't need to require, if you install by package.el
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
(need-package 'anaconda-mode)
(add-hook 'python-mode-hook #'anaconda-mode)
(add-hook 'python-mode-hook #'eldoc-mode)
(need-package 'company-anaconda)

(setenv "PYMACS_PYTHON" "python2")
(setenv "PYTHONPATH" "/usr/bin/python2")
(autoload 'python-mode "python-mode.el" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(defvar python-indent)
(declare-function py-shift-right "ext:python-mode")
(declare-function py-shift-left "ext:python-mode")
(defun my-python-hook ()
  "Setup for python."
  (add-to-list 'company-backends 'company-anaconda)
  (setq indent-tabs-mode nil)
  (setq python-indent 4)
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
(need-package 'company)
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
(add-hook 'cider-repl-mode-hook #'paredit-mode)

(setq browse-url-browser-function #'browse-url-chromium)

;;;; Paredit
;; (need-package 'paredit)
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
(need-package 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; for over-80-chars line highlightning
;; (need-package 'column-enforce-mode)
;; (add-hook 'prog-mode-hook 'column-enforce-mode)
(need-package 'fill-column-indicator)
(require 'fill-column-indicator)
(set 'fci-rule-column 80)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(setq eval-expression-debug-on-error t)

;;;; Web developement
(need-package 'web-mode)
(need-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; (need-package 'react-snippets)
(defvar js2-highlight-level)
(setq js2-highlight-level 3)
(defvar js2-idle-timer-delay)
(setq js2-idle-timer-delay 2)
(setq blink-matching-paren nil)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(need-package 'ac-js2)
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


;; tern
;; (need-package 'tern)
;; (need-package 'company-tern)
;; (add-to-list 'company-backends 'company-tern)
;; (defvar company-tern-meta-as-single-line)
;; (setq company-tern-meta-as-single-line t)
;; (defvar company-tooltip-align-annotations)
;; (setq company-tooltip-align-annotations t)

;; (add-hook 'js2-mode-hook #'tern-mode)
;; (add-hook 'js-mode-hook #'tern-mode)
;; (add-hook 'web-mode-hook #'tern-mode)

(need-package 'js2-refactor)
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

(need-package 'skewer-mode)
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
(need-package 'emmet-mode)
(require 'emmet-mode)
(add-hook 'sgml-mode-hook #'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook #'emmet-mode)
(add-hook 'css-mode-hook #'emmet-mode) ;; enable Emmet's css abbreviation.
(setq emmet-move-cursor-between-quotes t) ;; default nil


;;
;; key chord
;;
(need-package 'key-chord)
(require 'key-chord)
(key-chord-mode 1)

;;
;; ace jump mode major function
;; 
;; (need-package 'ace-jump-mode)
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
(need-package 'avy)
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
(need-package 'expand-region)
(require 'expand-region)
(key-chord-define-global "zj" 'er/expand-region)
(key-chord-define-global "zk" 'er/contract-region)
(delete-selection-mode)

;;
;; multiple cursors
;;
(need-package 'multiple-cursors)
(require 'multiple-cursors)
(need-package 'ace-mc)

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
;; (need-package 'tagedit)
;; (eval-after-load "sgml-mode"
;;   '(progn
;;      (require 'tagedit)
;;      (tagedit-add-paredit-like-keybindings)
;;      (add-hook 'html-mode-hook #'tagedit-mode)))

;;
;; yasnippet
;;
(need-package 'yasnippet)
(require 'yasnippet)
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
(need-package 'helm)
(require 'helm-config)
(global-set-key (kbd "C-x C-x") #'helm-M-x)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-r") #'(lambda () (interactive)
                                  (progn
                                    (load-file "~/.emacs.d/init.el")
                                    (byte-recompile-file "~/.emacs.d/init.el"))))
(setq x-hyper-keysym 'meta)
;; ivy
(need-package 'ivy)
(need-package 'ivy-hydra)
(ivy-mode 1)
(defvar ivy-initial-inputs-alist)
(setq ivy-initial-inputs-alist nil)
(defvar ivy-re-builders-alist)
(setq ivy-re-builders-alist
      '((counsel-M-x . ivy--regex-fuzzy)
        ;; (t . ivy--regex-ignore-order)
        (t . ivy--regex-fuzzy)
        ))
(setq completion-in-region-function 'ivy-completion-in-region)
(need-package 'swiper)
(global-set-key "\C-s" #'swiper)
(global-set-key (kbd "C-c s k") #'ivy-resume)
(need-package 'counsel)
(need-package 'smex)
(need-package 'flx)
(need-package 'wgrep)
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
(need-package 'counsel-projectile)

(need-package 'helm-descbinds)
(require 'helm-descbinds)
(helm-descbinds-mode 1)

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

(global-set-key (kbd "C-:") #'my-counsel-company)

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
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

;; rebind F1 for xterm
;(global-set-key (kbd "M-o p") 'help)

;;
;; helm-swoop
;;
(need-package 'helm-swoop)
;; (need-package 'migemo)
;; (require 'migemo)
(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") #'helm-swoop)
(global-set-key (kbd "M-I") #'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") #'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") #'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") #'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") #'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") #'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") #'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") #'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") #'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; Match/Search methods (Fuzzy matching, Migemo)
;; If you do not preferr fuzzy, remove it from the list below
;; (defvar helm-c-source-swoop-match-functions
;;   '(helm-mm-exact-match
;;     helm-mm-match
;;     helm-fuzzy-match
;;     helm-mm-3-migemo-match))
;; (setq helm-c-source-swoop-search-functions
;;       '(helm-mm-exact-search
;;         helm-mm-search
;;         helm-candidates-in-buffer-search-default-fn
;;         helm-fuzzy-search
;;         helm-mm-3-migemo-search))

;; In addition of above, you need to enable migemo mode if you'd like to
;; (helm-migemo-mode 1)

;; restclient-mode
;; (require 'restclient)


;;;; Projectile
(need-package 'projectile)
(need-package 'helm-projectile)
(projectile-global-mode)
(defvar projectile-completion-system)
(setq projectile-completion-system 'ivy)
(helm-projectile-on)


;;;; Gnu global
(need-package 'helm-gtags)

;; key bindings
(defvar helm-gtags-mode-map)
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t d") #'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-t t") #'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-t r") #'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-t s") #'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-t u") #'helm-gtags-update-tags)
     (define-key helm-gtags-mode-map (kbd "M-t c") #'helm-gtags-create-tags)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") #'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") #'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") #'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") #'helm-gtags-pop-stack)))

;;;; OpenGrok
(need-package 'eopengrok)
(defvar eopengrok-jar)
(setq eopengrok-jar
      (expand-file-name "~/.emacs.d/opengrok/clj-opengrok-0.3.0-standalone.jar"))
(defvar eopengrok-ctags)
(setq eopengrok-ctags "/usr/bin/ctags")
(require 'eopengrok)
(defun my-opengrok-hook ()
  "Hook for eopengrok."
  (local-set-key (kbd "o") #'(lambda ()
                               (interactive)
                               (other-window 1))))
(add-hook 'eopengrok-mode-hook #'my-opengrok-hook)

;; speed-typing
(need-package 'speed-type)
(require 'speed-type)

;; magit
(need-package 'magit)
(require 'magit)

;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(defvar org-log-done)
(setq org-log-done t)
; ditaa
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t) (dot . t)))

;; indirect region
(defvar indirect-mode-name nil
      "Mode to set for indirect buffers.")

(make-variable-buffer-local 'indirect-mode-name)
(defun indirect-region (start end)
  "Edit the current region (between START & END) in another buffer.
If the buffer-local variable `indirect-mode-name' is not set, prompt
for mode name to choose for the indirect buffer interactively.
Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
        (mode
         (if (not indirect-mode-name)
             (setq indirect-mode-name
                   (intern
                    (completing-read
                     "Mode: "
                     (mapcar #'(lambda (e)
                                 (list (symbol-name e)))
                             (apropos-internal "-mode$" 'commandp))
                     nil t)))
           indirect-mode-name)))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))

(key-chord-define-global (kbd ";r") 'indirect-region)

;; helm flycheck
(need-package 'helm-flycheck)

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "Monaco" :foundry "FontForge" :slant normal :weight normal :height 90 :width normal)))))

;; pandoc
(need-package 'pandoc-mode)
(require 'pandoc-mode)
(add-hook 'markdown-mode-hook #'pandoc-mode)
(declare-function pandoc-load-default-settings "ext:pandoc")
(add-hook 'pandoc-mode-hook #'pandoc-load-default-settings)

;; guile support
(need-package 'geiser)
;; (add-hook 'geiser-repl-mode-hook #'paredit-mode)
;; (add-hook 'geiser-mode-hook #'paredit-mode)
(defvar geiser-chez-binary)
(setq geiser-chez-binary "chez-scheme")
(require 'geiser-impl)
(add-to-list 'geiser-active-implementations 'chez)

;; slime
(need-package 'slime)
(need-package 'slime-company)
(slime-setup '(slime-repl slime-company))

(defvar swank-kawa-jar "")
(defvar swank-kawa-cp "")
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
(need-package 'erlang)
(setq flycheck-erlang-include-path '("../include" "../deps"))

(defun fix-erlang-project-includes ()
  "Find erlang include paths for selected directory with project deps."
  (interactive)
  (let
      ((dir
        (expand-file-name (read-directory-name
                           "Select project directory:" default-directory))))
    (setq flycheck-erlang-include-path (append
                                        (s-split
                                         "\n"
                                         (shell-command-to-string
                                          (concat "find "
                                                  dir
                                                  "/*"
                                                  " -type d -name include"))
                                         t)
                                        (list dir
                                              (concat dir "/include")
                                              (concat dir "/deps")
                                              default-directory
                                              (concat
                                               (locate-dominating-file
                                                default-directory
                                                "src") "include")
                                              (concat
                                               (locate-dominating-file
                                                default-directory
                                                "src") "deps"))))))

(defun fix-erlang-project-code-path ()
  "Find erlang include paths for selected directory with project deps."
  (interactive)
  (let ((code-path
         (let
             ((dir
               (expand-file-name (read-directory-name
                                  "Select project directory:" default-directory))))
           (split-string (shell-command-to-string
                        (concat "find " dir " -type d -name ebin"))))
         ))
    (setq flycheck-erlang-library-path code-path)))
(need-package 'ivy-erlang-complete)
(require 'ivy-erlang-complete)
(defvar erlang-mode-map)
(defun my-erlang-hook ()
  "Setup for erlang."
  (define-key erlang-mode-map (kbd "C-:")
    'ivy-erlang-complete)
  (define-key erlang-mode-map (kbd "C-c C-h")
    'ivy-erlang-complete-show-doc-at-point)
  (define-key erlang-mode-map (kbd "C-c C-e")
    #'(lambda ()
        (interactive)
        (eopengrok-make-index-with-enable-projects
         (ivy-erlang-complete-set-project-root))))
  (define-key erlang-mode-map (kbd "C-c C-d")
    #'(lambda () (interactive)
        (if (ivy-erlang-complete-record-at-point)
            (eopengrok-find-text
             (concat "\""
                     (s-replace "#" "record("
                                (ivy-erlang-complete-thing-at-point))
                     "\""))
          (eopengrok-find-definition (ivy-erlang-complete-thing-at-point)))))
  (define-key erlang-mode-map (kbd "C-c C-r")
    #'(lambda () (interactive)
        (let ((thing (ivy-erlang-complete-thing-at-point)))
         (if (and (not (s-matches? ":" thing))
                  (-reduce (lambda (a b) (or a b))
                           (-map (lambda (s) (s-prefix? thing s))
                                   (ivy-erlang-complete--find-local-functions))))
            (eopengrok-find-reference
             (concat (file-name-base (buffer-file-name)) ":" thing))
          (eopengrok-find-reference thing)))))
  (define-key erlang-mode-map (kbd "C-c i")
    'fix-erlang-project-includes)
  (define-key erlang-mode-map (kbd "C-c b")
    'fix-erlang-project-code-path))
(add-hook 'erlang-mode-hook #'my-erlang-hook)
(add-hook 'after-save-hook #'ivy-erlang-complete-reparse)
(eval-after-load 'erlang (define-key erlang-mode-map (kbd "C-c C-s") nil))

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
(need-package 'link-hint)
(global-set-key (kbd "C-x u") #'link-hint-open-multiple-links)
(defun all-urls-in-buffer ()
  "Find all links."
  (interactive)
  (helm-swoop :$query "https?://"))
(global-set-key (kbd "C-x C-u") #'all-urls-in-buffer)

;;;; Lua
(need-package 'lua-mode)
(require 'lua-mode)

;; nXML mode customization
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))


;;;; C, C++ Development
;; Rtags
(need-package 'rtags)
(require 'rtags)
(setq rtags-completions-enabled nil)
(setq rtags-use-helm t)
;; completion
(need-package 'irony)
(need-package 'company-irony)
(need-package 'company-irony-c-headers)
;; (need-package 'company-c-headers)
(add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

;; show docs at point
(need-package 'xah-lookup)
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
  (local-set-key (kbd "C-c C-j") #'rtags-find-symbol-at-point)
  (local-set-key (kbd "C-c C-r") #'rtags-find-references-at-point)
  (local-set-key (kbd "C-'") #'company-irony-c-headers)
  (rtags-start-process-unless-running)
  (irony-mode)
  (add-to-list 'company-backends '(company-irony company-irony-c-headers)))
(add-hook 'c++-mode-hook #'my-cc-mode-hook)
(add-hook 'c-mode-hook #'my-cc-mode-hook)
;; Semantic
;; (require 'cc-mode)
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)
;; (global-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
;; (global-semantic-idle-summary-mode 1)

(need-package 'cmake-ide)
(cmake-ide-setup)

(need-package 'cmake-mode)
; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))
(need-package 'cmake-font-lock)
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
(need-package 'viking-mode)
(require 'viking-mode)
(viking-global-mode)

(show-paren-mode 1)

;;; Smartparens
(need-package 'smartparens)
(require 'smartparens-config)

(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'erlang-mode-hook #'smartparens-mode)

;;;; Java development
(need-package 'emacs-eclim)
(defun my-java-hook ()
  "Setup for java development."
  (smartparens-mode t)
  (require 'eclim)
  (eclim-mode)
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (require 'company-emacs-eclim)
  (company-emacs-eclim-setup)
  (require 'eclimd)
  (setq eclimd-wait-for-process nil)
  (if (not (eclimd--running-p))
      (start-eclimd (expand-file-name "~/java"))))

(add-hook 'java-mode-hook #'my-java-hook)

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
 '(frame-background-mode (quote light))
 '(geiser-default-implementation (quote guile))
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
 '(indent-line-function (quote insert-tab) t)
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
    (emmet-mode flx wgrep ivy-hydra darkokai-theme smart-mode-line monokai-theme clojure-mode cider counsel-projectile counsel cousel consel-ivy consel swiper ivy powerline async smooth-scroll link-hint helm-core avy ace-mc company-irony-c-headers company-irony flycheck yasnippet tern irony paredit-menu zenburn-theme web-mode tagedit sublime-themes speed-type solarized-theme smex smart-mode-line-powerline-theme slime-company rtags restclient react-snippets paredit pandoc-mode noflet nlinum multiple-cursors markdown-mode magit lua-mode key-chord json-rpc js3-mode jquery-doc ido-vertical-mode helm-themes helm-swoop helm-projectile helm-ls-git helm-gtags helm-flycheck helm-descbinds helm-company go-eldoc go-autocomplete geiser fuzzy fsm fill-column-indicator expand-region erlang company-tern company-quickhelp company-go company-c-headers company-anaconda column-marker column-enforce-mode color-theme-solarized color-theme-sanityinc-solarized cmake-ide auto-complete-clang ace-jump-mode ac-js2 ac-emmet ac-cider)))
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

(provide 'init)
;;; init.el ends here
