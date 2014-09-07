(add-to-list 'load-path "~/.emacs.d")
(progn (cd "~/.emacs.d")
       (normal-top-level-add-subdirs-to-load-path))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(jabber-account-list (quote (("feofankss@jabber.tsk.ru" (:password . "180719893745") (:connection-type . ssl)))))
 '(jabber-backlog-days 300)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(tool-bar-mode nil))
(require 'color-theme)
(color-theme-initialize)
(color-theme-jsc-dark)
(global-linum-mode 1)
; carlo@r500:~/work/opt-lisp-r500/emacs-23.3$ ./configure --prefix=`pwd`/r500-build --with-x-toolkit=gtk
;
;
;
; Use VIM keybindings :)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)  
(evil-mode 1)
; SLIME for programming with Common Lisp.
; http://functionalrants.wordpress.com/2008/09/06/how-to-set-up-emacs-slime-sbcl-under-gnulinux/
(load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "/usr/bin/ccl")



;; to setup tabs
(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tabs-mode nil)

; (highlight-tabs)
; (highlight-trailing-whitespace)



;; Text and the such
;; Use colors to highlight commands, etc.
(global-font-lock-mode t) 
;; Disable the welcome message
(setq inhibit-startup-message t)
;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")
;; Display time
(display-time)
;; Make the mouse wheel scroll Emacs
(mouse-wheel-mode t)
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




(defun faces_x ()
  ;; these are used when in X
  (custom-set-faces
   '(default ((t (:foreground "wheat" :background "black"))))
   '(flyspell-duplicate ((t (:foreground "Gold3" :underline t :weight normal))))
   '(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t :weight normal))))
   '(font-lock-comment-face ((t (:foreground "SteelBlue1"))))
   '(font-lock-function-name-face ((t (:foreground "gold"))))
   '(font-lock-keyword-face ((t (:foreground "springgreen"))))
   '(font-lock-type-face ((t (:foreground "PaleGreen"))))
   '(font-lock-variable-name-face ((t (:foreground "Coral"))))
   '(menu ((((type x-toolkit)) (:background "light slate gray" :foreground "wheat" :box (:line-width 2 :color "grey75" :style released-button)))))
   '(mode-line ((t (:foreground "black" :background "light slate gray"))))
   '(tool-bar ((((type x w32 mac) (class color)) (:background "midnight blue" :foreground "wheat" :box (:line-width 1 :style released-button))))))
  (set-cursor-color "deep sky blue")
  (set-foreground-color "wheat")
  (set-background-color "black")
  (set-face-foreground 'default "wheat")
  (set-face-background 'default "black"))

(defun faces_nox ()
  ;; these are used when in terminal
  (custom-set-faces
   '(default ((t (:foreground "white" :background "black"))))
   '(font-lock-comment-face ((t (:foreground "magenta"))))
   '(font-lock-function-name-face ((t (:foreground "red"))))
   '(font-lock-keyword-face ((t (:foreground "green"))))
   '(font-lock-type-face ((t (:foreground "blue"))))
   '(font-lock-string-face ((t (:foreground "cyan"))))
   '(font-lock-variable-name-face ((t (:foreground "blue"))))
   '(menu ((((type x-toolkit)) (:background "white" :foreground "black" :box (:line-width 2 :color "grey75" :style released-button)))))
   '(modeline ((t (:foreground "blue" :background "white")))))
  (set-cursor-color "blue")
  (set-foreground-color "white")
  (set-background-color "black")
  (set-face-foreground 'default "white")
  (set-face-background 'default "black"))


; (if window-system (faces_x) (faces_nox))

; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
; huaiyuan's answer:
(set-face-attribute 'default nil :height 110)


; for compiling C/C++
(global-font-lock-mode t)
(global-set-key "\C-xs" 'save-buffer)
(global-set-key "\C-xv" 'quoted-insert)
(global-set-key "\C-xg" 'goto-line)
(global-set-key "\C-xf" 'search-forward)
(global-set-key "\C-xc" 'compile)
(global-set-key "\C-xt" 'text-mode);
(global-set-key "\C-xr" 'replace-string);
(global-set-key "\C-xa" 'repeat-complex-command);
(global-set-key "\C-xm" 'manual-entry);
(global-set-key "\C-xw" 'what-line);
(global-set-key "\C-x\C-u" 'shell);
(global-set-key "\C-x0" 'overwrite-mode);
(global-set-key "\C-x\C-r" 'toggle-read-only);
(global-set-key "\C-t" 'kill-word);
(global-set-key "\C-p" 'previous-line);
(global-set-key "\C-u" 'backward-word);
(global-set-key "\C-o" 'forward-word);
(global-set-key "\C-h" 'backward-delete-char-untabify);
(global-set-key "\C-x\C-m" 'not-modified);
(setq make-backup-files 'nil);
(setq default-major-mode 'text-mode)
(setq text-mode-hook 'turn-on-auto-fill)
(set-default-font "-misc-fixed-medium-r-normal--15-140-*-*-c-*-*-1")
(setq auto-mode-alist (cons '("\\.cxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hpp$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))

;(require 'font-lock)
;(add-hook 'c-mode-hook 'turn-on-font-lock)
;(add-hook 'c++-mode-hook 'turn-on-font-lock)

; http://nex-3.com/posts/45-efficient-window-switching-in-emacs#comments
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window



; Settings by Carlo Hamalainen:
(global-set-key [f8] 'save-buffer)  ; F8 saves the current buffer (file)
; (global-set-key [f9] 'compile)  
(global-set-key [f9] 'slime-compile-and-load-file)

;; http://emacs-fu.blogspot.com/2008/12/cycling-through-your-buffers-with-ctrl.html
;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)



; http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun iwb ()
    "indent whole buffer"
    (interactive)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max)))

(put 'downcase-region 'disabled nil)


;;jabber
(require 'jabber)
(global-set-key "\C-x\C-a" 'jabber-activity-switch-to)
;input history
(defvar my-jabber-input-history '() "Variable that holds input history")
(make-variable-buffer-local 'my-jabber-input-history)

(defvar my-jabber-input-history-position 0 "Current position in input history")
(make-variable-buffer-local 'my-jabber-input-history-position)

(defvar my-jabber-input-history-current nil "Current input value")
(make-variable-buffer-local 'my-jabber-input-history-current)

(defun my-jabber-input-history-hook (body id)
  (add-to-list 'my-jabber-input-history body t)
  (setq my-jabber-input-history-position (length my-jabber-input-history)))
(add-hook 'jabber-chat-send-hooks 'my-jabber-input-history-hook)

(defun my-jabber-previous-input ()
  (interactive)
  (let (current-input (pos my-jabber-input-history-position) (len (length my-jabber-input-history)))
    (if (= pos 0)
        (message "%s" "No previous input")
      (setq current-input (delete-and-extract-region jabber-point-insert (point-max)))
      (when (= pos len) ; running first time, save current input
          (setq my-jabber-input-history-current current-input))
      (decf my-jabber-input-history-position)
      (insert (nth my-jabber-input-history-position my-jabber-input-history)))))

(defun my-jabber-next-input ()
  (interactive)
  (let ((pos my-jabber-input-history-position) (len (length my-jabber-input-history)))
    (cond
     ((= pos (1- len)) ; pointing at the last element, insert saved input
       (incf my-jabber-input-history-position)
       (delete-region jabber-point-insert (point-max))
       (insert my-jabber-input-history-current)
       (setq my-jabber-input-history-current nil))
      ((= pos len)                              ; pointing beyound last element, notify user
       (message "%s" "No next input"))
      (t                                ; insert next history item
       (incf my-jabber-input-history-position)
       (delete-region jabber-point-insert (point-max))
       (insert (nth my-jabber-input-history-position my-jabber-input-history))))))

(define-key jabber-chat-mode-map (kbd "M-p") 'my-jabber-previous-input)
(define-key jabber-chat-mode-map (kbd "M-n") 'my-jabber-next-input)

;;; Python mode
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;; Octave mode
(autoload 'octave-mode "octave-mod" nil t)
          ;(setq auto-mode-alist
          ;      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
;; to turn on the abbrevs, auto-fill and font-lock features automatically  
       (add-hook 'octave-mode-hook
         (lambda ()
         (abbrev-mode 1)
         (auto-fill-mode 1)
         (if (eq window-system 'x)
          (font-lock-mode 1))))

;; And finally, inferior-octave-mode-hook is run after starting the process 
    ;; and putting its buffer into Inferior Octave mode. Hence, if you like 
    ;; the up and down arrow keys to behave in the interaction buffer as in 
    ;; the shell, and you want this buffer to use nice colors:
    
       (add-hook 'inferior-octave-mode-hook
         (lambda ()
           (turn-on-font-lock)
           (define-key inferior-octave-mode-map [up]
             'comint-previous-input)
           (define-key inferior-octave-mode-map [down]
             'comint-next-input)))
;; run an inferior Octave process in a special Emacs buffer
       (autoload 'run-octave "octave-inf" nil t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setq fsm-debug nil)


;;; Auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-clang)
(require 'ac-octave)
					;(require 'go-autocomplete)
(defun auto-complete-clang-get-system-includes ()
  (with-temp-buffer
    (shell-command 
     "echo | cpp -x c++ -Wp,-v 2>&1 | grep '^ .*include' | sed 's/^ //g'" 
     (current-buffer))
    (split-string (buffer-string) "\n" t)))

(ac-config-default)

(setq clang-completion-suppress-error t)
(setq ac-clang-flags
     (mapcar (lambda (item)(concat "-I" item))
             ;(c-get-system-includes)
	     (auto-complete-clang-get-system-includes)))



;; Generic setup.
(global-auto-complete-mode t)           ;enable global-mode
(setq ac-auto-start nil)                ;automatically start (disabled)
(setq ac-dwim t)                        ;Do what i mean
(setq ac-override-local-map nil)        ;don't override local map
(setq ac-quick-help-delay 1)
(setq ac-auto-show-menu 0.5)
(setq ac-ignore-case t)
(setq ac-delay 0.5)
(setq ac-use-fuzzy t)
(setq ac-use-comphist t)
;(setq ac-use-quick-help nil)

;; (setq ac-modes
;;       '(emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode
;;                         c-mode cc-mode c++-mode java-mode
;;                         perl-mode cperl-mode python-mode ruby-mode
;;                         ecmascript-mode javascript-mode php-mode css-mode
;;                         makefile-mode sh-mode fortran-mode f90-mode ada-mode
;;                         xml-mode sgml-mode
;;                         haskell-mode literate-haskell-mode
;;                         emms-tag-editor-mode
;;                         asm-mode
;;                         org-mode
;;                         lua-mode go-mode))

;; The sources for common all mode.
(custom-set-variables
 '(ac-sources
   '(
     ;ac-source-filename
     ac-source-files-in-current-dir
     ;ac-source-words-in-buffer
     ac-source-words-in-same-mode-buffers
     ;ac-source-yasnippet
     ;ac-source-words-in-all-buffer
     ;ac-source-gtags
     ;ac-source-imenu
     ;ac-source-abbrev
     ;ac-source-semantic
     ;ac-source-semantic-raw
     ;ac-source-ropemacs
     ;ac-source-symbols
     )))

;;; Lisp mode
(dolist (hook (list
               'emacs-lisp-mode-hook
               'lisp-mode-hook
               'lisp-interaction-mode-hook))
  (add-hook hook '(lambda ()
                    (add-to-list 'ac-sources 'ac-source-symbols))))

(add-hook 'haskell-mode-hook '(lambda ()
                                (add-to-list 'ac-sources 'ac-source-haskell)))

(add-hook 'c-mode-common-hook '(lambda ()
                                 (add-to-list 'ac-omni-completion-sources
                                              (cons "\\." '(ac-source-semantic)))
                                 (add-to-list 'ac-omni-completion-sources
                                              (cons "->" '(ac-source-semantic)))
                                 (add-to-list '(ac-source-clang ac-source-yasnippet) ac-sources)))


(add-hook 'c++-mode-hook '(lambda ()
                                 (add-to-list 'ac-omni-completion-sources
                                              (cons "\\." '(ac-source-semantic)))
                                 (add-to-list 'ac-omni-completion-sources
                                              (cons "->" '(ac-source-semantic)))
                                 (add-to-list '(ac-source-clang ac-source-yasnippet) ac-sources)))
;; (add-hook 'go-mode-hook '(lambda()
;;                            (add-to-list 'ac-sources 'ac-source-go)))


(ac-flyspell-workaround)

;(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "C-SPC") 'auto-complete)

;(defun ac-octave-mode-setup ()
;(setq ac-sources '(ac-source-octave)))
(add-hook 'octave-mode-hook
'(lambda ()
   (progn
   (add-to-list 'ac-sources 'ac-source-octave)
   (auto-complete-mode t))))
  ; (ac-octave-mode-setup)))
;; Python
;; to use autocomlete install ropemacs and pymacs
(add-hook 'python-mode-hook
          (lambda ()
	    (progn
	      (pymacs-load "ropemacs" "rope-")
	      (ac-ropemacs-initialize)
	      (add-to-list 'ac-sources 'ac-source-ropemacs))))



(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(jabber-auto-reconnect t)
 '(jabber-avatar-verbose nil)
 '(jabber-vcard-avatars-retrieve nil)
 '(jabber-chat-buffer-format "*-jabber-%n-*")
 '(jabber-history-enabled t)
 '(jabber-mode-line-mode t)
 '(jabber-roster-buffer "*-jabber-*")
 '(jabber-roster-line-format " %c %-25n %u %-8s (%r)")
 '(jabber-show-offline-contacts nil))



;;; ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


;;; Commenting
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
        "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
          (interactive "*P")
          (comment-normalize-vars)
          (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position))
            (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)
