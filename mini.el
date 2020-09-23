;; -*- lexical-binding: t -*-
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(load-theme 'dichromacy)
(global-font-lock-mode 1)
(show-paren-mode 1)

(electric-pair-mode 1)

(require 'icomplete)
(fido-mode -1)
(icomplete-mode 1)

(setq-default icomplete-delay-completions-threshold 0)
(setq-default icomplete-max-delay-chars 0)
(setq-default icomplete-compute-delay 0)
(setq-default icomplete-separator "\n ")
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
      my-need-fix-bg t)

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



(my-set-themes)
