;; -*- lexical-binding: t -*-
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(load-theme 'dichromacy)
(global-font-lock-mode 1)
(show-paren-mode 1)

(electric-pair-mode 1)
(fido-mode 1)

(setq icomplete-delay-completions-threshold 0)
(setq icomplete-max-delay-chars 0)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-prospects-height 10)
(setq icomplete-separator "\n ")
(setq icomplete-with-completion-tables t)
(setq icomplete-in-buffer t)


(define-key icomplete-fido-mode-map (kbd "C-n") #'icomplete-forward-completions)
(define-key icomplete-fido-mode-map (kbd "C-p") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-j") #'minibuffer-force-complete)

(defun my-icomplete-yank-kill-ring ()
    "Insert the selected `kill-ring' item directly at point."
    (interactive)
    (let ((icomplete-separator
           (concat "\n" (propertize "..................." 'face 'shadow) "\n ")))
      (insert
       (completing-read "" kill-ring nil t))))

(global-set-key (kbd "M-y") 'my-icomplete-yank-kill-ring)

(global-set-key (kbd "C-;") #'hippie-expand)
(global-set-key (kbd "M-i") #'imenu)
(global-set-key "\C-cff" #'toggle-frame-fullscreen)
(fset 'yes-or-no-p 'y-or-n-p)

(flymake-mode 1)
(global-set-key (kbd "C-x `") #'flymake-goto-next-error)
(global-set-key (kbd "C-c r") #'flymake-show-diagnostics-buffer)

(global-set-key (kbd "M-p") #'other-window)

(defun my-set-themes ()
  "Hook for setting themes after init."
  (interactive)
  (let ((light-theme
         ;; 'tsdh-light
	 'dichromacy)
        (dark-theme
         ;; 'tsdh-dark
	 'misterioso)
        (cur-hour (nth 2 (decode-time))))
    (if (and (>  cur-hour 7)
             (<  cur-hour 20))
        (progn
          (disable-theme dark-theme)
          (load-theme light-theme t))
      (disable-theme light-theme)
      (load-theme dark-theme t))))

(my-set-themes)
