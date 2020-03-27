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


(my-set-themes)
