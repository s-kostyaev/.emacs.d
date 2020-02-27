;; -*- lexical-binding: t -*-
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(load-theme 'tsdh-light)
(global-font-lock-mode 1)
(show-paren-mode 1)

(electric-pair-mode 1)
(fido-mode 1)

(setq icomplete-delay-completions-threshold 0)
(setq icomplete-max-delay-chars 0)
(setq icomplete-compute-delay 0)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-prospects-height 1)
(setq icomplete-separator " · ")
(setq icomplete-with-completion-tables t)
(setq icomplete-in-buffer t)


(defun prot/icomplete-show-vertical (&optional str)
    "Allow `icomplete' to present results vertically.

This is meant to be used by other functions that need to show
their results as a vertical list, with an optional string marking
the demarcation line.

For an interactive version see `prot/icomplete-toggle-vertical'."
    (when (bound-and-true-p icomplete-mode)
      (setq icomplete-prospects-height 10)
      (if str
          (setq icomplete-separator
                (concat "\n" (propertize str 'face 'shadow) "\n "))
        (setq icomplete-separator "\n "))))

(defun prot/icomplete-yank-kill-ring ()
    "Insert the selected `kill-ring' item directly at point.

Defaults to a vertical layout.  This is restored on exit by means
of `prot/icomplete-restore-horizontal'."
    (interactive)
    (prot/icomplete-show-vertical "··········")
    (insert
     (completing-read "Yank from kill ring: " kill-ring nil t)))

(prot/icomplete-show-vertical)

(global-set-key (kbd "M-y") 'prot/icomplete-yank-kill-ring)
(define-key icomplete-fido-mode-map (kbd "C-n") #'icomplete-forward-completions)
(define-key icomplete-fido-mode-map (kbd "C-p") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-j") #'minibuffer-force-complete)

(global-set-key (kbd "C-;") #'hippie-expand)
(global-set-key (kbd "M-i") #'imenu)
(global-set-key "\C-cff" 'toggle-frame-fullscreen)
(fset 'yes-or-no-p 'y-or-n-p)
