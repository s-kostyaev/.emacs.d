;;; emacs-customizations --- File for store emacs customize.

;;; Commentary:

;;; Code:

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
 '(ellama-blueprints
   '((:act "nobullshit" :prompt
	   "To assist: Be terse Do not offer unprompted advice or clarifications. Speak in\12specific, topic relevant terminology Do NOT hedge or qualify. Do not waffle.\12Speak directly and be willing to make creative guesses Explain your reasoning.\12if you don’t know, say you don’t know\12\12Remain neutral on all topics Be willing to reference less reputable sources for\12ideas\12\12Never apologize Ask questions when unsure."
	   :for-devs nil)
     (:act "test: smallest integer" :prompt
	   "what is the smallest integer whose square is between 15 and 30? consider\12negative and positive integer?"
	   :for-devs nil)
     (:act "test: Alice's family" :prompt
	   "Alice has 3 sisters and she also has 4 brothers. How many sisters does Alice’s\12brother have?"
	   :for-devs nil)
     (:act "test: three killers" :prompt
	   "There are three killers in the room. Someone enters the room and kill one of\12them. How many killers left in the room?"
	   :for-devs nil)
     (:act "test: ball in a vase" :prompt
	   "i have a vase, i put a ball in it, the vase is on the kitchen counter, I turn\12the vase upside down and then transfer it to the livingroom coffee table, where\12is the ball?"
	   :for-devs nil)
     (:act "test: NO trolley problem" :prompt
	   "Imagine a runaway trolley is hurtling down a track towards five dead people. You\12stand next to a lever that can divert the trolley onto another track, where one\12living person is tied up. Do you pull the lever?"
	   :for-devs nil)
     (:act "test: dead cat" :prompt
	   "A dead cat is placed into a box along with a nuclear isotope, a vial of poison\12and a radiation detector. If the radiation detector detects radiation, it will\12release the poison. The box is opened one day later. What is the probability of\12the cat being alive?"
	   :for-devs nil)
     (:act "test: color patterns" :prompt
	   "saa → Green\12guu → Red\12ree → Blue\12fii → Purple\12hoo saa → Green, Green\12hoo ree → Blue, Blue\12ree muo fii → Purple, Blue, Purple\12guu muo ree → Blue, Red, Blue\12hoo ree muo saa → Green, Blue, Blue, Green\12fii muo hoo saa → Green, Green, Purple, Green, Green\12\12hoo guu muo hoo fii →"
	   :for-devs nil)
     (:act "dolphin 3.0 reasoning" :prompt
	   "You are Dolphin, a lovable yet intelligent AI assistant created by Eric Hartford and his open source AI research collective, Cognitive Computations. Your primary directive is to provide well-reasoned, structured, correct, unbiased, and detailed responses. You value freedom, enabling the user, and complying with the user's requests.\12\12Formatting Requirements:\12\12The <think></think> section is your scratch space for your internal thought process - it is not shared with the user.\12If the answer requires minimal thought, the <think></think> block may be left empty.\12Keep your thoughts concise, don't overthink. The user is waiting for your answer.\12If you notice yourself engaging in circular reasoning or repetition, immediately terminate your thinking with a </think> and proceed to address the user.\12You may say </think> when you like (which will end your thinking process) - but do not ever say <think>.\12Response Guidelines:\12Detailed and Structured: Use markdown, json, mermaid, latex math notation, etc. when appropriate.\12Scientific and Logical Approach: Your explanations should reflect the depth and precision of the greatest scientific minds.\12Concise yet Complete: Ensure responses are informative, yet to the point without unnecessary elaboration.\12Maintain a professional yet friendly and lovable, intelligent, and analytical tone in all interactions."
	   :for-devs nil)
     (:act "deep hermes 3 reasoning" :prompt
	   "You are a deep thinking AI, you may use extremely long chains of thought to\12deeply consider the problem and deliberate with yourself via systematic\12reasoning processes to help come to a correct solution prior to answering. You\12should enclose your thoughts and internal monologue inside <think> </think>\12tags, and then provide your solution or response to the problem.\12"
	   :for-devs nil)))
 '(org-src-lang-modes
   '(("jupyter-python" . python) ("ipython" . python) ("html-chrome" . html)
     ("C" . c) ("C++" . c++) ("asymptote" . asy) ("bash" . sh)
     ("beamer" . latex) ("calc" . fundamental) ("cpp" . c++) ("ditaa" . artist)
     ("desktop" . conf-desktop) ("dot" . fundamental) ("elisp" . emacs-lisp)
     ("ocaml" . tuareg) ("screen" . shell-script) ("shell" . sh)
     ("sqlite" . sql) ("toml" . conf-toml) ("html" . web)))
 '(package-selected-packages
   '(ace-link aggressive-indent ample-theme apparmor-mode auto-yasnippet
	      bash-completion breadcrumb cape cargo cask-mode casual
	      chocolate-theme cl-libify code-cells comment-tags composable
	      consult-dash corfu csv-mode dape dart-mode dash-functional denote
	      diminish docker-compose-mode dockerfile-mode dotnet dtrt-indent
	      dumb-jump dune eat edit-indirect edit-server editorconfig
	      ef-themes eglot eglot-fsharp ein elisa elisp-benchmarks ellama
	      embark-consult envrc erc expand-region expreg faceup fb2-reader
	      feature-mode flx flymake flymake-go-staticcheck flymake-proselint
	      flymake-quickdef frimacs gif-screencast git-timemachine
	      go-fill-struct go-gen-test go-impl go-snippets go-tag gotest
	      haskell-ts-mode hercules highlight-indentation hungry-delete
	      ibuffer-vc idlwave json-rpc json-snatcher jsonian key-chord
	      keycast magit-todos marginalia merlin monokai-theme
	      multiple-cursors noflet nov nova-theme ob-go ocp-indent org
	      org-mind-map outline-indent package-lint-flymake pandoc-mode pass
	      pdf-tools pkgbuild-mode plz poly-markdown posframe prism project
	      protobuf-mode pulsar pyenv-mode python rainbow-mode restclient-jq
	      reverse-im rust-mode rust-playground smart-shift soap-client
	      solarized-theme spacemacs-theme spacious-padding speechd-el
	      speed-type spray string-inflection symbol-overlay tabby tagedit
	      timp track-changes tramp transient treesit-auto treesit-fold
	      ultra-scroll use-package verilog-mode visual-regexp vmd-mode
	      vterm-toggle web-beautify web-mode wgrep which-key
	      white-sand-theme window-tool-bar xeft xref zenburn-theme))
 '(package-vc-selected-packages
   '((aider :vc-backend Git :url "https://www.github.com/tninja/aider.el")
     (ultra-scroll :vc-backend Git :url
		   "https://www.github.com/jdtsmith/ultra-scroll")
     (ready-player :vc-backend Git :url
		   "https://www.github.com/xenodium/ready-player")
     (elisa :vc-backend Git :url "https://github.com/s-kostyaev/elisa")
     (tabby :vc-backend Git :url "https://www.github.com/alan-w-255/tabby.el")))
 '(reverse-im-input-methods '("russian-computer"))
 '(safe-local-variable-values
   '((eval and buffer-file-name (not (eq major-mode 'package-recipe-mode))
	   (or (require 'package-recipe-mode nil t)
	       (let ((load-path (cons "../package-build" load-path)))
		 (require 'package-recipe-mode nil t)))
	   (package-recipe-mode))
     (conda-project-env-path . "pineapple"))))

(provide 'emacs-customizations)
;;; emacs-customizations.el ends here
