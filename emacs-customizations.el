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
	   :for-devs nil)
     (:act "Idea honing" :prompt
	   "Ask me one question at a time so we can develop a thorough, step-by-step spec\12for this idea. Each question should build on my previous answers, and our end\12goal is to have a detailed specification I can hand off to a developer. Let’s do\12this iteratively and dig into every relevant detail. Remember, only one question\12at a time.\12\12Here’s the idea:\12\12{idea}"
	   :for-devs t)
     (:act "Idea to spec" :prompt
	   "Now that we’ve wrapped up the brainstorming process, can you compile our\12findings into a comprehensive, developer-ready specification? Include all\12relevant requirements, architecture choices, data handling details, error\12handling strategies, and a testing plan so a developer can immediately begin\12implementation."
	   :for-devs t)
     (:act "Planning TDD" :prompt
	   "Draft a detailed, step-by-step blueprint for building this project. Then, once\12you have a solid plan, break it down into small, iterative chunks that build on\12each other. Look at these chunks and then go another round to break it into\12small steps. Review the results and make sure that the steps are small enough to\12be implemented safely with strong testing, but big enough to move the project\12forward. Iterate until you feel that the steps are right sized for this project.\12\12From here you should have the foundation to provide a series of prompts for a\12code-generation LLM that will implement each step in a test-driven manner.\12Prioritize best practices, incremental progress, and early testing, ensuring no\12big jumps in complexity at any stage. Make sure that each prompt builds on the\12previous prompts, and ends with wiring things together. There should be no\12hanging or orphaned code that isn't integrated into a previous step.\12\12Make sure and separate each prompt section. Use markdown. Each prompt should be\12tagged as text using code tags. The goal is to output prompts, but context, etc\12is important as well.\12\12{spec}"
	   :for-devs t)
     (:act "Planning non TDD" :prompt
	   "Draft a detailed, step-by-step blueprint for building this project. Then, once\12you have a solid plan, break it down into small, iterative chunks that build on\12each other. Look at these chunks and then go another round to break it into\12small steps. review the results and make sure that the steps are small enough to\12be implemented safely, but big enough to move the project forward. Iterate until\12you feel that the steps are right sized for this project.\12\12From here you should have the foundation to provide a series of prompts for a\12code-generation LLM that will implement each step. Prioritize best practices,\12and incremental progress, ensuring no big jumps in complexity at any stage. Make\12sure that each prompt builds on the previous prompts, and ends with wiring\12things together. There should be no hanging or orphaned code that isn't\12integrated into a previous step.\12\12Make sure and separate each prompt section. Use markdown. Each prompt should be\12tagged as text using code tags. The goal is to output prompts, but context, etc\12is important as well.\12\12{spec}"
	   :for-devs t)
     (:act "Code review" :prompt
	   "You are a senior developer. Your job is to do a thorough code review of this\12code. You should write it up and output markdown. Include line numbers, and\12contextual info. Your code review will be passed to another teammate, so be\12thorough. Think deeply before writing the code review. Review every part, and\12don't hallucinate."
	   :for-devs t)
     (:act "Create issue" :prompt
	   "You are a senior developer. Your job is to review this code, and write out the\12top issues that you see with the code. It could be bugs, design choices, or code\12cleanliness issues. You should be specific, and be very good. Do Not\12Hallucinate. Think quietly to yourself, then act - write the issues. The issues\12will be given to a developer to executed on, so they should be in a format that\12is compatible with github issues"
	   :for-devs t)
     (:act "Missing tests" :prompt
	   "You are a senior developer. Your job is to review this code, and write out a\12list of missing test cases, and code tests that should exist. You should be\12specific, and be very good. Do Not Hallucinate. Think quietly to yourself, then\12act - write the issues. The issues will be given to a developer to executed on,\12so they should be in a format that is compatible with github issues"
	   :for-devs t)
     (:act "test: contact rupture gestalt ru" :prompt
	   "Расскажи мне про срыв контакта в Гештальт терапии" :for-devs nil)
     (:act "Spec" :prompt
	   "Ask me one question at a time so we can develop a thorough, step-by-step spec\12for this idea. Each question should build on my previous answers, and our end\12goal is to have a detailed specification I can hand off to a developer. Let’s do\12this iteratively and dig into every relevant detail. Remember, only one question\12at a time. Use ask_user tool to collect data. Use write_file, append_file,\12edit_file and read_file tools to write result to {result_file_path}\12\12Here’s the idea:\12\12{idea}"
	   :for-devs t)
     (:act "improve clarity" :prompt
	   "Your GOAL is improve clarity.\12\12Read the document {path} using read_file tool. Use ask_user tool to improve\12clarity. Ask as many questions as needed until it will be crystal clear. Update\12the document using edit_file, append_file or write file tools."
	   :for-devs nil)
     (:act "spec to plan" :prompt
	   "Your GOAL is create detailed implementation plan based on spec {path_to_spec}.\12Plan should be formatted as a markdown checklist. Ask user questions to\12crystallize user's intent for any major decisions using ask_user tool.\12Save result plan into {result_file_path}.\12"
	   :for-devs t)))
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
	      difftastic diminish docker-compose-mode dockerfile-mode dotnet
	      dtrt-indent dumb-jump dune eat eca edit-indirect edit-server
	      editorconfig ef-themes eglot eglot-fsharp ein eldoc elisa
	      elisp-benchmarks ellama embark-consult envrc erc expand-region
	      expreg faceup fb2-reader feature-mode flx flymake
	      flymake-go-staticcheck flymake-proselint flymake-quickdef frimacs
	      gif-screencast git-timemachine go-fill-struct go-gen-test go-impl
	      go-snippets go-tag gotest haskell-ts-mode hercules
	      highlight-indentation hungry-delete ibuffer-vc idlwave json-rpc
	      json-snatcher jsonian jsonrpc key-chord keycast magit-todos
	      marginalia mcp merlin monokai-theme multiple-cursors noflet nov
	      nova-theme ob-go ocp-indent org org-mind-map outline-indent
	      package-lint-flymake pandoc-mode pass pdf-tools peg pkgbuild-mode
	      plz poly-markdown posframe prism project protobuf-mode pulsar
	      pyenv-mode python rainbow-mode restclient-jq reverse-im rust-mode
	      rust-playground smart-shift soap-client solarized-theme
	      spacemacs-theme spacious-padding speechd-el speed-type spray
	      string-inflection symbol-overlay tabby tagedit timp track-changes
	      tramp transient treesit-auto treesit-fold ultra-scroll use-package
	      verilog-mode visual-regexp vmd-mode vterm-toggle web-beautify
	      web-mode wgrep which-key white-sand-theme window-tool-bar xeft
	      xref yaml zenburn-theme))
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
