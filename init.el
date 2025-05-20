(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (css-mode . css-ts-mode)
             (go-mode . go-ts-mode)
             (js-mode . js-ts-mode)
             (javascript-mode . js-ts-mode)
             (js-json-mode . json-ts-mode)
             (js2-mode . js-ts-mode)
	     (json-mode . json-ts-mode)
	     (html-mode . html-ts-mode)
	     (mhtml-mode . html-ts-mode)
             (python-mode . python-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("/home/kevin/workspace/combobulate")))






(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))


  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt) ;; Always preselect the prompt

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  
  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  
  ;; Enable auto completion and configure quitting
  (setq corfu-auto nil
      corfu-quit-no-match 'separator) ;; or t
  )

;; A few more useful Corfu configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Need to set this for Corfu to work in terminal mode
(unless (display-graphic-p)
  (corfu-terminal-mode +1))



(use-package eglot-booster
	:after eglot
	:config	(eglot-booster-mode))

(use-package markdown-mode
  :ensure t)

(setq eglot-ignored-server-capabilities '(:inlayHintProvider))
(setq eldoc-echo-area-prefer-doc-buffer t
      eldoc-echo-area-use-multiline-p nil)

(use-package vterm
    :ensure t)

(keymap-global-set "M-%" 'query-replace-regexp)
(keymap-global-set "C-s" 'isearch-forward-regexp)
(keymap-global-set "C-r" 'isearch-backward-regexp)

(global-set-key (kbd "M-l") nil)
(keymap-global-set "M-." 'xref-find-definitions-other-window)
(keymap-global-set "M-l M-l" 'imenu-list-smart-toggle)
(keymap-global-set "M-l M-r" 'eglot-rename)
(keymap-global-set "M-l M-e" 'flymake-show-project-diagnostics)
(keymap-global-set "M-l M-n" 'flymake-goto-next-error)
(keymap-global-set "M-l M-p" 'flymake-goto-prev-error)


(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)



;; Llama.cpp offers an OpenAI compatible API
(gptel-make-openai "llama-cpp"          ;Any name
  :stream t                             ;Stream responses
  :protocol "http"
  :host "localhost:8080"                ;Llama.cpp server location
  :models '(bartowski_Qwen2.5-Coder-32B-Instruct-GGUF_Qwen2.5-Coder-32B-Instruct-IQ4_XS.gguf))                    ;Any names, doesn't matter for Llama

(setq
 gptel-model   'bartowski_Qwen2.5-Coder-32B-Instruct-GGUF_Qwen2.5-Coder-32B-Instruct-IQ4_XS.gguf
 gptel-backend (gptel-make-openai "llama-cpp"
                 :stream t
                 :protocol "http"
                 :host "localhost:8080"
                 :models '(bartowski_Qwen2.5-Coder-32B-Instruct-GGUF_Qwen2.5-Coder-32B-Instruct-IQ4_XS.gguf)))

(keymap-global-set "C-c a g" 'gptel)
(keymap-global-set "C-c a RET" 'gptel-send)
(keymap-global-set "C-c a DEL" 'gptel-rewrite)
(keymap-global-set "C-c a +" 'gptel-add-file)

(use-package aidermacs
  :bind (("C-c a a" . aidermacs-transient-menu))
  :config
  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  ;; (setenv "ANTHROPIC_API_KEY" "sk-...")
  ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
  ;; (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
  (setq aidermacs-extra-args '("--openai-api-base" "http://localhost:8080" "--openai-api-key" "NONE"))
  
  :custom
  ; See the Configuration section below
  (aidermacs-default-model "openai/Qwen2.5-Coder-32B-Instruct")
  )





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(major-mode-remap-alist
   '((bash-mode . bash-ts-mode) (conf-toml-mode . toml-ts-mode)
     (css-mode . css-ts-mode) (go-mode . go-ts-mode)
     (json-mode . json-ts-mode) (javascript-mode . js-ts-mode)
     (js-mode . js-ts-mode) (js-json-mode . json-ts-mode)
     (json-mode . json-ts-mode) (js2-mode . js-ts-mode)
     (html-mode . html-ts-mode) (mhtml-mode . html-ts-mode)
     (python-mode . python-ts-mode)
     (typescript-mode . typescript-ts-mode) (yaml-mode . yaml-ts-mode)))
 '(package-selected-packages '(aidermacs eglot-booster gptel vterm yasnippet))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url
		    "https://github.com/jdtsmith/eglot-booster"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-blue ((t (:background "cornflowerblue" :foreground "cornflowerblue"))))
 '(ansi-color-bright-blue ((t (:background "cornflowerblue" :foreground "cornflowerblue"))))
 '(error ((t (:foreground "brightred" :weight bold)))))
