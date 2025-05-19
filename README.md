# Overview

This is my personal emacs set up as of May 19, 2025. I use the following packages:

-Magit - git management
-imenu-list - source code outline
-corfu-terminal - code autocomplete
-vterm
-treesitter - proper grammar parsing in Emacs, fixes code highlights and enables combobulate
-combobulate - structured code navgiation and editing
-eglot - LSP for code language analysis
-eglot-booster + emacs-lsp-booster - faster eglot performance
-gptel - simple and general LLM interface
-aidermacs - LLM with source code context for code-specific tasks

# Installation

Install the following system dependencies, then add the init.el content into your init.el (typically `~/.emacs` or `~/.emacs.d/init.el`)

### System Dependencies

1. `sudo apt install vterm-dev`
2. `git clone https://github.com/mickeynp/combobulate?tab=readme-ov-file  # note the dirpath`
3. Install the Jedi LSP: https://github.com/pappasam/jedi-language-server
4. `cargo install emacs-lsp-booster`
5. The AI features (gptel, aidermacs) assume you have a local LLM running via llama.cpp's `llama-server` (OpenAI API-compatible server) on `http://localhost:8080`

### Install the following packages in Emacs

1. Open emacs
2. `M-x package-install <RET> <package_name>`
3. Install these:
   1. `magit`
   2. `imenu-list`
   3. `corfu-terminal`
   4. `vterm`
   5. `gptel`
   6. `aidermacs`
4. Evalute these three expressions in Emacs with `M-:`
```
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
```
```
(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
```
```
(setq major-mode-remap-alist
 '((bash-mode . bash-ts-mode)
   (conf-toml-mode . toml-ts-mode)
   (css-mode . css-ts-mode)
   (go-mode . go-ts-mode)
   (json-mode . json-ts-mode)
   (javascript-mode . js-ts-mode)
   (js-mode . js-ts-mode)
   (js-json-mode . json-ts-mode)
   (json-mode . json-ts-mode)
   (js2-mode . js-ts-mode)
   (html-mode . html-ts-mode)
   (mhtml-mode . html-ts-mode)
   (python-mode . python-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (yaml-mode . yaml-ts-mode)))
```
5. Save the major mode remapping list in Emacs via `M-x customize-option <RET> major-mode-remap-alist <RET>` and hit the apply and save button
6. `M-x package-vc-install <RET> https://github.com/jdtsmith/eglot-booster <RET>`
7. Restart Emacs
8. Activate Corfu globally in Emacs: `M-x global-corfu-mode`

### Update your init.el

The `init.el` file is typically `~/.emacs` itself, or `~/.emacs.d/init.el`.

Copy the contents of the `init.el` from this repo onto your `init.el`.


# Emacs hotkey cheat sheet

I assume you know the basic Emac hot keys. These are the slightly more advanced onces that I often forget (some of these are remapped from the default via the `init.el` config).

### General hotkeys

`C-h b` - list all keyboard commands available in buffer
`M-m` - move to beginning of line, past indent tabs
`M-r` - move cursor to middle/top/bottom page
`M-a/e` - move to beginning/end of block
`C-M-a/e` - move to beginning/end of function definition
`C-M-u/d/n/p` - move up/down/next/prev parenthesis grouping
`C-x r SPC <register_id>` - set register
`C-x r j <register_id>` - jump cursor to register
`C-x r m` - set bookmark
`C-x r b` - open bookmark
`C-x r l` - list bookmark
`C-x C-x` - move to end/beginning of selected region
`F3` - start macro
`F4` - end macro / play back macro

### Combobulate hotkeys:

`M-p/n` - next/prev identifier
`C-c o t x` - replace same-text (region-selectable)
`C-c o t s` - replace sibling nodes
`C-c o t t` - replace same-type (region-selectable)

### LSP hotkeys:

`M-' ` - open imenu list (project structure)
`M-. ` - open definition
`M-/ ` - open docs of function
`M-? ` - find references
`M-g M-g` - show project errors window
`M-g M-n/p` - go to next/prev error

### AI hotkeys:

`C-c a a` - open Aidermacs main menu
`C-c a g` - open Gptel buffer menu
`C-c a RET` - send all text from beginning of buffer up until the cursor to Gptel
`C-c a DEL` - after highlighting text, this will open a buffer to prompt Gptel to rewrite the text (e.g. translate it to another language)
