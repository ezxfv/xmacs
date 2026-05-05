;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       chinese

       :completion
       (vertico +icons)

       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       indent-guides
       (modeline +light)
       nav-flash
       ophints
       (popup +defaults)
       treemacs
       vc-gutter
       vi-tilde-fringe
       (window-select +switch-window +numbers)
       workspaces
       zen

       :editor
       (evil +everywhere)
       fold
       (format +onsave)
       multiple-cursors
       rotate-text
       snippets
       word-wrap

       :emacs
       (dired +icons +ranger)
       electric
       (ibuffer +icons)
       undo
       vc

       :term
       vterm

       :checkers
       (syntax +childframe)

       :tools
       (debugger +lsp)
       docker
       editorconfig
       (lookup +dictionary)
       (lsp +peek)
       magit
       make

       :os
       (:if IS-MAC macos)
       (tty +osc)

       :lang
       data
       emacs-lisp
       (go +lsp)
       (json +lsp)
       (javascript +lsp)
       lua
       (markdown +grip)
       (org +dragndrop +journal +pandoc +pretty +roam2)
       plantuml
       (python +lsp +ruff)
       rest
       (sh +lsp)
       yaml

       :config
       (default +bindings +smartparens +snippets +evil-commands)
       )
