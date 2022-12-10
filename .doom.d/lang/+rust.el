;;; lang/+rust.el -*- lexical-binding: t; -*-
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-format-on-save t))
