;;; lang/rust ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/straight)
(require 'core/tweaks)
(require 'editor/lsp)

(add-to-path "~/.cargo/bin")

(use-package rust-mode
  :straight t
  :config
  (add-hook 'rust-mode-hook #'lsp)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; We disable this error, as it doesn't play nice with nalgebra.
  (setq lsp-rust-analyzer-diagnostics-disabled (vector "mismatched-arg-count")))

(provide 'lang/rust)
;;; rust.el ends here
