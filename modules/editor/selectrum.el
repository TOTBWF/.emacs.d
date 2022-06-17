;;; editor/selectrum ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/keys)

(require 'use-package)

(use-package selectrum
  :straight t
  :config
  (selectrum-mode 1)
  ;; I find it annoying that C-backspace doesn't let me change directories,
  ;; so I've rebound it to `backward-kill-sexp'.
  (general-def
    :keymaps 'selectrum-minibuffer-map
    "<C-backspace>" 'backward-kill-sexp))

(use-package selectrum-prescient
  :straight t
  :after selectrum
  :config
  (selectrum-prescient-mode 1))

;; `ctrlf' is a handy way of searching within a buffer.
(use-package ctrlf
  :straight t
  :config
  (ctrlf-change-search-style 'fuzzy)
  :general

  ;; Use `C-j' and `C-k' for navigation.
  (general-def
    :keymaps 'ctrlf-mode-map
    "C-j" #'ctrlf-forward-literal
    "C-k" #'ctrlf-backward-literal)

  (general-def
    :keymaps 'override
    :states '(normal motion visual operator)
    "/" '(ctrlf-forward-literal :wk "search")))

(provide 'editor/selectrum)
;;; selectrum.el ends here
