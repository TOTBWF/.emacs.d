;;; lang/emacs-lisp ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'use-package)

(require 'core/keys)
(require 'editor/snippets)

(create-file-template ".*.el$" "emacs-lisp-template" 'emacs-lisp-mode)

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(add-hook 'emacs-lisp-mode-hook #'company-mode)

(mode-leader-definer
  :keymaps 'emacs-lisp-mode-map
  "e" '(eval-last-sexp :wk "eval sexp")
  "E" '(eval-print-last-sexp :wk "print sexp")
  "l" '(eval-buffer :wk "load buffer")
  "L" '(emacs-lisp-native-compile-and-load :wk "load and compile buffer")
  "s" '(emacs-lisp-switch-to-repl :wk "switch to repl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edebug

(evil-collection-edebug-setup)

(mode-leader-definer
  :keymaps 'emacs-lisp-mode-map
  "i" '(edebug-eval-top-level-form :wk "instrument"))


(provide 'lang/emacs-lisp)
;;; emacs-lisp.el ends here
