;;; init --- The main init file  -*- lexical-binding: t; -*-

;;; Commentary:
;; My Emacs configuration is split up into small modules
;; that can `require' to load.
;;
;; I try to be as good as I can with deferred loading, but sometimes
;; life is just hard.

;;; Code:

;; Make sure that everything inside of our modules folder can actually
;; be loaded by Emacs.
(add-to-list 'load-path "~/.emacs.d/modules")

;; Core:
;; This loads the core functionality required to bootstrap the
;; rest of the configuration. This mostly involves bootstrapping `straight.el',
;; and getting `evil' + `general' all configured.
(require 'core/straight)
(require 'core/tweaks)
(require 'core/keys)

;; Editor:
;; The second load phase consists of editor features that don't need
;; to be loaded immediately.
(require'editor/company)
(require 'editor/default-bindings)
(require 'editor/flycheck)
(require 'editor/lsp)
(require 'editor/projectile)
(require 'editor/selectrum)
(require 'editor/snippets)

;; Languages:
;; Does what it says on the tin!
(require 'lang/agda)
(require 'lang/c)
(require 'lang/coq)
(require 'lang/cooltt)
(require 'lang/emacs-lisp)
(require 'lang/haskell)
(require 'lang/latex)
(require 'lang/ocaml)
(require 'lang/rust)

;; Tools:
;; These are distinguished from `editor' modules as they tend to provide
;; an interface to a single tool (git, a terminal, etc) as opposed to a core
;; editor feature.
(require 'tools/dired)
(require 'tools/doc-view)
(require 'tools/magit)
(require 'tools/org)
(require 'tools/vterm)

(require 'theme/modus)
(require 'theme/modeline)
(require 'theme/tweaks)

(provide 'init)
;;; init.el ends here
