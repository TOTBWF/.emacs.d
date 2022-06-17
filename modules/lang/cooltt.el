;;; lang/cooltt ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; We load `cooltt' editor support from github, but we also keep around
;; the possibility of loading from our local repository.

;;; Code:
(require 'core/keys)

(require 'use-package)

(use-package cooltt
  :straight (cooltt :type git
		    :host github
		    :repo "RedPRL/cooltt"
		    ;; :local-repo "~/Documents/Projects/cooltt"
		    :files ("emacs/cooltt.el"))
  :config
  (setq cooltt-command "dune exec cooltt --")
  :general
  (mode-leader-definer
    :keymaps 'cooltt-mode-map
    "l" '(cooltt-compile-buffer :wk "load")))

(provide 'lang/cooltt)
;;; cooltt.el ends here
