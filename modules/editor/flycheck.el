;;; editor/flycheck ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; We use `flycheck' for our in-buffer error reporting. I tried `flymake' for a while
;; to see what vanilla Emacs had to offer, and it wasn't as good.

;;; Code:
(require 'core/straight)
(require 'core/keys)

(use-package flycheck
  :straight t
  :commands flycheck-mode)

(general-define-key
 :states '(normal motion)
 :keymaps 'flycheck-error-list-mode-map
 "q" 'quit-window)

(general-create-definer error-menu-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC e"
  "" '(:ignore t :wk "error"))

(error-menu-definer
  :keymaps 'flycheck-mode-map
  "e" '(flycheck-list-errors :wk "list errors")
  "j" '(flycheck-next-error :wk "next error")
  "k" '(flycheck-previous-error :wk "prev error"))

(provide 'editor/flycheck)
;;; flycheck.el ends here
