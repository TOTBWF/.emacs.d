;;; lang/coq ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Coq support via `proof-general'.

;;; Code:
(require 'core/keys)
(require 'core/tweaks)

(when (eq system-type 'darwin)
  (add-to-path "/Applications/Coq_Platform_2021.09.0.app/Contents/Resources/bin")
  (setq coq-library-directory "/Applications/Coq_Platform_2021.09.0.app/Contents/Resources/lib/coq"))

(use-package proof-general
  :straight t
  :config
  ;; No anime allowed!
  (setq proof-splash-enable nil)
  :general
  (mode-leader-definer 'coq-mode-map
    "l" '(proof-process-buffer :wk "load")
    "r" '(proof-retract-buffer :wk "retract")
    "j" '(proof-assert-next-command-interactive :wk "assert next")
    "k" '(proof-undo-last-successful-command :wk "undo command")
    "." '(proof-assert-until-point-interactive :wk "assert to point")
    "," '(proof-retract-until-point-interactive :wk "retract to point"))
  (global-motion-definer
    :keymaps 'coq-mode-map
    "j" '(proof-assert-next-command-interactive :wk "assert next")
    "k" '(proof-undo-last-successful-command :wk "undo command")))

(provide 'lang/coq)
;;; coq.el ends here
