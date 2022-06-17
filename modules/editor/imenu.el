;;; editor/imenu ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Small tweaks to `imenu' behavior.

;;; Code:

(require 'core/keys)

(require 'imenu)

(setq imenu-auto-rescan t)

(use-package imenu-list
  :straight t)

;; `flimenu' will flatten out imenu buffers, making for
;; easier navigation.
(use-package flimenu
  :straight t
  :config
  (flimenu-global-mode 1))

(global-definer
  "i" '(imenu :wk "imenu"))

(provide 'editor/imenu)
;;; imenu.el ends here
