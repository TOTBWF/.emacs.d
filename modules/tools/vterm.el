;;; tools/vterm ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; `vterm' is our terminal of choice: `eshell' is cool, but the IO is almost absurdly
;; slow, so we have to go with the less cool option.

;;; Code:

(use-package vterm
  :straight t
  :config
  (defun vterm-new-session ()
    (interactive)
    (vterm t))
  :general
  (open-menu-definer
    "t" '(vterm-new-session :wk "vterm")))

(provide 'tools/vterm)
;;; vterm.el ends here
