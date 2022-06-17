;;; theme/tweaks ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Various UI tweaks that don't really belong anywhere else.

;;; Code:

;; I like a nice margin around the edges of the frame, makes things
;; feel a bit less crowded on large monitors.
(add-to-list 'default-frame-alist '(internal-border-width . 48))
(set-face-attribute 'default nil :family "Iosevka Fixed" :height 140)

;; `hl-todo' allows us to nicely highlight things like
;; TODO and FIXME so that we can see them easier.
(use-package hl-todo
  :straight t
  :config
  (global-hl-todo-mode))

;; Highlight the current error in the `next-error' buffer.
(setq next-error-highlight t)

(provide 'theme/tweaks)
;;; tweaks.el ends here
