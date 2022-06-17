;;; core/keys --- Basic infrastructure for keybindings and evil  -*- lexical-binding: t; -*-

;;; Commentary:
;;; This is where we set up `evil' and `general', along with a bunch of handy
;;; utilities for defining keybindings.

;;; Code:
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General


;; We are going to use `general.el' for all of our keybinding needs
(use-package general
  :straight t
  :functions
  global-definer
  global-motion-definer
  model-leader-definer
  :config
  (general-evil-setup)
  (general-auto-unbind-keys))

(require 'general)

;; Create a `general' definer starting with 'SPC', which will be the base
;; for all of our other definers. By using the `override' keymap, keybindings
;; defined using this will override other keybindings.
(general-create-definer global-definer
  :keymaps 'override
  :states '(insert emacs normal hybrid motion visual operator)
  :prefix "SPC"
  :non-normal-prefix "C-SPC")

(global-definer
  "SPC" '(execute-extended-command :wk "execute command"))

;; TODO: Perhaps some macros for menu/mode definers would be nice

;; Create a "general" definer for vim motions in normal/visual/motion mode, which are prefixed
;; with 'g'.
(general-create-definer global-motion-definer
  :keymaps 'override
  :states '(normal motion visual operator)
  :prefix "g")

(general-create-definer mode-leader-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC m"
  "" '(:ignore t :which-key "mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

;; Let's load up `evil' for vim-keybindings
(use-package evil
  :straight t
  :init
  ;; NOTE: All of these need to get set _before_ evil loads,
  ;; as they have `defcustom` setters attached
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  ;; Use the emacs-28 undo/redo system (if possible)
  (unless (version< emacs-version "28")
    (setq evil-undo-system 'undo-redo))
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t)

;; `evil-nerd-commenter' let's us easily comment out lines with `g c c'.
(use-package evil-nerd-commenter
  :straight t
  :config
  :general
  (global-motion-definer
    "c" 'evilnc-comment-operator))

;; "which-key" lets us see what possible keybindings are available at any given time
(use-package which-key
  :straight t
  :diminish which-key-mode
  :config (which-key-mode))

(provide 'core/keys)
;;; keys.el ends here
