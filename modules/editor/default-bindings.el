;;; editor/default-bindings --- A set of default keybindings  -*- lexical-binding: t; -*-

;;; Commentary:
;; There are a lot of keybindings required for things like buffer management,
;; finding files, etc. This file collects them all into a single place.

;;; Code:
(require 'core/keys)

(require 'evil-collection)
(require 'general)
(require 'recentf)
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers

(general-create-definer buffer-menu-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC b"
  "" '(:ignore t :wk "buffer"))

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun open-scratch-buffer ()
  "Open the scratch buffer."
  (interactive)
  (display-buffer (get-buffer-create "*scratch*")))

(buffer-menu-definer
  "b" '(switch-to-buffer :wk "switch buffer")
  "d" '(kill-current-buffer :wk "kill buffer")
  "D" '(kill-other-buffers :wk "kill other buffers"))

(global-definer
  "," '(switch-to-buffer :wk "switch buffer")
  "x" '(open-scratch-buffer :wk "scratch buffer"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

(general-create-definer file-menu-definer
  :wrapping global-definer
  :prefix "SPC f"
  "" '(:ignore t :wk "file"))

(defun find-emacs-module ()
  "Open an Emacs configuration module."
  (interactive)
  (let ((default-directory "~/.emacs.d/modules/"))
    (call-interactively 'find-file)))

(defun find-package-module ()
  "Open a third-party Emacs module."
  (interactive)
  (let ((default-directory "~/.emacs.d/straight/repos/"))
    (call-interactively 'find-file)))

(defun open-init-file ()
  "Open the initialization file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Enable `recentf-mode' so that our helper function works.
(recentf-mode 1)

(defun recentf-find-file ()
  "Open a recently opened file."
  (interactive)
  (find-file (completing-read "Find Recent File: " recentf-list)))

(file-menu-definer
  "f" '(find-file :wk "find file")
  "r" '(recentf-find-file :wk "recent file")
  "i" '(open-init-file :wk "configuration file")
  "m" '(find-emacs-module :wk "find module")
  "s" '(save-buffer :wk "save file")
  "x" '(delete-file :wk "delete file"))

(global-definer
  "." '(find-file :wk "find file"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows

(general-create-definer window-menu-definer
  :wrapping global-definer
  :prefix "SPC w"
  "" '(:ignore t :wk "window"))

(window-menu-definer
  "w" '(ace-window :wk "select window")
  "h" '(evil-window-left :wk "left")
  "j" '(evil-window-down :wk "down")
  "k" '(evil-window-up :wk "up")
  "l" '(evil-window-right :wk "right")
  "v" '(evil-window-vsplit :wk "vertical split")
  "s" '(evil-window-split :wk "horizontal split")
  "d" '(evil-window-delete :wk "close")
  "o" '(delete-other-windows :wk "close other")
  "f" '(toggle-frame-fullscreen :wk "toggle fullscreen")
  "=" '(balance-windows :wk "balance windows"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help

(general-create-definer help-menu-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC h"
  "" '(:ignore t :wk "help"))

(use-package helpful
  :straight t
  :init
  ;; All of our elisp files are source controlled thanks to `straight',
  ;; so we need to set this to be able to easily navigate to them inside
  ;; of help buffers.
  (setq vc-follow-symlinks t)
  :config
  (evil-collection-init 'helpful)
  (setq describe-bindings-outline t)
  :general
  (help-menu-definer
    "d" '(toggle-debug-on-error :wk "toggle debugger")
    "f" '(helpful-callable :wk "describe function")
    "c" '(helpful-command :wk "describe command")
    "v" '(helpful-variable :wk "describe variable")
    "m" '(describe-mode :wk "describe mode")
    "F" '(describe-face :wk "describe face")
    "k" '(helpful-key :wk "describe key")
    "K" '(describe-keymap :wk "describe keymap")
    "i" '(info-display-manual :wk "open manual")
    "s" '(shortdoc-display-group :wk "open cheat sheet")
    "'" '(describe-char :wk "describe char")
    "d" '(toggle-debug-on-error :wk "toggle debug on error")))

(use-package elisp-demos
  :straight t
  :defer t
  :init
  (advice-add 'helpful-update :after 'elisp-demos-advice-helpful-update))

(evil-collection-init 'info)

;; Banish the annoying `HELLO' file!
(global-unset-key (kbd "C-h h"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open

(general-create-definer open-menu-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC o"
  "" '(:ignore t :wk "open"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Universal Argument

;; Because we will `universal-argument' behind a prefix,
;; we need to write a teeny wrapper function that
;; works when we have multiple `SPC u SPC u' in sequence.
(defun better-universal-argument ()
  "If `current-prefix-arg' is set, apply `universal-argument-more',
instead of `universal-argument'."
  (interactive)
  (if current-prefix-arg
      (universal-argument-more current-prefix-arg)
    (universal-argument)))

(global-definer
  "u" '(better-universal-argument :wk "universal"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Methods

(advice-add 'toggle-input-method
	    :after (lambda (&rest _)
		     (if-let ((method evil-input-method))
			 (message "Switched to %s input method." method)
		       (message "Switched to default input method."))))

(setq default-input-method "Agda")

(global-definer
  "'" '(toggle-input-method :wk "toggle input method"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

(evil-collection-compile-setup)
(evil-collection-xref-setup)
(evil-collection-custom-setup)

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode))

(global-motion-definer
  "s" #'sort-lines
  "=" #'align-regexp)

(electric-pair-mode)

(provide 'editor/default-bindings)
;;; default-bindings.el ends here
