;;; editor/lsp ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; We use `lsp-mode' instead of `elgot' because it is more feature complete,
;; and actually takes care to do IO properly instead of the "simple" option
;; provided by `elgot'.

;;; Code:
(require 'use-package)

(require 'core/keys)

(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  ;; LSP servers tend to dump a /lot/ of output.
  ;; Therefore, it makes sense to bump up the maximum number of
  ;; bytes that we can read from a subprocess in a single chunk
  ;; to something much higher.
  (setq read-process-output-max (* 1024 16))
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-lens-enable nil)
  :config

  (defun kill-lsp-log-buffers ()
    "Kill all buffers but the current one.
Don't mess with special buffers."
    (interactive)
    (dolist (buffer (buffer-list))
      (when (s-match "lsp-log" (buffer-name buffer))
	(kill-buffer buffer))))

  ;; Ensure that the `*lsp-help*` buffer appears at the bottom of the frame.
  (add-to-list 'display-buffer-alist
	       '("\\*lsp-help\\*"
		 (display-buffer-below-selected display-buffer-at-bottom)
		 (inhibit-same-window . t)
		 (window-height . 15)))
  :general
  (mode-leader-definer
    :keymaps 'lsp-mode-map
    "a" '(lsp-execute-code-action :wk "code action"))
  (global-motion-definer
    :keymaps 'lsp-mode-map
    "a" '(lsp-execute-code-action :wk "code action")
    "d" '(lsp-find-definition :wk "goto definition")
    "i" '(lsp-find-implementation :wk "goto implementation")
    "r" '(lsp-find-references :wk "find references")
    "D" '(xref-pop-marker-stack :wk "go back"))
  (general-define-key
   :states 'normal
   :keymaps 'lsp-mode-map
   "K" 'lsp-describe-thing-at-point))

(provide 'editor/lsp)
;;; lsp.el ends here
