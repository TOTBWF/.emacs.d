;;; lang/c ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; C support is provided by `clangd'.

;;; Code:
(require 'core/keys)
(require 'editor/lsp)

(require 'man)
;; [FIXME: Reed M, 16/06/2022] This should really be deferred, but it's required
;; to set `lsp-clients-clangd-args'...
(require 'lsp-clangd)

(add-hook 'c-mode-hook 'lsp)

;; The default `man' command is super slugish, so let's define
;; a quick helper for opening the man page at the point.
(defun man-at-point ()
  "Open the manpage of the identifier at the current point."
  (interactive)
  (Man-getpage-in-background (Man-default-man-entry)))

(defun clightgen-file ()
  "Invoke `clightgen' on the current file."
  (interactive)
  (shell-command (format "clightgen -normalize %s" (buffer-file-name))))

(add-to-list 'lsp-clients-clangd-args "--enable-config")

(mode-leader-definer
  :keymaps 'c-mode-map
  "i" '(man-at-point :wk "man"))

(provide 'lang/c)
;;; c.el ends here
