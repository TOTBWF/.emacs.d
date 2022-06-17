;;; tools/dired ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; I don't use `dired' very much, but I should.
;; The size of this module reflects that.

;;; Code:
(require 'core/keys)

(require 'dired)

(evil-collection-dired-setup)
(setq dired-kill-when-opening-new-dired-buffer t)

(provide 'tools/dired)
;;; dired.el ends here
