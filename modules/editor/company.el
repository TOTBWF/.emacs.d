;;; editor/company ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; We use `company-mode' for autocompletion.
;; It works very well, and requires basically no configuration.

;;; Code:
(require 'core/straight)

(use-package company
  :straight t
  :diminish company-mode
  :commands company-mode)

(provide 'editor/company)
;;; company.el ends here
