;;; core/straight --- Bootsraping code for straight.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; `straight.el' bootstrapping code. Used to initialize the package management system.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

;; Load up `use-package', so we can declaratively manage packages.
(straight-use-package 'use-package)
(require 'use-package)

;; By default, the GC threshold for emacs is 800Kib, which is a bit low for initialization.
;; To avoid GC while we initialize, let's bump it up to 10Mib.
(setq gc-cons-threshold 10000000)

;; Restore the GC threshold after initialization is complete.
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; We want to load this relatively early so that we can restart emacs nicely
;; even if we make a mistake in our config.
(use-package restart-emacs
  :straight t)

(use-package benchmark-init
  :straight t
  :ensure t
  :config
  (benchmark-init/activate))

;; "diminish" lets us easily hide active modes from the modeline.
;; This seems like a bit of an odd place to load this, but
;; we use it absolutely everywhere, so we want it right
;; out the gate.
(use-package diminish
  :straight t
  :functions diminish
  :config
  (diminish 'global-whitespace-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core packages
;;
;; These packages are extremely common, so we load them first ourselves
;; so we can use them too!

(use-package dash
  :straight t)

(use-package f
  :straight t)

(use-package s
  :straight t)

(provide 'core/straight)
;;; bootstrap.el ends here
