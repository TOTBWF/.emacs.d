;;; lang/latex ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'core/keys)
(require 'core/tweaks)

(use-package auctex
  :straight t
  :init
  ;; Make sure to add the TeX binaries to the path.
  (when (eq system-type 'darwin)
    (add-to-path "/Library/TeX/texbin/"))
  
  :config
  ;; This is a buffer-local variable, so we have to set it via hook...
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (setq TeX-command-extra-options "-shell-escape")
	      (setq TeX-engine 'luatex)))

  (add-to-list 'TeX-view-program-list '("zathura" "zathura %o" "zathura"))

  :general
  (mode-leader-definer
    :keymaps 'LaTeX-mode-map
    "c" '(TeX-command-master :wk "compile")
    "e" '(LaTeX-environment :wk "environment")
    "i" '(LaTeX-insert-item :wk "item"))
  (error-menu-definer
    :keymaps 'LaTeX-mode-map
    "j" '(TeX-next-error :wk "next error")
    "k" '(TeX-previous-error :wk "previous error"))

  (mode-leader-definer
    :keymaps 'flyspell-mode-map
    "s" '(flyspell-correct-word-before-point :wk "spellcheck")))

(use-package evil-tex
  :straight t
  :after auctex)

(provide 'lang/latex)
;;; latex.el ends here
