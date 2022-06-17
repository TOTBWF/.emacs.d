;;; editor/snippets ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; I use `yasnippet' for all my snippeting needs, and I use it pretty heavily!

;;; Code:
(require 'core/keys)

(require 's)

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :init
  (setq yas-indent-line 'fixed)
  :config
  (yas-global-mode 1))

(general-create-definer snippet-menu-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC s"
  "" '(:ignore t :wk "snippets"))

(snippet-menu-definer
  "n" '(yas-new-snippet :wk "new snippet")
  "i" '(yas-insert-snippet :wk "insert snippet")
  "e" '(yas-visit-snippet-file :wk "edit snippet"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Templates

;; We often find ourselves wanting some sort of header automatically
;; inserted at the top of a file. We can use `yasnippet' + `autoinsert'
;; to accomplish this.

(use-package autoinsert
  :straight t
  :custom
  (auto-insert-query nil)
  (auto-insert-alist nil)
  :preface
  (defun create-file-template (regex template mode)
    "Automatically insert the TEMPLATE snippet when REGEX match the file name."
    (add-to-list 'auto-insert-alist
		 `(,regex . [(lambda () (yas-expand-snippet (yas-lookup-snippet ,template ',mode)))])))
  :config
  ;; When we open a new file, automatically insert the file template
  (auto-insert-mode 1)
  (add-hook 'find-file-hook 'auto-insert))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet Helpers

;; When we are writing complicated snippets, we may want to use some elisp
;; to manipulate strings. This provides some common utilities that we
;; may want.

(defun camel-case (s)
  "Convert a snake_case string S into camelCase."
  (let* ((upcased (mapconcat 's-capitalize (s-split "_" s 'omit-nulls) ""))
         (head (substring upcased 0 1))
         (tail (substring upcased 1)))
    (concat (s-downcase head) tail)))

(provide 'editor/snippets)
;;; snippets.el ends here
