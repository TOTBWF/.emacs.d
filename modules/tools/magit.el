;;; tools/magit ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Does `magit' even need an introduction?

;;; Code:
(require 'core/straight)
(require 'core/keys)

(use-package magit
  :straight t
  :commands magit-status magit-blame
  :config
  (evil-collection-magit-setup)
  :general
  (mode-leader-definer
   :keymaps 'with-editor-mode-map
   "c" '(with-editor-finish :wk "commit")
   "k" '(with-editor-cancel :wk "cancel")))

;; Show TODO, FIXME, etc in the `magit' buffer.
(use-package magit-todos
  :straight t
  :after magit
  :config
  (magit-todos-mode t))

;; Allow for time-travel within buffers. Not /technically/
;; `magit' related, but it's close enough. 
(use-package git-timemachine
  :straight t
  :commands git-timemachine)

(general-create-definer magit-menu-definer
  :wrapping global-definer
  :prefix "SPC g"
  "" '(:ignore t :wk "git"))

(magit-menu-definer
  "g" '(magit-status :wk "status")
  "s" '(magit-status :wk "status")
  "b" '(magit-blame :wk "blame")
  "t" '(git-timemachine :wk "timemachine"))

(general-define-key
 :keymaps 'git-timemachine-mode-map
 :states 'normal
 "C-j" '(git-timemachine-show-previous-revision :wk "previous revision")
 "C-k" '(git-timemachine-show-next-revision :wk "next revision")
 "q"   '(git-timemachine-quit :wk "quit timemachine"))

(provide 'tools/magit)
;;; magit.el ends here
