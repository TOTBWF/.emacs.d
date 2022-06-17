;;; lang/agda ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Agda is a bit of a tricky beast to handle with Emacs.
;; In general, `straight' doesn't manage it very well!
;; Therefore, we use some hacks to get the files using
;; the `agda-mode' binary.

;;; Code:
(require 'core/straight)
(require 'core/keys)
(require 'core/tweaks)

(require 'editor/snippets)

(require 'abbrev)
(require 'display-fill-column-indicator)

;; First, let's add some common locations to all the relevant PATH variables,
;; using our `add-to-path' helper from before.
(add-to-path "~/.local/bin/")
(add-to-path "~/.cabal/bin")

(create-file-template ".*.agda$" "agda-template" 'agda2-mode)

;; Now for the tricky part. `agda-mode' is a bit odd, as Agda ships with a bundle of elisp
;; files that need to match up with the version of Agda you've installed. This makes
;; `use-package' + `straight' not work very well, so we have to do this by hand.

;; First, let's define a little helper function that will use the `agda-mode' binary to
;; find the location of the elisp files.
(defun agda-mode-locate ()
  "Determine the location of the `agda2-mode' elisp files on your system."
  (condition-case _ (with-temp-buffer (call-process "agda-mode" nil t nil "locate")
                                    (buffer-string))
      (error (error "Could not find the `agda-mode' binary in your path. Do you have agda installed?"))))

;; Now, let's load up `agda-mode'
(load (agda-mode-locate))

;; Let's make sure to load up agda mode for literate agda markdown files.
(add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode))
(create-file-template ".*.lagda.md$" "lagda-template" 'agda2-mode)
(add-to-list 'projectile-globally-ignored-file-suffixes ".agdai")

;; Once that file is loaded, we apply our configuration, which mostly consists of keybindings.
(with-eval-after-load 'agda2-mode
  (require 'agda2-mode)

  (defun agda2-goal-and-context-normalized ()
    (interactive)
    (agda2-goal-and-context 4))

  (defun agda2-goal-and-context-and-inferred-normalized ()
    (interactive)
    (agda2-goal-and-context-and-inferred 4))

  (mode-leader-definer
    :keymaps 'agda2-mode-map
    "a" '(agda2-auto-maybe-all :wk "auto")
    "c" '(agda2-make-case :wk "case split")
    "l" '(agda2-load :wk "load")
    "n" '(agda2-compute-normalised-maybe-toplevel :wk "normalize")
    "e" '(agda2-elaborate-give :wk "elaborate")
    "i" '(agda2-search-about-toplevel :wk "info")
    "r" '(agda2-refine :wk "refine")
    "s" '(agda2-solve-maybe-all :wk "solve")
    "w" '(agda2-why-in-scope-maybe-toplevel :wk "describe scope")
    "o" '(agda2-module-contents-maybe-toplevel :wk "module contents")
    "," '(agda2-goal-and-context :wk "display goal")
    "<" '(agda2-goal-and-context-normalized :wk "display goal (normalized)")
    "." '(agda2-goal-and-context-and-inferred :wk "display type")
    ">" '(agda2-goal-and-context-and-inferred-normalized :wk "display type (normalized)"))

  (global-motion-definer
    :keymaps 'agda2-mode-map
    "d" '(agda2-goto-definition-keyboard :wk "goto definition")
    "j" '(agda2-next-goal :wk "next goal")
    "k" '(agda2-previous-goal :wk "previous goal"))

  (add-hook 'agda2-mode-hook
	    (lambda ()
	      (push '(?< . ("⟨" . "⟩")) evil-surround-pairs-alist)))

  (defun 1lab-toggle-fill ()
    (interactive)
    (if (eq fill-column 72)
	(set-fill-column 85)
      (set-fill-column 72))
    (redraw-frame))

  ;; TODO: Check the file type.
  (define-minor-mode 1lab-mode "Helpers for writing 1lab documents"
    :init-value nil
    :lighter "1Lab"
    :keymap (make-sparse-keymap)
    (set-fill-column 72)
    (display-fill-column-indicator-mode 1lab-mode))

  (mode-leader-definer
    :keymaps '1lab-mode-map
    "f" '(1lab-toggle-fill :wk "toggle fill"))

  (add-hook 'agda2-mode-hook #'1lab-mode))

(with-eval-after-load 'agda-input
  ;; Needed to silence the byte compiler.
  (declare-function agda-input-add-translations "agda-input")
  (agda-input-add-translations '(("hom" . "⇒")
				 ("lam" . "λ")
				 ("lam-" . "ƛ")
				 ("iso" . "≅")
				 ("embed" . "↪")
				 ("mono" . "↣")
				 ("epi" . "↠")
				 ("nat" . "ℕ")
				 ("int" . "ℤ")
				 ("alpha" . "α")
				 ("beta" . "β")
				 ("gamma" . "γ")
				 ("yo" . "よ")
				 ("inv" . "⁻¹")
				 ("monus" . "∸")
				 ("uu" . "⇑"))))

(require 'agda-input)

(provide 'lang/agda)
;;; agda.el ends here
