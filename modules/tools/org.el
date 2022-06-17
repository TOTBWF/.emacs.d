;;; tools/org ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; This module is a bit of a beast. I use `org' pretty heavily, and the amount of
;; configuration reflects this!

;;; Code:

;; We set up a definer for all things that are
;; related to `org', so we can easily extend
;; the global bindings when we add other packages.
(general-create-definer notes-menu-definer
  :states '(normal motion)
  :wrapping global-definer
  :prefix "SPC n"
  "" '(:ignore t :wk "notes"))

(general-create-definer org-latex-definer
  :states '(normal motion)
  :keymaps 'org-mode-map
  :prefix "SPC m g"
  "" '(:ignore t :wk "latex"))

(use-package org
  :straight t
  ;; [FIXME: Reed M, 16/06/2022] This will slow down our load times,
  ;; but removes a lot of deferred loading jank that I don't want to
  ;; deal with solving.
  :ensure t
  :init
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq org-todo-keywords '((sequence
			     "TODO(t)"
			     "IDEA(i)"
			     "PROJ(p)"
			     "STRT(s)"
			     "WAIT(w)"
			     "REVW(r)"
			     "BLCK(b)"
			     "|"
			     "DONE(d)"
			     "KILL(k)")))

  ;; Use custom faces for our org keywords.
  (custom-declare-face 'org-todo-active '((t (:inherit (bold font-lock-string-face org-todo)))) "")
  (custom-declare-face 'org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face 'org-todo-onhold '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face 'org-todo-blocked '((t (:inherit (bold error org-todo)))) "")
  (custom-declare-face 'org-todo-idea '((t (:inherit (bold font-lock-builtin-face org-todo)))) "")

  (setq org-startup-folded 'content)
  (setq org-startup-indented t)
  :config

  ;; Make sure that `TAB' properly cycles visibility.
  (evil-collection-org-setup)

  (defun find-task-file ()
    "Open an org task file."
    (interactive)
    (let ((default-directory "~/org/"))
      (call-interactively 'find-file)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org Babel

  (org-babel-do-load-languages 'org-babel-load-languages '((haskell . t)
							   (emacs-lisp . t)
							   (ocaml . t)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; LaTeX

  ;; Let's enable LaTeX display by default
  (setq org-startup-with-latex-preview t)

  ;; We use `dvisvgm' for crisper LaTeX fragments display.
  (setq org-latex-create-formula-image-program 'dvisvgm)
  ;; I also find the default size a bit too small.
  (plist-put org-format-latex-options :scale 1.5)
  ;; Let's also add some packages
  (setq org-latex-packages-alist
	'(("" "tikz-cd" t)
	  ("" "tikz" t)
	  ("" "ebproof" t)
	  ("" "mlmodern" t)))

  ;; We need to tweak `org-format-latex-header' to add the `dvisvgm' argument
  ;; to the document class. This is required to make sure that tikz diagrams
  ;; get rendered properly.
  (setq org-format-latex-header
	(s-join "\n" '("\\documentclass[dvisvgm]{article}"
		       "\\usepackage[usenames]{color}"
		       "[PACKAGES]" "[DEFAULT-PACKAGES]"
		       "\\pagestyle{empty}             % do not remove"
		       "% The settings below are copied from fullpage.sty"
		       "\\setlength{\\textwidth}{\\paperwidth}"
		       "\\addtolength{\\textwidth}{-3cm}"
		       "\\setlength{\\oddsidemargin}{1.5cm}"
		       "\\addtolength{\\oddsidemargin}{-2.54cm}"
		       "\\setlength{\\evensidemargin}{\\oddsidemargin}"
		       "\\setlength{\\textheight}{\\paperheight}"
		       "\\addtolength{\\textheight}{-\\headheight}"
		       "\\addtolength{\\textheight}{-\\headsep}"
		       "\\addtolength{\\textheight}{-\\footskip}"
		       "\\addtolength{\\textheight}{-3cm}"
		       "\\setlength{\\topmargin}{1.5cm}"
		       "\\addtolength{\\topmargin}{-2.54cm}"
		       ;; The following bit is required so that we can use macros defined
		       ;; in the `latex_header'.
		       "[EXTRA]")))

  :general
  (org-latex-definer
    "g" '(org-latex-preview :wk "preview latex"))

  (mode-leader-definer
    :keymaps 'org-mode-map
    "l" '(org-insert-link :wk "insert link")
    "e" '(org-export-dispatch :wk "export")
    "o" '(org-open-at-point :wk "open at point")
    "t" '(org-todo :wk "change todo state")
    "w" '(org-refile :wk "refile")
    "'" '(org-edit-special :wk "edit block"))

  (mode-leader-definer
    :keymaps 'org-capture-mode-map
    "c" '(org-capture-finalize :wk "capture")
    "k" '(org-capture-kill :wk "cancel"))

  (mode-leader-definer
    :keymaps 'org-src-mode-map
    "c" '(org-edit-src-exit :wk "save")
    "k" '(org-edit-src-abort :wk "cancel"))

  (general-create-definer org-set-definer
    :wrapping mode-leader-definer
    :keymaps 'org-mode-map
    :prefix "SPC m x"
    "" '(:ignore t :wk "set")
    "p" '(org-set-property :wk "set property"))

  (general-create-definer org-clock-definer
    :wrapping mode-leader-definer
    :keymaps 'org-mode-map
    :prefix "SPC m c"
    "" '(:ignore t :which-key "clock"))

  (org-clock-definer
    "i" '(org-clock-in :wk "clock in")
    "o" '(org-clock-out :wk "clock out")
    "e" '(org-set-effort :wk "effort"))

  (notes-menu-definer
    "l" '(org-store-link :wk "store link")
    "o" '(org-clock-goto :wk "open current task")
    "s" '(org-clock-in :wk "clock in")
    "S" '(org-clock-in :wk "clock out")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Source Blocks
;;
;; I like to be able to have `diagram' and `latex' source blocks.

(defun render-tikz-make-standalone (tikz)
  "Wrap a TIKZ in a header/footer for latex rendering."
  (s-join "\n" `("\\documentclass[dvisvgm]{standalone}"
		 "\\usepackage{amsmath}"
		 "\\usepackage{amssymb}"
		 "\\usepackage{tikz}"
		 "\\usepackage{tikz-cd}"
		 "\\usepackage{ebproof}"
		 "\\begin{document}"
		 "\\special{color White}"
		 ,tikz
		 "\\end{document}")))

(defun render-latex-to-svg (tikz)
  "Render TIKZ to a standalone svg using `dvisvgm'"
  (let ((latex-file (make-temp-file "tikz-latex" nil nil (render-tikz-make-standalone tikz)))
	(default-directory (temporary-file-directory)))
    (call-process "latex" latex-file)
    (f-delete latex-file)
    (shell-command-to-string "dvisvgm --clipjoin --stdout -v0 texput.dvi")))

(use-package org-special-block-extras
  :straight t
  :after org
  :ensure t
  :hook (org-mode . org-special-block-extras-mode)
  :config
  (o-defblock "diagram" () ()
	      (if (equal backend 'html)
		  (format "<div class=\"diagram\">%s</div>"
			  (render-latex-to-svg
			   (s-replace
			    "\\begin{tikzcd}"
			    "\\begin{tikzcd}[color=white]"
			    raw-contents)))
		raw-contents))

  (o-defblock "latex" () ()
	      (if (equal backend 'html)
		  (format "<div class=\"latex\">%s</div>"
			  (render-latex-to-svg
			   raw-contents))
		raw-contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Roam
;;
;; This is the other big part of the `org' config. I find that
;; the tree model of `org' makes a terrible fit for notes,
;; as you spend a huge amount of time shuffling note heirarchies.
;; Instead, we want something that's more like a personal wiki, which
;; `org-roam' provides perfectly.

(use-package org-roam
  :straight t
  ;; [FIXME: Reed M, 16/06/2022] This will slow down our load times,
  ;; but removes a lot of deferred loading jank that I don't want to
  ;; deal with solving.
  :ensure t
  :init
  (setq org-roam-directory "~/org-roam")
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry
	   "* %?"
	   :target (file+head "%<%Y-%m-%d>.org"
			      "#+title: %<%Y-%m-%d>\n"))))

  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; PDF Management
  ;;
  ;; I like to keep track of PDFs with `org-roam', so I've cooked up a little
  ;; helper for adding the most recently downloaded PDF as an `org-roam' node.

  (defun latest-downloaded-pdf ()
    "Get the PDF file that was most recently downloaded."
    (car (-max-by (-on #'> (lambda (attrs) (time-convert (nth 5 attrs) 'integer)))
		  (directory-files-and-attributes "~/Downloads" t ".*.pdf" t))))

  (defconst org-roam-reference-template
    '("r" "reference"
      plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+ref-file: ${ref-file}")
      :unnarrowed t)
    "The `org-roam-capture' template we use when creating a new reference node.")

  (defun org-roam-reference (pdf name)
    "Move the most recently downloaded PDF file to `~/org/papers', and create an `org-roam' node for it."
    (interactive
     (let ((pdf (latest-downloaded-pdf)))
       (list pdf (read-from-minibuffer (format "Title [%s]: " pdf)))))
    (let ((final-pdf (f-join "~/org/papers/" (f-swap-ext name (f-ext pdf)))))
      (f-move pdf final-pdf)
      (org-roam-capture- :goto nil
			 :info `(:ref-file ,final-pdf)
			 :keys "r"
			 :templates (list org-roam-reference-template)
			 :node (org-roam-node-create :title name)
			 :props '(:immediate-finish nil))))

  (defun org-roam-on-reference ()
    (interactive)
    "If the `org-roam' node has a `ref-file' header argument, open the associated document."
    (if-let ((ref-file (org-roam-get-keyword "ref-file")))
	(find-file ref-file)))

  (add-hook 'org-roam-find-file-hook #'org-roam-on-reference)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Daily Notes

  (setq org-roam-capture-templates
	'(("d" "default"
	   plain "%?"
	   :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Agenda

  (defun org-roam-open-todo ()
    "Open the org roam agenda file."
    (interactive)
    (org-roam-node-visit (org-roam-node-from-title-or-alias "Agenda")))

  :general
  (mode-leader-definer
    :keymaps 'org-mode-map
    "n" '(org-roam-node-insert :wk "roam insert"))
  (notes-menu-definer
    "n" '(org-roam-node-find :wk "open note")
    "d" '(org-roam-dailies-goto-today :wk "open daily")
    "t" '(org-roam-open-todo :wk "open todos")
    "c" '(org-roam-capture :wk "capture")))

(provide 'tools/org)
;;; org.el ends here
