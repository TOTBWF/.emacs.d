;;; core/tweaks --- An assortment of tweaks to the core emacs experience  -*- lexical-binding: t; -*-

;;; Commentary:
;; This may seem a bit out of place, but we want to get some important
;; UI tweaking code loaded early in the init process so that we can
;; use it inside of things like `core/keys'.  Also included in this
;; file are UI tweaks that we want to apply ASAP, in case some other
;; part of the loading process fails.

;;; Code:
(require 'use-package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-Save

;; By default, emacs will fill your directories with trash by storing auto-save data
;; alongside the files, which is very annoying.
(setq backup-directory (expand-file-name "autosave" user-emacs-directory))
(setq backup-directory-alist `((".*" . ,backup-directory)))
(setq auto-save-file-name-transforms `((".*" ,backup-directory t)))

;; While we are at it, let's tweak the various auto-save
;; options.
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; I literally never edit files using 2 copies of emacs.
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface Tweaks

;; We also want to tweak how scrolling works so we can scroll line-by-line.
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)
(setq redisplay-skip-fontification-on-input t)
;; Avoid calling line-move-partial, increasing scroll speed.
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

;; Disable the window decorations
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Disable the bell ring (Who thought this was a good idea!?!?)
(setq ring-bell-function 'ignore)

;; Make "yes or no" prompts use "y" and "n" instead.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Of course we want this!
(setq enable-recursive-minibuffers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Native Compilation

;; Let's go with the fastest optimization level possible
;; that still adheres with the semantics of elisp (what a cursed sentence....)
(setq native-comp-speed 2)
;; The compilation warnings are very annoying, and spam you with things
;; like "docstring wider than 80 characters" which is really not
;; something I care about.
(add-to-list 'warning-suppress-types '(comp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exec Path
;; `exec-path-from-shell' is kind of cursed, so we roll our own solution.
;; We prefer to explicitly add paths that we need to emacs own PATH variable.

(defun add-to-path (path)
  "Add PATH to the variable `exec-path' and update $PATH.
This is used in place of `exec-path-from-shell' to avoid having
to start up a shell process, and is also more consistent."
  (let ((expanded-path (expand-file-name path)))
    (add-to-list 'exec-path expanded-path)
    (setenv "PATH" (concat expanded-path ":" (getenv "PATH")))))

(add-to-path "/usr/bin")
(add-to-path "/usr/local/bin")

(provide 'core/tweaks)
;;; tweaks.el ends here
