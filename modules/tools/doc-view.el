;;; tools/doc-view ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; I use `doc-view' for viewing PDFs. I know `pdf-tools' exists, but
;; IMO `doc-view' is more robust, and I don't ever do any PDF annotation
;; in Emacs anyways.

;;; Code:
(require 'evil-collection)
(require 'doc-view)

;; doc-view is super blurry unless you crank up the resolution.
(setq doc-view-resolution 300)
(evil-collection-doc-view-setup)

(provide 'tools/doc-view)
;;; doc-view.el ends here
