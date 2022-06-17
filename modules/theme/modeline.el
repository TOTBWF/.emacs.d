;;; theme/modeline ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my modeline. Is it a bit shoogly? Yes. Do I still use it? yes.

;;; Code:

;; First, let's activate some modes that add some info to the mode line.
(setq display-time-default-load-average nil)
(display-time-mode 1)
(display-battery-mode 1)
(column-number-mode 1)

;; If we want some stuff right justified, we need to define a little helper function to compute the correct spacing.
(defun mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT,
aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defconst mode-line-saved-status
  '(:eval (if (and buffer-file-name (buffer-modified-p))
              (propertize "×" 'face '(:inherit error))
            " "))
  "Displays a red × when the buffer is not saved.")

(defconst mode-line-org-clock
  '(:eval
    (if org-clock-current-task
	(propertize org-mode-line-string 'face '(:inherit font-lock-string-face))
      (propertize "[No Task]" 'face '(:inherit error)))))


;; Mode Line Contents
(defconst mode-line-left
  '(" "
    mode-line-saved-status
    " %b "
    mode-name
    " "
    ;; Org
    ;; org-pomodoro-mode-line
    ;; TODO I don't love the look of this... Perhaps we could insert it somewhere else?
    " "
    display-time-string
    (:eval (s-replace "%" "%%%%" battery-mode-line-string))
    mode-line-org-clock)
  "The left justified portion of the modeline.")

(defconst mode-line-evil
  '(:eval
    (pcase evil-state
      ('insert "I")
      ('normal "N")
      ('visual "Vl")
      ('emacs "E")
      ('operator "O")
      ('motion "M")
      ;; Make it very obvious when I have missed a state
      (s (propertize (symbol-name s) 'face '(:inherit error)))))
  "Custom evil indicator for the mode line.
We need this to avoid weird padding issues.")

(defconst mode-line-right
  '(
    " "
    mode-line-position
    "<" mode-line-evil ">")
  "The right justified portion of the modeline.")

;; Back up the default modeline for reference.
;; We use defvar here to prevent reevaluation of the buffer from cloberring.
(defvar mode-line-backup mode-line-format)

;; Now, let's put everything together.
(setq-default mode-line-format
              '((:eval
                 (mode-line-render (format-mode-line mode-line-left) (format-mode-line mode-line-right)))))

(provide 'theme/modeline)
;;; modeline.el ends here
