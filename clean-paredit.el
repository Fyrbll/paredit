(defun clean-paredit-parens-balanced ()
  "Evaluates to t when the parens in the current buffer are balanced, 
and false otherwise."
  (condition-case x
      (progn (check-parens) t)
    (error nil)))

(defvar clean-paredit-mode-map
  (make-sparse-keymap)
  "Keymap for the clean-paredit minor mode.")

(define-minor-mode clean-paredit-mode
  "Minor mode for structurally editing Lisp and Scheme code.
Unlike classic Paredit, this one doesn't take prefix arguments.
For simplicity's sake, this mode cannot be enabled if there are
unbalanced parentheses in the buffer."
  :lighter " Clean-Paredit"
  (unless (clean-paredit-parens-balanced)
    (clean-paredit-disable)))
