;; Mode Definition

(defun clean-paredit-check-parens ()
  "Evaluates to t when the parens in the current buffer are balanced, 
and false otherwise."
  (condition-case nil
      (progn (check-parens) t)
    (error nil)))

(defvar clean-paredit-mode-map (make-sparse-keymap)
  "Keymap for the clean-paredit minor mode.")

(define-minor-mode clean-paredit-mode
  "Minor mode for structurally editing Lisp and Scheme code.
Unlike classic Paredit, this one doesn't take prefix arguments.
For simplicity's sake, this mode cannot be enabled if there are
unbalanced parentheses in the buffer."
  :lighter " Clean-Paredit"
  (unless (clean-paredit-check-parens)
    (clean-paredit-disable)))

;; Inserting Parentheses

(defun clean-paredit-parser-state ()
  "Returns the state of the parser when starting from the most recent
top-level definition and ending at the current location of point."
  (let ((current-point (point)))
    (beginning-of-defun)
    (let ((beginning-of-defun-point (point)))
      (parse-partial-sexp beginning-of-defun-point current-point))))

(defun clean-paredit-syntax-class ()
  "Returns the kind of syntax structure that POINT is currently in,
as a symbol.
Examples of such symbols are comment, string, escape, and other."
  (let ((parser-state (clean-paredit-parser-state)))
    (cond ((nth 4 parser-state)            'comment)
          ((nth 3 parser-state)            'string)
          ((equal 9 (nth 10 parser-state)) 'escape)
          (t                               'other))))

(defun clean-paredit-enforce-delimiter-before-point ()
  "Insert a space before point if there is no left paren or whitespace
before point"
  (unless (memq (char-before) (cons ?\( clean-paredit-whitespace))
    (insert-char ?\ )))

(defun clean-paredit-enforce-delimiter-after-point ()
  "Insert a space after point if there is no right paren or whitespace
at point, but don't change the value of point."
  (unless (memq (char-after) (cons ?\) clean-paredit-whitespace))
    (insert-char ?\ )
    (backward-char)))

(defvar clean-paredit-whitespace '(?\  ?\n)
  "A list of whitespace characters.
Or are these list delimiters?")

(defun clean-paredit-open-round ()
  "Usually inserts a ( followed by an ).
In special cases only inserts (.
Special cases are: point is within string delimiterss,
point is within a comment, and point immediately follows
?\\"
  (if (memq (clean-paredit-syntax-class) '(comment string escape))
      (insert "(")
    (clean-paredit-enforce-delimiter-before-point)
    (clean-paredit-enforce-delimiter-after-point)
    (insert "()")
    (backward-char)))
