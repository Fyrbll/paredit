;; Constructs

(defalias 'try-catch 'condition-case
  "Emacs Lisp's condition-case fulfils the same role as the try-catch
block in other languages eg. Java, I'm trying to use the more mainstream
name to make the code friendlier.")

;; Mode Definition

(defun clean-paredit-check-parens ()
  "Evaluates to t when the parens in the current buffer are balanced, 
and false otherwise."
  (try-catch e         ; `e` will store information on any error raised
                       ; within the following block.
      (progn
        (check-parens) ; Raise an error if there are unbalanced parens or
                       ; quotes in the buffer.
        t)             ; If no error is raised, all parens are balanced,
                       ; so evaluate to t.
    (error nil)))      ; In case an error is raised, evaluate to `nil`.

(defvar clean-paredit-mode-map (make-sparse-keymap)
  "Keymap for the clean-paredit minor mode.")

(define-minor-mode clean-paredit-mode
  "Minor mode for structurally editing Lisp and Scheme code.
Unlike classic Paredit, this one doesn't take prefix arguments.
For simplicity's sake, this mode cannot be enabled if there are
unbalanced parentheses in the buffer."
  :lighter " Clean-Paredit"
  (unless (clean-paredit-check-parens) ; Unless the parens and quotes in the
                                       ; buffer are balanced,
    (clean-paredit-disable)))          ; don't allow clean-paredit-mode to
                                       ; be enabled.

;; Inserting Parentheses

(defun clean-paredit-parser-state ()
  "Returns the state of the parser when starting from the most recent
top-level definition and ending at the current location of point.
Doesn't change the location of point."
  (let ((initial-point (point))) ; Save the cursor's present location.
    (beginning-of-defun)         ; Move cursor to the start of the most
                                 ; recent top-level definition.
    (parse-partial-sexp          ; Compute a parser state while
                                 ; moving the cursor from
     (point)                     ; the most recent top level definition
     initial-point)))            ; to its original position.

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
  (unless                          ; Unless
      (memq                        
       (char-before)               ; the char preceding the cursor is
       (cons                       
        ?\(                        ; a left parenthesis
        clean-paredit-whitespace)) ; or a whitespace character,
    (insert-char ?\ )))            ; insert a single space.

(defun clean-paredit-enforce-delimiter-after-point ()
  "Insert a space after point if there is no right paren or whitespace
at point, but don't change the value of point."
  (unless                          ; Unless
      (memq                        
       (char-after)                ; the character at the cursor is
       (cons             
        ?\)                        ; a right parenthesis
        clean-paredit-whitespace)) ; or a whitespace character,
    (insert-char ?\ )              ; insert a single space and
    (backward-char)))              ; move the cursor one space backward.

(defvar clean-paredit-whitespace '(?\  ?\n)
  "A list of whitespace characters. Or are these list delimiters?")

(defun clean-paredit-open-round ()
  "Usually inserts a ( followed by an ).
In special cases only inserts (.
Special cases are: point is within string delimiterss,
point is within a comment, and point immediately follows
?\\"
  (if (memq                         ; If
       (clean-paredit-syntax-class) ; the cursor is
       '(comment string escape))    ; in a syntax class where parens
                                    ; need not be balanced,
      (insert "(")                  ; insert a single left parenthesis.
                                    ; Otherwise,
                                    ; ensure that there's a space or (
                                    ; before the cursor, and
    (clean-paredit-enforce-delimiter-before-point) 
                                    ; ensure that there's a space or )
                                    ; after the cursor, finally
    (clean-paredit-enforce-delimiter-after-point)
    (insert "()")                   ; insert matching parens,
    (backward-char)))               ; and position the cursor between them.

;; Wrapping in Parentheses

(defun clean-paredit-wrap-round ()
  "Wraps the following sexp in parentheses."
  (insert-parentheses 1))

