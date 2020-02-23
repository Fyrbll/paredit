;; Constants

(defconst cdredit-spaces " \t\n")

;; Helpers

;;;; Delimiter Correspondence

(defun cdredit-close-of (open)
  (cdr (assoc
    open
    '((?\( . ?\))
      (?\" . ?\")
      (?\[ . ?\])))))

;;;; Syntax Class at Point

(defun cdredit-syntax-class ()
  (let ((parser-state (parse-partial-sexp
                       (save-excursion
                         (beginning-of-defun)
                         (point))
                       (point))))
    (cond ((nth 4 parser-state)            'comment)
          ((nth 3 parser-state)            'string)
          ((equal 9 (nth 10 parser-state)) 'escape)
          (t                               'other))))

;;;; Insert Spaces

(defun cdredit-ensure-left-delimited ()
  (unless (seq-contains (concat "(" cdredit-spaces) (char-before))
    (insert-char ?\ )))

(defun cdredit-ensure-right-delimited ()
  (unless (seq-contains (concat ")" cdredit-spaces) (char-after))
    (progn (insert-char ?\ )
           (backward-char))))

;; Insert Balanced Pair

(defun cdredit-insert-pair (open)
  (interactive)
  (let ((close        (cdredit-close-of open))
        (syntax-class (cdredit-syntax-class)))
    (if (seq-contains '(comment string escape) syntax-class)
        (insert-char open)      
      (cdredit-ensure-left-delimited)
      (cdredit-ensure-right-delimited)
      (insert-char open)
      (insert-char close)
      (backward-char))))

(defun cdredit-insert-round ()
  (interactive)
  (cdredit-insert-pair ?\())

(defun cdredit-insert-double-quotes ()
  (interactive)
  (cdredit-insert-pair ?\"))

(defun cdredit-insert-square ()
  (interactive)
  (cdredit-insert-pair ?\[))

;; Forward

(defun cdredit-forward ()
  (interactive)
  (forward-sexp))

;;;; Up

(defun cdredit-forward-up ()
  (interactive)
  (backward-up-list)
  (forward-list))

;;;; Down

(defun cdredit-forward-down ()
  (interactive)
  (down-list))

;;;; Slurp

(defun cdredit-forward-slurp ()
  (interactive)
  (save-excursion
    (cdredit-forward-up)
    (backward-delete-char 1)
    (forward-sexp)
    (insert-char ?\))))

;;;; Barf

(defun cdredit-forward-barf ()
  (interactive)
  (save-excursion
    (cdredit-forward-up)
    (backward-delete-char 1)
    (backward-sexp)
    (insert-char ?\))))

;; Backward

(defun cdredit-backward ()
  (interactive)
  (backward-sexp))

;;;; Up

(defun cdredit-backward-up ()
  (interactive)
  (backward-up-list))

;;;; Down

(defun cdredit-backward-down ()
  (interactive)
  (backward-list)
  (forward-list)
  (backward-char))

;;;; Slurp

(defun cdredit-backward-slurp ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (delete-char 1)
    (backward-sexp)
    (insert-char ?\()))

;;;; Barf

(defun cdredit-backward-barf ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (delete-char 1)
    (forward-sexp)
    (insert-char ?\()))

;; Raise

(defun cdredit-raise ()
  (interactive)
  (let ((sexp-at-point (sexp-at-point)))
    (backward-up-list)
    (kill-sexp)
    (princ sexp-at-point (current-buffer))))

;; Splice

(defun cdredit-splice ()
  (interactive)
  (let ((opening-round (progn
                         (backward-up-list)
                         (point)))
        (closing-round (progn
                         (forward-sexp)
                         (1- (point)))))
    (goto-char closing-round)
    (delete-char 1)
    (goto-char opening-round)
    (delete-char 1)))

;; Keymap

(defvar cdredit-mode-map (make-sparse-keymap))

(let ((cdredit-mode-map-alist
       '(("(" . cdredit-insert-round)
         ("\"" . cdredit-insert-double-quotes)
         ("[" . cdredit-insert-square)
         ("C-M-f" . cdredit-forward)
         ("C-M-n" . cdredit-forward-up)
         ("C-M-d" . cdredit-forward-down)
         ("C-c s" . cdredit-forward-slurp)
         ("C-c b" . cdredit-forward-barf)
         ("C-M-b" . cdredit-backward)
         ("C-M-u" . cdredit-backward-up)
         ("C-M-p" . cdredit-backward-down)
         ("C-c S" . cdredit-backward-slurp)
         ("C-c B" . cdredit-backward-barf)
         ("M-r" . cdredit-raise)
         ("M-s" . cdredit-splice))))
  (dolist (x cdredit-mode-map-alist)
    (define-key cdredit-mode-map (kbd (car x)) (cdr x))))

;; Minor Mode

(define-minor-mode cdredit-mode ""
  nil
  " CDRedit"
  cdredit-mode-map)
