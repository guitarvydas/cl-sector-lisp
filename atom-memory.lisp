(defun true () t)
(defun false () nil)

(defclass atom-memory ()
  ((bytes :initarg :bytes :accessor bytes)
   (current-atom-index :initform @NIL :accessor current-atom-index)
   (eof :initform (false) :accessor eof)))

(defmethod ?eof ((self atom-memory))
  (@null? (current-atom-index self)))

(defmacro exit-when (pred) `(when ,pred (return)))

(defmethod mem-end ((self atom-memory))
  (1- (length (bytes self))))

(defmethod mem-end? ((self atom-memory) index)
  (>= (@adjust-index index) (mem-end self)))

(defmethod advance-to-next-nil ((self atom-memory) index)
  (if (@null? index)
      (if (mem-end? self index)
          index
        (incf index))
    (advance-to-next-nil self (@cdr index))))

(defmethod @advance-to-next-atom ((self atom-memory))
  (let ((next-null-index (advance-to-next-nil self (@cdr (current-atom-index self)))))
    (if (>= next-null-index (mem-end self))
        (setf (current-atom-index self) next-null-index)
        (setf (current-atom-index self) (1+ next-null-index)))))

(defmethod ?current-atom-index ((self atom-memory))
  (current-atom-index self))

(defmethod ?match-string ((self atom-memory) s)
  ;; return (true) if every character of s matches the current atom
  ;; return false otherwise
  ;; don't advance the atom-index, simply test s against the current atom
  (match-string self s (current-atom-index self)))

(defun @NIL-as-char () (code-char @NIL))

(defun getc (i)
  (let ((n (@get i)))
    (if (eq 'character (type-of n))
        n
      (if (eq 'fixnum (type-of n))
          (code-char n)
        (assert nil)))))
      

(defmethod match-string ((self atom-memory) s atom-index)
  (if (and
       (@null? atom-index)
       (at-end s))
      (true)
    (let ((c-atom (getc atom-index)))
      (if (at-end s)
          (false)
        (let ((c-s (string-car s)))
          (if (char= c-s c-atom)
              (match-string self (string-cdr s) (@cdr atom-index))
            (false)))))))

(defun string-car (s)
  (char s 0))

(defun string-cdr (s)
  (subseq s 1))

(defun at-end (s)
  (= 0 (length s)))
      
(defun atest ()
  (initialize-memory)
  (let ((mem (make-instance 'atom-memory :bytes *memory*)))
    (let ((nileq (?match-string mem "NIL")))
      (let ((quoteeq0 (?match-string mem "QUOTE")))
        (format *standard-output* "NIL match success=~a ~a~%" nileq (current-atom-index mem))
        (format *standard-output* "QUOTE match success=~a ~a~%" quoteeq0 (current-atom-index mem))
        (let ()
          (@advance-to-next-atom mem)
          (let ((quoteeq (?match-string mem "QUOTE")))
            (format *standard-output* "QUOTE match success=~a ~a~%" quoteeq (current-atom-index mem))
            (values)))))))
