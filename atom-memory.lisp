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

(defun getc (i)
  (let ((n (@get i)))
    (if (eq 'character (type-of n))
        n
      (if (eq 'fixnum (type-of n))
          (code-char n)
        (assert nil)))))
      
(defun sl-car (index)
  (char-code (getc index)))

(defmethod next-cell ((self atom-memory) cell-index)
  (declare (ignore self))
  (+ +cell-size+ cell-index))

(defmethod maxed-out ((self atom-memory) cell-index)
  (mem-end? self (next-cell self cell-index)))

(defmethod @advance-to-next-atom ((self atom-memory))
  (let ((cell-index (current-atom-index self)))
    ;; cell-index might be @NIL - assume that there are >0 characters at @NIL
    (if (maxed-out self cell-index)
        cell-index
      (let ()
        (setf cell-index (+ +cell-size+ cell-index))
        ;;;     while not (@null? (@cdr cell-index))
        ;;;         cell-index := (@cdr cell-index)
        ;;;     if maxed-out (cell-index)
        ;;;         return cell-index
        ;;;     else
        ;;;         return (@cdr cell-index)
        (loop while (not (@null? (@cdr cell-index)))
              do (setf cell-index (@cdr cell-index)))
        (setf (current-atom-index self)
              (if (maxed-out self cell-index)
                  cell-index
                (next-cell self cell-index)))))))

(defmethod ?current-atom-index ((self atom-memory))
  (current-atom-index self))

(defmethod ?match-string ((self atom-memory) s)
  ;; return (true) if every character of s matches the current atom
  ;; return false otherwise
  ;; don't advance the atom-index, simply test s against the current atom
  (match-string self s (current-atom-index self)))

(defun @NIL-as-char () (code-char @NIL))


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
          (format *standard-output* "next atom=~a~%" (current-atom-index mem))
          (let ((quoteeq (?match-string mem "QUOTE")))
            (format *standard-output* "QUOTE match success=~a ~a~%" quoteeq (current-atom-index mem))

            (@advance-to-next-atom mem)
            (@advance-to-next-atom mem)
            (let ((eqeq (?match-string mem "EQ")))
              (format *standard-output* "EQ match success=~a ~a~%" eqeq (current-atom-index mem))

              (values))))))))
