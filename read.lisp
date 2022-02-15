(defun @read(str)
  (let ((lstr (@listify-string str)))
    (@lread lstr)))

(defun @listify-string(s)
  (concatenate 'list s))

(defun @lread(raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (if (char= #\( (car lstr))
	(multiple-value-bind (result leftover)
	    (@lread-list lstr)
	  (if (char= #\) (car leftover))
	      (values result (cdr leftover))
	      (@read-error (format nil "while reading list ~a, expected ')' but got ~a" raw-lstr leftover))))
	(@lread-atom lstr))))

(defun @lread-list (raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (if (@empty lstr)
	(%NIL)
	(let ((front (@upto-separator lstr)))
	  (let ((tail (@after-separator-inclusive lstr)))
	    (%cons (@lread front) (@lread-list tail)))))))

(defun @lread-atom (raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (let ((front (@upto-separator lstr)))
      (let ((tail (@after-separator-inclusive lstr)))
	(let ((atom-index (@lread front)))
	  (values atom-index tail))))))

(defun %cons (a b) (cons a b))
(defun %NIL () nil)
(defun @trim-leading-spaces (lstr)
  (if lstr
      (if (char= #\Space (car lstr))
	  (@trim-leading-spaces (cdr lstr))
	  lstr)
      nil))
(defun @upto-separator (lstr)
  (if (null lstr)
      nil
      (if (char= (car lstr) #\Space)
	  (@upto-separator (cdr lstr))
	  (if (or (char= (car lstr) #\() (char= (car lstr) #\)))
	      (@upto-separator (cdr lstr))
	      lstr))))
(defun @after-separator-inclusive (lstr)
  (@after-separator-inclusive-helper lstr nil))
(defun @after-separator-inclusive-helper (lstr accumulator)
  (if (null lstr)
      nil
      (if (char= (car lstr) #\Space)
	  (@after-separator-inclusive-helper (cdr lstr) accumulator)
	  (if (or (char= (car lstr) #\() (char= (car lstr) #\)))
	      accumulator
	      (@after-separator-inclusive-helper (cdr lstr) (cons (car lstr) accumulator))))))
(defun @empty(s) (= 0 (length s)))
(defun @read-error (s)
  (format *error-output* "~a~%" s)
  (assert nil))

(defun rtest ()
  (@read "x"))