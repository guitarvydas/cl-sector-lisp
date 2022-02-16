(defun @read(str)
  (let ((lstr (@listify-string str)))
    (@lread lstr)))

(defun @listify-string(s)
  (concatenate 'list s))

(defun @lread(raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (if (not (null lstr))
      (if (char= #\( (car lstr))
          (multiple-value-bind (result leftover)
              (@lread-list (cdr lstr))
            (if (null leftover)
                (values result nil)
              (if (char= #\) (car leftover))
                  (values result (cdr leftover))
                (@read-error (format nil "while reading list ~a, expected ')' but got ~a" raw-lstr leftover)))))
	  (if (@is-follow-separator (car lstr))
	      (values nil nil)
              (@lread-atom lstr)))
      (values nil nil))))

(defun @lread-list (raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (if (not (null lstr))
        (multiple-value-bind (front leftover)
            (@lread lstr)
          (multiple-value-bind (more leftovers-from-leftovers)
              (@lread leftover)
            (values (%cons front more) leftovers-from-leftovers)))
      (values nil nil))))

(defun @lread-atom (raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (let ((front (@upto-separator lstr)))
      (let ((tail (@after-separator-inclusive lstr)))
        (if (null front)
            (values nil tail)
          (let ((astring (collapse-character-list-to-string front)))
            (let ((atom-index (@intern astring)))
              (values atom-index tail))))))))

(defun %cons (a b) (cons a b))
(defun %list (x) (cons x nil))
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
      (if (@is-separator (car lstr))
          nil
        (cons (car lstr) (@upto-separator (cdr lstr))))))

(defun @after-separator-inclusive (lstr)
  (@after-separator-inclusive-helper lstr))
(defun @after-separator-inclusive-helper (lstr)
  (if (null lstr)
      nil
      (if (@is-separator (car lstr))
          (cdr lstr)
        (@after-separator-inclusive-helper (cdr lstr)))))
(defun @is-separator (c)
  (or (char= c #\Space)
      (char= c #\()
      (char= c #\))))

(defun @is-follow-separator (c)
  (or (char= c #\Space)
      (char= c #\))))

(defun @empty(s) (= 0 (length s)))
(defun @read-error (s)
  (format *error-output* "~a~%" s)
  (assert nil))

(defun collapse-character-list-to-string (l)
  (format nil "~{~a~}" l))

(defun @intern (s)
  (intern s))

(defun @reverse (l)
  (reverse l))




(defun rtry-a (s)
  (multiple-value-bind (result leftover)
      (@lread-atom (@listify-string s))
    (format *error-output* "~s -> ~a ~a~%" s result leftover)))

(defun rtry-l (s)
  (multiple-value-bind (result leftover)
      (@lread-list (@listify-string s))
    (format *error-output* "~s -> ~a ~a~%" s result leftover)))

(defparameter *R* nil)
(defparameter *L* nil)

(defun rtry-r (s)
  (multiple-value-bind (result leftover)
      (@read s)
    (setf *R* result)  ;; debug
    (setf *L* leftover) ;; debug
    (format *error-output* "~s -> ~s ~s~%" s result leftover)))

(defun rtest ()
  (rtry-a "X")
  (rtry-r "Y")
  (rtry-r "(Z)")
  (rtry-r "(A B)")
  (rtry-r "((E))")
  (rtry-r "(F(G))")
  (values))

