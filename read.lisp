(defun @read(str)
  (catch 'read-failure
    (let ((lstr (@listify-string str)))
      (multiple-value-bind (result remaining)
          (@lread lstr)
        (let ((tail (@trim-leading-spaces remaining)))
          (cond
           ((null tail) (values result nil))
           ((@is-follow-separator (car tail) (throw 'read-failure (format nil "too many right parentheses in %s" str))))
           (t (values result tail))))))))

(defun @listify-string(s)
  (concatenate 'list s))

(defun @lread(raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (cond

     ((null lstr)
      (values nil nil))

     ((@is-begin-separator (car lstr))
      (multiple-value-bind (result-list leftover)
          (@lmap-read (cdr lstr))
        (@need-follow-separator leftover raw-lstr)
        (values result-list (cdr leftover))))

     ((@is-follow-separator (car lstr))
      (values nil lstr))

     (t (@lread-atom lstr)))))

(defun @lmap-read (raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (if (not (null lstr))
        (multiple-value-bind (front leftover)
            (@lread lstr)
          (let ((tail (@lread leftover)))
            (if (null front)
                (values nil tail)
              (values (%list front) tail))))
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
  (if (null lstr)
      nil
    (if (@is-separator (car lstr))
        lstr
      (@after-separator-inclusive (cdr lstr)))))

(defun @is-separator (c)
  (or (char= c #\Space)
      (char= c #\()
      (char= c #\))))

(defun @is-begin-separator (c)
  (char= c #\( ))

(defun @is-follow-separator (c)
  (char= c #\) ))

(defun @need-follow-separator (leftover original-string)
  (if (and (listp leftover) (not (null leftover)) (char= #\) (car leftover)))
      t
    (@read-error (format nil "*** ERROR: while reading list ~s, expected ')' but got ~s" original-string leftover))))


(defun @empty(s) (= 0 (length s)))
(defun @read-error (s)
  (format *error-output* "~a~%" s)
  (throw 'read-failure (values nil nil)))

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
      (@lmap-read (@listify-string s))
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
  (rtry-r "X")
  (rtry-r "Y")
  (rtry-r "(Z)")
  (rtry-r "(A B)")
  (rtry-r "((E))")
  (rtry-r "(F(G))")
  (values))

