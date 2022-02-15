(defun @read(str)
  (let ((lstr @listify-string(str)))
    @lread(lstr)))

(defun @listify-string(s)
  (concatenate 'list s))

(defun @lread(raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (if (char= #\( (car lstr))
	(multiple-value-bind (result leftover)
	    (@read-list lstr)
	  (if (char= #\) (car leftover))
	      (values result (cdr leftover))
	      (@read-error (format nil "while reading list ~a, expected ')' but got ~a" raw-lster leftover))))
	(@lread-atom lstr))))

(defun @lread-list (raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (if (@empty lstr)
	@NIL
	(let ((front (@upto-separator lstr)))
	  (let ((tail (@after-separator lstr)))
	    (@cons (@lread front) (@lread-list tail)))))))

(defun @lread-atom (raw-lstr)
  (let ((lstr (@trim-leading-spaces raw-lstr)))
    (let ((front (@upto-separator lstr)))
      (let ((tail (@after-separator-inclusive lstr)))
