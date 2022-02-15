(defun @read (raw-str mem)
  (let ((str (@trim-leading-spaces raw-str)))
    (if (string= "(" (@string-car str))
        (multiple-value-bind (str-leftovers lis)
            (@read-list (@string-cdr str) mem)
          (@need ")" str-leftovers))
      (@read-atom str mem))))

(defun @read-atom (str mem)
  (@intern str mem))

(defun @read-list (str mem)
  
  (assert nil)) ;; niy

(defun @string-car (s break-char)
  (let ((i (@search-char s break-char)))
    (@substring-upto s i)))

(defun @string-cdr (s break-char)
  (let ((i (@search-char s break-char)))
    (@substring-starting-at s i)))

(defun @search-char (s char)
  (search (list char) s))

(defun @substring-upto (s i)
  (subseq s 0 i))

(defun @substring-starting-at (s i)
  (subseq s i))

(defun @trim-leading-spaces (s)
  (@trim-leading-characters s #\Space))

(defun @trim-leading-characters (s char)
  (if (= 0 (@search-char s char))
      (@trim-leading-characters (@substring-starting-at s 1))
    s))

string-left-trim
string-right-trim
