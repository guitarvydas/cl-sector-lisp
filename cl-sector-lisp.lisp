;; memory is 256 cells, cell[0] is NIL, cell[<0]->List, cell[>0]->Atom
(defparameter *memory* nil)

(defstruct @
  (car cdr))

(defconstant +cell-size+ 2) ;; each cell is 2 units long (in Sector Lisp, unit=byte)

(defun @adjust-index (i) (+ 256 i))

(defun @fetch-cell (adjusted-index)
  (nth *memory* adjusted-index)) ;; could be implemented with AREF

(defun @get (index)
  (let ((ix (@adjust-index i)))
    (@fetch-cell ix)))

(defun @put (index vcar vcdr)
  (let ((ix (@adjust-index index)))
    (let ((cell (@fetch-cell ix)))
      (setf (@car cell) vcar)
      (setf (@cdr cell) vcdr))))

;; initialize memory
(let ((i 0))
  (loop while (< i 256)
	do (@put i 0 0)
	do (incf i)))

(defparameter *next-free-list-cell* -1)
(defparameter *next-free-atom* 0)

(defconstant kNil 0)
(defconstant @NIL kNil)

(defun @putatom (chars)
  (when chars
    (let ((atom-index *next-free-atom*))
      (let ((next-atom-index (+ +cell-size+ atom-index)))
	(@put atom-index (car chars) next-atom-index)
	(setf *next-free-atom* next-atom-index)
	atom-index))))
  
(@putatom '(#\N #\I #\L))

(defconstant kQuote (@putatom '(#\Q #\U #\O #\T #\E)))
(defconstant kCond  (@putatom '(#\C #\O #\N #\D    )))
(defconstant kEq    (@putatom '(#\E #\Q            )))
(defconstant kCons  (@putatom '(#\C #\O #\N #\S    )))
(defconstant kAtom  (@putatom '(#\A #\T #\O #\M    )))
(defconstant kCar   (@putatom '(#\C #\A #\R        )))
(defconstant kCdr   (@putatom '(#\C #\D #\R        )))

  
;;;;;;;;;;;;; basic functions

(defun @cons (vcar vcdr)
  (let ((index *next-free-list-cell*))
    (decf *next-free-list-cell*)
    (putcell (index vcar vdr))))

(defun @car (index)
  (@car (@get index)))

(defun @cdr (index)
  (@cdr (@get index)))


(defun @eq (index-A index-B)
  (= index-A index-B))


;; evaluator
(defun @eval (e env)
  (let ((previous-SP *next-free-cell*))
    (cond
      ((= e 0) 0)
      ((eq (@car e) kQuote) (@car (@cdr e)))
      ((eq (@cdr e) kCond) (@evcon (@cdr e) env))
      ((> e 0) (@assoc e env))
      (t (let ((v (@apply (@car e) (@evlis e) env)))
	   (@gc previous-SP v env))))))

(defun @apply (f args env)
  ;; apply function f to a *list* of values (args) in given environment
  (cond

    ((< f 0) 
     (let ((new-env (@pailis args env))) 
       (@eval (@car (@cdr (@cdr f)) new-env))))

    ((eq f kEQ) 
     (let ((first-arg (@car args))
	   (second-arg (@car (@cdr args))))
       (@eq first-arg second-arg)))

    ((eq f kCons) 
     (let ((first-arg (@car args))
	   (second-arg (@car (@cdr args))))
       (@cons first-arg second-arg)))

    ((eq f kAtom) 
     (let ((first-arg (@car args)))
       (>= first-arg 0)))

    ((eq f kCar) 
     (let ((first-arg (@car args)))
       (@car first-arg)))

    ((eq f kCdr) 
     (let ((first-arg (@car args)))
       (@cdr first-arg)))

    (t
     (let ((anonymous-function (@assoc f env)))
       (@apply anonymous-function args env)))))


(defun @pairlis (arg-names vals env)
  ;; pair up each arg-name with a value
  (cond

    ((not (null arg-names))
     (let ((first-name (@car arg-names))
	   (first-value (@car vals))
	   (rest-of-names (@cdr arg-names))
	   (rest-of-values (@cdr vals)))
       (let ((pairing (@cons first-name first-value)))
	 (@cons pairing (@pairlis rest-of-names rest-of-values)))))       

    (t env)))

(defun @assoc (name env)
  ;; get value of "name" in environment
  ;; undefined: if "name" is not in the environment
  (let ((first-pairing (@car env))
	(rest-of-pairings (@cdr env)))
    (let ((first-name (@car first-pairing)))
      (cond
	((= name first-name)
	 (let ((first-value (@car (@cdr first-pairing))))
	   first-value))
	(t (@assoc name rest-of-pairings))))))

(defun @evlis (expr-list env)
  ;; expr-list is a list of expressions which will form the args to a function
  ;; eval each arg, return a list of eval()ed args
  (cond
    ((null expr-list) @NIL)
    (t
     (let ((first-expr (@car expr-list))
	   (rest-of-exprs (@cdr expr-list)))
       (let ((first-value ((@eval first-expr env))))
	 (@cons first-value (@evlis rest-of-exprs env))))

(defun @evcon (list-to-be-interpreted-as-a-condition env)
  ;; interpret a list as a COND
  ;; each item in list-... is a pair
  ;;   1. guard
  ;;   2. expr to be eval()ed if guard is true
  ;; stop after first true guard
  ;; undefined: if no guards are true
  (let ((first-pair (@car list-to-be-interpreted-as-a-condition))
	(rest-of-pairs (@cdr list-to-be-interpreted-as-a-condition)))
    (let ((guard (@car first-pair)))
      (let ((guard-value (@eval guard)))
	(cond ((> guard-value 0)
	       (let ((expr (@car first-pair)))
		 (let ((expr-value (eval expr env)))
		   expr-value)))
	      (t (@evcon rest-of-pairs env)))))))


(defun main ()
  ;; (quote A)
  (let ((index-A (@putatom #\A @NIL)))
    (let ((list-to-be-interpreted (@cons kQuote index-A)))
      (let ((result (@eval list-to-be-interpreted)))
	(format *standard-output* "~a~%" result)))))
