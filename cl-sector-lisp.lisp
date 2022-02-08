;; memory is 256 cells, cell[0] is NIL, cell[<0]->List, cell[>0]->Atom
(defparameter *memory* nil)
(defconstant +min-address+ -127)
(defconstant +max-address+ +128)

(defun memsize ()
  (1+ (+ (abs +max-address+) (abs +min-address+))))

(defconstant +cell-size+ 2) ;; each cell is 2 units long (in Sector Lisp, unit=byte)

(defun @adjust-index (i) (+ 128 i))
(defun @unadjust-index (i) (- i 128))

(defun @fetch (ix)
  (aref *memory* ix))

(defun @get (index)
  (let ((ix (@adjust-index index)))
    (@fetch ix)))

(defun @put (index v)
  (let ((ix (@adjust-index index)))
    (setf (aref *memory* ix) v))
    index)

(defparameter *next-free-list-pointer* -1)
(defparameter *next-free-atom-pointer* 0)


(defconstant kNil 0)
(defconstant @NIL kNil)

(defun @putatombyte (v)
  (@put *next-free-atom-pointer* v)
  (incf *next-free-atom-pointer*))
  
(defun @putatomcell (vcar vcdr)
  (let ((index *next-free-atom-pointer*))
    (@putatombyte vcar)
    (@putatombyte vcdr)
    index))

(defun bumplist ()
  (decf *next-free-list-pointer*))

(defun @putlistcell (vcar vcdr)
    (@put *next-free-list-pointer* vcdr)
    (bumplist)
    (let ((index *next-free-list-pointer*))
      (@put *next-free-list-pointer* vcar)
      (bumplist)
      index))


(defun @putatom (chars)
  (if (null chars)
      0
    (let ((atom-index *next-free-atom-pointer*))
      (@putatombyte (car chars))
      (let ((cdr-address *next-free-atom-pointer*))
        (incf *next-free-atom-pointer*)
        (let ((vcdr (@putatom (cdr chars))))
          (@put cdr-address vcdr)
          atom-index)))))


;; in C, these would be top-level #defines (using human-calculated offsets)
;; in assembler, we could use assembler directives to calculate the offsets,
;; but, we want to call Lisp functions to calculate these indices
;;  (i.e. we want Lisp to be the assembler app AND the programming language,
;;   i.e. we could use Lisp to be all languages to all people (assembler and HLL))
;; we *could* do this with the appropriate application of eval-when, but
;;  I'm feeling lazy, so, instead I will make these constants into parameters
;; (N.B. defparameter causes these items to be "special"s - dynamically bound,
;;  so, if we cared about optimizing this code, we would cause these items to
;;  be compile-time constants, or we would bind them in a LET (static binding),
;;  but we'd have to make sure that all code referring to these constants would
;;  also be included in the LET (Lisp, in fact would help us do this, but, I'm
;;  more interested in writing code for Humans than for Lisp).

(defparameter kQuote -1)
(defparameter kCond  -1)
(defparameter kEq    -1)
(defparameter kCons  -1)
(defparameter kAtom  -1)
(defparameter kCar   -1)
(defparameter kCdr   -1)

(defun initialize-memory ()
  (setf *memory* (make-array (memsize) :initial-element @NIL))
  (setf *next-free-list-pointer* -1)
  (setf *next-free-atom-pointer* 0)
  (let ((i +min-address+))
    (loop 
     (when (< i +max-address+) (return)) ;; break from loop
     (@put *next-free-atom-pointer* 0)
     (@put *next-free-atom-pointer* 0)
     (incf i)))
  (assert ( = @NIL (@putatom '(#\N #\I #\L))))
  (setf kQuote (@putatom '(#\Q #\U #\O #\T #\E)))
  (setf kCond  (@putatom '(#\C #\O #\N #\D    )))
  (setf kEq    (@putatom '(#\E #\Q            )))
  (setf kCons  (@putatom '(#\C #\O #\N #\S    )))
  (setf kAtom  (@putatom '(#\A #\T #\O #\M    )))
  (setf kCar   (@putatom '(#\C #\A #\R        )))
  (setf kCdr   (@putatom '(#\C #\D #\R        ))))


  
;;;;;;;;;;;;; basic functions

(defun @cons (vcar vcdr)
  (@putlistcell vcar vcdr))

(defun @car (index)
  (@get index))

(defun @cdr (index)
  (@get (1+ index)))


(defun @eq (index-A index-B)
  (= index-A index-B))


;; evaluator
(defun @eval (e env)
  (let ((previous-SP *next-free-list-pointer*))
    (cond
      ((= e 0) 0)
      ((eq (@car e) kQuote) (@car (@cdr e)))
      ((eq (@cdr e) kCond) (@evcon (@cdr e) env))
      ((> e 0) (@assoc e env))
      (t (let ((v (@apply (@car e) (@evlis e env) env)))
	   (@gc previous-SP v))))))

(defun @apply (f args env)
  ;; apply function f to a *list* of values (args) in given environment
  (cond

    ((< f 0) 
     ;; we have ((... f ...) (... exprs ...))
     ;; f is a list with the shape (lambda (args ...) (body ...))
     ;; the car of the cdr is (args ...)
     ;; the car of the cddr is (body ...)
     ;; the actual exprs that are to be bound to the args is args
     (let ((arg-names (@car (@cdr f)))
           (body (@car (@cdr (@cdr f)))))
     (let ((new-env (@pairlis arg-names args env)))
       (@eval body new-env))))

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
	 (@cons pairing (@pairlis rest-of-names rest-of-values env)))))       

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
       (let ((first-value (@eval first-expr env)))
	 (@cons first-value (@evlis rest-of-exprs env)))))))

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
      (let ((guard-value (@eval guard env)))
	(cond ((> guard-value 0)
	       (let ((expr (@car first-pair)))
		 (let ((expr-value (@eval expr env)))
		   expr-value)))
	      (t (@evcon rest-of-pairs env)))))))


;;;; Garbage Collection
(defun @copy (index m offset)
  (if (< index m)
      (let ((car-copy (@copy (@car index) m offset))
            (cdr-copy (@copy (@cdr index) m offset)))
        (@cons car-copy cdr-copy))
    index))

(defun @gc (A index)
  (let ((B *next-free-list-pointer*))
    (let ((copied-cell-index (@copy index A (- A B))))
      (let ((C *next-free-list-pointer*)) ;; updated by above line
        (@move A B C)
        (setf *next-free-list-pointer* A)
        copied-cell-index))))

(defun @move (A B C)
  (loop
   (when (< C B) (return))
   (decf A)
   (decf B)
   (let ((B-car (@car B))
         (B-cdr (@cdr B)))
     (@put A B-car)
     (@put (1+ A) B-cdr))))

;;;;

(defun main ()
  (initialize-memory)
  ;; (quote A)
  (let ((index-A (@putatom '(#\A))))
    (let ((list-to-be-interpreted (@cons kQuote (@cons index-A @NIL))))
      (let ((result (@eval list-to-be-interpreted @NIL)))
	(format *standard-output* "~a~%" result)))))
