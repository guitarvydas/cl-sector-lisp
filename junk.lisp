(defun junk (lis dontcare)
  ;;(lambda (x)
  (cond 
    ((null lis) nil)
    (t (cons (car lis) (junk (cdr lis) dontcare)))))

(junk '(a b) '(a b))

1: (lambda (lis=(a b) x=(a b))
  (cons 'a (junk lis=(b) dontcare)))

2: (lambda (lis=(b) x=(a b))
     (cons 'b (junk lis=nil) dontcare))
