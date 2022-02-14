(defun lookup-atom (str memory)
  (let (atom-memory
	buffer
	connections
	(answer nil)
	found
        (parts nil)
        conclude)
    (let ((conclude-predicate (lambda () conclude)))
      (flet ((not-concluded () (setf conclude nil))
             (concluded () (setf conclude t)))
        (let (
              (self-handler (lambda (message) 
                              (cond ((string= "init" (message-tag message))
                                     (send '(:self "init") t message parts))
                                    ((string= "answer" (message-tag message))
                                     (setf answer (message-data message)))
                                    ((string= "found" (message-tag message))
                                     (setf found (message-data message))
                                     (concluded))
                                    (t (assert nil))
                                    )))
              (match-top-level (lambda (message)
                                 (cond ((string= "init" (message-tag message))
                                        (send '("match top level" "start name match") t message parts))
                                       ((string= "advance" (message-tag message))
                                        (if (?eof atom-memory)
                                            (send '("match top level" "EOF") t message parts)
                                          (progn
                                            (@advance-to-next-atom atom-memory)
                                            (send '("match top level" "start name match") t message parts))))
                                       (t (assert nil))
                                       )))
              (name-match (lambda (message) 
                            (cond ((string= "go" (message-tag message))
                                   (if (?match-string atom-memory buffer)
                                       (send '("name match" "ok") t message parts)
				     (send '("name match" "mismatch") t message parts)))
                                  (t (assert nil))
                                  )))
              (unsuccessful (lambda (message)
                              (cond ((string= "conclude" (message-tag message))
                                     (send '(:self "found") nil message parts))
                                    (t (assert nil))
                                    )))

              (successful (lambda (message)
                            (cond ((string= "conclude" (message-tag message))
                                   (send '(:self "answer") (current-atom-index atom-memory) message parts)
                                   (send '(:self "found") t message parts))
                                  (t (assert nil))
                                  )))
              )
			  

          (setf parts (list 
                       (list :self self-handler nil nil)
                       (list "match top level" match-top-level nil nil)
                       (list "name-match" name-match nil nil)
                       (list "unsuccessful" unsuccessful nil nil)
                       (list "successful" successful nil nil)
                       ))

          (setf connections '(
                              ((:self "init") (("match top level" "init")))
                              (("match top level" "EOF") (("unsuccessful" "conclude")))
                              (("match top level" "start name match") (("name match" "go")))
                              (("name match" "mismatch") (("match top level" "advance")))
                              (("name match" "ok") (("successful" "conclude")))
                              (("unsuccessful" "found") ((:self "found")))
                              (("unsuccessful" "answer") ((:self "answer")))
                              (("successful" "found") ((:self "found")))
                              (("successful" "answer") ((:self "answer")))
                              ))


          ;; main body
          (not-concluded)
          (setf buffer str)
          (set atom-memory memory)
          (send '(:self "init") t nil parts)
          (route-messages connections parts parts)
          (dispatch parts connections conclude-predicate)
          (values found answer))))))