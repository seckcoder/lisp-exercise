(setf v 10)
(let ((v 4))
  (print (symbol-value 'v)))   ; lexical variabls are different, symbol is just a placeholder

;; Symbols are big
(setf sym 'abc)
(setf abc 5)
(print (eval sym))
(setf abc #'(lambda (x) (print x)))
(funcall (eval sym) 13)
