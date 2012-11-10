(defun test ()
  (with-open-file (out "~/code/lisp/practical/data/test.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create :element-type '(unsigned-byte 8))
    (write-byte 32 out)
    (write-byte 22 out)
    (write-byte 90 out)
    (write-byte 12 out)))

(defun test2 ()
  (with-open-file (in "~/code/lisp/practical/data/test.txt" :direction :input :if-does-not-exist nil :element-type '(unsigned-byte 8))
    (if in
	(print (read-byte in)))))

(defun read-u2 (in)
  (let ((u 0))
    (setf (ldb (byte 8 8) u) (read-byte in))
    (setf (ldb (byte 8 0) u) (read-byte in))))

(defun write-u2 (out v)
  (write-byte (ldb (byte 8 8) v) out)
  (write-byte (ldb (byte 8 0) v) out))
  
(defmacro mac (macro)
  `(pprint (macroexpand ',macro)))
(defmacro mac1 (macro)
  `(pprint (macroexpand-1 ',macro)))
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(mapcar #'(lambda (name) `(,name (gensym))) names)
     ,@body))
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))
(defconstant +null+ (code-char 0))
(defun as-keyword (sym) (intern (string sym) :keyword))
(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun mklist (x) (if (listp x) x (list x)))
(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))
(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))


;;(defclass iso-8859-1-string () ())
(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))
(defmethod read-value ((type (eql 'iso-8859-1-string)) stream &key length)
  (progn
    (format t "read-value iso-8859-1-string:~A~%" length)))

(defmethod read-value ((type (eql 'u1)) stream &key)
  (print "read-value u1"))
(defmethod read-value ((type (eql 'id3-tag-size)) stream &key)
  (print "read-value id3-tag-size"))
(defmethod read-value ((type (eql 'id3-frames)) stream &key tag-size)
  (progn
    (format t "read-value id3-frames:~A~%" tag-size)))
(defgeneric write-value (type stream value &key)
  (:documentation "Write a value of the given type to the stream"))
(defun defun-write-value (type &rest args)
  `(defmethod write-value ((type (eql ',type)) stream value ,@args)
     (format t "write-value ~A as ~A~%" value ',type)))
(defmacro defun-write-value-multi (&rest args)
  `(progn
     ,@(mapcar #'(lambda (uarg) (if (listp uarg)
				    (funcall #'defun-write-value (first uarg) (second uarg))
				    (funcall #'defun-write-value uarg)))
					 args)))
(defun-write-value-multi (u1 &key) (id3-tag-size &key) (id3-frames &key tag-size))

(defmacro define-binary-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
	 ,(mapcar #'slot->defclass-slot slots))

       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
	 (let ((,objectvar (make-instance ',name)))
	   (with-slots ,(mapcar #'first slots) ,objectvar
	     ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
	   ,objectvar)))))

(define-binary-class id3-tag 
  ((identifier (iso-8859-1-string :length 3))
   (major-version u1)
   (revision u1)
   (flags u1)
   (size id3-tag-size)
   (frames (id3-frames :tag-size size))))

(with-open-file (in "~/code/lisp/practical/data/test.txt" :direction :input :if-does-not-exist nil
		    :element-type '(unsigned-byte 8))
  (if in
      (read-value 'id3-tag in)))
