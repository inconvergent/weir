
(in-package #:weir-utils)

(defvar *opt-settings* '(optimize (safety 1) (speed 3) (debug 2) (space 2)))


(declaim (type double-float PI* PII PI5))

; changed based on comment in this PR:
; https://github.com/inconvergent/weir/pull/2
(defconstant PI* #.(coerce pi 'double-float))
(defconstant PII #.(coerce (* pi 2d0) 'double-float))
(defconstant PI5 #.(coerce (* pi 0.5d0) 'double-float))

(defun v? (&optional silent)
  (if silent (slot-value (asdf:find-system 'weir) 'asdf:version)
             (format t "~%weir version: ~a~%"
               (slot-value (asdf:find-system 'weir) 'asdf:version))))


(defun f? (f)
  (declare (symbol f))
  (format t "~%---~%")
  (describe f)
  (format t "~%---~%") )

(defun i? (f) (inspect f))


;http://cl-cookbook.sourceforge.net/os.html
(defun cmd-args ()
  (or #+SBCL sb-ext:*posix-argv*
      #+LISPWORKS system:*line-arguments-list*
      #+CMU extensions:*command-line-words*
      nil))


;https://www.rosettacode.org/wiki/Program_termination#Common_Lisp
(defun terminate (status)
  (format t "~%terminated with status: ~a~%" status)
  #+sbcl (sb-ext:quit :unix-status status)
  #+ccl (ccl:quit status)
  #+clisp (ext:quit status)
  #+cmu (unix:unix-exit status)
  #+abcl (ext:quit:status status)
  #+allegro (excl:exit status :quiet t)
  #+gcl (common-lisp-user::bye status)
  #+ecl (ext:quit status))


;https://github.com/inconvergent/weir/pull/1/commits/4a1df51914800c78cb34e8194222185ebde12388
(defmacro define-struct-load-form (struct-name)
  "Allow the structure named STRUCT-NAME to be dumped to FASL files."
  `(defmethod make-load-form ((obj ,struct-name) &optional env)
     (make-load-form-saving-slots obj :environment env)))


;from on lisp by pg
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))


;from on lisp by pg
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))


;from on lisp by pg
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


;from on lisp by pg
(defmacro mac (expr) `(pprint (macroexpand-1 ',expr)))


;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
(defun psymb (package &rest args)
  (values (intern (apply #'mkstr args) package)))


;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(psymb (symbol-package name) name f) ,gs)))
                     fields)
         ,@body))))


(defun append-postfix (fn postfix)
  (declare (string fn postfix))
  (concatenate 'string fn postfix))


(defun append-number (fn i)
  (declare (string fn) (fixnum i))
  (format nil "~a-~8,'0d" fn i))


(defun ensure-filename (fn &optional (postfix "") (silent nil))
  (let ((fn* (append-postfix (if fn fn "tmp") postfix)))
    (declare (string fn*))
    (format (not silent) "~%file: ~a~%~%" fn*)
    fn*))


(defun print-every (i &optional (n 1))
  (declare (fixnum i n))
  (when (= 0 (mod i n)) (format t "~%itt: ~a~%" i)))


(defun string-list-concat (l)
  (declare (list l))
  (format nil "~{~a~}" l))


(defun numshow (a &key (ten 6) (prec 6))
  (declare (double-float a))
  (let ((show (format nil "~~,~af" prec)))
    (if (< 1d-6 (the double-float (abs a)) (expt 10 ten))
        (format nil show a)
        (format nil "~,1e" a))))


(abbrev vextend vector-push-extend)

(defun lvextend (xx arr)
  (declare (sequence xx) (vector arr))
  (typecase xx (cons (loop for x in xx do (vextend x arr)))
               (t (loop for x across xx do (vextend x arr)))))


(declaim (inline vector-last))
(defun vector-last (a)
  (declare (vector a))
  (aref a (1- (the fixnum (length a)))))


(declaim (inline vector-first))
(defun vector-first (a)
  (declare (vector a))
  (aref a 0))


(defun make-adjustable-vector (&key init (type t) (size 128))
  (let ((res (if init (make-array (length init) :fill-pointer 0
                                                :initial-contents init
                                                :element-type type
                                                :adjustable t)
                      (make-array size :fill-pointer 0
                                       :element-type type
                                       :adjustable t))))
    (when init (lvextend init res))
    res))


(declaim (inline to-vector))
(defun to-vector (init &key (type t))
  (declare (list init))
  (make-array (length init) :initial-contents init
                            :adjustable nil
                            :element-type type))


(declaim (inline ensure-vector))
(defun ensure-vector (o &key (type t))
  (declare (sequence o))
  (typecase o (cons (to-vector o :type type))
              (t o)))


(defun to-adjustable-vector (init &key (type t))
  (declare (sequence init))
  (make-array (length init) :fill-pointer t
                            :initial-contents init
                            :element-type type
                            :adjustable t))


(declaim (inline to-list))
(defun to-list (a)
  (declare (sequence a))
  (coerce a 'list))

(defun undup (e) (delete-duplicates (alexandria:flatten e)))

(defun internal-path-string (path)
  (declare (string path))
  (namestring (asdf:system-relative-pathname :weir path)))

