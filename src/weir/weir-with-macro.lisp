
(in-package :weir)

(defvar *errormsg* "~%
------------------------------------------------------------------------------
  alteration error on:~%~%~a~%~%message:~%~%~a~%~%
------------------------------------------------------------------------------~%")

(defvar *with-errormsg* "~%
------------------------------------------------------------------------------
  weir:with ~a error, message:~%~%~a~%~%
------------------------------------------------------------------------------~%")


(defun get-alteration-result-list (wer &key (all t))
  (declare #.*opt-settings* (weir wer) (boolean all))
  "returns alist with tuples of alteration name and corresponding result."
  (loop for k being the hash-keys of (weir-alt-res wer)
          using (hash-value v)
        if (or all (and (not all) v))
        collect `(,k . ,v)))

(defun get-alteration-result-map (wer)
  (declare #.*opt-settings* (weir wer))
  "returns hash-table with results of all alterations by :name."
  (weir-alt-res wer))


(declaim (inline -if-all-resolved))
(defun -if-all-resolved (alt-res ref)
  (declare #.*opt-settings* (hash-table alt-res) (list ref))
  "check if all references of an alteration have been resolved."
  (loop for k of-type keyword in ref
        do (multiple-value-bind (res exists) (gethash k alt-res)
             (declare (boolean exists))
             (unless exists (return-from -if-all-resolved :wait))
             (unless res (return-from -if-all-resolved :bail))))
  :ok)


; TODO: this makes a brute force attempt to gradually resolve all futures. it
; is probably possible to actually build the graph, and execute everything in
; the right order?
(declaim (inline -resolve-all))
(defun -resolve-all (fxs wer)
  (declare #.*opt-settings* (list fxs) (weir wer))
  "attempt to resolve all alterations until all have resolved, or bailed
   (given up because a referenced alteration has failed)."
  (labels ((-is-resolved (fx)
            (declare (function fx))
            (multiple-value-bind (is-resolved _)
              (handler-case (funcall fx wer)
                 (error (err) (format t *errormsg* fx err)
                              (weir-utils::terminate 666)))
              (declare (ignore _) (boolean is-resolved))
              is-resolved)))
    (loop while fxs do (setf fxs (remove-if #'-is-resolved fxs)))))

; TODO: this is rather complicated and messy. revisit this to see if it can be
; improved
(defun _args->gensyms (root ref)
  "
  find forms in expr that can be shadowed, and replace them with (gensym).
  these forms will be wrapped around expr/root in a closure.

  if expr/root is a lambda, the same process will be applied to the body of the
  lambda.

  any function call that contains a ref will be left as is.

  any symbol that is a ref or an argument to the lambda will be left as is.
  "
  (let ((gslst (list)))
    (labels
      ((-gs-with-name (r)
         "make gensym from r"
         (gensym (if (atom r) (concatenate 'string (string r) ":")
                              "non-atom:")))
       (-replace-with-gs (r)
         "replace symbol with gensym"
         (let ((g (-gs-with-name r)))
           (push (list g r) gslst)
           g))
       (-match-refp (r ref*)
         "t if r matches any symbol in ref"
         (some (lambda (x) (equal x r)) ref*))
       (-any-futures (root* ref*)
         "t if root contains any refs"
         (cond ((atom root*) (-match-refp root* ref*))
               ((consp root*) (or (-any-futures (car root*) ref*)
                                  (-any-futures (cdr root*) ref*)))))
       (-setfp (r)
         "t if r is a (setf ...)"
         (and (listp r) (equal (car r) 'setf)))
       (-lambdap (r)
         "t if r is a (lambda () ...)"
         (and (listp r) (equal (car r) 'lambda)))
       (-sharp-quotep (x)
         "t if r is a #'symbol"
         (and (consp x) (equal 'function (first x)) (= 2 (length x))))
       (-do-walk-fx-test (r ref*)
         "check if first in r is a (fx ...)"
         (cond ((and (consp r) (not (-any-futures r ref*))) (-replace-with-gs r))
               ((consp r) (-do-walk r ref* :fx t))
               (t (-do-walk r ref* :fx nil))))
       (-args-or-rest (expr*)
         "append rest to lambda arg if empty"
         (if (not (second expr*)) `(lambda (&rest ,(gensym "rest-"))
                                     ,(third expr*))
                                  expr*))
       (-do-walk (r ref* &key fx)
         (cond ((not r) nil)
               ((or (numberp r) (-sharp-quotep r) (keywordp r)) r)
               ((-lambdap r) (append (subseq r 0 2)
                                     (-do-walk (cddr r) (append ref* (second r)))))
               ((-setfp r) (append (subseq r 0 2)
                                   (-do-walk (cddr r) ref*)))
               ((atom r) (if (not (-any-futures r ref*)) (-replace-with-gs r) r))
               (fx (append (cons (first r) (list (-do-walk-fx-test (second r) ref*)))
                           (-do-walk (cddr r) ref*)))
               ((consp r) (cons (-do-walk-fx-test (car r) ref*)
                                (-do-walk (cdr r) ref*))))))
      (values (if (-lambdap root)
                  (-args-or-rest (-do-walk root ref))
                  (cons (first root) (-do-walk (cdr root) ref)))
              gslst))))


(defun -valid-ref (v) (or (keywordp v) (symbolp v)))

(defmacro future (alt-res expr name ref)
  (declare (symbol alt-res) (list expr))
  "convert to future if alteration has references to other alterations in the
   same block."

  (unless (or (not ref) (listp ref))
          (error "ref must be a list or nil, got ~a" ref))
  (unless (every #'-valid-ref ref)
          (error "all refs must be variable/keyword/symbol or nil, got ~a" ref))
  (when (and name (not (-valid-ref name)))
        (error "res must be variable/keyword/symbol or nil, got: ~a" name))

  (labels
    ((-call-fx (wname expr*)
       "ensure result is set if alteration is named."
       (if name `(setf (gethash ,name ,alt-res) (funcall ,expr* ,wname))
         `(funcall (the function ,expr*) ,wname)))
     (-set-bail ()
       "ensure result is set to nil if named alteration bails"
       (if name `(setf (gethash ,name ,alt-res) nil) nil))
     (-subst-all-refs (expr* ref* d)
       "replace all references to future alteration results with
        (gethash :ref alt-res)"
       (loop for r in ref* do (setf expr* (subst `(gethash ,r ,d) r expr*)))
       expr*)
     (-let-over-lambda (gs->arg lambda*)
       (declare (list gs->arg lambda*))
       "wrap function in closure to ensure expressions are evaluated before the
        alteration is collected."
       (if (> (length gs->arg) 0) `(let ,(reverse gs->arg) ,lambda*)
                                   lambda*)))

    (alexandria:with-gensyms (wname)
      (multiple-value-bind (expr-gs gs->arg) (_args->gensyms expr ref)
        (if ref ; expr has futures
            (-let-over-lambda gs->arg
              `(lambda (,wname)
                 (case (-if-all-resolved ,alt-res (list ,@ref))
                       (:ok (values t ,(-call-fx wname
                                         (-subst-all-refs expr-gs ref alt-res))))
                       (:bail (values t ,(-set-bail)))
                       (:wait (values nil nil)))))
            ; expr has no futures
            (-let-over-lambda gs->arg
              `(lambda (,wname)
                 (values t ,(-call-fx wname expr-gs)))))))))

(defmacro with ((wer accfx &key db) &body body)
  (declare (symbol wer accfx) (boolean db))
  "creates a context for manipulating weir via alterations (see README)"
  (labels
    ((-get-name-ref (root)
       "extract name (res) and refs (arg) of an alteration."
        (values (getf root :res)
                (getf root :arg)))

     (-do-transform-to-future (root alt-res &aux (expr (first root))
                                                 (root* (cdr root)))
       (declare (symbol alt-res) (list root* expr))
       "wrap alteration in closure, so it can be used as a future."
       (unless (<= 0 (length root*) 5)
               (error "incorrect alteration, got: ~a" root*))
       (multiple-value-bind (name ref) (-get-name-ref root*)
         (let ((f `(future ,alt-res ,expr ,name ,ref)))
           (when db (format t "~%~%------------------~%~%alt: ~a~%--->~%" root)
                    (pprint (macroexpand-1 `(future ,alt-res ,expr ,name ,ref)))
                    (format t "~%" root))
           `(,accfx ,f))))

     (-transform-body (root alt-res)
       (declare (symbol alt-res))
       "find all alterations to be collected, and transform them to futures."
       (cond ((atom root) root)
             ((listp root)
                (if (equal (first root) accfx)
                    ; transform body
                    (-do-transform-to-future (cdr root) alt-res)
                    ; else recurse
                    (cons (-transform-body (car root) alt-res)
                          (-transform-body (cdr root) alt-res)))))))

    (alexandria:with-gensyms (wname x alts alt-res clear-alt-res with-res)
      (let ((new-body (handler-case (-transform-body body alt-res)
                                    (error (ename) (format t *with-errormsg*
                                                           "transform" ename)))))
        `(let* ((,wname ,wer)
                (,alts (list))
                (,alt-res (weir-alt-res ,wname)))
           (declare (weir ,wer) (hash-table ,alt-res) (list ,alts))

         (incf (weir-wc ,wname))

         (labels ((,accfx (,x) (typecase ,x (function (push ,x ,alts))))
                  (,clear-alt-res () (loop for ,x being the hash-keys of ,alt-res
                                           do (remhash ,x ,alt-res))))
           (,clear-alt-res)

           (handler-case
             ; ensure that body returns the final form, unless error
             (let ((,with-res (progn ,@new-body)))
               (-resolve-all ,alts ,wname)
               ,with-res)
             (error (ename) (format t *with-errormsg* "macro" ename)
                            (weir-utils::terminate 667)))))))))

