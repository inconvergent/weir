
(in-package :weir)

(defvar *errormsg* "

--------------------------------------------------------------------------------
  alteration error on:

  ~a

  message:

  ~a
--------------------------------------------------------------------------------

")


(defun get-alteration-result-list (wer &key (all t))
  (declare #.*opt-settings* (weir wer) (boolean all))
  "returns list with tuples of alteration name and corresponding result."
  (loop for k being the hash-keys of (weir-alt-res wer)
          using (hash-value v)
        if (or all (and (not all) v))
        collect (list k v)))

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
                 (error (ename) (format t *errormsg* fx ename)
                                (weir-utils::terminate 666)))
              (declare (ignore _) (boolean is-resolved))
              is-resolved)))
    (loop while fxs do (setf fxs (remove-if #'-is-resolved fxs)))))

(defun _get-name-ref (root)
  "extract name, refs and :loop of an alteration as it is collected."
  (loop with ref = nil
        with name = nil
        with lp = nil
        for v in root
        do (when (equal v :loop) (setf lp t))
           (when (keywordp v) (setf name v))
           (when (consp v) (setf ref v))
        finally (return (values name ref lp))))

(defun _do-transform-to-future (root accfx alt-res &aux (expr (first root))
                                                        (root* (cdr root)))
  (declare (symbol accfx alt-res) (list root* expr))
  "wrap alteration in closure, so it can be used as a future."
  (unless (< -1 (length root*) 4)
          (error "incorrect alteration, got: ~a" root*))
  (multiple-value-bind (name ref lp) (_get-name-ref root*)
    `(,accfx (weir::future ,alt-res ,expr ,name ,ref ,lp))))

; macro helper
(defun _transform-body (root accfx alt-res)
  (declare (symbol accfx alt-res))
  "find all alterations to be collected, and transform them to futures."
  (cond ((atom root) root)
        ((listp root)
           (if (equal (first root) accfx)
               ; transform body
               (_do-transform-to-future (cdr root) accfx alt-res)
               ; else recurse
               (cons (_transform-body (car root) accfx alt-res)
                     (_transform-body (cdr root) accfx alt-res))))))

(defun _find-non-atom-args (expr ref lp &aux (gs->arg (list)))
  "
  find all non-atom forms in expr, and replace them with (gensym). these forms
  will be wrapped around expr in a closure.

  if expr is a lambda, the same process will be applied to the body of the
  lambda.

  any form that contains a reference (or an argument to the lambda) will be
  left as is.
  "
  (labels ((-is-lambda () (equal (first expr) 'lambda))
           (-replace-with-gensym (e)
             (let ((gs (gensym "non-atom-arg")))
               (push (list gs e) gs->arg)
               gs))
           (-match-some-refs (e ref*)
             "t if e matches any ref."
             (some (lambda (r) (equal r e)) ref*))
           (-any-futures (root* ref*)
             "t if root contains any refs"
             (cond ((atom root*) (-match-some-refs root* ref*))
                   ((consp root*) (or (-any-futures (car root*) ref*)
                                      (-any-futures (cdr root*) ref*)))))
           (-e-or-gensym (e ref*)
             (if (and (or (not (atom e)) (and (atom e) lp))
                      (not (-any-futures e ref*)))
                 (-replace-with-gensym e)
                 e))
           (-args-or-_ (a)
             "replace empty lambda args with ((gensym _))"
             (if (not (second a)) `(lambda (&rest ,(gensym "rest"))) a))
           (-process (expr* ref*)
             (loop for e in expr* collect (-e-or-gensym e ref*))))

    (values (if (-is-lambda)
                (append (-args-or-_ (subseq expr 0 2))
                        (-process (cddr expr) (append ref (second expr))))
                (cons (first expr) (-process (cdr expr) ref)))
            gs->arg)))


(defmacro future (alt-res expr name ref lp)
  (declare (symbol alt-res) (list expr) (boolean lp))
  "convert to future if alteration has references to other alterations in the
   same block."

  (unless (every (lambda (r) (keywordp r)) ref)
          (error "refs must be (keywords) or nil, got ~a" ref))
  (when (and name (not (keywordp name)))
        (error "name must be keyword or nil, got: ~a" name))

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
       "wrap lambda in let closure to ensure non-atom expressions are evaluated
        before the alteration is collected."
       (if (> (length gs->arg) 0) `(let ,(reverse gs->arg) ,lambda*)
                                   lambda*)))

    (alexandria:with-gensyms (wname)
      (multiple-value-bind (expr-atom gs->arg) (_find-non-atom-args expr ref lp)
        (if ref ; expr depends on future result
            (-let-over-lambda gs->arg
              `(lambda (,wname)
                 (case (-if-all-resolved ,alt-res ',ref)
                       (:ok (values t ,(-call-fx wname
                                         (-subst-all-refs expr-atom ref alt-res))))
                       (:bail (values t ,(-set-bail)))
                       (:wait (values nil nil)))))
            ; expr has no future dependencies
            (-let-over-lambda gs->arg
              `(lambda (,wname)
                 (values t ,(-call-fx wname expr-atom)))))))))

(defmacro with ((wer accfx) &body body)
  (declare (symbol wer accfx))
  "
  creates a context for manipulating weir via alterations.
  example:

    (weir:with (wer %)
      (% (weir:add-edge? ...)))

  all (% ...) forms inside the weir context will cause the alteration inside to
  be created, collected and executed. if it is nil, nothing happens.

  you can assign a name to an alteration result, and reference future
  alteration results.

  names must be keywords: :a
  references must be a list of keywords: (:a :b)

  example:

    (weir:with (wer %)
      (% (weir:add-vert? pt) :a)
      (% (weir:add-vert? pt) :b)
      (% (weir:add-edge? :a :b) (:a :b)))

  it is always possible to both reference future results, and assign a name to
  the current the alteration (result). that is, the second and third arguments
  to (% ...) are both optional.

  results will be available after call to (with:weir ...), see
  (get-alteration-result-list) or (get-alteration-result-map).
  note that using the same name for multiple alterations might result in
  unexpected behaviour.

  Note that all alterations must follow a specific format. That is, there can
  be at most three forms inside `(% ...). The first must be a form that
  evaluates to a lambda. The third and second are both optional, and the order
  does not matter, however, they must be a :keyword if you want to give the
  alteration result a name, and a list of keywords to indicate that you are
  referencing alteration results inside the first form.
  "
  (alexandria:with-gensyms (wname x alts alt-res clear-alt-res)
    (let ((new-body (_transform-body body accfx alt-res)))
     `(let* ((,wname ,wer)
             (,alts (list))
             (,alt-res (weir-alt-res ,wname)))
        (declare (weir ,wer) (hash-table ,alt-res) (list ,alts))

      (incf (weir-wc ,wname))

      (labels ((,accfx (,x) (typecase ,x (function (push ,x ,alts))))
               (,clear-alt-res () (loop for ,x being the hash-keys of ,alt-res
                                        do (remhash ,x ,alt-res))))
        (,clear-alt-res)
        (progn ,@new-body)
        (-resolve-all ,alts ,wname))))))

