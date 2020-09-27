
(in-package :weir)


(defun get-alteration-result-list (wer &key (all t))
  (declare (weir wer) (boolean all))
  (loop for k being the hash-keys of (weir-alt-res wer)
          using (hash-value v)
        if (or all (and (not all) v))
        collect (list k v)))

(defun get-alteration-result-map (wer)
  (declare (weir wer))
  (weir-alt-res wer))


(declaim (inline -if-all-resolved))
(defun -if-all-resolved (alt-res ref)
  (declare #.*opt-settings* (hash-table alt-res) (list ref))
  (loop for k of-type keyword in ref
        do (multiple-value-bind (res exists) (gethash k alt-res)
             (declare (boolean exists))
             (when (not exists) (return-from -if-all-resolved :wait))
             (when (not res) (return-from -if-all-resolved :bail))))
  :ok)


; TODO: this makes a brute force attempt to gradually resolve all futures. it
; is probably possible to actually build the graph, and execute everything in
; the right order?
(declaim (inline -resolve-all))
(defun -resolve-all (fxs wer)
  (declare #.*opt-settings* (list fxs) (weir wer))
  (labels ((-is-resolved (fx)
            (declare (function fx))
            (multiple-value-bind (is-resolved _) (funcall fx wer)
              (declare (ignore _) (boolean is-resolved))
              is-resolved)))
    (loop while fxs do (setf fxs (remove-if #'-is-resolved fxs)))))


; macro helper
(defun _do-transform-to-future (root accfx alt-res
                                &aux (expr (first root)) (root* (cdr root)))
  (declare (symbol accfx alt-res) (list root* expr))
  ;(when (not root*) (error "argument to accfx can not be nil"))
  (when (not (< -1 (length root*) 3))
        (error "incorrect arguments to accumulator, got: ~a" root*))
  (multiple-value-bind (name ref)
    (loop with ref = nil
          with name = nil
          for v in root*
          do (if (keywordp v) (setf name v))
             (if (consp v) (setf ref v))
          finally (return (values name ref)))
    `(,accfx (weir::future ,alt-res ,expr ,name ,ref))))

; macro helper
(defun _transform-body (root accfx alt-res)
  (declare (symbol accfx alt-res))
  (cond ((atom root) root)
        ((listp root)
           (if (equal (first root) accfx)
               ; transform body
               (_do-transform-to-future (cdr root) accfx alt-res)
               ; else recurse
               (cons (_transform-body (car root) accfx alt-res)
                     (_transform-body (cdr root) accfx alt-res))))
        (t (error "incorrect syntax in weir:with"))))

; macro helper
(defun _subst-all (expr ref d)
  (loop for r in ref do (setf expr (subst `(gethash ,r ,d) r expr)))
  expr)

; macro helper
(defun _call-fx (alt-res wname name expr)
  (if name `(setf (gethash ,name ,alt-res) (funcall ,expr ,wname))
           `(funcall (the function ,expr) ,wname)))

; macro helper
(defun _set-bail (alt-res name)
  (if name `(setf (gethash ,name ,alt-res) nil) nil))

; macro helper
(defun _any-futures (root ref)
  (labels ((_match-some-refs () (some (lambda (r) (equal r root)) ref)))
    (cond ((atom root) (_match-some-refs))
          ((consp root) (or (_any-futures (car root) ref)
                            (_any-futures (cdr root) ref)))
          (t (error "unexpected error in _any-futures, got ~a" root)))))

; macro helper
(defun _find-non-atom-args (expr ref &aux (gs->arg (list)))
  (labels ((-e-or-gensym (e)
            (if (and (not (atom e))
                     (not (_any-futures e ref)))
                (let ((gs (gensym "non-atom-arg")))
                  (push (list gs e) gs->arg)
                  gs)
                e)))
    (values (loop for e in expr collect (-e-or-gensym e))
            gs->arg)))

; macro helper
(defun _let-over-lambda (gs->arg l)
  (declare (list gs->arg l))
  (if (> (length gs->arg) 0) `(let ,(reverse gs->arg) ,l)
                             l))

(defmacro future (alt-res expr name ref)
  (declare (symbol alt-res) (list expr))

  (when (not (every (lambda (r) (keywordp r)) ref))
        (error "refs must be (keywords) or nil, got ~a" ref))
  (when (and name (not (keywordp name)))
        (error "name must be keyword or nil, got: ~a" name))

  (alexandria:with-gensyms (wname)
    (multiple-value-bind (expr-atom gs->arg) (_find-non-atom-args expr ref)
      (if ref ; expr depends on future result
          (_let-over-lambda gs->arg
            `(lambda (,wname)
               (case (-if-all-resolved ,alt-res ',ref)
                     (:ok (values t ,(_call-fx alt-res wname name
                                       (_subst-all expr-atom ref alt-res))))
                     (:bail (values t ,(_set-bail alt-res name)))
                     (:wait (values nil nil)))))
          ; expr has no future dependencies
          (_let-over-lambda gs->arg
            `(lambda (,wname)
               (values t ,(_call-fx alt-res wname name expr-atom))))))))

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

  it is always posslbe to both reference future results, and assign a name to
  the current the alteration (result). that is, the second and third arguments
  to (% ...) are both optional.

  results will be available after call to (with:weir ...), see
  (get-alteration-result-list) or (get-alteration-result-map).
  note that using the same name for multiple alterations might result in
  unexpected behaviour.
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


