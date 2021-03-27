
(in-package :weir)


(defun -get-prop (wer key prop &key default)
  (declare #.*opt-settings* (weir wer) (symbol prop))
  "get first matching prop of key"
  (multiple-value-bind (alist exists) (gethash key (weir-props wer))
    (unless exists (return-from -get-prop
                                (values (if default default nil) nil)))
    (let ((res (assoc prop alist)))
      (unless res (return-from -get-prop (values (if default default nil) nil)))
      (values (cdr res) t))))

(defun -set-prop (wer key prop &optional (val t))
  (declare #.*opt-settings* (weir wer) (symbol prop))
  "set prop of key to val.  shadows previous entries of prop"
  (multiple-value-bind (_ exists) (gethash key (weir-props wer))
    (declare (ignore _))
    (if exists (setf (gethash key (weir-props wer))
                     (acons prop val (gethash key (weir-props wer))))
               (setf (gethash key (weir-props wer)) `((,prop . ,val)))))
    val)

(defun -clear-prop (wer key)
  (declare #.*opt-settings* (weir wer))
  (remhash key (weir-props wer)))


(defun set-edge-prop (wer e prop &optional (val t))
  (declare #.*opt-settings* (weir wer) (list e) (symbol prop))
  "set prop of edge e"
  (-set-prop wer (sort e #'<) prop val))

(defun lset-edge-prop (wer edges prop &optional (val t))
  (declare #.*opt-settings* (weir wer) (list edges) (symbol prop))
  "set prop of edges"
  (loop for e of-type list in edges
        do (set-edge-prop wer e prop val)))

(defun set-vert-prop (wer v prop &optional (val t))
  (declare #.*opt-settings* (weir wer) (fixnum v) (symbol prop))
  "set prop of vert v"
  (-set-prop wer v prop val))

(defun lset-vert-prop (wer verts prop &optional (val t))
  (declare #.*opt-settings* (weir wer) (list verts) (symbol prop))
  "set prop of verts"
  (loop for v of-type fixnum in (remove-duplicates (alexandria:flatten verts))
        do (set-vert-prop wer v prop val)))


(defun set-grp-prop (wer g prop &optional (val t))
  (declare #.*opt-settings* (weir wer) (symbol g) (symbol prop))
  "set prop of grp g"
  (-set-prop wer g prop val))


(defun get-edge-prop (wer e prop &key default)
  (declare #.*opt-settings* (weir wer) (list e) (symbol prop))
  "get prop ov edge e"
  (-get-prop wer (sort e #'<) prop :default default))

(defun get-vert-prop (wer v prop &key default)
  (declare #.*opt-settings* (weir wer) (fixnum v) (symbol prop))
  "get prop of vert v"
  (-get-prop wer v prop :default default))

(defun get-grp-prop (wer g prop &key default)
  (declare #.*opt-settings* (weir wer) (symbol g) (symbol prop))
  "get prop of grp g"
  (-get-prop wer g prop :default default))


(defsetf -get-prop -set-prop)
(defsetf get-edge-prop set-edge-prop)
(defsetf get-vert-prop set-vert-prop)
(defsetf get-grp-prop set-grp-prop)


(defun edge-has-prop (wer e prop &key val)
  (declare #.*opt-settings* (weir wer) (list e) (symbol prop))
  "t if edge e has prop (and val)"
  (multiple-value-bind (v exists) (get-edge-prop wer e prop)
    (if val (and exists (equal v val)) exists)))

(defun vert-has-prop (wer v prop &key val)
  (declare #.*opt-settings* (weir wer) (fixnum v) (symbol prop))
  "t if vert v has prop (and val)"
  (multiple-value-bind (v exists) (get-vert-prop wer v prop)
    (if val (and exists (equal v val)) exists)))


;TODO: reverse index for faster lookup?
(defun edges-with-prop (wer prop &key val g &aux (res (list)))
  (declare #.*opt-settings* (weir wer) (symbol prop) (list res))
  "find edges with prop (and val)"
  (labels ((accept (e) (get-edge-prop wer e prop))
           (acceptval (e) (let ((pv (get-edge-prop wer e prop)))
                            (and pv (equal pv val)))))
    (let ((do-test (if val #'acceptval #'accept)))
      (with-grp (wer g* g)
        (graph:with-graph-edges ((grp-grph g*) e)
          (when (funcall do-test e) (push e res)))))
    res))


(defun verts-with-prop (wer prop &key val &aux (res (list)))
  (declare #.*opt-settings* (weir wer) (symbol prop) (list res))
  "find verts with prop (and val)"
  (labels ((accept (v) (get-vert-prop wer v prop))
           (acceptval (v) (let ((pv (get-vert-prop wer v prop)))
                            (and pv (equal pv val)))))
    (let ((do-test (if val #'acceptval #'accept)))
      (loop for v from 0 below (weir-num-verts wer)
            do (when (funcall do-test v) (push v res))))
    res))


(defun edge-prop-nxt-vert (wer v prop &key val (except -1) g)
  (declare #.*opt-settings* (weir wer) (fixnum v except) (symbol prop))
  "
  get first (encountered) incident vert w from v, with prop (and val).
  ignores w when w == except.
  returns nil if there is no incident vert.
  "
  (loop for w in (get-incident-verts wer v :g g)
        if (not (= except w))
        do (when (edge-has-prop wer (sort (list v w) #'<) prop :val val)
                 (return-from edge-prop-nxt-vert w)))
  nil)


(defun all-grps->main! (wer &key g)
  (itr-grps (wer g*)
    (itr-edges (wer e :g g*)
      (set-edge-prop wer (ladd-edge! wer e :g g) g*))))


(defun show-props (wer)
  (loop for key being the hash-keys of (weir-props wer) using (hash-value v)
        do (format t "~%~%------~%")
           (print (list :key key))
           (print v)))

