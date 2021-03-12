
(in-package :rnd)


(declaim (inline -swap))
(defun -swap (a i j)
  (declare #.*opt-settings* (vector a) (fixnum i j))
  (let ((tmp (aref a i)))
    (setf (aref a i) (aref a j)
          (aref a j) tmp)))

; https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
(defun shuffle (a &aux (a* (ensure-vector a)) (n (length a*)))
  (declare #.*opt-settings* (sequence a) (vector a*))
  "shuffle a"
  (loop for i of-type fixnum from 0 to (- n 2)
        do (-swap a* i (rndrngi i n)))
  a*)


(defun nrnd-u-from (n a)
  (declare #.*opt-settings* (fixnum n) (sequence a))
  "n random distinct elements from a"
  (let* ((a* (ensure-vector a))
         (resind nil)
         (anum (length (the vector a*))))
    (when (> n anum) (error "not enough distinct elements in a."))
    (loop until (>= (hset:num (hset:make :init resind)) n)
          do (setf resind (nrndi n anum)))
    (loop for i in resind collect (aref a* i))))


(defun nrnd-from (n a)
  (declare #.*opt-settings* (fixnum n) (sequence a))
  "n random elements from a."
  (loop for i in (nrndi n (length a)) collect (aref a i)))


(defun array-split (a p)
  (declare #.*opt-settings* (sequence a) (double-float p))
  (let ((res (make-adjustable-vector)))
    (vextend (make-adjustable-vector :init (list (aref a 0))) res)
    (loop for i of-type fixnum from 1 below (length a) do
      (prob p (vextend (make-adjustable-vector :init (list (aref a i))) res)
              (vextend (aref a i) (aref res (1- (length res))))))
    res))


; SHAPES

; some version of mitchell's best candidate algorithm
; https://bl.ocks.org/mbostock/1893974/c5a39633db9c8b1f12c73b069e002c388d4cb9bf
(defun max-distance-sample (n fx &key (sample-num 50)
                                      (res (weir-utils:make-adjustable-vector))
                                      (dstfx #'vec:dst2))
  (declare (fixnum n sample-num) (array res) (function fx dstfx))
  (labels ((-get-cand (c) (second (first c)))
           (-closest (res* c) (loop for v across res*
                                    minimizing (funcall dstfx v c))))
    (loop with wanted-length of-type fixnum = (+ n (length res))
          until (>= (length res) wanted-length)
          do (weir-utils:vextend
               (-get-cand (sort (loop for c in (funcall fx sample-num)
                                      collect (list (-closest res c) c))
                          #'> :key #'first))
               res))
    res))


; TODO: this can be optimized
(defun on-circ (rad &key (xy vec:*zero*))
  (declare #.*opt-settings* (double-float rad) (vec:vec xy))
  (vec:from xy (vec:cos-sin (rnd PII)) rad))


(defun non-circ (n rad &key (xy vec:*zero*))
  (declare #.*opt-settings* (fixnum n) (double-float rad))
  (loop repeat n collect (on-circ rad :xy xy)))


(declaim (inline in-circ))
(defun in-circ (rad &key (xy vec:*zero*))
  (declare #.*opt-settings* (double-float rad))
  (let ((a (rnd))
        (b (rnd)))
    (declare (double-float a b))
    (if (< a b) (setf a (* PII (/ a b)) b (* b rad))
                (let ((d a)) (setf a (* PII (/ b a)) b (* d rad))))
    (vec:vec (+ (vec:vec-x xy) (* (cos a) b))
             (+ (vec:vec-y xy) (* (sin a) b)))))


(defun nin-circ (n rad &key (xy vec:*zero*))
  (declare #.*opt-settings* (fixnum n) (double-float rad))
  (loop repeat n collect (in-circ rad :xy xy)))


(declaim (inline in-rect))
(defun in-rect (sx sy &key (xy vec:*zero*))
  (declare #.*opt-settings* (double-float sx sy) (vec:vec xy))
  (vec:vec (+ (vec:vec-x xy) (rnd* sx))
           (+ (vec:vec-y xy) (rnd* sy))))

(defun in-square (s &key (xy vec:*zero*))
  (declare #.*opt-settings* (double-float s) (vec:vec xy))
  (vec:vec (+ (vec:vec-x xy) (rnd* s))
           (+ (vec:vec-y xy) (rnd* s))))


(defun nin-rect (n sx sy &key (xy vec:*zero*))
  (declare #.*opt-settings* (fixnum n) (double-float sx sy) (vec:vec xy))
  (loop repeat n collect (in-rect sx sy :xy xy)))

(defun nin-square (n s &key (xy vec:*zero*))
  (declare #.*opt-settings* (fixnum n) (double-float s) (vec:vec xy))
  (loop repeat n collect (in-square s :xy xy)))


;TODO: improve. avoid extra vec creation
(defun on-line (a b)
  (declare #.*opt-settings* (vec:vec a b))
  (vec:from a (vec:sub b a) (rnd)))

(defun on-line* (ab)
  (declare #.*opt-settings* (list ab))
  (apply #'on-line ab))


;TODO: improve. avoid extra vec creation
(defun non-line (n a b)
  (declare #.*opt-settings* (fixnum n) (vec:vec a b))
  (loop with ba = (vec:sub b a)
        repeat n
        collect (vec:from a ba (rnd))))

(defun non-line* (n ab)
  (declare #.*opt-settings* (fixnum n) (list ab))
  (apply #'non-line n ab))


(defmacro with-in-circ ((n rad v &key xy) &body body)
  (declare (symbol v))
  (alexandria:with-gensyms (rad* xy* m)
    `(let* ((,rad* ,rad)
            (,xy* ,xy)
            (,m (if ,xy* ,xy* vec:*zero*)))
      (declare (vec:vec ,m))
      (loop repeat ,n
            do (let ((,v (in-circ ,rad* :xy ,m)))
                 (declare (vec:vec ,v))
                 (progn ,@body))))))

(defmacro with-in-rect ((n sx sy v &key xy) &body body)
  (declare (symbol v))
  (alexandria:with-gensyms (sx* sy* xy* m)
    `(let* ((,sx* ,sx)
            (,sy* ,sy)
            (,xy* ,xy)
            (,m (if ,xy* ,xy* vec:*zero*)))
      (declare (vec:vec ,m))
      (loop repeat ,n
            do (let ((,v (in-rect ,sx* ,sy* :xy ,m)))
                 (declare (vec:vec ,v))
                 (progn ,@body))))))

(defmacro with-on-line ((n a b rn) &body body)
  (declare (symbol rn))
  (alexandria:with-gensyms (sub a*)
    `(let* ((,a* ,a)
            (,sub (vec:sub ,b ,a*)))
      (loop repeat ,n
            do (let ((,rn  (vec:from ,a* ,sub (rnd))))
                 (declare (vec:vec ,rn))
                 (progn ,@body))))))

