
(in-package :ortho)

"
Simple orthographic projection with camera position (cam), view vector (vpn)
view plane offset (xy) and scaling (s).
"

(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))

(declaim (inline make-ortho))
(defstruct (ortho)
  (vpn vec:*3zero* :type vec:3vec :read-only nil) ; away from dop
  (up vec:*3zero* :type vec:3vec :read-only nil) ; cam up
  (cam vec:*3zero* :type vec:3vec :read-only nil) ; cam position
  (u vec:*3zero* :type vec:3vec :read-only nil) ; view plane horizontal
  (v vec:*3zero* :type vec:3vec :read-only nil) ; view plane vertical
  (su vec:*3zero* :type vec:3vec :read-only nil) ; u scaled by s
  (sv vec:*3zero* :type vec:3vec :read-only nil) ; v scaled by s
  (xy vec:*zero* :type vec:vec :read-only nil) ; view plane offset
  (s 1d0 :type double-float :read-only nil)  ; scale
  (raylen 5000d0 :type double-float :read-only nil)
  (projfx #'identity :type function :read-only nil)
  (dstfx #'identity :type function :read-only nil)
  (rayfx #'identity :type function :read-only nil))


(declaim (inline -get-u-v))
(defun -get-u-v (up vpn s)
  (declare #.*opt-settings* (vec:3vec up vpn) (double-float s))

  (assert (< (abs (vec:3dot up vpn)) 0.9999d0) (up vpn)
          "ortho: gimbal lock. up: ~a vpn: ~a" up vpn)

  (let* ((v (vec:3norm! (vec:3neg (vec:3norm-reject up vpn))))
         (u (vec:3rot v vpn PI5)))
    (declare (vec:3vec u v))
    (values u v (vec:3smult u s) (vec:3smult v s))))


(declaim (inline -look))
(defun -look (cam look)
  (vec:3norm! (vec:3sub cam look)))


(defun make-dstfx (proj)
  (declare #.*opt-settings*)
  "
  distance from pt to camera plane with current parameters
  "
  (let ((cam (ortho-cam proj))
        (vpn (ortho-vpn proj)))
    (declare (vec:3vec cam vpn))
    (lambda (pt) (declare (vec:3vec pt))
      (multiple-value-bind (x d) (vec:3planex vpn cam (list pt (vec:3add pt vpn)))
        (declare (boolean x) (double-float d))
        (if x d 0d0)))))


(defun make-projfx (proj)
  (declare #.*opt-settings*)
  "
  function to project pt into 2d with current parameters
  "
  (let ((su (ortho-su proj))
        (sv (ortho-sv proj))
        (cam (ortho-cam proj))
        (xy (ortho-xy proj)))
    (declare (vec:3vec su sv cam) (vec:vec xy))
    (vec:with-xy (xy x y)
      (lambda (pt) (declare (vec:3vec pt))
        (let ((pt* (vec:3sub pt cam)))
          (declare (vec:3vec pt*))
          (vec:vec (+ x (vec:3dot su pt*))
                   (+ y (vec:3dot sv pt*))))))))


(defun make-rayfx (proj)
  (declare #.*opt-settings* (ortho proj))
  "
  cast a ray in direction -vpn from pt
  "
  (let ((dir (vec:3smult! (vec:3neg (ortho-vpn proj)) (ortho-raylen proj))))
    (declare (vec:3vec dir))
    (lambda (pt) (declare (vec:3vec pt))
      (list pt (vec:3add pt dir)))))


(defun make (&key (up (vec:3vec 0d0 0d0 1d0)) (cam (vec:3rep 1000d0))
                  (xy vec:*zero*) (s 1d0) vpn look (raylen 5000d0))
  (declare (vec:3vec up cam) (double-float s raylen) (vec:vec xy))
  "
  make projection.

  default up is (0 0 1)
  default cam is (1000 1000 1000)
  if look and vpn are unset, the camera will look at the origin.

  default scale is 1
  default xy is (0 0)
  "

  (assert (not (and vpn look)) (vpn look)
          "make: can only use (or vpn look)." vpn look)

  (let ((vpn* (if vpn vpn (-look cam (if look look vec:*3zero*)))))
    (multiple-value-bind (u v su sv) (-get-u-v up vpn* s)
      (declare (vec:3vec u v su sv))
      (let ((res (make-ortho :vpn vpn* :up up :cam cam
                             :u u :v v :su su :sv sv
                             :s s :xy xy
                             :raylen raylen)))
        (setf (ortho-dstfx res) (make-dstfx res)
              (ortho-projfx res) (make-projfx res)
              (ortho-rayfx res) (make-rayfx res))
        res))))


(defun export-data (proj)
  (declare (ortho proj))
  (with-struct (ortho- up cam xy s vpn raylen) proj
    (list :ortho up cam xy s vpn raylen)))

(defun import-data (o)
  (declare (list o))
  (destructuring-bind (up cam xy s vpn raylen) (cdr o)
    (make :up up :cam cam :xy xy :s s :vpn vpn :raylen raylen)))


(defun update (proj &key s xy up cam vpn look)
  "
  update projection parameters.

  use vpn to set view plane normal directly, or look to set view plane normal
  relative to camera.

  ensures that internal state is updated appropriately.
  "
  (declare #.*opt-settings* (ortho proj))

  (assert (not (and vpn look)) (vpn look)
          "update: can only use (or vpn look)." vpn look)

  (when cam (setf (ortho-cam proj) (the vec:3vec cam)))
  (when up (setf (ortho-up proj) (the vec:3vec up)))
  (when vpn (setf (ortho-vpn proj) (the vec:3vec vpn)))
  (when look (setf (ortho-vpn proj)
                   (the vec:3vec (-look (ortho-cam proj) look))))
  (when s (setf (ortho-s proj) (the double-float s)))
  (when xy (setf (ortho-xy proj) (the vec:vec xy)))
  (when (or s up vpn look)
        (multiple-value-bind (u v su sv)
          (-get-u-v (ortho-up proj) (ortho-vpn proj) (ortho-s proj))
          (declare (vec:3vec u v su sv))
          (setf (ortho-u proj) u (ortho-v proj) v
                (ortho-su proj) su (ortho-sv proj) sv)))

  (setf (ortho-dstfx proj) (make-dstfx proj)
        (ortho-projfx proj) (make-projfx proj)
        (ortho-rayfx proj) (make-rayfx proj)))

(defun zoom (proj s)
  (declare (ortho proj) (double-float s))
  (update proj :s (+ (ortho-s proj) s)))

(defun pan-xy (proj xy)
  (declare (ortho proj) (vec:vec xy))
  (update proj :xy (vec:add (ortho-xy proj) xy)))

(defun pan-cam (proj xy)
  (declare (ortho proj) (vec:vec xy))
  (let ((diff (vec:3add (vec:3smult (ortho-su proj) (vec:vec-x xy))
                        (vec:3smult (ortho-sv proj) (vec:vec-y xy)))))
    (update proj :cam (vec:3add (ortho-cam proj) diff))
    diff))

(defun rotate (proj &key azimuth zenith (look (vec:zero)))
  (declare (ortho proj))
  (let* ((cam (ortho-cam proj))
         (up (ortho-up proj))
         (up-x-vpn (vec:3norm (vec:3cross up (ortho-vpn proj)))))
    (when azimuth (update proj :cam (vec:3rot cam up azimuth :xy look)
                               :look look))
    (when zenith (update proj :cam (vec:3rot cam up-x-vpn zenith :xy look)
                              :look look))))


(declaim (inline project))
(defun project (proj pt)
  (declare #.*opt-settings* (ortho proj) (vec:3vec pt))
  "
  project single point
  "
  (values (funcall (ortho-projfx proj) pt)
          (funcall (ortho-dstfx proj) pt)))


(declaim (inline project*))
(defun project* (proj path)
  (declare #.*opt-settings* (ortho proj) (list path))
  "
  project list of points
  returns list (pts dsts)
  "
  (with-struct (ortho- projfx dstfx) proj
    (declare (function projfx dstfx))
    (loop for pt of-type vec:3vec in path
          collect (list (funcall projfx pt)
                        (funcall dstfx pt)))))


(declaim (inline -identity2))
(defun -identity2 (&key xy d)
  (declare (vec:vec xy) (ignore d))
  xy)

(declaim (inline project-offset))
(defun project-offset (proj pt &key (fx #'-identity2))
  (declare #.*opt-settings* (ortho proj) (vec:3vec pt) (function fx))
  "
  perform (fx :d (dstfx pt) :xy (projfx pt))
  "
  (with-struct (ortho- projfx dstfx) proj
    (declare (function projfx dstfx))
    (let ((d (funcall dstfx pt)))
      (declare (double-float d))
      (values (funcall fx :d d :xy (funcall projfx pt))
              d))))

(declaim (inline project-offset*))
(defun project-offset* (proj path &key (fx #'-identity2))
  (declare #.*opt-settings* (ortho proj) (list path) (function fx))
  "
  see project-offset
  "
  (with-struct (ortho- projfx dstfx) proj
    (declare (function projfx dstfx))
    (loop for pt of-type vec:3vec in path
          collect (let ((d (funcall dstfx pt)))
                    (declare (double-float d))
                    (list (funcall fx :d d :xy (funcall projfx pt))
                          d)))))

