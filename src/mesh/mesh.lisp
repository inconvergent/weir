
(in-package :mesh)

(deftype pos-double () `(double-float 0d0 *))


(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))

(defparameter *nilpoly* (list -1 -1 -1))


(defmacro -valid-vert ((num v &key (err t)) &body body)
  (alexandria:with-gensyms (v* num*)
    `(let ((,v* ,v)
           (,num* ,num))
      (declare (pos-int ,v* ,num*))
      (if (< -1 ,v* ,num*)
        (progn ,@body)
        ,(when err `(error "vert does not exist: ~a" ,v*))))))

(defmacro -valid-verts ((num vv v) &body body)
  (alexandria:with-gensyms (vv* num*)
    `(let ((,vv* ,vv)
           (,num* ,num))
      (declare (list ,vv*) (pos-int ,num*))
      (loop for ,v of-type pos-int in ,vv*
            if (< -1 ,v ,num*) collect (progn ,@body)))))


(defstruct (mesh (:constructor -make-mesh))
  (name :main :type symbol :read-only t)
  (verts nil :type simple-array)
  (polygons nil :type hash-table)
  (edges nil :type hash-table)
  (num-verts 0 :type pos-int)
  (max-verts 5000 :type pos-int :read-only t))


(defun make (&key (max-verts 5000) name)
  "
  make mesh instances

  - max-verts is the maximum number of verts in mesh.

  - edges are represented as lists of two indices, they are always sorted so
    that the first index is the lowest.
  - polygons are represented as lists of three indices.
    original rotation is preserved, but the first index is always the lowest.
    this makes it easier to test for duplicate polygons
  "
  (declare (pos-int max-verts) (symbol name))
  (-make-mesh
    :name name
    :verts (make-array (* max-verts 3) :initial-element 0d0
                                         :element-type 'double-float)
    :max-verts max-verts
    :polygons (make-hash-table :test #'equal)
    :edges (make-hash-table :test #'equal)))


(declaim (inline -sort-list))
(defun -sort-list (l)
  (declare #.*opt-settings* (list l))
  (sort (copy-list l) #'<))


(declaim (inline -sort-polygon))
(defun -sort-polygon (poly)
  (declare #.*opt-settings* (list poly))
  "
  maintain vert order, but ensure that the smallest index is first.
  use -polygon-exists to test for duplicate polygons.
  "
  (destructuring-bind (a b c) poly
    (declare (pos-int a b c))
    (cond ((and (< a c) (< a b)) poly)
          ((and (< b a) (< b c)) (list b c a))
          (t (list c a b)))))


(declaim (inline -ensure-valid-verts))
(defun -ensure-valid-verts (vv num-verts)
  (declare #.*opt-settings* (list vv) (pos-int num-verts))
  (loop for v of-type pos-int in vv
        do (when (>= v num-verts)
                 (return-from -ensure-valid-verts nil)))
  t)

(declaim (inline -ensure-no-duplicates))
(defun -ensure-no-duplicates (vv)
  (declare #.*opt-settings* (list vv))
  (destructuring-bind (a b c) (-sort-list vv)
    (declare (pos-int a b c))
    (assert (< a b c) (a b c) "duplicate indices in ~a" vv)))


(declaim (inline -add-edge))
(defun -add-edge (edges poly a b)
  (declare #.*opt-settings* (hash-table edges) (list poly) (pos-int a b))
  (let ((e (-sort-list (list a b))))
    (multiple-value-bind (pp exists) (gethash e edges)
      (setf (gethash e edges) (the list (if exists (cons poly pp)
                                                   (list poly)))))
    e))

(declaim (inline -duplicate-cands))
(defun -duplicate-cands (poly)
  (declare #.*opt-settings* (list poly))
  (destructuring-bind (a b c) poly
    (list poly (list a c b))))

(declaim (inline -polygon-exists))
(defun -polygon-exists (polygons poly)
  (declare #.*opt-settings* (hash-table polygons) (list poly))
  (loop for cand of-type list in (-duplicate-cands poly)
        do (let ((res (gethash cand polygons)))
             (declare (list res))
             (when res (return-from -polygon-exists cand))))
  nil)

(declaim (inline -add-polygon-edges))
(defun -add-polygon-edges (edges poly)
  (declare #.*opt-settings* (hash-table edges) (list poly))
  (destructuring-bind (a b c) poly
    (declare (pos-int a b c))
    (list (-add-edge edges poly a b)
          (-add-edge edges poly b c)
          (-add-edge edges poly c a))))

(declaim (inline -add-polygon))
(defun -add-polygon (polygons edges poly)
  (declare #.*opt-settings* (hash-table polygons edges) (list poly))
  ; if polygon exists, return existing polygon
  (let ((exists (-polygon-exists polygons poly)))
    (when exists (return-from -add-polygon exists)))
  ; else, add polygon
  (setf (gethash poly polygons) (-add-polygon-edges edges poly))
  poly)

(defun add-polygon! (msh poly)
  (declare #.*opt-settings* (mesh msh) (list poly))
  (-ensure-no-duplicates poly)
  (with-struct (mesh- num-verts edges polygons) msh
    (let ((poly* (-sort-polygon poly)))
      (if (-ensure-valid-verts poly* num-verts)
          (-add-polygon polygons edges poly*)
          (error "mesh: trying to add invalid vert in polygon: ~a" poly*)))))


(defun -del-poly-from-edge (edges poly edge)
  (declare #.*opt-settings* (hash-table edges) (list poly edge))
  (let ((new-poly-list (remove-if (lambda (p) (declare (list p))
                                              (equal p poly))
                                  (gethash edge edges))))
    (declare (list new-poly-list))
    (if new-poly-list (setf (gethash edge edges) new-poly-list)
                      (remhash edge edges))))

(defun del-polygon! (msh poly)
  (declare #.*opt-settings* (mesh msh) (list poly))
  (with-struct (mesh- edges polygons) msh
    (let ((poly* (-polygon-exists polygons poly)))
      (declare (list poly*))
      (if poly*
          ; del poly if exists, return t, else return nil
          (progn (destructuring-bind (a b c) poly*
                   (declare (pos-int a b c))
                   (-del-poly-from-edge edges poly* (-sort-list (list a b)))
                   (-del-poly-from-edge edges poly* (-sort-list (list b c)))
                   (-del-poly-from-edge edges poly* (-sort-list (list c a))))
                 (remhash poly* polygons)
                 t)
          nil))))


(defun add-polygons! (msh pp)
  (declare #.*opt-settings* (mesh msh) (list pp))
  (loop for poly of-type list in pp collect (add-polygon! msh poly)))


(defun -hash-table-get (ht extra)
  (declare #.*opt-settings* (hash-table ht) (boolean extra))
  (if extra (loop for k of-type list being the hash-keys of ht
                    using (hash-value v)
                  collect (list k v) of-type list)
            (loop for k of-type list being the hash-keys of ht
                  collect k of-type list)))

(defun get-all-polygons (msh &key extra)
  (declare #.*opt-settings* (mesh msh) (boolean extra))
  (-hash-table-get (mesh-polygons msh) extra))

(defun get-all-edges (msh &key extra)
  (declare #.*opt-settings* (mesh msh) (boolean extra))
  (-hash-table-get (mesh-edges msh) extra))


(defun -hash-table-get-key (ht key)
  (declare #.*opt-settings* (hash-table ht) (list key))
  (multiple-value-bind (v exists) (gethash key ht)
    (if exists v nil)))

(defun get-polygon-edges (msh poly &key (err t))
  (declare #.*opt-settings* (mesh msh))
  (let* ((poly* (-sort-polygon poly))
         (res (-hash-table-get-key (mesh-polygons msh) poly*)))
    (declare (list res poly*))
    (when (and err (not res))
          (error "mesh: polygon does not exist: ~a" poly*))
    res))

(defun get-edge-polygons (msh edge &key (err t))
  (declare #.*opt-settings* (mesh msh) (list edge))
  (let* ((edge* (-sort-list edge))
         (res (-hash-table-get-key (mesh-edges msh) edge*)))
    (declare (list res edge*))
    (when (and err (not res))
          (error "mesh: edge does not exist: ~a" edge*))
    res))


(defun get-num-verts (msh)
  (declare #.*opt-settings* (mesh msh))
  (mesh-num-verts msh))

(defun get-num-edges (msh)
  (declare #.*opt-settings* (mesh msh))
  (hash-table-count (mesh-edges msh)))

(defun get-num-polygons (msh)
  (declare #.*opt-settings* (mesh msh))
  (hash-table-count (mesh-polygons msh)))

