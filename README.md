# WEIR-A System for Making Generative Systems


## About

This library is specifically written to be useful for a broad range of ways in
which I create art using various generative algorithms. It is the next
iteration of [snek](https://github.com/inconvergent/snek). I made a new version
because I wanted to make some significant changes.

Main components:

1. 2d and 3d vectors with corresponding operations:
   ```lisp
   ; create a 2d vector
   (vec:vec 1d0 3d0)

   ; create a 3d vector
   (vec:3vec 1d0 2d0 3d0)
   ```
   `vec` supports common vector operations such as `add`, `mult`, `div`, `sub`,
   as well as cross products `cross`, dot products, `dot`.

   *Note: Functions that operate on 3d vectors are prefixed with `3`.*

   Furthermore there are corresponding functions for scalars, lists of vectors,
   and broadcasting. They are indicated by prefix `l` and `s`:
   ```lisp
   ; add scalar s to a, return result
   (vec:sadd va s)

   ; add vectors in two lists, returns list of new vectors
   (vec:ladd aa bb)

   ; add b to elements of aa, return list of new vectors
   (vec:ladd* aa vb)
   ```

   Most of these functions also have a corresponding function postfixed with `!`,
   which performs the same operation, but assigns the result to the first
   argument. This is faster since there is no need to create a new `vec` struct.

   *Note: This can cause strange behaviour since you can inadvertently change the
   internal state of a struct. Use with caution.*

   Example:
   ```lisp
   ; add b to a, store result in a; also returns a
   (vec:add! va vb)
   ```
   There are also some common geometric operations, such as line interpolation,
   line-line intersection, angles, and line-plane intersection.
   ```lisp
   ; find the position s between a and b. s should be between 0 and 1
   (vec:on-line s va vb)
   ; find intersection of two lines, returns
   ; a bool, and interpolation value along line 1, line 2
   (vec:segx line1 line2)
   ```
   See the code in the package for more details.

2. Random numbers, some examples:
   ```lisp
   ; random double 0d0 and a (default: 1d0)
   (rnd:rnd a)

   ; ditto, between -a and a
   (rnd:rnd* a)

   ; between a and b
   (rnd:rndrng a b)

   ; random fixnum
   (rnd:rndi 10)

   ; return n random numbers between a and b
   (rnd:rndspace n a b)

   ; random number from normal distribution
   (rnd:norm :mu 0d0 :sigma 1d0)

   ; uniform in circle of radius a at centered at p
   (rnd:in-circ a :xy p)

   ; uniform numbers in a rectangle
   (rnd:in-rect w h :xy p)

   ; pick a random point between points va and vb
   (rnd:on-line va vb)

   ; execute a form with a certain probability
   ; second form is optional
   (rnd:prob 0.1d0 (print "10% hi") (print "90% oh no"))

   ; perform either form 1 or 2
   (rnd:either (print "form 1") (print "form 2"))

   ; repeat the form at most n-1 times
   (rnd:rep n (print 1))
   ```
   There are also some utils for random 3d vector numbers, see
   `src/rnd/3rnd.lisp`.

3. A simple graph data structure, `weir`, for working with vertices and edges.
   The structure is combined with a programming pattern for applying changes to
   the structure. The pattern relies on `alterations`, see below. You can also
   manipulate the structure directly.

   Below is a small example:
   ```lisp
   (let ((wer (weir:make))) ; use :dim 3 for 3d
     ; add three edges
     (loop repeat 3
           do (weir:add-edge! wer
                (weir:add-vert! wer (rnd:in-circ 200d0))
                (weir:add-vert! wer (rnd:in-circ 200d0
                                :xy (vec:rep 500d0)))))
     ; iterate verts
     (weir:itr-verts (wer v) (print (weir:get-vert wer v)))

     ; move a vert relativ to current position:
     (weir:move-vert! wer 0 (vec:vec 1d0 2d0))
     ; move a vert to an absolute position
     (weir:move-vert! wer 1 (vec:vec 1d0 2d0) :rel nil)

     ; iterate edges
     (weir:itr-edges (wer vv) (print (weir:get-verts wer vv)))

     ; edges are represented as lists of verts, and they are always
     ; sorted with the smallest vert index first, so both
     (weir:edge-exists wer '(0 1)) ; and
     (weir:edge-exists wer '(1 0)) ; returns t

     ; get edges incident to vert 0
     (weir:get-incident-edges wer 0))
   ```
   See the `examples` folder for more.

4. A series of other useful data structures and tools. E.g. a package for
   handling colors (`pigment`), splines (`bzspl`), and various vector and path
   functionality. Eg. `math`, `lin-path` and `simplify-path`.
   ```lisp
   (let* ((points (rnd:nin-circ 5 400d0))
          (bz (bzspl:make points))
          (lp (lin-path:make points)))
     ; sample a point on the spline
     (bzspl:pos bz (rnd:rnd))
     ; sample a point on path
     (lin-path:pos lp (rnd:rnd))

     ; represent the spline with a limited number of points
     (bzspl:adaptive-pos bz :lim 1d0))

   ; return n numbers evenly spaced between a and b, inclusive
   (math:linspace n a b :end t)

   ; all fixnums from a to b-1
   (math:range a b)

   ; repeat the form n times
   (math:nrep n (rnd:rnd))
   ```

5. Orthogonal projection `ortho`:
   ```lisp
   (let ((proj (ortho:make :s 1d0
                           :xy (vec:rep 500d0)
                           :cam (vec:3vec 1000d0 1000d0 0d0)
                           :look (vec:3zero))))
     (multiple-value-bind (v d)
       (ortho:project proj (rnd:3in-sphere :rad 300d0))
       ; point in 2d
       (print v)
       ; distance from 3d point to camera plane
       (print d))

     ; update cam position and look at something else
     (ortho:update proj :cam (vec:3vec 3d0 4d0 1d0)
                        :look (vec:3rep 79d0)))
   ```

6. A tool for drawing `svg` files: `draw-svg`. Mainly files that are good for
   plotting.
   ```lisp
   (let ((psvg (draw-svg:make :stroke "black"))
         (pts (list (vec:vec 10d0 20d0) (vec:vec 20d0 30d0)
                    (vec:vec 10d0 50d0))))
     ; sw is the stroke width
     (draw-svg:path psvg pts :sw 3d0)
     (draw-svg:bzspl psvg pts :sw 3d0 :so 0.5d0)
     (draw-svg:circ psvg (vec:rep 30d0) 10d0 :sw 3d0 :stroke "red")
     (draw-svg:save psvg "filename"))
   ```

7. A tool for drawing `pngs` called `sandpaint`. This package uses random
   sampling to draw. This creates a fairly distinct and gritty look in many
   cases. Also supports direct pixel manipulations and a few filters.


## Weir Graphs and Alterations

An `alteration` is a change that will be applied to the structure at the end of
a given context. In practical terms, an alteration is a function that returns a
`lambda` (or just a `lambda`).

The main motivation behid this is that this makes it possible to "queue" up a
number of changes that will be applied at a later time. This makes it possible
to access the state in the `weir` instance while you are creating the
alterations. Without there being any changes made to the state of the `weir`
instance while the alterations are being created. Once all alterations are
created, they will be applied.

Existing alterations in `weir` are postfixed `?` by convention, and it might
look like this:

```lisp
(weir:with (wer %)
  ( ; some code
    (% (weir:add-vert? ...))
    ; more code
    (% (weir:add-edge? ...))))
```

all `(% ...)` forms inside the weir context will cause the alteration inside to
be created and collected. They will be executed at the end of the context. if
an alteration evaluates to nil, nothing will happen.

### Names and Args

You can assign a name (`:res`) to the result of an alteration. This makes it
possible to create alterations that depend (`:arg`) on the result of other
alterations:

```lisp
(weir:with (wer %)
  (let ((pt (...)))
    (% (weir:add-vert? pt) :res :a) ; alteration result is named :a
    (% (weir:add-vert? (vec:vec 1d0 2d0)) :res 'b) ; result is named 'b
    (% (weir:add-edge? :a 'b) :arg (:a 'b)))) ; uses :a and 'b
```

Note that `res` must be a keyword, symbol, or a variable with keyword or symbol
value. Similarly, `arg` must be a list of `res` elements that exist inside the
same context. make sure that all elements in the `:arg` are present in the
context, or the code will loop infinitely.

it is always possible to both reference future results, and assign the result a
name. The order the order of `:res` and `:arg` does not matter:

```lisp
(% (some-alteration? :a :b) :res :x :arg (:a :b)) ; is equivalent to
(% (some-alteration? :a :b) :arg (:a :b) :res :x)
```

Results will be available after the `(with:weir ...)` context. See
`(get-alteration-result-list)` or `(get-alteration-result-map)`.  Also, note
that using the same name for multiple alterations _will_ result in undefined
behaviour.

### Dependency and Futures

You can consider a named alteration as something akin to a _future_; the value
of `:res` is a reference to a value that does not yet exist. For this to work,
any alteration that depends on a future that fails to be fulfilled will be
skipped.

As an example, we can make the alteration `prob-add-edge?` like this:

```lisp
(defun prob-add-edge? (l a b)
  'add edge (a b) with probability p'
  (lambda (w) (when (< (rnd:rnd 100d0) l)
                    (add-edge! w a b))))
```

This will attempt to create edge `(a b)`, but only if the random number is less
than `l`.  This is to illustrate that the alteration may or may not attempt to
`weir` instance.  If no edge is created, the above lambda will return `nil`.

Here is a an example of use:

```lisp
; context start
(let (wer (weir:make))

  ; add some data to wer here ...

  ; (% ...) is used to accumulate alterations
  ; alterations are applied at the end of (weir:with ...)
  (weir:with (wer %)
    ; iterate all vertices in wer
    (weir:itr-verts (wer v)
      (% (move-vert? v (rnd:in-circ 10d0)))
      ; w will be an arbitrary vertex in wer
      (weir:with-rnd-vert (wer w)
        (% (prob-add-edge? (weir:edge-length wer v w) v w))))))
```

The important thing to note here is that it is _crucial_ that the
length of edge `(v w)` is calculated outside `(defun prob-add-edge? ...)`.
This ensure that `(weir:edge-length wer v w)` is the length of the edge
_before_ the calls to `(move-vert? ...)` have a chance to move either `v` or
`w`.

Naturally, you can construct an alteration that checks the length of the edge
inside the  `lambda` in `(prob-add-edge? ...)`, but this will result in
different behaviour. In this case, any edge length will be calculated while
all the other vertices are moving around. Thus resulting in what can be
considered "unexpected side effects".

This becomes more clear if you consider an n-body simulation. If any single
body in the simulation moves before you have calculated the force between each
pair of bodies, you will get an incorrect result.

### Shadowing

As we have mentioned, arguments to an alteration may, or may not, exist right
away.  To handle this, arguments to an alteration will be shadowed before the
alteration is collected. This applies to arguments that are atoms, or forms
that do not contain a reference to a future alteration result. This is the
behaviour you will usually want in an example such as the one above. But it
might cause unexpected behaviour.

As an example of what happes, consider the alteration:

```lisp
(% (my-alteration? (first var-1)
                   :a
                   (my-function (rnd:rnd) :b (second var-2)))
   :arg (:a :b))
```

This will be expanded by the macro to something similar to:

```lisp
(LET ((#:|non-atom:91| (FIRST VAR-1)) ; values are evaluated at time of collection
      (#:|non-atom:92| (RND:RND))
      (#:|non-atom:93| (SECOND VAR-2)))
  (LAMBDA (#:WNAME90)
    (CASE (WEIR::-IF-ALL-RESOLVED #:ALT-RES88 (LIST :A :B))
      (:OK ; :A and :B both have a value
       (VALUES T
               (FUNCALL
                (THE FUNCTION
                     (MY-ALTERATION? #:|non-atom:91| (GETHASH :A #:ALT-RES88)
                      (MY-FUNCTION #:|non-atom:92| (GETHASH :B #:ALT-RES88)
                                     #:|non-atom:93|)))
                #:WNAME90)))
      (:BAIL (VALUES T NIL)) ; either :A or :B returned nil. skip alteration
      (:WAIT (VALUES NIL NIL))))) ; :A or :B does not yet exist
```

As you can see, the variables/forms that will be shadowed here are: `(first var-1)`,
`(second var-2)` and `(rnd:rnd)`.

You can use `(weir:with ... :bd t)` to see how an alteration is expanded. This
might make it easier to find issues with shadowed/non-shadowed variables. Also,
you can usually solve some problems you might encounter by defining custom
alterations locally (but outside `weir:with`) using `(labels ())`.

### Looping

It is possible to use `:ref` and `:arg` inside loops as well. but it requires
a bit more careful consideration. Here is an example:

```lisp
(weir:with (wer % :db t)
  (loop for x in (math:linspace 20 -20d0 20d0)
        do (loop for z in (list 1d0 2d0)
                 do (let ((xy (vec:vec x y z))
                          (s (vec:vec 1d0 80d0))
                          (g (gensym "g"))) ; create a distinct name
                      (% (weir:add-grp? :name (gensym "line")) :res g)
                      (% (weir:add-path? (list (vec:sub xy s) (vec:add xy s))
                                         :g g)
                         :arg (g))))))
```

The second alteration will be expanded to:

```lisp
(LET ((#:|non-atom:8| (LIST (VEC:SUB XY S) (VEC:ADD XY S))))
  (LAMBDA (#:WNAME7)
    ; every G is now a distinct future
    (CASE (WEIR::-IF-ALL-RESOLVED #:ALT-RES3 (LIST G))
      (:OK
       (VALUES T
               (FUNCALL
                (THE FUNCTION
                     (WEIR:ADD-PATH? #:|non-atom:8| :G
                                     (GETHASH G #:ALT-RES3)))
                #:WNAME7)))
      (:BAIL (VALUES T NIL))
      (:WAIT (VALUES NIL NIL)))))

```

### Custom Alterations

You can define your own arbitrary alterations. There is an example of
custom alterations and references in `examples/custom-alt.lisp`.

### Final Note

In the previous implementations of the `(weir:with ...)` context, `(% ...)` was
a function. This ensured that the arguments to the alteration, and indeed the
lambda inside the alteration, was created before the end of the context. This
eradicated the need for these complex shadowing rules. I'm not currently sure
whether it is possible to avoid shadowing as long as some arguments do not
exist at the time the alteration is collected.

Also, I use the term "shadowing" above. Not sure if this is really appropriate,
but I failed to think of a better term


## Use

I use `weir` for most of the work that I post online
(https://inconvergent.net/, https://img.inconvergent.net/,
https://twitter.com/inconvergent). Both for raster images, as well as vector
images for plotter drawings.

Here are some plotted examples:

  - https://inconvergent.net/2017/spline-script-plots/
  - https://inconvergent.net/mechanical-plotter-drawings/
  - https://inconvergent.net/mechanical-plotter-drawings/3/
  - https://inconvergent.net/mechanical-plotter-drawings/5/


## Writing

I have written about things related to this code (when it was called `snek`) at:

  - https://inconvergent.net/2017/snek-is-not-an-acronym/
  - https://inconvergent.net/2017/a-method-for-mistakes/
  - https://inconvergent.net/2017/arbitrary-alterations/
  - https://inconvergent.net/2017/grains-of-sand/
  - https://inconvergent.net/2017/a-propensity-for-mistakes/

And recently at:

  - https://inconvergent.net/2020/future-alterations/


## On Use and Contributions

This code is written for my personal use, and parts of it is rather
experimental. Also, it is likely to change at my whim. For this reason I don't
recommend depending on this library for anything.

I release it publicly in case people find it useful or interesting. It is not,
however, intended as a collaboration/Open Source project. As such I am unlikely
to accept PRs, reply to issues, or take requests.


## Installation and Dependencies

This code requires Quicklisp to install dependencies (which are listed in
`weir.asd`). To install and load Weir, do:
```lisp
(ql:quickload :weir)
```
If this does not work, Weir may not be in a place Quicklisp or ASDF can see
them. To fix this, either
```lisp
(load "weir.asd")
```
or, for a long term solution, push the directory in which Weir sits to the
variable `quicklisp:*local-project-directories*`:
```lisp
; in your .sbclrc, for example:
#+quicklisp
(push "/path/to/dir/containing/weir" ql:*local-project-directories*)
```

The `fn` package (for generating file names) depends on the `fn` command from
https://github.com/inconvergent/fn, but this is not necessary to use any of the
other packages.

The code has only been tested in `Ubuntu 18.04 LTS` with `SBCL 2.0.1`. I've
been told that examples work with `SBCL` in `macOS`.


## Tests

Run:
```lisp
(asdf:test-system :weir)
```


## In Case of QL Version Issues

See http://blog.quicklisp.org/2011/08/going-back-in-dist-time.html

Summary:
```lisp
(use-package :ql-dist)
; see versions
(available-versions (dist "quicklisp"))
; select a dist version
(install-dist
  "http://beta.quicklisp.org/dist/quicklisp/2019-03-07/distinfo.txt"
  :replace t)
```


## Thanks

I would like to thank:

  - https://twitter.com/RainerJoswig
  - https://twitter.com/jackrusher
  - https://twitter.com/paulg
  - https://twitter.com/porglezomp
  - https://twitter.com/stylewarning
  - https://github.com/Hellseher

Who have provided me with useful hints and code feedback.

The ASDF config and test setup was kindly suggested and implemented by Robert
Smith (https://twitter.com/stylewarning). The remaining weirdness in the test
system is my fault. Hope to fix it properly later.

Also, many thanks to https://twitter.com/xach for making Quicklisp.

