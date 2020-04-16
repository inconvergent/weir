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
  as well as cross products `cross`, dot products, `dot`. There are also some
  common geometric operations, such as line-line intersection, angles, and
  line-plane intersection. (See the code in the package for more details.)

  *Note: Functions that operate on 3d vectors are prefixed with `3`.*

  Furthermore there are corresponding functions for scalars, lists of vectors,
  and broadcasting. They are indicated by prefix `l` and `s`:
  ```lisp
  ; add scalar s to a, return result
  (vec:sadd a s)
  ; add vectors in two lists, returns list of new vectors
  (vec:ladd aa bb)
  ; add b to elements of aa, return list of new vectors
  (vec:ladd* aa b)
  ```

  Most of these functions also have a corresponing function postfixed with `!`,
  which performs the same operation, but assigns the result to the first
  argument. This is faster since there is no need to create a new `vec` struct.

  *Note: This can cause strange behaviour since you can inadvertedly change the
  internal state of a struct. Use with caution.)*

  Example:
  ```lisp
  ; add b to a, store result in a; also returns a
  (vec:add! a b)
  ```

2. A simple graph data structure, `weir`, for working with vertices and edges.
   The structure is combined with a programming pattern for applying changes to
   the structure. The pattern relies on `alterations`, see below. You can also
   manipulate the structure directly.

   Below is a very basic example
  ```lisp
  (let ((wer (weir:make))) ; use :dim 3 for 3d
    (weir:add-edge! wer (weir:add-vert! wer (vec:vec 1d0 2d0))
                        (weir:add-vert! wer (vec:vec 2d0 3d0))))
  ```
  See the `examples` folder for more.

3. A series of other useful data structures and tools. E.g. a package for
   generating different kinds of random numbers: `rnd`, as well as tools for
   handling colors `pigment`, splines `bzspl`, and various vector and path
   functionality. Eg. `math` and `lin-path`.

4. Orthogonal projection `ortho`.

5. Some simple raytracing functionality for triangle meshes `mesh` and point
   clouds `point-cloud`. This is immature and slow.

6. A tool for drawing `pngs` called `sandpaint`. This package uses random
   sampling to draw. This creates a fairly distinct and gritty look in many
   cases.

7. A tool for drawing `svg` files: `draw-svg`. Mainly files that are good for
   plotting.


## Weir Graphs and Alterations

An `alteration` is a change that will be applied to the structure at the end of
a given context. In many ways this is similar to map/reduce.

Here is and example of manipulating a `weir` instance called `wer` using
`alterations`. Alteration constructors are postfixed with `?`.

```lisp
; context start
(let (wer (weir:make))

  ; add some data to wer here ...

  (weir:with (wer %)
    ; iterate vertices
    (weir:itr-verts (wer v)
      ; move alteration
      ; (% ...) is used to accumulate alterations
      (% (weir:move-vert? v (rnd:in-circ 1d0)))
      ; w will be an arbitrary
      ; vertex in wer
      (weir:with-rnd-vert (wer w)
        ; join v and w if they are closer than d
        (if (< (weir:edge-length wer v w) d)
          ; join vertices alteration
          (% (weir:add-edge? v w))))))
; context end. alterations have been applied
```

You can also manipulate the state directly. These functions are postfixed with
`!`.  Eg. `(weir:move-vert! ...)`. This convention is used in other parts of
the code as well.

You can define your own arbitrary alterations. There is an example of this in
`ex/custom-alt.lisp`.


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

I have written about things related to this code (when it was called `snek`) at

  - https://inconvergent.net/2017/snek-is-not-an-acronym/
  - https://inconvergent.net/2017/a-method-for-mistakes/
  - https://inconvergent.net/2017/arbitrary-alterations/
  - https://inconvergent.net/2017/grains-of-sand/
  - https://inconvergent.net/2017/a-propensity-for-mistakes/


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

The code has only been tested in `Ubuntu 18.04 LTS` with `SBCL 2.0.1`.


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

Who have provided me with useful hints and code feedback.

The ASDF config and test setup was kindly suggested and implemented by Robert
Smith (https://twitter.com/stylewarning). The remaining weirdness in the test
system is my fault. Hope to fix it properly later.

Also, many thanks to https://twitter.com/xach for making Quicklisp.

