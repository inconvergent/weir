# WEIR-A System for Making Generative Systems


## About

This library is specifically written to be useful for a broad range of ways in
which I create art using various generative algorithms. It is the next
iteration of [SNEK](https://github.com/inconvergent/snek). I made a new version
because I wanted to make some significant changes.

Main components:

1. A simple graph data structure (`weir`) for working with vertices and edges
   This structure is combined with a programming pattern for applying changes
   to the structure. The pattern relies on `alterations`, see below. You can
   also manipulate the structure directly.

2. A series of useful data structures and tools. E.g. 2D/3D vectors `vec`, a
   package for generating different kinds of random numbers: `rnd`, as well as
   tools for handling colors (`pigment`), splines (`bzspl`), and various vector
   an path functionality (eg. `math`, `lin-path`).

3. Orthogonal projection (`ortho`).

4. Some simple raytracing functionality for meshes (`mesh`) and point clouds
   (`point-cloud`).

5. A tool for drawing, called `sandpaint`. `sandpaint` uses random sampling to
   draw. This creates a fairly distinct and gritty look in many cases.

6. A tool for drawing `svg` files (`draw-svg`). Mainly files that are good for
   plotting.

7. A tool for drawing `png` files (`sandpaint`).


## Alterations

An `alteration` is a change that will be applied to the structure at the end of
a given context. In many ways this is similar to map/reduce.

Here is and example of manipulating a `weir` instance called `wer` using
`alterations`. Alteration constructors are postfixed with `?`.

```lisp
; context start
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
        (% (weir:add-edge? v w)))))
; context end. alterations have been applied
```

You can also manipulate the state directly. These functions are postfixed with
`!`.  Eg. `(weir:move-vert! ...)`. This convention is used in other parts of
the code as well.

You can define your own arbitrary alterations. There is an example of this in
`ex/custom-alt.lisp`.


## Writing

I have written about things related to this code (when it was called `snek`) at

  - https://inconvergent.net/2017/snek-is-not-an-acronym/
  - https://inconvergent.net/2017/a-method-for-mistakes/
  - https://inconvergent.net/2017/arbitrary-alterations/
  - https://inconvergent.net/2017/grains-of-sand/
  - https://inconvergent.net/2017/a-propensity-for-mistakes/


## Usage

I use `weir` for most of the work that I post online
(https://inconvergent.net/, https://img.inconvergent.net/,
https://twitter.com/inconvergent). Both for raster images, as well as vector
images for plotter drawings.

Here are some plotted examples:

 - https://inconvergent.net/2017/spline-script-plots/
 - https://inconvergent.net/mechanical-plotter-drawings/
 - https://inconvergent.net/mechanical-plotter-drawings/3/
 - https://inconvergent.net/mechanical-plotter-drawings/5/


## Installation and Dependencies

This code requires `Quicklisp`, `alexandria`, `libpng-dev`, `zpng`, `cl-svg`,
`inferior-shell`, `cl-png` and `cl-json`. Install by running `install.lisp`.

The path to Quicklisp (https://www.quicklisp.org/beta/) must be set in
`src/load`. `zpng`, `cl-svg` and `cl-png` are automatically installed via
`quicklisp`.

The `fn` package (for generating file names) depends on the `fn` command from
https://github.com/inconvergent/fn, but this is not necessary to use any of the
other packages.

The code has only been tested in Ubuntu 18.04 LTS with SBCL 2.0.1.


## Tests

See the `test` folder.


## In Case of QL Version Issues

See http://blog.quicklisp.org/2011/08/going-back-in-dist-time.html

Summary:

    (use-package :ql-dist)
    (available-versions (dist "quicklisp"))
    (install-dist
      "http://beta.quicklisp.org/dist/quicklisp/2019-03-07/distinfo.txt"
      :replace t)


## Stability, Changes and Versioning

This code is highly experimental on my part. It is likely to change with no
warning or explanation. I will keep a note of the version number in
`src/load.lisp`, but even minor version bumps may contain breaking changes.


## On Use and Contributions

This code is written for my personal use. I release it publicly in case people
find it useful. It is not however intended as a collaboration/Open Source
project. As such I am unlikely to accept PRs, reply to issues, or take
requests.


## Thanks

I would like to thank:

  - https://twitter.com/RainerJoswig
  - https://twitter.com/jackrusher
  - https://twitter.com/paulg
  - https://twitter.com/porglezomp

Who have provided me with useful hints and code feedback.

