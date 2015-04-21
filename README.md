# Abstract

*cl-libnuma* is a set of CFFI bindings for numa(3).

*cl-libnuma* allows you following operations on Lisp:

- Getting current NUMA environments.
- Changing NUMA policies after the Lisp launched.
- Getting memories with custom NUMA policies.
  - Using NUMA-aware allocators in libnuma.
  - Setting NUMA policies to static memories, allocated out of the Lisp heap.


In addition, this library contains *cl-libnuma.error-handling*.
This is a lispy error handling scheme built around *cl-libnuma*.


# License

the MIT License. See LICENSE file.


# Loading

## Libraries depending on

- cffi

## Loads cl-libnuma

```lisp
(load "cl-libnuma.asd")
(asdf:load-system :cl-libnuma)
```

To run tests, use `asdf:test-system`.

```lisp
(asdf:test-system :cl-libnuma)
```

## Loads cl-libnuma.error-handling

*cl-libnuma.error-handling* must be loaded before *cl-libnuma*.

```lisp
(load "cl-libnuma.asd")
(asdf:load-system :cl-libnuma.error-handling)
```

(*cl-libnuma* is automatically loaded after that.)


To run test, try below.

```lisp
(asdf:test-system :cl-libnuma)
(asdf:test-system :cl-libnuma.error-handling)
```


# Examples

see example/ directory.


# API

(stub)


