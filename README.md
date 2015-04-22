# Abstract

*cl-libnuma* is a set of CFFI bindings for **numa(3)**.

*cl-libnuma* allows you following operations on Lisp:

- Getting current NUMA environments.
- Changing NUMA policies after the Lisp launched.
- Getting memories with custom NUMA policies.
  - Using NUMA-aware allocators in libnuma.
  - Setting NUMA policies to static memories, allocated out of the Lisp heap.


# License

the MIT License. See LICENSE file.


# Loading

## Requirements 

### Lisp Libraries

- `asdf` for loading. For running tests, asdf 3 or later is required.
- `cffi` for FFI.

### C Libraries

*libnuma* development package. You can get it with following ways:

- `apt-get install libnuma-dev`
- `yum install libnuma-devel`
- Get a tarball from http://oss.sgi.com/projects/libnuma/

## Loads cl-libnuma

```lisp
(load "cl-libnuma.asd")
(asdf:load-system :cl-libnuma)
```

To run tests, use `asdf:test-system`.

```lisp
(load "cl-libnuma-test.asd")
(asdf:test-system :cl-libnuma)
```

## Loads cl-libnuma.error-handling

*cl-libnuma.error-handling* is a lispy error handling scheme built
around *cl-libnuma*, but currently unstable.
It must be loaded before *cl-libnuma*.

```lisp
(load "cl-libnuma.asd")
(load "cl-libnuma-error-handler.asd")
(asdf:load-system :cl-libnuma.error-handling)
```

To run tests, try below.

```lisp
(load "cl-libnuma-error-handler-test.asd")
(asdf:test-system :cl-libnuma)
(asdf:test-system :cl-libnuma.error-handling)
```


# Examples

see example/ directory.


# API

(stub)


