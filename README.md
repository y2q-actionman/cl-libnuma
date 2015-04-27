# Abstract

*cl-libnuma* is a set of Common Lisp FFI bindings of [libnuma](http://oss.sgi.com/projects/libnuma/), based on CFFI.

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

- `asdf` for loading. For running tests, asdf3 or later is required.
- `cffi` for FFI.

### C Libraries

*libnuma development package* is required.

You can get it with one of following ways:

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

## Loads cl-libnuma.error-handler (optional)

*cl-libnuma.error-handler* is a lispy error handling scheme built
around *cl-libnuma*.  This is experimental and currently
unstable. This does not work on some platforms.

It must be loaded before the *cl-libnuma* system.

```lisp
(load "cl-libnuma.asd")
(load "cl-libnuma-error-handler.asd")
;; Please don't load :cl-libnuma before :cl-libnuma.error-handler. 
;; :cl-libnuma.error-handler automatically loads it.
(asdf:load-system :cl-libnuma.error-handler)
```

To run tests, try below.

```lisp
(load "cl-libnuma-test.asd")
(load "cl-libnuma-error-handler-test.asd")
(asdf:test-system :cl-libnuma.error-handler)
```


# Examples

see example/ directory.


# API

(stub)


