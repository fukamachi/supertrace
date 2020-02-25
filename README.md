# supertrace

[![Build Status](https://github.com/fukamachi/supertrace/workflows/CI/badge.svg)](https://github.com/fukamachi/supertrace/actions?query=workflow%3ACI)

Superior Common Lisp `trace` functionality for debugging/profiling real world applications.

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Usage

```
;; List a function names to trace
(supertrace dbi:execute dbi:prepare)

;; Trace all exported functions from 'dbi' package and 'dex:request'
(supertrace (package dbi) dex:request)

;; Custom before/after functions
(supertrace (package dbi) dex:request
            :before #'before-logger      ;; <- A function takes 2 arguments -- a function name and arguments
            :after #'after-logger)       ;; <- A function takes 4 arguments -- a function name, arguments, returned value and elapsed time in nanoseconds.
```

Output is like as follows:

```
running <DBI.DRIVER> (prepare #<dbd.postgres:dbd-postgres-connection {1001937FF3}> "SELECT COUNT(*) AS \"count\" FROM \"entry\" WHERE \"user_id\" = ?")
0.210ms <DBI.DRIVER> (prepare #<dbd.postgres:dbd-postgres-connection {1001937FF3}> "SELECT COUNT(*) AS \"count\" FROM \"entry\" WHERE \"user_id\" = ?") -> #<dbd.postgres:dbd-postgres-query {100459D6A3}>
running <DBI.DRIVER> (execute #<dbd.postgres:dbd-postgres-query {100459D6A3}> "eb073a91-d098-4c38-805b-cede8e39d278")
0.565ms <DBI.DRIVER> (execute #<dbd.postgres:dbd-postgres-query {100459D6A3}> "eb073a91-d098-4c38-805b-cede8e39d278") -> #<dbd.postgres:dbd-postgres-query {100459D6A3}>
```

### Options

- `:before`: A function to run _before_ the function call
- `:after`: A function to run _after_ the function call
- `:threshold`: Run `:after` function only when the elapsed time exceeded this value. (in nanoseconds)

## Supported implementations

- [x] SBCL
- [ ] Clozure CL
- [ ] Others

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2020 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
