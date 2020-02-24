# supertrace

Superior Common Lisp `trace` functionality for debugging/profiling real world applications.

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Usage

```
(supertrace dbi:execute)
(supertrace (package dbi))
(supertrace (package dbi) dex:request)
```

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
