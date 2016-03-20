Early development stage, not for production yet.

To build:
1. `$ cabal configure --extra-lib-dirs <full path to libcouchbase lib dir>`
1. `$ cabal build`

Or build with stack:
`$ stack build --extra-lib-dirs <full path to libcouchbase lib dir>`

You can also build and run tests:
`$ stack build --extra-lib-dirs <full path to libcouchbase lib dir> --test`
