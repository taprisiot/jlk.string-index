# jlk.string-index

A simple string indexing library for Common Lisp.

Produces a tree of alists, indexed by character.

## Usage

Provide a list of (strings . value) pairs to the function `make-index`
to create an index.

Use the function `human-search` to find the matching value for the
provided term. Partial matches are returned as the second
value. Internally this uses `search-index` and `flatten-index`.

## Licence

BSD 3-Clause, see LICENCE file in the repository
