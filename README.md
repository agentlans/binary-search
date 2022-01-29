# binary-search

This library does binary search to find the upper and lower bounds of x corresponding to a given y.

The (x, y) pairs can be specified as a monotone increasing function or as
the (index, value) pairs of a sorted array.

- The `infimum`* functions find the greatest lower bound of x in the domain
- The `supremum`* functions find the least upper bound of x in the domain

For example:
```lisp
;; Find 2 in the sorted list (sorted by <=)
;; 2 is in the sorted list from index 3 to index 5, inclusive.
(infimum-array '(0 1 1.5 2 2 2 3 5) #'<= 2) ; => 5, 2
(supremum-array '(0 1 1.5 2 2 2 3 5) #'<= 2) ; => 3, 2
```

This may sound trivial but there are many edge cases,
hence the need for a library.

# Install and use

1. Copy this respository to `~/common-lisp` or another place that ASDF can find.
2. In a Lisp session, run `(asdf:load-system :binary-search)`
3. Call the functions as `(binary-search:supremum ...)`
4. See the `example.lisp` file for a demo on using this library in another package.

# Author and license

Copyright :copyright: 2021-2022 Alan Tseng

MIT License
