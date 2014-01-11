# Intension

An implementation of [Intensional Equality][0] for Clojure functions.

Clojure provides referential equality of functions such that `(= f g)` is true
only when `f` and `g` refer to the same instance, e.g. `(identical? f g)` is
also true. Intension provides intensional equality of functions such that `(= f
g)` is true so long as the forms used to define them are equivalent. No attempt
is made to normalize parameter names etc. so `(fn [x] x)` is NOT equal to `(fn
[y] y)`.

## Usage

TODO...

## Thanks!

This code was developed with the support of [otherpeoplespixels][1].

## License

Copyright © 2014 Kevin Neaton

Distributed under the [Eclipse Public License][2].

[0]: http://en.wikipedia.org/wiki/First-class_function#Equality_of_functions
[1]: http://www.otherpeoplespixels.com
[2]: http://www.eclipse.org/legal/epl-v10.html
