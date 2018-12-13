# Geometric

A DSL to define geometric constructions with straight edge and compass.

## Primitives

* Point with coordinates 2 and 3: `(point 2 3)`
* Line passing through `p`and `q`: `(line p q)`
* Circle withe center `c`and passing by `p`: `(circ c p)`
* Locus of the intersection of `a` and `b` (list pf points); `(cut a b)`
* Get the first/second point of the intersection `I`: `(first I)` `(second I)`

