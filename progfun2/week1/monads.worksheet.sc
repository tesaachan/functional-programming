// Monads

// A monad M is a param type M[T] with 2 operations: flatMap and unit,
// satisfying some laws.

// extension [T,U](m: M[T])
//     def flatMap(f: T => M[U]): M[U]

// def unit[T](x: T): M[T]

// For example, List is a monad with unit(x) = List(x),
// Option is a monad with unit(x) = Some(x), etc.


// 'map' can be defined for every monad with flatMap and unit:
// m.map(f) == m.flatMap(x => unit(f(x)))
//          == m.flatMap(f andThen unit)


// To qualify as a monad, a type has to satisfy three laws:

// Associativity:
// m.flatMap(f).flatMap(g) == m.flatMap(f(_).flatMap(g))

// Left unit
// unit(x).flatMap(f) == f(x)

// Right unit
// m.flatMap(unit) == m


// Option as an example of a monad

extension  [T](xo: Option[T])
    def flatMap[U](f: T => Option[U]): Option[U] = xo match
        case Some(x) => f(x)
        case None => None

// Left unit:
// Some(x).flatMap(f)

// Right unit:
// opt.flatMap(Some)

// Assoc:
// opt.flatMap(f).flatMap(g)
// opt.flatMap(x => f(x).flatMap(g)) ==
// opt.flatMap(f(_).flatMap(g))
