// Translation of For

// The syntax of for is like funcs: map, flatMap, filter.
// We even can define these funcs in terms of for:

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    for x <- xs yield f(x)

def flatMap[T, U](xs: List[T], f: T => Iterable[U]): List[U] =
    for x <- xs; y <- f(x) yield y

def filter[T](xs: List[T], p: T => Boolean): List[T] =
    for x <- xs if p(x) yield x


// Really Scala compiler process for-exprs in terms of map, flatMap and a lazy filter
// with these rules:

// 1.   for x <- e1 yield e2                            is translated to
//      e1.map(x => e2)
//
// 2.   for x <- e1 if f; s yield e2                    is translated to
//      for x <- e1.withFilter(x => f); s yield e2
//
// 3.   for x <- e1; y <- e2; s yield e2                is translated to
//      e1.flatMap(x => for y <- e2; s yield e3)


// So it is IMPORTANT because 
// it lets you use the 'for' syntax for your own types as well -
// you must only define map, flatMap and withFilter for these types.
// There are many types for which this is useful.
