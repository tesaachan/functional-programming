def assertAllPos[S <: AnyVal](r: S): S = ???

/*  type bounds:
 *
 *  S <: T means S is a subtype of T or T   (upper bound)
 *  S >: T means S is a supertype of T or T (lower bound)
 */

// mix to get an interval:  [s >: NonEmpty <: IntSet] 

// arrays in scala are not covariant:
// it is illegal (type error) to assign Array[Subtype] to Array[Supertype]
