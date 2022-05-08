// Type classes let one define concepts that are quite abstract,
// and that can be instantiated with many types.

// The model of a semigroup algebraic structure with an associative operator.
trait SemiGroup[T]:
    extension (x: T) def combine (y: T): T

// Now we can define methods that work for all semigroups.
def reduce[T: SemiGroup](xs: List[T]): T =
    xs.reduceLeft(_.combine(_))


// Hierarchies:
// Algebraic type classes often form natural hierarchies.

// A 'monoid' is defined as a semigroup with a left and right unit elem.
trait Monoid[T] extends SemiGroup[T]:
    def unit: T

// needed object for 'alsoReduce'
object Monoid:
    def apply[T](using m: Monoid[T]) = m

def reduce[T](xs: List[T])(using mon: Monoid[T]): T = xs match
    case Nil => mon.unit
    case _ => xs.reduceLeft(_.combine(_))

// Recap: foldLeft works with empty lists:
def otherReduce[T](xs: List[T])(using m: Monoid[T]): T =
    xs.foldLeft(m.unit)(_.combine(_))


// Context bounds:
// But we can get the same result without explicit named type class.
// With a context bound and a summon:

def moreReduce[T: Monoid](xs: List[T]): T =
    xs.foldLeft(summon[Monoid[T]].unit)(_.combine(_))

// But it could be simpler if we have some preparation in the Monoid trait.
// This defines a global function 'apply' that returns the 'Monoid[T]',
// instance that is currently visible:

def alsoReduce[T: Monoid](xs: List[T]): T =
    xs.foldLeft(Monoid[T].unit)(_.combine(_))


// Multiple typeclass instances:
// It's possible to have several given instances for a typeclass.
// For example, Int could be a Monoid in ways:
//   1) with + as combine and 0 as unit,
//   2) with * as combine and 1 as unit

given sumMonoid: Monoid[Int] with
    extension (x: Int) def combine(y: Int): Int = x + y
    def unit: Int = 0

given prodMonoid: Monoid[Int] with
    extension (x: Int) def combine(y: Int): Int = x * y
    def unit: Int = 1

def sum(xs: List[Int]) = alsoReduce(xs)(using sumMonoid)
def prod(xs: List[Int]) = alsoReduce(xs)(using prodMonoid)

// without explicit usings above we would get 'ambiguity error'.


// Typeclass laws:
// Algebraic type classes are not just defined by their signatures,
// but also by the laws.

// For example, any given instance of Monoid[T]:

// For arbitrary values of T x,y,z and unit Monoid.unit[T]:
// x.combine(y).combine(z) == x.combine(y.combine(z))
// unit.combine(x) == x
// x.combine(unit) == x

// The laws can be verified either by a formal or informal proof,
// or by testing them.
// A good way to test that an instance is lawful is using randomized testing
// with a tool like ScalaCheck.
