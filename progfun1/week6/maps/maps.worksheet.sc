// Map

// Class Map[Key, Value] extends the type Iterable[(Key, Value)].

val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

val countryOfCapital = capitalOfCountry.map((x, y) => (y, x))

// In fact, the syntax 'key -> value' is another way to write '(key, value)'.

// Class Map[Key, Value] also extends the func type 'Key => Value',
// it means maps can be used everywhere funcs can.

capitalOfCountry("US")

// Applying a map to a non existing key gives 'NoSuchElementException' error.
// To query a map you can use the 'get operation' returning an 'Option' value:

capitalOfCountry.get("US")
capitalOfCountry.get("Andorra")


// The Option type

// Is defined as:

trait OPTION[+A]
case class SOME[+A](value: A) extends OPTION[A]
object NONE extends OPTION[Nothing]

// map.get(key) returns
// None     if does not contain
// Some(x)  if found value x associated with the key

// Since options are defined as case classes, they can be decomposed using
// pattern matching:

def showCapital(country: String) = capitalOfCountry.get(country) match
    case Some(capital) => capital
    case None => "missing data"

showCapital("US")
showCapital("Andorra")


// Updating Maps

// m + (k -> v)     The map that takes key 'k' to value 'v' and is otherwise equal to 'm'

// m ++ kvs         The map 'm' updated via '+' with all key/value pairs in 'kvs'

val m1 = Map("red" -> 1, "blue" -> 2)
val m2 = m1 + ("blue" -> 3)
m1


// Sorted and GroupBy

val fruit = List("apple", "pear", "orange", "pineapple")
fruit.sortWith(_.length < _.length)
fruit.sorted

// groupBy is available on Scala collections. 
// It partitions a collection into a map of collections according to a discriminator func f.

fruit.groupBy(_.head)


// an operation withDefaultValue that turns a map into a total function,
// the not existing key will be defaule value.

val cap1 = capitalOfCountry.withDefaultValue("<unknown>")
cap1("Andorra")
