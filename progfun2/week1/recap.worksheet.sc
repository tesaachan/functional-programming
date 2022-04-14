// Case classes
// preferred way to define complex data

// example for json class:
abstract class JSON
object JSON:
    case class Seq (elems: List[JSON])              extends JSON
    case class Obj (bindings: Map[String, JSON])    extends JSON
    case class Num (num: Double)                    extends JSON
    case class Str (str: String)                    extends JSON
    case class Bool (b: Boolean)                    extends JSON
    case object Null                                extends JSON

// Case class hierarchies can be represented (more concisely) as enums:
enum EnumJSON:
    case Seq (elems: List[EnumJSON])
    case Obj (bindings: Map[String, EnumJSON])
    case Num (num: Double)
    case Str (str: String)
    case Bool (b: Boolean)
    case Null

// JSON object example:
val jsData = JSON.Obj(Map(
    "firstName" -> JSON.Str("John"),
    "lastName" -> JSON.Str("Smith"),
    "address" -> JSON.Obj(Map(
        "streetAddress" -> JSON.Str("21 2nd Street"),
        "state" -> JSON.Str("NY"),
        "postalCode" -> JSON.Num(10021)
    )),
    "phoneNumbers" -> JSON.Seq(List(
        JSON.Obj(Map(
            "type" -> JSON.Str("home"), "number" -> JSON.Str("212 555-1234")
        )),
        JSON.Obj(Map(
            "type" -> JSON.Str("fax"), "number" -> JSON.Str("646 555-4567")
))  ))  ))


// Pattern Matching

def show(json: JSON): String = json match
    case JSON.Seq(elems) =>
        elems.map(show).mkString("[", ",", "]")
    case JSON.Obj(bindings) =>
        val assocs = bindings.map(
            (key, value) => s"${inQuotes(key)}: ${show(value)}")
        assocs.mkString("{", "\n", "}")
    case JSON.Num(num)  => num.toString
    case JSON.Str(str)  => inQuotes(str)
    case JSON.Bool(b)   => b.toString
    case JSON.Null      => "null"

def inQuotes(str: String): String = "\"" + str + "\""

show(jsData)

//list functions implementation:
extension [T](xs: List[T])
    def map[U](f: T => U): List[U] = xs match
        case x :: xs1 => f(x) :: xs1.map(f)
        case Nil => Nil
    
    def flatMap[U](f: T => List[U]): List[U] = xs match
        case x :: xs1 => f(x) ++ xs1.flatMap(f)
        case Nil => Nil

    def filter(p: T => Boolean): List[T] = xs match
        case x :: xs1 =>
            if p(x) then x :: xs1.filter(p) else xs1.filter(p)
        case Nil => Nil
    
// in practice, the implementaiton and type of these methods are different
// in order to:
// > make them apply to all collections
// > make them tail-recursive on lists


// For-Expressions:
val n = 7
def isPrime(num: Int) = num == 7

for
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
yield (i, j)



// Something new:

// For-Expressions and Pattern Matching

def bindings(x: JSON): List[(String, JSON)] = x match
    case JSON.Obj(bindings) => bindings.toList
    case _ => Nil

for
    case ("phoneNumbers", JSON.Seq(numberInfos)) <- bindings(jsData)
    numberInfo <- numberInfos
    case ("number", JSON.Str(number)) <- bindings(numberInfo)
    if number.startsWith("212")
yield
    number
