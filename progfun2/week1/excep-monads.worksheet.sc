// Exceptions and Try

class BadInput(msg: String) extends Exception(msg)
// throw BadInput("missing data")

// A thrown exception terminates computation, if it is not handled with a try/catch.


// def validatedInput(): String =
//     try getInput()
//     catch
//         case BadInput(msg) => println(msg); validatedInput()
//         case ex: Exception => println("fatal error; aborting"); throw ex



// Try

// Try resembles Option:
abstract class Try[+T]
case class Success[+T](x: T)        extends Try[T]
case class Failure(ex: Exception)   extends Try[Nothing]

// useful to pass between threads unlike try/catch

// Try(expr) -> Success(someVal) or Failure(someExcep)

// object Try:
//     def apply[T](expr: => T): Try[T] =
//         try Success(expr)
//         catch case NonFatal(ex) => Failure(ex)

// Here, NonFatal matches all exceptions that allow to continue the program.



// Extensions for 'for-exprs':

// extension [T](xt: Try[T])
//     def flatMap[U](f: T => Try[U]): Try[U] = xt match
//         case Success(x) => try f(x) catch case NonFatal(ex) => Failure(ex)
//         case fail: Failure => fail

//     def map[U](f: T => U): Try[U] = xt match
//         case Success(x) => Try(f(x))
//         case fail: Failure => fail


// As we know map can be expressed with flatMap and Try as 'unit'.
// But Try is not true monad because it fails left unit law, but it is not needed for 'for'.
// Try(expr).flatMap(f) != f(expr)
