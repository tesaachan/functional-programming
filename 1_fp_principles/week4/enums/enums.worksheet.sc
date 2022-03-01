enum Expr:
    case Var(s: String)
    case Number(n: Int)
    case Sum(e1: Expr, e2: Expr)
    case Prod(e1: Expr, e2: Expr)

// import Expr.*

def show(e: Expr): String = e match
    case Expr.Var(s) => s
    case Expr.Number(n) => n.toString
    case Expr.Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Expr.Prod(e1, e2) => s"${showP(e1)} * ${showP(e2)}"

def showP(e: Expr): String = e match
    case e: Expr.Sum => s"(${show(e)})"
    case _ => show(e)

// simple enums

enum Color:
    case Red, Green, Blue

enum DayOfWeek:
    case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

import DayOfWeek.*

def isWeekend(day: DayOfWeek) = day match
    case Saturday | Sunday => true
    case _ => false

// more enums

enum Direction(val dx: Int, val dy: Int):
    case Right extends Direction( 1,  0)  // ordinal 0
    case Up    extends Direction( 0,  1)
    case Left  extends Direction(-1,  0)
    case Down  extends Direction( 0, -1)

    def leftTurn = Direction.values((ordinal + 1) % 4)
    // ordinal of 'case Right' is 0, of 'case Up' is 1 and so on
    // values method gets an Int and returns Direction case indexed with this Int
end Direction

val x = Direction.Right
val y = x.leftTurn
val z = (y.dx, y.dy)

/* 
 * Direction enums is expanded by Scala Compiler to:
 * 
 * abstract class Direction(val dx: Int, val dy: Int):
 *    def leftTurn = Direction.values((ordinal + 1) % 4)
 * object Direction:
 *    val Right = new Direction( 1,  0) {}
 *    val Up    = new Direction( 0,  1) {}
 *    val Left  = new Direction(-1,  0) {}
 *    val Down  = new Direction( 0, -1) {}
 * end Direction
*/

// modelling payment methods
// ADTs used for pure data, operations implemented somewhere else

enum PaymentMethod:
    case CreditCard(kind: Card, holder: String, number: Long, expires: Int)
    case PayPal(email: String)
    case Cash

enum Card:
    case Visa, Mastercard, Amex
