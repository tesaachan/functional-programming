// Exercise Session 4
// This week, we will work on the idea of variance, and on pattern matching.



// Question 1

// Recall that:
// > Lists are covariant in their only type parameter.
// > Functions are contravariant in the argument, and covariant in the result.

// Consider the following hierarchies:

abstract class Fruit
class Banana extends Fruit
class Apple extends Fruit
abstract class Liquid
class Juice extends Liquid

// Consider also the following typing relationships for A, B, C, D: A <: B and C <: D.
// Fill in the subtyping relation between the types below. Bear in mind that it might be that neither type is a subtype of the other.


// Left hand side
// ?:
// Right hand side

// List[Banana]
// <:
// List[Fruit]

// List[A]
// <:
// List[B]

// Banana => Juice
// >:
// Fruit => Juice

// Banana => Juice
// <:
// Banana => Liquid

// A => C
// None
// B => D

// List[Banana => Liquid]
// >: 
// List[Fruit => Juice]

// List[A => D]
// >:
// List[B => C]

// (Fruit => Juice) => Liquid
// >:
// (Banana => Liquid) => Liquid

// (B => C) => D
// >:
// (A => D) => D

// Fruit => (Juice => Liquid)
// None
// Banana => (Liquid => Liquid)

// B => (C => D)
// None
// A => (D => D)



// Question 2
// The following data type represents simple arithmetic expressions:

enum Expr:
  case Number(x: Int)
  case Var(name: String)
  case Sum(e1: Expr, e2: Expr)
  case Prod(e1: Expr, e2: Expr)


// Define a function deriv(expr: Expr, v: String): Expr returning the expression that is the partial derivative of expr with respect to the variable v.

def deriv(expr: Expr, v: String): Expr = expr match
    case Expr.Number(_) => Expr.Number(0)
    case Expr.Var(name) => if name != v then Expr.Number(0) else Expr.Number(1)
    case Expr.Sum(e1, e2) => Expr.Sum(deriv(e1, v), deriv(e2, v))
    case Expr.Prod(e1, e2) => 
        Expr.Sum(
            Expr.Prod(deriv(e1, v), e2), 
            Expr.Prod(e1, deriv(e2, v))) 

// Here's an example run of the function:

val a = deriv(Expr.Sum(Expr.Prod(Expr.Var("x"), Expr.Var("x")), Expr.Var("y")), "x")
// Sum(Sum(Prod(Var("x"), Number(1)), Prod(Number(1), Var("x"))), Number(0))



// Question 3
// Write an expression simplifier that applies some arithmetic simplifications to an expression. For example, it would turn the above monstrous result into the following expression:

def simple(expr: Expr): Expr = expr match
    case Expr.Number(_) => expr
    case Expr.Var(_) => expr
    case Expr.Sum(e1, e2) =>
        val (se1, se2) = (simple(e1), simple(e2))
        (se1, se2) match
            case (Expr.Number(x), Expr.Number(y)) => Expr.Number(x + y)
            case (Expr.Number(0), y) => y
            case (x, Expr.Number(0)) => x
            case (x, y) => if x == y then Expr.Prod(x, Expr.Number(2)) else Expr.Sum(x, y)
    case Expr.Prod(e1, e2) =>
        val (se1, se2) = (simple(e1), simple(e2))
        (se1, se2) match
            case (Expr.Number(x), Expr.Number(y)) => Expr.Number(x * y)
            case (Expr.Number(0), y) => Expr.Number(0)
            case (x, Expr.Number(0)) => Expr.Number(0)
            case (Expr.Number(1), y) => y
            case (x, Expr.Number(1)) => x
            case (x, y) => Expr.Prod(x, y)


val b = simple(a)
// Prod(Var("x"), Number(2))
