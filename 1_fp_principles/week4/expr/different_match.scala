// reference to the arguments of the constructor
def eval(e: Expr): Int = e match
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)

// type test
def isNumber(e: Expr): Boolean = e match
  case n: Number => true
  case _ => false

// variable
def isNumberMessage(e: Expr): String = e match
  case Number(n) => "This is a number"
  case v => "This is not a number"

// literal
def isNumber2(n: Int): Boolean = e match
  case 2 => true
  case _ => false

// wildcards
def isNumberMessage(e: Expr): String = e match
  case Number(n) => "This is a number"
  case _ => "This is not a number"
