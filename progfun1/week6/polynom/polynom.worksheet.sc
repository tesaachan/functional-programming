// Varibale Length Argument Lists

class Polynom(nonZeroTerms: Map[Int, Double]):
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = nonZeroTerms.withDefaultValue(0.0)

    // def + (other: Polynom): Polynom =
    //     Polynom(terms ++ other.terms.map((exp, coeff) => (exp, terms(exp) + coeff)))

    def + (other: Polynom): Polynom =
        Polynom(other.terms.foldLeft(this.terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)) =
        val (exp, coeff) = term
        terms + (exp -> (terms(exp) + coeff))

    override def toString = 
        val termStrings =
            for (exp, coeff) <- terms.toList.sorted.reverse
            yield
                val exponent = if exp == 0 then "" else s"x^$exp"
                s"$coeff$exponent"
        if terms.isEmpty then "0"
        else concat(termStrings, "")

    def concat(rest: List[String], acc: String): String =
            if rest.isEmpty then acc
            else if rest.head.take(3) == "0.0" then concat(rest.tail, acc)
            else if rest.head(0) == '-' then concat(rest.tail, List(acc, rest.head.drop(1)).mkString(" - "))
            else 
                if acc != "" then concat(rest.tail, List(acc, rest.head).mkString(" + "))
                else concat(rest.tail, rest.head)
end Polynom


val x = Polynom(0 -> 2, 1 -> -3, 2 -> 1)
val z = Polynom(0 -> -4, 1 -> 6)
x + x + z
