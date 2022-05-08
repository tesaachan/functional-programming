case class Person(name: String)
case class Paper(title: String, authors: List[Person], body: String)

object ConfManagement:
    opaque type Viewers = Set[Person]
    def viewers(using vs: Viewers) = vs
    type Viewed[T] = Viewers ?=> T

    class Conference(ratings: (Paper, Int)*):
        private val realScore = ratings.toMap

        def papers: List[Paper] = ratings.map(_._1).toList
        
        def score(paper: Paper): Viewed[Int] =
            if paper.authors.exists(viewers.contains) then -100
            else realScore(paper)
        
        def rankings: Viewed[List[Paper]] =
            papers.sortBy(score(_)).reverse

        def ask[T](p: Person, query: Viewed[T]) =
            query(using Set(p))
        
        def delegateTo[T](p: Person, query: Viewed[T]): Viewed[T] =
            query(using viewers + p)

    end Conference

    val Smith  = Person("Smith")
    val Peters = Person("Peters")
    val Abel   = Person("Abel")
    val Black  = Person("Black")
    val Ed     = Person("Ed")

    val conf = Conference(
        Paper("How to grow beans", List(Smith, Peters), "...") -> 92, Paper("Organic gardening", List(Abel, Peters), "...") -> 83, Paper("Composting done right", List(Black, Smith), "...") -> 99, Paper("The secret life of snails", authors = List(Ed), "...") -> 77
    )

    given review: Viewers = Set(Smith, Peters, Ed)

    def highlyRankedProlificAuthors(asking: Person): Set[Person] =
        def query: Viewed[Set[Person]] =
            val highlyRanked =
                conf.rankings.takeWhile(conf.score(_) > 80).toSet
            for
                p1 <- highlyRanked
                p2 <- highlyRanked
                author <- p1.authors
                if p1 != p2 && p2.authors.contains(author)
            yield author
        conf.ask(asking, query)

    def testAs(person: Person) =
        highlyRankedProlificAuthors(person)
            .map(_.name)
            .mkString(", ")

end ConfManagement

import ConfManagement.*

testAs(Black)
testAs(Ed)


// In 'context passing' we still need explicit "using" parameter clauses.

//  def rankings = (viewers: Viewers) ?=>
//      papers.sortBy(scor(_, viewers)).reverse

// ?=> is used for implicit func types. The ? signifies that we want the parameter
// 'viewers' be implicit so that its arguments can be inferred.

// For a normal anonym func its type would be:
// Viewers => List[Paper]

// For an anonym func with a using clause it is:
// Viewers ?=> List[Paper]


// There are 2 typing rules involving such types:
// 
// 1. Implicit functions get their arguments inferred just like methods with
//    using clauses. 
// 
//      def f: A ?=> B
//      given a: A
//      f
// 
//    In this statement the expression f expands to f(using a)
// 
// 2. Implicit functions get created on demand.
//    If the expected type of an expression b is A ?=> B, then b expands to
//    the anonym func (_: A) ?=> B

// Examples:
// after introducing a type alias:
//  type Viewed[T] = Viewers ?=> T
//  (using Viewers): SomeType replaces with : Viewed[SomeType]
//  query: Viewers => Sometype replaces with query: Viewed[SomeType]


// * Implicit parameters in 'using' trade types for terms:
// Required type of the parameter is passed, 
// the compiler infers an expression (a term) for it.

// * Implicit function types trade types for parameters:
// Required return type of the method is passed, 
// the compiler infers one or more method parameters that match the type.


// So implicit func types may be considered as second degree abstraction.
// An abstraction over the 'using' cluases, which meanwhile abstract over the context.
