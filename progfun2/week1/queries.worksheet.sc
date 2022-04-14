// Queries with for
// The for notation is equiv to the common operations of query languages for DBs:

case class Book(title: String, authors: List[String])

val books: List[Book] = List(
    Book(title   = "S a I o C P",
         authors = List("Abelson, Harald", "Sussman, Gerald J")),
    Book(title      = "I t F P",
         authors = List("Bird, Richard", "Wadler, Phi")),
    Book(title   = "E J",
         authors = List("Bloch, Joshua")),
    Book(title   = "J P",
         authors = List("Bloch, Joshua", "Gafterm Neal")),
    Book(title = "P i S",
         authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

// some queries

for
    b <- books
    a <- b.authors
    if a.startsWith("Bird, ")
yield b.title

for b <- books if b.title.indexOf("Program") >= 0
yield b.title


// distinct seq's method -> removes duplicates from seq

List('a', 'b', 'c', 'a', 'c').distinct


// set of authors with 2+ books:

val bookSet = books.toSet
for
    b1 <- bookSet
    b2 <- bookSet
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
yield a1
