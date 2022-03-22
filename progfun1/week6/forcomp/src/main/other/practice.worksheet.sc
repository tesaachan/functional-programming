import scala.quoted.ToExpr.NilToExpr
type Word = String

type Sentence = List[Word]

type Occurrences = List[(Char, Int)]


def wordOccurrences(w: Word): Occurrences = 
    (for (i, j) <- w.groupBy(x => x.toLower).toList yield (i, j.length)).sortBy(x => x._1)

def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)

val dictionary: List[Word] = List("eat", "ate", "tea", "aba", "caba", "abac", "room")

lazy val dictionaryByOccurrences = 
    dictionary.groupBy(x => wordOccurrences(x)).withDefaultValue(List())

def wordAnagrams(word: Word): List[Word] = 
    dictionaryByOccurrences(wordOccurrences(word))

def combinations(occurrences: Occurrences): List[Occurrences] = 
    def combine(c: (Char, Int)) =
        (for i <- 1 to c._2 yield (c._1, i)).toList

    occurrences match
        case List() => List(Nil)
        case head :: tail => 
            val tails = combinations(tail)
            (for
                i <- combine(head)
            yield tails ++ tails.map(x => i :: x)).flatten


def subtract(x: Occurrences, y: Occurrences): Occurrences = 
    val yToMap = y.toMap
    def find(key: Char) = yToMap.get(key) match
        case Some(x) => x
        case None => 0
    for (i, j) <- x if j - find(i) != 0 
    yield (i, j - find(i))

def sentenceAnagrams(sentence: Sentence): List[Sentence] = 
    def loop(rest: Occurrences): List[Sentence] =
        if rest.isEmpty then List(Nil)
        else
            for
                i <- combinations(rest)
                head <- dictionaryByOccurrences(i)
                tail <- loop(subtract(rest, i))
            yield head :: tail
    loop(sentenceOccurrences(sentence))

                    
wordOccurrences("AbaCaBa")
val a = sentenceOccurrences(List("I", "want", "to", "see", "you"))

dictionaryByOccurrences
wordAnagrams("eat")

combinations(List(('a', 2), ('b', 2)))
val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
subtract(List(('e', 1), ('a', 2), ('b', 2)), List((('k', 1))))

val alist = List("Linux", "rulez")
val alistOccur = sentenceOccurrences(alist)
val combsOfAlist = combinations(alistOccur)
for
    i <- combsOfAlist
    head <- dictionaryByOccurrences(i)
yield head
// sentenceAnagrams()
