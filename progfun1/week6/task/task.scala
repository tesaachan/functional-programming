class Coder(words: List[String]):
    val mnemonics = Map(
        '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
        '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

    // maps a letter to the digit it represents
    private val charCode: Map[Char, Char] =
        for (digit, str) <- mnemonics; ltr <- str yield ltr -> digit

    // maps a word to the digit string it can represent
    private def wordCode(word: String): String = word.toUpperCase.map(charCode)
    
    // maps a digit string to all words in the dictionary that represents it
    private val wordsForNum: Map[String, List[String]] =
        words.groupBy(wordCode).withDefaultValue(Nil)

    // all ways to encode a number as a list of words
    def encode(number: String): Set[List[String]] =
        if number.isEmpty then Set(Nil)
        else
            for
                splitPoint <- (1 to number.length).toSet
                word <- wordsForNum(number.take(splitPoint))
                rest <- encode(number.drop(splitPoint))
            yield word :: rest

end Coder

@main def code(number: String) =
    val coder = Coder(List(
        "Scala", "Python", "Ruby", "C",
        "rocks", "socks", "sucks", "works", "pack"))
    println(coder.encode(number).map(_.mkString(" ")))
