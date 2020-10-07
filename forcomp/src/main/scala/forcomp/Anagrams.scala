package forcomp

object Anagrams extends AnagramsInterface {
  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = Dictionary.loadDictionary

  def wordOccurrences(w: Word): Occurrences = w.toLowerCase
    .toCharArray
    .groupBy((c: Char) => c)
    .map((f: (Char, Array[Char])) => (f._1, f._2.length))
    .toList
    .sorted

  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.mkString)

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(word => wordOccurrences(word))

  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences.get(wordOccurrences(word)) match {
      case Some(s) => s
      case None => List()
    }
  }

  def combinations(occurrences: Occurrences): List[Occurrences] =
    List() :: (for {
      (char, range) <- occurrences
      num <- 1 to range
      rest <- combinations(occurrences.filter(pair => pair._1 > char))
    } yield List((char, num)) ++ rest)

  def subtract(x: Occurrences, y: Occurrences): Occurrences = x.toMap.foldLeft(Map[Char, Int]())(
    (acc, pair) => {
      acc.updated(pair._1, pair._2 - y.toMap.withDefaultValue(0)(pair._1))
    }).filter(pair => pair._2 > 0).toList.sortBy(pair => pair._1)


  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def anagram(occurrences: Occurrences): List[Sentence] = occurrences match {
      case List() => List(List())
      case _ => {
        for {
          combination <- combinations(occurrences)
          if (dictionaryByOccurrences.contains(combination))
          word <- dictionaryByOccurrences(combination)
          rest <- anagram(subtract(occurrences, combination))
        } yield word :: rest

      }
    }

    anagram(sentenceOccurrences(sentence))
  }

}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}
