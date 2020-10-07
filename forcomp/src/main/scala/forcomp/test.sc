import forcomp.Anagrams._

wordOccurrences("Hello")
sentenceOccurrences(List("Hello", "hell"))
wordAnagrams("lives")

def subtract(x: Occurrences, y: Occurrences): Occurrences = x.toMap.foldLeft(Map[Char, Int]())(
  (acc, pair) => {
    acc.updated(pair._1, pair._2 - y.toMap.withDefaultValue(0)(pair._1))
  }).filter(pair => pair._2 > 0).toList.sortBy(pair => pair._1)


val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val y = List(('r', 1))
subtract(x, y)

sentenceAnagrams(List("Mickey", "Mouse"))
