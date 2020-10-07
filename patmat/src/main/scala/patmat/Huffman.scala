package patmat

abstract class CodeTree

case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

case class Leaf(char: Char, weight: Int) extends CodeTree

trait Huffman extends HuffmanInterface {

  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, weight) => weight
    case Leaf(_, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, chars, _) => chars
    case Leaf(chars, _) => List(chars)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def f(chars: List[Char], x: Char): Int = {
      if (chars.isEmpty) 0
      else if (chars.head == x) 1 + f(chars.tail, x)
      else f(chars.tail, x)
    }

    @scala.annotation.tailrec
    def has(list: List[(Char, Int)], x: Char): Boolean = {
      if (list.isEmpty) false
      else if (list.head._1 == x) true
      else has(list.tail, x)
    }

    @scala.annotation.tailrec
    def timesAcc(acc: List[(Char, Int)], x: List[Char]): List[(Char, Int)] = {
      if (x.nonEmpty) {
        if (!has(acc, x.head)) timesAcc((x.head, f(x, x.head)) :: acc, x.tail)
        else timesAcc(acc, x.tail)
      } else acc
    }

    timesAcc(List(), chars)
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def insert(x: (Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
      case List() => List(x)
      case y :: ys => if (x._2 < y._2) x :: xs else y :: insert(x, ys)
    }

    def iSort(xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
      case List() => List()
      case y :: ys => insert(y, iSort(ys))
    }

    val x = iSort(freqs)

    def toLeafs(x: List[(Char, Int)]): List[Leaf] = if (x.isEmpty) List() else Leaf(x.head._1, x.head._2) :: toLeafs(x.tail)

    toLeafs(x)
  }

  def singleton(trees: List[CodeTree]): Boolean = trees.size == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case left :: right :: cs => (makeCodeTree(left, right) :: cs).sortWith((t1, t2) => weight(t1) < weight(t2))
    case _ => trees
  }

  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (!done(trees)) until(done, merge)(merge(trees))
    else trees
  }

  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  type Bit = Int


  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    @scala.annotation.tailrec
    def decodeSymbol(tree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = tree match {
      case Fork(left, right, _, _) =>
        if (bits.head == 0) decodeSymbol(left, bits.tail)
        else decodeSymbol(right, bits.tail)
      case Leaf(char, _) => (char, bits)
    }

    @scala.annotation.tailrec
    def decodeBits(tree: CodeTree, bits: List[Bit], ans: List[Char]): List[Char] = {
      if (bits.isEmpty) ans
      else {
        val decodedChar = decodeSymbol(tree, bits)
        decodeBits(tree, decodedChar._2, decodedChar._1 :: ans)
      }
    }

    decodeBits(tree, bits, List()).reverse
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeSymbol(tree: CodeTree, symbol: Char): List[Bit] = tree match {
      case Fork(left, right, _, _) =>
        if (chars(left).contains(symbol)) 0 :: encodeSymbol(left, symbol)
        else 1 :: encodeSymbol(right, symbol)
      case Leaf(_, _) => List()
    }

    def encodeAll(text: List[Char]): List[Bit] =
      if (text.isEmpty) List()
      else encodeSymbol(tree, text.head).concat(encodeAll(text.tail))

    encodeAll(text)
  }

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] =
    if (table.head._1 == char) table.head._2
    else if (table.isEmpty) List()
    else codeBits(table.tail)(char)

  def convert(tree: CodeTree): CodeTable = {
    val x = chars(tree)

    def convertAcc(chars: List[Char]): CodeTable = {
      if (chars.isEmpty) List()
      else (chars.head, encode(tree)(List(chars.head))) :: convertAcc(chars.tail)
    }

    convertAcc(x)
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a.concat(b)

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table = convert(tree)

    def encodeText(table: CodeTable, text: List[Char]): List[Bit] = {
      if (text.isEmpty) List()
      else codeBits(table)(text.head).concat(encodeText(table, text.tail))
    }

    encodeText(table, text)
  }
}

object Huffman extends Huffman
