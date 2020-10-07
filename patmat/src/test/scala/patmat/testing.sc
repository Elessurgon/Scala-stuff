
import patmat.Huffman._
import patmat._

val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)

def convert(tree: CodeTree): CodeTable = {
  val x = chars(tree)

  def convertAcc(chars: List[Char]): CodeTable = {
    if (chars.isEmpty) List()
    else (chars.head, encode(tree)(List(chars.head))) :: convertAcc(chars.tail)
  }

  convertAcc(x)
}

def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  val table = convert(tree)

  def encodeText(table: CodeTable, text: List[Char]): List[Bit] = {
    if (text.isEmpty) List()
    else codeBits(table)(text.head).concat(encodeText(table, text.tail))
  }

  encodeText(table, text)
}

convert(t1)
encode(t1)("bbbaa".toList)
print(quickEncode(t1)("bbbaa".toList))
