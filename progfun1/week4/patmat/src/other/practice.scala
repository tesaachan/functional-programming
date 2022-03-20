abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

def times(chars: List[Char]): List[(Char, Int)] =
    def count(rest: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] =
      if rest.isEmpty then acc
      else
        val cur = rest.head
        if acc.exists((x: (Char, Int)) => x._1 == cur) then 
          count(rest.tail, replace(cur, acc))
        else count(rest.tail, (cur, 1) :: acc)
        
    def replace(c: Char, rest: List[(Char, Int)]): List[(Char, Int)] =
      if rest.head._1 == c then (c, rest.head._2 + 1) :: rest.tail
        else rest.head :: replace(c, rest.tail)
    
    count(chars, Nil)
end times

val a = times(List('b', 'a', 'a', 'c', 'a', 'b', 'k', 'a'))

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = 
    def make_list(rest: List[(Char, Int)], acc: List[Leaf]): List[Leaf] =
        if rest.isEmpty then acc
        else make_list(rest.tail, Leaf(rest.head._1, rest.head._2) :: acc)
    make_list(freqs, Nil).sortBy(_.weight)

// makeOrderedLeafList(a)

def weight(tree: CodeTree): Int = tree match
    case Fork(l, r, chars, weight) => weight
    case Leaf(char, weight) => weight

def chars(tree: CodeTree): List[Char] = tree match
    case Fork(l, r, chars, weight) => chars
    case Leaf(char, weight) => char :: Nil

def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


def combine(trees: List[CodeTree]): List[CodeTree] = 
  val frst = trees.head
  val scnd = trees.tail.head
  def sorted(node: Fork, rest: List[CodeTree], acc: List[CodeTree]): List[CodeTree] =
    if rest.isEmpty then acc ::: List(node)
    else if weight(rest.head) >= node.weight then acc ::: List(node) ::: rest
    else sorted(node, rest.tail, acc ::: List(rest.head))
  sorted(makeCodeTree(frst, scnd), trees.tail.tail, Nil)

val c = combine(List(Leaf('e', 1), Leaf('t', 2),  Leaf('v', 3), Leaf('x', 4)))

def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
  if done(trees) then trees
  else until(done, merge)(merge(trees))

def singleton(trees: List[CodeTree]): Boolean = 
  if trees.tail.isEmpty then true else false

  // until(singleton, combine)(c)

def createCodeTree(chars: List[Char]): CodeTree = 
  until(singleton, combine)(makeOrderedLeafList(times(chars))).head

// createCodeTree("babcaa".toList)

type Bit = Int

def decode(tree: CodeTree, bits: List[Bit]): List[Char] = 
  def loop(route: CodeTree, rest: List[Bit], acc: List[Char]): List[Char] = 
    if rest.isEmpty then route match
      case Leaf(c, w) => acc ::: List(c)
      case _ => acc
    else route match
      case Fork(l, r, c, w) => 
        if rest.head == 0 then loop(l, rest.tail, acc) else loop(r, rest.tail, acc)
      case Leaf(c, w) => loop(tree, rest, acc ::: List(c))
  loop(tree, bits, Nil)

val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
  

val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
val secret2: List[Bit] = List(0,1,0,0,1)

  // decode(t2, secret2)

def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
  
  def find(route: CodeTree, char: Char): Boolean = route match
    case Fork(l, r, c, w) => c.contains(char)
    case Leaf(c, w) => c == char
  
  def loop(route: CodeTree, rest: List[Char], acc: List[Bit]): List[Bit] = 
    if rest.isEmpty then acc else route match
      case Fork(l, r, c, w) =>
        if find(l, rest.head) then loop(l, rest, acc ::: List(0))
        else loop(r, rest, acc ::: List(1))
      case Leaf(c, w) => loop(tree, rest.tail, acc)
  
  loop(tree, text, Nil)
end encode

type CodeTable = List[(Char, List[Bit])]
type Code = (Char, List[Bit])


def codeBits(table: CodeTable)(char: Char): List[Bit] = 
  table.filter(code => code._1 == char).head._2

def convert(tree: CodeTree): CodeTable = tree match
  case Fork(l, r , c, w) => mergeCodeTables(convert(l), convert(r))
  case Leaf(c, w) => List((c, Nil))

def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = 
  def expand(bit: Bit)(code: (Char, List[Bit])) = (code._1, bit :: code._2)
  def mapped(ct: CodeTable, func: Code => Code): CodeTable =
    if ct.isEmpty then Nil else func(ct.head) :: mapped(ct.tail, func)
  
  mapped(a, expand(0)) ::: mapped(b, expand(1))
end mergeCodeTables


def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = 
  val ct = convert(tree)
  def loop(rest: List[Char], acc: List[Bit]): List[Bit] =
    if rest.isEmpty then acc else loop(rest.tail, acc ::: codeBits(ct)(rest.head))
  loop(text, Nil)


@main def main() =
  // val dec = decode(frenchCode, secret)
  // println(dec)
  // val a = encode(frenchCode)(dec)
  // println(a)
  // val c = decode(frenchCode, a)
  // println(c)
  val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  val f = quickEncode(t2)("bad".toList)
  println(f)

  // val d = encode(t2)(List('b','a','d'))
  // println(d)
