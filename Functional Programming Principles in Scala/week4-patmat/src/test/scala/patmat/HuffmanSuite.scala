package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of a singleton or nil::5") {
    val list = List(Leaf('e', 1))

    assert(combine(list) === List(Leaf('e', 1)))
  }

  test("encode") {
    val tree = Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),
      Leaf('b',13822),List('g', 'b'),27110),List('v', 'g', 'b'),52085),List('c', 'v', 'g', 'b'),102088),List('r', 'c', 'v', 'g', 'b'),202588)

    val text = "ccvvggbb".toList
    val encoded = encode(tree)(text)
    assert(encoded === List(1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1))
  }


  test("create code tree") {
    val chars = string2Chars("ab ab ab aa cc")
    val codeTree = createCodeTree(chars)
    assert(codeTree === Fork(Leaf('a',5),Fork(Leaf(' ',4),Fork(Leaf('c',2),Leaf('b',3),List('c', 'b'),5),List(' ', 'c', 'b'),9),List('a',' ', 'c', 'b'),14))
  }

  test("decode secret") {
    val secret = decodedSecret.mkString("")
    assert(secret === "huffmanestcool")
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quick decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
