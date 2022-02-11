package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.y2021.Day_18.{SFNumber, SFRegular}
import hu.nemzsom.aoc.{Solver, Tester}

class Day_18Spec extends Tester {
  override def solver: Solver = Day_18

  override def input: String =
    """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
      |[[[5,[2,8]],4],[5,[[9,9],0]]]
      |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
      |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
      |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
      |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
      |[[[[5,4],[7,7]],8],[[8,3],8]]
      |[[9,3],[[9,9],[6,[4,9]]]]
      |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
      |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin

  override val expectedResult: String = "4140"
  override def expectedResultPart2: String = "3993"

  "Explode" should "return the exploded number" in {
      assert(SFNumber("[[[[[9,8],1],2],3],4]").explode.get.toString === "[[[[0,9],2],3],4]")
      assert(SFNumber("[7,[6,[5,[4,[3,2]]]]]").explode.get.toString === "[7,[6,[5,[7,0]]]]")
      assert(SFNumber("[[6,[5,[4,[3,2]]]],1]").explode.get.toString === "[[6,[5,[7,0]]],3]")
      assert(SFNumber("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]").explode.get.toString === "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
      assert(SFNumber("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]").explode.get.toString === "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    }

  it should "return None if there is nothing to explode" in {
    assert(SFNumber("[[[[0,9],2],3],4]").explode === None)
    assert(SFNumber("[[3,[2,[8,0]]],[9,[5,[7,0]]]]").explode === None)
  }

  "Split" should "split regular numbers if they are greater than 9" in {
    assert(SFRegular(9).split === None)
    assert(SFRegular(10).split.get.toString === "[5,5]")
    assert(SFRegular(11).split.get.toString === "[5,6]")
    assert(SFRegular(12).split.get.toString === "[6,6]")
  }

  it should "split pair numbers" in {
    assert(SFNumber("[[[[0,7],4],[15,[0,13]]],[1,1]]").toString === "[[[[0,7],4],[15,[0,13]]],[1,1]]")
    assert(SFNumber("[[[[0,7],4],[15,[0,13]]],[1,1]]").split.get.toString === "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
  }

  "Reduce" should "reduce the number based on snailfish maths" in {
    assert(SFNumber("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]").reduce.toString === "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  }

  "Addition" should "concatenate and the reduce numbers" in {
    assert((SFNumber("[[[[4,3],4],4],[7,[[8,4],9]]]") + SFNumber("[1,1]")).reduce.toString === "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  }

}
