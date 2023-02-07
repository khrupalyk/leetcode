import org.scalatest.funsuite.AnyFunSuite

class v98_Validate_Binary_Search_Tree extends AnyFunSuite {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def isValidBST(rooot: TreeNode): Boolean = {
    def validate(root: TreeNode, lower: Long, upper: Long): Boolean = {
      if(root == null) {
        true
      } else if(root.value <= lower || root.value >= upper) false else  {
        validate(root.left, lower, root.value) && validate(root.right, root.value, upper)
      }
    }
    validate(rooot, Long.MinValue, Long.MaxValue)
  }


  test("Validate Binary Search Tree") {

    assert(isValidBST(
      new TreeNode(3,
        new TreeNode(1,
         new TreeNode(0),
         new TreeNode(2)),
        new TreeNode(5,
          new TreeNode(4),
          new TreeNode(6))
      )
      )
    )
    assert(isValidBST(new TreeNode(0, new TreeNode(-1))))
    assert(isValidBST(new TreeNode(2147483647)))
    assert(isValidBST(new TreeNode(2, new TreeNode(1), new TreeNode(3))))
    assert(!isValidBST(new TreeNode(2, new TreeNode(2), new TreeNode(2))))
    assert(!isValidBST(new TreeNode(5, new TreeNode(1), new TreeNode(4, new TreeNode(3), new TreeNode(6)))))
    assert(!isValidBST(
      new TreeNode(5,
        new TreeNode(4),
        new TreeNode(6,
          new TreeNode(3),
          new TreeNode(7)
        )
      )
    )
    )

  }

}
