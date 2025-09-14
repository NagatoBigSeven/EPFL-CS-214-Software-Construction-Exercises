package recursion

def treeSize(t: IntTree): Int =
  if t.isEmpty then 0
  else 1 + treeSize(t.left) + treeSize(t.right)

def treeDepth(t: IntTree): Int =
  if t.isEmpty then 0
  else if t.left.isEmpty && t.right.isEmpty then 1
  else scala.math.max(treeDepth(t.left), treeDepth(t.right)) + 1

def treeSum(t: IntTree): Int =
  if t.isEmpty then 0
  else t.value + treeSum(t.left) + treeSum(t.right)

def treeAllEven(t: IntTree): Boolean =
  if t.isEmpty then true
  else if t.value % 2 != 0 then false
  else treeAllEven(t.left) && treeAllEven(t.right)

def treeIncrement(t: IntTree): IntTree =
  if t.isEmpty then IntEmptyTree()
  else IntBranch(t.value + 1, treeIncrement(t.left), treeIncrement(t.right))

def treeShow(t: IntTree): String =
  if t.isEmpty then ""
  else t.value.toString + "\n" + treeShow(t.left) + treeShow(t.right)

def treeShowPostOrder(t: IntTree): String =
  if t.isEmpty then ""
  else treeShowPostOrder(t.left) + treeShowPostOrder(t.right) + t.value.toString + "\n"

def treeShowInOrder(t: IntTree): String =
  if t.isEmpty then ""
  else treeShowInOrder(t.left) + t.value.toString + "\n" + treeShowInOrder(t.right)