package debugging

def isBST(t: IntTree): Boolean =
  t match
    case IntEmptyTree() => true
    case IntBranch(v, l, r) => 
         def maxVal(t: IntTree): Int = 
            t match
              case IntBranch(v, _, IntEmptyTree()) => v
              case IntBranch(v, _, r) => maxVal(r)
         def minVal(t: IntTree): Int = 
            t match
              case IntBranch(v, IntEmptyTree(), _) => v
              case IntBranch(_, l, _) => minVal(l)
         (l == IntEmptyTree() || v > maxVal(l))
      && (r == IntEmptyTree() || v < minVal(r))
      && isBST(l)
      && isBST(r)

def insert(t: IntTree, v1: Int): IntTree =
  t match
    case IntEmptyTree() => IntBranch(v1, IntEmptyTree(), IntEmptyTree())
    case IntBranch(v0, l, r) =>
      if v1 == v0 then t
      else if v1 < v0 then IntBranch(v0, insert(l, v1), r)
      else IntBranch(v0, l, insert(r, v1))
