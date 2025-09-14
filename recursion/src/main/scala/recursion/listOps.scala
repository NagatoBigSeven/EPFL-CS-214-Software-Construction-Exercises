package recursion

def length(l: IntList): Int =
  if l.isEmpty then 0
  else length(l.tail) + 1

def allPositiveOrZero(l: IntList): Boolean =
  if l.isEmpty then true
  else if l.head < 0 then false
  else allPositiveOrZero(l.tail)

def countPositive(l: IntList): Int =
  if l.isEmpty then 0
  else if l.head <= 0 then countPositive(l.tail)
  else 1 + countPositive(l.tail)

def sum(l: IntList): Int =
  if l.isEmpty then 0
  else l.head + sum(l.tail)

def product(l: IntList): Int =
  if l.isEmpty then 1
  else l.head * product(l.tail)

def anyOdd(l: IntList): Boolean =
  if l.isEmpty then false
  else if l.head % 2 != 0 then true
  else anyOdd(l.tail)

def decrement(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else IntCons(l.head - 1, decrement(l.tail))

def collectEven(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else if l.head % 2 == 0 then IntCons(l.head, collectEven(l.tail))
  else collectEven(l.tail)

def min(l: IntList): Int =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else if l.tail.isEmpty then l.head
  else
    val min_tail = min(l.tail)
    if l.head < min_tail then l.head
    else min_tail

def increment(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else IntCons(l.head + 1, increment(l.tail))

def subtract(l: IntList): Int =
  if l.isEmpty then throw IllegalArgumentException("Empty List!")
  else if l.tail.isEmpty then l.head
  else l.head - subtract(l.tail)

def removeOdd(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else if l.head % 2 != 0 then removeOdd(l.tail)
  else IntCons(l.head, removeOdd(l.tail))

def countEven(l: IntList): Int =
  if l.isEmpty then 0
  else if l.head % 2 == 0 then 1 + countEven(l.tail)
  else countEven(l.tail)

/** `countEven` using `collectEven` and `length` */
def countEven2(l: IntList): Int =
  length(collectEven(l))

def multiplyBy2(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else IntCons(l.head * 2, multiplyBy2(l.tail))

def anyNegative(l: IntList): Boolean =
  if l.isEmpty then false
  else if l.head < 0 then true
  else anyNegative(l.tail)

def allEven(l: IntList): Boolean =
  if l.isEmpty then true
  else if l.head % 2 != 0 then false
  else allEven(l.tail)

def multiplyOdd(l: IntList): Int =
  if l.isEmpty then 1
  else if l.head % 2 != 0 then l.head * multiplyOdd(l.tail)
  else multiplyOdd(l.tail)

def horner(x: Int, l: IntList): Int =
  if l.isEmpty then 0
  else if l.tail.isEmpty then l.head
  else l.head + x * horner(x, l.tail)

def capAtZero(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else if l.head > 0 then IntCons(0, capAtZero(l.tail))
  else IntCons(l.head, capAtZero(l.tail))

def removeZeroes(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else if l.head == 0 then removeZeroes(l.tail)
  else IntCons(l.head, removeZeroes(l.tail))

def reverseAppend(l1: IntList, l2: IntList): IntList =
  if l1.isEmpty then l2
  else reverseAppend(l1.tail, IntCons(l1.head, l2))

def reverse(l: IntList): IntList =
  reverseAppend(l, IntNil())

def takeWhilePositive(l: IntList): IntList =
  if l.isEmpty then IntNil()
  else if l.head <= 0 then IntNil()
  else IntCons(l.head, takeWhilePositive(l.tail))

def append(l1: IntList, l2: IntList): IntList =
  if l1.isEmpty then l2
  else IntCons(l1.head, append(l1.tail, l2))

def collectMultiples(d: Int, l: IntList): IntList =
  if l.isEmpty then IntNil()
  else if l.head % d == 0 then IntCons(l.head, collectMultiples(d, l.tail))
  else collectMultiples(d, l.tail)

def last(l: IntList): Int =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else if l.tail.isEmpty then l.head
  else last(l.tail)

def init(l: IntList): IntList =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else if l.tail.isEmpty then IntNil()
  else IntCons(l.head, init(l.tail))

def contains(l: IntList, n: Int): Boolean =
  if l.isEmpty then false
  else if l.head == n then true
  else contains(l.tail, n)

def isSubset(l: IntList, L: IntList): Boolean =
  if l.isEmpty then true
  else if contains(L, l.head) then isSubset(l.tail, L)
  else false

def intersection(l: IntList, L: IntList): IntList =
  if l.isEmpty then IntNil()
  else if contains(L, l.head) then IntCons(l.head, intersection(l.tail, L))
  else intersection(l.tail, L)

def difference(l: IntList, L: IntList): IntList =
  if l.isEmpty then IntNil()
  else if !contains(L, l.head) then IntCons(l.head, difference(l.tail, L))
  else difference(l.tail, L)

def minMax(l: IntList): (Int, Int) =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else if l.tail.isEmpty then (l.head, l.head)
  else
    val (minimum, maximum) = minMax(l.tail)
    (scala.math.min(l.head, minimum), scala.math.max(l.head, maximum))

val Add = -1
val Multiply = -2

def polishEval(l: IntList): (Int, IntList) =
  if l.isEmpty then throw EmptyListException()
  else if l.head == Add || l.head == Multiply then
    val (left_operand, another_l) = polishEval(l.tail)
    val (right_operand, yet_another_l) = polishEval(another_l)
    if l.head == Add then (left_operand + right_operand, yet_another_l)
    else (left_operand * right_operand, yet_another_l)
  else if l.head >= 0 then (l.head, l.tail)
  else throw InvalidOperatorException(l.head)