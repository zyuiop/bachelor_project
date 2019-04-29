package ch.epfl.lara.engine.game.actions.control.compiler

import ch.epfl.lara.engine.game.actions.control.compiler.Tokens.Token

/**
  * @author Louis Vialar
  */
object Tree {

  trait Expr

  case class And(left: Expr, right: Expr) extends Expr

  case class Or(left: Expr, right: Expr) extends Expr

  trait Entity

  case object PlayerEntity extends Entity

  case object AnyEntity extends Entity

  case class NamedEntity(name: String) extends Entity

  trait Trigger extends Expr

  case class EntersTrigger(who: Entity) extends Trigger

  case class LeavesTrigger(who: Entity) extends Trigger

  case class TalksTrigger(who: Entity) extends Trigger

  case class InteractsTrigger(who: Entity) extends Trigger

  case class HasTrigger(who: Entity, what: Comparison) extends Trigger

  trait Value extends Expr

  trait Literal extends Value

  case class Variable(name: String) extends Value

  case class IntLiteral(value: Int) extends Literal

  case class StringLiteral(value: String) extends Literal


  trait Comparison extends Expr

  case class Eq(left: Value, right: Value) extends Comparison

  case class Neq(left: Value, right: Value) extends Comparison

  case class Lte(left: Value, right: Value) extends Comparison

  case class Lt(left: Value, right: Value) extends Comparison

  case class Ht(left: Value, right: Value) extends Comparison

  case class Hte(left: Value, right: Value) extends Comparison

  case class Error(err: String) extends Expr


  trait Tree

  case class Leaf(tokens: List[Token]) extends Tree

  case class ParsedLeaf(tokens: Expr) extends Tree

  case class Node(left: Tree, mid: Token, right: Tree) extends Tree

  case class ParsedNode(tokens: Expr) extends Tree

  private def extractTree(tokens: List[Token]): Tree = {
    def produceNext(tokens: List[Token], acc: List[Token] = List()): (Tree, List[Token]) = {
      if (tokens.isEmpty) (Leaf(acc.reverse), Nil)
      else {
        if (tokens.head == Tokens.And || tokens.head == Tokens.Or) {
          val left = Leaf(acc.reverse)
          val mid = tokens.head
          val (right, rest) = produceNext(tokens.tail)

          (Node(left, mid, right), rest)
        } else {
          produceNext(tokens.tail, tokens.head :: acc)
        }
      }
    }

    val (tree, _) = produceNext(tokens)
    tree
  }


  def extractExpr(tokens: List[Token]): Expr = {
    // Produce the tree
    val tree = extractTree(tokens)

    def parseSubTree(tokens: List[Token]): Expr = tokens match {
      // Triggers
      case l :: Tokens.Has :: r =>
        val entity = parseEntity(l)
        val rest = parseComparison(r)
        HasTrigger(entity, rest)

      case l :: Tokens.Enters :: Nil =>
        EntersTrigger(parseEntity(l))

      case l :: Tokens.Leaves :: Nil =>
        LeavesTrigger(parseEntity(l))

      case l :: Tokens.Talks :: Nil =>
        TalksTrigger(parseEntity(l))

      case l :: Tokens.Interacts :: Nil =>
        InteractsTrigger(parseEntity(l))


      // Other conditions
      case _ => parseComparison(tokens)
    }

    def parseComparison(tokens: List[Token]): Comparison = {
      if (tokens.length != 3) throw new IllegalArgumentException("Illegal comparison " + tokens)

      val (l, r) = (parseValue(tokens.head), parseValue(tokens(2)))
      tokens(1) match {
        case Tokens.Eq => Eq(l, r)
        case Tokens.Neq => Neq(l, r)
        case Tokens.Lte => Lte(l, r)
        case Tokens.Lt => Lt(l, r)
        case Tokens.Hte => Hte(l, r)
        case Tokens.Ht => Ht(l, r)
      }
    }

    def parseEntity(token: Token): Entity = token match {
      case Tokens.Player => PlayerEntity
      case Tokens.Symbol(name) => NamedEntity(name)
      case Tokens.AnyOne => AnyEntity
    }

    def parseValue(token: Token): Value = token match {
      case Tokens.IntLiteral(v) => IntLiteral(v)
      case Tokens.StringLiteral(v) => StringLiteral(v)
      case Tokens.Symbol(v) => Variable(v)
    }

    def parseNode(node: Tree): Expr = node match {
      case Node(left, mid, right) =>
        val (pLeft, pRight) = (parseNode(left), parseNode(right))
        if (mid == Tokens.And) And(pLeft, pRight)
        else Or(pLeft, pRight)

      case Leaf(list) =>
        parseSubTree(list)
    }

    parseNode(tree)
  }
}
