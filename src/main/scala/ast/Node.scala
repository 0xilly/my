package io.ansan.my
package ast

import java.util.{List => JList}

import io.ansan.my.token.Token

trait Node {
  def accept[T](visitor:IVisitor[T]):T
}

case class PkgNode(name:Token, body:JList[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class UseNode(name:Token, path: UseNodePath) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class UseNodePath(token:JList[Token]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class ExprNode(expr:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class StmtNode(stmt:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class EnumNode(name:Token, body:JList[Node], typ:Option[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class EnumMemberNode(name:Token, value:Option[Token]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class DataNode(name:Token, body:JList[Node], parent:Option[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class DataMemberNode(name:Token, typ:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class UnionNode(name:Token, body:JList[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class UnionMemberNode(typ:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class BlockNode(body:JList[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class FnNode(name:Token, args:FnArgListNode, ret:Node, body:BlockNode) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class FnArgListNode(args:JList[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class FnArgNode(name:Token, typ:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class InterfaceNode(name:Token, body:Node, parent:Option[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class InterfaceFnListNode(members:JList[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class InterfaceFnNode(name:Token, args:Node, ret:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class DeferBlockNode(ident:Token, body:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class DeferNode(ident:Token, body:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class FieldNode(values:Node, typ:Node, expr:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class FieldInferNode(values:Node, expr:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class IfNode(ident:Token, cond:Node, thenBody:Node, elseBody:Option[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class ForNode(ident:Token, cond:Node, body:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class ForInfinitNode(ident:Token, body:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class ForRangeNode(ident:Token, range:Node, body:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class RangeNode(start:Node, end:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

//Todo(anita): I need to still implement this
//case class ForColletionNode(ident:Token, expr:Node, body:Node) extends Node {
//  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
//}

case class CallNode(name:Token, args:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class LiteralNode(value:Token) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class UnaryNode(op:Token, expr:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class BinaryNode(left:Node, op:Token, right:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class ArrayInizializerNode(elements:JList[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class ArraySliceNode(expr:Node, start:Node, end:Node) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class TypeNode(typ:Token) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class MultiTypeNode(typ:JList[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}

case class MultiValueNode(values:JList[Node]) extends Node {
  override def accept[T](visitor: IVisitor[T]): T = visitor.visit(this)
}
