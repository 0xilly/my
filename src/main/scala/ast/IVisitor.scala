package io.ansan.my
package ast

trait IVisitor[T] {

  def visit(node:PkgNode): T
  def visit(node:UseNode): T
  def visit(node:UseNodePath): T

  def visit(node:ExprNode): T
  def visit(node:StmtNode): T

  def visit(node:EnumNode): T
  def visit(node:EnumMemberNode): T

  def visit(node:DataNode): T
  def visit(node:DataMemberNode): T

  def visit(node:UnionNode): T
  def visit(node:UnionMemberNode): T

  def visit(node:BlockNode): T

  def visit(node:FnNode): T
  def visit(node:FnArgListNode): T
  def visit(node:FnArgNode): T

  def visit(node:InterfaceNode): T
  def visit(node:InterfaceFnListNode): T
  def visit(node:InterfaceFnNode): T

  def visit(node:DeferNode): T
  def visit(node:DeferBlockNode): T

  //statement
  def visit(node:IfNode): T
  def visit(node:ForNode): T
  def visit(node:FieldNode): T
  def visit(node:FieldInferNode): T
  def visit(node:ForInfinitNode): T
  def visit(node:ForRangeNode): T
  def visit(node:RangeNode): T

  //expr
  def visit(node:CallNode): T

  def visit(node: LiteralNode): T
  def visit(node: UnaryNode): T
  def visit(node: BinaryNode): T
  def visit(node: ArrayInizializerNode): T
  def visit(node: ArraySliceNode): T

  def visit(node: TypeNode): T
  def visit(node: MultiTypeNode): T
  def visit(node: MultiValueNode): T
 
}

