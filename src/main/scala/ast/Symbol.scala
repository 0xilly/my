package io.ansan.my
package ast

import token.{Token, TokenKind}

enum Scope(val name: String) {
  case Class extends Scope("class")
  case Fn extends Scope("fn")
  case Block extends Scope("block")
  case Global extends Scope("global")
  case Local extends Scope("local")
  case Param extends Scope("param")
  case Mut extends Scope("mut")
  case None extends Scope("none")
}



class Symbol(name: String, scope: Scope, typ: TokenKind, value: Option[Token] = None, parent:Option[Symbol] = None) {
  def this(name: String, scope: Scope, typ: TokenKind, value: Token, parent:Option[Symbol]) = this(name, scope, typ, Some(value), parent)
  def this(name: String, scope: Scope, typ: TokenKind, value: Token) = this(name, scope, typ, Some(value), None)
  def this(name: String, scope: Scope, typ: TokenKind) = this(name, scope, typ, None, None)
  def this(name: String, scope: Scope, typ: TokenKind, parent: Symbol) = this(name, scope, typ, None, Some(parent))
  def this(name: String, scope: Scope, typ: TokenKind, value: Token, parent: Symbol) = this(name, scope, typ, Some(value), Some(parent))

  def is_global: Boolean = scope == Scope.Global
  def is_local: Boolean = scope == Scope.Local
  def is_param: Boolean = scope == Scope.Param
  def is_none: Boolean = scope == Scope.None
  def is_class: Boolean = scope == Scope.Class
  def is_fn: Boolean = scope == Scope.Fn
  def is_block: Boolean = scope == Scope.Block

  def is_mut: Boolean = scope == Scope.Mut
}

class SymbolTable {
  var symbols: Map[String, Symbol] = Map()
  var parent: Option[SymbolTable] = None

  def define(name: String, scope: Scope, typ: TokenKind, value: Option[Token] = None): Symbol = {
    val symbol = new Symbol(name, scope, typ, value)
    symbols += (name -> symbol)
    symbol
  }

  def define(name: String, scope: Scope, typ: TokenKind, value: Token): Symbol = {
    val symbol = new Symbol(name, scope, typ, value)
    symbols += (name -> symbol)
    symbol
  }

  def define(name: String, scope: Scope, typ: TokenKind): Symbol = {
    val symbol = new Symbol(name, scope, typ)
    symbols += (name -> symbol)
    symbol
  }

  def define(name: String, scope: Scope, typ: TokenKind, parent: Symbol): Symbol = {
    val symbol = new Symbol(name, scope, typ, parent)
    symbols += (name -> symbol)
    symbol
  }

  def define(name: String, scope: Scope, typ: TokenKind, value: Token, parent: Symbol): Symbol = {
    val symbol = new Symbol(name, scope, typ, value, parent)
    symbols += (name -> symbol)
    symbol
  }

  def resolve(name: String): Option[Symbol] = {
    symbols.get(name) match {
      case Some(symbol) => Some(symbol)
      case None => parent match {
        case Some(p) => p.resolve(name)
        case None => None
      }
    }
  }
}

