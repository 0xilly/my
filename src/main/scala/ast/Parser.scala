package io.ansan.my
package ast

import token.{Token, TokenKind, Tokenize}

import java.util.{List => JList}
import java.util.ArrayList

class Parser(val tokenize: Tokenize) {
  val tokens = tokenize.tokens
  private var index = 0
  
  def expression():Node = {
    ???
  }

  def statement():Node = {
    ??? //Implement this
  }

  def parse():Node = {
    expression()
  }

  // declarations 
  private def parse_declaration():Node = {
    val ident = consume(TokenKind.IdentLiteral)
    consume(TokenKind.Decelerator)

    current().kind match {
      case TokenKind.Data => parse_struct_declaration(ident)
      case TokenKind.Enum => parse_enum_declaration(ident)
      case TokenKind.Union => parse_union_declaration(ident)
      case TokenKind.Fn => parse_function_declaration(ident)
      case TokenKind.Interface => parse_interface_declaration(ident)
      case TokenKind.IdentLiteral => parse_variable_declaration(ident)
      case _ => throw new RuntimeException(s"Expected declaration, but got ${current().kind}")
    }

  }

  private def parse_function_declaration(ident:Token):Node = {
    ???
  }

  private def parse_variable_declaration(ident:Token):Node = {
    ???
  }

  private def parse_struct_declaration(ident:Token):Node = {
    ???
  }

  private def parse_enum_declaration(ident:Token):Node = {
    val _ident = ident
    var typ:Option[Node] = None

    if (check(TokenKind.LeftArrow)) {
      advance()
      typ = Some(parse_type())
    }

    consume(TokenKind.OpenBrace)
    val members = parse_enum_members()
    consume(TokenKind.CloseBrace)

    EnumNode(_ident, members, typ)
  }

  private def parse_enum_members():JList[Node] = {
    var members:JList[Node] = new ArrayList[Node]()

    while (check(TokenKind.IdentLiteral)) {
      members.add(parse_enum_member_declaration())
      if (check(TokenKind.Eol)) {
        advance()
      }
    }
    members
  }

  private def parse_enum_member_declaration():Node = {
    val ident = consume(TokenKind.IdentLiteral)
    if (check(TokenKind.Assign)) {
      advance()
      val value = current();
      advance()
      EnumMemberNode(ident, Some(value))
    } else {
      EnumMemberNode(ident, None)
    }
  }

  private def parse_data_declaration(ident:Token):Node = {
    val _ident = ident
    var parent:Option[Node] = None

    if (check(TokenKind.LeftArrow)) {
      advance()
      parent = Some(literal())
    }

    consume(TokenKind.OpenBrace)
    val members = parse_data_members()
    consume(TokenKind.CloseBrace)
    DataNode(_ident, members, parent)
  }

  private def parse_data_members():JList[Node] = {
    var members:JList[Node] = new ArrayList[Node]()
    while (check(TokenKind.IdentLiteral)) {
      members.add(parse_data_member_declaration())
      if (check(TokenKind.Eol)) {
        advance()
      }
    }
    members
  }

  private def parse_data_member_declaration():Node = {
    val ident = consume(TokenKind.IdentLiteral)
    consume(TokenKind.Colon)
    val typ = parse_type()
    DataMemberNode(ident, typ)
  }

  private def parse_union_declaration(ident:Token):Node = {
    val _ident = ident
    consume(TokenKind.OpenBrace)
    val members = parse_union_members()
    consume(TokenKind.CloseBrace)
    UnionNode(_ident, members)
  }

  private def parse_union_members():JList[Node] = {
    var members:JList[Node] = new ArrayList[Node]()
    while (check(TokenKind.IdentLiteral)) {
      members.add(parse_union_member_declaration())
      if (check(TokenKind.Eol)) {
        advance()
      }
    }
    members
  }

  private def parse_union_member_declaration():Node = {
    val typ = parse_type()
    UnionMemberNode(typ)
  }

  def parse_block():Node = {
    val body:JList[Node] = new ArrayList[Node]()
    consume(TokenKind.OpenBrace)
    while (!check(TokenKind.CloseBrace)) {
      body.add(parse_declaration())
    }
    consume(TokenKind.CloseBrace)
    BlockNode(body)
  }

  private def parse_fn_args():Node = {
    val args:JList[Node] = new ArrayList[Node]()
    consume(TokenKind.OpenParen)
    while (!check(TokenKind.CloseParen)) {
      args.add(parse_fn_arg())
      if (check(TokenKind.Comma)) {
        advance()
      }
    }
    consume(TokenKind.CloseParen)
    FnArgListNode(args)
  }

  private def parse_fn_arg():Node = {
    val ident = consume(TokenKind.IdentLiteral)
    consume(TokenKind.Colon)
    val typ = parse_type()
    FnArgNode(ident, typ)
  }  

  private def parse_interface_declaration(ident:Token):Node = {
    val _ident = ident
    var parent:Option[Node] = None

    if (check(TokenKind.LeftArrow)) {
      advance()
      parent = Some(literal())
    }

    val body = parse_interface_fn_list()
    InterfaceNode(_ident, body, parent)
  }

  private def parse_interface_fn_list():Node = {
    val members:JList[Node] = new ArrayList[Node]()
    consume(TokenKind.OpenBrace)
    while (!check(TokenKind.CloseBrace)) {
      members.add(parse_interface_fn())
      if (check(TokenKind.Eol)) {
        advance()
      }
    }
    consume(TokenKind.CloseBrace)
    InterfaceFnListNode(members)
  }


  private def parse_interface_fn():Node = {
    val ident = consume(TokenKind.IdentLiteral)
    consume(TokenKind.Decelerator)
    consume(TokenKind.Fn)
    val args = parse_fn_args()
    val ret = parse_type()
    InterfaceFnNode(ident, args, ret)
  }

  private def parse_defer():Node = {
    val ident = consume(TokenKind.Defer)
    val body = expression()
    DeferNode(ident, body)
  }

  private def parse_defer_block():Node = {
    val ident = consume(TokenKind.Defer)
    val body = parse_block()
    DeferBlockNode(ident, body)
  }

  private def parse_if():Node = {
    val ident = consume(TokenKind.If)
    val condition = expression()
    val body = parse_block()
    val else_body = if (check(TokenKind.Else)) {
      advance()
      if (check(TokenKind.If)) {
        Some(parse_if())
      } else {
        Some(parse_block())
      }
    } else {
      None
    }
    
    IfNode(ident, condition, body, else_body)
  }

  private def parse_for():Node = {
    val _for = if (check(TokenKind.OpenBrace)) {
      return parse_for_infinit()
    } else if (current().is_literal() && check(TokenKind.Assign, 1)) {
      return parse_for_base()
    } else  {
      //Note(anita): maybe swap this with the base? - 2/22/2024
      return parse_for_range()
    }
    _for
  }

  private def parse_for_infinit():Node = {
    val ident = consume(TokenKind.For)
    consume(TokenKind.OpenBrace)
    val body = parse_block()
    consume(TokenKind.CloseBrace)
    ForInfinitNode(ident, body)
  }

  private def parse_for_base():Node = {
    val ident = consume(TokenKind.For)
    val condition = expression()
    val body = parse_block()
    ForNode(ident, condition, body)
  }

  private def parse_for_range():Node = {
    val ident = consume(TokenKind.For)
    val range = parse_range()
    val body = parse_block()
    ForRangeNode(ident, range, body)
  }

  private def parse_range():Node = {
    val start = expression()
    consume(TokenKind.Ellipsis)
    val end = expression()
    RangeNode(start, end)
  }

  private def parse_call():Node = {
    val ident = consume(TokenKind.IdentLiteral)
    val args = parse_fn_args()
    CallNode(ident, args)
  }

  private def parse_multi_value():Node = {
    val values:JList[Node] = new ArrayList[Node]()
    while (!check(TokenKind.Assign, TokenKind.AssignInfer)) {
      values.add(literal())
      if (check(TokenKind.Comma)) {
        advance()
      }
    }
    MultiValueNode(values)
  }

  private def parse_field():Node = {
    val idents = parse_multi_value()
    consume(TokenKind.Colon)
    val typ = parse_multi_type()
    consume(TokenKind.Assign)
    var expr = expression()
    FieldNode(idents, typ, expr)
  }

  private def parse_field_infer():Node = {
    val idents = parse_multi_value()
    consume(TokenKind.AssignInfer)
    val assign = expression()
    FieldInferNode(idents, assign)
  }

  private def parse_multi_type():Node = {
    val types:JList[Node] = new ArrayList[Node]()
    consume(TokenKind.OpenParen)
    while (!check(TokenKind.CloseParen)) {
      types.add(parse_type())
      if (check(TokenKind.Comma)) {
        advance()
      }
    }
    consume(TokenKind.CloseParen)
    MultiTypeNode(types)
  }

  private def parse_type():Node = {
    val typ = current()
    if (!typ.is_type()) {
      throw new RuntimeException(s"Expected type, but got ${typ.kind}")
    }
    advance()
    TypeNode(typ)
  }



  //equality → comparison ( ( "!=" | "==" ) comparison )* ;
  //comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  //exponent → unary ( ( "**" ) unary )* ;
  //term → factor ( ( "-" | "+" ) factor )* ;
  //factor → unary ( ( "/" | "*" ) unary )* ;
  //unary → ( "!" | "-" ) unary | primary ;
  //parimary
  //
  private def equality():Node = {
    var node = comparison()
    while (check(TokenKind.NotEqual, TokenKind.Equal)) {
      val op = current()
      advance()
      val right = comparison()
      node = BinaryNode(node, op, right)
    }
    node
  }
  
  private def comparison():Node = {
    var node = term()
    while (check(TokenKind.GreaterThan, TokenKind.GreaterThanOrEqual, TokenKind.LessThan, TokenKind.LessThanOrEqual)) {
      val op = current()
      advance()
      val right = term()
      node = BinaryNode(node, op, right)
    }
    node
  }
  
  private def term():Node = {
    var node = factor()
    while (check(TokenKind.Sub, TokenKind.Add)) {
      val op = current()
      advance()
      val right = factor()
      node = BinaryNode(node, op, right)
    }
    node
  }
  
  private def factor():Node = {
    var node = unary()
    while (check(TokenKind.Slash, TokenKind.Star)) {
      val op = current()
      advance()
      val right = unary()
      node = BinaryNode(node, op, right)
    }
    node
  }

  private def unary():Node = {
    if (check(TokenKind.Bang, TokenKind.Sub)) {
      val op = current()
      advance()
      val right = unary()
      UnaryNode(op, right)
    } else {
      primary()
    }
  }
  
  private def primary():Node = {
    if (check(TokenKind.OpenParen)) {
      advance()
      val expr = expression()
      consume(TokenKind.CloseParen)
      expr
    } else {
      literal()
    }
  }
  
  private def literal():Node = {
    val tok = current()
    if (tok.is_literal()) {
      advance()
      LiteralNode(tok)
    } else {
      throw new RuntimeException(s"Expected literal, but got ${tok.kind}")
    }
  }
  
  private def consume(kind: TokenKind): Token = {
    if (check(kind)) {
      val token = current()
      advance()
      token
    } else {
      throw new RuntimeException(s"Expected $kind, but got ${current().kind}")
    }
  }

  private def consume(kind: TokenKind, message: String): Token = {
    if (check(kind)) {
      val token = current()
      advance()
      token
    } else {
      throw new RuntimeException(s"Expected $kind, but got ${current().kind} $message")
    }
  }

  private def optional_consume(kind: TokenKind): Option[Token] = {
    if (check(kind)) {
      val token = current()
      advance()
      Some(token)
    } else {
      None
    }
  }

  private def advance(int: Int): Unit = index += int
  private def advance(): Unit = advance(1)

  private def at(index: Int): Token = tokens.get(index)
  private def previous(): Token = at(index - 1)
  private def current(): Token = at(index)
  private def next(): Token = at(index + 1)

  private def check(kind: TokenKind, n:Int): Boolean = at(index + n) == kind

  private def check(tokens: TokenKind*): Boolean = {
    var status = true
    if (tokens.isEmpty) status = false
    for (i <- tokens.indices) {
      if (at(index + i) != tokens(i)) {
        status = false
      }
    }
    status
  }

}
