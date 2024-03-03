package io.ansan.my
package ast

import token.{Token, TokenKind, Tokenizer}

class Parser(val tokenize: Tokenizer) {
  private val tokens = tokenize.tokens
  private var index = 0
  
  def expression():Node = {
    ???
  }

  def statement():Node = {
    ??? //Implement this
  }

  def parse():Node = {
    ???
  }

  private def parse_pkg():Node = {
    val pkg = consume(TokenKind.Pkg)
    val ident = consume(TokenKind.IdentLiteral)
    consume(TokenKind.Eol)
    PkgNode(pkg, ident)
  }

  private def parse_use():Node = {
    val use = consume(TokenKind.Use)
    val moudel = consume(TokenKind.IdentLiteral)
    consume(TokenKind.Colon)
    ???
  }

  private def parse_use_path():Node = {
    var path = List[Token]()
    while (check(TokenKind.Eol)) {
      advance()
      path = path :+ consume(TokenKind.IdentLiteral)
    }
    UsePathNode(path)
  }

  // declarations 
  private def parse_declaration():Node = {
    /**
     * 
     *
     * Fixme(antia): This is not done implement this for the rest of the declarations
     */
    val ex = parse_export()
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

  private def parse_export():Option[Node] = {
    val ex = optional_consume(TokenKind.Export)
    if (ex.isEmpty) return None

    val open = optional_consume(TokenKind.OpenParen)
    if (open.isDefined) {
      val token = consume(TokenKind.Pkg, TokenKind.Global)
      consume(TokenKind.CloseParen)
      return Some(ExportNode(Some(token)))
    }
    Some(ExportNode(None))
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

  private def parse_enum_members():List[Node] = {
    var members = List[Node]()

    while (check(TokenKind.IdentLiteral)) {
      members = members :+ parse_enum_member_declaration()
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

  private def parse_data_members():List[Node] = {
    var members = List[Node]()
    while (check(TokenKind.IdentLiteral)) {
      members = members :+ parse_data_member_declaration()
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

  private def parse_union_members():List[Node] = {
    var members = List[Node]()
    while (check(TokenKind.IdentLiteral)) {
      members = members :+ parse_union_member_declaration()
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
    var body = List[Node]()
    consume(TokenKind.OpenBrace)
    while (!check(TokenKind.CloseBrace)) {
      body = body :+ parse_declaration()
    }
    consume(TokenKind.CloseBrace)
    BlockNode(body)
  }

  private def parse_fn_args():Node = {
    var args:List[Node] = List[Node]()
    consume(TokenKind.OpenParen)
    while (!check(TokenKind.CloseParen)) {
      args = args :+ parse_fn_arg()
      if (check(TokenKind.Comma)) {
        advance()
      }
    }
    consume(TokenKind.CloseParen)
    FnArgListNode(args)
  }

  private def parse_fn_arg():Node = {
    val own = optional_consume(TokenKind.Own)
    val ident = consume(TokenKind.IdentLiteral)
    consume(TokenKind.Colon)
    val typ = parse_type()
    FnArgNode(own, ident, typ)
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
    var members:List[Node] =  List[Node]()
    consume(TokenKind.OpenBrace)
    while (!check(TokenKind.CloseBrace)) {
      members = members :+ parse_interface_fn()
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
  
  //call -> primary ( "(" arguments ")" | "." ident )*
  private def parse_arg_call():Node = {
    ???
  }
  


  private def parse_multi_value():Node = {
    var values:List[Node] = List[Node]()
    while (!check(TokenKind.Assign, TokenKind.AssignInfer)) {
      values = values :+ literal()
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

  private def parse_array_initializer():Node = {
    var values:List[Node] = List[Node]()
    while (!check(TokenKind.CloseBracket)) {
      values = values :+ expression()
      if (check(TokenKind.Comma)) {
        advance()
      }
    }
    ArrayInitializerNode(values)
  }

  private def parse_slice():Node = {
    val ident = literal()
    val open = consume(TokenKind.OpenBracket)
    val lit = ident.asInstanceOf[LiteralNode]
    if (distance(open, lit.value) != 1) {
      throw new RuntimeException(s"Expected slice")
    }
    val start = expression()
    consume(TokenKind.Colon)
    val end = expression()
    consume(TokenKind.CloseBracket)
    ArraySliceNode(ident, start, end)
  }

  private def parse_multi_type():Node = {
    var types = List[Node]()
    consume(TokenKind.OpenParen)
    while (!check(TokenKind.CloseParen)) {
      types = types :+ parse_type()
      if (check(TokenKind.Comma)) {
        advance()
      }
    }
    consume(TokenKind.CloseParen)
    MultiTypeNode(types)
  }

  private def parse_type():Node = {
    if (check(TokenKind.OpenParen)) {
      parse_multi_type()
    } else if (check(TokenKind.OpenBracket)) {
      parse_array_type()
    } else if (check(TokenKind.Carrot)) {
      parse_pointer_type()
    } else {
      parse_base_type()
    }
  }

  //parse array_type []type
  private def parse_array_type():Node = {
    consume(TokenKind.OpenBracket)
    val ident = optional_consume(TokenKind.IdentLiteral, TokenKind.IntLiteral)
    val close = consume(TokenKind.CloseBracket)
    val typ = parse_base_type() //Note(anita): Maybe this should be literal and an optional for literal? - 2/28/2024
    //Type location must be +1 in the source location of the ] token
    val node = typ.asInstanceOf[TypeNode]
    if (distance(close, node.typ) != 1) {
      throw new RuntimeException(s"the format for array type is []type")
    }

    TypeArrayNode(ident, typ)
  }

  private def parse_pointer_type():Node = {
    val carrot = consume(TokenKind.Carrot)
    val typ = parse_base_type()
    val node = typ.asInstanceOf[TypeNode]
    if (distance(carrot, node.typ) != 1) {
      throw new RuntimeException(s"Format for pointer type is ^type")
    }
    TypePointerNode(carrot, typ)
  }

  private def parse_base_type():Node = {
    val typ = current()
    if (!typ.is_type()) {
      throw new RuntimeException(s"Expected base type, but got ${typ.kind}")
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
    var left = comparison()
    while (check(TokenKind.NotEqual, TokenKind.Equal)) {
      val op = current()
      advance()
      val right = comparison()
      left = BinaryNode(left, op, right)
    }
    left
  }

  private def logical_and():Node = {
    var left = equality()
    while (check(TokenKind.LogicalAnd)) {
      val op = current()
      advance()
      val right = equality()
      left = BinaryNode(left, op, right)
    }
    left
  }

  private def logical_or():Node = {
    var left = logical_and()
    while (check(TokenKind.LogicalOr)) {
      val op = current()
      advance()
      val right = logical_and()
      left = BinaryNode(left, op, right)
    }
    left
  }
  
  private def comparison():Node = {
    var left = term()
    while (check(TokenKind.GreaterThan, TokenKind.GreaterThanOrEqual, TokenKind.LessThan, TokenKind.LessThanOrEqual)) {
      val op = current()
      advance()
      val right = term()
      left = BinaryNode(left, op, right)
    }
    left
  }
  
  private def term():Node = {
    var left = factor()
    while (check(TokenKind.Sub, TokenKind.Add)) {
      val op = current()
      advance()
      val right = factor()
      left = BinaryNode(left, op, right)
    }
    left
  }
  
  private def factor():Node = {
    var left = unary()
    while (check(TokenKind.Slash, TokenKind.Star)) {
      val op = current()
      advance()
      val right = unary()
      left = BinaryNode(left, op, right)
    }
    left
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

  ////////////// SOURCE TRAVERSAL  ///////////////
  
  private def consume(kind: TokenKind): Token = {
    if (check(kind)) {
      val token = current()
      advance()
      token
    } else {
      throw new RuntimeException(s"Expected $kind, but got ${current().kind}")
    }
  }

  private def consume(kind: TokenKind*): Token = {
    for (i <- kind.indices) {
      if (check(kind(i))) {
        val tok = current()
        advance()
        return tok
      }
    }
    throw new RuntimeException(s"Expected $kind, but got ${current().kind}")
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

  private def optional_consume(token:TokenKind*): Option[Token] = {
    for (i <- token.indices) {
      if (check(token(i))) {
        val tok = current()
        advance()
        return Some(tok)
      }
    }
    None
  }

  private def advance(int: Int): Unit = index += int
  private def advance(): Unit = advance(1)

  private def at(i: Int): Token = tokens(i)
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

  private def distance(token: Token, token2: Token): Int = token2.location.start - token.location.end

}
