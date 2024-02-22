package io.ansan.my
package token


import java.nio.file.{Files, Paths}
import java.util
import java.util.List as JList

enum TokenKind(val name:String, val value:String) extends Enumeration {
  case Eol extends TokenKind("Eol", "\\n")
  case Eof extends TokenKind("Eof", "<eof>")
  case Comment extends TokenKind("Comment", "<comment>")

  case OpenParen extends TokenKind("ParenOpen", "(")
  case CloseParen extends TokenKind("ParenClose", ")")
  case OpenBrace extends TokenKind("BraceOpen", "{")
  case CloseBrace extends TokenKind("BraceClose", "}")
  case OpenBracket extends TokenKind("BracketOpen", "[")
  case CloseBracket extends TokenKind("BracketClose", "]")
  case Comma extends TokenKind("Comma", ",")
  case Colon extends TokenKind("Colon", ":")
  case Semicolon extends TokenKind("Semicolon", ";")
  case Dot extends TokenKind("Dot", ".")
  case Question extends TokenKind("Question", "?")
  case At extends TokenKind("At", "@")
  case Dollar extends TokenKind("Dollar", "$")
  case Backslash extends TokenKind("Backslash", "\\")
  case RightArrow extends TokenKind("RightArrow", "->")
  case LeftArrow extends TokenKind("LeftArrow", "<-")
  case Todo extends TokenKind("Todo", "???")
  case Ellipsis extends TokenKind("Ellipsis", "...")
  case Underscore extends TokenKind("Underscore", "_")
  case Decelerator extends TokenKind("Decelerator", "::")
  case Bang extends TokenKind("Bang", "!")

  case Whitespace extends TokenKind("Whitespace", "<whitespace>")
  case IdentLiteral extends TokenKind("IdentLiteral", null)
  case IntLiteral extends TokenKind("IntLiteral", null)
  case BinaryLiteral extends TokenKind("BinaryLiteral", null)
  case OctalLiteral extends TokenKind("OctalLiteral", null)
  case HexLiteral extends TokenKind("HexLiteral", null)
  case DecimalLiteral extends TokenKind("DecimalLiteral", null)
  case FloatLiteral extends TokenKind("FloatLiteral", null)
  case StringLiteral extends TokenKind("StringLiteral", null)
  case RuneLiteral extends TokenKind("RuneLiteral", null)

  case Add extends TokenKind("Add", "+")
  case Sub extends TokenKind("Sub", "-")
  case Star extends TokenKind("Star", "*")
  case Pow extends TokenKind("Power", "**")
  case Slash extends TokenKind("Slash", "/")
  case Modulo extends TokenKind("Modulo" , "%")

  case BitwiseAnd extends TokenKind("BitwiseAnd", "&")
  case BitwiseOr extends TokenKind("BitwiseOr", "|")
  case Carrot extends TokenKind("Carrot", "^")
  case BitwiseNot extends TokenKind("BitwiseNot", "~")
  case BitwiseLeftShift extends TokenKind("BitwiseLeftShift", "<<")
  case BitwiseRightShift extends TokenKind("BitwiseRightShift", ">>")
  case LogicalAnd extends TokenKind("LogicalAnd", "&&")
  case LogicalOr extends TokenKind("LogicalOr", "||")
  case LogicalNot extends TokenKind("LogicalNot", "!")

  case Equal extends TokenKind("Equal", "==")
  case NotEqual extends TokenKind("NotEqual", "!=")

  case LessThan extends TokenKind("LessThan", "<")
  case GreaterThan extends TokenKind("GreaterThan", ">")
  case LessThanOrEqual extends TokenKind("LessThanOrEqual", "<=")
  case GreaterThanOrEqual extends TokenKind("GreaterThanOrEqual", ">=")

  case Assign extends TokenKind("Assign", "=")
  case AssignInfer extends TokenKind("AssignInfer", ":=")
  case AddAssign extends TokenKind("AddAssign", "+=")
  case SubAssign extends TokenKind("SubAssign", "-=")
  case MulAssign extends TokenKind("MulAssign", "*=")
  case PowAssign extends TokenKind("PowAssign", "**=")
  case DivAssign extends TokenKind("DivAssign", "/=")
  case ModAssign extends TokenKind("ModAssign", "%=")

  //Declarations
  case Pkg extends TokenKind("Pkg", "pkg")
  case Use extends TokenKind("Use", "use")
  case Type extends TokenKind("Type", "type")
  case Data extends TokenKind("Data", "data")
  case Fn extends TokenKind("Fn", "fn")
  case Enum extends TokenKind("Enum", "enum")
  case Union extends TokenKind("Union", "union")
  case Interface extends TokenKind("Interface", "interface")
  case Link extends TokenKind("Link", "link")
  case Bind extends TokenKind("Bind", "bind")
  case Alias extends TokenKind("Alias", "alias")

  //Keywords
  case If extends TokenKind("If", "if")
  case Else extends TokenKind("Else", "else")
  case For extends TokenKind("For", "for")
  case Match extends TokenKind("Match", "match")
  case Defer extends TokenKind("Defer", "defer")
  case Return extends TokenKind("Return", "return")
  case Break extends TokenKind("Break", "break")
  case Continue extends TokenKind("Continue", "continue")
  case Fall extends TokenKind("Fall", "fall")
  case With extends TokenKind("With", "with")
  case Construct extends TokenKind("Construct", "construct")
  case Destruct extends TokenKind("Destruct", "destruct")
  case As extends TokenKind("As", "as")

  //Types
  case U8 extends TokenKind("U8", "u8")
  case U16 extends TokenKind("U16", "u16")
  case U32 extends TokenKind("U32", "u32")
  case U64 extends TokenKind("U64", "u64")
  case I8 extends TokenKind("I8", "i8")
  case I16 extends TokenKind("I16", "i16")
  case I32 extends TokenKind("I32", "i32")
  case I64 extends TokenKind("I64", "i64")
  case F32 extends TokenKind("F32", "f32")
  case F64 extends TokenKind("F64", "f64")
  case Bool extends TokenKind("Bool", "bool")
  case Rune extends TokenKind("Rune", "rune")
  case String extends TokenKind("String", "string")
  case Void extends TokenKind("Void", "void")
  case nil extends TokenKind("Nil", "nil")
  case True extends TokenKind("True", "true")
  case False extends TokenKind("False", "false")
  case Self extends TokenKind("Self", "self")
  case Super extends TokenKind("Super", "super")
}

case class Location(target:String, start:Int, line: Int, column: Int, end:Int)

case class Token(val kind:TokenKind, val lexme:String, val location:Location) {
  def this(kind:TokenKind, loc:Location) = this(kind, kind.value, loc)
  
  def is_literal():Boolean = {
    kind match {
      case TokenKind.IntLiteral 
           | TokenKind.BinaryLiteral 
           | TokenKind.OctalLiteral
           | TokenKind.HexLiteral
           | TokenKind.DecimalLiteral 
           | TokenKind.FloatLiteral 
           | TokenKind.StringLiteral 
           | TokenKind.RuneLiteral => true
      case _ => false
    }
  }

  def is_type():Boolean = {
    kind match {
      case TokenKind.U8 
           | TokenKind.U16 
           | TokenKind.U32 
           | TokenKind.U64 
           | TokenKind.I8 
           | TokenKind.I16 
           | TokenKind.I32 
           | TokenKind.I64 
           | TokenKind.F32 
           | TokenKind.F64 
           | TokenKind.Bool 
           | TokenKind.Rune 
           | TokenKind.String 
           | TokenKind.Void 
           | TokenKind.IdentLiteral => true
      case _ => false
    }
  }

  def is_int_type():Boolean = {
    kind match {
      case TokenKind.U8 
           | TokenKind.U16 
           | TokenKind.U32 
           | TokenKind.U64 
           | TokenKind.I8 
           | TokenKind.I16 
           | TokenKind.I32 
           | TokenKind.I64 => true
      case _ => false
    }
  }
}


class Tokenize(val target:String) {
  private val path = Paths.get(target)
  private val buffer = Files.readAllBytes(path)
  private var idx = 0
  private var line = 1
  private var column = 0
  val tokens:JList[Token] = new util.ArrayList[Token]

  private def scan_token():Token = {
    val start = idx
    val tok: Token = current() match {
      case '\n' => new Token(TokenKind.Eol, Location(target, start, line, column, idx))
      case ' ' | '\r' | '\t' => new Token(TokenKind.Whitespace, Location(target, start, line, column, idx))
      case '(' => new Token(TokenKind.OpenParen, Location(target, start, line, column, idx))
      case ')' => new Token(TokenKind.CloseParen, Location(target, start, line, column, idx))
      case '{' => new Token(TokenKind.OpenBrace, Location(target, start, line, column, idx))
      case '}' => new Token(TokenKind.CloseBrace, Location(target, start, line, column, idx))
      case '[' => new Token(TokenKind.OpenBracket, Location(target, start, line, column, idx))
      case ']' => new Token(TokenKind.CloseBracket, Location(target, start, line, column, idx))
      case ',' => new Token(TokenKind.Comma, Location(target, start, line, column, idx))
      case ':' => {
        if (check("::")) {
          advance(1)
          new Token(TokenKind.Decelerator, Location(target, start, line, column, idx))
        }
        if (check(":=")) {
          advance()
          new Token(TokenKind.AssignInfer, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.Colon, Location(target, start, line, column, idx))
      }
      case ';' => new Token(TokenKind.Semicolon, Location(target, start, line, column, idx))
      case '.' => {
        if (check("...")) {
          advance(2)
          new Token(TokenKind.Ellipsis, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.Dot, Location(target, start, line, column, idx))
      }
      case '?' => {
        if (check("???")) {
          advance(2)
          new Token(TokenKind.Todo, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.Question, Location(target, start, line, column, idx))
      }
      case '@' => new Token(TokenKind.At, Location(target, start, line, column, idx))
      case '$' => new Token(TokenKind.Dollar, Location(target, start, line, column, idx))
      case '\\' => new Token(TokenKind.Backslash, Location(target, start, line, column, idx))
      case '+' => {
        if (check("+=")) {
          advance()
          new Token(TokenKind.AddAssign, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.Add, Location(target, start, line, column, idx))
      }
      case '-' => {
        if (check("-=")) {
          advance()
          new Token(TokenKind.SubAssign, Location(target, start, line, column, idx))
        }
        if (check("->")) {
          advance()
          new Token(TokenKind.RightArrow, Location(target, start, line, column, idx))
        }

        new Token(TokenKind.Sub, Location(target, start, line, column, idx))
      }
      case '*' => {
        if (check("*=")) {
          advance()
          new Token(TokenKind.MulAssign, Location(target, start, line, column, idx))
        }
        if (check("**")) {
          advance()
          new Token(TokenKind.Pow, Location(target, start, line, column, idx))
        }
        if (check("**=")) {
          advance(2)
          new Token(TokenKind.PowAssign, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.Star, Location(target, start, line, column, idx))
      }
      case '/' => {
        if (check("/=")) {
          advance()
          new Token(TokenKind.DivAssign, Location(target, start, line, column, idx))
        }
        if (check("//")) {

          new Token(TokenKind.Comment, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.Slash, Location(target, start, line, column, idx))
      }
      case '%' => {
        if (check("%=")) {
          advance()
          new Token(TokenKind.ModAssign, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.Modulo, Location(target, start, line, column, idx))
      }
      case '&' => {
        if (check("&&")) {
          advance()
          new Token(TokenKind.LogicalAnd, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.BitwiseAnd, Location(target, start, line, column, idx))
      }
      case '|' => {
        if (check("||")) {
          advance()
          new Token(TokenKind.LogicalOr, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.BitwiseOr, Location(target, start, line, column, idx))
      }

      case '^' => new Token(TokenKind.Carrot, Location(target, start, line, column, idx))
      case '~' => new Token(TokenKind.BitwiseNot, Location(target, start, line, column, idx))
      case '=' => {
        if (check("==")) {
          advance()
          new Token(TokenKind.Equal, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.Assign, Location(target, start, line, column, idx))
      }
      case '!' => {
        if (check("!=")) {
          advance()
          new Token(TokenKind.NotEqual, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.LogicalNot, Location(target, start, line, column, idx))
      }
      case '<' => {
        if (check("<=")) {
          advance()
          new Token(TokenKind.LessThanOrEqual, Location(target, start, line, column, idx))
        }
        if (check("<<")) {
          advance()
          new Token(TokenKind.BitwiseLeftShift, Location(target, start, line, column, idx))
        }
        if (check("<-")) {
          advance()
          new Token(TokenKind.LeftArrow, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.LessThan, Location(target, start, line, column, idx))
      }
      case '>' => {
        if (check(">=")) {
          advance()
          new Token(TokenKind.GreaterThanOrEqual, Location(target, start, line, column, idx))
        }
        new Token(TokenKind.GreaterThan, Location(target, start, line, column, idx))
      }
      //check digits or letters
      case _ => {
        if (Character.isDigit(current()) || check(".")) {
          return concat_number()
        } else if (Character.isLetter(current()) || check("_")) {
          val ident = concat_ident()
          val kind = TokenKind.values.find(_.value == ident).getOrElse(TokenKind.IdentLiteral)
          Token(kind, ident, Location(target, start, line, column, idx))
        } else {
          new Token(TokenKind.Eof, Location(target, start, line, column, idx))
        }
      }
    }
    tok
  }
  
  private def concat_number():Token = {
    val start = idx

    if (check("0x")) {
      advance(1)
      while(Character.isDigit(current()) || (current() >= 'a' && current() <= 'f')) {
        advance()
      }
      return Token(TokenKind.IntLiteral, new String(buffer, start, idx - start), Location(target, start, line, column, idx))
    }

    if(check("0b")) {
      advance(1)
      while(current() == '0' || current() == '1') {
        advance()
      }
      return Token(TokenKind.IntLiteral, new String(buffer, start, idx - start), Location(target, start, line, column, idx))
    }

    if (check("0o")) {
      advance(1)
      while(current() >= '0' && current() <= '7') {
        advance()
      }
      return Token(TokenKind.IntLiteral, new String(buffer, start, idx - start), Location(target, start, line, column, idx))
    }

    while(Character.isDigit(current())) {
      advance()
    }

    if (current() == '.' && Character.isDigit(next())) {
      advance()
      while(Character.isDigit(current())) {
        advance()
      }
      return Token(TokenKind.DecimalLiteral, new String(buffer, start, idx - start), Location(target, start, line, column, idx))
    }

    return Token(TokenKind.IntLiteral, new String(buffer, start, idx - start), Location(target, start, line, column, idx))
  }
  
  private def concat_ident():String = {
    val start = idx
    while(Character.isLetterOrDigit(current()) || current() == '_') {
      advance()
    }
    new String(buffer, start, idx - start)
  }


  private def scan_comment():Token = {
    val start = idx
    while(current() != '\n') {
      advance()
    }
    val comment = new String(buffer, start, idx - start)
    idx -= 1;
    new Token(TokenKind.Comment, comment, Location(target, start, line, column, idx))
  }


  private def advance(n:Int):Unit = {
    idx += n
    column += n
  }


  private def at(n:Int):Char = buffer(idx + n).toChar
  private def previous():Char = this.at(-1)
  private def current():Char = this.at(0)
  private def next():Char = this.at(1)

  private def advance():Unit = advance(1)

  private def check(str:String):Boolean = {
    var brk: Boolean = true
    for(i <- 0 until str.length) {
      if(this.at(i) != str(i)) {
        brk = false
      }
    }
    brk
  }

}
