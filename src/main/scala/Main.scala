package io.ansan.my

import token.{TokenKind, Tokenizer}

object Main {
  def main(args: Array[String]): Unit = {
    val tokenizer = new Tokenizer("./tests/data.my")
    tokenizer.tokenize()
    val tokens = tokenizer.tokens.filterNot(_.kind == TokenKind.Whitespace)
    tokens.foreach(println)
  }
}