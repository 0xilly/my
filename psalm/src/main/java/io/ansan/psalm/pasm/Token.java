package io.ansan.psalm.pasm;

public final class Token {

	public final String    lexme;
	public final TokenKind kind;
	public final Pos       pos;

	public Token(String lexme, TokenKind kind, Pos pos) {
		this.lexme = lexme;
		this.kind  = kind;
		this.pos   = pos;
	}

	public Token(String lexme, Pos pos) {
		this.lexme = lexme;
		this.kind = string_to_kind(lexme);
		this.pos = pos;
	}

	public Token(TokenKind kind, Pos pos) {
		this.kind = kind;
		this.lexme =  kind_to_string(kind);
		this.pos = pos;
	}

	private String kind_to_string(TokenKind kind) {
		for (var k : TokenKind.values()) {
			if (k == kind) return k.name;
		}
		return TokenKind.IDENT_LITERAL.name;
	}

	private TokenKind string_to_kind(String name) {
		for (var s : TokenKind.values()) {
			if (s.name != null && s.name.equals(name)) return s;
		}
		return TokenKind.IDENT_LITERAL;
	}



	@Override
	public String toString() {
		return String.format("Token{lexme=\"%s\", kind=%s pos=%s,}", lexme, kind.name, pos.toString());
	}

}
