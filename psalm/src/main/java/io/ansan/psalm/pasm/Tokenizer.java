package io.ansan.psalm.pasm;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import static io.ansan.psalm.pasm.TokenKind.*;

public final class Tokenizer {

	private byte[]           buffer;
	private int              idx    = 0;
	private final long       line   = 1;
	private final long       column = 1;
	private final String     target;
	public final List<Token> tokens;

	public Tokenizer(String target) {
		tokens      = new ArrayList<>();
		Path path   = Paths.get(target);
		this.target = target;

		try {
			if (!Files.exists(path)) {
				System.err.println("Lex Error: Lexer not able to read file");
			}
			this.buffer = Files.readAllBytes(path);
		} catch (IOException ioex) {
			ioex.printStackTrace();
		}

		for(;;) {
		//	var tok = scan_token();

		//	if (tok.kind == EOF) {
		//		tokens.add(tok);
		//		break;
		//	}

			//tokens.add(tok);
		}
	}

	private Token scan_token() {
		var start = idx;
		return switch(peek()) {
			case '\0' -> add_token(EOF, start);
			case '\n' -> add_token(EOL, start);
			case '\t' -> add_token(TAB, start);
			case ' '  -> add_token(SPACE, start);
			case '@'  -> add_token(DIRECTIVE, start);
			case ':'  -> add_token(COLON, start);
			case '-'  -> add_token(DASH, start);

			default -> {
				if (Character.isDigit(peek())) {
					yield number();
				} else {
					yield ident_literal();
				}
			}
		};
	}

	private String concat_comment() {
		var sb = new StringBuilder();
		while (!check('\n')) {
			sb.append(peek());
			advance();
		}
		return sb.toString();
	}

	private Token ident_literal() {
		var start = idx;
		var sb = new StringBuilder();
		for (;;) {
			if (is_separator() || Character.isLetter(peek()) || !check('_')) break;
			sb.append(peek());
			advance();
		}
		return add_token(sb.toString(), start);
	}

	private Token number() {
		var sb = new StringBuilder();
		boolean has_dot = false;
		var start = idx;
		if (check('0')) {
			sb.append(peek());
			advance();
			switch(peek()) {
				case 'x':{
					sb.append(peek());
					advance();
					while(is_hex()) {
						sb.append(peek());
						advance();
					}

					if (!is_separator()){
						tokenizer_error("Missing a seporator token got '" + peek() + "' instead.");
					}

					return add_token(sb.toString(), HEX_LITERAL, start);
				}
				case 'b':{
					sb.append(peek());
					advance();
					while(is_binary()) {
						sb.append(peek());
						advance();
					}

					if (!is_separator()){
						tokenizer_error("Missing a seporator token got '" + peek() + "' instead.");
					}

					return add_token(sb.toString(), HEX_LITERAL, start);
				}
				case 'o':{
					sb.append(peek());
					advance();
					while(is_octal()) {
						sb.append(peek());
						advance();
					}

					if (!is_separator()){
						tokenizer_error("Missing a seporator token got '" + peek() + "' instead.");
					}

					return add_token(sb.toString(), HEX_LITERAL, start);
				}
				default: tokenizer_error("Illgal integer type of '" + peek() + "' only x = hex, b = binary, and o = octal are allowed.");
			}
		} else {
			for(;;) {
				if (!is_digit() || !check('.')) {
					break;
				}

				if (check('.')) {
					if (!has_dot) {
						has_dot = true;
					} else {
						tokenizer_error("Floats can only have one '.' we got two.");
					}
					sb.append(peek());
				} else {
					sb.append(peek());
				}
				advance();
			}

			if (has_dot) {
				return add_token(sb.toString(), FLOAT_LITERAL, start);
			} else {
				return add_token(sb.toString(), DIGIT_LITERAL, start);
			}
		}
		return null;
	}

	private Token concat_string() {
		var start_open = idx;
		Token open = null;
		Token str   = null;
		Token close   = null;

		if (check('"')) {
			open = add_token(DOUBLE_QUOTE, start_open);
			advance();
		}

		var start_str = idx;

		var sb = new StringBuilder();
		while (!check('"')) {
			sb.append(peek());
			advance();
		}
		str = add_token(sb.toString(), STRING_LITERAL, start_str);

		var start_close = idx;
		if (check('"')) {
			open = add_token(DOUBLE_QUOTE, start_close);
		} else {
			tokenizer_error("Unable to parse '\"' got %s instead".formatted(peek()));
		}
		tokens.add(open);
		tokens.add(str);

		return close;
	}

	private boolean is_hex() {
		return switch(peek()) {
			case '0', '1', '2', '3', '4',
			     '5', '6', '7', '8', '9',
			     'a', 'b', 'c', 'd', 'e', 'f',
			     'A', 'B', 'C', 'D', 'E', 'F' -> true;
			default -> false;
		};
	}

	private boolean is_binary() {
		return check('0') || check('1');
	}

	private boolean is_octal() {
		return switch(peek()) {
			case '0', '1', '2', '3',
			     '4', '5', '6', '7' -> true;
			default -> false;
		};
	}

	private boolean is_digit() {
		return Character.isDigit(peek());
	}

	private boolean is_separator() {
		return check(',') || check('\n') || check(';') || is_space();
	}

	private boolean is_space() {
		return check(' ') || check('\t');
	}

	private char peek(int n) {
		return (char)this.buffer[this.idx + n];
	}

	private char peek() {
		return peek(0);
	}

	private boolean check(int n, char c) {
		return peek(n) == c;
	}

	private boolean check(char c) {
		return check(0, c);
	}

	private void advance(int n) {
		this.idx = this.idx + n;
	}

	private void advance() {
		advance(1);
	}

	private void tokenizer_error(String msg) {
		var str = String.format("Tokenizer Error: %s", msg);
		System.err.println(str);
		System.exit(-1);
	}

	private Token add_token(String lexme, TokenKind kind, Pos pos) {
		return new Token(lexme, kind, pos);
	}

	private Token add_token(String lexme, TokenKind kind, long start) {
		var pos = new Pos(target, start, line, column, idx);
		return new Token(lexme, kind, pos);
	}

	private Token add_token(String lexme, Pos pos) {
		return new Token(lexme, pos);
	}

	private Token add_token(String lexme, long start) {
		var pos = new Pos(target, start, line, column, idx);
		return new Token(lexme, pos);
	}

	private Token add_token(TokenKind kind, Pos pos) {
		return new Token(kind, pos);
	}

	private Token add_token(TokenKind kind, long start) {
		var pos = new Pos(target, start, line, column, idx);
		return new Token(kind, pos);
	}

}
