package io.ansan.psalm.pasm;

public enum TokenKind {

	SYMBOL_START,
		EOF("EOF"),
		EOL("EOL"),
		TAB("TAB"),
		SPACE("SPACE"),
		DIRECTIVE("@"),
		COLON(":"),
		DASH("-"),
		DOUBLE_QUOTE("DOUBLE_QUOTE"),
	SYMBOL_END,

	LITERAL_START,
		IDENT_LITERAL("IDENT_LITERAL"),
		STRING_LITERAL("STRING_LITERAL"),
		COMMENT_LITERAL("COMMENT_LITERAL"),

		NUMBER_START,
			DIGIT_LITERAL("DIGIT_LITERAL"),
			OCTET_LITERAL("OCTET_LITERAL"),
			HEX_LITERAL("HEX_LITERAL"),
			BINARY_LITERAL("BINARY_LITERAL"),
			FLOAT_LITERAL("FLOAT_LITERAL"),
		NUMBER_END,
	LITERAL_END,

	SCRATCH_REGISTER_START,
		S0("s0"),
		S1("s1"),
		S2("s2"),
		S3("s3"),
		S4("s4"),
		S5("s5"),
		S6("s6"),
		S7("s7"),
		S8("s8"),
		S9("s9"),
	SCRATCH_REGISTER_END,

	INSTRUCTIONS_START,
		LOAD("load"),
		STORE("store"),
		PUSH("push"),
		POP("pop"),
		RET("ret"),
		JMP("jmp"),

		MOV("mov"),
		ADD("add"),
		SUB("sub"),
		MUL("mul"),
		DIV("div"),

		CALL("call"),
	INSTRUCTIONS_END,
	;

	public final String name;

	TokenKind(String name) {
		this.name = name;
	}

	TokenKind() {
		this.name = null;
	}

	public boolean is_literal() {
		return LITERAL_START.ordinal() < this.ordinal() && this.ordinal() > LITERAL_END.ordinal();
	}
}
