package io.ansan.psalm.pasm;

public final class Pos {
	public final String  file;
	public final long    offset_start;
	public final long    line;
	public final long    column;
	public final long    offset_end;

	public Pos(String file, long offset_start, long line, long column, long offset_end) {
		this.file         = file;
		this.offset_start = offset_start;
		this.line         = line;
		this.column       = column;
		this.offset_end   = offset_end;
	}

	public long getLen() {return offset_end - offset_start;}

	@Override
	public String toString() {
		return String.format("Pos{file=%s, offset_start=%d, line=%d, column=%d, offset_end=%d}", file, offset_start, line, column, offset_end);
	}
}
