unit MemFormatters;

	(*
		A. V. Shiryaev, 2015.03, 2016.04

		LE: Little-Endian
		BE: Big-Endian
	*)

interface

type
	BYTE = System.Byte;
	INTEGER = System.Integer;
	REAL = System.Single;
	LONGREAL = System.Double;

	(* Write *)
		PROCEDURE WriteIntLE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		PROCEDURE WriteIntBE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		PROCEDURE WriteInt24LE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		PROCEDURE WriteInt24BE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		PROCEDURE WriteInt16LE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		PROCEDURE WriteInt16BE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		PROCEDURE WriteInt8 (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		PROCEDURE WriteRealLE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: REAL);
		PROCEDURE WriteRealBE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: REAL);
		PROCEDURE WriteLongRealLE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: LONGREAL);
		PROCEDURE WriteLongRealBE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: LONGREAL);

	(* Read *)
		PROCEDURE ReadIntLE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadIntBE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadUInt24LE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadUInt24BE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadSInt24LE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadSInt24BE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadUInt16LE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadUInt16BE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadSInt16LE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadSInt16BE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadUInt8 (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadSInt8 (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		PROCEDURE ReadRealLE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: REAL);
		PROCEDURE ReadRealBE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: REAL);
		PROCEDURE ReadLongRealLE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: LONGREAL);
		PROCEDURE ReadLongRealBE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: LONGREAL);

implementation

	(* Write *)

		PROCEDURE WriteIntLE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		BEGIN
			a[w] := x;
			a[w + 1] := x shr 8;
			a[w + 2] := x shr 16;
			a[w + 3] := x shr 24;
			INC(w, 4)
		END (* WriteIntLE *);

		PROCEDURE WriteIntBE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		BEGIN
			a[w] := x shr 24;
			a[w + 1] := x shr 16;
			a[w + 2] := x shr 8;
			a[w + 3] := x;
			INC(w, 4)
		END (* WriteIntBE *);

		PROCEDURE WriteInt24LE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		BEGIN
			a[w] := x;
			a[w + 1] := x shr 8;
			a[w + 2] := x shr 16;
			INC(w, 3)
		END (* WriteInt24LE *);

		PROCEDURE WriteInt24BE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		BEGIN
			a[w] := x shr 16;
			a[w + 1] := x shr 8;
			a[w + 2] := x;
			INC(w, 3)
		END (* WriteInt24BE *);

		PROCEDURE WriteInt16LE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		BEGIN
			a[w] := x;
			a[w + 1] := x shr 8;
			INC(w, 2)
		END (* WriteInt16LE *);

		PROCEDURE WriteInt16BE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		BEGIN
			a[w] := x shr 8;
			a[w + 1] := x;
			INC(w, 2)
		END (* WriteInt16BE *);

		PROCEDURE WriteInt8 (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: INTEGER);
		BEGIN
			a[w] := x;
			INC(w)
		END (* WriteInt8 *);

		PROCEDURE WriteRealLE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: REAL);
			var y: packed array [0..3] of BYTE absolute x;
		BEGIN
			a[w] := y[0];
			a[w + 1] := y[1];
			a[w + 2] := y[2];
			a[w + 3] := y[3];
			INC(w, 4)
		END (* WriteRealLE *);

		PROCEDURE WriteRealBE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: REAL);
			var y: packed array [0..3] of BYTE absolute x;
		BEGIN
			a[w] := y[3];
			a[w + 1] := y[2];
			a[w + 2] := y[1];
			a[w + 3] := y[0];
			INC(w, 4)
		END (* WriteRealBE *);

		PROCEDURE WriteLongRealLE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: LONGREAL);
			var y: packed array [0..7] of BYTE absolute x;
		BEGIN
			a[w] := y[0];
			a[w + 1] := y[1];
			a[w + 2] := y[2];
			a[w + 3] := y[3];
			a[w + 4] := y[4];
			a[w + 5] := y[5];
			a[w + 6] := y[6];
			a[w + 7] := y[7];
			INC(w, 8)
		END (* WriteLongRealLE *);

		PROCEDURE WriteLongRealBE (VAR a: ARRAY OF BYTE; VAR w: INTEGER; x: LONGREAL);
			var y: packed array [0..7] of BYTE absolute x;
		BEGIN
			a[w] := y[7];
			a[w + 1] := y[6];
			a[w + 2] := y[5];
			a[w + 3] := y[4];
			a[w + 4] := y[3];
			a[w + 5] := y[2];
			a[w + 6] := y[1];
			a[w + 7] := y[0];
			INC(w, 8)
		END (* WriteLongRealBE *);

	(* Read *)

		PROCEDURE ReadIntLE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
(*
			Move(a[r], x, 4);
*)
			x := a[r] + a[r + 1] shl 8 + a[r + 2] shl 16 + a[r + 3] shl 24;
			INC(r, 4)
		END (* ReadIntLE *);

		PROCEDURE ReadIntBE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r] shl 24 + a[r + 1] shl 16 + a[r + 2] shl 8 + a[r + 3];
			INC(r, 4)
		END (* ReadIntBE *);

		PROCEDURE ReadUInt24LE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r] + a[r + 1] shl 8 + a[r + 2] shl 16;
			INC(r, 3)
		END (* ReadUInt24LE *);

		PROCEDURE ReadUInt24BE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r] shl 16 + a[r + 1] shl 8 + a[r + 2];
			INC(r, 3)
		END (* ReadUInt24BE *);

		PROCEDURE ReadSInt24LE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r] + a[r + 1] shl 8 + a[r + 2] shl 16;
			IF x >= $800000 THEN begin x := x - $1000000 END;
(*
			x := ASR(LSL(
					a[r] + a[r + 1] shl 8 + a[r + 2] shl 16,
				8), 8);
*)
			INC(r, 3)
		END (* ReadSInt24LE *);

		PROCEDURE ReadSInt24BE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r] shl 16 + a[r + 1] shl 8 + a[r + 2];
			IF x >= $800000 THEN begin x := x - $1000000 END;
(*
			x := ASR(LSL(
					a[r] shl 16 + a[r + 1] shl 8 + a[r + 2],
				8), 8);
*)
			INC(r, 3)
		END (* ReadSInt24BE *);

		PROCEDURE ReadUInt16LE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r] + a[r + 1] shl 8;
			INC(r, 2)
		END (* ReadUInt16LE *);

		PROCEDURE ReadUInt16BE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r] shl 8 + a[r + 1];
			INC(r, 2)
		END (* ReadUInt16BE *);

		PROCEDURE ReadSInt16LE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r] + a[r + 1] shl 8;
			IF x >= 32768 THEN begin x := x - 65536 END;
(*
			x := ASR(LSL(a[r] + a[r + 1] shl 8, 16), 16);
*)
			INC(r, 2)
		END (* ReadSInt16LE *);

		PROCEDURE ReadSInt16BE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r] shl 8 + a[r + 1];
			IF x >= 32768 THEN begin x := x - 65536 END;
(*
			x := ASR(LSL(a[r] shl 8 + a[r + 1], 16), 16);
*)
			INC(r, 2)
		END (* ReadSInt16BE *);

		PROCEDURE ReadUInt8 (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r];
			INC(r)
		END (* ReadUInt8 *);

		PROCEDURE ReadSInt8 (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: INTEGER);
		BEGIN
			x := a[r];
			IF x >= 128 THEN begin x := x - 256 END;
(*
			x := ASR(LSL(a[r], 24), 24);
*)
			INC(r)
		END (* ReadSInt8 *);

		PROCEDURE ReadRealLE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: REAL);
			VAR t: packed array [0..3] of BYTE absolute x;
		BEGIN
			t[0] := a[r];
			t[1] := a[r + 1];
			t[2] := a[r + 2];
			t[3] := a[r + 3];
			INC(r, 4)
		END (* ReadRealLE *);

		PROCEDURE ReadRealBE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: REAL);
			VAR t: packed array [0..3] of BYTE absolute x;
		BEGIN
			t[0] := a[r + 3];
			t[1] := a[r + 2];
			t[2] := a[r + 1];
			t[3] := a[r];
			INC(r, 4)
		END (* ReadRealBE *);

		PROCEDURE ReadLongRealLE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: LONGREAL);
			VAR t: packed array [0..7] of BYTE absolute x;
		BEGIN
			t[0] := a[r];
			t[1] := a[r + 1];
			t[2] := a[r + 2];
			t[3] := a[r + 3];
			t[4] := a[r + 4];
			t[5] := a[r + 5];
			t[6] := a[r + 6];
			t[7] := a[r + 7];
			INC(r, 8)
		END (* ReadLongRealLE *);

		PROCEDURE ReadLongRealBE (const a: ARRAY OF BYTE; VAR r: INTEGER; out x: LONGREAL);
			VAR t: packed array [0..7] of BYTE absolute x;
		BEGIN
			t[0] := a[r + 7];
			t[1] := a[r + 6];
			t[2] := a[r + 5];
			t[3] := a[r + 4];
			t[4] := a[r + 3];
			t[5] := a[r + 2];
			t[6] := a[r + 1];
			t[7] := a[r];
			INC(r, 8)
		END (* ReadLongRealBE *);

end (* MemFormatters *).
