unit UDPPorts2;

(*
	А. В. Ширяев, 2011.03, 2013.03, 2015.05, 2016.03
*)

{$OVERFLOWCHECKS ON}
(* {$RANGECHECKS ON} *)

interface

uses WinSock;

const
	maxInfoBytesOut = 65535;
	maxInfoBytesIn = 65535;

type
  BYTE = System.Byte;
  INTEGER = System.Integer;
  Word = System.Word;

	TObrabotka = procedure (id: BYTE; const a: array of BYTE; len: INTEGER);

	TPort = record
		isOpen: Boolean;
		localAdr: String; localPort: Word;
		s: WinSock.TSocket
	end;

procedure Init (var p: TPort; const localAdr: String; localPort: Word);

procedure Open (var p: TPort; var ok: Boolean);

procedure Close (var p: TPort);

procedure OtpravitPosylku (var p: TPort; const remoteAdr: string; remotePort: Word; id: BYTE; const a: array of BYTE; len: INTEGER; var ok: BOOLEAN);

procedure PoluchitPosylki (var p: TPort; h: TObrabotka);

implementation

var
	RRR: Integer; (* для отладки *)

procedure Init (var p: TPort; const localAdr: String; localPort: Word);
begin
	p.isOpen := False;
	p.localAdr := localAdr;
	p.localPort := localPort
end (* Init *);

procedure Open (var p: TPort; var ok: Boolean);
	var
		s: WinSock.TSocket;
		src: WinSock.TSockAddr;
		r: Integer;
begin
	Assert(not p.isOpen);

	ok := False;

	(* create socket *)
	s := WinSock.socket( WinSock.AF_INET, WinSock.SOCK_DGRAM, WinSock.IPPROTO_UDP );
	if s <> WinSock.INVALID_SOCKET then begin
		(* bind socket *)
		src.sin_family := WinSock.PF_INET;
		src.sin_port := WinSock.htons(p.localPort);
		src.sin_addr.S_addr := WinSock.inet_addr(PAnsiChar(p.localAdr));
		FillChar(src.sin_zero, SizeOf(src.sin_zero), 0);
		r := WinSock.bind(s, src, SizeOf(src));

		if r = 0 then begin
			(* set socket to non-blocking *)
			r := 1;
			r := WinSock.ioctlsocket(s, WinSock.FIONBIO, r);

			if r = 0 then begin
				p.s := s;
				p.isOpen := True;

				ok := True
			end else begin
				r := WinSock.closesocket(s)
			end
		end
	end
end (* Open *);

procedure Close (var p: TPort);
	var r: Integer;
begin
	if p.isOpen then begin
		r := WinSock.closesocket(p.s);
		p.isOpen := False
	end
end (* Close *);

procedure OtpravitPosylku (var p: TPort; const remoteAdr: string; remotePort: Word; id: BYTE; const a: array of BYTE; len: INTEGER; var ok: BOOLEAN);
	var dst: WinSock.TSockAddr;
		buf: array [0..1+2+maxInfoBytesOut-1] of Byte;
		r: Integer;
begin
	Assert(len <= maxInfoBytesOut);

	if p.isOpen then begin
		dst.sin_family := WinSock.PF_INET;
		dst.sin_port := WinSock.htons(remotePort);
		dst.sin_addr.S_addr := WinSock.inet_addr(PAnsiChar(remoteAdr));
		FillChar(dst.sin_zero, SizeOf(dst.sin_zero), 0);

		buf[0] := Id;
    buf[1] := len;
		buf[2] := len shr 8;

    if len > 0 then begin Move(a[0], buf[1+2], len) end;

		r := WinSock.sendto(p.s, buf, 1 + 2 + len, 0 (* flags *), dst, SizeOf(dst));
		RRR := r;
		if r = WinSock.SOCKET_ERROR then begin
			ok := False
		end else if r >= 0 then begin
			Assert(r = 1 + 2 + len);
			ok := True
		end else begin
			Assert(False);
			ok := False
		end
	end else begin
		ok := False
	end
end (* OtpravitPosylku *);

procedure PoluchitPosylki (var p: TPort; h: TObrabotka);
	var buf: array [0..65535] of BYTE;
		r, r1: Integer;
    from: WinSock.TSockAddrIn;
    fromLen: Integer;

  procedure DataReceived (const buf: array of Byte; n: Integer);
    var i: Integer; ok: Boolean;

    procedure R (var i: INTEGER; out ok: BOOLEAN);
      var id: BYTE;
        len: System.Word;
		    a: array [0..maxInfoBytesIn-1] of BYTE;
    begin ok := False;
      if i + 3 <= n then begin
        id := buf[i]; INC(i);
        Move(buf[i], len, 2); INC(i, 2);
        if (len <= maxInfoBytesIn) and (i + Integer(len) <= n) then begin
          if len > 0 then begin Move(buf[i], a[0], len); INC(i, len) end;
          h(id, a, len);
          ok := True
        end
      end
    end (* R *);

  begin
    i := 0; repeat R(i, ok) until not ok
  end (* DataReceived *);

begin
	if p.isOpen then begin
		repeat
			r := WinSock.recv(p.s, buf, SizeOf(buf), 0 (* flags *));
      {
        fromLen := SizeOf(from);
        r := WinSock.recvfrom(p.s, buf, SizeOf(buf), 0 (* flags *), from, fromLen);
      }
			RRR := r;
			if r = SOCKET_ERROR then begin
				r1 := System.GetLastError();
				RRR := r1;
				if r1 = WinSock.WSAEWOULDBLOCK then begin
					(* не делать ничего *)
        end else if r1 = WinSock.WSAECONNRESET then begin
          (* ??? Assert(False) FIXME *) (* A. V. Shiryaev, 2013.03.09 *)
				end else begin
					Assert(False)
				end
			end else if r >= 0 then begin
				(* Assert(r >= 1 + 2); *)
				DataReceived(buf, r)
			end else begin
				Assert(False)
			end
		until r <= 0
	end
end (* PoluchitPosylki *);

procedure InitUnit;
	const version = $0202; (* 2.2 *)
	var r: Integer;
		WSAData: WinSock.TWSAData;
begin
	r := WinSock.WSAStartup(version, WSAData);
	Assert(r = 0)
end (* InitUnit *);

procedure CloseUnit;
	var r: Integer;
begin
	r := WinSock.WSACleanup()
end (* CloseUnit *);

initialization
	InitUnit
finalization
	CloseUnit
end (* UDPPorts *).
