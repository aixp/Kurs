unit COMTPorts;

(*
	А. В. Ширяев (Alexander Shiryaev), 2014.11, 2015.09, 2016.04

	Encoding of this file is CP1251

	Обмен сообщениями через COM-порты по протоколу Trimble

	ЗАМЕЧАНИЯ ПО РЕАЛИЗАЦИИ:
		http://msdn.microsoft.com/ru-ru/library/ff802693.aspx
		this is NOT OVERLAPPED I/O implementation
*)

(* {$OVERFLOWCHECKS ON} *)
{$RANGECHECKS ON}

interface

uses Windows, Classes;

const
	DLE = $10; ETX = $03; (* параметры протокола Tribmle *)

	parityNone = Windows.NOPARITY;
	parityOdd = Windows.ODDPARITY;
	parityEven = Windows.EVENPARITY;

	CBR_110 = Windows.CBR_110;
	CBR_300 = Windows.CBR_300;
	CBR_600 = Windows.CBR_600;
	CBR_1200 = Windows.CBR_1200;
	CBR_2400 = Windows.CBR_2400;
	CBR_4800 = Windows.CBR_4800;
	CBR_9600 = Windows.CBR_9600;
	CBR_14400 = Windows.CBR_14400;
	CBR_19200 = Windows.CBR_19200;
	CBR_38400 = Windows.CBR_38400;
	CBR_56000 = Windows.CBR_56000;
	CBR_57600 = Windows.CBR_57600;
	CBR_115200 = Windows.CBR_115200;
	CBR_128000 = Windows.CBR_128000;
	CBR_256000 = Windows.CBR_256000;

	maxInfoBytesIn = 40;
	maxInfoBytesOut = 40;

type
	INTEGER = System.Integer;
	BYTE = System.Byte;
	BOOLEAN = System.Boolean;

	(*
		обработчик принятых сообщений
		входные параметры:
			id: тип содержимого сообщения (идентификатор сообщения)
			a: содержимое сообщения (массив информационных байтов)
			len: длина содержимого сообщения (количество информационных байтов)
	*)
	TReceive = procedure (id: BYTE; const a: array of BYTE; len: INTEGER);

	(* для реализации *)
	TDecoderDesc = record (* состояние автомата дешифрации входного байтпотока *)
		len: INTEGER; (* длина a *)
		a: array [0..maxInfoBytesIn-1] of BYTE;
		id: BYTE; (* идентификатор сообщения *)
		state: INTEGER; (* 0 - вне; 1 - ожидается id; 2 - внутри; 3 - внутри (DLE) *)
	end;

	TPort = class
	private
		(* для переоткрытия (при ошибках ReadFile) *)
		state: INTEGER; (* 0 - закрыт, 1 - открыт *)
		port: string; baud, parity: INTEGER;

		decoder: TDecoderDesc; (* состояние автомата дешифрации входного байтпотока *)

		(* статистика/инфо *)
			readErrors, writeErrors: INTEGER;

		handle: Windows.THandle
	end;

	(* получить список COM-портов *)
	procedure EnumComPorts (const ports: Classes.TStringList);

	(*
		открыть порт
		входные значения:
			port: идентификатор COM-порта (может быть получен процедурой EnumComPorts)
			baud: Windows.CBR_*
			parity: parityNone | parityOdd | parityEven
		выходные значения:
			res = 0: успешно
				p # nil
			res # 0: ошибка при открытии
				p = nil
	*)
	procedure OpenPort (const port: string; baud, parity: INTEGER; out p: TPort; out res: INTEGER);

	(*
		p = nil: не делать ничего
		p # nil: закрыть порт
		выходные значения:
			p = nil (всегда)
	*)
	procedure ClosePort (var p: TPort);

	(*
		p = nil: не делать ничего
		p # nil: получить 0 или более сообщений, вызывая обработчик посылок recv при получении каждого
			замечания:
				сообщения могут быть не получены из-за ошибок ReadFile COM-порта
	*)
	procedure Receive (p: TPort; const recv: TReceive);

	(*
		p = nil: не делать ничего
		p # nil: отправить 1 сообщение
			входные параметры:
				id: тип содержимого сообщения (идентификатор сообщения)
					id # DLE
					id # ETX
				a: содержимое сообщения (массив информационных байтов)
				len: длина содержимого сообщения (количество информационных байтов)
					0 <= len <= maxInfoBytesOut
			замечания:
				не учитывается результат функции WriteFile
	*)
	procedure Send (p: TPort; id: BYTE; const a: array of BYTE; len: INTEGER);

	(*
		p = nil: возвращает 0
		p # nil: возвращает количество ошибок чтения COM-порта
	*)
	function ReadErrors (p: TPort): INTEGER;

	(*
		p = nil: возвращает 0
		p # nil: возвращает количество ошибок записи COM-порта
	*)
	function WriteErrors (p: TPort): INTEGER;

implementation

uses SysUtils, Registry;

const
	inQueue = 8192;
	outQueue = 8192;

var
	Lerr: Windows.DWORD; (* для отладки, должна быть глобальной *)

(* http://stackoverflow.com/a/22091084 *)
procedure EnumComPorts (const ports: Classes.TStringList);
	var i: INTEGER;
begin
	with Registry.TRegistry.Create(Windows.KEY_READ) do
		try RootKey := Windows.HKEY_LOCAL_MACHINE;
			if OpenKey('hardware\devicemap\serialcomm', FALSE) then begin
				try ports.BeginUpdate;
					try GetValueNames(ports);
						for i := ports.Count - 1 downto 0 do begin
							ports.Strings[i] := ReadString(ports.Strings[i])
						end;
						ports.Sort
					finally ports.EndUpdate
					end
				finally CloseKey
				end
			end else begin ports.Clear
			end
		finally Free
		end
end (* EnumComPorts *);

procedure PortOpen (const port: string; baud, parity: INTEGER; var handle: THandle; var res: INTEGER);
	var ok: Windows.BOOL;
		dcb: Windows.TDCB;
		tm: Windows.COMMTIMEOUTS;
begin
	handle := Windows.CreateFile(
		PChar(port),
		Windows.GENERIC_READ or Windows.GENERIC_WRITE,
		0,
		nil,
		Windows.OPEN_EXISTING,
		0 (* Windows.FILE_FLAG_OVERLAPPED *),
		0);
	if handle <> Windows.INVALID_HANDLE_VALUE then begin
		FillChar(dcb, sizeof(dcb), 0);
		dcb.DCBlength := sizeof(dcb);
		if Windows.GetCommState(handle, dcb) then begin
			(* dcb.DCBlength := sizeof(TDCB); *)
			dcb.BaudRate := baud;
			(*
			DWORD fBinary	:1;
			DWORD fParity	:1;
			DWORD fOutxCtsFlow	:1;
			DWORD fOutxDsrFlow	:1;
			DWORD fDtrControl	:2;
			DWORD fDsrSensitivity	:1;
			DWORD fTXContinueOnXoff	:1;
			DWORD fOutX	:1;
			DWORD fInX	:1;
			DWORD fErrorChar	:1;
			DWORD fNull	:1;
			DWORD fRtsControl	:2;
			DWORD fAbortOnError	:1;
			*)
				dcb.Flags := $00000081;
			(* dcb.wReserved := 0; *)
			(* dcb.XonLim := 0; *)
			(* dcb.XoffLim := 0; *)
			dcb.ByteSize := 8;
			dcb.Parity := parity;
			dcb.StopBits := Windows.ONESTOPBIT;
			(* dcb.XonChar := 0; *)
			(* dcb.XoffChar := 0; *)
			(* dcb.ErrorChar := 0; *)
			(* dcb.EofChar := 0; *)
			(* dcb.EvtChar := 0; *)
			(* dcb.wReserved1 := 0; *)
			if Windows.SetCommState(handle, dcb) then begin
				tm.ReadIntervalTimeout := MAXDWORD;
				tm.ReadTotalTimeoutMultiplier := 0;
				tm.ReadTotalTimeoutConstant := 0;
				tm.WriteTotalTimeoutMultiplier := 0;
				tm.WriteTotalTimeoutConstant := 0;
				if Windows.SetCommTimeouts(handle, tm) then begin
					if Windows.SetupComm(handle, inQueue, outQueue) then begin
						if Windows.SetCommMask(handle, 0) then begin
							if Windows.PurgeComm(handle, Windows.PURGE_TXABORT or Windows.PURGE_RXABORT or Windows.PURGE_TXCLEAR or Windows.PURGE_RXCLEAR) then begin
								(* NXP *)
								if Windows.EscapeCommFunction(handle, Windows.CLRRTS) then begin
									if Windows.EscapeCommFunction(handle, Windows.CLRDTR) then begin
										res := 0
									end else begin res := 9
									end
								end else begin res := 8
								end
							end else begin res := 7
							end
						end else begin res := 6
						end
					end else begin res := 5
					end
				end else begin res := 4
				end
			end else begin res := 3
			end
		end else begin res := 2
		end;
		if res <> 0 then begin ok := Windows.CloseHandle(handle)
		end
	end else begin res := 1
	end
end (* PortOpen *);

(* инициализация автомата дешифрации входного байтпотока (сброс) *)
procedure ResetDecoder (var a: TDecoderDesc);
begin
	a.state := 0
end (* ResetDecoder *);

procedure OpenPort (const port: string; baud, parity: INTEGER; out p: TPort; out res: INTEGER);
	var handle: Windows.THandle;
begin
	PortOpen(port, baud, parity, handle, res);
	if res = 0 then begin
		p := TPort.Create();

		p.port := port; p.baud := baud; p.parity := parity;
		p.handle := handle;
		p.readErrors := 0; p.writeErrors := 0;

		ResetDecoder(p.decoder);

		p.state := 1;

		res := 0
	end else begin (* ошибка при открытии *)
		p := nil
	end
end (* OpenPort *);

(* закрыть порт *)
procedure Close0 (p: TPort);
	var res: Windows.BOOL;
begin
	res := Windows.CloseHandle(p.handle)
end (* Close0 *);

procedure ClosePort (var p: TPort);
begin
	if p <> nil then begin
		if p.state = 1 then begin
			Close0(p)
		end;
		p.Destroy;
		p := nil
	end
end (* ClosePort *);

procedure Open0 (p: TPort);
	var res: INTEGER;
begin
	assert(p.state = 0);

	PortOpen(p.port, p.baud, p.parity, p.handle, res);
	if res = 0 then begin
		ResetDecoder(p.decoder);
		p.state := 1
	end
end (* Open0 *);

function DecoderPut (var a: TDecoderDesc; x: BYTE): BOOLEAN;
begin
	Result := FALSE;
	case a.state of
	0: (* вне, ожидать DLE *)
		begin
			if x = DLE then begin
				a.state := 1
			end
		end;
	1: (* ожидать id *)
		begin
			if (x <> DLE) and (x <> ETX) then begin
				a.id := x;
				a.len := 0;
				a.state := 2
			end else begin
				a.state := 0
			end
		end;
	2: (* внутри *)
		begin
			if x <> DLE then begin
				if a.len < maxInfoBytesIn then begin
					a.a[a.len] := x; inc(a.len)
				end else begin (* переполнение (сообщение слишком длинное) *)
					a.state := 0
				end
			end else begin
				a.state := 3
			end
		end;
	3: (* внутри (DLE: ожидать DLE или ETX) *)
		begin
			if x = ETX then begin
				a.state := 0;
				Result := TRUE (* сообщение принято *)
			end else if x = DLE then begin
				if a.len < maxInfoBytesIn then begin
					a.a[a.len] := DLE; inc(a.len);
					a.state := 2
				end else begin (* переполнение (сообщение слишком длинное) *)
					a.state := 0
				end
			end else begin
				a.state := 0
			end
		end;
	else (* ошибка в программе *)
		assert(FALSE);
		a.state := 0
	end
end (* DecoderPut *);

procedure ReadPort (var decoder: TDecoderDesc; handle: Windows.THandle;
		const recv: TReceive;
		out res: INTEGER);
	var ok: Windows.BOOL;
		buf: array [0..inQueue-1] of BYTE;
		i, N: Windows.DWORD;
begin
	ok := Windows.ReadFile(handle, buf, inQueue, N, nil);
	if ok then begin
		if N > 0 then begin
			i := 0;
			while i < N do begin
				if DecoderPut(decoder, buf[i]) then begin
					recv(decoder.id, decoder.a, decoder.len)
				end;
				inc(i)
			end;
			res := 0
		end else if N = 0 then begin
			res := 0
		end else begin (* N < 0 *)
			res := 3
		end
	end else begin
		LErr := Windows.GetLastError();
		if LErr = Windows.ERROR_ACCESS_DENIED then begin
			(* неоднократно наблюдалось в начале 2009 года, причина неизвестна *)
			res := 1
		end else begin
			res := 2
		end
	end
end (* ReadPort *);

procedure Receive (p: TPort; const recv: TReceive);
	var res: INTEGER;
begin
	if p <> nil then begin
		if p.state = 0 then begin
			Open0(p)
		end;
		if p.state = 1 then begin
			ReadPort(p.decoder, p.handle, recv, res);
			if res <> 0 then begin
				inc(p.readErrors);
				Close0(p);
				p.state := 0
			end
		end
	end
end (* Receive *);

procedure Encode (id: BYTE; const a: array of BYTE; len: INTEGER; out e: array of BYTE; out eLen: INTEGER);
	var i: INTEGER;
begin
	e[0] := DLE;
	e[1] := id;
	eLen := 2;
	i := 0;
	while i < len do begin
		e[eLen] := a[i]; INC(eLen);
		if a[i] = DLE then begin
			e[eLen] := DLE; INC(eLen)
		end;
		INC(i)
	end;
	e[eLen] := DLE; INC(eLen);
	e[eLen] := ETX; INC(eLen)
end (* Encode *);

procedure WritePort (p: TPort; id: BYTE; const a: array of BYTE; len: INTEGER; out res: INTEGER);
	var e: array [0..4+2*maxInfoBytesOut-1] of BYTE; eLen, i: INTEGER;
		ok: Windows.BOOL;
		written: Windows.DWORD;
begin
	Encode(id, a, len, e, eLen);

	ok := Windows.WriteFile(p.handle, e, eLen, written, nil);
	if ok then begin
		assert(written = eLen);
		res := 0
	end else begin
		LErr := Windows.GetLastError();
		if LErr = Windows.ERROR_BAD_COMMAND then begin (* такое бывает, если порт отключили *)
			res := 1
		end else if LErr = Windows.ERROR_ACCESS_DENIED then begin (* такое бывает, если порт отключили *)
			res := 2
		end else begin
			(* write error *)
			res := 3;
			assert(FALSE) (* debug *)
		end
	end
end (* WritePort *);

procedure Send (p: TPort; id: BYTE; const a: array of BYTE; len: INTEGER);
	var res: INTEGER;
begin
	assert(id <> DLE);
	assert(id <> ETX);
	assert(len >= 0);
	assert(len <= maxInfoBytesOut);

	if p <> nil then begin
		if p.state = 0 then begin
			Open0(p)
		end;
		if p.state = 1 then begin
			WritePort(p, id, a, len, res);
			if res <> 0 then begin
				INC(p.writeErrors);
				Close0(p);
				p.state := 0
			end
		end
	end
end (* Send *);

function ReadErrors (p: TPort): INTEGER;
begin
	if p <> nil then begin Result := p.readErrors
	end else begin Result := 0
	end
end (* ReadErrors *);

function WriteErrors (p: TPort): INTEGER;
begin
	if p <> nil then begin Result := p.writeErrors
	end else begin Result := 0
	end
end (* WriteErrors *);

initialization
	assert(DLE <> ETX)
end.
