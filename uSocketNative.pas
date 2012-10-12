unit uSocketNative;

{$M+}

interface

uses
  v8napi,
  WinSock,
  SysUtils,
  Windows;

type
  TSocketNative = class(TV8UserObject)
    private
      // properties
      FEndOfLine: AnsiString;
      FTimeout: Integer;
      FHostName: AnsiString;
      FPort: Integer;

      // private vars
      FWSAData: TWSAData;
      FHost: TSockAddrIn;
      FSock: TSocket;
      FBuff: array [1..1024] of AnsiChar;
      FConnected: Boolean;

      procedure Error(s: AnsiString);
      function Resolve: Integer;
      function GetLastError: AnsiString;
    public
      constructor Create; override;
      destructor Destroy; override;

      function ConnectFunc(RetValue: PV8Variant; Params: PV8ParamArray; const ParamCount: Integer; var v8: TV8AddInDefBase): Boolean;
      function DisconnectFunc(RetValue: PV8Variant; Params: PV8ParamArray; const ParamCount: Integer; var v8: TV8AddInDefBase): Boolean;
      function SendDataFunc(RetValue: PV8Variant; Params: PV8ParamArray; const ParamCount: Integer; var v8: TV8AddInDefBase): Boolean;
      function RecvDataFunc(RetValue: PV8Variant; Params: PV8ParamArray; const ParamCount: Integer; var v8: TV8AddInDefBase): Boolean;

      function PropertyEndOfLineGetSet(propValue: PV8Variant; Get: Boolean; var v8: TV8AddInDefBase): Boolean;
      function PropertyTimeoutGetSet(propValue: PV8Variant; Get: Boolean; var v8: TV8AddInDefBase): Boolean;
      function PropertyHostNameGetSet(propValue: PV8Variant; Get: Boolean; var v8: TV8AddInDefBase): Boolean;
      function PropertyPortGetSet(propValue: PV8Variant; Get: Boolean; var v8: TV8AddInDefBase): Boolean;
    published
      property EndOfLine: AnsiString read FEndOfLine write FEndOfLine;
      property Timeout: Integer read FTimeout write FTimeout;
      property HostName: AnsiString read FHostName write FHostName;
      property Port: Integer read FPort write FPort;
  end;

implementation

{ resolve helper functions }

var
  resolve_hostname: AnsiString;
  resolve_result: Cardinal;
  resolve_ok: Boolean;

procedure resolve_thread(Param: Integer);
var
  he: PHostEnt;
begin
  resolve_ok := False;

  resolve_result := inet_addr(PAnsiChar(resolve_hostname));
  if resolve_result = INADDR_NONE then
  begin
    he := gethostbyname(PAnsiChar(resolve_hostname));
    if he = nil then
    begin
      resolve_ok := True;
      exit;
    end;
    resolve_result := PInteger(he^.h_addr_list^)^;
  end;
  resolve_ok := True;
end;

function resolve_with_timeout(const HostName: AnsiString; Timeout: Integer): Cardinal;
var
  i: Integer;
  h1: Cardinal;
begin
  resolve_hostname := HostName;
  CreateThread(nil, 128, @resolve_thread, nil, 0, h1);
  for i := 1 to Timeout * 100 do
  begin
    sleep(10);
    if resolve_ok then
      break;
  end;

  if resolve_ok then
  begin
    Result := resolve_result;
  end else begin
    TerminateThread(h1, 0);
    Result := INADDR_NONE;
  end
end;

{ send helper functions }

var
  send_sock: TSocket;
  send_string: AnsiString;
  send_result: Integer;
  send_ok: Boolean;

procedure send_thread(Param: Integer);
var
  i: Integer;
begin
  send_ok := False;
  send_result := send(send_sock, send_string[1], Length(send_string), 0);

  if send_result = SOCKET_ERROR then
  begin
    if WSAGetLastError() = WSAEMSGSIZE then
    begin
      for i := 1 to Length(send_string) do
      begin
        send_result := send(send_sock, send_string[i], 1, 0);
        if send_result = SOCKET_ERROR then
          Break;
      end;
    end;
  end;

  send_ok := True;
end;

function send_with_timeout(s: TSocket; str: AnsiString; Timeout: Integer): Integer;
var
  h1: Cardinal;
  i: Integer;
begin
  send_sock := s;
  send_string := str;
  CreateThread(nil, 128, @send_thread, nil, 0, h1);
  for i := 1 to Timeout*100 do
  begin
    sleep(10);
    if send_ok then
      break;
  end;
  if send_ok = False then
    TerminateThread(h1, 0);
  Result := send_result;
end;

{ recv helper functions }

function recv_IsDataReady(sock: TSocket): Boolean;
var
  lArg: u_long;
  szBuf: array [1..10] of Char;
  n: Integer;
begin
  lArg := 1;
  ioctlsocket(sock, FIONBIO, lArg);
  n := recv(sock, szBuf, 1, MSG_PEEK);
  Result := True;
  if n = SOCKET_ERROR then
    if WSAGetLastError() = WSAEWOULDBLOCK then
      Result := False;

  lArg := 0;
  ioctlsocket(sock, FIONBIO, lArg);
end;

{ TSocketNative }

constructor TSocketNative.Create;
begin
  inherited;
  FConnected := False;
  FEndOfLine := #13#10;
end;

destructor TSocketNative.Destroy;
begin
  closesocket(FSock);
  WSACleanup;
  inherited;
end;

function TSocketNative.ConnectFunc(RetValue: PV8Variant; Params: PV8ParamArray;
  const ParamCount: Integer; var v8: TV8AddInDefBase): Boolean;
var
  res: Cardinal;
begin
  if WSAStartup(65535, FWSAData) <> 0 then
    Error('Ошибка функции WSAStartup()');

  FSock := socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
  if FSock = INVALID_SOCKET then
    Error('Ошибка функции socket()');

  // устанавливаем хост и порт сервера
  res := Resolve;
  if not resolve_ok then
    Error('Ошибка определения IP-адреса узла');

  FHost.sin_family := AF_INET;
  FHost.sin_port := htons(FPort);
  FHost.sin_addr.S_addr := res;

  // подключаемся к серверу
  if Winsock.connect(FSock, FHost, SizeOf(FHost)) > 0 then
    Error('Ошибка подключения к серверу');

  FConnected := True;

  Result := True;
end;

function TSocketNative.DisconnectFunc(RetValue: PV8Variant;
  Params: PV8ParamArray; const ParamCount: Integer;
  var v8: TV8AddInDefBase): Boolean;
begin
  if not FConnected then
    Error('Попытка закрыть сокет без подключения к серверу');

  FConnected := False;
  if closesocket(FSock) <> 0 then
    Error('Ошибка закрытия сокета');

  if WSACleanup = SOCKET_ERROR then
    Error('Ошибка завершения работы сокета');

  Result := True;
end;

procedure TSocketNative.Error(s: AnsiString);
var
  msg: AnsiString;
begin
  msg := GetLastError;

  closesocket(FSock);
  WSACleanup;

  raise EZeroDivide.Create(string(s + ': ' + msg));
end;

function TSocketNative.GetLastError: AnsiString;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAGetLastError;

  case ErrorCode of
    0: begin
          Result := '';
          Exit;
       end;
    WSAEINTR: {10004}
      Result := 'Interrupted system call';
    WSAEBADF: {10009}
      Result := 'Bad file number';
    WSAEACCES: {10013}
      Result := 'Permission denied';
    WSAEFAULT: {10014}
      Result := 'Bad address';
    WSAEINVAL: {10022}
      Result := 'Invalid argument';
    WSAEMFILE: {10024}
      Result := 'Too many open files';
    WSAEWOULDBLOCK: {10035}
      Result := 'Operation would block';
    WSAEINPROGRESS: {10036}
      Result := 'Operation now in progress';
    WSAEALREADY: {10037}
      Result := 'Operation already in progress';
    WSAENOTSOCK: {10038}
      Result := 'Socket operation on nonsocket';
    WSAEDESTADDRREQ: {10039}
      Result := 'Destination address required';
    WSAEMSGSIZE: {10040}
      Result := 'Message too long';
    WSAEPROTOTYPE: {10041}
      Result := 'Protocol wrong type for Socket';
    WSAENOPROTOOPT: {10042}
      Result := 'Protocol not available';
    WSAEPROTONOSUPPORT: {10043}
      Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT: {10044}
      Result := 'Socket not supported';
    WSAEOPNOTSUPP: {10045}
      Result := 'Operation not supported on Socket';
    WSAEPFNOSUPPORT: {10046}
      Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT: {10047}
      Result := 'Address family not supported';
    WSAEADDRINUSE: {10048}
      Result := 'Address already in use';
    WSAEADDRNOTAVAIL: {10049}
      Result := 'Can''t assign requested address';
    WSAENETDOWN: {10050}
      Result := 'Network is down';
    WSAENETUNREACH: {10051}
      Result := 'Network is unreachable';
    WSAENETRESET: {10052}
      Result := 'Network dropped connection on reset';
    WSAECONNABORTED: {10053}
      Result := 'Software caused connection abort';
    WSAECONNRESET: {10054}
      Result := 'Connection reset by peer';
    WSAENOBUFS: {10055}
      Result := 'No Buffer space available';
    WSAEISCONN: {10056}
      Result := 'Socket is already connected';
    WSAENOTCONN: {10057}
      Result := 'Socket is not connected';
    WSAESHUTDOWN: {10058}
      Result := 'Can''t send after Socket shutdown';
    WSAETOOMANYREFS: {10059}
      Result := 'Too many references:can''t splice';
    WSAETIMEDOUT: {10060}
      Result := 'Connection timed out';
    WSAECONNREFUSED: {10061}
      Result := 'Connection refused';
    WSAELOOP: {10062}
      Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG: {10063}
      Result := 'File name is too long';
    WSAEHOSTDOWN: {10064}
      Result := 'Host is down';
    WSAEHOSTUNREACH: {10065}
      Result := 'No route to host';
    WSAENOTEMPTY: {10066}
      Result := 'Directory is not empty';
    WSAEPROCLIM: {10067}
      Result := 'Too many processes';
    WSAEUSERS: {10068}
      Result := 'Too many users';
    WSAEDQUOT: {10069}
      Result := 'Disk quota exceeded';
    WSAESTALE: {10070}
      Result := 'Stale NFS file handle';
    WSAEREMOTE: {10071}
      Result := 'Too many levels of remote in path';
    WSASYSNOTREADY: {10091}
      Result := 'Network subsystem is unusable';
    WSAVERNOTSUPPORTED: {10092}
      Result := 'Winsock DLL cannot support this application';
    WSANOTINITIALISED: {10093}
      Result := 'Winsock not initialized';
    WSAEDISCON: {10101}
      Result := 'Disconnect';
    WSAHOST_NOT_FOUND: {11001}
      Result := 'Host not found';
    WSATRY_AGAIN: {11002}
      Result := 'Non authoritative - host not found';
    WSANO_RECOVERY: {11003}
      Result := 'Non recoverable error';
    WSANO_DATA: {11004}
      Result := 'Valid name, no data record of requested type'
  else
    Result := 'Other Winsock error (' + AnsiString(IntToStr(ErrorCode)) + ')';
  end;

  Result:='('+AnsiString(IntToStr(ErrorCode))+') '+Result;
end;

function TSocketNative.PropertyEndOfLineGetSet(propValue: PV8Variant;
  Get: Boolean; var v8: TV8AddInDefBase): Boolean;
begin
  if Get then
    V8SetString(propValue, FEndOfLine)
  else
    FEndOfLine := V8AsAString(propValue);

  Result := True;
end;

function TSocketNative.PropertyHostNameGetSet(propValue: PV8Variant;
  Get: Boolean; var v8: TV8AddInDefBase): Boolean;
begin
  if Get then
    V8SetString(propValue, FHostName)
  else
    FHostName := V8AsAString(propValue);

  Result := True;
end;

function TSocketNative.PropertyPortGetSet(propValue: PV8Variant; Get: Boolean;
  var v8: TV8AddInDefBase): Boolean;
begin
  if Get then
    V8SetInt(propValue, FPort)
  else
    FPort := V8AsInt(propValue);

  Result := True;
end;

function TSocketNative.PropertyTimeoutGetSet(propValue: PV8Variant;
  Get: Boolean; var v8: TV8AddInDefBase): Boolean;
begin
  if Get then
    V8SetInt(propValue, FTimeout)
  else
    FTimeout := V8AsInt(propValue);

  Result := True;
end;

function TSocketNative.RecvDataFunc(RetValue: PV8Variant; Params: PV8ParamArray;
  const ParamCount: Integer; var v8: TV8AddInDefBase): Boolean;
var
  res: Integer;
begin
  if not FConnected then
    Error('Попытка принять данные без подключения к серверу');

  res := recv(FSock, FBuff, SizeOf(FBuff)-1, 0);

  if res = SOCKET_ERROR then
  begin
    Error('Ошибка приёма данных');
    FBuff[1] := #0;
  end else begin
    FBuff[res+1] := #0;
  end;

  V8SetString(RetValue, AnsiString(PAnsiChar(@FBuff[1])));

  closesocket(FSock);

  Result := True;
end;

function TSocketNative.Resolve: Integer;
begin
  if FTimeout = 0 then
    FTimeOut := 10;
  Result := resolve_with_timeout(FHostName, FTimeout);
  if not resolve_ok then
    Error('Превышено время ожидания при определении IP-адреса узла: ' + FHostName);
end;

function TSocketNative.SendDataFunc(RetValue: PV8Variant; Params: PV8ParamArray;
  const ParamCount: Integer; var v8: TV8AddInDefBase): Boolean;
var
  res: Integer;
  send_string: AnsiString;
  s: AnsiString;
begin
  s := V8AsAString(@Params[1]);

  if not FConnected then
    Error('Попытка послать данные без подключения к серверу');

//  send_string := s + Chr(0);
  send_string := s;

  res := send(FSock, send_string[1], length(send_string), 0);
  if res = SOCKET_ERROR then
    V8SetString(RetValue, 'Ошибка отправки запроса')
  else begin
    res := recv(FSock, FBuff, SizeOf(FBuff)-1, 0);

    if res = SOCKET_ERROR then
    begin
      Error('Ошибка приёма данных');
      FBuff[1] := #0;
      V8SetString(RetValue, 'Ошибка получения');
    end else begin
      FBuff[res+1] := #0;
      V8SetString(RetValue, AnsiString(PAnsiChar(@FBuff[1])));
    end;
  end;

  closesocket(FSock);

  Result := True;
end;

end.
