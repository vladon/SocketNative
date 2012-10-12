library SocketNative;

uses
  System.SysUtils,
  System.Classes,
  v8napi,
  uSocketNative in 'uSocketNative.pas';

{$R *.res}
{$M+}

begin
  with ClassRegList.RegisterClass(TSocketNative, 'SocketNative', 'TSocketNative') do
  begin
    AddFunc('Connect', 'Подключиться', @TSocketNative.ConnectFunc, 0);
    AddFunc('Disconnect', 'Отключиться', @TSocketNative.DisconnectFunc, 0);
    AddFunc('SendData', 'Послать', @TSocketNative.SendDataFunc, 1);
    AddFunc('RecvData', 'Принять', @TSocketNative.RecvDataFunc, 0);

    AddProp('EndOfLine', 'КонецСтроки', True, True, @TSocketNative.PropertyEndOfLineGetSet);
    AddProp('Timeout', 'Таймаут', True, True, @TSocketNative.PropertyTimeoutGetSet);
    AddProp('HostName', 'Сервер', True, True, @TSocketNative.PropertyHostNameGetSet);
    AddProp('Port', 'Порт', True, True, @TSocketNative.PropertyPortGetSet);
  end;
end.
