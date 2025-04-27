unit MyFile;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TFile = class
  private
    FStart: PAnsiChar;
    FCurrent: PAnsiChar;
    FLine: Integer;
    FColumn: Integer;
    function GetIsOpen: Boolean;
    function GetIsAtEOF: Boolean;
    class function LoadText(const FileName: string): PAnsiChar; static;
  public
    constructor Create; overload;
    constructor Create(const FileName: string); overload;
    destructor Destroy; override;
    procedure Open(const FileName: string);
    procedure Close;
    procedure Rewind;
    function Advance: AnsiChar;
    function Peek: AnsiChar;
    function PeekNext: AnsiChar;
    function PeekPrev: AnsiChar;
    property IsOpen: Boolean read GetIsOpen;
    property IsAtEOF: Boolean read GetIsAtEOF;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

implementation

constructor TFile.Create;
begin
  inherited Create;
  FStart := nil;
  FCurrent := nil;
  FLine := 0;
  FColumn := 0;
end;

constructor TFile.Create(const FileName: string);
begin
  Create;
  Open(FileName);
end;

destructor TFile.Destroy;
begin
  Close;
  inherited Destroy;
end;

class function TFile.LoadText(const FileName: string): PAnsiChar;
var
  FS: TFileStream;
  Size: Int64;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Size := FS.Size;
    GetMem(Result, Size + 1);
    FS.ReadBuffer(Result^, Size);
    Result[Size] := #0;
  finally
    FS.Free;
  end;
end;

function TFile.GetIsOpen: Boolean;
begin
  Result := FStart <> nil;
end;

function TFile.GetIsAtEOF: Boolean;
begin
  Result := (FCurrent <> nil) and (FCurrent^ = #0);
end;

procedure TFile.Open(const FileName: string);
begin
  if GetIsOpen then
    Close;
  FStart := LoadText(FileName);
  FCurrent := FStart;
  FLine := 1;
  FColumn := 1;
end;

procedure TFile.Close;
begin
  if GetIsOpen then
  begin
    FreeMem(FStart);
    FStart := nil;
    FCurrent := nil;
    FLine := 0;
    FColumn := 0;
  end;
end;

procedure TFile.Rewind;
begin
  if GetIsOpen then
  begin
    FCurrent := FStart;
    FLine := 1;
    FColumn := 1;
  end;
end;

function TFile.Advance: AnsiChar;
begin
  if not GetIsOpen or GetIsAtEOF then
    Exit(#0);
  Inc(FCurrent);
  Result := FCurrent^;
  if PeekPrev = #10 then
  begin
    Inc(FLine);
    FColumn := 1;
  end
  else
    Inc(FColumn);
end;

function TFile.Peek: AnsiChar;
begin
  if not GetIsOpen then
    Exit(#0);
  Result := FCurrent^;
end;

function TFile.PeekNext: AnsiChar;
begin
  if not GetIsOpen or (FCurrent^ = #0) then
    Exit(#0);
  Result := (FCurrent + 1)^;
end;

function TFile.PeekPrev: AnsiChar;
begin
  if not GetIsOpen or (FCurrent = FStart) then
    Exit(#0);
  Result := (FCurrent - 1)^;
end;

end.

