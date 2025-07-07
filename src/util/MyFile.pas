unit MyFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFile = class
  private
    FStart: PChar;
    FCurrent: PChar;
    FLine: Integer;
    FColumn: Integer;
    function GetIsOpen: Boolean;
    function GetIsAtEOF: Boolean;
    class function GetText(const filename: string): PChar; static;
  public
    constructor Create;
    constructor Create(const filename: string);
    destructor Destroy; override;
    procedure Open(const filename: string);
    procedure Close;
    procedure Rewind;
    function Advance: Char;
    function Peek: Char;
    function PeekNext: Char;
    function PeekPrev: Char;
    property IsOpen: Boolean read GetIsOpen;
    property IsAtEOF: Boolean read GetIsAtEOF;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

implementation

constructor TFile.Create;
begin
  FStart := nil;
  FCurrent := nil;
  FLine := 0;
  FColumn := 0;
end;

constructor TFile.Create(const filename: string);
begin
  Create;
  Open(filename);
end;

destructor TFile.Destroy;
begin
  Close;
  inherited Destroy;
end;

class function TFile.GetText(const filename: string): PChar;
var
  stream: TFileStream;
  size: Int64;
begin
  try
    stream := TFileStream.Create(filename, fmOpenRead);
    try
      size := stream.Size;
      GetMem(Result, size + 1);
      stream.Read(Result^, size);
      Result[size] := #0;
    finally
      stream.Free;
    end;
  except
    raise Exception.Create('Couldn''t open file ' + filename);
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

procedure TFile.Open(const filename: string);
begin
  Close;
  FStart := GetText(filename);
  FCurrent := FStart;
  FLine := 1;
  FColumn := 1;
end;

procedure TFile.Close;
begin
  if IsOpen then
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
  if IsOpen then
  begin
    FCurrent := FStart;
    FLine := 1;
    FColumn := 1;
  end;
end;

function TFile.Advance: Char;
begin
  if not IsOpen or IsAtEOF then
    Exit(#0);

  Result := FCurrent^;
  Inc(FCurrent);

  if PeekPrev = #10 then
  begin
    Inc(FLine);
    FColumn := 1;
  end
  else
    Inc(FColumn);
end;

function TFile.Peek: Char;
begin
  if not IsOpen then
    Exit(#0);
  Result := FCurrent^;
end;

function TFile.PeekNext: Char;
begin
  if not IsOpen or IsAtEOF then
    Exit(#0);
  Result := (FCurrent + 1)^;
end;

function TFile.PeekPrev: Char;
begin
  if not IsOpen or (FCurrent = FStart) then
    Exit(#0);
  Result := (FCurrent - 1)^;
end;

end.
