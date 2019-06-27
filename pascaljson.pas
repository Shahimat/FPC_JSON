(*===========================================================================*)
(*                                                                           *)
(*                         The MIT License (MIT)                             *)
(*                                                                           *)
(*       Copyright (c) 2019 Nakhapetyan Gevorg <ngs22071993@gmail.com>       *)
(*     Simple and fast JSON converter to Object Pascal internal structure    *)
(*                                                                           *)
(*===========================================================================*)
unit PascalJSON;

{$mode objfpc}{$H+}

interface

type

  JStype = (JS_NONE, JS_NUMBER, JS_STRING, JS_BOOL, JS_OBJECT, JS_ARRAY);

  Function JSON_LoadFromFile(FileName: String): String;
  //Function JSON_ValidationCheck(Text: String): Boolean;


implementation

uses SysUtils;

type

  { TSimpleBlockJSON }

  TSimpleBlockJSON = record
    BlockType:  JStype;
    Data, Name: String;
  end;

  { TBlockJSON }

  PBlockJSON = ^TBlockJSON;
  TBlockJSON = class
    private
      Item:   array of TSimpleBlockJSON;
      Child:  array of TBlockJSON;
      Name:   String;
      BlockType: JStype;
      Parent:    PBlockJSON;
      Procedure SetCountSB(Value: Integer);
      Function  GetCountSB: Integer;
      Procedure SetCountB(Value: Integer);
      Function  GetCountB: Integer;
      Procedure AddSimpleBlock(bName, bData: String; bBlockType: JStype);
      Procedure AddBlock(bName, bData: String; bBlockType: JStype);
    protected
      property Count:      Integer read GetCountSB write SetCountSB;
      property CountBlock: Integer read GetCountB  write SetCountB;
    public
      Destructor Destroy;
      Procedure  Clear;
      Procedure  Add(bName, bData: String; bBlockType: JStype);
  end;

var
 Root:    TBlockJSON;
 Current: PBlockJSON;

Function isJSONexpansion( FileName: String ): Boolean;
var
 a, i: Integer;
 s: String;
begin
 Result := false;
 s := '';
 if FileName = '' then Exit;
 For i := Length(FileName) downto 1 do
 begin
  if FileName[i] = '.' then
  begin
   if (length(s) <> 4) or (i = 1) then Exit;
   if  (s[1] in ['j','J']) and (s[2] in ['s','S'])
   and (s[3] in ['o','O']) and (s[4] in ['n','N']) then Result := True;
   Exit;
  end;
  s := FileName[i] + s;
  if length(s) > 4 then Exit;
 end;
end;

Function Compress(Text: String): String;
var
 i:       Integer;
 Bracket: Boolean;
begin
 Result := '';
 Bracket := False;
 for i := 1 to Length(Text) do
 begin
  if Text[i] = '"' then Bracket := not Bracket;
  if (Text[i] in [' ', #9, #13, #10]) and (not Bracket) then Continue;
  Result += Text[i];
 end;
end;

Function CompressDefinitions(Text: String): String;
var
 i: integer;
 Quotes: Boolean;
begin
 Result := '';
 Quotes := False;
 for i := 1 to Length(Text) do
 case Text[i] of
  '"':
    begin
      Quotes := not Quotes;
      if not Quotes then Result += '#';
    end;
  //'0'..'9','.','-','e','E':
  //  begin
  //   if Result[Length(Result)] = '$' then
  //   if Text[i] = '-' then Exit
  //                    else Continue;
  //   if Text[i] in ['e','E'] then
  //   begin
  //
  //   end;
  //   Result += '$';
  //  end
  else if not Quotes then Result += Text[i];
 end;
end;

function JSON_ValidationCheck(Text: String): Boolean;
var
 i: integer;
 numBraces, numSqrBrackets: Integer;
 Quotes, Colon: Boolean;
begin
 // Необходимо донастроить, включая проблемы с сиюминутным сжатием вопрос решить
 Result := True;
 Exit;

 Text := CompressDefinitions(Text);
 Result := False;
 Quotes := False;
 Colon  := False;
 numBraces := 0;
 numSqrBrackets := 0;
 if Text = '' then Exit;
 if (Text[1] <> '{') or (Text[Length(Text)] <> '}') then Exit;
 for i := 2 to Length(Text) - 1 do
 case Text[i] of
  '"': Quotes := not Quotes;
  '{': if not Quotes then inc(numBraces);
  '}': if not Quotes then dec(numBraces);
  '[': if not Quotes then inc(numSqrBrackets);
  ']': if not Quotes then dec(numSqrBrackets);
  ',': if not Quotes then
    begin
     if Text[i + 1] in ['}',']'] then Exit;
     Colon := False;
    end;
  ':': if not Quotes then
    begin
     if not Colon then Colon := True
                  else Exit;
     if Text[i - 1] <> '"' then Exit;
    end;
 end;
 if (numBraces <> 0) or (numSqrBrackets <> 0) or (Quotes) then Exit;
 Result := True;
end;

Function JSON_LoadFromFile(FileName: String): String;
var
 F:       TextFile;
 Text, s: String;
begin
 if not isJSONexpansion(FileName) then
 begin
  Result := 'error: the file extension is incorrect';
  Exit;
 end;
 if not FileExists( FileName ) then
 begin
  Result := 'error: file not found';
  Exit;
 end;
 Text := '';
 AssignFile(F, FileName);
 Reset(F);
 while not EOF(F) do
 begin
  ReadLn(F, s);
  Text += s;
 end;
 if not JSON_ValidationCheck(Text) then
 begin
  Result := 'error: downloaded text file does not match JSON format';
  Exit;
 end;
 Text := Compress( Text );
 Result := 'JSON format loaded from ' + FileName;
 Result := Text;
 CloseFile(F);
end;

{ TBlockJSON }

procedure TBlockJSON.SetCountSB(Value: Integer);
var
 i, OldCount: Integer;
begin
 OldCount := Length( Item );
 SetLength(Item, Value);
 if OldCount < Value then
 for i := OldCount to Value do
 begin
  Item[i].Data      := '';
  Item[i].BlockType := JS_NONE;
 end;
end;

function TBlockJSON.GetCountSB: Integer;
begin
 Result := Length( Item );
end;

procedure TBlockJSON.SetCountB(Value: Integer);
var
 i, OldCount: Integer;
begin
 OldCount := Length( Child );
 if Value < 0 then Value := 0;
 if OldCount < Value then
 begin
   SetLength( Child, Value );
   for i := OldCount to Value - 1 do
     Child[i] := TBlockJSON.Create;
 end;
 if OldCount > Value then
 begin
   for i := Value to OldCount - 1 do
     Child[i].Destroy;
   SetLength( Child, Value );
 end;
end;

function TBlockJSON.GetCountB: Integer;
begin
 Result := Length( Child );
end;

procedure TBlockJSON.AddSimpleBlock(bName, bData: String; bBlockType: JStype);
begin
 Count := Count + 1;
 with Item[Count - 1] do
 begin
  Name := bName;
  Data := bData;
  BlockType := bBlockType;
 end;
end;

procedure TBlockJSON.AddBlock(bName, bData: String; bBlockType: JStype);
begin
 CountBlock := CountBlock + 1;
end;

destructor TBlockJSON.Destroy;
begin
 Clear;
end;

procedure TBlockJSON.Clear;
begin
 CountBlock := 0;
 Count      := 0;
 Name       := '';
end;

procedure TBlockJSON.Add(bName, bData: String; bBlockType: JStype);
begin
 //case BlockType of
 //end;
 //
end;

initialization
 Root      := TBlockJSON.Create;
 Root.Name := 'root';
 Current   := @Root;

finalization
 Current := nil;
 Root.Destroy;

end.

