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

 Procedure jsonInterface(var jsonNumber: PDouble; var jsonBool: PBoolean;
   var jsonString, jsonType: PString; var jsonError: PBoolean);
 Function JSON_LoadFromFile(FileName: String): String;
 //Function JSON_ValidationCheck(Text: String): Boolean;

 Procedure jsonWrite(Name, Value: String); Overload;
 Procedure jsonWrite(Name: String; Value: Double); Overload;
 Procedure jsonWrite(Name: String; Value: Integer); Overload;
 Procedure jsonWrite(Name: String; Value: Boolean); Overload;
 Procedure jsonWriteObj(Name: String);
 Procedure jsonWriteArr(Name: String);
 Procedure jsonEndObj;
 Procedure jsonAddObj(Name: String);
 Procedure jsonAddArr(Name: String);
 Procedure jsonToChild;
 Procedure jsonToParent;
 Procedure jsonToRoot;
 //Function  jsonFind(Name: String; ElemType: JStype): Integer;
 Procedure jsonRead(Text: String);
 Procedure jsonCD(Text: String);
 Procedure jsonSaveToFile(FileName: String);
 Procedure jsonClear;
 Function  jsonInfo: String;
 Function  jsonPath: String;
 Function  jsonGetPath(Path: String): String;

 Function  jsonString: String;


implementation

uses SysUtils, debugger;

type

 JSsimpleType = (JSS_NONE, JSS_NUMBER, JSS_STRING, JSS_BOOL);
 JStype = (JS_NONE, JS_SIMPLE, JS_OBJECT, JS_ARRAY);

 PSimpleBlockJSON = ^TSimpleBlockJSON;
 PRecordBlockJSON = ^TRecordBlockJSON;
 PBlockJSON       = ^TBlockJSON;

 TSimpleBlockJSON = packed record
   BlockType: JSsimpleType;
   numData:   PDouble;
   strData:   PString;
   boolData:  PBoolean;
 end;

 TRecordBlockJSON = record
   CurrentPos: Integer;
   Data:       Array of PBlockJSON;
 end;

 TBlockJSON = record
   BlockType:   JStype;
   Name:        String;
   BlockSimple: PSimpleBlockJSON;
   BlockRecord: PRecordBlockJSON;
   Parent:      PBlockJSON;
   ObjEnd:      Boolean;
 end;

  { TPascalJSON }

  TPascalJSON = class
    private
      Item: array of TBlockJSON;
      Level: Integer;
      Current: PBlockJSON;
      Procedure SetCount(Value: Integer);
      Function  GetCount: Integer;
      Procedure Change(Value: PSimpleBlockJSON; SomeType: JSsimpleType); Overload;
      Procedure Clear(Value: PRecordBlockJSON); Overload;
      Procedure Clear(Value: PSimpleBlockJSON); Overload;
      Procedure AddChild(Parent, Child: PBlockJSON);
      Function  TabLevel: String;
      Function  toString(Value: PSimpleBlockJSON): String; Overload;
    protected
      Procedure Clear(i: Integer); Overload;
      Procedure Change(i: Integer; SomeType: JStype); Overload;
      Function  toString(i: Integer): String; Overload;
      Procedure Bind(Parent, Child: PBlockJSON);
      property  Count: Integer read GetCount write SetCount;

      Procedure writeJSsimpleType(Value: JSsimpleType);
      Procedure writeJStype(Value: JStype);
      Procedure writeSimpleBlockJSON(Value: TSimpleBlockJSON);
      Procedure writeRecordBlockJSON(Value: TRecordBlockJSON);
      Procedure writeMainBlockJSON(Value: TBlockJSON);
      Procedure writeBlockJSON(Value: TBlockJSON);

      Procedure logJSsimpleType(Value: JSsimpleType);
      Procedure logJStype(Value: JStype);
      Procedure logSimpleBlockJSON(Value: TSimpleBlockJSON);
      Procedure logRecordBlockJSON(Value: TRecordBlockJSON);
      Procedure logMainBlockJSON(Value: TBlockJSON);
      Procedure logBlockJSON(Value: TBlockJSON);
      Procedure logAll;
    public
      Constructor Create;
      Destructor  Destroy;
      Procedure   Clear; Overload;
      Procedure   Reset;
      Procedure   toRoot;
      Procedure   toChild;
      Procedure   toParent;
      Procedure   Write(Name, Value: String); Overload;
      Procedure   Write(Name: String; Value: Double);  Overload;
      Procedure   Write(Name: String; Value: Integer); Overload;
      Procedure   Write(Name: String; Value: Boolean); Overload;
      Procedure   WriteObj(Name: String);
      Procedure   WriteArr(Name: String);
      Procedure   EndObj;
      Function    toString: String; Overload;
  end;

var
 jsonMain: TPascalJSON;

{$MACRO ON}

{$DEFINE WRITE_NAME_VALUE :=
begin
 Write(Name);
 WriteDef;
 Write;
 Write(Value);
 WriteEnd;
 Write;
end;}

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

Procedure Analys(Text: String);
var
 i: Integer;
 Quotes, isObject: Boolean;
 S: String;
begin
 Quotes := False;
 isObject := True;
 S := '';
 for i := 2 to Length(Text) - 1 do
 case Text[i] of
  '"': Quotes := not Quotes;
  '{': if not Quotes then;
  '}': if not Quotes then;
  '[': if not Quotes then;
  ']': if not Quotes then;
  ',': if not Quotes then;
  ':': if not Quotes then
    case Text[i + 1] of
      '{':;
      '[':;
      '"':;
      '0'..'9':;
      else;
    end;
  else S += Text[i];
 end;

end;

procedure jsonInterface(var jsonNumber: PDouble; var jsonBool: PBoolean;
  var jsonString, jsonType: PString; var jsonError: PBoolean);
begin

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
 Analys( Text );
 Result := 'JSON format loaded from ' + FileName;
 Result := Text;
 CloseFile(F);
end;

procedure jsonWrite(Name, Value: String);
begin
 jsonMain.Write(Name, Value);
end;

procedure jsonWrite(Name: String; Value: Double);
begin
 jsonMain.Write(Name, Value);
end;

procedure jsonWrite(Name: String; Value: Integer);
begin
 jsonMain.Write(Name, Value);
end;

procedure jsonWrite(Name: String; Value: Boolean);
begin
 jsonMain.Write(Name, Value);
end;

procedure jsonWriteObj(Name: String);
begin
 jsonMain.WriteObj(Name);
end;

procedure jsonWriteArr(Name: String);
begin
 jsonMain.WriteArr(Name);
end;

procedure jsonEndObj;
begin
 jsonMain.EndObj;
end;

procedure jsonAddObj(Name: String);
begin

end;

procedure jsonAddArr(Name: String);
begin

end;

procedure jsonToChild;
begin
 jsonMain.toChild;
end;

procedure jsonToParent;
begin
 jsonMain.toParent;
end;

procedure jsonToRoot;
begin
 jsonMain.toRoot;
end;

//function jsonFind(Name: String; ElemType: JStype): Integer;
//begin
//
//end;

procedure jsonRead(Text: String);
begin

end;

procedure jsonCD(Text: String);
begin

end;

procedure jsonSaveToFile(FileName: String);
begin

end;

procedure jsonClear;
begin

end;

function jsonInfo: String;
begin

end;

function jsonPath: String;
begin

end;

function jsonGetPath(Path: String): String;
begin

end;

function jsonString: String;
begin
 Result := jsonMain.toString;
end;

{ TPascalJSON }

procedure TPascalJSON.SetCount(Value: Integer);
var
 i, OldCount: Integer;
begin
 OldCount := Length( Item );
 if Value < 0 then Value := 0;
 if OldCount < Value then
 begin
   SetLength( Item, Value );
   for i := OldCount to Value - 1 do
   begin
     Item[i].BlockType := JS_NONE;
     Item[i].ObjEnd := FALSE;
   end;
 end;
 if OldCount > Value then
 begin
   for i := Value to OldCount - 1 do
     Clear(i);
   SetLength( Item, Value );
 end;
end;

function TPascalJSON.GetCount: Integer;
begin
 Result := Length( Item );
end;

procedure TPascalJSON.Change(Value: PSimpleBlockJSON; SomeType: JSsimpleType
  );
begin
 case Value^.BlockType of
   JSS_NUMBER: dispose(Value^.numData);
   JSS_STRING: dispose(Value^.strData);
   JSS_BOOL:   dispose(Value^.boolData);
 end;
 case SomeType of
   JSS_NUMBER: new(Value^.numData);
   JSS_STRING: new(Value^.strData);
   JSS_BOOL:   new(Value^.boolData);
 end;
 Value^.BlockType := SomeType;
end;

procedure TPascalJSON.Clear(Value: PRecordBlockJSON);
begin
 SetLength(Value^.Data, 0);
end;

procedure TPascalJSON.Clear(Value: PSimpleBlockJSON);
begin
 Change(Value, JSS_NONE);
end;

procedure TPascalJSON.AddChild(Parent, Child: PBlockJSON);
begin
 with Parent^.BlockRecord^ do
 begin
  SetLength(Data, Length(Data) + 1);
  Data[Length(Data) - 1] := Child;
 end;
end;

function TPascalJSON.TabLevel: String;
var
 i: Integer;
begin
 Result := '';
 for i := 0 to Level do
   Result += #9;
end;

function TPascalJSON.toString(Value: PSimpleBlockJSON): String;
begin
 case Value^.BlockType of
  JSS_NUMBER: Result := FloatToStr(Value^.numData^);
  JSS_STRING: Result := '"' + Value^.strData^ + '"';
  JSS_BOOL:   if Value^.boolData^ then Result := 'true'
                                  else Result := 'false';
 end;
end;

procedure TPascalJSON.Clear(i: Integer);
begin
 with Item[i] do
 begin
  case BlockType of
   JS_SIMPLE:
     begin
      Clear(   BlockSimple );
      dispose( BlockSimple );
     end;
   JS_OBJECT, JS_ARRAY:
     begin
      Clear(   BlockRecord );
      dispose( BlockRecord );
     end;
  end;
  Name := '';
  BlockType := JS_NONE;
  ObjEnd := FALSE;
 end;
end;

procedure TPascalJSON.Change(i: Integer; SomeType: JStype);
begin
 Clear(i);
 case SomeType of
  JS_SIMPLE:           New( Item[i].BlockSimple );
  JS_OBJECT, JS_ARRAY:
    begin
      New( Item[i].BlockRecord );
      Item[i].BlockRecord^.CurrentPos := 0;
    end;
 end;
 Item[i].BlockType := SomeType;
end;

function TPascalJSON.toString(i: Integer): String;
begin
 with Item[i] do
 if Name <> '' then
 case BlockType of
  JS_SIMPLE: Result := '"' + Name + '":' + toString( Item[i].BlockSimple );
  JS_OBJECT: Result := '"' + Name + '": {';
  JS_ARRAY:  Result := '"' + Name + '": [';
 end else
 case BlockType of
  JS_SIMPLE: Result := toString( Item[i].BlockSimple );
  JS_OBJECT: Result := '{';
  JS_ARRAY:  Result := '[';
 end
end;

procedure TPascalJSON.Bind(Parent, Child: PBlockJSON);
begin
  Console.Write('Bind');
  Console.Write;
  logBlockJSON(Parent^);
  AddChild(Parent, Child);
  Child^.Parent := Parent;
end;

procedure TPascalJSON.writeJSsimpleType(Value: JSsimpleType);
begin
 Case Value of
  JSS_NONE:   Console.Write('JSsimpleType', 'JSS_NONE');
  JSS_NUMBER: Console.Write('JSsimpleType', 'JSS_NUMBER');
  JSS_STRING: Console.Write('JSsimpleType', 'JSS_STRING');
  JSS_BOOL:   Console.Write('JSsimpleType', 'JSS_BOOL');
 end;
end;

procedure TPascalJSON.writeJStype(Value: JStype);
begin
 Case Value of
  JS_NONE:   Console.Write('JStype', 'JS_NONE');
  JS_SIMPLE: Console.Write('JStype', 'JS_SIMPLE');
  JS_OBJECT: Console.Write('JStype', 'JS_OBJECT');
  JS_ARRAY:  Console.Write('JStype', 'JS_ARRAY');
 end;
end;

procedure TPascalJSON.writeSimpleBlockJSON(Value: TSimpleBlockJSON);
begin
 writeJSsimpleType(Value.BlockType);
 Case Value.BlockType of
  JSS_NUMBER: Console.Write('Value', Value.numData^);
  JSS_STRING: Console.Write('Value', Value.strData^);
  JSS_BOOL:   Console.Write('Value', Value.boolData^);
 end;
end;

procedure TPascalJSON.writeRecordBlockJSON(Value: TRecordBlockJSON);
var
 i: Integer;
begin
  Console.Log('CurrentPos', Value.CurrentPos);
  for i := Low(Value.Data) to High(Value.Data) do
  begin
    Console.Write;
    logMainBlockJSON(Value.Data[i]^);
  end;
end;

procedure TPascalJSON.writeMainBlockJSON(Value: TBlockJSON);
begin
 Console.Write('Name', Value.Name);
 writeJStype(Value.BlockType);
 if Value.Parent <> nil then
 Console.Write('Parent', Value.Parent^.Name);
end;

procedure TPascalJSON.writeBlockJSON(Value: TBlockJSON);
begin
 writeMainBlockJSON(Value);
 Case Value.BlockType of
  JS_SIMPLE:           writeSimpleBlockJSON(Value.BlockSimple^);
  JS_OBJECT, JS_ARRAY: writeRecordBlockJSON(Value.BlockRecord^);
 end;
end;

procedure TPascalJSON.logJSsimpleType(Value: JSsimpleType);
begin
 writeJSsimpleType(Value);
 Console.Log;
end;

procedure TPascalJSON.logJStype(Value: JStype);
begin
 writeJStype(Value);
 Console.Log;
end;

procedure TPascalJSON.logSimpleBlockJSON(Value: TSimpleBlockJSON);
begin
 writeSimpleBlockJSON(Value);
 Console.Log;
end;

procedure TPascalJSON.logRecordBlockJSON(Value: TRecordBlockJSON);
begin
 writeRecordBlockJSON(Value);
 Console.Log;
end;

procedure TPascalJSON.logMainBlockJSON(Value: TBlockJSON);
begin
 writeMainBlockJSON(Value);
 Console.Log;
end;

procedure TPascalJSON.logBlockJSON(Value: TBlockJSON);
begin
 writeBlockJSON(Value);
 Console.Log;
end;

procedure TPascalJSON.logAll;
var
 i: Integer;
begin
 for i := 0 to Length(Item) - 1 do
 begin
   Console.Write(i);
   Console.Write;
   logBlockJSON(Item[i]);
 end;
 Console.FinishWork;
end;

constructor TPascalJSON.Create;
begin
 Clear;
end;

destructor TPascalJSON.Destroy;
begin
 Current := nil;
 Count   := 0;
end;

procedure TPascalJSON.Clear;
begin
 Count := 1;
 Change(0, JS_OBJECT);
 Item[0].Name := 'root';
 toRoot;
end;

procedure TPascalJSON.Reset;
var
 i: Integer;
begin
 for i := 0 to Count - 1 do
 if Item[i].BlockType in [JS_OBJECT, JS_ARRAY] then
   Item[i].BlockRecord^.CurrentPos := 0;
 toRoot;
end;

procedure TPascalJSON.toRoot;
begin
 Current := @Item[0];
 Level   := 0;
end;

procedure TPascalJSON.toChild;
begin
 logAll;
 with Current^.BlockRecord^ do
 if CurrentPos <= Length(Data) then
 begin
  inc(CurrentPos);
  inc(Level);
  Current := Data[CurrentPos - 1];
 end;
end;

procedure TPascalJSON.toParent;
begin
 if Current^.Name <> 'root' then
 begin
  Current := Current^.Parent;
  dec(Level);
 end;
end;

procedure TPascalJSON.Write(Name, Value: String);
const
 BlockType = JSS_STRING;
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_SIMPLE);
 Change(Item[Position].BlockSimple, BlockType);
 Item[Position].BlockSimple^.strData^ := Value;
 case Current^.BlockType of
  JS_OBJECT: Item[Position].Name := Name;
  JS_ARRAY:  Item[Position].Name := '';
 end;
 Bind(Current, @Item[Position]);
end;

procedure TPascalJSON.Write(Name: String; Value: Double);
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_SIMPLE);
 Change(Item[Position].BlockSimple, JSS_NUMBER);
 Item[Position].BlockSimple^.numData^ := Value;
 case Current^.BlockType of
  JS_OBJECT: Item[Position].Name := Name;
  JS_ARRAY:  Item[Position].Name := '';
 end;
 Bind(Current, @Item[Position]);
end;

procedure TPascalJSON.Write(Name: String; Value: Integer);
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_SIMPLE);
 Change(Item[Position].BlockSimple, JSS_NUMBER);
 Item[Position].BlockSimple^.numData^ := Value;
 case Current^.BlockType of
  JS_OBJECT: Item[Position].Name := Name;
  JS_ARRAY:  Item[Position].Name := '';
 end;
 Bind(Current, @Item[Position]);
end;

procedure TPascalJSON.Write(Name: String; Value: Boolean);
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_SIMPLE);
 Change(Item[Position].BlockSimple, JSS_BOOL);
 Item[Position].BlockSimple^.boolData^ := Value;
 case Current^.BlockType of
  JS_OBJECT: Item[Position].Name := Name;
  JS_ARRAY:  Item[Position].Name := '';
 end;
 Bind(Current, @Item[Position]);
end;

procedure TPascalJSON.WriteObj(Name: String);
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_OBJECT);
 Item[Position].Name := Name;
 Bind(Current, @Item[Position]);
end;

procedure TPascalJSON.WriteArr(Name: String);
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_ARRAY);
 Item[Position].Name := Name;
 Bind(Current, @Item[Position]);
end;

procedure TPascalJSON.EndObj;
begin
 Item[Count - 1].ObjEnd := TRUE;
end;

function TPascalJSON.toString: String;
var
 i: Integer;
begin
 Result := '{' + #13#10;
 Reset;
 if Count > 0 then
 for i := 0 to Count - 1 do
 begin
  Result += toString(i);
 end;
 Result += '}'
end;

initialization
 jsonMain  := TPascalJSON.Create;

finalization
 jsonMain.Destroy;

{$MACRO OFF}

end.

