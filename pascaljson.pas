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
  JStype = (  JS_NONE, JS_NUMBER, JS_STRING, JS_BOOL, JS_OBJECT, JS_ARRAY );
  JSset  = set of JStype;

const
  JS_SIMPLE:  JSset = [JS_NUMBER, JS_STRING, JS_BOOL];
  JS_COMPLEX: JSset = [JS_OBJECT, JS_ARRAY];

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
 Procedure jsonBegin(SomeType: JStype; Name: String);
 Procedure jsonEnd;
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



 PDataBlockJSON = ^TDataBlockJSON;
 PBlockJSON     = ^TBlockJSON;

 TDataBlockJSON = packed record
   numData:   PDouble;
   strData:   PString;
   boolData:  PBoolean;
 end;

 TBlockJSON = packed record
   BlockType: JStype;
   Name:      String;
   Data:      PDataBlockJSON;
   Parent:    PBlockJSON;
   ObjEnd:    Boolean;
   Level:     Integer;
 end;

  { TPascalJSON }

  TPascalJSON = class
    private
      Item:      array of PBlockJSON;
      jsRecords: array of PBlockJSON;
      fCount, Current: Integer;
      Procedure SetCount(Value: Integer);
      Function  GetCountRec: Integer;
      Procedure SetCountRec(Value: Integer);
      Function  GetLevel(i: Integer): Integer;
      Procedure Change(Value: PDataBlockJSON; OldType, NewType: JStype); Overload;
      Procedure Clear(Value: PDataBlockJSON; OldType: JStype); Overload;
      Function  TabLevel(Level: Integer): String;
      Function  toString(Value: PDataBlockJSON): String; Overload;
    protected
      Procedure Clear(i: Integer); Overload;
      Procedure Change(i: Integer; SomeType: JStype); Overload;
      Function  toString(i: Integer): String; Overload;
      Procedure AddBind(i: Integer);
      property  Count:    Integer read fCount      write SetCount;
      property  CountRec: Integer read GetCountRec write SetCountRec;

      //Procedure writeJSimpleType(Value: JSimpleType);
      Procedure writeJStype(Value: JStype);
      Procedure writeSimpleBlockJSON(Value: TDataBlockJSON);
      Procedure writeMainBlockJSON(Value: TBlockJSON);
      Procedure writeBlockJSON(Value: TBlockJSON);

      //Procedure logJSimpleType(Value: JSimpleType);
      Procedure logJStype(Value: JStype);
      Procedure logSimpleBlockJSON(Value: TDataBlockJSON);
      Procedure logMainBlockJSON(Value: TBlockJSON);
      Procedure logBlockJSON(Value: TBlockJSON);
      Procedure logAll;
    public
      Constructor Create;
      Destructor  Destroy;
      Procedure   Clear; Overload;
      Procedure   toRoot;
      Procedure   jsonBegin(SomeType: JStype; Name: String);
      Procedure   jsonEnd;
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

function JSON_LoadFromFile(FileName: String): String;
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

procedure jsonBegin(SomeType: JStype; Name: String);
begin
 jsonMain.jsonBegin(SomeType, Name);
end;

procedure jsonEnd;
begin
 jsonMain.jsonEnd;
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
 OldCount := fCount;
 if Value < 0 then Value := 0;
 fCount := Value;
 if OldCount < Value then
 begin
   SetLength(Item, fCount);
   for i := OldCount to Value - 1 do
   begin
     new(Item[i]);
     Item[i]^.BlockType := JS_NONE;
     Item[i]^.ObjEnd := FALSE;
   end;
 end;
 if OldCount > Value then
 begin
   for i := Value to OldCount - 1 do
   begin
     Clear(i);
     dispose(Item[i]);
   end;
   SetLength(Item, fCount);
 end;
end;

function TPascalJSON.GetCountRec: Integer;
begin
 Result := Length(jsRecords);
end;

procedure TPascalJSON.SetCountRec(Value: Integer);
begin
 SetLength(jsRecords, Value);
end;

function TPascalJSON.GetLevel(i: Integer): Integer;
var
 dlevel: integer;
begin
  if i < 0 then
  begin
   Result := 0;
   Exit;
  end;
  if i <= 1 then
  begin
   Result := 1;
   Exit;
  end;
  dlevel := 0;
  if Item[i-1]^.BlockType in JS_COMPLEX then inc(dlevel);
  if Item[i-1]^.ObjEnd then dec(dlevel);
  Result := Item[i-1]^.Level + dlevel;
end;

procedure TPascalJSON.Change(Value: PDataBlockJSON; OldType, NewType: JStype);
begin
 case OldType of
   JS_NUMBER: dispose(Value^.numData);
   JS_STRING: dispose(Value^.strData);
   JS_BOOL:   dispose(Value^.boolData);
 end;
 case NewType of
   JS_NUMBER: new(Value^.numData);
   JS_STRING: new(Value^.strData);
   JS_BOOL:   new(Value^.boolData);
 end;
end;

procedure TPascalJSON.Clear(Value: PDataBlockJSON; OldType: JStype);
begin
 Change(Value, OldType, JS_NONE);
end;

function TPascalJSON.TabLevel(Level: Integer): String;
var
 i: Integer;
begin
 Result := '';
 for i := 0 to Level do
   Result += #9;
end;

function TPascalJSON.toString(Value: PDataBlockJSON): String;
begin
 //case Value^.BlockType of
 // JS_NUMBER: Result := FloatToStr(Value^.numData^);
 // JS_STRING: Result := '"' + Value^.strData^ + '"';
 // JS_BOOL:   if Value^.boolData^ then Result := 'true'
 //                                 else Result := 'false';
 //end;
end;

procedure TPascalJSON.Clear(i: Integer);
begin
 with Item[i]^ do
 begin
  if BlockType in JS_SIMPLE then
  begin
   Clear(   Data, BlockType );
   dispose( Data );
  end;
  Name      := '';
  BlockType := JS_NONE;
  ObjEnd    := FALSE;
  Level     := 0;
  Parent    := nil;
 end;
end;

//TBlockJSON = packed record
//  BlockType: JStype;
//  Name:      String;
//  Data:      PDataBlockJSON;
//  Parent:    PBlockJSON;
//  ObjEnd:    Boolean;
//  Level:     Integer;
//end;

procedure TPascalJSON.Change(i: Integer; SomeType: JStype);
begin
 Clear(i);
 with Item[i]^ do
 begin
  if SomeType in JS_SIMPLE then
    New( Data );
  BlockType := SomeType;
  Level     := GetLevel(i);
 end;
end;

function TPascalJSON.toString(i: Integer): String;
var
 TabLev: String;
begin
 //TabLev := TabLevel(Item[i].Parent^.Level);
 //with Item[i] do
 //if Name <> '' then
 //case BlockType of
 // JS_SIMPLE: Result := TabLev + '"' + Name + '":' + toString( Item[i].Data ) + #13#10;
 // JS_OBJECT: Result := TabLev + '"' + Name + '": {' + #13#10;
 // JS_ARRAY:  Result := TabLev + '"' + Name + '": [' + #13#10;
 //end else
 //case BlockType of
 // JS_SIMPLE: Result := toString( Item[i].Data ) + #13#10;
 // JS_OBJECT: Result := TabLev + '{' + #13#10;
 // JS_ARRAY:  Result := TabLev + '[' + #13#10;
 //end
end;

procedure TPascalJSON.AddBind(i: Integer);
begin
 CountRec := CountRec + 1;
 jsRecords[CountRec - 1] := Item[i];
end;

//procedure TPascalJSON.writeJSimpleType(Value: JSimpleType);
//begin
// Case Value of
//  JS_NONE:   Console.Write('JSimpleType', 'JS_NONE');
//  JS_NUMBER: Console.Write('JSimpleType', 'JS_NUMBER');
//  JS_STRING: Console.Write('JSimpleType', 'JS_STRING');
//  JS_BOOL:   Console.Write('JSimpleType', 'JS_BOOL');
// end;
//end;

procedure TPascalJSON.writeJStype(Value: JStype);
begin
 //Case Value of
 // JS_NONE:   Console.Write('JStype', 'JS_NONE');
 // JS_SIMPLE: Console.Write('JStype', 'JS_SIMPLE');
 // JS_OBJECT: Console.Write('JStype', 'JS_OBJECT');
 // JS_ARRAY:  Console.Write('JStype', 'JS_ARRAY');
 //end;
end;

procedure TPascalJSON.writeSimpleBlockJSON(Value: TDataBlockJSON);
begin
 //writeJSimpleType(Value.BlockType);
 //Case Value.BlockType of
 // JS_NUMBER: Console.Write('Value', Value.numData^);
 // JS_STRING: Console.Write('Value', Value.strData^);
 // JS_BOOL:   Console.Write('Value', Value.boolData^);
 //end;
end;

procedure TPascalJSON.writeMainBlockJSON(Value: TBlockJSON);
begin
 Console.Write('Name', Value.Name);
 writeJStype(Value.BlockType);
 if (Value.Parent <> nil) and (Value.BlockType <> JS_NONE) then
 begin
  Console.Write('Parent', Value.Parent^.Name);
  Console.ToFile;
  //Console.Write('Parent', Value.Parent^.Name);

 end;
end;

procedure TPascalJSON.writeBlockJSON(Value: TBlockJSON);
begin
 //writeMainBlockJSON(Value);
 //Case Value.BlockType of
 // JS_SIMPLE:           writeSimpleBlockJSON(Value.Data^);
 // JS_OBJECT, JS_ARRAY:;
 //end;
end;

procedure TPascalJSON.logJStype(Value: JStype);
begin
 writeJStype(Value);
 Console.Log;
end;

procedure TPascalJSON.logSimpleBlockJSON(Value: TDataBlockJSON);
begin
 writeSimpleBlockJSON(Value);
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
 //for i := 0 to Count - 1 do
 //begin
 //  Console.Write(i);
 //  Console.Write;
 //  logBlockJSON(Item[i]);
 //end;
 //Console.FinishWork;
end;

constructor TPascalJSON.Create;
begin
 Clear;
end;

destructor TPascalJSON.Destroy;
begin
 Current := 0;
 Count   := 0;
end;

procedure TPascalJSON.Clear;
begin
 Count    := 1;
 CountRec := 1;
 Change(0, JS_OBJECT);
 Item[0]^.Name := 'root';
 Item[0]^.Level := 1;
 jsRecords[0] := Item[0];
 toRoot;
end;

procedure TPascalJSON.toRoot;
begin
 Current := 0;
end;

procedure TPascalJSON.jsonBegin(SomeType: JStype; Name: String);
var
 Position: Integer;
begin
 if not SomeType in JS_COMPLEX then Exit;
 Position := Count;
 Count := Position + 1;
 Change(Position, SomeType);
 Item[Position]^.Name := Name;
 AddBind(Position);
 //inc(Current);   Подумай над этим!!
end;

procedure TPascalJSON.jsonEnd;
begin
 Item[Count - 1]^.ObjEnd := TRUE;
end;

procedure TPascalJSON.Write(Name, Value: String);
const
 BlockType = JS_STRING;
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_SIMPLE);
 Change(Item[Position].Data, BlockType);
 Item[Position].Data^.strData^ := Value;
 case Current^.BlockType of
  JS_OBJECT: Item[Position].Name := Name;
  JS_ARRAY:  Item[Position].Name := '';
 end;
 Item[Position].Parent := Current;
end;

procedure TPascalJSON.Write(Name: String; Value: Double);
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_SIMPLE);
 Change(Item[Position].Data, JS_NUMBER);
 Item[Position].Data^.numData^ := Value;
 case Current^.BlockType of
  JS_OBJECT: Item[Position].Name := Name;
  JS_ARRAY:  Item[Position].Name := '';
 end;
 Item[Position].Parent := Current;
end;

procedure TPascalJSON.Write(Name: String; Value: Integer);
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_SIMPLE);
 Change(Item[Position].Data, JS_NUMBER);
 Item[Position].Data^.numData^ := Value;
 case Current^.BlockType of
  JS_OBJECT: Item[Position].Name := Name;
  JS_ARRAY:  Item[Position].Name := '';
 end;
 Item[Position].Parent := Current;
end;

procedure TPascalJSON.Write(Name: String; Value: Boolean);
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_SIMPLE);
 Change(Item[Position].Data, JS_BOOL);
 Item[Position].Data^.boolData^ := Value;
 case Current^.BlockType of
  JS_OBJECT: Item[Position].Name := Name;
  JS_ARRAY:  Item[Position].Name := '';
 end;
 Item[Position].Parent := Current;
end;

procedure TPascalJSON.WriteObj(Name: String);
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_OBJECT);
 Item[Position].Name := Name;
 //Bind(Current, @Item[Position]);
end;

procedure TPascalJSON.WriteArr(Name: String);
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_ARRAY);
 Item[Position].Name := Name;
 //Bind(Current, @Item[Position]);
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
 //Reset;
 if Count > 1 then
 for i := 1 to Count - 1 do
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

