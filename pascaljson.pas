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

 //Function JSON_LoadFromFile(FileName: String): String;
 //Function JSON_ValidationCheck(Text: String): Boolean;

 Procedure jsonWrite(Name, Value: String); Overload;
 Procedure jsonWrite(Name: String; Value: Double); Overload;
 Procedure jsonWrite(Name: String; Value: Integer); Overload;
 Procedure jsonWrite(Name: String; Value: Boolean); Overload;
 Procedure jsonWrite(Value: String); Overload;
 Procedure jsonWrite(Value: Double); Overload;
 Procedure jsonWrite(Value: Integer); Overload;
 Procedure jsonWrite(Value: Boolean); Overload;
 Procedure jsonBegin(SomeType: JStype; Name: String = '');
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

 JSCmessage = (  JSC_RIGHT, JSC_ERROR, JSC_BRACET_NUM, JSC_FILE_EXTENSION,
   JSC_FILE_FOUND, JSC_VALID_JSON, JSC_VALID_EXTEN_LEN, JSC_VALID_EXTEN_NAME);

 JSlexeme = (JSL_LETTER, JSL_DIGIT, JSL_QUOTES, JSL_DEFINITION, JSL_OPEN_BRACE,
   JSL_CLOSE_BRACE, JSL_OPEN_BRACKET, JSL_CLOSE_BRACKET, JSL_COMMA, JSL_POINT,
   JSL_BACKSLASH, JSL_SPACE, JSL_ENTER, JSL_TAB, JSL_OVER);

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
   Level:     Integer;
 end;

  { TPascalJSON }

  TPascalJSON = class
    private
      Item: array of PBlockJSON;
      fCount, Current: Integer;
      Procedure SetCount(Value: Integer);
      Procedure Change(Value: PDataBlockJSON; OldType, NewType: JStype); Overload;
      Procedure Clear(Value: PDataBlockJSON; OldType: JStype); Overload;
    protected
      (*Parser procedure's and function's*)
       Function Lexeme(c: Char): JSlexeme;

      (*Data*)
      Function  GetLevel(i: Integer): Integer;
      Function  TabLevel(Level: Integer): String;
      Procedure BindParent(i: Integer);
      Procedure Clear(i: Integer); Overload;
      Procedure Change(i: Integer; SomeType: JStype); Overload;
      Procedure SetData(i: Integer; Value: String); Overload;
      Procedure SetData(i: Integer; Value: Double); Overload;
      Procedure SetData(i: Integer; Value: Integer); Overload;
      Procedure SetData(i: Integer; Value: Boolean); Overload;

      (*Checks*)
      Function  ConverterMessage(Code: JSCmessage): String;
      Function  objValidationCheck: JSCmessage;
      Function  jsonValidationCheck(Text: String): JSCmessage;
      Function  jsonExpansion( FileName: String ): JSCmessage;

      (*Over*)
      Function  toString(i: Integer): String; Overload;
      Function  LoadFromFile(FileName: String; var jsonText: String): JSCmessage;
      Function  SaveToFile(FileName, Text: String): JSCmessage; Overload;
      property  Count: Integer read fCount write SetCount;

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
      Function    SaveToFile(FileName: String): JSCmessage; Overload;
      Procedure   Parse(Text: String);
      Procedure   Write(Name, Value: String); Overload;
      Procedure   Write(Name: String; Value: Double);  Overload;
      Procedure   Write(Name: String; Value: Integer); Overload;
      Procedure   Write(Name: String; Value: Boolean); Overload;
      Function    toString: String; Overload;
  end;

var
 jsonMain: TPascalJSON;

{$MACRO ON}

{$DEFINE JSONWRITE_NAME_VALUE := begin jsonMain.Write(Name, Value); end;}
{$DEFINE JSONWRITE_VALUE := begin jsonMain.Write('', Value); end;}

{$DEFINE WRITE_NAME_VALUE :=
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, BlockType);
 SetData(Position, Value);
 BindParent(Position);
 if Item[position]^.Parent^.BlockType = JS_OBJECT then
    Item[Position]^.Name := Name;
end;}

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

procedure jsonWrite(Name,         Value: String);  JSONWRITE_NAME_VALUE
procedure jsonWrite(Name: String; Value: Double);  JSONWRITE_NAME_VALUE
procedure jsonWrite(Name: String; Value: Integer); JSONWRITE_NAME_VALUE
procedure jsonWrite(Name: String; Value: Boolean); JSONWRITE_NAME_VALUE
procedure jsonWrite(Value: String);  JSONWRITE_VALUE
procedure jsonWrite(Value: Double);  JSONWRITE_VALUE
procedure jsonWrite(Value: Integer); JSONWRITE_VALUE
procedure jsonWrite(Value: Boolean); JSONWRITE_VALUE

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
 jsonMain.SaveToFile(FileName);
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

function TPascalJSON.GetLevel(i: Integer): Integer;
var
 dlevel: integer;
begin
 if i <= 0 then
 begin
  Result := 0;
  Exit;
 end;
 dlevel := 0;
 if Item[i - 1]^.BlockType in JS_COMPLEX then inc(dlevel);
 if Item[i]^.BlockType  =  JS_NONE       then dec(dlevel);
 Result := Item[i - 1]^.Level + dlevel;
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

{$DEFINE corr := :Result:=}
function TPascalJSON.Lexeme(c: Char): JSlexeme;
begin
 case c of
  'a'..'z','A'..'Z','_' corr JSL_LETTER;
  '0'..'9' corr JSL_DIGIT;
  '.'      corr JSL_POINT;
  ','      corr JSL_COMMA;
  '{'      corr JSL_OPEN_BRACE;
  '}'      corr JSL_CLOSE_BRACE;
  '['      corr JSL_OPEN_BRACKET;
  ']'      corr JSL_CLOSE_BRACKET;
  '"'      corr JSL_QUOTES;
  ':'      corr JSL_DEFINITION;
  '\'      corr JSL_BACKSLASH;
  ' '      corr JSL_SPACE;
  #13,#10  corr JSL_ENTER;
  #9       corr JSL_TAB;
  else
   Result := JSL_OVER;
 end;
end;

function TPascalJSON.TabLevel(Level: Integer): String;
var
 i: Integer;
begin
 Result := '';
 for i := 0 to Level - 1 do
   Result += #9;
end;

procedure TPascalJSON.BindParent(i: Integer);
var
 ds, j: Integer;
begin
  if i <= 1 then
  begin
   Item[i]^.Parent := Item[0];
   Exit;
  end;
  if Item[i - 1]^.BlockType in JS_SIMPLE then
  begin
   Item[i]^.Parent := Item[i - 1]^.Parent;
   Exit;
  end;
  ds := 1;
  for j := i - 1 downto 0 do
  begin
   if  Item[j]^.BlockType =  JS_NONE then begin inc(ds); Continue; end;
   if  Item[j]^.BlockType in JS_COMPLEX then
   begin
    dec(ds);
    if ds = 0 then
    begin
     Item[i]^.Parent := Item[j];
     Exit;
    end;
   end;
  end;
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
  begin
   New( Data );
   Change(Data, JS_NONE, SomeType);
  end;
  BlockType := SomeType;
  Level     := GetLevel(i);
 end;
end;

procedure TPascalJSON.SetData(i: Integer; Value: String);
begin
 Item[i]^.Data^.strData^  := Value;
end;

procedure TPascalJSON.SetData(i: Integer; Value: Double);
begin
 Item[i]^.Data^.numData^  := Value;
end;

procedure TPascalJSON.SetData(i: Integer; Value: Integer);
begin
 Item[i]^.Data^.numData^  := Value;
end;

procedure TPascalJSON.SetData(i: Integer; Value: Boolean);
begin
 Item[i]^.Data^.boolData^ := Value;
end;

function TPascalJSON.ConverterMessage(Code: JSCmessage): String;
begin
 case Code of
   JSC_RIGHT:            Result := 'right';
   JSC_ERROR:            Result := 'unknown error';
   JSC_BRACET_NUM:       Result := 'different number of brackets';
   JSC_FILE_EXTENSION:   Result := 'the file extension is incorrect';
   JSC_FILE_FOUND:       Result := 'file not found';
   JSC_VALID_JSON:       Result := 'text file does not match JSON format';
   JSC_VALID_EXTEN_LEN:  Result := 'extension length is not equal to format length';
   JSC_VALID_EXTEN_NAME: Result := 'extension name is not "JSON"';
  else
   Result := 'unknown error';
 end;
 if Code <> JSC_RIGHT then Result := 'Error: ' + Result;
end;

function TPascalJSON.objValidationCheck: JSCmessage;
var
 i, ds: Integer;
begin
 ds := 0;
 for i := 0 to Count - 1 do
 with Item[i]^ do
 begin
  if BlockType in JS_COMPLEX then inc(ds);
  if BlockType =  JS_NONE    then dec(ds);
 end;
 if ds <> 0 then
 begin
  Result := JSC_BRACET_NUM;
  Exit;
 end;
 Result := JSC_RIGHT;
end;

function TPascalJSON.jsonValidationCheck(Text: String): JSCmessage;
var
 i: integer;
 numBraces, numSqrBrackets: Integer;
 Quotes, Colon: Boolean;
begin
 // Необходимо донастроить, включая проблемы с сиюминутным сжатием вопрос решить
 Result := JSC_RIGHT;
 Exit;

 Text := CompressDefinitions(Text);
 //Result := False;
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
 Result := JSC_RIGHT;
end;

function TPascalJSON.toString(i: Integer): String;
begin
 with Item[i]^ do
 begin
  case BlockType of
   JS_STRING: Result := '"' + Data^.strData^ + '"';
   JS_NUMBER: Result := FloatToStr(Data^.numData^);
   JS_BOOL: if Data^.boolData^ then Result := 'true'
                               else Result := 'false';
   JS_OBJECT: Result := '{';
   JS_ARRAY:  Result := '[';
   JS_NONE:
    case Parent^.BlockType of
     JS_OBJECT: Result := '}';
     JS_ARRAY:  Result := ']';
    end;
  end;
  if (Name <> '') and (i > 0) and (BlockType <> JS_NONE) then
    Result := '"' + Name + '": ' + Result;
  if i < Count - 1 then
  if (Item[i + 1]^.BlockType <> JS_NONE) and not (BlockType in JS_COMPLEX) then
    Result := Result + ',';
  Result := TabLevel(Level) + Result + #13#10;
 end;
end;

function TPascalJSON.jsonExpansion(FileName: String): JSCmessage;
var
 i: Integer;
 s: String;
begin
 Result := JSC_ERROR;
 s := '';
 if FileName = '' then Exit;
 For i := Length(FileName) downto 1 do
 begin
  if FileName[i] = '.' then
  begin
   if (length(s) <> 4) or (i = 1) then Exit;
   if  (s[1] in ['j','J']) and (s[2] in ['s','S'])
   and (s[3] in ['o','O']) and (s[4] in ['n','N']) then Result := JSC_RIGHT
                                                   else Result := JSC_VALID_EXTEN_NAME;
   Exit;
  end;
  s := FileName[i] + s;
  if length(s) > 4 then
  begin
   Result := JSC_VALID_EXTEN_LEN;
   Exit;
  end;
 end;
end;

function TPascalJSON.LoadFromFile(FileName: String; var jsonText: String
  ): JSCmessage;
var
 F: TextFile;
 s: String;
begin
 Result := jsonExpansion(FileName);
 if Result <> JSC_RIGHT then Exit;
 if not FileExists( FileName ) then
 begin
  Result := JSC_FILE_FOUND;
  Exit;
 end;
 try
  jsonText := '';
  AssignFile(F, FileName);
  Reset(F);
  while not EOF(F) do
  begin
   ReadLn(F, s);
   jsonText += s;
  end;
  Result := jsonValidationCheck(jsonText);
  if Result <> JSC_RIGHT then Exit;
  Result := JSC_RIGHT;
 finally
  CloseFile(F);
 end;
end;

function TPascalJSON.SaveToFile(FileName, Text: String): JSCmessage;
var
 F: TextFile;
begin
 Result := jsonExpansion(FileName);
 if Result <> JSC_RIGHT then Exit;
 Result := objValidationCheck;
 if Result <> JSC_RIGHT then Exit;
 if FileExists( FileName ) then DeleteFile(FileName);
 try
  AssignFile(F, FileName);
  Rewrite(F);
  WriteLn(F, Text);
  Result := JSC_RIGHT;
 finally
  CloseFile(F);
 end;
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
 Clear;
end;

procedure TPascalJSON.Clear;
begin
 Count := 0;
end;

procedure TPascalJSON.toRoot;
begin
 Current := 0;
end;

procedure TPascalJSON.jsonBegin(SomeType: JStype; Name: String);
var
 Position: Integer;
begin
 if not (SomeType in JS_COMPLEX) then Exit;
 Position := Count;
 Count := Position + 1;
 Change(Position, SomeType);
 Item[Position]^.Name := Name;
 BindParent(Position);
end;

procedure TPascalJSON.jsonEnd;
var
 Position: Integer;
begin
 Position := Count;
 Count := Position + 1;
 Change(Position, JS_NONE);
 BindParent(Position);
end;

function TPascalJSON.SaveToFile(FileName: String): JSCmessage;
begin
 Result := SaveToFile(FileName, toString);
end;

procedure TPascalJSON.Parse(Text: String);
begin
 Clear;

end;

procedure TPascalJSON.Write(Name, Value: String);
const BlockType = JS_STRING; WRITE_NAME_VALUE

procedure TPascalJSON.Write(Name: String; Value: Double);
const BlockType = JS_NUMBER; WRITE_NAME_VALUE

procedure TPascalJSON.Write(Name: String; Value: Integer);
const BlockType = JS_NUMBER; WRITE_NAME_VALUE

procedure TPascalJSON.Write(Name: String; Value: Boolean);
const BlockType = JS_BOOL;   WRITE_NAME_VALUE

function TPascalJSON.toString: String;
var
 i: Integer;
 msg: JSCmessage;
begin
 msg := objValidationCheck;
 if msg <> JSC_RIGHT then
 begin
  Result := ConverterMessage(msg);
  Exit;
 end;
 Result := '';
 if Count > 0 then for i := 0 to Count - 1 do Result += toString(i)
              else Result := '{' + #13#10 + '}';
end;

initialization
 DefaultFormatSettings.DecimalSeparator := '.';
 jsonMain  := TPascalJSON.Create;

finalization
 jsonMain.Destroy;

{$MACRO OFF}

end.

