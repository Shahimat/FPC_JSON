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

uses PascalParserJSON;

type
  jsType = (  JS_NONE, JS_NUMBER, JS_STRING, JS_BOOL, JS_OBJECT, JS_ARRAY );
  jsSet  = set of jsType;

const
  //JS_BOOL:    jsSet = [JS_TRUE, JS_FALSE];
  JS_SIMPLE:  jsSet = [JS_NUMBER, JS_STRING, JS_BOOL];
  JS_COMPLEX: jsSet = [JS_OBJECT, JS_ARRAY];

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
 Procedure jsonBegin(SomeType: jsType; Name: String = '');
 Procedure jsonEnd;
 Procedure jsonToRoot;
 //Function  jsonFind(Name: String; ElemType: jsType): Integer;
 Procedure jsonRead(Text: String);
 Procedure jsonCD(Text: String);
 Procedure jsonSaveToFile(FileName: String);
 Procedure jsonClear;
 Function  jsonInfo: String;
 Function  jsonPath: String;
 Function  jsonGetPath(Path: String): String;

 Function  jsonString: String; inline;


implementation

uses SysUtils;

type

 //JSCmessage = (JSI_RIGHT, JSI_ERROR, JSI_BRACET_NUM, JSI_FILE_EXTENSION,
 //  JSI_FILE_FOUND, JSI_VALID_JSON, JSI_VALID_EXTEN_LEN, JSI_VALID_EXTEN_NAME);

jsPInfoType = ^jsInfoType;
jsInfoType  = (JSI_RIGHT, JSI_ERROR, JSI_ERROR_EXPECTED, JSI_ERROR_UNTYPE,
  JSI_ERROR_STRING_OUT, JSI_ERROR_INCOMPLETE, JSI_ERROR_SYNTAX_NOT_COMPLETED,
  JSI_ERROR_LEXER_OUT_OF_SYNTAX, JSI_ERROR_NO_DATA);

(* TControllerBlock *)

PControllerBlock = ^TControllerBlock;
TControllerBlock = class
 private
  isError: Boolean;
  Info: jsInfoType;
  Param1, Param2: String;
  Function ErrorMessages: Boolean;
 public
  Constructor Create;
  Property  Err: Boolean read isError;
  Function  GetInfo: String;
  Procedure Reset;
  Procedure Msg(Message: jsInfoType; SomeParam: String); Overload;
  Procedure Msg(Message: jsInfoType); Overload;
  Procedure Bind(var SomeErr: PBoolean); Overload;
  Procedure Bind(var SomeInfo: jsPInfoType); Overload;
  Procedure Error;
end;

 PDataBlockJSON = ^TDataBlockJSON;
 PBlockJSON     = ^TBlockJSON;

 TDataBlockJSON = packed record
   numData:   PDouble;
   strData:   PString;
   boolData:  PBoolean;
 end;

 TBlockJSON = packed record
   BlockType: jsType;
   Name:      String;
   Data:      PDataBlockJSON;
   Parent:    PBlockJSON;
   Level:     Integer;
 end;

(* TPascalJSON *)

TPascalJSON = class
  private
    Item: array of PBlockJSON;
    fCount, Current: Integer;
    Control: PControllerBlock;
    Procedure SetCount(Value: Integer);
    Procedure Change(Value: PDataBlockJSON; OldType, NewType: jsType); Overload;
    Procedure Clear(Value: PDataBlockJSON; OldType: jsType); Overload;

    (*Data*)
    Function  GetLevel(i: Integer): Integer;
    Function  TabLevel(Level: Integer): String;
    Procedure BindParent(i: Integer);
    Procedure Clear(i: Integer); Overload;
    Procedure Change(i: Integer; SomeType: jsType); Overload;
    Procedure SetData(i: Integer; Value: String); Overload;
    Procedure SetData(i: Integer; Value: Double); Overload;
    Procedure SetData(i: Integer; Value: Integer); Overload;
    Procedure SetData(i: Integer; Value: Boolean); Overload;

    (*Checks*)
    //Function  ConverterMessage(Code: JSCmessage): String;
    //Function  jsonExpansion( FileName: String ): JSCmessage;

    (*Over*)
    Function  toString(i: Integer): String; Overload;
    Procedure LoadFromFile(FileName: String; var jsonText: String);
    Procedure SaveToFile(FileName, Text: String); Overload;
    property  Count: Integer read fCount write SetCount;
  public
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure Clear; Overload;
    Procedure toRoot;
    Procedure jsonBegin(SomeType: jsType; Name: String);
    Procedure jsonEnd;
    Procedure SaveToFile(FileName: String); Overload;
    Procedure Parse(Text: String);
    Procedure Write(Name, Value: String); Overload;
    Procedure Write(Name: String; Value: Double);  Overload;
    Procedure Write(Name: String; Value: Integer); Overload;
    Procedure Write(Name: String; Value: Boolean); Overload;
    Function  toString: String; Override; Overload;
    Procedure BindController(SomeControl: PControllerBlock);
end;

var
 jsonMain: TPascalJSON;
 Controller: TControllerBlock;

{$MACRO ON}

{$DEFINE JSONWRITE_NAME_VALUE := begin jsonMain.Write(Name, Value); end;}
{$DEFINE JSONWRITE_VALUE := begin jsonMain.Write('', Value); end;}
{$DEFINE corr := :Result:=}

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
 Bracket := FALSE;
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
 Quotes := FALSE;
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
 Quotes := FALSE;
 isObject := TRUE;
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

procedure jsonBegin(SomeType: jsType; Name: String);
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

//function jsonFind(Name: String; ElemType: jsType): Integer;
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

{ TControllerBlock }

function TControllerBlock.ErrorMessages: Boolean;
begin
 case Info of
  JSI_ERROR,
  JSI_ERROR_EXPECTED,
  JSI_ERROR_UNTYPE,
  JSI_ERROR_STRING_OUT,
  JSI_ERROR_INCOMPLETE,
  JSI_ERROR_SYNTAX_NOT_COMPLETED,
  JSI_ERROR_LEXER_OUT_OF_SYNTAX,
  JSI_ERROR_NO_DATA:
        Result := TRUE;
  else  Result := FALSE;
 end;
end;

constructor TControllerBlock.Create;
begin
 isError := TRUE;
 Info    := JSI_ERROR_NO_DATA;
end;

function TControllerBlock.GetInfo: String;
begin
 case Info of
  JSI_RIGHT:            Result := 'All right';
  JSI_ERROR:            Result := 'Error: Unknown error';
  JSI_ERROR_EXPECTED:   Result := 'Error: Expected ' + Param1;
  JSI_ERROR_UNTYPE:     Result := 'Error: Unknown type';
  JSI_ERROR_STRING_OUT: Result := 'Error: Out of string';
  JSI_ERROR_INCOMPLETE: Result := 'Error: Out of text';
  JSI_ERROR_NO_DATA:    Result := 'Error: No data';
  JSI_ERROR_SYNTAX_NOT_COMPLETED: Result := 'Error: Syntax not completed';
  JSI_ERROR_LEXER_OUT_OF_SYNTAX:  Result := 'Error: Lexer out of syntax';
  else                  Result := 'Error: Unknown information';
 end;
end;

procedure TControllerBlock.Reset;
begin
 Info    := JSI_RIGHT;
 isError := FALSE;
 Param1  := '';
 Param2  := '';
end;

procedure TControllerBlock.Msg(Message: jsInfoType; SomeParam: String);
begin
 Info    := Message;
 isError := ErrorMessages;
 Param1  := SomeParam;
 Param2  := '';
end;

procedure TControllerBlock.Msg(Message: jsInfoType);
begin
 Info    := Message;
 isError := ErrorMessages;
 Param1  := '';
 Param2  := '';
end;

procedure TControllerBlock.Bind(var SomeErr: PBoolean);
begin
 SomeErr := @isError;
end;

procedure TControllerBlock.Bind(var SomeInfo: jsPInfoType);
begin
 SomeInfo := @Info;
end;

procedure TControllerBlock.Error;
begin
 isError := TRUE;
 Info    := JSI_ERROR;
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

procedure TPascalJSON.Change(Value: PDataBlockJSON; OldType, NewType: jsType);
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

procedure TPascalJSON.Clear(Value: PDataBlockJSON; OldType: jsType);
begin
 Change(Value, OldType, JS_NONE);
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

procedure TPascalJSON.Change(i: Integer; SomeType: jsType);
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

function TPascalJSON.toString(i: Integer): String;
begin

end;

//function TPascalJSON.jsonExpansion(FileName: String): JSCmessage;
//var
// i: Integer;
// s: String;
//begin
// Result := JSI_ERROR;
// s := '';
// if FileName = '' then Exit;
// For i := Length(FileName) downto 1 do
// begin
//  if FileName[i] = '.' then
//  begin
//   if (length(s) <> 4) or (i = 1) then Exit;
//   if  (s[1] in ['j','J']) and (s[2] in ['s','S'])
//   and (s[3] in ['o','O']) and (s[4] in ['n','N']) then Result := JSI_RIGHT
//                                                   else Result := JSI_VALID_EXTEN_NAME;
//   Exit;
//  end;
//  s := FileName[i] + s;
//  if length(s) > 4 then
//  begin
//   Result := JSI_VALID_EXTEN_LEN;
//   Exit;
//  end;
// end;
//end;

procedure TPascalJSON.LoadFromFile(FileName: String; var jsonText: String);
var
 F: TextFile;
 s: String;
begin
 //Result := jsonExpansion(FileName);
 //if Result <> JSI_RIGHT then Exit;
 //if not FileExists( FileName ) then
 //begin
 // Result := JSI_FILE_FOUND;
 // Exit;
 //end;
 //try
 // jsonText := '';
 // AssignFile(F, FileName);
 // Reset(F);
 // while not EOF(F) do
 // begin
 //  ReadLn(F, s);
 //  jsonText += s;
 // end;
 // Result := jsonValidationCheck(jsonText);
 // if Result <> JSI_RIGHT then Exit;
 // Result := JSI_RIGHT;
 //finally
 // CloseFile(F);
 //end;
end;

procedure TPascalJSON.SaveToFile(FileName, Text: String);
var
 F: TextFile;
begin
 //Result := jsonExpansion(FileName);
 //if Result <> JSI_RIGHT then Exit;
 //Result := objValidationCheck;
 //if Result <> JSI_RIGHT then Exit;
 //if FileExists( FileName ) then DeleteFile(FileName);
 //try
 // AssignFile(F, FileName);
 // Rewrite(F);
 // WriteLn(F, Text);
 // Result := JSI_RIGHT;
 //finally
 // CloseFile(F);
 //end;
end;

constructor TPascalJSON.Create;
begin
 Clear;
end;

destructor TPascalJSON.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TPascalJSON.Clear;
begin
 Count := 0;
end;

procedure TPascalJSON.toRoot;
begin
 Current := 0;
end;

procedure TPascalJSON.jsonBegin(SomeType: jsType; Name: String);
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

procedure TPascalJSON.SaveToFile(FileName: String);
begin
 //Result := SaveToFile(FileName, toString);
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
begin
 Result := '';
 if Count > 0 then for i := 0 to Count - 1 do Result += toString(i)
              else Result := '{' + #13#10 + '}';
end;

procedure TPascalJSON.BindController(SomeControl: PControllerBlock);
begin
 Control := SomeControl;
end;

initialization
 DefaultFormatSettings.DecimalSeparator := '.';
 jsonMain := TPascalJSON.Create;
 Controller := TControllerBlock.Create;
 jsonMain.BindController( @Controller );

finalization
 Controller.Destroy;
 jsonMain.Destroy;

{$MACRO OFF}

end.

