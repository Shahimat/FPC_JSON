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
 jsType = (JS_NONE, JS_NUMBER, JS_STRING, JS_BOOL, JS_OBJECT, JS_ARRAY, JS_END);
 jsSet  = set of jsType;

 PBlockJSON = ^TBlockJSON;
 TBlockJSON = packed record
   BlockType:  jsType;
   Name, Data: String;
   Parent:     PBlockJSON;
   Level:      Integer;
 end;

const
 JS_SIMPLE:  jsSet = [JS_NUMBER, JS_STRING, JS_BOOL];
 JS_COMPLEX: jsSet = [JS_OBJECT, JS_ARRAY];

 Procedure jsonWrite(Name, Value: String); inline; Overload;
 Procedure jsonWrite(Name: String; Value: Double); inline; Overload;
 Procedure jsonWrite(Name: String; Value: Integer); inline; Overload;
 Procedure jsonWrite(Name: String; Value: Boolean); inline; Overload;
 Procedure jsonWrite(Value: String); inline; Overload;
 Procedure jsonWrite(Value: Double); inline; Overload;
 Procedure jsonWrite(Value: Integer); inline; Overload;
 Procedure jsonWrite(Value: Boolean); inline; Overload;
 Function  jsonRead(var PValue: PBlockJSON): Boolean; inline;
 Function  jsonGetType(SomeType: jsType): String;
 Procedure jsonBegin(SomeType: jsType; Name: String = ''); inline;
 Procedure jsonEnd; inline;
 Procedure jsonParse(Text: PString); inline;
 Procedure jsonReset; inline;
 Function  jsonFind(Name: String; ElemType: jsType; var Pos: PBlockJSON): Boolean; inline; Overload;
 Function  jsonFind(ElemType: jsType; var Pos: PBlockJSON): Boolean; inline; Overload;
 Function  jsonFindNext(Name: String; ElemType: jsType; var Pos: PBlockJSON): Boolean; inline; Overload;
 Function  jsonFindNext(ElemType: jsType; var Pos: PBlockJSON): Boolean; inline; Overload;
 Procedure jsonSaveToFile(FileName: String); inline;
 Procedure jsonLoadFromFile(FileName: String); inline;
 Procedure jsonClear; inline;
 Function  jsonInfo: String; inline;
 Function  jsonString: String; inline;
 Function  jsonValidate: Boolean; inline;


implementation

uses SysUtils;

type

jsPInfoType = ^jsInfoType;
jsInfoType  = (JSI_RIGHT, JSI_ERROR, JSI_ERROR_PARSER, JSI_ERROR_EXTENSION,
  JSI_ERROR_FILEEXISTS, JSI_ERROR_VALIDATION, JSI_ERROR_FILENAME);

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
  Procedure Right;
end;

(* TPascalJSON *)

TPascalJSON = class
  private
    Item: array of PBlockJSON;
    fCount, Current: Integer;
    Control: PControllerBlock;
    jsonPToken: jsPToken;
    jsonFinished, jsonError, jsonObject: PBoolean;
    Procedure SetCount(Value: Integer);

    (*Data*)
    Function  GetLevel(i: Integer): Integer;
    Function  TabLevel(Level: Integer): String;
    Procedure BindParent(i: Integer);
    Procedure Clear(i: Integer); Overload;
    Procedure Change(i: Integer; SomeType: jsType); Overload;
    Procedure SetData(i: Integer; Value: String);   Overload;
    Procedure SetData(i: Integer; Value: Double);   Overload;
    Procedure SetData(i: Integer; Value: Integer);  Overload;
    Procedure SetData(i: Integer; Value: Boolean);  Overload;

    (*Checks*)
    Function  jsonExtension( FileName: String ): Boolean;
    Procedure PushTokens(i: Integer); inline;

    (*Over*)
    Function  toString(i: Integer): String; Overload;
    property  Count: Integer read fCount write SetCount;
  public
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure Clear; Overload;
    Procedure Reset;
    Procedure jsonBegin(SomeType: jsType; Name: String);
    Procedure jsonEnd;
    Procedure LoadFromFile(FileName: String);
    Procedure SaveToFile(FileName, Text: String); Overload;
    Procedure SaveToFile(FileName: String); inline; Overload;
    Procedure Parse(Text: PString);
    Procedure Write(Name, Value: String); Overload;
    Procedure Write(Name: String; Value: Double);  Overload;
    Procedure Write(Name: String; Value: Integer); Overload;
    Procedure Write(Name: String; Value: Boolean); Overload;
    Procedure WriteType(Name, Value: String; BlockType: jsType);
    Function  Read(var PValue: PBlockJSON): Boolean;
    Function  toString: String; Override; Overload;
    Procedure BindController(SomeControl: PControllerBlock);
    Procedure BindParser;
    Function  Find(Name: String; ElemType: jsType; var Pos: PBlockJSON): Boolean;
    Function  FindNext(Name: String; ElemType: jsType; var Pos: PBlockJSON): Boolean;
    Function  Validate: Boolean;
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

procedure jsonWrite(Name,         Value: String);  JSONWRITE_NAME_VALUE
procedure jsonWrite(Name: String; Value: Double);  JSONWRITE_NAME_VALUE
procedure jsonWrite(Name: String; Value: Integer); JSONWRITE_NAME_VALUE
procedure jsonWrite(Name: String; Value: Boolean); JSONWRITE_NAME_VALUE
procedure jsonWrite(Value: String);  JSONWRITE_VALUE
procedure jsonWrite(Value: Double);  JSONWRITE_VALUE
procedure jsonWrite(Value: Integer); JSONWRITE_VALUE
procedure jsonWrite(Value: Boolean); JSONWRITE_VALUE

function jsonRead(var PValue: PBlockJSON): Boolean;
begin
 Result := jsonMain.Read(PValue);
end;

function jsonGetType(SomeType: jsType): String;
begin
 case SOmeType of
  JS_NONE   corr 'NONE';
  JS_NUMBER corr 'NUMBER';
  JS_STRING corr 'STRING';
  JS_BOOL   corr 'BOOL';
  JS_OBJECT corr 'OBJECT';
  JS_ARRAY  corr 'ARRAY';
  JS_END    corr 'END';
 end;
end;

procedure jsonBegin(SomeType: jsType; Name: String);
begin
 jsonMain.jsonBegin(SomeType, Name);
end;

procedure jsonEnd;
begin
 jsonMain.jsonEnd;
end;

procedure jsonParse(Text: PString);
begin
 jsonMain.Parse(Text);
end;

procedure jsonReset;
begin
 jsonMain.Reset;
end;

function jsonFind(Name: String; ElemType: jsType; var Pos: PBlockJSON): Boolean;
begin
 Result := jsonMain.Find(Name, ElemType, Pos);
end;

function jsonFind(ElemType: jsType; var Pos: PBlockJSON): Boolean;
begin
 Result := jsonMain.Find('', ElemType, Pos);
end;

function jsonFindNext(Name: String; ElemType: jsType; var Pos: PBlockJSON
  ): Boolean;
begin
 Result := jsonMain.FindNext(Name, ElemType, Pos);
end;

function jsonFindNext(ElemType: jsType; var Pos: PBlockJSON): Boolean;
begin
 Result := jsonMain.FindNext('', ElemType, Pos);
end;

procedure jsonSaveToFile(FileName: String);
begin
 jsonMain.SaveToFile(FileName);
end;

procedure jsonLoadFromFile(FileName: String);
begin
 jsonMain.LoadFromFile(FileName);
end;

procedure jsonClear;
begin
 jsonMain.Clear;
end;

function jsonInfo: String;
begin
 Result := Controller.GetInfo;
end;

function jsonString: String;
begin
 Result := jsonMain.toString;
end;

function jsonValidate: Boolean;
begin
 Result := jsonMain.Validate;
end;

{ TControllerBlock }

function TControllerBlock.ErrorMessages: Boolean;
begin
 case Info of
  JSI_ERROR,
  JSI_ERROR_PARSER,
  JSI_ERROR_EXTENSION,
  JSI_ERROR_FILEEXISTS,
  JSI_ERROR_VALIDATION,
  JSI_ERROR_FILENAME:
        Result := TRUE;
  else  Result := FALSE;
 end;
end;

constructor TControllerBlock.Create;
begin
 isError := FALSE;
 Info    := JSI_RIGHT;
end;

function TControllerBlock.GetInfo: String;
begin
 case Info of
  JSI_RIGHT:            Result := 'All right';
  JSI_ERROR:            Result := 'Error: Unknown error';
  JSI_ERROR_PARSER:     Result := 'Failed Parse: ' + jsonpGetInfo;
  JSI_ERROR_EXTENSION:  Result := 'Error: Incorrect file extension entry';
  JSI_ERROR_FILEEXISTS: Result := 'Error: File not exists';
  JSI_ERROR_VALIDATION: Result := 'Failed validation: ' + jsonpGetInfo;
  JSI_ERROR_FILENAME:   Result := 'Error: No file name entered';
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

procedure TControllerBlock.Right;
begin
 isError := FALSE;
 Info    := JSI_RIGHT;
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
 if Item[i]^.BlockType  =  JS_END        then dec(dlevel);
 Result := Item[i - 1]^.Level + dlevel;
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
   if  Item[j]^.BlockType = JS_END then begin inc(ds); Continue; end;
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
  Item[i]^.Parent := Item[0];
end;

procedure TPascalJSON.Clear(i: Integer);
begin
 with Item[i]^ do
 begin
  Name      := '';
  Data      := '';
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
  BlockType := SomeType;
  Level     := GetLevel(i);
 end;
end;

procedure TPascalJSON.SetData(i: Integer; Value: String);
begin
 Item[i]^.Data := Value;
end;

procedure TPascalJSON.SetData(i: Integer; Value: Double);
begin
 Item[i]^.Data := FloatToStr(Value);
end;

procedure TPascalJSON.SetData(i: Integer; Value: Integer);
begin
 Item[i]^.Data := IntToStr(Value);
end;

procedure TPascalJSON.SetData(i: Integer; Value: Boolean);
begin
 if Value then Item[i]^.Data := 'true'
          else Item[i]^.Data := 'false';
end;

function TPascalJSON.toString(i: Integer): String;
begin
 with Item[i]^ do
 begin
  case BlockType of
   JS_STRING: Result := '"' + Data + '"';
   JS_NUMBER, JS_BOOL: Result := Data;
   JS_OBJECT: Result := '{';
   JS_ARRAY:  Result := '[';
   JS_END:
    case Parent^.BlockType of
     JS_OBJECT: Result := '}';
     JS_ARRAY:  Result := ']';
    end;
  end;
  if (Name <> '') and (i > 0) and (BlockType <> JS_END) then
    Result := '"' + Name + '": ' + Result;
  if i < Count - 1 then
  if (Item[i + 1]^.BlockType <> JS_END) and not (BlockType in JS_COMPLEX) then
    Result := Result + ',';
  Result := TabLevel(Level) + Result + #13#10;
 end;
end;

function TPascalJSON.jsonExtension(FileName: String): Boolean;
var
 i: Integer;
 s: String;
begin
 Result := FALSE;
 if FileName = '' then
 begin
  Control^.Msg(JSI_ERROR_FILENAME);
  Exit;
 end;
 s := '';
 For i := Length(FileName) downto 1 do
 begin
  if FileName[i] = '.' then
  begin
   if (length(s) <> 4) or (i = 1) then Break;
   if  (s[1] in ['j','J']) and (s[2] in ['s','S'])
   and (s[3] in ['o','O']) and (s[4] in ['n','N']) then
   begin
    Result := TRUE;
    Exit;
   end;
   Break;
  end;
  s := FileName[i] + s;
  if length(s) > 4 then Break;
 end;
 Control^.Msg(JSI_ERROR_EXTENSION);
end;

procedure TPascalJSON.PushTokens(i: Integer);
begin
 with Item[i]^ do
 begin
  if i < Count - 1 then
  if (Item[i + 1]^.BlockType <> JS_END) and not (BlockType in JS_COMPLEX) then
    jsonpPushToken(JST_VALUE_SEPARATOR);
  case BlockType of
   JS_NUMBER: jsonpPushToken(JST_NUMBER);
   JS_STRING: jsonpPushToken(JST_STRING);
   JS_BOOL:   jsonpPushToken(JST_TRUE);
   JS_OBJECT: jsonpPushToken(JST_OBJECT_BEGIN);
   JS_ARRAY:  jsonpPushToken(JST_ARRAY_BEGIN);
   JS_END:    if Parent^.BlockType = JS_OBJECT then jsonpPushToken(JST_OBJECT_END)
                                               else jsonpPushToken(JST_ARRAY_END);
  end;
  if (Parent^.BlockType = JS_OBJECT) and (i > 0) and (BlockType <> JS_END) then
    jsonpPushToken([JST_NAME_SEPARATOR, JST_STRING]);
 end;
end;

constructor TPascalJSON.Create;
begin
 Clear;
end;

destructor TPascalJSON.Destroy;
begin
 Clear;
 jsonPToken := nil;
 jsonFinished := nil;
 jsonError := nil;
 Control := nil;
 inherited Destroy;
end;

procedure TPascalJSON.Clear;
begin
 Count := 0;
end;

procedure TPascalJSON.Reset;
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
 Change(Position, JS_END);
 BindParent(Position);
end;

procedure TPascalJSON.LoadFromFile(FileName: String);
var
 F: TextFile;
 s, Text: String;
begin
 if not jsonExtension(FileName) then Exit;
 if not FileExists( FileName ) then
 begin
  Control^.Msg(JSI_ERROR_FILEEXISTS);
  Exit;
 end;
 Text := '';
 try
  AssignFile(F, FileName);
  System.Reset(F);
  while not EOF(F) do
  begin
   ReadLn(F, s);
   Text += s;
  end;
 finally
  CloseFile(F);
 end;
 jsonClear;
 jsonParse(@Text);
end;

procedure TPascalJSON.SaveToFile(FileName, Text: String);
var
 F: TextFile;
begin
 if not jsonExtension(FileName) then Exit;
 if not Validate then Exit;
 if FileExists( FileName ) then DeleteFile(FileName);
 try
  AssignFile(F, FileName);
  Rewrite(F);
  WriteLn(F, Text);
 finally
  CloseFile(F);
 end;
end;

procedure TPascalJSON.SaveToFile(FileName: String);
begin
 SaveToFile(FileName, toString);
end;

procedure TPascalJSON.Parse(Text: PString);
var
 name: String;
 NameSep: Boolean;
{$DEFINE NameSepDef:= if NameSep then begin Name := ''; NameSep := FALSE; end}
begin
 Clear;
 jsonpBind(Text);
 jsonpReset;
 Name := '';
 NameSep := FALSE;
 repeat
  jsonpNextTerminalCheck;
  case jsonPToken^ of
   JST_NUMBER:
   begin
    WriteType(Name, jsonpGetData, JS_NUMBER);
    NameSepDef;
   end;
   JST_STRING:
   if (not NameSep) and (jsonObject^) then Name := jsonpGetData
   else begin
    Write(Name, jsonpGetData);
    Name := '';
    NameSep := FALSE;
   end;
   JST_TRUE, JST_FALSE:
   begin
    WriteType(Name, jsonpGetData, JS_BOOL);
    NameSepDef;
   end;
   JST_NAME_SEPARATOR: NameSep := TRUE;
   JST_OBJECT_BEGIN:
   begin
    jsonBegin(JS_OBJECT, Name);
    NameSepDef;
   end;
   JST_ARRAY_BEGIN:
   begin
    jsonBegin(JS_ARRAY, Name);
    NameSepDef;
   end;
   JST_OBJECT_END, JST_ARRAY_END: jsonEnd;
  end;
 until jsonFinished^ or jsonError^;
 if jsonError^ then Control^.Msg(JSI_ERROR_PARSER)
               else Control^.Right;
end;

procedure TPascalJSON.Write(Name, Value: String);
const BlockType = JS_STRING; WRITE_NAME_VALUE

procedure TPascalJSON.Write(Name: String; Value: Double);
const BlockType = JS_NUMBER; WRITE_NAME_VALUE

procedure TPascalJSON.Write(Name: String; Value: Integer);
const BlockType = JS_NUMBER; WRITE_NAME_VALUE

procedure TPascalJSON.Write(Name: String; Value: Boolean);
const BlockType = JS_BOOL;   WRITE_NAME_VALUE

procedure TPascalJSON.WriteType(Name, Value: String; BlockType: jsType);
WRITE_NAME_VALUE

function TPascalJSON.Read(var PValue: PBlockJSON): Boolean;
begin
 PValue := Item[Current];
 Result := Current < Count - 1;
 if Result then inc(Current);
end;

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

procedure TPascalJSON.BindParser;
begin
 jsonpBind(jsonPToken, jsonFinished, jsonError, jsonObject);
end;

function TPascalJSON.Find(Name: String; ElemType: jsType; var Pos: PBlockJSON
  ): Boolean;
var
 i: Integer;
begin
 Result := FALSE;
 for i := 0 to Count - 1 do
 if Item[i]^.BlockType = ElemType then
 if Item[i]^.Name = Name then
 begin
  Result := TRUE;
  Current := i;
  Break;
 end;
 Pos := Item[Current];
end;

function TPascalJSON.FindNext(Name: String; ElemType: jsType;
  var Pos: PBlockJSON): Boolean;
var
 i: Integer;
begin
 Result := FALSE;
 for i := Current + 1 to Count - 1 do
 if Item[i]^.BlockType = ElemType then
 if Item[i]^.Name = Name then
 begin
  Result := TRUE;
  Current := i;
  Break;
 end;
 Pos := Item[Current];
end;

function TPascalJSON.Validate: Boolean;
var
 i: Integer;
begin
 jsonpReset;
 for i := Count - 1 downto 0 do
  PushTokens(i);
 repeat
  jsonpNextTerminalAnalys;
 until jsonFinished^ or jsonError^;
 Result := not jsonError^;
 if jsonError^ then Control^.Msg(JSI_ERROR_VALIDATION);
end;

initialization
 DefaultFormatSettings.DecimalSeparator := '.';
 jsonMain := TPascalJSON.Create;
 Controller := TControllerBlock.Create;
 jsonMain.BindController( @Controller );
 jsonMain.BindParser;

finalization
 Controller.Destroy;
 jsonMain.Destroy;

{$MACRO OFF}

end.

