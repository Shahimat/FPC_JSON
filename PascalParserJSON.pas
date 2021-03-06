(*===========================================================================*)
(*                                                                           *)
(*                         The MIT License (MIT)                             *)
(*                                                                           *)
(*       Copyright (c) 2019 Nakhapetyan Gevorg <ngs22071993@gmail.com>       *)
(*          ObjectPascal parser of JavaScript object nonation (JSON)         *)
(*                                                                           *)
(*===========================================================================*)
unit PascalParserJSON;

interface

type

jsPToken = ^jsToken;
jsToken  = (JST_NONE, JST_NUMBER, JST_STRING, JST_TRUE, JST_FALSE, JST_NULL,
  JST_VALUE_SEPARATOR, JST_NAME_SEPARATOR, JST_OBJECT_BEGIN, JST_OBJECT_END,
  JST_ARRAY_BEGIN, JST_ARRAY_END);

Procedure jsonpBind(var ValType: jsPToken; var ValFinished, ValError,
  ValObject: PBoolean); Overload;
Procedure jsonpBind(ValStructText: PString); inline; Overload;
Procedure jsonpReset; inline;
Procedure jsonpNextTerminalCheck; inline;  //with parser and lexer
Procedure jsonpNextTerminal; inline;       //with only lexer
Procedure jsonpNextTerminalAnalys; inline; //with only parser
Procedure jsonpPushToken(ValToken: jsToken); inline; Overload;
Procedure jsonpPushToken(const ValTokens: array of jsToken); inline; Overload;
Function  jsonpGetData: String; inline;
Function  jsonpGetToken: String; inline;
Function  jsonpGetInfo: String; inline;


implementation

uses GStack;

type

 jsLexeme = (
   JSL_NONE, JSL_TAB, JSL_PLUS, JSL_MINUS, JSL_POINT, JSL_ZERO, JSL_DIGIT19,
   JSL_VALUE_SEPARATOR, JSL_NAME_SEPARATOR, JSL_QUOTATION_MARK, JSL_OBJECT_BEGIN,
   JSL_OBJECT_END, JSL_ARRAY_BEGIN, JSL_ARRAY_END, JSL_LETTER, JSL_BACKSLASH
 );

 jsTerm = (
   (*TerminalS*)
   JSTS_NONE, JSTS_NUMBER, JSTS_STRING, JSTS_TRUE, JSTS_FALSE, JSTS_NULL,
   JSTS_VALUE_SEPARATOR, JSTS_NAME_SEPARATOR, JSTS_OBJECT_BEGIN, JSTS_OBJECT_END,
   JSTS_ARRAY_BEGIN, JSTS_ARRAY_END, JSTS_OUT,
   (*NonterminalS (laws)*)
   JSNS_JST, JSNS_O, JSNS_OO, JSNS_OOO, JSNS_A, JSNS_AA, JSNS_AAA, JSNS_V, JSNS_M
 );

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
   Step: Integer;
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

 (* TLexerJS *)

 PTLexerJS = ^TLexerJS;
 TLexerJS  = class
  private
   Control: PControllerBlock;
   Stage: Integer;
   CurToken: jsToken;
   CurLexeme: jsLexeme;
   Pos, PosText1, PosText2: Integer;
   PText: PString;
   Exelent, isFinished: Boolean;
   Function  GetNumberInterval: String;
   Function  GetStringInterval: String;
   Function  GetChar: Char; Inline;
   Procedure DETECT_NONE;   Inline;
   Procedure DETECT_STRING; Inline;
   Procedure DETECT_NUMBER; Inline;
   Procedure DETECT_TRUE;   Inline;
   Procedure DETECT_FALSE;  Inline;
   Procedure DETECT_NULL;   Inline;
   Procedure GetLexeme;
   Procedure Cycle;
   Procedure Step;
   Procedure Back;
   Procedure SetPar(SomeType: jsToken); Overload;
   Procedure SetPar(SomeLevel: Integer); Overload;
   Procedure SetPar(SomeType: jsToken; SomeLevel: Integer); Overload;
   Property  C: Char read GetChar;
   Property  CL: jsLexeme read CurLexeme;
  public
   Constructor Create;
   Destructor  Destroy; Override;
   Procedure Clear;
   Procedure Reset;
   property  Finished: Boolean read isFinished write isFinished;
   Property  Token: jsToken read CurToken write CurToken;
   Procedure Bind(SomeText: PString); Overload;
   Procedure Bind(var SomeFinished: PBoolean); Overload;
   Procedure Bind(var SomeTerminal: jsPToken); Overload;
   Procedure BindController(SomeControl: PControllerBlock);
   Procedure NextTerminal;
   Function  GetData: String;
   Function  GetToken: String;
 end;

 (* TParserJS *)

 TParserJS = class
  private
   Control:  PControllerBlock;
   isObject: Boolean;
   Lex: PTLexerJS;
   CurTerm: jsTerm;
   type
    jsTermStack  = specialize TStack<jsTerm>;
    jsTokenStack = specialize TStack<jsToken>;
   var
    Stack: jsTermStack;
    TokenStack: jsTokenStack;
   Procedure ClearStack; inline;
   Function  GetCurToken: jsToken; inline;
   Procedure SetCurToken(Value: jsToken); inline;
   Procedure Push(ValType: jsTerm); Overload; inline;
   Procedure Push(const ValTypes: array of jsTerm); Overload; inline;
   Procedure Pop;        inline;
   Procedure toJSNS_JST; inline;
   Procedure toJSNS_O;   inline;
   Procedure toJSNS_OO;  inline;
   Procedure toJSNS_OOO; inline;
   Procedure toJSNS_A;   inline;
   Procedure toJSNS_AA;  inline;
   Procedure toJSNS_AAA; inline;
   Procedure toJSNS_V;   inline;
   Procedure toJSNS_M;   inline;
   Procedure TerminalAnalys; inline;
   Property  CurToken: jsToken read GetCurToken write SetCurToken;
  public
   Constructor Create;
   Destructor  Destroy; Override;
   Procedure Clear;
   Procedure BindLexer(SomeLexer: PTLexerJS);
   Procedure BindController(SomeControl: PControllerBlock);
   Procedure Bind(var ValObject: PBoolean);
   Procedure Reset;
   Procedure NextTerminal;
   Procedure NextTerminalAnalys;
   Procedure PushToken(SomeToken: jsToken); Overload;
   Procedure PushToken(const SomeTokens: array of jsToken); Overload;
   Function  PopToken: jsToken;
 end;

var
  Lexer:      TLexerJS;
  Parser:     TParserJS;
  Controller: TControllerBlock;

procedure jsonpBind(var ValType: jsPToken; var ValFinished, ValError,
  ValObject: PBoolean);
begin
 Lexer.Bind(ValType);
 Lexer.Bind(ValFinished);
 Parser.Bind(ValObject);
 Controller.Bind(ValError);
end;

procedure jsonpBind(ValStructText: PString);
begin
 Lexer.Bind( ValStructText );
end;

procedure jsonpReset;
begin
 Controller.Reset;
 Lexer.Reset;
 Parser.Reset;
end;

procedure jsonpNextTerminalCheck;
begin
 Parser.NextTerminal;
end;

procedure jsonpNextTerminal;
begin
 Lexer.NextTerminal;
end;

procedure jsonpNextTerminalAnalys;
begin
 Parser.NextTerminalAnalys;
end;

procedure jsonpPushToken(ValToken: jsToken);
begin
 Parser.PushToken(ValToken);
end;

procedure jsonpPushToken(const ValTokens: array of jsToken);
begin
 Parser.PushToken(ValTokens);
end;

function jsonpGetData: String;
begin
 Result := Lexer.GetData;
end;

function jsonpGetToken: String;
begin
 Result := Lexer.GetToken;
end;

function jsonpGetInfo: String;
begin
 Result := Controller.GetInfo;
end;

(* TControllerBlock *)

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
var
 s: String;
begin
 str(Step - 1, s);
 case Info of
  JSI_RIGHT:            Result := 'All right';
  JSI_ERROR:            Result := 'Error: Unknown error';
  JSI_ERROR_EXPECTED:
   Result := 'Error: Expected ' + Param1 + ' at step ' + s + ' but found ' +
     jsonpGetToken;
  JSI_ERROR_UNTYPE:     Result := 'Error: Unknown type';
  JSI_ERROR_STRING_OUT: Result := 'Error: Out of string';
  JSI_ERROR_INCOMPLETE: Result := 'Error: Out of text';
  JSI_ERROR_NO_DATA:    Result := 'Error: No data. Are you sure you bind the text?';
  JSI_ERROR_SYNTAX_NOT_COMPLETED: Result := 'Error: Syntax not completed';
  JSI_ERROR_LEXER_OUT_OF_SYNTAX:  Result := 'Error: Lexer out of syntax. Step = '
    + s;
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

(* TLexerJS *)

function TLexerJS.GetNumberInterval: String;
var i: Integer;
begin
 Result := '';
 For i := PosText1 to PosText2 do Result += PText^[i];
end;

function TLexerJS.GetStringInterval: String;
var i: Integer;
var
  NewCode: Boolean;
begin
 Result  := '';
 NewCode := FALSE;
 For i := PosText1 to PosText2 do
 if NewCode then
 begin
  case PText^[i] of
   '0': Result += #0;
   #39: Result += #39;
   '"': Result += '"';
   '\': Result += '\';
   'n': Result += #13#10;
   'r': Result += #10#13;
   't': Result += #9;
  end;
  NewCode := FALSE;
  Continue;
 end else if PText^[i] = '\' then NewCode := TRUE else Result += PText^[i];
end;

function TLexerJS.GetChar: Char;
begin
 Result := PText^[Pos];
end;

procedure TLexerJS.DETECT_NONE;
begin
 case CL of
  JSL_MINUS:
  begin
   SetPar(JST_NUMBER);
   PosText1 := Pos;
  end;
  JSL_ZERO:
  begin
   SetPar(JST_NUMBER, 1);
   PosText1 := Pos;
  end;
  JSL_DIGIT19:
  begin
   SetPar(JST_NUMBER, 2);
   PosText1 := Pos;
  end;
  JSL_QUOTATION_MARK:
  begin
   SetPar(JST_STRING);
   PosText1 := Pos + 1;
  end;
  JSL_LETTER:
  case C of
   't': SetPar(JST_TRUE);
   'f': SetPar(JST_FALSE);
   'n': SetPar(JST_NULL);
   else Control^.Msg(JSI_ERROR_EXPECTED, 'TRUE | FALSE | NULL');
  end;
  JSL_NAME_SEPARATOR:
  begin
   SetPar(JST_NAME_SEPARATOR);
   Exelent := TRUE;
  end;
  JSL_VALUE_SEPARATOR:
  begin
   SetPar(JST_VALUE_SEPARATOR);
   Exelent := TRUE;
  end;
  JSL_OBJECT_BEGIN:
  begin
   SetPar(JST_OBJECT_BEGIN);
   Exelent := TRUE;
  end;
  JSL_OBJECT_END:  
  begin
   SetPar(JST_OBJECT_END);
   Exelent := TRUE;
  end;
  JSL_ARRAY_BEGIN: 
  begin
   SetPar(JST_ARRAY_BEGIN);
   Exelent := TRUE;
  end;
  JSL_ARRAY_END:
  begin
   SetPar(JST_ARRAY_END);
   Exelent := TRUE;
  end;
  JSL_TAB:;
  else
   Control^.Error;
 end;
end;

procedure TLexerJS.DETECT_STRING;
begin
 case stage of
  0:
  case CL of
   JSL_BACKSLASH: SetPar(1);
   JSL_QUOTATION_MARK:
   begin
    Exelent  := TRUE;
    PosText2 := Pos - 1;
    Exit;
   end;
  end;
  1: if C in ['0',#39,'"','\','n','r','t'] then SetPar(0)
     else Control^.Msg(JSI_ERROR_EXPECTED, 'JS CHAR CODE');
 end;
 if C = #13 then Control^.Msg(JSI_ERROR_STRING_OUT);
end;

procedure TLexerJS.DETECT_NUMBER;
begin
 case stage of
  0:
  case CL of
   JSL_ZERO:    SetPar(1);
   JSL_DIGIT19: SetPar(2);
   else Control^.Msg(JSI_ERROR_EXPECTED, 'DIGIT');
  end;
  1:
  case CL of
   JSL_POINT:   SetPar(3);
   JSL_LETTER:  if C in ['e', 'E'] then SetPar(5)
    else Control^.Msg(JSI_ERROR_EXPECTED, 'e | E');
   JSL_TAB, JSL_VALUE_SEPARATOR, JSL_OBJECT_END, JSL_ARRAY_END:
   begin
    Exelent  := TRUE;
    PosText2 := Pos - 1;
    Back;
   end;
   else Control^.Msg(JSI_ERROR_EXPECTED, 'POINT | e | E | SEPARATOR');
  end;
  2:
  case CL of
   JSL_ZERO, JSL_DIGIT19:;
   JSL_POINT:   SetPar(3);
   JSL_LETTER:  if C in ['e', 'E'] then SetPar(5)
    else Control^.Msg(JSI_ERROR_EXPECTED, 'e | E');
   JSL_TAB, JSL_VALUE_SEPARATOR, JSL_OBJECT_END, JSL_ARRAY_END:
   begin
    Exelent  := TRUE;
    PosText2 := Pos - 1;
    Back;
   end;
   else Control^.Msg(JSI_ERROR_EXPECTED, 'DIGIT | POINT | e | E | SEPARATOR');
  end;
  3: if CL in [JSL_ZERO, JSL_DIGIT19] then SetPar(4)
    else Control^.Msg(JSI_ERROR_EXPECTED, 'DIGIT');
  4:
  case CL of
   JSL_ZERO, JSL_DIGIT19:;
   JSL_LETTER:  if C in ['e', 'E'] then SetPar(5)
    else Control^.Msg(JSI_ERROR_EXPECTED, 'e | E');
   JSL_TAB, JSL_VALUE_SEPARATOR, JSL_OBJECT_END, JSL_ARRAY_END:
   begin
    Exelent  := TRUE;
    PosText2 := Pos - 1;
    Back;
   end;
   else Control^.Msg(JSI_ERROR_EXPECTED, 'DIGIT | e | E | SEPARATOR');
  end;
  5:
  case CL of
   JSL_PLUS, JSL_MINUS:   SetPar(6);
   JSL_ZERO, JSL_DIGIT19: SetPar(7);
   else Control^.Msg(JSI_ERROR_EXPECTED, '+ | - | DIGIT');
  end;
  6: if CL in [JSL_ZERO, JSL_DIGIT19] then SetPar(7)
   else Control^.Msg(JSI_ERROR_EXPECTED, 'DIGIT');
  7:
  case CL of
   JSL_ZERO, JSL_DIGIT19:;
   JSL_TAB, JSL_VALUE_SEPARATOR, JSL_OBJECT_END, JSL_ARRAY_END:
   begin
    Exelent  := TRUE;
    PosText2 := Pos - 1;
    Back;
   end;
   else Control^.Msg(JSI_ERROR_EXPECTED, 'DIGIT | SEPARATOR');
  end;
  else Control^.Error;
 end;
end;

procedure TLexerJS.DETECT_TRUE;
begin
 case stage of
  0: if C = 'r' then SetPar(1) else Control^.Msg(JSI_ERROR_EXPECTED, 'r');
  1: if C = 'u' then SetPar(2) else Control^.Msg(JSI_ERROR_EXPECTED, 'u');
  2: if C = 'e' then SetPar(3) else Control^.Msg(JSI_ERROR_EXPECTED, 'e');
  3: if CL in [JSL_TAB, JSL_VALUE_SEPARATOR, JSL_OBJECT_END, JSL_ARRAY_END] then
  begin
   Exelent := TRUE;
   Back;
  end else Control^.Msg(JSI_ERROR_EXPECTED, 'SEPARATOR');
  else Control^.Error;
 end;
end;

procedure TLexerJS.DETECT_FALSE;
begin
 case stage of
  0: if C = 'a' then SetPar(1) else Control^.Msg(JSI_ERROR_EXPECTED, 'a');
  1: if C = 'l' then SetPar(2) else Control^.Msg(JSI_ERROR_EXPECTED, 'l');
  2: if C = 's' then SetPar(3) else Control^.Msg(JSI_ERROR_EXPECTED, 's');
  3: if C = 'e' then SetPar(4) else Control^.Msg(JSI_ERROR_EXPECTED, 'e');
  4: if CL in [JSL_TAB, JSL_VALUE_SEPARATOR, JSL_OBJECT_END, JSL_ARRAY_END] then
  begin
   Exelent := TRUE;
   Back;
  end else Control^.Msg(JSI_ERROR_EXPECTED, 'SEPARATOR');
  else Control^.Error;
 end;
end;

procedure TLexerJS.DETECT_NULL;
begin
 case stage of
  0: if C = 'u' then SetPar(1) else Control^.Msg(JSI_ERROR_EXPECTED, 'u');
  1: if C = 'l' then SetPar(2) else Control^.Msg(JSI_ERROR_EXPECTED, 'l');
  2: if C = 'l' then SetPar(3) else Control^.Msg(JSI_ERROR_EXPECTED, 'l');
  3: if CL in [JSL_TAB, JSL_VALUE_SEPARATOR, JSL_OBJECT_END, JSL_ARRAY_END] then
  begin
   Exelent := TRUE;
   Back;
  end else Control^.Msg(JSI_ERROR_EXPECTED, 'SEPARATOR');
  else Control^.Error;
 end;
end;

procedure TLexerJS.GetLexeme;
begin
 case C of
   #13,#10,#9,' ':
        CurLexeme := JSL_TAB;
   '\': CurLexeme := JSL_BACKSLASH;
   '+': CurLexeme := JSL_PLUS;
   '-': CurLexeme := JSL_MINUS;
   '.': CurLexeme := JSL_POINT;
   '0': CurLexeme := JSL_ZERO;
   '1'..'9':
        CurLexeme := JSL_DIGIT19;
   ',': CurLexeme := JSL_VALUE_SEPARATOR;
   ':': CurLexeme := JSL_NAME_SEPARATOR;
   '"': CurLexeme := JSL_QUOTATION_MARK;
   '{': CurLexeme := JSL_OBJECT_BEGIN;
   '}': CurLexeme := JSL_OBJECT_END;
   '[': CurLexeme := JSL_ARRAY_BEGIN;
   ']': CurLexeme := JSL_ARRAY_END;
   'a'..'z','A'..'Z':
        CurLexeme := JSL_LETTER;
  else  CurLexeme := JSL_NONE;
 end;
end;

procedure TLexerJS.Cycle;
begin
 Stage     := 0;
 CurToken   := JST_NONE;
 Exelent   := FALSE;
end;

procedure TLexerJS.Step;
begin
 inc(Pos);
 GetLexeme;
end;

procedure TLexerJS.Back;
begin
 dec(Pos);
end;

procedure TLexerJS.SetPar(SomeType: jsToken);
begin
 CurToken := SomeType;
end;

procedure TLexerJS.SetPar(SomeLevel: Integer);
begin
 Stage := SomeLevel;
end;

procedure TLexerJS.SetPar(SomeType: jsToken; SomeLevel: Integer);
begin
 SetPar(SomeType);
 SetPar(SomeLevel);
end;

constructor TLexerJS.Create;
begin
 Clear;
end;

destructor TLexerJS.Destroy;
begin
 Clear;
 Control := nil;
 inherited Destroy;
end;

procedure TLexerJS.Clear;
begin
 PText   := nil;
 Control := nil;
end;

procedure TLexerJS.Reset;
begin
 if PText = nil then Control^.Msg(JSI_ERROR_NO_DATA);
 Pos := 0;
 isFinished := FALSE;
 Cycle;
end;

procedure TLexerJS.Bind(SomeText: PString);
begin
 PText := SomeText;
end;

procedure TLexerJS.Bind(var SomeFinished: PBoolean);
begin
 SomeFinished := @isFinished;
end;

procedure TLexerJS.Bind(var SomeTerminal: jsPToken);
begin
 SomeTerminal := @CurToken;
end;

procedure TLexerJS.BindController(SomeControl: PControllerBlock);
begin
 Control := SomeControl;
end;

procedure TLexerJS.NextTerminal;
begin
 Cycle;
 repeat
  Step;
  case CurToken of
   JST_NONE:   DETECT_NONE;
   JST_NUMBER: DETECT_NUMBER;
   JST_STRING: DETECT_STRING;
   JST_TRUE:   DETECT_TRUE;
   JST_FALSE:  DETECT_FALSE;
   JST_NULL:   DETECT_NULL;
   else Control^.Msg(JSI_ERROR_UNTYPE);
  end;
  if Pos = Length(PText^) then
  begin
   isFinished := TRUE;
   if not (CurToken in [JST_OBJECT_END, JST_ARRAY_END, JST_NONE]) then
     Control^.Msg(JSI_ERROR_INCOMPLETE);
   Exit;
  end;
 until Control^.Err or Exelent;
end;

function TLexerJS.GetData: String;
begin
 if (not Control^.Err) and (Exelent) then
 case CurToken of
  JST_NUMBER:          Result := GetNumberInterval;
  JST_STRING:          Result := GetStringInterval;
  JST_TRUE:            Result := 'true';
  JST_FALSE:           Result := 'false';
  JST_NULL:            Result := 'null';
  JST_VALUE_SEPARATOR: Result := ',';
  JST_NAME_SEPARATOR:  Result := ':';
  JST_OBJECT_BEGIN:    Result := '{';
  JST_OBJECT_END:      Result := '}';
  JST_ARRAY_BEGIN:     Result := '[';
  JST_ARRAY_END:       Result := ']';
  else                 Result := 'NONE';
 end else              Result := 'NONE';
end;

function TLexerJS.GetToken: String;
begin
  case CurToken of
   JST_NUMBER:          Result := 'NUMBER';
   JST_STRING:          Result := 'STRING';
   JST_TRUE:            Result := 'TRUE';
   JST_FALSE:           Result := 'FALSE';
   JST_NULL:            Result := 'NULL';
   JST_VALUE_SEPARATOR: Result := 'VALUE_SEPARATOR';
   JST_NAME_SEPARATOR:  Result := 'NAME_SEPARATOR';
   JST_OBJECT_BEGIN:    Result := 'OBJECT_BEGIN';
   JST_OBJECT_END:      Result := 'OBJECT_END';
   JST_ARRAY_BEGIN:     Result := 'ARRAY_BEGIN';
   JST_ARRAY_END:       Result := 'ARRAY_END';
   else                 Result := 'NONE';
  end;
end;

(* TParserJS *)

procedure TParserJS.ClearStack;
begin
 while not Stack.IsEmpty do Stack.Pop;
 while not TokenStack.IsEmpty do TokenStack.Pop;
end;

function TParserJS.GetCurToken: jsToken;
begin
 Result := Lex^.CurToken;
end;

procedure TParserJS.SetCurToken(Value: jsToken);
begin
 Lex^.CurToken := Value;
end;

procedure TParserJS.Push(ValType: jsTerm);
begin
 Stack.Push(ValType);
end;

procedure TParserJS.Push(const ValTypes: array of jsTerm);
var
 i: Integer;
begin
 For i := High(ValTypes) downto Low(ValTypes) do
  Stack.Push(ValTypes[i]);
end;

procedure TParserJS.Pop;
begin
 if Stack.IsEmpty then CurTerm := JSTS_NONE else
 begin
  CurTerm := Stack.Top;
  Stack.Pop;
 end;
end;

procedure TParserJS.toJSNS_JST;
begin
 case CurToken of
  JST_OBJECT_BEGIN: Push(JSNS_O);
  JST_ARRAY_BEGIN:  Push(JSNS_A);
  else Control^.Msg(JSI_ERROR_EXPECTED, 'OBJECT BEGIN | ARRAY BEGIN');
 end;
end;

procedure TParserJS.toJSNS_O;
begin
 case CurToken of
  JST_OBJECT_BEGIN: Push([JSTS_OBJECT_BEGIN, JSNS_OO, JSTS_OBJECT_END]);
  else Control^.Msg(JSI_ERROR_EXPECTED, 'OBJECT BEGIN');
 end;
end;

procedure TParserJS.toJSNS_OO;
begin
 case CurToken of
  JST_STRING: Push([JSNS_M, JSNS_OOO]);
  JST_OBJECT_END:;
  else Control^.Msg(JSI_ERROR_EXPECTED, 'STRING | OBJECT END');
 end;
end;

procedure TParserJS.toJSNS_OOO;
begin
 case CurToken of
  JST_VALUE_SEPARATOR: Push([JSTS_VALUE_SEPARATOR, JSNS_OO]);
  JST_OBJECT_END:;
  else Control^.Msg(JSI_ERROR_EXPECTED, 'VALUE SEPARATOR | OBJECT END');
 end;
 isObject := TRUE;
end;

procedure TParserJS.toJSNS_A;
begin
 case CurToken of
  JST_ARRAY_BEGIN: Push([JSTS_ARRAY_BEGIN, JSNS_AA, JSTS_ARRAY_END]);
  else Control^.Msg(JSI_ERROR_EXPECTED, 'ARRAY BEGIN');
 end;
end;

procedure TParserJS.toJSNS_AA;
const
 message = 'NUMBER | STRING | TRUE | FALSE | NULL '
  + '| OBJECT BEGIN | ARRAY BEGIN | ARRAY END';
begin
 case CurToken of
  JST_NUMBER,
  JST_STRING,
  JST_TRUE,
  JST_FALSE,
  JST_NULL,
  JST_OBJECT_BEGIN,
  JST_ARRAY_BEGIN: Push([JSNS_V, JSNS_AAA]);
  JST_ARRAY_END:;
  else Control^.Msg(JSI_ERROR_EXPECTED, message);
 end;
end;

procedure TParserJS.toJSNS_AAA;
begin
 case CurToken of
  JST_VALUE_SEPARATOR: Push([JSTS_VALUE_SEPARATOR, JSNS_AA]);
  JST_ARRAY_END:;
  else Control^.Msg(JSI_ERROR_EXPECTED, 'VALUE SEPARATOR | ARRAY END');
 end;
 isObject := FALSE;
end;

procedure TParserJS.toJSNS_V;
const
 message = 'NUMBER | STRING | TRUE | FALSE | NULL '
  + '| OBJECT BEGIN | ARRAY BEGIN';
begin
 case CurToken of
  JST_NUMBER: Push(JSTS_NUMBER);
  JST_STRING: Push(JSTS_STRING);
  JST_TRUE:   Push(JSTS_TRUE);
  JST_FALSE:  Push(JSTS_FALSE);
  JST_NULL:   Push(JSTS_NULL);
  JST_OBJECT_BEGIN: Push(JSNS_O);
  JST_ARRAY_BEGIN: Push(JSNS_A);
  else Control^.Msg(JSI_ERROR_EXPECTED, message);
 end;
end;

procedure TParserJS.toJSNS_M;
begin
 case CurToken of
  JST_STRING: Push([JSTS_STRING, JSTS_NAME_SEPARATOR, JSNS_V]);
  else Control^.Msg(JSI_ERROR_EXPECTED, 'STRING');
 end;
end;

procedure TParserJS.TerminalAnalys;
begin
 repeat
   Pop;
   case CurTerm of
    JSNS_JST: toJSNS_JST;
    JSNS_O:   toJSNS_O;
    JSNS_OO:  toJSNS_OO;
    JSNS_OOO: toJSNS_OOO;
    JSNS_A:   toJSNS_A;
    JSNS_AA:  toJSNS_AA;
    JSNS_AAA: toJSNS_AAA;
    JSNS_V:   toJSNS_V;
    JSNS_M:   toJSNS_M;
    JSTS_NUMBER: if CurToken = JST_NUMBER then Break
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'NUMBER');
    JSTS_STRING: if CurToken = JST_STRING then Break
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'STRING');
    JSTS_TRUE:   if CurToken = JST_TRUE then Break
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'TRUE');
    JSTS_FALSE:  if CurToken = JST_FALSE then Break
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'FALSE');
    JSTS_NULL:   if CurToken = JST_NULL then Break
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'NULL');
    JSTS_VALUE_SEPARATOR: if CurToken = JST_VALUE_SEPARATOR then Break
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'VALUE SEPARATOR');
    JSTS_NAME_SEPARATOR:  if CurToken = JST_NAME_SEPARATOR then Break
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'NAME SEPARATOR');
    JSTS_OBJECT_BEGIN:    if CurToken = JST_OBJECT_BEGIN then
                 begin isObject := TRUE; Break; end
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'OBJECT BEGIN');
    JSTS_OBJECT_END:      if CurToken = JST_OBJECT_END then Break
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'OBJECT END');
    JSTS_ARRAY_BEGIN:     if CurToken = JST_ARRAY_BEGIN then
                 begin isObject := FALSE; Break; end
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'ARRAY BEGIN');
    JSTS_ARRAY_END:       if CurToken = JST_ARRAY_END then Break
                 else Control^.Msg(JSI_ERROR_EXPECTED, 'ARRAY END');
    JSTS_OUT: Control^.Msg(JSI_ERROR_LEXER_OUT_OF_SYNTAX);
    else Control^.Error;
   end;
  until Control^.Err;
  inc( Control^.Step );
  if Lex^.Finished then
  begin
   Pop;
   if not (CurTerm = JSTS_OUT) then
     Control^.Msg(JSI_ERROR_SYNTAX_NOT_COMPLETED);
  end;
end;

constructor TParserJS.Create;
begin
 Stack := jsTermStack.Create;
 TokenStack := jsTokenStack.Create;
end;

destructor TParserJS.Destroy;
begin
 Clear;
 Stack.Destroy;
 TokenStack.Destroy;
 inherited Destroy;
end;

procedure TParserJS.Clear;
begin
 ClearStack;
 Control := nil;
 Lex := nil;
end;

procedure TParserJS.BindLexer(SomeLexer: PTLexerJS);
begin
 Lex := SomeLexer;
end;

procedure TParserJS.BindController(SomeControl: PControllerBlock);
begin
 Control := SomeControl;
end;

procedure TParserJS.Bind(var ValObject: PBoolean);
begin
 ValObject := @isObject;
end;

procedure TParserJS.Reset;
begin
 ClearStack;
 Push([JSNS_JST, JSTS_OUT]);
 isObject := TRUE;
 Control^.Step := 0;
end;

procedure TParserJS.NextTerminal;
begin
 if not Control^.Err then Lex^.NextTerminal
                     else Exit;
 TerminalAnalys;
end;

procedure TParserJS.NextTerminalAnalys;
begin
 if not Control^.Err then Lex^.Token := PopToken
                     else Exit;
 TerminalAnalys;
end;

procedure TParserJS.PushToken(SomeToken: jsToken);
begin
 TokenStack.Push(SomeToken);
end;

procedure TParserJS.PushToken(const SomeTokens: array of jsToken);
var
 i: Integer;
begin
 For i := Low(SomeTokens) to High(SomeTokens) do
  TokenStack.Push(SomeTokens[i]);
end;

function TParserJS.PopToken: jsToken;
begin
 if TokenStack.IsEmpty then Result := JST_NONE
 else begin
  Result := TokenStack.Top;
  TokenStack.Pop;
  if TokenStack.IsEmpty then Lex^.Finished := TRUE;
 end;
end;

initialization
  Lexer := TLexerJS.Create;
  Parser := TParserJS.Create;
  Controller := TControllerBlock.Create;
  Parser.BindLexer( @Lexer );
  Parser.BindController( @Controller );
  Lexer.BindController( @Controller );

finalization
  Controller.Destroy;
  Parser.Destroy;
  Lexer.Destroy;

end.
