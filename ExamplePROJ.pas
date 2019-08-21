program ExamplePROJ;

{$APPTYPE CONSOLE}

uses SysUtils, PascalParserJSON, PascalJSON;

Procedure LoadFromFile(FileName: String; var SomeText: String);
var
 F: TextFile;
 s: String;
begin
 SomeText := '';
 AssignFile(F, FileName);
 Reset(F);
 while not EOF(F) do
 begin
  ReadLn(F, s);
  SomeText += s;
 end;
 CloseFile(F);
end;

Procedure Print(const Text: String); inline;
begin
 WriteLn;
 WriteLn;
 WriteLn;
 WriteLn;
 WriteLn('=========================================================');
 WriteLn;
 Write('    ');
 WriteLn(Text);
 WriteLn;
 WriteLn('=========================================================');
 WriteLn;
 WriteLn;
end;

Function toText(Value: Boolean): String;
begin
 if Value then Result := 'TRUE'
          else Result := 'FALSE';
end;

var
 Text: String;
 TermType: jsPToken;
 isFinished, isError, isObject: PBoolean;
 Value: PBlockJSON;
begin
  (*========================= Example 1 =========================*)
  Print('Example 1: Error syntax example');
  Text := '{"R": true,"f":[ 45, 4,"e":{}}';
  Writeln('Input = ' + Text);
  jsonpBind(TermType, isFinished, isError, isObject);
  jsonpBind(@Text);
  jsonpReset;
  repeat
   jsonpNextTerminalCheck;
   Writeln( jsonpGetData + #9 + toText(isObject^) + #9 + jsonpGetToken );
  until isFinished^ or isError^;
  Writeln(jsonpGetInfo);

  (*========================= Example 2 =========================*)
  Print('Example 2: Error syntax example with parser only');
  jsonpReset;
  jsonpPushToken([JST_OBJECT_END, JST_OBJECT_END, JST_OBJECT_BEGIN,
    JST_NAME_SEPARATOR, JST_STRING, JST_VALUE_SEPARATOR, JST_NUMBER,
    JST_VALUE_SEPARATOR, JST_NUMBER, JST_ARRAY_BEGIN, JST_NAME_SEPARATOR,
    JST_STRING, JST_VALUE_SEPARATOR, JST_TRUE, JST_NAME_SEPARATOR,
    JST_STRING, JST_OBJECT_BEGIN]);
  repeat
   jsonpNextTerminalAnalys;
   Writeln( jsonpGetData + #9 + toText(isObject^) + #9 + jsonpGetToken );
  until isFinished^ or isError^;
  Writeln(jsonpGetInfo);

  (*========================= Example 3 =========================*)
  Print('Example 3: An example of direct access to the parser');
  LoadFromFile('ExampleInput.json', Text);
  jsonpReset;
  repeat
   jsonpNextTerminalCheck;
   Writeln( jsonpGetData + #9 + toText(isObject^) + #9 + jsonpGetToken );
  until isFinished^ or isError^;
  Writeln(jsonpGetInfo);

  (*========================= Example 4 =========================*)
  Print('Example 4: Add data directly to the format');
  jsonClear;
  jsonBegin(JS_OBJECT);                                          //  0
   jsonBegin(JS_OBJECT, 'first');                                //  1
    jsonWrite('inline',-34e3);                                   //  2
    jsonWrite('second',87.2);                                    //  3
    jsonBegin(JS_ARRAY, 'array');                                //  4
     jsonWrite(34);                                              //  5
     jsonWrite(-89.2e2);                                         //  6
     jsonWrite('+');                                             //  7
     jsonBegin(JS_OBJECT);                                       //  8
      jsonBegin(JS_ARRAY, 'array');                              //  9
      jsonEnd;                                                   // 10
      jsonBegin(JS_OBJECT, 'clean');                             // 11
       jsonBegin(JS_ARRAY, 'array');                             // 12
        jsonWrite('yes');                                        // 13
       jsonEnd;                                                  // 14
      jsonEnd;                                                   // 15
     jsonEnd;                                                    // 16
    jsonEnd;                                                     // 17
   jsonEnd;                                                      // 18
   jsonBegin(JS_ARRAY, 'wow');                                   // 19
    jsonWrite(1);                                                // 20
    jsonWrite(20.45);                                            // 21
    jsonWrite(FALSE);                                            // 22
    jsonWrite(TRUE);                                             // 23
    jsonWrite('what you"\ say?');                                // 24
    jsonBegin(JS_OBJECT);                                        // 25
     jsonWrite('maybe true','ok');                               // 26
     jsonBegin(JS_ARRAY, 'some');                                // 27
      jsonWrite(234);                                            // 28
      jsonWrite(TRUE);                                           // 29
      jsonBegin(JS_OBJECT);                                      // 30
       jsonWrite('end','no');                                    // 31
       jsonWrite('big int',64567890);                            // 32
      jsonEnd;                                                   // 33
     jsonEnd;                                                    // 34
    jsonEnd;                                                     // 35
   jsonEnd;                                                      // 36
   jsonWrite('description','some    JSON converter?     yeah!'); // 37
  jsonEnd;                                                       // 38
  writeLn(jsonString); //Full output
  jsonSaveToFile('Output.json');
  WriteLn('Validate = ' + ToText(jsonValidate));
  WriteLn( jsonInfo );

  (*========================= Example 5 =========================*)
  Print('Example 5: Using the built-in parser');
  jsonParse(@Text);
  WriteLn('The result is similar to the previous one');
  //writeLn(jsonString); //Full output
  WriteLn( jsonInfo );

  (*========================= Example 6 =========================*)
  Print('Example 6: Using the built-in uploader');
  jsonLoadFromFile('ExampleInput.json');
  WriteLn('The result is similar to the previous one');
  //writeLn(jsonString); //Full output
  WriteLn( jsonInfo );

  (*========================= Example 7 =========================*)
  Print('Example 7: Direct access to internal structure');
  jsonReset;
  while jsonRead(Value) do
  with Value^ do
    WriteLn(
     IntToStr(Level) + #9 +
     jsonGetType(BlockType) + #9 +
     jsonGetType(Parent^.BlockType) + #9 +
     Name + #9 +
     Data
    );
  WriteLn( jsonInfo );

  (*========================= Example 8 =========================*)
  Print('Example 8: Search example');
  if jsonFind('wow', JS_ARRAY, Value) then
  with Value^ do
  WriteLn(
     IntToStr(Level) + #9 +
     jsonGetType(BlockType) + #9 +
     Name + #9 +
     Data
    );
  if jsonFindNext(JS_STRING, Value) then
  with Value^ do
  WriteLn(
     IntToStr(Level) + #9 +
     jsonGetType(BlockType) + #9 +
     Name + #9 +
     Data
    );
  WriteLn( jsonInfo );

  WriteLn;
  WriteLn('press Enter to exit...');
  ReadLn;
end.

