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
 WriteLn('==============================================');
 WriteLn;
 Write('    ');
 WriteLn(Text);
 WriteLn;
 WriteLn('==============================================');
 WriteLn;
 WriteLn;
end;

var
 Text: String;
 TermType: jsPToken;
 isFinished, isError: PBoolean;
 Value: PBlockJSON;
begin

  (*========================= Example 1 =========================*)
  Print('Error syntax example');
  Text := '{"R": true,"f":[ 45, 4,"e":{}}';
  jsonpBind(TermType, isFinished, isError);
  jsonpBind(@Text);
  jsonpReset;
  repeat
   jsonpNextTerminalCheck;
   Writeln(jsonpGetData + #9 + jsonpGetType);
  until isFinished^ or isError^;
  Writeln(jsonpGetInfo);

  (*========================= Example 2 =========================*)
  Print('An example of direct access to the parser');
  LoadFromFile('ExampleInput.json', Text);
  jsonpReset;
  repeat
   jsonpNextTerminalCheck;
   Writeln(jsonpGetData + #9 + jsonpGetType);
  until isFinished^ or isError^;
  Writeln(jsonpGetInfo);

  (*========================= Example 3 =========================*)
  Print('Add data directly to the format');
  jsonClear;
  jsonBegin(JS_OBJECT);                                          //  0
   jsonBegin(JS_OBJECT, 'first');                                //  1
    jsonWrite('first',34);                                       //  2
    jsonWrite('second',87.2);                                    //  3
    jsonBegin(JS_ARRAY, 'array');                                //  4
     jsonWrite(34);                                              //  5
     jsonWrite(89);                                              //  6
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
   jsonBegin(JS_ARRAY, 'second');                                // 19
    jsonWrite(1);                                                // 20
    jsonWrite(20.45);                                            // 21
    jsonWrite(FALSE);                                            // 22
    jsonWrite(TRUE);                                             // 23
    jsonWrite('what you say?');                                  // 24
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

  (*========================= Example 4 =========================*)
  Print('Direct access to the syntax tree');
  jsonReset;
  while jsonRead(Value) do
    WriteLn(
     Value^.Name + #9 + Value^.Data + #9 + jsonGetType(Value^.BlockType) +
     #9 + 'level = ' + IntToStr(Value^.Level)
    );

  WriteLn;
  WriteLn('press Enter to exit...');
  ReadLn;
end.

