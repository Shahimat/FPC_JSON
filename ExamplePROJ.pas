program ExamplePROJ;

{$APPTYPE CONSOLE}

uses PascalParserJSON; //PascalJSON  (in process)

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
 WriteLn('==============================================');
 WriteLn(Text);
 WriteLn('==============================================');
 WriteLn;
end;

var
 Text: String;
 TermType: jsPToken;
 isFinished, isError: PBoolean;
begin

  (*========================= Example 1 =========================*)
  Print('Error syntax example');
  Text := '{"R": true,"f":[ 45, 4,"e":{}}';
  jspBind(TermType, isFinished, isError);
  jspBind(@Text);
  jspReset;
  repeat
   jspNextTerminalCheck;
   Writeln(jspGetData + #9 + jspGetType);
  until isFinished^ or isError^;
  Writeln(jspGetInfo);

  (*========================= Example 2 =========================*)
  Print('An example of direct access to the parser');
  LoadFromFile('ExampleInput.json', Text);
  jspReset;
  repeat
   jspNextTerminalCheck;
   Writeln(jspGetData + #9 + jspGetType);
  until isFinished^ or isError^;
  Writeln(jspGetInfo);


  (*========================= Example 3 =========================*)
  //jsonClear;
  //jsonBegin(JS_OBJECT);                                          //  0
  // jsonBegin(JS_OBJECT, 'first');                                //  1
  //  jsonWrite('first',34);                                       //  2
  //  jsonWrite('second',87.2);                                    //  3
  //  jsonBegin(JS_ARRAY, 'array');                                //  4
  //   jsonWrite(34);                                              //  5
  //   jsonWrite(89);                                              //  6
  //   jsonWrite('+');                                             //  7
  //   jsonBegin(JS_OBJECT);                                       //  8
  //    jsonBegin(JS_ARRAY, 'array');                              //  9
  //    jsonEnd;                                                   // 10
  //    jsonBegin(JS_OBJECT, 'clean');                             // 11
  //     jsonBegin(JS_ARRAY, 'array');                             // 12
  //      jsonWrite('yes');                                        // 13
  //     jsonEnd;                                                  // 14
  //    jsonEnd;                                                   // 15
  //   jsonEnd;                                                    // 16
  //  jsonEnd;                                                     // 17
  // jsonEnd;                                                      // 18
  // jsonBegin(JS_ARRAY, 'second');                                // 19
  //  jsonWrite(1);                                                // 20
  //  jsonWrite(20.45);                                            // 21
  //  jsonWrite(FALSE);                                            // 22
  //  jsonWrite(TRUE);                                             // 23
  //  jsonWrite('what you say?');                                  // 24
  //  jsonBegin(JS_OBJECT);                                        // 25
  //   jsonWrite('maybe true','ok');                               // 26
  //   jsonBegin(JS_ARRAY, 'some');                                // 27
  //    jsonWrite(234);                                            // 28
  //    jsonWrite(TRUE);                                           // 29
  //    jsonBegin(JS_OBJECT);                                      // 30
  //     jsonWrite('end','no');                                    // 31
  //     jsonWrite('big int',64567890);                            // 32
  //    jsonEnd;                                                   // 33
  //   jsonEnd;                                                    // 34
  //  jsonEnd;                                                     // 35
  // jsonEnd;                                                      // 36
  // jsonWrite('description','some    JSON converter?     yeah!'); // 37
  //jsonEnd;                                                       // 38

  (*Sample output in json view*)
  //writeLn(jsonString);
  //jsonSaveToFile('Output.json');

  //writeln( JSON_LoadFromFile('Example.json') );

  WriteLn;
  WriteLn('press Enter to exit...');
  ReadLn;
end.

