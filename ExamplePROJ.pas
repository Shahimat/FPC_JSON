program ExamplePROJ;

{$APPTYPE CONSOLE}

uses PascalJSON, debugger;

begin

  //writeln(isJSONformat('wrgmnrogjn.json'));
  //writeln(isJSONformat('wrgvw2g.JSoN'));
  //writeln(isJSONformat('/wlvgno.SONJ'));
  //writeln(isJSONformat('qwrgvo2rwr ono2rfmp'));
  //writeln(isJSONformat('2f 2.w frwgv 2.rf2 rf2.'));
  //writeln(isJSONformat('on.JSON'));
  //writeln(isJSONformat('.JSON'));
  //writeln(isJSONformat('SON'));


  //Console.Log(N, S);




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

  //writeLn(jsonString);
  jsonSaveToFile('some.json');

  //writeln( JSON_LoadFromFile('Example.json') );

  writeLn('press Enter to exit...');
  //ReadLn;
end.

