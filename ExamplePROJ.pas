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



  jsonBegin;
  jsonWrite('valroot1',8767890);
  jsonWrite('valroot2', true);
  jsonWrite('valroot3','sometext');
  jsonWriteObj('obj1');
   jsonBegin;
   jsonWrite('valobj1','sometext1');
   jsonWrite('valobj2','sometext2');
   jsonEnd;
  jsonWrite('valroot4',87.6);
  jsonEnd;

  writeLn(jsonString);


  //writeln( JSON_LoadFromFile('Example.json') );

  writeLn('press Enter to exit...');
  ReadLn;
end.

