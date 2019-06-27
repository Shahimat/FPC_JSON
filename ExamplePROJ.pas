program ExamplePROJ;

{$APPTYPE CONSOLE}

uses PascalJSON;

begin

  //writeln(isJSONformat('wrgmnrogjn.json'));
  //writeln(isJSONformat('wrgvw2g.JSoN'));
  //writeln(isJSONformat('/wlvgno.SONJ'));
  //writeln(isJSONformat('qwrgvo2rwr ono2rfmp'));
  //writeln(isJSONformat('2f 2.w frwgv 2.rf2 rf2.'));
  //writeln(isJSONformat('on.JSON'));
  //writeln(isJSONformat('.JSON'));
  //writeln(isJSONformat('SON'));

  writeln( JSON_LoadFromFile('Example.json') );

  writeLn('press Enter to exit...');
  ReadLn;
end.

