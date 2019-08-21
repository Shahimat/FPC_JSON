## Simple and fast JSON converter and parser to Object Pascal internal structure

The standard was adopted as the basis JSON [RFC 8259](https://tools.ietf.org/html/rfc8259) without `null` definition with the RBNF structure described below.

The whole project is divided into 3 files:
1. `ExamplePROJ.pas` - Main file containing examples. To compile and run, you need to create a console application project.
2. `PascalParserJSON.pas` - the parser module containing methods for working with the JSON text file determines the validity of the text, data and types of tokens.
3. `PascalJSON.pas` - a module with an abstract syntax tree in the form of a one-dimensional list, containing methods for working with the final JSON file, translation into internal representation, saving and loading the file.

### EBNF notation of standart JSON RFC 8259

```EBNF
(*=== Lexical part ===*)
(* Lexemes *)
(* char -> all symbols without " *)
plus            = '+';
minus           = '-';
point           = '.';
zero            = '0';
digit19         = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
quotation_mark  = '"';
separator       = #9 | #13 | #10 | ' ';

(* Definitions *)
digit = zero | digit19;
e     = 'e' | 'E';
int   = zero | (digit19, {digit});
frac  = point, digit, {digit};
exp   = e, [minus | plus], digit, {digit};

(* Tokens *)
value_separator = ',';
name_separator  = ':';
object_begin    = '{';
object_end      = '}';
array_begin     = '[';
array_end       = ']';
true    = 'true';
false   = 'false';
null    = 'null';
string  = quotation_mark, {char}, quotation_mark;
number  = [minus], int, [frac], [exp];

(*=== Syntax part ===*)
json_text = object | array;
object    = object_begin, [member, {value_separator, member}], object_end;
array     = array_begin, [value, {value_separator, value}], array_end;
value     = true | false | null | number | string | object | array;
member    = string, name_separator, value;
```

## Простой и быстрый конвертер и парсер из JSON во внутреннее представление Object Pascal

За базу был принят стандарт JSON [RFC 8259](https://tools.ietf.org/html/rfc8259) без `null` определения со структурой РБНФ, описанной выше (EBNF).

Весь проект разбит на 3 файла:
1. `ExamplePROJ.pas` - Главный файл, содержащий примеры. Для компиляции и запуска необходимо создать проект консольного приложения.
2. `PascalParserJSON.pas` - модуль парсера, содержащий методы работы с текстовой файлом JSON, определяет валидность текста, данные и типы токенов.
3. `PascalJSON.pas` - модуль с абстрактным синтаксическим деревом в форме одномерного списка, содержащий методы работы с конечным файлом JSON, переводом во внутреннее представление, сохранение и загрузки файла.

## The MIT License

JavaScript object notation file format with MIT License (read `LICENSE` file).