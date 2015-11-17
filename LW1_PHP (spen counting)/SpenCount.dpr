program SpenCount;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows;

const
  WordSide = ' !<>[],./?;:-+=*&(){}';
  VarName = ['A'..'Z'] + ['0'..'9'] + ['_'];
  DoublePrefix = 2;
  TriplePrefix = 3;

type

  typestates = (ScanNormal, ScanName, ScanParams, ScanFunction, ScanCom,
    ScanStringOne, ScanStringTwo, ScanVar, ScanHeredoc);

type

  Stack = ^StackElement;

  StackElement = record
    Data: typestates;
    Next: stack;
  end;



  VarType = record
    Name: string;
    level: string;
    Number: integer;
  end;

  point_function = ^functiontype;

  functiontype = record
    balanse: integer;
    Name: string;
    Uplevel: string;
    Vars: array of VarType;
    Next: point_function;
  end;



var

  stack_pointer: stack;

  function EcranCheck(iterator: integer; var str: string): boolean;
  var
    Count: integer;
  begin
    Count := 0;
    while (iterator <> 0) and (str[iterator] = '\') do
    begin
      Count := Count + 1;
      iterator := iterator - 1;
    end;
    if Count mod DoublePrefix = 0 then
      Result := False
    else
      Result := True;
  end;

  procedure PushStack(State: typestates; var StackOfStates: stack);
  var
    Current_pointer: stack;
  begin
    New(Current_pointer);
    Current_pointer^.Data := state;
    Current_pointer^.Next := StackOfStates;
    StackOfStates := Current_pointer;
  end;

  function PopStack(var StackOfStates: stack): typestates;
  var
    Current_pointer: stack;
  begin
    if StackOfStates <> nil then
    begin
      Current_pointer := StackOfStates;
      Result := Current_pointer^.Data;
      StackOfStates := StackOfStates^.Next;
      Dispose(Current_pointer);
    end
    else
      Result := ScanNormal;
  end;

  function FindPosition(Current_Char: AnsiChar; const Current_String: string): integer;
  var
    iterator: integer;
  begin
    Result := 0;
    for iterator := 1 to Length(Current_String) do
      if (Current_String[iterator] = Current_Char) then
      begin
        Result := iterator;
        Break;
      end;
  end;


  procedure AddFunction(Name: string; uplevel: string; var head_function_list: point_function);
  var
    Current_pointer: point_function;
  begin
    Current_pointer := head_function_list;
    while Current_pointer^.Next <> nil do
      Current_pointer := Current_pointer^.Next;


    New(Current_pointer^.Next);
    Current_pointer := Current_pointer^.Next;
    Current_pointer^.balanse := 0;
    Current_pointer^.Name := Name;
    Current_pointer^.Uplevel := uplevel;
    setlength(Current_pointer^.vars, 0);
    Current_pointer^.Next := nil;

  end;

  function FindFunction(call: string; var head_function_list: point_function): point_function;
  var
    Current_pointer: point_function;
  begin
    Current_pointer := head_function_list;
    while (Current_pointer^.Next <> nil) and (Current_pointer^.Name <> call) do
      Current_pointer := Current_pointer^.Next;
    Result := Current_pointer;
  end;

  procedure AddVar(Name: string; var plevel: point_function);
  begin
    setlength(plevel^.vars, length(plevel^.vars) + 1);
    plevel^.vars[length(plevel^.vars) - 1].Name := Name;
    plevel^.vars[length(plevel^.vars) - 1].Number  := 1;
    plevel^.vars[length(plevel^.vars) - 1].level := plevel^.Name;
  end;

  procedure CheckVar(Name: string; var this_level: point_function);
  var
    isflagup: boolean;
    iterator: integer;
  begin
    isflagup := True;
    iterator := 0;
    while (isflagup) and (iterator < length(this_level^.vars)) do
    begin
      if Name = this_level^.vars[iterator].Name then
      begin
        this_level^.vars[iterator].Number := this_level^.vars[iterator].Number + 1;
        isflagup := False;
      end
      else
        iterator := iterator + 1;
    end;
    if isflagup = True then
      addvar(Name, this_level);
  end;

  procedure WriteFunction(var head_function_list: point_function);
  var
    Current_pointer: point_function;
    iterator: integer;
    outfile: textfile;
  begin
    AssignFile(outfile, 'Result.txt');
    Rewrite(outfile);
    Current_pointer := head_function_list^.Next;
    while (Current_pointer <> nil) do
    begin
      writeln(outfile);
      writeln(outfile, 'Level (function):', Current_pointer^.Name);
      writeln(outfile, '///////-------\\\\\\');
      for iterator := 0 to length(Current_pointer^.Vars) - 1 do
      begin
        writeln(outfile, 'Identificator: ', Current_pointer^.Vars[iterator].Name);
        writeln(outfile, 'Spen: ', Current_pointer^.Vars[iterator].Number - 1);
        writeln(outfile);
      end;
      writeln(outfile, '\\\\\\\-------//////');
      Current_pointer := Current_pointer^.Next;
    end;
    CloseFile(outfile);
  end;

  procedure VariablesScan(var CurrentChar: ansichar; var state: typestates; var Currentword: string;
  var Currentlevel: string; var Current_pointer: point_function; var head_function_list: point_function; var StackOfStates: stack;
  var iterator_main: integer; var buffer: string);
  begin
    if (upcase(CurrentChar) in VarName) then
    begin
      CurrentWord := CurrentWord + CurrentChar;
      if iterator_main = length(buffer) then
      begin
        Current_pointer := FindFunction(CurrentLevel, head_function_list);
        if not (CurrentWord[1] in ['0'..'9']) then
          CheckVar(CurrentWord, Current_pointer);
        CurrentWord := '';
        State := PopStack(StackOfStates);
      end;

    end
    else

    if (StackOfStates^.Data = ScanStringTwo) and (CurrentChar = '"') then
    begin
      Current_pointer := FindFunction(CurrentLevel, head_function_list);
      if not (CurrentWord[1] in ['0'..'9']) then
        CheckVar(CurrentWord, Current_pointer);

      PopStack(StackOfStates);
      State := PopStack(StackOfStates);
    end
    else
    begin
      Current_pointer := FindFunction(CurrentLevel, head_function_list);
      if not (CurrentWord[1] in ['0'..'9']) then
        CheckVar(CurrentWord, Current_pointer);
      CurrentWord := '';
      State := PopStack(StackOfStates);
    end;
  end;


  procedure FunctionScan(var CurrentChar: ansichar; var state: typestates;
  var Currentword: string; var Currentlevel: string; var Current_pointer: point_function; var head_function_list: point_function; var StackOfStates: stack);
  begin
    Current_pointer := findFunction(Currentlevel, head_function_list);
    if CurrentChar = '{' then
      Current_pointer^.balanse := Current_pointer^.balanse + 1
    else
    if CurrentChar = '}' then
    begin
      Current_pointer^.balanse := Current_pointer^.balanse - 1;

      if Current_pointer^.balanse = 0 then
      begin
        State := PopStack(StackOfStates);
        CurrentLevel := Current_pointer^.Uplevel;
      end;
    end
    else
    begin

      if FindPosition(CurrentChar, WordSide) <> 0 then

      begin

        if ansilowercase(CurrentWord) = 'function' then
        begin
          PushStack(ScanFunction, StackOfStates);
          State := ScanName;
        end;
        CurrentWord := '';
      end
      else

        CurrentWord := CurrentWord + CurrentChar;

    end;

  end;

  procedure StringRead(var CurrentChar: ansichar; var buffer: string; var Currentword: string;
  var Currentlevel: string; var iterator_main: integer; var state: typestates;
  var is_jump_string: byte; var buffer_pointer: point_function; var head_function_list: point_function; var Heredoc: string; var IsHeredoc: byte);
  begin
    if is_jump_string = 0 then
      case State of

        ScanVar: VariablesScan(CurrentChar, state, Currentword, Currentlevel, buffer_pointer, head_function_list, stack_pointer, iterator_main, buffer);

        ScanHeredoc: if (IsHeredoc = 0) then
            State := PopStack(stack_pointer);


        ScanStringOne:
        begin
          if (CurrentChar = '''') and (not EcranCheck(iterator_main - 1, buffer)) then
            State := PopStack(stack_pointer);
        end;

        ScanStringTwo:
        begin
          if (CurrentChar = '"') and (not EcranCheck(iterator_main - 1, buffer)) then
            State := PopStack(stack_pointer);
        end;

        ScanCom:
        begin
          if copy(buffer, iterator_main, DoublePrefix) = '*/' then
            State := PopStack(stack_pointer);
        end;

        ScanNormal:
        begin
          Currentlevel := ':GLOBAL:';
          if FindPosition(CurrentChar, WordSide) <> 0 then
          begin
            if ansilowercase(CurrentWord) = 'function' then
            begin
              PushStack(ScanNormal, stack_pointer);
              State := ScanName;
            end;
            CurrentWord := '';
          end
          else
            CurrentWord := CurrentWord + CurrentChar;
        end;

        ScanName:
        begin
          if FindPosition(CurrentChar, WordSide) <> 0 then
          begin
            State := ScanParams;
            AddFunction(CurrentWord, CurrentLevel, head_function_list);
            CurrentLevel := CurrentWord;
          end
          else
            CurrentWord := CurrentWord + CurrentChar;
        end;

        ScanParams:
        begin
          if CurrentChar = '{' then
          begin
            State := ScanFunction;
            buffer_pointer := findFunction(Currentlevel, head_function_list);
            buffer_pointer^.balanse := +1;
          end;
        end;

        ScanFunction: FunctionScan(CurrentChar, state, Currentword, Currentlevel, buffer_pointer, head_function_list, stack_pointer);

      end
    else
      is_jump_string := 0;
  end;


  procedure Initialize(var FileSourceCode: TextFile; var FileResult: textfile;
  var Currentword: string; var Currentlevel: string; var state: typestates;
  var is_jump_string: byte; var head_function_list: point_function);
  begin
    State := ScanNormal;

    AssignFile(FileSourceCode, 'Source.txt');
    AssignFile(FileResult, 'Res.txt');
    Reset(FileSourceCode);
    Rewrite(FileResult);


    head_function_list := nil;
    New(head_function_list);
    head_function_list^.Name := '';
    head_function_list^.Next := nil;

    CurrentLevel := ':GLOBAL:';
    AddFunction(':GLOBAL:', ':GLOBAL:', head_function_list);


    CurrentWord := '';
    is_jump_string := 0;
  end;

  procedure MakeStringMode(ChangeState: typestates; var is_jump_string: byte; var State: typestates;
  var StackOfStates: stack);
  begin
    PushStack(State, StackOfStates);
    State := ChangeState;
    is_jump_string  := 1;
  end;

  procedure MainScan();

  var
    state: typestates;
  var
    buffer: string;
    iterator_main: integer;
    CurrentChar:  char;

  var
    FileSourceCode, FileResult: textfile;

  var
    head_function_list, buffer_pointer: point_function;

  var
    CurrentWord: string;
    CurrentLevel: string;
    Heredoc:  string;
  var
    is_jump_string: byte;
    IsHeredoc: byte;
  begin

    Initialize(FileSourceCode, FileResult, Currentword, Currentlevel, state, is_jump_string, head_function_list);

    while not EOF(FileSourceCode) do
    begin
      Readln(FileSourceCode, buffer);
      if (State = ScanHeredoc) and ((copy(buffer, 1, length(Heredoc)) + ';') = (Heredoc + ';')) then
        IsHeredoc := 0
      else
        IsHeredoc := 1;
      iterator_main := 1;
      while (iterator_main <= length(buffer)) do
      begin

        CurrentChar := buffer[iterator_main];

        if ((State <> ScanStringTwo) and (State <> ScanStringOne) and
          (State <> ScanCom) and (State <> ScanVar) and (State <> ScanHeredoc)) then
        begin

          case CurrentChar of

            '<': if (iterator_main >= TriplePrefix) and (copy(buffer, iterator_main - DoublePrefix, DoublePrefix) = '<<') then
              begin
                MakeStringMode(ScanHeredoc, is_jump_string, State, stack_pointer);
                is_jump_string := 0;
                Heredoc  := copy(buffer, iterator_main + 1, length(buffer) - iterator_main);
                break;
              end;

            '/': if (iterator_main >= DoublePrefix) and (buffer[iterator_main - 1] = '/') then
              begin
                iterator_main := iterator_main - 1;
                break;
              end;

            '#': break;


            '*': if (iterator_main >= DoublePrefix) and (buffer[iterator_main - 1] = '/') then
              begin
                MakeStringMode(ScanCom, is_jump_string, State, stack_pointer);
              end;
            '''':
            begin
              MakeStringMode(ScanStringOne, is_jump_string, State, stack_pointer);
            end;
            '"':
            begin
              MakeStringMode(ScanStringTwo, is_jump_string, State, stack_pointer);
            end;

          end;

        end;


        if ((State <> ScanCom) and (State <> ScanStringOne) and
          (CurrentChar = '$') and (State <> ScanVar)) then

          if (((State = ScanStringTwo) or (State = ScanHeredoc)) and
            (not EcranCheck(iterator_main - 1, buffer))) or ((State <> ScanStringTwo) and
            (State <> ScanHeredoc)) then
          begin
            CurrentWord := '';
            PushStack(State, stack_pointer);
            State := ScanVar;
            is_jump_string  := 1;
          end;


        StringRead(CurrentChar, buffer, Currentword, Currentlevel, iterator_main, state, is_jump_string, buffer_pointer, head_function_list, Heredoc, IsHeredoc);

        iterator_main := iterator_main + 1;
      end;


      if iterator_main <= length(buffer) then
        Delete(buffer, iterator_main, length(buffer) - iterator_main + 1);
      writeln(FileResult, buffer);
      CurrentWord := '';

    end;
    CloseFile(FileSourceCode);
    CloseFile(FileResult);
    buffer_pointer := head_function_list;
    buffer_pointer := buffer_pointer^.Next;
    WriteFunction(head_function_list);
    if state = scannormal then
      writeln('Scanned successfully!');
  end;



  procedure Execute;
  begin
    SetConsoleCP(1251);
    SetConsoleOutputCP(1251);
    stack_pointer := nil;
    New(stack_pointer);

    if FileExists('Source.txt') then
      MainScan()
    else
      Writeln('File does not exist');
  end;


begin

  Execute;

  Readln;

end.
