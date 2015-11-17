program Metric;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows;

const
  GLOBAL = ':GLOBAL:';
  LEXEME_COUNT = 11;
  DIVIDER = ';';
  STRINGS = '"';
  COLON  = ':';
  FORMAT = 3;
  CHAR_NEXT_LINE = #10;
  CHAR_ZERO = #0;
  SOURCE_FILE = 'Source.txt';



const
  WordSide = ' !<>[],./?;:-+=*&(){}'#10;
  LexemePrefix = ['a'..'z'] + ['_'];
  LexemeName = ['a'..'z'] + ['0'..'9'] + ['_'];
  Other = ['=', ':'];



type

  TypeStates = (ScanNormal, ScanProgramName, ScanParametres, ScanComment,
    ScanString, ScanOperator, ScanComplexOperator, ScanLexeme, ScanVariables, ScanValue);

  TypeLexeme = (LexemeBegin, LexemeEnd, LexemeDeclaration, LexemeCondition,
    LexemeCycle, LexemeGoTo, LexemeComment, LexemeProgram, LexemeValue, LexemeOther);

type

  TypeIdentificator = record
    Name:  string;
    level: string;
    is_procedure: boolean;
    number: integer;
  end;

type

  ///Stack Of States

  stack_of_states = ^stack_of_states_element;

  stack_of_states_element = record
    Data: TypeStates;
    operator_if_level: integer;
    operator_for_level: integer;
    Next: stack_of_states;
  end;

type

  ///Stack Of Segments

  stack_of_segments = ^stack_of_segments_element;

  stack_of_segments_element = record
    program_name: string;
    variables: array of TypeIdentificator;
    Next: stack_of_segments;
  end;

type
  //List of programs

  pointer_subprogram = ^TypeSubProgram;

  TypeSubProgram = record
    Balanse: integer;
    Name: string;
    Upper_level: string;
    Variables: array of array of TypeIdentificator;
    Next: pointer_subprogram;
  end;


type
  MainStatus = record
    current_char: char;
    current_word: string;
    current_program: string;
    head_subprogram_list: pointer_subprogram;
    maximum_level_condition: integer;
    maximum_level_cycle: integer;
    total_operator_count: integer;
    total_conditions_count: integer;
    total_cycles_count: integer;
    scanned_string: string;
    scanned_position: integer;
  end;



type
  //Convert Strings to TypeLexeme

  element_lexeme = record
    Name: string;
    correlation: Typelexeme;
  end;

  array_of_lexeme = array [0..LEXEME_COUNT] of element_lexeme;


var

  pointer_stack_states: stack_of_states;
  pointer_stack_segments: stack_of_segments;


  function FindProgram(call: string;
  var head_subprogram_list: pointer_subprogram): pointer_subprogram;
  var
    Current_pointer: Pointer_SubProgram;
  begin

    Current_pointer := head_subprogram_list;
    while (Current_pointer^.Next <> nil) and (Current_pointer^.Name <> call) do
      Current_pointer := Current_pointer^.Next;
    Result := Current_pointer;
  end;

  procedure ChangeBalanse(number_of_parametr: integer; Status: MainStatus);
  var
    current_pointer: pointer_subprogram;
  begin
    current_pointer := FindProgram(Status.current_program, Status.head_subprogram_list);
    if current_pointer <> nil then
    begin
      if number_of_parametr = 0 then
        current_pointer^.Balanse := current_pointer^.Balanse + 1
      else
        current_pointer^.Balanse := current_pointer^.Balanse - 1;
    end;
  end;

  function CheckProgramBalanse(var current_program: string;
    Status: MainStatus): boolean;
  var
    current_pointer: pointer_subprogram;
  begin
    Result := True;
    current_pointer := FindProgram(current_program, Status.head_subprogram_list);
    if current_pointer <> nil then
    begin
      if current_pointer^.Balanse = 0 then
        Result := True
      else
        Result := False;
    end;
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

  function TakeCurrentLevel(number_of_parametr: byte): integer;
  begin
    if pointer_stack_states = nil then
      Result := 0
    else
    begin
      if number_of_parametr = 0 then
        Result := pointer_stack_states^.operator_if_level
      else
        Result := pointer_stack_states^.operator_for_level;

    end;
  end;

  function TakeCurrentState(): TypeStates;
  begin
    if pointer_stack_states = nil then
      Result := ScanNormal
    else
      Result := pointer_stack_states^.Data;

  end;



  procedure InitializeArrayOfLexeme(var current_array: array_of_lexeme);
  const
    standart_lexeme_array: array [0..LEXEME_COUNT] of element_lexeme = (
      (Name: 'begin'; Correlation: LexemeBegin)
      , (Name: 'end'; Correlation: LexemeEnd)
      , (Name: 'integer'; Correlation: LexemeDeclaration)
      , (Name: 'real'; Correlation: LexemeDeclaration)
      , (Name: 'boolean'; Correlation: LexemeDeclaration)
      , (Name: 'goto'; Correlation: LexemeGoTo)
      , (Name: 'if'; Correlation: LexemeCondition)
      , (Name: 'for'; Correlation: LexemeCycle)
      , (Name: 'comment'; Correlation: LexemeComment)
      , (Name: 'procedure'; Correlation: LexemeProgram)
      , (Name: 'array'; Correlation: LexemeDeclaration)
      , (Name: 'value'; Correlation: LexemeValue)
      );
  var
    iterator: integer;
  begin
    for iterator := 0 to LEXEME_COUNT do
      current_array[iterator] := standart_lexeme_array[iterator];

  end;

  function ConvertLexeme(Source: string): TypeLexeme;
  var
    iterator: integer;
  var
    current_array: array_of_lexeme;
  begin
    InitializeArrayOfLexeme(current_array);
    Result := LexemeOther;
    for iterator := 0 to LEXEME_COUNT do
      if Source = current_array[iterator].Name then
        Result := current_array[iterator].correlation;

  end;

  /////////////////////////////////////////////////////////

  procedure CheckMaximumLevel(if_nesting, for_nesting: integer; var Status: Mainstatus);
  begin
    if if_nesting >= Status.maximum_level_condition then
      Status.maximum_level_condition := if_nesting;
    if for_nesting >= Status.maximum_level_cycle then
      Status.maximum_level_cycle := for_nesting;
  end;

  procedure PushState(State: typestates; if_nesting, for_nesting: integer;
  var current_stack: stack_of_states; var Status: Mainstatus);
  var
    current_pointer: stack_of_states;

  begin
    CheckMaximumLevel(if_nesting, for_nesting, Status);
    New(Current_pointer);
    Current_pointer^.Data := State;
    Current_pointer^.operator_if_level := if_nesting;
    Current_pointer^.operator_for_level := for_nesting;
    Current_pointer^.Next := current_stack;
    current_stack := current_pointer;

  end;

  function PopState(var current_stack: stack_of_states): stack_of_states_element;
  var
    Current_pointer: stack_of_states;
  begin
    if current_stack <> nil then
    begin
      current_pointer := current_stack;
      Result := Current_pointer^;
      current_stack := current_stack^.Next;
      Dispose(Current_pointer);
    end
    else
      Result.Data := ScanNormal;
    Result.operator_if_level := 0;
    Result.operator_for_level := 0;
    Result.Next := nil;
  end;


  procedure PushNewSegment(name_of_program: string;
  var current_stack: stack_of_segments);
  var
    Current_pointer: stack_of_segments;
  begin
    New(Current_pointer);
    Current_pointer^.program_name := name_of_program;
    setlength(Current_pointer^.variables, 0);
    Current_pointer^.Next := current_stack;
    current_stack := Current_pointer;
  end;

  function PopSegment(var current_stack: stack_of_segments): stack_of_segments_element;
  var
    Current_pointer: stack_of_segments;
  begin
    if current_stack <> nil then
    begin

      current_pointer := current_stack;
      Result := Current_pointer^;
      current_stack := current_stack^.Next;
      Dispose(Current_pointer);

    end
    else
      Result.program_name := GLOBAL;
    setlength(Result.variables, 0);
    Result.Next := nil;
  end;

  //////////////////////////////////////////

  //////////////////////////////////////////

  procedure AddSubProgram(Name: string; var head_subprogram_list: pointer_subprogram);
  var
    Current_pointer: pointer_subprogram;
  begin
    Current_pointer := head_subprogram_list;
    while Current_pointer^.Next <> nil do
      Current_pointer := Current_pointer^.Next;

    New(Current_pointer^.Next);
    Current_pointer := Current_pointer^.Next;
    Current_pointer^.balanse := 0;
    Current_pointer^.Name := Name;
    setlength(Current_pointer^.variables, 0, 0);
    Current_pointer^.Next := nil;

  end;


  //////////////////////////////////////////

  procedure OpenNewSegment(var Status: Mainstatus);
  begin
    Inc(Status.total_operator_count);
    ChangeBalanse(0, Status);
    PushState(ScanComplexOperator, TakeCurrentLevel(0), TakeCurrentLevel(1),
      pointer_stack_states, Status);
    PushNewSegment(Status.current_program, pointer_stack_segments);
  end;




  procedure WriteJilb(Status: Mainstatus);
  begin
    writeln('Total count: ', Status.total_operator_count);
    writeln;
    writeln('Total count [IF]: ', Status.total_conditions_count);
    writeln('Maximum Level of Nesting [IF]: ', Status.maximum_level_condition - 1);
    writeln('The ratio of operators to the others: ', (Status.total_conditions_count) /
      (Status.total_operator_count): FORMAT: FORMAT);
    writeln('Total count [FOR]: ', Status.total_cycles_count);
    writeln('Maximum Level of Nesting [FOR]: ', Status.maximum_level_cycle - 1);
    writeln('The ratio of operators to the others: ', (Status.total_cycles_count) /
      (Status.total_operator_count): FORMAT: FORMAT);
    writeln;
  end;

  procedure WriteProgramList(var Status: Mainstatus);
  const
    RESULT_FILE = 'Result.txt';
  var
    current_pointer: pointer_subprogram;
    output_file: textfile;
    iterator_level, iterator_line: integer;
  begin
    AssignFile(output_file, RESULT_FILE);
    Rewrite(output_file);
    current_pointer := Status.head_subprogram_list^.Next;
    while current_pointer <> nil do
    begin
      writeln(output_file, '////////////\\\\\\\\\\\');
      writeln(output_file, 'Level name:', current_pointer^.Name);
      writeln(output_file, '--------');
      for iterator_level := 0 to length(current_pointer^.Variables) - 1 do
      begin

        for  iterator_line :=
          0 to length(current_pointer^.Variables[iterator_level]) - 1 do
        begin
          if (not current_pointer^.Variables[iterator_level,
            iterator_line].is_procedure) and
            (current_pointer^.Variables[iterator_level, iterator_line].number >= 0) then
          begin
            writeln(output_file, 'Identificator: ',
              current_pointer^.Variables[iterator_level, iterator_line].Name);
            writeln(output_file, 'Spen:',
              current_pointer^.Variables[iterator_level, iterator_line].number);
          end;
        end;
        writeln(output_file, '--------');
      end;
      writeln(output_file, '\\\\\\\\\\\//////////');
      current_pointer := current_pointer^.Next;
    end;
    CloseFile(output_file);
    WriteJilb(Status);

  end;



  procedure AddProgramVariables(var Status: Mainstatus);
  var
    current_pointer: pointer_subprogram;
    local_length: integer;
    iterator: integer;
  begin
    current_pointer := FindProgram(Status.current_program, Status.head_subprogram_list);
    if current_pointer <> nil then
    begin
      local_length := length(current_pointer^.Variables);
      if (pointer_stack_segments <> nil) and
        (length(pointer_stack_segments^.Variables) <> 0) then
      begin
        setlength(current_pointer^.Variables, local_length + 1);
        setlength(current_pointer^.Variables[local_length],
          length(pointer_stack_segments^.Variables));
        for iterator := 0 to length(pointer_stack_segments^.Variables) - 1 do
        begin

          current_pointer^.Variables[local_length, iterator] :=
            pointer_stack_segments^.Variables[iterator];

        end;
      end;
    end;
  end;

  procedure CloseProgram(var Status: Mainstatus);
  var
    is_stack_full: boolean;
  begin

    if pointer_stack_segments <> nil then
      is_stack_full := True
    else
      is_stack_full := False;
    while (is_stack_full) and (pointer_stack_segments^.program_name =
        Status.current_program) do
    begin
      AddProgramVariables(Status);
      PopSegment(pointer_stack_segments);
      if pointer_stack_segments <> nil then
        is_stack_full := True
      else
        is_stack_full := False;
    end;
    if pointer_stack_segments <> nil then
      Status.current_program := pointer_stack_segments^.program_name
    else
      Status.current_program := GLOBAL;
  end;

  procedure PopOperatorLevels(Status: MainStatus; is_end: boolean);
  var

    is_stack_full: boolean;
  begin
    if pointer_stack_states <> nil then
      is_stack_full := True
    else
      is_stack_full := False;
    if (is_end = True) and (is_stack_full) then
    begin

      while (is_stack_full) and (TakeCurrentState() = ScanOperator) do
      begin

        PopState(pointer_stack_states);
        if pointer_stack_states <> nil then
          is_stack_full := True
        else
          is_stack_full := False;
      end;

    end;
  end;

  procedure CloseSegment(var Status: Mainstatus);
  begin

    PopOperatorLevels(Status, True);
    PopState(pointer_stack_states);

    ChangeBalanse(1, Status);
    if CheckProgramBalanse(Status.current_program, Status) then
      CloseProgram(Status)
    else
    begin
      AddProgramVariables(status);
      PopSegment(pointer_stack_segments);
    end;
  end;

  procedure OpenNewProgram(var Status: Mainstatus);
  begin
    if TakeCurrentState = ScanVariables then
      PopState(pointer_stack_states);
    PushState(ScanProgramName, 0, 0, pointer_stack_states, Status);
  end;

  function IsKeyWord(lexeme: string): boolean;
  const
    KEYWORDS_FILE = 'Keywords.txt';
  var
    KeyFile: TextFile;
    buffer_string: string;
  begin
    Result := False;
    if FileExists(KEYWORDS_FILE) then
    begin
      AssignFile(KeyFile, KEYWORDS_FILE);
      Reset(KeyFile);
      while (not EOF(KeyFile)) do
      begin
        Readln(KeyFile, buffer_string);
        if buffer_string = lexeme then
          Result := True;
      end;
      CloseFile(KeyFile);
    end;
  end;



  procedure AddIdentificator(is_procedure: boolean; var Status: Mainstatus);
  var
    buffer_length: integer;
  begin
    if pointer_stack_segments <> nil then
    begin
      buffer_length := length(pointer_stack_segments^.variables);
      setlength(pointer_stack_segments^.variables, buffer_length + 1);
      pointer_stack_segments^.variables[buffer_length].Name := Status.current_word;
      pointer_stack_segments^.variables[buffer_length].number := 0 - 1;
      pointer_stack_segments^.variables[buffer_length].is_procedure := is_procedure;
    end;
  end;

  procedure IncrementIdentificatorValues(var Status: Mainstatus);
  var
    current_pointer: stack_of_segments;
    iterator: integer;
    is_find:  boolean;
  begin
    is_find := False;
    current_pointer := pointer_stack_segments;
    while (not is_find) and (current_pointer <> nil) do
    begin
      for iterator := 0 to length(current_pointer^.variables) - 1 do
        if current_pointer^.variables[iterator].Name = Status.current_word then
        begin
          if (current_pointer^.variables[iterator].is_procedure) and
            (Status.current_program <> Status.current_word) then
            Inc(Status.total_operator_count)
          else
            current_pointer^.variables[iterator].Number :=
              current_pointer^.variables[iterator].Number + 1;
          is_find := True;
        end;

      current_pointer := current_pointer^.Next;
    end;
  end;

  function IsStandartProcedure(lexeme: string): boolean;
  const
    STANDART_SUBPROGRAMS_FILE = 'Standart.txt';
  var
    KeyFile: TextFile;
    buffer_string: string;
  begin
    Result := False;
    if FileExists(STANDART_SUBPROGRAMS_FILE) then
    begin
      AssignFile(KeyFile, STANDART_SUBPROGRAMS_FILE);
      Reset(KeyFile);
      while (not EOF(KeyFile)) do
      begin
        Readln(KeyFile, buffer_string);
        if buffer_string = lexeme then
          Result := True;
      end;
      CloseFile(KeyFile);
    end;
  end;

  procedure OtherOperations(var Status: Mainstatus);
  begin
    if (not IsKeyWord(Status.current_word)) and ((TakeCurrentState = ScanVariables))
    then
      AddIdentificator(False, Status);



    if (not IsKeyWord(Status.current_word)) then
      if IsStandartProcedure(Status.current_word) then
        Inc(Status.total_operator_count)
      else
        IncrementIdentificatorValues(Status);
  end;

  procedure AddConditionOperator(var Status: Mainstatus);
  begin
    Inc(Status.total_operator_count);
    Inc(Status.total_conditions_count);
    PushState(ScanOperator, TakeCurrentLevel(0) + 1, TakeCurrentLevel(1),
      pointer_stack_states, Status);
  end;

  procedure AddCycleOperator(var Status: Mainstatus);
  begin
    Inc(Status.total_cycles_count);
    PushState(ScanOperator, TakeCurrentLevel(0), TakeCurrentLevel(1) + 1,
      pointer_stack_states, Status);
  end;

  procedure CheckLexeme(var Status: Mainstatus);
  var
    current_lexeme: TypeLexeme;
  begin

    current_lexeme := ConvertLexeme(ansilowercase(Status.current_word));
    PopState(pointer_stack_states);
    case current_lexeme of
      LexemeBegin: OpenNewSegment(Status);
      LexemeEnd: CloseSegment(Status);
      LexemeDeclaration: PushState(ScanVariables, TakeCurrentLevel(0),
          TakeCurrentLevel(1), pointer_stack_states, Status);
      LexemeCondition: AddConditionOperator(Status);
      LexemeCycle: AddCycleOperator(Status);
      LexemeGoTo: Inc(Status.total_operator_count);
      LexemeComment: PushState(ScanComment, TakeCurrentLevel(0),
          TakeCurrentLevel(1), pointer_stack_states, Status);
      LexemeProgram: OpenNewProgram(Status);
      LexemeValue: PushState(ScanValue, TakeCurrentLevel(0),
          TakeCurrentLevel(1), pointer_stack_states, Status);
      LexemeOther: OtherOperations(Status);
      else
    end;
  end;


  procedure InitializeMain(var FileSourceCode: TextFile; var Status: Mainstatus);
  begin
    AssignFile(FileSourceCode, SOURCE_FILE);

    Reset(FileSourceCode);
    Status.scanned_position := 0;
    Status.scanned_string := '';
    Status.maximum_level_condition := 0;
    Status.maximum_level_cycle := 0;
    Status.total_operator_count := 0;
    Status.total_conditions_count := 0;
    Status.total_cycles_count := 0;
    PushState(ScanNormal, 0, 0, pointer_stack_states, Status);
    Status.current_program := GLOBAL;
    Status.head_subprogram_list := nil;
    New(Status.head_subprogram_list);
    Status.head_subprogram_list^.Name := '';
    Status.head_subprogram_list^.Next := nil;
    AddSubProgram(GLOBAL, Status.head_subprogram_list);

    Status.current_word := '';
    Status.current_char := CHAR_ZERO;
  end;


  procedure StringOperations(var Status: MainStatus);
  begin
    if Status.current_char = STRINGS then
      PopState(pointer_stack_states);
  end;

  procedure CommentOperations(var Status: MainStatus);
  begin
    if Status.current_char = DIVIDER then
      PopState(pointer_stack_states);
  end;

  procedure ValueModeOperations(var Status: MainStatus);
  begin
    if Status.current_char = DIVIDER then
      PopState(pointer_stack_states);
  end;

  procedure OrdinaryOperations(var Status: MainStatus);
  begin
    if Status.current_char = STRINGS then
      PushState(ScanString, TakeCurrentLevel(0), TakeCurrentLevel(1),
        pointer_stack_states, Status)

    else if Status.current_char in LexemePrefix then
    begin
      PushState(ScanLexeme, TakeCurrentLevel(0), TakeCurrentLevel(1),
        pointer_stack_states, Status);
      Status.current_word := status.current_word + Status.current_char;
    end;
  end;

  procedure LexemeOperations(var Status: Mainstatus);
  begin
    if FindPosition(Status.current_char, WordSide) <> 0 then
    begin

      CheckLexeme(Status);
      Status.current_word := '';
      if Status.current_char = DIVIDER then
        PopState(pointer_stack_states);
    end
    else
      Status.current_word := Status.current_word + Status.current_char;
  end;

  procedure ProgramNameOperations(var Status: Mainstatus);
  begin
    if FindPosition(Status.current_char, WordSide) <> 0 then
    begin
      AddSubProgram(Status.current_word, Status.head_subprogram_list);
      AddIdentificator(True, Status);
      Status.current_program := Status.current_word;


      PushNewSegment(Status.current_program, pointer_stack_segments);
      AddIdentificator(False, Status);


      if Status.current_char = DIVIDER then
        PopState(pointer_stack_states)

      else
        PushState(ScanParametres, TakeCurrentLevel(0), TakeCurrentLevel(1),
          pointer_stack_states, Status);

      Status.current_word := '';
    end
    else
      Status.current_word := Status.current_word + Status.current_char;
  end;

  procedure ParametresOperations(var Status: Mainstatus);
  begin
    if Status.current_char = DIVIDER then
    begin
      PopState(pointer_stack_states);
      PopState(pointer_stack_states);
    end;
  end;

  procedure PreparatoryOperations(var Status: Mainstatus; var is_end: boolean);
  begin
    if (TakeCurrentState <> ScanString) and (TakeCurrentState <> ScanComment) then
      if (Status.current_char = COLON) and
        (copy(Status.scanned_string, Status.scanned_position, 2) = ':=') then
        Inc(Status.total_operator_count)
      else if (TakeCurrentState <> ScanParametres) and
        (Status.current_char = DIVIDER) then
        is_end := True;

  end;



  procedure ReadString(var Status: MainStatus;
  var head_subprogram_list: pointer_subprogram);
  var
    is_end: boolean;
  begin
    is_end := False;
    PreparatoryOperations(status, is_end);

    case TakeCurrentState of

      ScanString: StringOperations(Status);

      ScanComment: CommentOperations(Status);

      ScanNormal: OrdinaryOperations(Status);

      ScanComplexOperator: OrdinaryOperations(Status);

      ScanLexeme: LexemeOperations(Status);

      ScanProgramName: ProgramNameOperations(Status);

      ScanParametres: ParametresOperations(Status);

      ScanVariables: OrdinaryOperations(Status);

      ScanOperator: OrdinaryOperations(Status);

      ScanValue: ValueModeOperations(Status);

    end;

    PopOperatorLevels(Status, is_end);
  end;

  function LowCase(current_char: char): char;
  const
    DISTANSE = 32;
  begin
    if (current_char in ['a'..'z']) then
      Result := chr(Ord(current_char) + DISTANSE)
    else
      Result := current_char;
  end;




  procedure MainScanning();
  var
    Status: MainStatus;

  var
    current_string: string;
    iterator_main:  integer;

  var
    FileSourceCode: textfile;

  begin
    InitializeMain(FileSourceCode, Status);

    while not EOF(FileSourceCode) do
    begin
      iterator_main := 1;

      Readln(FileSourceCode, current_string);
      current_string := current_string + CHAR_NEXT_LINE;
      current_string := ansilowercase(current_string);
      Status.scanned_string := current_string;

      while (iterator_main <= length(current_string)) do
      begin
        Status.current_char := (current_string[iterator_main]);
        Status.scanned_position := iterator_main;
        ReadString(Status, Status.head_subprogram_list);
        iterator_main := iterator_main + 1;
      end;
    end;
    CloseFile(FileSourceCode);

    if TakeCurrentState = ScanNormal then
      writeln('Scanned successfully!');
    WriteProgramList(Status);
  end;



  procedure Execute;
  begin
    pointer_stack_states := nil;
    pointer_stack_segments := nil;


    if FileExists(SOURCE_FILE) then
      MainScanning()
    else
      Writeln('File does not exist');
    Readln;
  end;


begin
  Execute;

end.

