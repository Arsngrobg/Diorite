// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Transform.fs
// Summary: The functions for transforming diorite source files into token streams and subsequently ASTs
// Author:  Arsngrobg, Borngle
// Version: v1.7
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

// separate module for helper functions since we have a lot of those
[<RequireQualifiedAccess>]
module private Transformers =
    let stringToChars (str: string): char list =
        [ for c in str do c ]

    let parseDigit (c: char): int =
        int c - int '0'

    let charsToString (chars: char list): string =
        System.String.Concat chars

    let parseNumber (str: char list): float =
        str |> charsToString |> System.Double.Parse

// predicates for the consume function
[<RequireQualifiedAccess>]
module private Predicates =
    let isLetter (c: char): bool =
        System.Char.IsLetter c

    let isDigit (c: char): bool =
        System.Char.IsDigit c

    let isBlank (c: char): bool =
        c <> '\n' && System.Char.IsWhiteSpace c

    let any (c: char): bool =
        not(isBlank c)

/// <summary>
///     The <c>Lexer</c> module groups up related bindings that represent the tokenization stage of the code
///     transformation. The tokens are then passed to the <c>Parser</c> module to extract meaning from the token
///     stream.
///     <code>
///         let tokens = Lexer.lex("x = 2")
///         printf $"{tokens}" // output: "[VARIABLE "x", EQUALS, NUMBER 2]"
///     </code>
/// </summary>
[<RequireQualifiedAccess>]
module Lexer =
    /// <summary>
    ///     All the accepted tokens in the <b>Diorite</b> language.
    ///     <c>IllegalToken</c> is any illegal string and is used for error checking.
    /// </summary>
    type Token =
        // lexing halts when this is discovered by the lexer and is used for syntax errors
        | IllegalToken      of string // contains the offending lexeme

        // value types
        | Number            of float   // contains the number literal
        | Identifier        of char    // contains the character
        | Symbol            of string  // contains the symbol name

        // constants
        | Undefined
        | Infinity
        | Pi
        | Tau

        // integral operator
        | Tick

        // boundary operators
        | Colon
        | Arrow

        // comparison operators
        | Equals
        | LessThan
        | GreaterThan
        | LessThanOrEqual
        | GreaterThanOrEqual
        | NotEqual

        // arithmetic operators
        | Exponent
        | Factorial
        | Multiply
        | Divide
        | Percentage
        | Plus
        | Subtract
        | Bar

        // control flow
        | If
        | Otherwise

        // parenthesis, brackets, and braces
        | LeftParenthesis
        | RightParenthesis
        | LeftBracket
        | RightBracket
        | LeftBrace
        | RightBrace

    /// <summary>
    ///     A debug function for outputting the tokens in a structured manner from a supplied token stream.
    /// </summary>
    /// <param name="tokens"> the <c>Lexer.Token</c> stream </param>
    let rec tokens2str (tokens: Token list): string =
        match tokens with
         | []        -> ""
         | t :: tail -> $"({t}) {tokens2str tail}"
    
    // recursively consume character given that they satisfy the given predicate
    let rec private consume (predicate: char -> bool) (src: char list): char list * char list =
        match src with
         | c :: tail when predicate c ->
            let (consumed: char list), (remaining: char list) = consume predicate tail
            (c :: consumed, remaining)
         | _ -> ([], src)

    /// <summary>
    ///     Searches through the list in order until it reaches an <c>IllegalToken</c>.
    ///     If it does reach an <c>IllegalToken</c>, the function will return a <c>IO.SyntaxError</c> containing a
    ///     message which states what the illegal token is.
    ///     <code>
    ///         let tokens: Token list = [Number 2; Plus; Number 2; IllegalToken ","]
    ///         let error: IO.DioriteError = Lexer.getError tokens
    ///         IO.output $"{error}" |> ignore // output: "SyntaxError "Unexpected token: ','"
    ///     </code>
    /// </summary>
    /// <param name='tokens'> the tokens to check for an <c>IllegalToken</c> </param>
    /// <returns> the first instance of <c>IllegalToken</c> in the list or <c>None</c> if no error </returns>
    let rec getError (tokens: Token list): IO.DioriteError option =
        match tokens with
         | []                     -> None
         | IllegalToken t :: _    -> Some ($"Unexpected token: '{t}'" |> IO.SyntaxError)
         | _              :: tail -> getError tail

    /// <summary>
    ///     Converts the supplied <c>src</c> string into a stream of tokens.
    /// </summary>
    /// <param name='src'> the raw string to be tokenized </param>
    /// <returns> a <c>IO.Result</c> that may contain the list of tokens or a <c>LexerError</c> </returns>
    let lex (src: string): Token list =
        let rec scan (src: char list): Token list =
            match src with
             | [] -> []

             // numbers
             | c :: tail when Predicates.isDigit c ->
                 let (integerComponent: char list), (remaining: char list) = consume Predicates.isDigit (c :: tail)
                 match remaining with
                 // with decimal component
                  | '.' :: tail ->
                      match consume Predicates.isDigit tail with
                      // produce two separate tokens to say that the '.' is an illegal token after the number
                       | [], remaining ->
                           (integerComponent |> Transformers.parseNumber |> Number) ::
                           IllegalToken "." ::
                           scan remaining
                       | decimalComponent, postDecimal ->
                          let charSequence: char list = integerComponent @ ['.'] @ decimalComponent
                          (charSequence |> Transformers.parseNumber |> Number) :: scan postDecimal
                  // only integer component
                  | _ -> (integerComponent |> Transformers.parseNumber |> Number) :: scan remaining

             // identifiers (+ subscript), symbols & constants
             | c :: tail when Predicates.isLetter c ->
                 match consume Predicates.isLetter tail with
                 // identifiers
                  | [], remaining ->
                      match remaining with
                      // subscripts
                       | digit :: postSubscript when Predicates.isDigit digit ->
                           Identifier c :: (Transformers.parseDigit(digit) |> float |> Number) :: scan postSubscript
                           
                       // catches blank space between identifier and subscript, misses space after an identifier
                       | blank :: next :: _ when Predicates.isBlank blank && Predicates.isDigit next ->
                           IllegalToken (string c) :: scan tail
                           
                      // just a letter
                       | _ -> Identifier c :: scan remaining

                 // symbols, constants, and keywords
                  | chars, remaining ->
                      match Transformers.charsToString (c :: chars) with
                       // keywords
                       | "if"               -> If         :: scan remaining
                       | "otherwise"        -> Otherwise  :: scan remaining

                       // constants
                       | "undefined"        -> Undefined  :: scan remaining
                       | "infinity" | "inf" -> Infinity   :: scan remaining
                       | "pi"               -> Pi         :: scan remaining
                       | "tau"              -> Tau        :: scan remaining

                       // symbols
                       | sym                -> Symbol sym :: scan remaining

             // integral operator
             | '\'' :: tail -> Tick :: scan tail

             // boundary operators
             | '-' :: '>' :: tail -> Arrow :: scan tail
             | ':'        :: tail -> Colon :: scan tail

             // comparison operators
             | '<' :: '=' :: tail -> LessThanOrEqual    :: scan tail
             | '>' :: '=' :: tail -> GreaterThanOrEqual :: scan tail
             | '!' :: '=' :: tail -> NotEqual           :: scan tail
             | '='        :: tail -> Equals             :: scan tail

             // arithmetic operators
             | '^'        :: tail -> Exponent    :: scan tail
             | '!'        :: tail -> Factorial   :: scan tail
             | '*'        :: tail -> Multiply    :: scan tail
             | '/'        :: tail -> Divide      :: scan tail
             | '%'        :: tail -> Percentage  :: scan tail
             | '+'        :: tail -> Plus        :: scan tail
             | '-'        :: tail -> Subtract    :: scan tail
             | '|'        :: tail -> Bar         :: scan tail
             | '<'        :: tail -> LessThan    :: scan tail
             | '>'        :: tail -> GreaterThan :: scan tail

             // brackets, curly braces & square brackets
             | '(' :: tail -> LeftParenthesis  :: scan tail
             | ')' :: tail -> RightParenthesis :: scan tail
             | '{' :: tail -> LeftBrace        :: scan tail
             | '}' :: tail -> RightBrace       :: scan tail
             | '[' :: tail -> LeftBracket      :: scan tail
             | ']' :: tail -> RightBracket     :: scan tail
             
             // comment (no token just ignores)
             | '#' :: tail ->
                let _, remaining = consume (fun c -> c <> '\n') tail
                scan remaining

             // skip whitespace (not newlines)
             | c :: _ when Predicates.isBlank c ->
                 let _, (remaining: char list) = consume Predicates.isBlank src
                 scan remaining

             // illegal token
             | _ ->
                 let (lexeme: char list), (remaining: char list) = consume Predicates.any src
                 (Transformers.charsToString(lexeme) |> IllegalToken) :: scan remaining

        src |> Transformers.stringToChars |> scan

/// <summary>
///     The <c>Parser</c> module groups up related bindings for parsing a token stream.
/// </summary>
[<RequireQualifiedAccess>]
module Parser =
    /// <summary>
    ///     The number sets supported by the <c>SetHint</c> feature.
    /// </summary>
    type NumberSet =
        | Natural    // N = {1, ..., ∞}
        | Integer    // Z = {-∞, ..., 0, ..., ∞}
        | Real       // R = {Q & I}
        | Rational   // Q = {x where x = a/b & b != 0}
        | Irrational // I = {x where x != a/b}
        | Complex    // C = {x where x = a + bi}

    /// <summary>
    ///     The linkage type of functions.
    ///     <c>Internal</c> linkage means that it is locally defined within the source file.
    ///     <c>External</c> linkage means that it is defined in the backend.
    /// </summary>
    type Linkage =
        | Internal of string
        | External of string

    /// <summary>
    ///     The metadata for a function.
    ///     <c>symbol</c> is the optional string value that also represents this function.
    ///     <c>inlined</c> is a tuple of <c>bool</c>s where it is of the pattern: <c>enabled * forced</c>.
    ///     <c>memoized</c> is a tuple of <c>bool</c>s where it is of the pattern: <c>enabled * forced</c>.
    /// </summary>
    type FunctionMetadata = {
        symbol:   string      option // optional, meaningful name that persists throughout the entire program
        inlined:  bool * bool        // (enabled, forced)
        memoized: bool * bool        // (enabled, forced)
    }

    /// <summary>
    ///     The attributes of a function.
    ///     If no <c>NumberSet</c> is provided to a parameter or the return type - it defaults to <c>Real</c>.
    /// </summary>
    type FunctionAttributes = {
        identifier: char * int option                      // the variable name
        parameters: ((char * int option) * NumberSet) list // parameter name    - defaults: Real
        returns:    NumberSet                              // return 'type'     - default:  Real
        metadata:   FunctionMetadata
    }

    /// <summary>
    ///     The <c>Node</c> type is a discriminated union which describes the structure of the AST of the <b>Diorite</b>
    ///     language.
    /// </summary>
    type Node =
        // values
        | Number          of float
        | Identifier      of id: char * subscript: int option

        // unary operations
        | Integration
        | Differentiation
        | Percentage
        | Positive
        | Negative

        // binary operations
        | Multiplication
        | Division
        | Modulo
        | Addition
        | Subtraction

        // comparison operations
        | Equals
        | NotEquals
        | GreaterThan
        | LessThan
        | GreaterThanOrEqual
        | LessThanOrEqual

        // structure
        | Begin            of Node list
        | Expression       of left:       Node               * operator:    Node      * right: Node
        | Comparison       of left:       Node               * operator:    Node      * right: Node
        | Conditions       of cases:      Node list          * defaultCase: Node
        | FunctionDef      of data:       FunctionAttributes * body:        Node
        | FunctionCall     of identifier: Node               * arguments:   Node list

    // matches the provided token with the series of expected tokens - returns the token it matched with
    let rec private matchIfExpected (token: Lexer.Token) (expected: Lexer.Token list): Lexer.Token option =
        match expected with
         | []                             -> None
         | head :: _    when token = head -> Some head
         | _    :: tail                   -> matchIfExpected token tail

    let parse(tokens: Lexer.Token list): Node =
        Begin []
