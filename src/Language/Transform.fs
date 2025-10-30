// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Transform.fs
// Summary: The bindings for transforming a stream of characters to a TokenStream or Abstract Syntax Tree (AST)
// Author:  Arsngrobg, Borngle
// Version: v1.11
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>Lexer</c> module groups up related bindings that represent the tokenization stage of the code
///     transformation. The tokens are then passed to the <c>Parser</c> module to extract meaning from the token
///     stream.
///     <code>
///         let tokens = Lexer.tokenize("x = 2")
///         IO.output $"{tokens}\n" |> ignore // output: "[VARIABLE "x", EQUALS, NUMBER 2]"
///     </code>
/// </summary>
[<RequireQualifiedAccess>]
module Lexer =
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
            System.Char.IsWhiteSpace c

        let untilNewline (c: char): bool =
            c <> '\n'

        let isNewline (c: char): bool =
            c = '\n'

        let any (c: char): bool =
            not(isBlank c)

    /// <summary>
    ///     All the valid tokens that can be accepted in the <b>Diorite</b> language.
    ///     <c>IllegalToken</c> is used to determine errors in source files / input.
    /// </summary>
    type Token =
        // lexing continues upon discovering an IllegalToken as it helps with finding all illegal tokens
        | IllegalToken      of lexeme: string                   // contains the offending lexeme

        // value types
        | Number            of value: float                     // contains the number literal
        | Identifier        of id: char * subscript: int option // contains the character + optional subscript
        | Symbol            of value: string                    // contains the symbol name

        // reserved words
        | Undefined
        | Infinity

        // symbolic constants
        | Pi
        | Tau
        | Euler

        // integral operator
        | Tick

        // boundary operators
        | Colon
        | Arrow

        // args & params
        | Comma

        // comparison operators
        | Equals
        | LessThan
        | GreaterThan
        | LessThanOrEqual
        | GreaterThanOrEqual
        | NotEqual

        // arithmetic operators
        | Hat
        | Exclamation
        | Asterisk
        | ForwardSlash
        | Percentage
        | Plus
        | Hyphen
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
        
        // end of statement
        | SemiColon

    /// <summary>
    ///     A descriptive wrapper for a <c>Token</c> list.
    /// </summary>
    type TokenStream = Token list

    // TODO: move to logging
    /// <summary>
    ///     A debug function for outputting the tokens in a structured manner from a supplied token stream.
    /// </summary>
    /// <param name='tokens'> the <c>Lexer.Token</c> stream </param>
    let rec tokens2str (tokens: TokenStream): string =
        match tokens with
         | []                   -> ""
         | SemiColon :: tail -> $"\n{tokens2str tail}"
         | t :: tail            -> $"({t}) {tokens2str tail}"

    /// <summary>
    ///     Reads the current statement from the head of this <c>TokenStream</c> to see if it contains the supplied
    ///     <c>token</c>.
    /// </summary>
    /// <param name='stream'> the token steam </param>
    /// <param name='token'> the token to check for this current statement </param>
    /// <returns> <c>true</c> if the supplied <c>Token</c> is within the current statement </returns>
    let rec statementContainsToken (stream: TokenStream) (token: Token): bool =
        match stream with
         | [] | SemiColon :: _            -> false
         | head :: _    when head = token -> true
         | _    :: tail                   -> statementContainsToken tail token

    /// <summary>
    ///     Reads the token stream to see if it contains the supplied <c>token</c>.
    /// </summary>
    /// <param name='stream'> the token steam </param>
    /// <param name='token'> the token to seek for </param>
    /// <returns> <c>true</c> if the supplied <c>Token</c> is in the <c>TokenStream</c> </returns>
    let rec streamContainsToken (stream: TokenStream) (token: Token): bool =
        match stream with
         | []                             -> false
         | head :: _    when head = token -> true
         | _    :: tail                   -> streamContainsToken tail token
    
    // recursively consume character given that they satisfy the given predicate
    let rec private consume (predicate: char -> bool) (src: char list): char list * char list =
        match src with
         | c :: tail when predicate c ->
            let (consumed: char list), (remaining: char list) = consume predicate tail
            (c :: consumed, remaining)
         | _ -> ([], src)

    /// <summary>
    ///     Searches through the list in order until it reaches an <c>IllegalToken</c>.
    ///     If it does reach an <c>IllegalToken</c>, the function will return a <c>SyntaxError</c> containing a
    ///     message which states what the illegal token is.
    ///     <code>
    ///         let tokens: TokenStream = [Number 2; Plus; Number 2; IllegalToken ","]
    ///         let error: DioriteError = Lexer.getError tokens
    ///         IO.output $"{error}" |> ignore // output: "SyntaxError "Unexpected token: ','"
    ///     </code>
    /// </summary>
    /// <param name='tokens'> the tokens to check for an <c>IllegalToken</c> </param>
    /// <returns> the first instance of <c>IllegalToken</c> in the list or <c>None</c> if no error </returns>
    let rec getError (tokens: TokenStream): DioriteError option =
        match tokens with
         | []                     -> None
         | IllegalToken t :: _    -> Some (DioriteError.SyntaxError $"Unexpected token: '{t}'")
         | _              :: tail -> getError tail

    /// <summary>
    ///     Converts the supplied <c>src</c> string into a stream of tokens.
    /// </summary>
    /// <param name='src'> the raw string to be tokenized </param>
    /// <returns> a <c>Result</c> that may contain the list of tokens or a <c>LexerError</c> </returns>
    let tokenize (src: string): TokenStream =
        let rec scan (src: char list): TokenStream =
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
                           (Transformers.parseNumber >> Number) integerComponent ::
                           IllegalToken "." ::
                           scan remaining
                       | decimalComponent, postDecimal ->
                          let charSequence: char list = integerComponent @ ['.'] @ decimalComponent
                          (Transformers.parseNumber >> Number) charSequence :: scan postDecimal
                  // only integer component
                  | _ -> (Transformers.parseNumber >> Number) integerComponent :: scan remaining

             // identifiers (+ subscript), symbols & constants
             | c :: tail when Predicates.isLetter c ->
                 match consume Predicates.isLetter tail with
                 // identifiers
                  | [], remaining ->
                      match remaining with
                      // subscripts
                       | digit :: postSubscript when Predicates.isDigit digit ->
                           Identifier (c, (Transformers.parseDigit >> Some) digit) :: scan postSubscript
                           
                      // just a letter
                       | _ -> Identifier (c, None) :: scan remaining

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
                       | "euler"            -> Euler      :: scan remaining

                       // symbols
                       | sym                -> Symbol sym :: scan remaining

             // args & params
             | ','  :: tail       -> Comma              :: scan tail

             // integral operator
             | '\'' :: tail       -> Tick               :: scan tail

             // boundary operators
             | '-' :: '>' :: tail -> Arrow              :: scan tail
             | ':'        :: tail -> Colon              :: scan tail

             // comparison operators
             | '<' :: '=' :: tail -> LessThanOrEqual    :: scan tail
             | '>' :: '=' :: tail -> GreaterThanOrEqual :: scan tail
             | '!' :: '=' :: tail -> NotEqual           :: scan tail
             | '='        :: tail -> Equals             :: scan tail

             // arithmetic operators
             | '^'        :: tail -> Hat                :: scan tail
             | '!'        :: tail -> Exclamation        :: scan tail
             | '*'        :: tail -> Asterisk           :: scan tail
             | '/'        :: tail -> ForwardSlash       :: scan tail
             | '%'        :: tail -> Percentage         :: scan tail
             | '+'        :: tail -> Plus               :: scan tail
             | '-'        :: tail -> Hyphen             :: scan tail
             | '|'        :: tail -> Bar                :: scan tail
             | '<'        :: tail -> LessThan           :: scan tail
             | '>'        :: tail -> GreaterThan        :: scan tail

             // brackets, curly braces & square brackets
             | '('        :: tail -> LeftParenthesis    :: scan tail
             | ')'        :: tail -> RightParenthesis   :: scan tail
             | '{'        :: tail -> LeftBrace          :: scan tail
             | '}'        :: tail -> RightBrace         :: scan tail
             | '['        :: tail -> LeftBracket        :: scan tail
             | ']'        :: tail -> RightBracket       :: scan tail
             
             // end of statement
             | ';'        :: tail -> SemiColon          :: scan tail
             
             // comment (no token just ignores)
             | '#' :: tail ->
                let _, (remaining: char list) = consume Predicates.untilNewline tail
                scan remaining

             // skip whitespace
             | c :: _ when Predicates.isBlank c ->
                 let _, (remaining: char list) = consume Predicates.isBlank src
                 scan remaining

             // illegal token
             | _ ->
                 let (lexeme: char list), (remaining: char list) = consume Predicates.any src
                 (Transformers.charsToString(lexeme) |> IllegalToken) :: scan remaining

        (Transformers.stringToChars >> scan) src

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
    ///     Gets the equivalent <c>NumberSet</c> for the supplied <c>ch</c> character.
    /// </summary>
    /// <param name='ch'> the character to match with the respective <c>NumberSet</c> </param>
    /// <returns> the equivalent <c>NumberSet</c> if it matches; <c>None</c> otherwise </returns>
    let getNumberSet (ch: char): NumberSet option =
        match ch with
         | 'N' -> Some Natural
         | 'Z' -> Some Integer
         | 'R' -> Some Real
         | 'Q' -> Some Rational
         | 'I' -> Some Irrational
         | 'C' -> Some Complex
         |  _  -> None

    /// <summary>
    ///     The metadata for a function.
    ///     <c>symbol</c> is the optional string value that also represents this function.
    ///     <c>inlined</c> is a <b>bool</b> which indicates whether the function is inlined.
    ///     <c>memoized</c> is a <b>bool</b> which indicates whether the function is memoized.
    /// </summary>
    [<Struct>]
    type FunctionMetadata = {
        symbol:   string option // optional, meaningful name that persists throughout the lifetime of the program
        inlined:  bool          // enabled/disabled
        memoized: bool          // enabled/disabled
    }

    /// <summary>
    ///     The default <c>FunctionMetadata</c>.
    ///     (Everything is disabled or <c>None</c>)
    /// </summary>
    let defaultMetadata: FunctionMetadata = {
        inlined  = false
        memoized = false
        symbol   = None
    }

    /// <summary>
    ///     A <c>FunctionParameter</c> is a tuple consisting of an identifier and a number set.
    ///     By default, the number set is <c>Real</c>.
    /// </summary>
    type FunctionParameter = (char * int option) * NumberSet

    /// <summary>
    ///     The attributes of a function.
    ///     If no <c>NumberSet</c> is provided to a parameter or the return type - it defaults to <c>Real</c>.
    /// </summary>
    [<Struct>]
    type FunctionAttributes = {
        identifier: char * int option      // the variable name
        parameters: FunctionParameter list // parameter name    - defaults: Real
        returns:    NumberSet              // return 'type'     - default:  Real
        metadata:   FunctionMetadata
    }

    /// <summary>
    ///     A function name is either a symbolic name or a <c>Identifier</c> name.
    /// </summary>
    type FunctionName =
        | Symbolic     of string
        | Identifiable of id: char * subscript: int option

    /// <summary>
    ///     The <c>ASTNode</c> type is a discriminated union which describes the structure of the AST of the <b>Diorite</b>
    ///     language.
    /// </summary>
    [<AutoOpen>]
    type ASTNode =
        // values
        | Number          of float
        | Identifier      of id: char * subscript: int option

        // reserved words
        | Undefined
        | Infinity

        // unary operations
        | Integration
        | Differentiation
        | Percentage
        | Positive
        | Negative
        | Absolution

        // binary operations
        | Factorial
        | Exponentiation
        | Multiplication
        | Division
        | Modulo
        | Addition
        | Subtraction

        // comparison operations
        | Equals
        | NotEqual
        | GreaterThan
        | LessThan
        | GreaterThanOrEqual
        | LessThanOrEqual

        // structure
        | Begin            of ASTNode list
        | BinaryOperation  of left:       ASTNode            * operator:    ASTNode      * right: ASTNode
        | UnaryOperation   of operand:    ASTNode            * operator:    ASTNode
        | Comparison       of ifTrue: ASTNode * left: ASTNode * operator: ASTNode * right: ASTNode
        | Conditions       of cases:      ASTNode list       * defaultCase: ASTNode
        | FunctionDef      of data:       FunctionAttributes * body:        ASTNode
        | FunctionCall     of name:       FunctionName       * arguments:   ASTNode list

    // the parser uses the idea of parser combinators for the parsing strategy
    // each stage of the parser is a parser within itself
    // very good resource:
    // https://tgdwyer.github.io/parsercombinators

    /// <summary>
    ///     The <c>ParseState</c> is the return type of successful <c>Parser</c> invocation.
    ///     It contains the <c>'a</c> which indicates a successful parse of an arbitrary sequence of tokens; and the
    ///     remaining tokens to parse as a result of the current call to a <c>Parser</c>.
    /// </summary>
    type ParseState<'a> = 'a * Lexer.TokenStream

    /// <summary>
    ///     The <c>Parser</c> is a function that accepts a <c>TokenStream</c> and returns a <c>ParseState</c>.
    /// </summary>
    type Parser<'a> = Lexer.TokenStream -> ParseState<'a> Result

    /// <summary>
    ///     The function signature for a function which accepts a raw <c>ParseState</c> and uplifts it to a
    ///     <c>Result</c>.
    /// </summary>
    type ResultLifter<'a, 'b> = ParseState<'a> -> ParseState<'b> Result

    // helper function for subsequent execution upon an accepted parse state - handles error propagation automatically
    let private ifOk<'a, 'b> (prev: ParseState<'a> Result) (callback: ResultLifter<'a, 'b>): ParseState<'b> Result =
        match prev with
         | Error err   -> Error err
         | Ok    value -> callback value

    // <program> ::= <equation>
    //            |  <equation> <program>
    let rec program: Parser<ASTNode list> = (fun tokens ->
        ifOk (equation tokens) (fun (root, remaining) ->
            match remaining with
             // <program> ::= <equation> ";"
             | [] -> Ok ([root], remaining)
             // <program> ::= <equation> ";" <program>
             | programTail ->
                 ifOk (program programTail) (fun (roots, remaining) ->
                      Ok (root :: roots, remaining)
                 )
        )
    )
    // <equation> ::= <functiondef> "=" <functionbody>
    //             |  <identifier>  "=" <expression> ";"
    //             |  <expression> ";"
    and equation: Parser<ASTNode> = (fun tokens ->
        // lookahead to check for '=' token
        match Lexer.statementContainsToken tokens Lexer.Equals with
         // <equation> ::= <expression> ";"
         | false ->
             ifOk (expression tokens) (fun (node, remaining) ->
                 match remaining with
                  | Lexer.SemiColon :: remaining ->
                      Ok (node, remaining)
                  | _ -> SyntaxError "Missing semicolon for expression"
             )
         // <equation> ::= <functiondef> "=" <functionbody>
         //             |  <identifier>  "=" <expression> ";"
         | true ->
             match tokens with
              | Lexer.Identifier (ch, sb) :: Lexer.Equals :: equationTail ->
                  ifOk (expression equationTail) (fun (expression, remaining) ->
                      match remaining with
                       | Lexer.SemiColon :: remaining ->
                           Ok (BinaryOperation (Identifier (ch, sb), Equals, expression), remaining)
                       | _ -> SyntaxError "Missing semicolon for assignment"
                  )
              | _ ->
                 ifOk (functiondef tokens) (fun (functionAttributes, equationTail) ->
                     match equationTail with
                      | Lexer.Equals :: equationTail ->
                           ifOk (functionbody equationTail) (fun (functionBody, remaining) ->
                              Ok (FunctionDef (functionAttributes, functionBody), remaining)
                           )
                      | _ -> SyntaxError "Expected Equals token for function definition"
                 )
    )
    // <expression> ::= <term>
    //               |  <term> "+" <expression>
    //               |  <term> "-" <expression>
    and expression: Parser<ASTNode> = (fun tokens ->
        ifOk (term tokens) (fun (termNode, remaining) ->
            match remaining with
             // <expression> ::= <term> "+" <expression>
             | Lexer.Plus :: expressionTail ->
                 ifOk (expression expressionTail) (fun (expNode, remaining) ->
                     Ok (BinaryOperation (termNode, Addition, expNode), remaining)
                 )
             // <expression> ::= <term> "-" <expression>
             | Lexer.Hyphen :: expressionTail ->
                 ifOk (expression expressionTail) (fun (expNode, remaining) ->
                     Ok (BinaryOperation (termNode, Subtraction, expNode), remaining)
                 )
             // <expression> ::= <term>
             | remaining -> Ok (termNode, remaining)
        )
    )
    // <term> ::= <factor>
    //         |  <factor> "*" <term>
    //         |  <factor> "/" <term>
    //         |  <factor> "%" <term>
    and term: Parser<ASTNode> = (fun tokens ->
        ifOk (factor tokens) (fun (factorNode, remaining) ->
            match remaining with
             // <term> ::= <factor> "*" <term>
             | Lexer.Asterisk :: termTail ->
                 ifOk (term termTail) (fun (termNode, remaining) ->
                     Ok (BinaryOperation (factorNode, Multiplication, termNode), remaining)
                 )
             // <term> ::= <factor> "/" <term>
             | Lexer.ForwardSlash :: termTail ->
                 ifOk (term termTail) (fun (termNode, remaining) ->
                     Ok (BinaryOperation (factorNode, Division, termNode), remaining)
                 )
             // <term> ::= <factor> "%" <term>
             | Lexer.Percentage :: termTail ->
                 ifOk (term termTail) (fun (termNode, remaining) ->
                     Ok (BinaryOperation (factorNode, Modulo, termNode), remaining)
                 )
             // <term> ::= <factor>
             | remaining -> Ok (factorNode, remaining)
        )
    )
    // <factor> ::= <signed>
    //           |  <signed> "^" <signed>
    and factor: Parser<ASTNode> = (fun tokens ->
        ifOk (signed tokens) (fun (exponentNode, remaining) ->
            match remaining with
             // <factor> ::= <exponent> "^" <subexpression>
             | Lexer.Hat :: factorTail ->
                 ifOk (signed factorTail) (fun (subExpNode, remaining) ->
                     Ok (BinaryOperation (exponentNode, Exponentiation, subExpNode), remaining)
                 )
             // <factor> ::= <exponent>
             | remaining -> Ok (exponentNode, remaining)
        )
    )
    // <signed> ::= <exponent>
    //           |  "+" <signed>
    //           |  "-" <signed>
    and signed: Parser<ASTNode> = (fun tokens ->
        match tokens with
         | Lexer.Plus :: signedTail ->
             ifOk (signed signedTail) (fun (exponentNode, remaining) ->
                 Ok (UnaryOperation (exponentNode, Positive), remaining)
             )
         | Lexer.Hyphen :: signedTail ->
             ifOk (signed signedTail) (fun (exponentNode, remaining) ->
                 Ok (UnaryOperation (exponentNode, Negative), remaining)
             )
         | signedTail -> exponent signedTail
    )
    // <exponent> ::= <integral> <exponent'>
    and exponent: Parser<ASTNode> = (fun tokens ->
        ifOk (integral tokens) (fun (integralNode, exponentTail) ->
            ifOk (exponent' exponentTail) (fun state ->
                match state with
                 // <exponent> ::= <integral> <exponent'>
                 | Some factorialNode, remaining ->
                     Ok (UnaryOperation (integralNode, factorialNode), remaining)
                 // <exponent> ::= <integral>
                 | None, remaining -> Ok (integralNode, remaining)
            )
        )
    )
    // <exponent'> ::= ε
    //              |  "!" <exponent'>
    and exponent': Parser<ASTNode option> = (fun tokens ->
        match tokens with
         // <exponent'> ::= "!" <exponent'>
         | Lexer.Exclamation :: exponent'Tail ->
             ifOk (exponent' exponent'Tail) (fun state ->
                 match state with
                  | Some factorialNode, remaining ->
                      Ok ((UnaryOperation >> Some) (factorialNode, Factorial), remaining)
                  | None, remaining -> Ok (Some Factorial, remaining)
             )
         // <exponent'> ::= ε
         | exponent'Tail -> Ok (None, exponent'Tail)
    )
    // <integral> ::= <subexpression> <integral'>
    //             |  "'" <integral>
    and integral: Parser<ASTNode> = (fun tokens ->
        match tokens with
         // <integral> ::= "'" <integral>
         | Lexer.Tick :: integralTail ->
             ifOk (integral integralTail) (fun (integralNode, remaining) ->
                 Ok (UnaryOperation (integralNode, Integration), remaining)
             )
         // <integral> ::= <subexpression> <integral'>
         | integralTail ->
             ifOk (subexpression integralTail) (fun (subExpNode, integralTail) ->
                 ifOk (integral' integralTail) (fun state ->
                     match state with
                      // <integral> ::= <subexpression> <integral'>
                      | Some differentialNode, remaining ->
                          Ok (UnaryOperation (subExpNode, differentialNode), remaining)
                      // <integral> ::= <subexpression>
                      | None, remaining -> Ok (subExpNode, remaining)
                 )
             )
    )
    // <integral'> ::= ε
    //              |  "'" <integral'>
    and integral': Parser<ASTNode option> = (fun tokens ->
        match tokens with
         // <integral'> ::= "'" <integral'>
         | Lexer.Tick :: integral'Tail ->
             ifOk (integral' integral'Tail) (fun state ->
                 match state with
                  // <integral'> ::= "'" <integral'>
                  | Some differentialNode, remaining ->
                      Ok ((UnaryOperation >> Some) (differentialNode, Differentiation), remaining)
                  // <integral'> ::= "'"
                  | None, remaining -> Ok (Some Differentiation, remaining)
             )
         // <exponent'> ::= ε
         | integral'Tail -> Ok (None, integral'Tail)
    )
    // <subexpression> ::= <value>
    //                  |  "(" <expression> ")"
    //                  |  "|" <expression> "|"
    //                  |  <identifier> "(" <args> ")"
    //                  |  <letters>    "(" <args> ")"
    and subexpression: Parser<ASTNode> = (fun tokens ->
        match tokens with
         // <subexpression> ::= "|" <expression> "|"
         | Lexer.LeftParenthesis :: subexpressionTail ->
             ifOk (expression subexpressionTail) (fun (expressionNode, remaining) ->
                 match remaining with
                  | Lexer.RightParenthesis :: remaining ->
                      Ok (expressionNode, remaining)
                  | _ ->
                      SyntaxError "Missing closing parenthesis for expression"
             )
         // <subexpression> ::= "(" <expression> ")"
         | Lexer.Bar :: subexpressionTail ->
             ifOk (expression subexpressionTail) (fun (expressionNode, remaining) ->
                 match remaining with
                  | Lexer.Bar :: remaining ->
                      Ok (UnaryOperation (expressionNode, Absolution), remaining)
                  | _ ->
                      SyntaxError "Missing closing bar for absolute expression"
             )
         // <subexpression> ::= <identifier> "(" <args> ")"
         | Lexer.Identifier (ch, sb) :: Lexer.LeftParenthesis :: subexpressionTail ->
             ifOk (args subexpressionTail) (fun (argList, remaining) ->
                 match remaining with
                  | Lexer.RightParenthesis :: remaining ->
                      Ok (FunctionCall (Identifiable (ch, sb), argList), remaining)
                  | _ -> SyntaxError "Missing closing parenthesis for application"
             )
         // <subexpression> ::= <letters> "(" <args> ")"
         | Lexer.Symbol symbolicName :: Lexer.LeftParenthesis :: subexpressionTail ->
             ifOk (args subexpressionTail) (fun (argList, remaining) ->
                 match remaining with
                  | Lexer.RightParenthesis :: remaining ->
                      Ok (FunctionCall (Symbolic symbolicName, argList), remaining)
                  | _ -> SyntaxError "Missing closing parenthesis for symbolic function"
             )
         // <subexpression> ::= <value>
         | subexpressionTail -> value subexpressionTail
    )
    // <args> ::= <expression>
    //         |  <expression> "," <args>
    and args: Parser<ASTNode list> = (fun tokens ->
        ifOk (expression tokens) (fun (expressionNode, argsTail) ->
            match argsTail with
             // <args> ::= <expression> "," <args>
             | Lexer.Comma :: argsTail ->
                 ifOk (args argsTail) (fun (argsList, remaining) ->
                     Ok (expressionNode :: argsList, remaining)
                 )
             // <args> ::= <expression>
             | remaining -> Ok ([expressionNode], remaining)
        )
    )
    // <value> ::= "undefined"
    //          |  "infinity" | "inf"
    //          |  "pi"
    //          |  "tau"
    //          |  "euler"
    //          |  <letters> <digit>
    //          |  <number>
    and value: Parser<ASTNode> = (fun tokens ->
        match tokens with
         | Lexer.Undefined           :: remaining -> Ok (Undefined,           remaining)
         | Lexer.Infinity            :: remaining -> Ok (Infinity,            remaining)
         | Lexer.Pi                  :: remaining -> Ok (Number 3.1415926,    remaining)
         | Lexer.Tau                 :: remaining -> Ok (Number 6.2831853,    remaining)
         | Lexer.Euler               :: remaining -> Ok (Number 2.7182818,    remaining)
         | Lexer.Identifier (ch, sb) :: remaining -> Ok (Identifier (ch, sb), remaining)
         | Lexer.Number      number  :: remaining -> Ok (Number number,       remaining)
         | head                      :: _         -> SyntaxError $"Expected value - got {head} instead"
         | []                                     -> SyntaxError "Expected value when TokenStream empty"
    )
    // <functiondef> ::= <functionmeta> <identifier> "(" <functionparams> ")" <functionreturn>
    and functiondef: Parser<FunctionAttributes> = (fun tokens ->
        printf $"{tokens}\n"
        ifOk (functionmeta tokens) (fun (maybeMeta, functionTail) ->
            let functionMeta = match maybeMeta with
                               | None      -> defaultMetadata
                               | Some meta -> meta
            match functionTail with
             // <functiondef> ::= <functionmeta> <identifier> "(" <functionparams> ")" <functionreturn>
             | Lexer.Identifier (ch, sb) :: Lexer.LeftParenthesis :: functionTail ->
                 ifOk (functionparams functionTail) (fun (parameters, remaining) ->
                     match remaining with
                      | Lexer.RightParenthesis :: functionTail ->
                          ifOk (functionreturn functionTail) (fun (parsedReturn, remaining) ->
                              Ok ({
                                  identifier = (ch, sb)
                                  parameters = parameters
                                  returns    = match parsedReturn with None -> Real | Some set -> set
                                  metadata   = functionMeta
                              }, remaining)
                          )
                      | _ -> SyntaxError "Missing closing parenthesis for function definition"
                 )
             | _ -> SyntaxError "Missing identifier for function definition"
        )
    )
    // <functionmeta> ::= ε
    //                 |  "[" "inlined"  "]"             <functionmeta>
    //                 |  "[" "memoized" "]"             <functionmeta>
    //                 |  "[" "symbol" ":" <letters> "]" <functionmeta>
    and functionmeta: Parser<FunctionMetadata option> = (fun tokens ->
        match tokens with
         | Lexer.LeftBracket :: metaTail ->
             match metaTail with
              // <functionmeta> ::= "[" "inlined" "]" <functionmeta>
              | Lexer.Symbol "inlined" :: Lexer.RightBracket :: metaTail ->
                  ifOk (functionmeta metaTail) (fun (maybeMeta, remaining) ->
                      match maybeMeta with
                       | None -> Ok (Some {
                           inlined  = true
                           memoized = defaultMetadata.memoized
                           symbol   = defaultMetadata.symbol
                         }, remaining)
                       | Some functionMeta ->
                           Ok (Some {
                               inlined  = true
                               memoized = functionMeta.memoized
                               symbol   = functionMeta.symbol
                           }, remaining)
                  )
              // <functionmeta> ::= "[" "memoized" "]" <functionmeta>
              | Lexer.Symbol "memoized" :: Lexer.RightBracket :: metaTail ->
                  ifOk (functionmeta metaTail) (fun (maybeMeta, remaining) ->
                      match maybeMeta with
                       | None -> Ok (Some {
                           inlined  = defaultMetadata.inlined
                           memoized = true
                           symbol   = defaultMetadata.symbol
                         }, remaining)
                       | Some functionMeta ->
                           Ok (Some {
                               inlined  = functionMeta.inlined
                               memoized = true
                               symbol   = functionMeta.symbol
                           }, remaining)
                  )
              // <functionmeta> ::= "[" "symbol" ":" <letters> "]" <functionmeta>
              | Lexer.Symbol "symbol" :: Lexer.Colon :: Lexer.Symbol symbolicName :: Lexer.RightBracket :: metaTail ->
                  ifOk (functionmeta metaTail) (fun (maybeMeta, remaining) ->
                      match maybeMeta with
                       | None -> Ok (Some {
                           inlined  = defaultMetadata.inlined
                           memoized = defaultMetadata.memoized
                           symbol   = Some symbolicName
                         }, remaining)
                       | Some functionMeta ->
                           Ok (Some {
                               inlined  = functionMeta.inlined
                               memoized = functionMeta.memoized
                               symbol   = Some symbolicName
                           }, remaining)
                  )
              | _ -> SyntaxError "Illegal meta attribute for function"
         | remaining -> Ok (None, remaining)
    )
    // <functionparams> ::= <functionparam>
    //                   |  <functionparam> "," <functionparams>
    and functionparams: Parser<FunctionParameter list> = (fun tokens ->
        ifOk (functionparam tokens) (fun (paramsHead, remaining) ->
            match remaining with
             // <functionparams> ::= <functionparam> "," <functionparams>
             | Lexer.Comma :: tail ->
                 ifOk (functionparams tail) (fun (paramsTail, remaining) ->
                     Ok (paramsHead :: paramsTail, remaining)
                 )
             // <functionparams> ::= <functionparam>
             | remaining -> Ok ([paramsHead], remaining)
        )
    )
    // <functionparam> ::= <identifier>
    //                  |  <identifier> ":" "N"
    //                  |  <identifier> ":" "Z"
    //                  |  <identifier> ":" "R"
    //                  |  <identifier> ":" "Q"
    //                  |  <identifier> ":" "I"
    //                  |  <identifier> ":" "C"
    and functionparam: Parser<FunctionParameter> = (fun tokens ->
        match tokens with
         | Lexer.Identifier (ch, sb) :: Lexer.Colon :: Lexer.Identifier (maybeSet, _) :: paramTail ->
             match getNumberSet maybeSet with
              | None -> SyntaxError $"Expected valid number set - got {maybeSet} instead"
              // <functionparam> ::= <identifier> ":" "N"
              //                  |  <identifier> ":" "Z"
              //                  |  <identifier> ":" "R"
              //                  |  <identifier> ":" "Q"
              //                  |  <identifier> ":" "I"
              //                  |  <identifier> ":" "C"
              | Some set -> Ok (((ch, sb), set), paramTail)
         | Lexer.Identifier (ch, sb) :: paramTail -> Ok (((ch, sb), Real), paramTail)
         // <functionparam> ::= <identifier>
         | _ -> SyntaxError "Expected identifier"
    )
    // <functionreturn> ::= ε
    //                   |  "->" "N"
    //                   |  "->" "Z"
    //                   |  "->" "R"
    //                   |  "->" "Q"
    //                   |  "->" "I"
    //                   |  "->" "C"
    and functionreturn: Parser<NumberSet option> = (fun tokens ->
        match tokens with
         | Lexer.Arrow :: Lexer.Identifier (maybeSet, _) :: returnTail ->
             match getNumberSet maybeSet with
              | None     -> SyntaxError "Illegal number set for return set"
              | Some set -> Ok (Some set, returnTail)
         | tokens -> Ok (None, tokens)
    )
    // <functionbody> ::= <expression> ";"
    //                 |  "{" <conditions> "}"
    and functionbody: Parser<ASTNode> = (fun tokens ->
        match tokens with
         // <functionbody> ::= "{" <conditions> "}"
         | Lexer.LeftBrace :: bodyTail                       ->
             ifOk (conditions bodyTail) (fun (functionBody, bodyTail) ->
                 match bodyTail with
                  | Lexer.RightBrace :: remaining ->
                      Ok (functionBody, remaining)
                  | _ -> SyntaxError "Missing closing brace from function body"
             )
         // <functionbody> ::= <expression> ";"
         | tokens ->
             ifOk (expression tokens) (fun (node, remaining) ->
                 match remaining with
                  | Lexer.SemiColon :: remaining -> Ok (node, remaining)
                  | _ -> SyntaxError "Missing semicolon for expression"
             )
    )
    // <conditions> ::= <ifcond> ";" <conditions>
    //               |  <ifcond> ";" <otherwisecond> ";"
    and conditions: Parser<ASTNode> = (fun tokens ->
        ifOk (ifcond tokens) (fun (ifCondition, conditionsTail) ->
            match conditionsTail with
             | Lexer.SemiColon :: conditionsTail ->
                 match Lexer.statementContainsToken conditionsTail Lexer.If with
                  // <conditions> ::= <ifcond> ";" <conditions>
                  | true ->
                      ifOk (conditions conditionsTail) (fun (conditions, remaining) ->
                          match conditions with
                           | Conditions (cases, defaultCase) ->
                               Ok (Conditions (ifCondition :: cases, defaultCase), remaining)
                           | node -> SystemError $"Unexpected node {node} - should be Conditions"
                      )
                  | false ->
                      // <ifcond> ";" <otherwisecond> ";"
                      ifOk (otherwisecond conditionsTail) (fun (defaultCase, remaining) ->
                          match remaining with
                           | Lexer.SemiColon :: remaining ->
                               Ok (Conditions ([ifCondition], defaultCase), remaining)
                           | _ -> SyntaxError "Missing semicolon for default case"
                      )
             | _ -> SyntaxError "Missing semicolon for If condition"
        )
    )
    // <ifcond> ::= <expression> "if" <expression> <comparison> <expression>
    and ifcond: Parser<ASTNode> = (fun tokens ->
        ifOk (expression tokens) (fun (ifTrue, ifTail) ->
            match ifTail with
             | Lexer.If :: ifTail ->
                 ifOk (expression ifTail) (fun (lhs, ifTail) ->
                     ifOk (comparison ifTail) (fun (cmpOp, ifTail) ->
                         ifOk (expression ifTail) (fun (rhs, remaining) ->
                             Ok (Comparison (ifTrue, lhs, cmpOp, rhs), remaining)
                         )
                     )
                 )
             | _ -> SyntaxError "Expected If token"
        )
    )
    // <otherwisecond> ::= <expression> "otherwise"
    and otherwisecond: Parser<ASTNode> = (fun tokens ->
        ifOk (expression tokens) (fun (defaultCase, otherwiseTail) ->
            match otherwiseTail with
             | Lexer.Otherwise :: remaining ->
                 Ok (defaultCase, remaining)
             | _ -> SyntaxError "Expected otherwise"
        )
    )
    // <comparison> ::= "="
    //               |  "!="
    //               |  "<"
    //               |  "<="
    //               |  ">"
    //               |  ">="
    and comparison: Parser<ASTNode> = (fun tokens ->
        match tokens with
         | Lexer.Equals             :: remaining -> Ok (Equals,             remaining)
         | Lexer.NotEqual           :: remaining -> Ok (NotEqual,           remaining)
         | Lexer.LessThan           :: remaining -> Ok (LessThan,           remaining)
         | Lexer.LessThanOrEqual    :: remaining -> Ok (LessThanOrEqual,    remaining)
         | Lexer.GreaterThan        :: remaining -> Ok (GreaterThan,        remaining)
         | Lexer.GreaterThanOrEqual :: remaining -> Ok (GreaterThanOrEqual, remaining)
         | head                      :: _        -> SyntaxError $"Expected comparison - got {head} instead"
         | []                                    -> SyntaxError "Expected value when TokenStream empty"
    )

    /// <summary>
    ///     Parses the provided <c>tokens</c> into an AST (Abstract Syntax Tree).
    ///     Any syntax errors will be propagated upwards through the parse tree and therefore should be checked
    ///     whenever parsing.
    /// </summary>
    /// <param name='tokens'> the <c>TokenStream</c> to be parsed </param>
    /// <returns> the root <c>ASTNode</c> that is ready to be evaluated </returns>
    let parse (tokens: Lexer.TokenStream): ASTNode Result =
        match program tokens with
         | Error err                -> Error err
         | Ok    (nodes, [])        -> (Begin >> Ok) nodes
         | Ok    (_,     head :: _) -> SyntaxError $"Unexpected trailing token: {head}"
