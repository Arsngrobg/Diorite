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
// Version: v1.12
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
        | DoubleForwardSlash
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
         | [] | [SemiColon]  -> ""
         | SemiColon :: tail -> $"\n{tokens2str tail}"
         | t :: tail         -> $"({t}) {tokens2str tail}"

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
             | '/' :: '/' :: tail -> DoubleForwardSlash :: scan tail
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
        | Natural    // N = {0, ..., ∞}
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
    ///     The default <c>FunctionParameter</c> type.
    ///     The function parameter domain that is inferred to be as <c>Real</c> if none is provided.
    /// </summary>
    let defaultFunctionDomain: NumberSet = Real

    /// <summary>
    ///     The default function range.
    ///     The function range that is inferred to be as <c>Real</c> if none is provided.
    /// </summary>
    let defaultFunctionRange: NumberSet = Real

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
        | SymbolicName of string
        | VariableName of id: char * subscript: int option

    /// <summary>
    ///     The <c>AST</c> type is a discriminated union which describes the structure of the AST of the <b>Diorite</b>
    ///     language.
    /// </summary>
    type AST =
        // values
        | Number          of float
        | Variable      of id: char * subscript: int option

        // reserved words
        | Undefined
        | PositiveInfinity
        | NegativeInfinity

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
        | FloorDivision
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
        | Begin            of AST list
        | BinaryOperation  of left:    AST                * operator:    AST      * right:    AST
        | UnaryOperation   of operand: AST                * operator:    AST
        | FunctionDef      of data:    FunctionAttributes * body:        AST
        | FunctionCall     of name:    FunctionName       * arguments:   AST list
        | Conditions       of cases:   AST list           * defaultCase: AST
        | Comparison       of ifTrue:  AST                * left:        AST      * operator: AST * right: AST

    // the parser uses the idea of parser combinators for the parsing strategy
    // each stage of the parser is a parser within itself
    // very good resource:
    // https://tgdwyer.github.io/parsercombinators

    /// <summary>
    ///     The successful parse of a particular <c>Parser</c>.
    ///     It contains the value of type <c>'a</c> and the remaining tokens to parse.
    /// </summary>
    type ParseState<'a> = 'a * Lexer.TokenStream

    /// <summary>
    ///     The <c>Parser</c> is a function that accepts a <c>TokenStream</c> and returns a <c>Result</c> that may
    ///     contain the resulting <c>'a</c> value and the remaining tokens.
    /// </summary>
    type Parser<'a> = Lexer.TokenStream -> ParseState<'a> Result

    // the function signature of a bind function for the parser combinators
    type private BindFunction<'a, 'b> = ParseState<'a> -> ParseState<'b> Result

    // here is the sort-of DSL for chaining parsers - better than the ifOk function (essentially the bind operation)
    let private (>>=) (state: ParseState<'a> Result) (fn: BindFunction<'a, 'b>): ParseState<'b> Result =
        match state with
         | Error err             -> Error err
         | Ok (value, remaining) -> fn (value, remaining)

    // <program> ::= ε
    //            |  <statement> <program>
    let rec program: Parser<AST list> = (fun tokens ->
        match tokens with
         // <program> ::= ε
         | [] -> Ok([], [])
         // <program> ::= <statement> <program>
         | _  ->
             statement tokens >>= (fun (maybeNode, programTail) ->
                 program programTail >>= (fun (nodes, remaining) ->
                     match maybeNode with
                      | None      -> Ok (nodes, remaining)
                      | Some node -> Ok (node :: nodes, remaining)
                 )
             )
    )
    // <statement> ::= ";"
    //              |  <expression> ";"
    //              |  <identifier>  "=" <expression> ";"
    //              |  <functiondef> "=" <functionbody>
    and statement: Parser<AST option> = (fun tokens ->
        if (Lexer.statementContainsToken tokens) Lexer.Equals then
            match tokens with
             | Lexer.Identifier (ch, sb) :: Lexer.Equals :: statementTail ->
                 expression statementTail >>= (fun (expression, statementTail) ->
                     match statementTail with
                      | Lexer.SemiColon :: remaining ->
                          Ok ((BinaryOperation >> Some) (Variable (ch, sb), Equals, expression), remaining)
                      | head         :: _            -> SyntaxError $"Expected semicolon - got {head}"
                      | []                           -> SyntaxError  "Expected semicolon"
                 )
             | _ ->
                 functionDef tokens >>= (fun (functionAttributes, statementTail) ->
                     match statementTail with
                      | Lexer.Equals :: statementTail ->
                          functionBody statementTail >>= (fun (body, remaining) ->
                              Ok ((FunctionDef >> Some) (functionAttributes, body), remaining)
                          )
                      | head         :: _             -> SyntaxError $"Expected closing parenthesis - got {head}"
                      | []                            -> SyntaxError  "Expected closing parenthesis"
                 )
        else
            // <statement> ::= ";"
            //              |  <expression> ";"
            match tokens with
             // <statement> ::= ";"
             | Lexer.SemiColon :: remaining -> Ok (None, remaining)
             // <statement> ::= <expression> ";"
             | _ ->
                 expression tokens >>= (fun (node, statementTail) ->
                     match statementTail with
                      | Lexer.SemiColon :: remaining -> Ok (Some node, remaining)
                      | _ -> SyntaxError "Missing semicolon after expression"
                 )
    )
    // <expression> ::= <term> <expression'>
    and expression: Parser<AST> = (fun tokens ->
        // <expression'> ::= ε
        //                |  "+" <term> <expression'>
        //                |  "-" <term> <expression'>
        let rec expression' (accumulated: AST): Parser<AST> = (fun tokens ->
            match tokens with
             // <expression'> ::= "+" <term> <expression'>
             | Lexer.Plus :: expressionTail ->
                 term expressionTail >>= (fun (termNode, expressionTail) ->
                     (expression' (BinaryOperation (accumulated, Addition, termNode))) expressionTail
                 )
             // <expression'> ::= "-" <term> <expression'>
             | Lexer.Hyphen :: expressionTail ->
                 term expressionTail >>= (fun (termNode, expressionTail) ->
                     expression' (BinaryOperation (accumulated, Subtraction, termNode)) expressionTail
                 )
             // <expression'> ::= ε
             | remaining -> Ok (accumulated, remaining)
        )

        // will return either the leftNode or the recursively parsed addition/subtraction operations
        term tokens >>= (fun (leftNode, expressionTail) ->
            (expression' leftNode) expressionTail
        )
    )
    // <term> ::= <factor> <term'>
    and term: Parser<AST> = (fun tokens ->
        // <term'> ::= ε
        //          |  "*"  <factor> <term'>
        //          |  "/"  <factor> <term'>
        //          |  "//" <factor> <term'>
        //          |  "%"  <factor> <term'>
        let rec term' (accumulated: AST): Parser<AST> = (fun tokens ->
            match tokens with
             // <term'> ::= "*" <factor> <term'>
             | Lexer.Asterisk :: termTail ->
                 factor termTail >>= (fun (factorNode, termTail) ->
                     (term' (BinaryOperation (accumulated, Multiplication, factorNode))) termTail
                 )
             // <term'> ::= "/" <factor> <term'>
             | Lexer.ForwardSlash :: termTail ->
                 factor termTail >>= (fun (factorNode, termTail) ->
                     (term' (BinaryOperation (accumulated, Division, factorNode))) termTail
                 )
             // <term'> ::= "//" <factor> <term'>
             | Lexer.DoubleForwardSlash :: termTail ->
                 factor termTail >>= (fun (factorNode, termTail) ->
                     (term' (BinaryOperation (accumulated, FloorDivision, factorNode))) termTail
                 )
             // <term'> ::= "%" <factor> <term'>
             | Lexer.Percentage :: termTail ->
                 factor termTail >>= (fun (factorNode, termTail) ->
                     (term' (BinaryOperation (accumulated, Modulo, factorNode))) termTail
                 )
             // <term'> ::= ε
             | remaining -> Ok (accumulated, remaining)
        )

        // will return either the leftNode or the recursively parsed multiplication/division operations
        factor tokens >>= (fun (leftNode, termTail) ->
            (term' leftNode) termTail
        )
    )
    // <factor> ::= <signed>
    //           |  <signed> "^" <signed>
    and factor: Parser<AST> = (fun tokens ->
        signed tokens >>= (fun (leftNode, remaining) ->
            match remaining with
             // <factor> ::= <signed> "^" <signed>
             | Lexer.Hat :: factorTail ->
                 signed factorTail >>= (fun (rightNode, remaining) ->
                     Ok (BinaryOperation (leftNode, Exponentiation, rightNode), remaining)
                 )
             // <factor> ::= <signed>
             | _ -> Ok (leftNode, remaining)
        )
    )
    // <signed> ::= <exponent>
    //           |  "+" <signed>
    //           |  "-" <signed>
    and signed: Parser<AST> = (fun tokens ->
        match tokens with
         // <signed> ::= "+" <exponent>
         | Lexer.Plus :: signedTail ->
             signed signedTail >>= (fun (node, remaining) ->
                 Ok (UnaryOperation (node, Positive), remaining)
             )
         // <signed> ::= "-" <exponent>
         | Lexer.Hyphen :: signedTail ->
             signed signedTail >>= (fun (node, remaining) ->
                 Ok (UnaryOperation (node, Negative), remaining)
             )
         // <signed> ::= <exponent>
         | _ -> exponent tokens
    )
    // <exponent> ::= <integral> <exponent'>
    and exponent: Parser<AST> = (fun tokens ->
        // <exponent'> ::= ε
        //              |  "!" <exponent'>
        let rec exponent' (accumulated: AST): Parser<AST> = (fun tokens ->
            match tokens with
             // <exponent'> ::= "!" <exponent'>
             | Lexer.Exclamation :: exponentTail ->
                 (exponent' accumulated) exponentTail >>= (fun (node, remaining) ->
                    Ok (UnaryOperation (node, Factorial), remaining)
                 )
             // <exponent'> ::= ε
             | remaining -> Ok (accumulated, remaining)
        )

        integral tokens >>= (fun (node, exponentTail) -> (exponent' node) exponentTail)
    )
    // <integral> ::= <subexpression> <integral'>
    //             |  "'" <integral>
    and integral: Parser<AST> = (fun tokens ->
        // <integral'> ::= ε
        //              |  "'" <integral'>
        let rec integral': Parser<AST option> = (fun tokens ->
            match tokens with
             // <integral'> ::= "'" <integral'>
             | Lexer.Tick :: integralTail ->
                 integral' integralTail >>= (fun (maybeIntegral, remaining) ->
                     match maybeIntegral with
                      | Some node -> Ok ((UnaryOperation >> Some) (node, Differentiation), remaining)
                      | None      -> Ok (Some Differentiation, remaining)
                 )
             // <integral'> ::= ε
             | remaining -> Ok (None, remaining)
        )

        match tokens with
         // <integral> ::= "'" <integral>
         | Lexer.Tick :: integralTail ->
             integral integralTail >>= (fun (node, remaining) ->
                 Ok (UnaryOperation (node, Integration), remaining)
             )
         // <integral> ::= <subexpression> <integral'>
         | _ ->
             subExpression tokens >>= (fun (node, integralTail) ->
                 integral' integralTail >>= (fun (maybeDifferential, remaining) ->
                     match maybeDifferential with
                      | Some differentialNode -> Ok (UnaryOperation (node, differentialNode), remaining)
                      | None                  -> Ok (node, remaining)
                 )
             )
    )
    // <subexpression>  ::= <value>
    //                   |  "(" <expression> ")"
    //                   |  "|" <expression> "|"
    //                   |  <identifier> "(" <args> ")"
    //                   |  <letters>    "(" <args> ")"
    and subExpression: Parser<AST> = (fun tokens ->
        match tokens with
         // <subexpression> ::= "(" <expression> ")"
         | Lexer.LeftParenthesis :: subExpressionTail ->
             expression subExpressionTail >>= (fun (node, subExpressionTail) ->
                 match subExpressionTail with
                  | Lexer.RightParenthesis :: remaining -> Ok (node, remaining)
                  | head                   :: _         -> SyntaxError $"Expected closing parenthesis - got {head}"
                  | []                                  -> SyntaxError  "Expected closing parenthesis"
             )
         // <subexpression> ::= "|" <expression> "|"
         | Lexer.Bar :: subExpressionTail ->
             expression subExpressionTail >>= (fun (node, subExpressionTail) ->
                 match subExpressionTail with
                  | Lexer.Bar :: remaining -> Ok (UnaryOperation (node, Absolution), remaining)
                  | head      :: _         -> SyntaxError $"Expected closing bar - got {head}"
                  | []                     -> SyntaxError  "Expected closing bar"
             )
         // <subexpression> ::= <identifier> "(" <args> ")"
         | Lexer.Identifier (ch, sb) :: Lexer.LeftParenthesis :: subExpressionTail ->
             args subExpressionTail >>= (fun (args, subExpressionTail) ->
                 match subExpressionTail with
                  | Lexer.RightParenthesis :: remaining -> Ok (FunctionCall (VariableName (ch, sb), args), remaining)
                  | head                   :: _         -> SyntaxError $"Expected closing parenthesis - got {head}"
                  | []                                  -> SyntaxError  "Expected closing parenthesis"
             )
         // <subexpression> ::= <letters> "(" <args> ")"
         | Lexer.Symbol name :: Lexer.LeftParenthesis :: subExpressionTail ->
             args subExpressionTail >>= (fun (args, subExpressionTail) ->
                 match subExpressionTail with
                  | Lexer.RightParenthesis :: remaining -> Ok (FunctionCall (SymbolicName name, args), remaining)
                  | head                   :: _         -> SyntaxError $"Expected closing parenthesis - got {head}"
                  | []                                  -> SyntaxError  "Expected closing parenthesis"
             )
         // <subexpression> ::= <value>
         | _ -> value tokens
    )
    // <args> ::= <expression>
    //         |  <expression> "," <args>
    and args: Parser<AST list> = (fun tokens ->
        expression tokens >>= (fun (node, argsTail) ->
            match argsTail with
             // <args> ::= <expression> "," <args>
             | Lexer.Comma :: argsTail ->
                 args argsTail >>= (fun (nodes, remaining) ->
                     Ok (node :: nodes, remaining)
                 )
             // <args> ::= <expression>
             | remaining -> Ok ([node], remaining)
        )
    )
    // <value> ::= "undefined"
    //          |  "infinity" | "inf"
    //          |  "pi"
    //          |  "tau"
    //          |  "euler"
    //          |  <letters> <digit>
    //          |  <number>
    and value: Parser<AST> = (fun tokens ->
        match tokens with
         | Lexer.Undefined           :: remaining -> Ok (Undefined,         remaining)
         | Lexer.Infinity            :: remaining -> Ok (PositiveInfinity,  remaining)
         | Lexer.Pi                  :: remaining -> Ok (Number 3.1415926,  remaining)
         | Lexer.Tau                 :: remaining -> Ok (Number 6.2831853,  remaining)
         | Lexer.Euler               :: remaining -> Ok (Number 2.7182818,  remaining)
         | Lexer.Identifier (ch, sb) :: remaining -> Ok (Variable (ch, sb), remaining)
         | Lexer.Number      number  :: remaining ->
             // if the number is too big it can be Double.Infinity - so map it to an Infinity node for consistent ops
             if number |> System.Double.IsInfinity then Ok (PositiveInfinity, remaining)
             else                                       Ok (Number number,    remaining)
         | head                      :: _         -> SyntaxError $"Expected value - got {head} instead"
         | []                                     -> SyntaxError "Expected value when TokenStream empty"
    )
    // <functiondef> ::= <functionmeta> <identifier> "(" <functionparams> ")" <functionreturn>
    and functionDef: Parser<FunctionAttributes> = (fun tokens ->
        functionMeta tokens >>= (fun (maybeMeta, functionTail) ->
            let functionMeta = match maybeMeta with Some meta -> meta | None -> defaultMetadata
            match functionTail with
             // <functiondef> ::= <functionmeta> <identifier> "(" <functionparams> ")" <functionreturn>
             | Lexer.Identifier (ch, sb) :: Lexer.LeftParenthesis :: functionTail ->
                 functionParams functionTail >>= (fun (parameters, remaining) ->
                     match remaining with
                      | Lexer.RightParenthesis :: functionTail ->
                          functionReturn functionTail >>= (fun (parsedReturn, remaining) ->
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
    and functionMeta: Parser<FunctionMetadata option> = (fun tokens ->
        match tokens with
         | Lexer.LeftBracket :: Lexer.Symbol "inlined" :: Lexer.RightBracket :: functionMetaTail ->
             functionMeta functionMetaTail >>= (fun (maybeMetadata, remaining) ->
                 match maybeMetadata with
                  | None -> Ok (Some {
                      inlined=true; memoized=defaultMetadata.memoized; symbol=defaultMetadata.symbol
                  }, remaining)
                  | Some functionMetadata -> Ok (Some {
                      inlined=true; memoized=functionMetadata.memoized; symbol=functionMetadata.symbol
                  }, remaining)
             )
         | Lexer.LeftBracket :: Lexer.Symbol "memoized" :: Lexer.RightBracket :: functionMetaTail ->
             functionMeta functionMetaTail >>= (fun (maybeMetadata, remaining) ->
                 match maybeMetadata with
                  | None -> Ok (Some {
                      inlined=defaultMetadata.inlined; memoized=true; symbol=defaultMetadata.symbol
                  }, remaining)
                  | Some functionMetadata -> Ok (Some {
                      inlined=functionMetadata.inlined; memoized=true; symbol=functionMetadata.symbol
                  }, remaining)
             )
         | Lexer.LeftBracket :: Lexer.Symbol "symbol" :: Lexer.Colon :: Lexer.Symbol name :: Lexer.RightBracket :: functionMetaTail ->
             functionMeta functionMetaTail >>= (fun (maybeMetadata, remaining) ->
                 match maybeMetadata with
                  | None -> Ok (Some {
                      inlined=defaultMetadata.inlined; memoized=defaultMetadata.memoized; symbol=(Some name)
                  }, remaining)
                  | Some functionMetadata -> Ok (Some {
                      inlined=functionMetadata.inlined; memoized=functionMetadata.memoized; symbol=(Some name)
                  }, remaining)
             )
         | remaining -> Ok (None, remaining)
    )
    // <functionparams> ::= <functionparam>
    //                   |  <functionparam> "," <functionparams>
    and functionParams: Parser<FunctionParameter list> = (fun tokens ->
        functionParam tokens >>= (fun (parameter, functionParamsTail) ->
            match functionParamsTail with
             // <functionparams> ::= <functionparam> "," <functionparams>
             | Lexer.Comma :: functionParamsTail ->
                 functionParams functionParamsTail >>= (fun (parameters, remaining) ->
                     Ok (parameter :: parameters, remaining)
                 )
             // <functionparams> ::= <functionparam>
             | remaining -> Ok ([parameter], remaining)
        )
    )
    // <functionparam> ::= <identifier>
    //                  |  <identifier> ":" "N"
    //                  |  <identifier> ":" "Z"
    //                  |  <identifier> ":" "R"
    //                  |  <identifier> ":" "Q"
    //                  |  <identifier> ":" "I"
    //                  |  <identifier> ":" "C"
    and functionParam: Parser<FunctionParameter> = (fun tokens ->
        match tokens with
         | Lexer.Identifier (ch, sb) :: functionParamTail ->
             match functionParamTail with
             // <functionparam> ::= <identifier> ":" "N"
             //                  |  <identifier> ":" "Z"
             //                  |  <identifier> ":" "R"
             //                  |  <identifier> ":" "Q"
             //                  |  <identifier> ":" "I"
             //                  |  <identifier> ":" "C"
              | Lexer.Colon :: functionParamTail ->
                  match functionParamTail with
                   | Lexer.Identifier (setCh, None) :: remaining ->
                       match getNumberSet setCh with
                        | Some numberSet -> Ok (FunctionParameter ((ch, sb), numberSet), remaining)
                        | None           -> SyntaxError $"Expected NumberSet for function parameter - got {ch}"
                   | Lexer.Identifier (ch, Some sb) :: _ ->
                        SyntaxError $"Expected NumberSet for function parameter - got {ch}{sb}"
                   | head :: _ -> SyntaxError $"Expected NumberSet for function parameter - got {head}"
                   | []        -> SyntaxError  "Expected NumberSet for function parameter"
              // <functionparam> ::= <identifier>
              | _ -> Ok (FunctionParameter ((ch, sb), defaultFunctionDomain), functionParamTail)
         | head :: _ -> SyntaxError $"Expected identifier for function parameter - got {head}"
         | []        -> SyntaxError  "Expected identifier for function parameter"
    )
    // <functionreturn> ::= ε
    //                   |  "->" "N"
    //                   |  "->" "Z"
    //                   |  "->" "R"
    //                   |  "->" "Q"
    //                   |  "->" "I"
    //                   |  "->" "C"
    and functionReturn: Parser<NumberSet option> = (fun tokens ->
        match tokens with
        // <functionreturn> ::= "->" "N"
        //                   |  "->" "Z"
        //                   |  "->" "R"
        //                   |  "->" "Q"
        //                   |  "->" "I"
        //                   |  "->" "C"
         | Lexer.Arrow :: functionReturnTail ->
              match functionReturnTail with
               | Lexer.Identifier (ch, None) :: remaining ->
                   match getNumberSet ch with
                    | Some numberSet -> Ok (Some numberSet, remaining)
                    | None           -> SyntaxError $"Expected NumberSet for function return - got {ch}"
               | Lexer.Identifier (ch, Some sb) :: _ ->
                    SyntaxError $"Expected NumberSet for function return - got {ch}{sb}"
               | head :: _ -> SyntaxError $"Expected NumberSet for function return - got {head}"
               | []        -> SyntaxError  "Expected NumberSet for function return"
         // <functionreturn> ::= ε
         | remaining -> Ok (None, remaining)
    )
    // <functionbody> ::= <expression> ";"
    //                 |  "{" <conditions> "}"
    and functionBody: Parser<AST> = (fun tokens ->
        match tokens with
         | Lexer.LeftBrace :: functionBodyTail ->
             conditions functionBodyTail >>= (fun (functionBody, bodyTail) ->
                 match bodyTail with
                  | Lexer.RightBrace :: remaining ->
                      Ok (functionBody, remaining)
                  | _ -> SyntaxError "Missing closing brace from function body"
             )
         // <functionbody> ::= <expression> ";"
         | _ ->
             expression tokens >>= (fun (node, functionBodyTail) ->
                 match functionBodyTail with
                  | Lexer.SemiColon :: remaining -> Ok (node, remaining)
                  | head :: _ -> SyntaxError $"Expected semicolon for function expression - got {head}"
                  | []        -> SyntaxError  "Expected semicolon for function expression"
             )
    )
    // <conditions> ::= <ifcond> ";" <conditions>
    //               |  <ifcond> ";" <otherwisecond> ";"
    and conditions: Parser<AST> = (fun tokens ->
        ifcond tokens >>= (fun (ifCondition, conditionsTail) ->
            match conditionsTail with
             | Lexer.SemiColon :: conditionsTail ->
                 match Lexer.statementContainsToken conditionsTail Lexer.If with
                  // <conditions> ::= <ifcond> ";" <conditions>
                  | true ->
                      conditions conditionsTail >>= (fun (conditions, remaining) ->
                          match conditions with
                           | Conditions (cases, defaultCase) ->
                               Ok (Conditions (ifCondition :: cases, defaultCase), remaining)
                           | node -> SystemError $"Unexpected node {node} - should be Conditions"
                      )
                  | false ->
                      // <ifcond> ";" <otherwisecond> ";"
                      otherwisecond conditionsTail >>= (fun (defaultCase, remaining) ->
                          match remaining with
                           | Lexer.SemiColon :: remaining ->
                               Ok (Conditions ([ifCondition], defaultCase), remaining)
                           | _ -> SyntaxError "Missing semicolon for default case"
                      )
             | _ -> SyntaxError "Missing semicolon for If condition"
        )
    )
    // <ifcond> ::= <expression> "if" <expression> <comparison> <expression>
    and ifcond: Parser<AST> = (fun tokens ->
        expression tokens >>= (fun (ifTrue, ifTail) ->
            match ifTail with
             | Lexer.If :: ifTail ->
                 expression ifTail >>= (fun (lhs, ifTail) ->
                     comparison ifTail >>= (fun (cmpOp, ifTail) ->
                         expression ifTail >>= (fun (rhs, remaining) ->
                             Ok (Comparison (ifTrue, lhs, cmpOp, rhs), remaining)
                         )
                     )
                 )
             | _ -> SyntaxError "Expected If token"
        )
    )
    // <otherwisecond> ::= <expression> "otherwise"
    and otherwisecond: Parser<AST> = (fun tokens ->
        expression tokens >>= (fun (defaultCase, otherwiseTail) ->
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
    and comparison: Parser<AST> = (fun tokens ->
        match tokens with
         | Lexer.Equals             :: remaining -> Ok (Equals,             remaining)
         | Lexer.NotEqual           :: remaining -> Ok (NotEqual,           remaining)
         | Lexer.LessThan           :: remaining -> Ok (LessThan,           remaining)
         | Lexer.LessThanOrEqual    :: remaining -> Ok (LessThanOrEqual,    remaining)
         | Lexer.GreaterThan        :: remaining -> Ok (GreaterThan,        remaining)
         | Lexer.GreaterThanOrEqual :: remaining -> Ok (GreaterThanOrEqual, remaining)
         | head                      :: _        -> SyntaxError $"Expected comparison - got {head}"
         | []                                    -> SyntaxError  "Expected comparison"
    )

    /// <summary>
    ///     Parses the provided <c>tokens</c> into an AST (Abstract Syntax Tree).
    ///     Any syntax errors will be propagated upwards through the parse tree and therefore should be checked
    ///     whenever parsing.
    /// </summary>
    /// <param name='tokens'> the <c>TokenStream</c> to be parsed </param>
    /// <returns> the root <c>AST</c> that is ready to be evaluated </returns>
    let parse (tokens: Lexer.TokenStream): AST Result =
        match program tokens with
         | Error err                -> Error err
         | Ok    (nodes, [])        -> (Begin >> Ok) nodes
         | Ok    (_,     head :: _) -> SyntaxError $"Unexpected trailing token {head}"
