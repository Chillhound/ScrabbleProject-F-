// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar : Parser<string>     = pstring "intToChar"
    let pPointValue : Parser<string>    = pstring "pointValue"

    let pCharToInt : Parser<string>     = pstring "charToInt"
    let pToUpper : Parser<string>     = pstring "toUpper"
    let pToLower : Parser<string>     = pstring "toLower"
    let pCharValue : Parser<string>     = pstring "charValue"

    let pTrue : Parser<string>     = pstring "true"
    let pFalse : Parser<string>     = pstring "false"
    let pIsDigit : Parser<string>     = pstring "isDigit"
    let pIsLetter : Parser<string>     = pstring "isLetter"

    let pif : Parser<string>     = pstring "if"
    let pthen : Parser<string>     = pstring "then"
    let pelse : Parser<string>     = pstring "else"
    let pwhile : Parser<string>     = pstring "while"
    let pdo : Parser<string>     = pstring "do"
    let pdeclare : Parser<string>     = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace
    let pletter        = satisfy System.Char.IsLetter
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit

    let spaces         = many whitespaceChar  
    let spaces1        = many1 whitespaceChar

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>*>. p2 |>> fst
    let (>*>.) p1 p2  = p1 .>*>. p2 |>> snd

    let parenthesise p = pstring "(" >*>. p .>*> pstring ")"
    let brackets p = pstring "{" >*>. p .>*> pstring "}"

    let charListToStr charList = System.String(List.toArray charList) |> string //Just yoinked this from parser
    let pid = pletter <|> pchar '_' .>>. many (palphanumeric <|> (pchar '_')) |>> fun(x,y) -> x::y |> charListToStr

    
    let unop op = fun a -> op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2  

    //Aexp Parser
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    //Cexp Parser
    let cParse, cref = createParserForwardedToRef<cExp>()

    //Bexp Parser
    let OrParse, oref = createParserForwardedToRef<bExp>()
    let CompParse, coref = createParserForwardedToRef<bExp>()
    let CheckParse, chref = createParserForwardedToRef<bExp>()

    //Stmnt Parser
    let stmntTermParse, stref = createParserForwardedToRef<stm>()
    let stmntProdParse, spref = createParserForwardedToRef<stm>()
    let stmntAtomParse, saref = createParserForwardedToRef<stm>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul(N -1,x)) <?> "Neg"
    let PVParse  = unop pPointValue AtomParse |>> (fun x -> PV x) <?> "PV"
    let VParse   = pid |>> V <?> "Var"
    let NParse   = pint32 |>> N <?> "Int"
    let CTIParse = unop pCharToInt (parenthesise cParse) |>> (fun x -> CharToInt x) <?> "CharToInt"
    let ParParse = parenthesise TermParse
    do aref := choice [CTIParse; NegParse; PVParse; VParse; NParse; ParParse]

    let AexpParse = TermParse 

    //////////////////////////////////////////////////////////////////
    let CParse   = pchar '\'' >>. anyChar .>> pchar '\'' |>> C <?> "C"
    let CVParse  = unop pCharValue (parenthesise AexpParse) |>> CV <?> "CV"
    let TUParse  = unop pToUpper (parenthesise cParse) |>> ToUpper <?> "ToUpper"
    let TLParse  = unop pToLower (parenthesise cParse)  |>> ToLower <?> "ToLower"
    let ITCParse = unop pIntToChar AexpParse |>> (fun x -> IntToChar x) <?> "IntToChar"
    do cref := choice[TUParse; TLParse; ITCParse; CParse; CVParse]

    let CexpParse = cParse

    //////////////////////////////////////////////////////////////////
    let ConjParse = binop (pstring(@"/\")) CompParse OrParse |>> Conj <?> "Conj"
    let DisParse = binop (pstring(@"\/")) CompParse OrParse |>> (fun (x,y) -> Not(Conj((Not x), (Not y)))) <?> "Conj"
    do oref := choice[ConjParse; DisParse; CompParse]

    let Equal = binop (pstring("=")) AexpParse AexpParse |>> (fun x -> (AEq(x))) <?> "AEq"
    let NotEqual = binop (pstring("<>")) AexpParse AexpParse |>> (fun x -> (Not(AEq(x)))) <?> "AEq"
    let LesserParse = binop (pstring("<")) AexpParse AexpParse |>> ALt <?> "ALt"
    let LesserEqual = binop (pstring("<=")) AexpParse AexpParse |>> (fun x ->Not(Conj(Not(ALt(x)), Not(Not(Not(AEq(x))))))) <?> "AEq"
    let GreaterParse = binop (pstring(">")) AexpParse AexpParse |>> (fun x -> Conj((Not (AEq(x))), (Not(ALt(x))))) <?> "ALt"
    let GreaterEqual = binop (pstring(">=")) AexpParse AexpParse |>> (fun x -> (Not(ALt(x)))) <?> "AEq"
    do coref := choice[Equal; NotEqual; LesserParse; LesserEqual; GreaterParse; GreaterEqual; CheckParse]

    let TTParse = pTrue |>> (fun _ -> TT) <?> "true"
    let FFParse = pFalse |>> (fun _ -> FF) <?> "false"
    let NotParse = unop (pchar '~') OrParse |>> Not <?> "Not"
    let IsLetterParse = unop pIsLetter (parenthesise CexpParse) |>> IsLetter <?> "IsLetter"
    let IsDigitParse = unop pIsDigit (parenthesise CexpParse) |>> IsDigit <?> "IsDigit"
    let BParParse = parenthesise OrParse
    do chref := choice[TTParse; FFParse; NotParse; IsLetterParse; IsDigitParse; BParParse]

    let BexpParse = OrParse

    //////////////////////////////////////////////////////////////////
    let AssParse = binop (pstring(":=")) pid AexpParse |>> Ass <?> "Ass"
    let DeclareParse = pdeclare .>> whitespaceChar >>. pid |>> Declare <?> "Declare"
    do saref := choice[AssParse; DeclareParse]

    let SeqParse = binop ((pchar ';' .>> many whitespaceChar) <|> pchar ';') stmntProdParse stmntTermParse |>> Seq <?> "Seq"
    do stref := choice[SeqParse; stmntProdParse]

    let ITEParse = pif >*>. (parenthesise BexpParse) .>*> pthen .>*>. (brackets stmntTermParse) .>*> pelse .>*>. (brackets stmntTermParse) |>> (fun x -> ITE(fst(fst (x)),snd(fst(x)),snd(x))) <?> "ITE"
    let ITParse =  pif >*>. (parenthesise BexpParse) .>*> pthen .>*>. (brackets stmntTermParse) |>> (fun x -> ITE(fst(x),snd(x),Skip)) <?> "ITE"
    let WhileParse = pwhile >*>. (parenthesise BexpParse) .>*> pdo .>*>. (brackets stmntTermParse) |>> While <?> "While"
    do spref := choice[WhileParse; ITEParse; ITParse; stmntAtomParse]

    let stmntParse = stmntTermParse

    (* The rest of your parser goes here *)

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun sqp = Map.map(fun _ stmnt -> stmntToSquareFun(getSuccess(run stmntParse stmnt))) sqp
       
    let parseBoardFun s m = getSuccess(run stmntParse s) |> (fun x -> stmntToBoardFun x m)

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let parseBoardProg (bp : boardProg) = {
        center = bp.center;
        defaultSquare = Map.find bp.usedSquare (Map.map (fun _ x ->(parseSquareFun x)) bp.squares);
        squares = parseBoardFun bp.prog (Map.map (fun _ x ->(parseSquareFun x)) bp.squares);
    }

