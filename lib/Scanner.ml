let is_at_end current end_len = 
    current >= end_len

let get_keyword_type text =
  match text with
  | "and" -> Some Token.AND
  | "class" -> Some Token.CLASS
  | "else" -> Some Token.ELSE
  | "false" -> Some Token.FALSE
  | "for" -> Some Token.FOR
  | "fun" -> Some Token.FUN
  | "if" -> Some Token.IF
  | "nil" -> Some Token.NIL
  | "or" -> Some Token.OR
  | "print" -> Some Token.PRINT
  | "return" -> Some Token.RETURN
  | "super" -> Some Token.SUPER
  | "this" -> Some Token.THIS
  | "true" -> Some Token.TRUE
  | "var" -> Some Token.VAR
  | "while" -> Some Token.WHILE
  | _ -> None


let advance source current = 
    (source.[current], current+1)

(*instead of addToken in java*)
let make_token source token_type literal start current line = 
    {Token.token_type = token_type; lexeme = String.sub source start (current-start); literal = literal; line = line}

let match_char source expected current end_len = 
    if is_at_end current end_len then
        (current, false)
    else
        if source.[current] <> expected then
            (current, false)
        else
            let new_current = current + 1 in
            (new_current, true)

let peek source current end_len = 
    if is_at_end current end_len then
        Char.chr 0
    else
        source.[current]

let peek_next source current end_len = 
    if is_at_end (current + 1) end_len then
        Char.chr 0
    else
        source.[current+1]

let rec scan_until_comment_end source current end_len = 
    let c = peek source current end_len in
    let is_end = is_at_end current end_len in
    if c <> Char.chr 0 && not is_end then
        let (_, new_current) = advance source current in
        scan_until_comment_end source new_current end_len
    else
        current

    
let rec scan_until_double_comment_end source current line end_len = 
    let c = peek source current end_len in
    let next_c = peek_next source current end_len in
    let is_end = is_at_end current end_len in

    if is_end then
        (current, line)
    else if c = '*' && next_c = '/' then
        let (_, temp_current) = advance source current in
        let (_, final_current) = advance source temp_current in
        (final_current, line)
    else 
        let new_line = if c = '\n' then line + 1 else line in
        let (_, new_current) = advance source current in
        scan_until_double_comment_end source new_current new_line end_len

let process_string source start current line end_len = 
    let rec advance_through_string source current line end_len = 
        let c = peek source current end_len in
        let is_end = is_at_end current end_len in
        if is_end then
            (current, line)
        else if c = '"' then
            (current, line)
        else
            let new_line = if c = '\n' then line + 1 else line in
            let (_, new_current) = advance source current in
            advance_through_string source new_current new_line end_len
    in 
    let (new_current, new_line) = advance_through_string source current line end_len in
    if is_at_end new_current (String.length source) then
        let _ = Error.error new_line "Unterminated string" in
        (new_current, new_line, true)
    else
        let (_, final_current) = advance source new_current in
        (final_current, new_line, false)

let is_digit c = 
    c >= '0' && c <= '9'


let process_number source start current line end_len = 
    let rec advance_through_number source current line end_len = 
        let c = peek source current end_len in
        let is_it_digit = is_digit c in
        if is_it_digit then
            let (_, new_current) = advance source current in
            advance_through_number source new_current line end_len
        else
            current
    in
    let new_current = advance_through_number source current line end_len in
    let c = peek source current end_len in
    let next_c = peek_next source current end_len in
    let final_current = if c = '.' && is_digit next_c then
        let (_, newer_current) = advance source new_current in
            let final_current = advance_through_number source newer_current line end_len in
            final_current
        else
            new_current
    in
    final_current

let is_alpha c = 
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'


let is_alphanumeric c = 
    is_alpha c || is_digit c

let process_identifier source start current line end_len = 
    let rec advance_through_identifier source current end_len = 
        let c = peek source current end_len in
        let is_alnum = is_alphanumeric c in
        if is_alnum then 
            let (_, new_current) = advance source current in
            advance_through_identifier source new_current end_len
        else
            current
    in
    let new_current = advance_through_identifier source current end_len in
    let text = String.sub source start (new_current - start) in
    let resolved_type = 
        match get_keyword_type text with
            | Some t -> t
            | None -> Token.IDENTIFIER
    in
    (resolved_type, new_current)



let scan_token source start current line = 
    let (char, new_current) = advance source current in 

    match char with
        | '(' -> (new_current, line, Some (make_token source Token.LEFT_PAREN Token.NoneLit start new_current line))
        | ')' -> (new_current, line, Some (make_token source Token.RIGHT_PAREN Token.NoneLit start new_current line))
        | '{' -> (new_current, line, Some (make_token source Token.LEFT_BRACE Token.NoneLit start new_current line))
        | '}' -> (new_current, line, Some (make_token source Token.RIGHT_BRACE Token.NoneLit start new_current line))
        | ',' -> (new_current, line, Some (make_token source Token.COMMA Token.NoneLit start new_current line))
        | '.' -> (new_current, line, Some (make_token source Token.DOT Token.NoneLit start new_current line))
        | '-' -> (new_current, line, Some (make_token source Token.MINUS Token.NoneLit start new_current line))
        | '+' -> (new_current, line, Some (make_token source Token.PLUS Token.NoneLit start new_current line))
        | ';' -> (new_current, line, Some (make_token source Token.SEMICOLON Token.NoneLit start new_current line))
        | '*' -> (new_current, line, Some (make_token source Token.STAR Token.NoneLit start new_current line))
        | '!' -> 
            let (after_match, resp) = match_char source '=' new_current (String.length source) in 
            if resp then
                (after_match, line, Some (make_token source Token.BANG_EQUAL Token.NoneLit start after_match line))
            else
                (new_current, line, Some (make_token source Token.BANG Token.NoneLit start new_current line))
        | '=' -> 
            let (after_match, resp) = match_char source '=' new_current (String.length source) in 
            if resp then
                (after_match, line, Some (make_token source Token.EQUAL_EQUAL Token.NoneLit start after_match line))
            else
                (new_current, line, Some (make_token source Token.EQUAL Token.NoneLit start new_current line))
        | '<' -> 
            let (after_match, resp) = match_char source '=' new_current (String.length source) in 
            if resp then
                (after_match, line, Some (make_token source Token.LESS_EQUAL Token.NoneLit start after_match line))
            else
                (new_current, line, Some (make_token source Token.LESS Token.NoneLit start new_current line))
        | '>' -> 
            let (after_match, resp) = match_char source '=' new_current (String.length source) in 
            if resp then
                (after_match, line, Some (make_token source Token.GREATER_EQUAL Token.NoneLit start after_match line))
            else
                (new_current, line, Some (make_token source Token.GREATER Token.NoneLit start new_current line))
        | '/' -> 
            let next_char = peek source new_current (String.length source) in
                (match next_char with
                    | '/' -> let (_, new_current) = advance source new_current in
                        let new_current = scan_until_comment_end source new_current (String.length source) in
                        (new_current, line, None)
                    | '*' -> let (_, new_current) = advance source new_current in
                        let (new_current, new_line) = scan_until_double_comment_end source new_current line (String.length source) in
                        (new_current, new_line, None)
                    | _ -> (new_current, line, Some (make_token source Token.SLASH Token.NoneLit start new_current line))
                )
        | ' ' | '\r' | '\t' -> (new_current, line, None)
        | '\n' -> (new_current, line + 1, None)
        | '"' -> let (new_current, new_line, is_terminated) = process_string source start new_current line (String.length source) in
                if not is_terminated then (
                    let value = String.sub source (start+1) (new_current - start - 2) in
                    (new_current, new_line, Some (make_token source Token.STRING (Token.StringLit value) start new_current new_line)))
                else (
                    (new_current, new_line, None)
                )
        | _ -> if is_digit char then
                let new_current = process_number source start current line (String.length source) in
                let value = String.sub source start (new_current - start) in
                (new_current, line, Some (make_token source Token.NUMBER (Token.NumberLit (float_of_string value)) start new_current line))
            else if is_alpha char then
                let (resolved_type, new_current) = process_identifier source start current line (String.length source) in
                (new_current, line, Some (make_token source resolved_type Token.NoneLit start new_current line))
            else
                let _ = Error.report line "" "Unexpected character" in
                (current, line, None) 

                
let scan_tokens source = 
    let rec token_loop current line tokens = 
        if is_at_end current (String.length source) then
            let eof_token: Token.token = {token_type = Token.EOF; lexeme = ""; literal = Token.NoneLit; line = line} in
            List.rev (eof_token :: tokens)     
        else
            let start = current in
            let (new_current, new_line, maybe_token) = scan_token source start current line in
            match maybe_token with
                | Some t -> token_loop new_current new_line (t :: tokens)
                | None -> token_loop new_current new_line tokens
    in 
    token_loop 0 1 []
