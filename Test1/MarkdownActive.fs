module MarkdownActive

type MarkdownDocument = list<MarkdownBlock>

and MarkdownBlock =
    | Heading of int * MarkdownSpans
    | Paragraph of MarkdownSpans
    | CodeBlock of list<string>

and MarkdownSpans = list<MarkdownSpan>

and MarkdownSpan =
    | Literal of string
    | InlineCode of string
    | Strong of MarkdownSpans
    | Emphasis of MarkdownSpans
    | HyperLink of MarkdownSpans * string

let toString chars =
    System.String(chars |> Array.ofList)

let (^) l r = sprintf "%s%s" l r

let rec ParseInlineBody acc = function
    | '`'::rest ->
        Some(List.rev acc, rest)
    | c::chars ->
        ParseInlineBody (c::acc) chars
    | [] -> None

let ParseInline = function
    | '`'::chars ->
        ParseInlineBody [] chars
    | _ -> None

let (|StartsWith|_|) prefix input =
    let rec loop = function
        | p::prefix, r::rest when p = r ->
            loop(prefix, rest)
        | [], rest ->
            Some(rest)
        | _ -> None
    loop (prefix, input)

let rec parseBracketedBody closing acc = function
    | StartsWith closing rest ->
        Some(List.rev acc, rest)
    | c::chars ->
        parseBracketedBody closing (c::acc) chars
    | _ -> None

let parseBracketed opening closing = function
    | StartsWith opening chars ->
        parseBracketedBody closing [] chars
    | _ -> None

let (|Delimited|_|) delim = parseBracketed delim delim