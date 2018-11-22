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