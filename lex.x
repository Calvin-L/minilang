
{
module Lex (
       miniLex,
       Token(
        Id,
        Num,
        Plus,
        OpenBrace,
        CloseBrace,
        EqualSign,
        Comma,
        OpenParen,
        CloseParen,
        Print,
        Def,
        Arrow)) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
       $white+                      ;
       [\+]+                        { \s -> Plus (fromIntegral.length $ s) }
       \=                           { \_ -> EqualSign }
       \{                           { \_ -> OpenBrace }
       \}                           { \_ -> CloseBrace }
       \(                           { \_ -> OpenParen }
       \)                           { \_ -> CloseParen }
       \,                           { \_ -> Comma }
       print                        { \_ -> Print }
       def                          { \_ -> Def }
       \-\>                         { \_ -> Arrow }
       $alpha [$alpha $digit \_]*   { \s -> Id s }
       $digit+                      { \s -> Num (read s) }

{

data Token =
       Id String
     | Num Integer
     | Plus Integer
     | OpenBrace | CloseBrace
     | OpenParen | CloseParen
     | Print
     | Def | Arrow
     | EqualSign
     | Comma
     deriving (Eq, Show)


miniLex = alexScanTokens

}
