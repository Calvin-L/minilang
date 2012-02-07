
{

module Parse(
       miniParse,
       Expression(
        Var,
        Call,
        Int), 
       Statement(
        Set,
        Inc,
        Loop,
        Block,
        Func,
        Write)) where

import Lex

}

%name miniParse
%tokentype { Token }
%error { parseError }

%token
        id      { Id $$ }
        num     { Num $$ }
        plus    { Plus $$ }
        '='     { EqualSign }
        '{'     { OpenBrace }
        '}'     { CloseBrace }
        '('     { OpenParen }
        ')'     { CloseParen }
        ','     { Comma }
        'def'   { Def }
        '->'    { Arrow }
        'print' { Print }

%%

Program     : Statements   { $1 }

Exp         : id      { Var $1 }
            | id Args { Call $1 $2 }
            | num     { Int $1 }

Args        : '(' ')'         { [] }
            | '(' ArgList ')' { $2 }
ArgList     : Exp             { [$1] }
            | Exp ',' ArgList { $1 : $3 }

ArgIds      : '(' ')'           { [] }
            | '(' ArgIdList ')' { $2 }
ArgIdList   : id                { [$1] }
            | id ',' ArgIdList  { $1 : $3 }

Block       : '{' '}'            { [] }
            | '{' Statements '}' { $2 }

Statement   : id '=' Exp     { Set $1 $3 }
            | 'def' id ArgIds '->' id '=' Block { Func $2 $3 $5 $7 }
            | Exp Block      { Loop $1 $2 }
            | id plus        { Inc $1 $2 }
            | 'print' Args   { Write $2 }

Statements  : Statement    { [$1] }
            | Statement Statements { $1 : $2 }

{

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ (show tokens)

data Expression = 
       Var  String
     | Call String [Expression]
     | Int  Integer
     deriving (Eq, Show)

data Statement = 
       Set String Expression
     | Func String [String] String [Statement] -- id, args, ret, code
     | Inc String Integer
     | Loop Expression [Statement]
     | Block [Statement]
     | Write [Expression]
     deriving (Eq, Show)

}
