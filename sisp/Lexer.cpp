//
//  Lexer.cpp
//  sisp
//
//  Created by 徐可 on 2020/2/19.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#include "Lexer.hpp"
#include <iostream>

using namespace std;

Token Lexer::GetChar() {
    if (Index >= TheCode.length())
        return (Token)EOF;
    Token CurChar = (Token)TheCode.at(Index++);
//    cout << "getchar [" << string(1, CurChar) << "]" << endl;

    if (CurChar == tok_newline || CurChar == tok_return) {
        CurLoc.Line++;
        CurLoc.Col = 0;
    } else {
        CurLoc.Col++;
    }

    return CurChar;
}

Token Lexer::getNextToken(unsigned ForwardStep) {
    if (ForwardStep == 0) {
        while (isspace(LastChar)) {
            LastChar = GetChar();
        }

        if (isalpha(LastChar)) {
            IdentifierStr = LastChar;
            while (isalnum(LastChar = GetChar())) {
                IdentifierStr += LastChar;
            }

            if (IdentifierStr == "extern")
                return CurTok = tok_extern;
            if (IdentifierStr == "exit")
                exit(0);
            if (IdentifierStr == "if")
                return CurTok = tok_if;
            if (IdentifierStr == "then")
                return CurTok = tok_then;
            if (IdentifierStr == "else")
                return CurTok = tok_else;
            if (IdentifierStr == "for")
                return CurTok = tok_for;
            if (IdentifierStr == "in")
                return CurTok = tok_in;
            if (IdentifierStr == "unary")
                return CurTok = tok_unary;
            if (IdentifierStr == "binary")
                return CurTok = tok_binary;
            if (IdentifierStr == "var")
                return CurTok = tok_var;
            if (IdentifierStr == "bool")
                return CurTok = tok_type_bool;
            if (IdentifierStr == "int")
                return CurTok = tok_type_int;
            if (IdentifierStr == "float")
                return CurTok = tok_type_float;
            if (IdentifierStr == "string")
                return CurTok = tok_type_string;
            if (IdentifierStr == "class")
                return CurTok = tok_class;

            return CurTok = tok_identifier;
        }

        if (LastChar == tok_dot) {
            LastChar = GetChar();
            return CurTok = tok_dot;
        }

        if (isdigit(LastChar) || LastChar == tok_dot) {
            string NumStr;
            do {
                NumStr += LastChar;
                LastChar = GetChar();
            } while (isdigit(LastChar) || LastChar == tok_dot);

            if (NumStr.find(tok_dot) == string::npos) {
                IntegerVal = stol(NumStr);
                return CurTok = tok_integer_literal;
            } else {
                FloatVal = stod(NumStr);
                return CurTok = tok_float_literal;
            }
        }

        if (LastChar == tok_hash) {
            do {
                LastChar = GetChar();
            } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

            if (LastChar != EOF) {
                return getNextToken();
            }
        }

        if (LastChar == EOF) {
            return CurTok = tok_eof;
        }

        Token ThisChar = LastChar;
        LastChar = GetChar();
        return CurTok = ThisChar;
    }

    Token SavedLastChar = LastChar;
    SourceLocation SavedCurLoc = CurLoc;
    Token SavedCurTok = CurTok;
    string::size_type SavedIndex = Index;
    string SavedIdentifierStr = IdentifierStr;

    Token Tok = (Token)0;
    for (unsigned i = 0; i < ForwardStep; i++) {
        Tok = getNextToken();
    }
    LastChar = SavedLastChar;
    CurLoc = SavedCurLoc;
    CurTok = SavedCurTok;
    Index = SavedIndex;
    IdentifierStr = SavedIdentifierStr;

    return Tok;
}
