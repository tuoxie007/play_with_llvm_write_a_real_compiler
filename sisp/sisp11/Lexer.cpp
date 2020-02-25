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

//SourceLocation CurLoc;
//SourceLocation LexLoc = {1, 0};
//
//Token CurTok;
//
//string IdentifierStr;
//double NumVal;
//string TheCode;

int Lexer::GetChar() {
    if (Index >= TheCode.length())
        return EOF;
    char CurChar = TheCode.at(Index++);
//    cout << "getchar [" << string(1, CurChar) << "]" << endl;

    if (CurChar == tok_newline || CurChar == tok_return) {
        LexLoc.Line++;
        LexLoc.Col = 0;
    } else {
        LexLoc.Col++;
    }

    return CurChar;
}

int Lexer::gettok() {
    while (isspace(LastChar)) {
        LastChar = GetChar();
    }

    CurLoc = LexLoc;

    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum(LastChar = GetChar())) {
            IdentifierStr += LastChar;
        }

        if (IdentifierStr == "def") {
            return tok_def;
        }
        if (IdentifierStr == "extern")
            return tok_extern;
        if (IdentifierStr == "exit")
            exit(0);
        if (IdentifierStr == "if")
            return tok_if;
        if (IdentifierStr == "then")
            return tok_then;
        if (IdentifierStr == "else")
            return tok_else;
        if (IdentifierStr == "for")
            return tok_for;
        if (IdentifierStr == "in")
            return tok_in;
        if (IdentifierStr == "unary")
            return tok_unary;
        if (IdentifierStr == "binary")
            return tok_binary;
        if (IdentifierStr == "var")
            return tok_var;
        if (IdentifierStr == "bool")
            return tok_type_bool;
        if (IdentifierStr == "int")
            return tok_type_int;
        if (IdentifierStr == "float")
            return tok_type_float;
        if (IdentifierStr == "string")
            return tok_type_string;

        return tok_identifier;
    }

    if (isdigit(LastChar) || LastChar == tok_dot) {
        string NumStr;
        do {
            NumStr += LastChar;
            LastChar = GetChar();
        } while (isdigit(LastChar) || LastChar == tok_dot);

        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    if (LastChar == tok_hash) {
        do {
            LastChar = GetChar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF) {
            return gettok();
        }
    }

    if (LastChar == EOF) {
        return tok_eof;
    }

    char ThisChar = LastChar;
    LastChar = GetChar();
    return ThisChar;
}

Token Lexer::getNextToken() {
    return CurTok = (Token)gettok();
}
