//
//  Lexer.hpp
//  sisp
//
//  Created by 徐可 on 2020/2/19.
//  Copyright © 2020 Beibei Inc. All rights reserved.
//

#ifndef Lexer_hpp
#define Lexer_hpp

#include <string>

using namespace std;

typedef enum Token {
    tok_eof = -1,
    tok_class = -2,
    tok_extern = -3,
    tok_identifier = -4,
//    tok_number = -5,
    tok_if = -6,
    tok_then = -7,
    tok_else = -8,
    tok_for = -9,
    tok_in = -10,
    tok_binary = -11,
    tok_unary = -12,
    tok_var = -13,
    tok_type_bool = -14,
    tok_type_int = -15,
    tok_type_float = -16,
    tok_type_string = -17,
    tok_type_object = -18,
    tok_integer_literal = -19,
    tok_float_literal = -20,

    tok_left_paren = '(',
    tok_right_paren = ')',
    tok_equal = '=',
    tok_less = '<',
    tok_greater = '>',
    tok_comma = ',',
    tok_colon = ';',
    tok_hash = '#',
    tok_dot = '.',
    tok_space = ' ',
    tok_left_bracket = '{',
    tok_right_bracket = '}',
    tok_newline = '\n',
    tok_return = '\r',

    tok_add = '+',
    tok_sub = '-',
    tok_mul = '*',
    tok_div = '/',
    tok_not = '!',
    tok_or = '|',
    tok_and = '&',

} Token;

static string tok_tos(Token t) {
    if (t > 0) {
        return to_string((char)t);
    }
    switch (t) {
        case tok_eof: return "<eof>";
        case tok_class: return "<class>";
        case tok_extern: return "<extern>";
        case tok_identifier: return "<id>";
//        case tok_number: return "<number>";
        case tok_if: return "<if>";
        case tok_else: return "<else>";
        case tok_for: return "<for>";
        case tok_in: return "<in>";
        case tok_binary: return "<binary>";
        case tok_unary: return "<unary>";
        case tok_var: return "<var>";
        case tok_type_bool: return "<bool>";
        case tok_type_int: return "<int>";
        case tok_type_float: return "<float>";
        case tok_type_string: return "<string>";
        case tok_type_object: return "<object>";
        default: return to_string((int)t);
    }
}

struct SourceLocation {
    int Line;
    int Col;
};

class Lexer {
public:

    SourceLocation CurLoc = {1, 0};

    Token CurTok = (Token)0;
    string::size_type Index = 0;
    Token LastChar = tok_space;

    string IdentifierStr;
    long IntegerVal;
    double FloatVal;
    string TheCode;

    Lexer(string &code): TheCode(code) {}
    Token getNextToken(unsigned ForwardStep = 0);
    Token getCurToken() {
        return CurTok;
    }
    Token GetChar();

    Token getVarType() {
        if (CurTok == tok_type_bool ||
            CurTok == tok_type_int ||
            CurTok == tok_type_float ||
            CurTok == tok_type_string ||
            CurTok == tok_type_object) {
            return CurTok;
        }
        return (Token)0;
    }
};

#endif /* Lexer_hpp */
