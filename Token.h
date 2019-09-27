/*
 * Token.h
 *
 *  Created on: Sep 27, 2019
 *      Author: menright
 */

#ifndef TOKEN_H_
#define TOKEN_H_

#include <string>

enum Token {
	T_NAME,
	T_NUM,
	T_CHAR,
	T_STR,
	T_PUNCT,
	T_SEMI,
	T_EQUALS,
	T_LPAREN,
	T_RPAREN,
	T_LT,
	T_GT,
	T_LE,
	T_GE,
	T_EQ,
	T_NE,
	T_OR,
	T_AND,
	T_ADD,
	T_SUB,
	T_MUL,
	T_DIV,
	T_MOD,
	T_NOT,
	T_LET,
	T_IN,
	T_IF,
	T_NULL,
	T_NIL,
	T_CONS,
	T_HD,
	T_TL,
	T_CHR,
	T_TRUE,
	T_FALSE,
	T_EOF
};
struct tkn {
	Token type;
	std::string s;
	int value;
};

extern tkn token;

void next();
void nextChar();
std::string token_to_string(const tkn& token);

#endif /* TOKEN_H_ */
