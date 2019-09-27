/*
 * Token.cpp
 *
 *  Created on: Sep 27, 2019
 *      Author: menright
 */

#include "Token.h"

#include <string>
#include <iostream>
#include <cassert>

using namespace std;

tkn token;
int ch;
void nextChar()
{
	ch = cin.get();
}
void next() {
	while (true){
		while (ch == ' ' || ch=='\t' || ch=='\n' || ch == '\r')
			nextChar();
		if (ch=='#') {
			while (ch != '\n' && ch != EOF)
				nextChar();
			if (ch == '\n') {
				continue;
			} else {
				break;
			}
		}
		break;
	}
	if (ch==EOF) {
		token.type = T_EOF;
		return;
	}
	switch (ch) {
	case ';': token.type = T_SEMI; nextChar(); return;
	case '(': token.type = T_LPAREN; nextChar(); return;
	case ')': token.type = T_RPAREN; nextChar(); return;
	case '\'': token.type = T_CHAR; nextChar(); token.value = ch; nextChar(); assert(ch=='\''); nextChar(); return;
	}
	token = tkn();
	if (ch=='"') {
		nextChar();
		while (ch != '"') {
			token.s.append(1,ch);
			nextChar();
		}
		nextChar();
		token.type = T_STR;
		return;
	}
	if (isalpha(ch)) {
		while (isalnum(ch) || ch=='_' || ch=='\'')
		{
			token.s.append(1,ch);
			nextChar();
		}
		token.type = T_NAME;
		if (token.s == "let")
			token.type = T_LET;
		else if (token.s == "in")
			token.type = T_IN;
		else if (token.s == "if")
			token.type = T_IF;
		else if (token.s == "cons")
			token.type = T_CONS;
		else if (token.s == "hd")
			token.type = T_HD;
		else if (token.s == "tl")
			token.type = T_TL;
		else if (token.s == "nil")
			token.type = T_NIL;
		else if (token.s == "null")
			token.type = T_NULL;
		else if (token.s == "chr")
			token.type = T_CHR;
		else if (token.s == "true")
			token.type = T_TRUE;
		else if (token.s == "false")
			token.type = T_FALSE;
		return;
	}
	if (isdigit(ch)) {
		token.value = ch - '0';
		nextChar();
		while (isdigit(ch)) {
			token.value = token.value*10 + ch-'0';
			nextChar();
		}
		token.type = T_NUM;
		return;
	}

	while (ch && !isalnum(ch) && ch!=';' &&ch !=')' && ch!='(' && ch!=' ' && ch!='\t' && ch!='\n' && ch!='#') {
		token.s.append(1,ch);
		nextChar();
	}
	token.type = T_PUNCT;
	switch (token.s.length()) {
	case 1: // 1-character sequence
		switch (token.s[0]){
		case '&': token.type = T_AND; break;
		case '|': token.type = T_OR; break;
		case '<': token.type = T_LT; break;
		case '>': token.type = T_GT; break;
		case '=': token.type = T_EQUALS; break;
		case '+': token.type = T_ADD; break;
		case '-': token.type = T_SUB; break;
		case '*': token.type = T_MUL; break;
		case '/': token.type = T_DIV; break;
		case '%': token.type = T_MOD; break;
		case '!': token.type = T_NOT; break;
		}
		break;
	case 2: // 2-character sequence
		if (token.s=="<=")
			token.type = T_LE;
		else if (token.s==">=")
			token.type = T_GE;
		else if (token.s=="!=")
			token.type = T_NE;
		else if (token.s=="==")
			token.type = T_EQ;
		break;
	}
	return;
}
string token_to_string(const tkn& token) {
	string rv;
	switch (token.type) {
	case T_NAME: rv = "T_NAME: "+ token.s; break;
	case T_PUNCT: rv = "T_PUNCT: "+ token.s; break;
	case T_NUM: rv = "T_NUM: " + to_string(token.value); break;
	case T_SEMI: rv = "T_SEMI"; break;
	case T_LPAREN: rv = "T_LPAREN"; break;
	case T_RPAREN: rv = "T_RPAREN"; break;
	case T_LT: rv = "T_LT"; break;
	case T_MUL: rv = "T_MUL"; break;
	case T_SUB: rv = "T_SUB"; break;
	case T_ADD: rv = "T_ADD"; break;
	case T_DIV: rv = "T_DIV"; break;
	case T_MOD: rv = "T_MOD"; break;
	case T_IN: rv = "T_IN"; break;
	case T_LET: rv = "T_LET"; break;
	case T_NOT: rv = "T_NOT"; break;
	default: rv = to_string((int)token.type); break;
	}
	return rv;
}



