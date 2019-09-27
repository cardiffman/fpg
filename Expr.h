/*
 * Expr.h
 *
 *  Created on: Sep 27, 2019
 *      Author: menright
 */

#ifndef EXPR_H_
#define EXPR_H_

#include <string>
#include <list>
#include <stddef.h>

struct ExprVar;
struct ExprNum;
struct ExprApp;
struct ExprLet;
struct ExprBool;
struct ExprStr;
struct ExprNil;
struct ExprIf;
struct ExprChar;
struct ExprCons;
struct ExprHd;
struct ExprTl;
struct ExprNull;
struct ExprAdd;
struct ExprSub;
struct ExprMul;
struct ExprDiv;
struct ExprMod;
struct ExprEq;
struct ExprNe;
struct ExprLt;
struct ExprGt;
struct ExprLe;
struct ExprGe;
struct ExprNeg;
struct ExprNot;
struct ExprVisitor {
	virtual ~ExprVisitor() {}
	virtual void visitExprVar(ExprVar* e) = 0;
	virtual void visitExprNum(ExprNum* e) = 0;
	virtual void visitExprApp(ExprApp* e) = 0;
	virtual void visitExprLet(ExprLet* e) = 0;
	virtual void visitExprBool(ExprBool* e) = 0;
	virtual void visitExprChar(ExprChar* e) = 0;
	virtual void visitExprStr(ExprStr* e) = 0;
	virtual void visitExprNil(ExprNil* e) = 0;
	virtual void visitExprIf(ExprIf* e) = 0;
	virtual void visitExprCons(ExprCons* e) = 0;
	virtual void visitExprHd(ExprHd* e) = 0;
	virtual void visitExprTl(ExprTl* e) = 0;
	virtual void visitExprNull(ExprNull* e) = 0;
	virtual void visitExprAdd(ExprAdd* e) = 0;
	virtual void visitExprSub(ExprSub* e) = 0;
	virtual void visitExprMul(ExprMul* e) = 0;
	virtual void visitExprDiv(ExprDiv* e) = 0;
	virtual void visitExprMod(ExprMod* e) = 0;
	virtual void visitExprEq(ExprEq* e) = 0;
	virtual void visitExprNe(ExprNe* e) = 0;
	virtual void visitExprLt(ExprLt* e) = 0;
	virtual void visitExprGt(ExprGt* e) = 0;
	virtual void visitExprLe(ExprLe* e) = 0;
	virtual void visitExprGe(ExprGe* e) = 0;
	virtual void visitExprNeg(ExprNeg* e) = 0;
	virtual void visitExprNot(ExprNot* e) = 0;
};

struct Expr {
	Expr() {}
	virtual ~Expr() {}
	virtual std::string to_string(int) const = 0;
	virtual void visit(ExprVisitor*) = 0;
};

struct ExprVar : public Expr {
	ExprVar(const std::string& v) : var(v) {}
	std::string to_string(int) const { return "VAR " + var; }
	void visit(ExprVisitor* v) { v->visitExprVar(this); }
	std::string var;
};
#define BIG_LIST_AP
/* BIG_LIST_AP is for the alternative of representing an application of a series of atoms
 * using an NAp node with one function and a list of arguments instead of as a chain of
 * NAp nodes with only one argument.
 */
struct ExprApp : public Expr {
#ifdef BIG_LIST_AP
	ExprApp(Expr* fun, Expr* arg) : fun(fun) { args.push_back(arg); }
#else
	ExprApp(Expr* fun, Expr* arg) : fun(fun),arg(arg) { }
#endif
	Expr* fun;
#ifdef BIG_LIST_AP
	std::list<Expr*> args;
#else
	Expr* arg;
#endif
	std::string to_string(int) const;
	void visit(ExprVisitor* v) { v->visitExprApp(this); }
};
struct ExprNum : public Expr {
	ExprNum(ptrdiff_t value) : value(value) {}
	ptrdiff_t value;
	std::string to_string(int) const { return "NUM " + std::to_string((int)value); }
	void visit(ExprVisitor* v) { v->visitExprNum(this); }
};
struct ExprBool : public Expr {
	ExprBool(bool v) : value(v) {}
	std::string to_string(int) const { return value?"true":"false"; }
	void visit(ExprVisitor* v) { v->visitExprBool(this); }
	bool value;
};
struct ExprStr : public Expr {
	ExprStr(const std::string str): str(str) {}
	std::string str;
	std::string to_string(int) const { return "STR \"" + str + '"'; }
	void visit(ExprVisitor* v) { v->visitExprStr(this); }
};
struct ExprNull : public Expr {
	ExprNull(Expr* subject) : subject(subject) {}
	std::string to_string(int c) const { return "NULLP "+subject->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprNull(this); }
	Expr* subject;
};
struct ExprHd : public Expr {
	ExprHd(Expr* subject) : subject(subject) {}
	std::string to_string(int c) const { return "HD "+subject->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprHd(this); }
	Expr* subject;
};
struct ExprTl : public Expr {
	ExprTl(Expr* subject) : subject(subject) {}
	std::string to_string(int c) const { return "Tl "+subject->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprTl(this); }
	Expr* subject;
};
struct ExprAdd : public Expr {
	ExprAdd(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "ADD "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprAdd(this); }
	Expr* left;
	Expr* right;
};
struct ExprSub : public Expr {
	ExprSub(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "SUB "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprSub(this); }
	Expr* left;
	Expr* right;
};
struct ExprMul : public Expr {
	ExprMul(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "MUL "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprMul(this); }
	Expr* left;
	Expr* right;
};
struct ExprDiv : public Expr {
	ExprDiv(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "DIV "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprDiv(this); }
	Expr* left;
	Expr* right;
};
struct ExprMod : public Expr {
	ExprMod(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "MOD "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprMod(this); }
	Expr* left;
	Expr* right;
};
struct ExprLt : public Expr {
	ExprLt(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "LT "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprLt(this); }
	Expr* left;
	Expr* right;
};
struct ExprLe : public Expr {
	ExprLe(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "LE "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprLe(this); }
	Expr* left;
	Expr* right;
};
struct ExprEq : public Expr {
	ExprEq(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "EQ "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprEq(this); }
	Expr* left;
	Expr* right;
};
struct ExprNe : public Expr {
	ExprNe(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "NE "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprNe(this); }
	Expr* left;
	Expr* right;
};
struct ExprGt : public Expr {
	ExprGt(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "GT "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprGt(this); }
	Expr* left;
	Expr* right;
};
struct ExprGe : public Expr {
	ExprGe(Expr* left, Expr* right) : left(left), right(right) {}
	std::string to_string(int c) const { return "GE "+left->to_string(c) + ' ' + right->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprGe(this); }
	Expr* left;
	Expr* right;
};
struct ExprNeg : public Expr {
	ExprNeg(Expr* subj) : subj(subj) {}
	std::string to_string(int c) const { return "NEG "+subj->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprNeg(this); }
	Expr* subj;
};
struct ExprNot : public Expr {
	ExprNot(Expr* subj) : subj(subj) {}
	std::string to_string(int c) const { return "NOT "+subj->to_string(c); }
	void visit(ExprVisitor* v) { v->visitExprNot(this); }
	Expr* subj;
};
struct ExprIf : public Expr {
	ExprIf(Expr* cond, Expr* trueExpr, Expr* falseExpr)
	: cond(cond), trueExpr(trueExpr), falseExpr(falseExpr)
	{}
	std::string to_string(int c) const;
	void visit(ExprVisitor* v) { v->visitExprIf(this); }
	Expr* cond;
	Expr* trueExpr;
	Expr* falseExpr;
};
struct ExprNil : public Expr {
	ExprNil()
	{}
	std::string to_string(int) const { return "nil"; }
	void visit(ExprVisitor* v) { v->visitExprNil(this); }
};
struct ExprChar : public Expr {
	ExprChar(int ch) : ch(ch)
	{}
	std::string to_string(int) const { return "ch("+std::to_string(ch)+")"; }
	void visit(ExprVisitor* v) { v->visitExprChar(this); }
	int ch;
};
struct ExprCons : public Expr {
	ExprCons(Expr* hd, Expr* tl) : hd(hd), tl(tl) {}
	Expr* hd;
	Expr* tl;
	std::string to_string(int) const { return "cons("+hd->to_string(0)+","+tl->to_string(0)+")"; }
	void visit(ExprVisitor* v) { v->visitExprCons(this); }
};
struct ExprLet : public Expr {
	struct Binding {
		std::string name;
		Expr* value;
	};
	std::string to_string(int col) const;
	void visit(ExprVisitor* v) { v->visitExprLet(this); }
	std::list<Binding> bindings;
	Expr* value;
};

extern std::string indent(int col);

#endif /* EXPR_H_ */
