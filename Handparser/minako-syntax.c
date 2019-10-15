#include <stdio.h>
#include <stdlib.h>
#include "minako.h"
#define EPS -100
#define NOFELEMS(x)  (sizeof(x) / sizeof((x)[0]))
#define PRINTERROR {fprintf(stderr, "ERROR: Syntaxfehler in Zeile (%d)\n", yylineno); exit(-1); }

yystype yylval;
int currentToken;
int nextToken;

void Programm();   
void FuncDef();    
void FuncCall();   
void StmtList();   
void Block();      
void Stmt();       
void IfStmt();     
void ReturnStmt(); 
void Printf();     
void Type();       
void StatAssign(); 
void Assign();     
void Expr();       
void SimplExpr();  
void Term();       
void Factor();     



int firstProgramm[]     = {KW_BOOLEAN, KW_FLOAT, KW_INT, KW_VOID, EOF};
int firstFuncDef[]      = {KW_BOOLEAN, KW_FLOAT, KW_INT, KW_VOID};
int firstFuncCall[]     = {ID};
int firstStmtList[]     = {'{',KW_IF, KW_RETURN, KW_PRINTF, ID};
int firstBlock[]        = {'{', KW_IF, KW_RETURN, KW_PRINTF, ID,EPS};
int firstStmt[]         = {KW_IF, KW_RETURN, KW_PRINTF, ID};
int firstIfStmt[]       = {KW_IF};
int firstReturnStmt[]   = {KW_RETURN};
int firstPrintf[]       = {KW_PRINTF};
int firstType[]         = {KW_BOOLEAN, KW_FLOAT, KW_INT, KW_VOID};
int firstStatAssign[]   = {ID};
int firstAssign[]       = {ID, '-', CONST_INT, CONST_FLOAT, CONST_BOOLEAN, '('};
int firstExpr[]         = {ID, '-', CONST_INT, CONST_FLOAT, CONST_BOOLEAN, '('};
int firstSimplExpr[]    = {ID, '-', CONST_INT, CONST_FLOAT, CONST_BOOLEAN, '('};
int firstTerm[]         = {ID, CONST_INT, CONST_FLOAT, CONST_BOOLEAN, '('};
int firstFactor[]       = {ID, CONST_INT, CONST_FLOAT, CONST_BOOLEAN, '('};


int followProgramm[]    = {EOF};
int followFuncDef[]     = {KW_BOOLEAN, KW_FLOAT, KW_INT, KW_VOID, EOF};
int followFuncCall[]    = {';', ')', EQ, NEQ, LEQ, GEQ, LSS, GRT, '+', '-', OR, KW_BOOLEAN, KW_INT, KW_FLOAT, EOF, '*', '/', AND};
int followStmtList[]    = {'}'};
int followBlock[]       = {'{'};
int followStmt[]        = {'{'};
int followIfStmt[]      = {';'};
int followReturnStmt[]  = {';'};
int followPrintf[]      = {';'};
int followType[]        = {ID};
int followStatAssign[]  = {';', ')', EQ, NEQ, LEQ, GEQ, LSS, GRT, '+', '-', OR, KW_BOOLEAN, KW_INT, KW_FLOAT, EOF, '*', '/', AND};
int followAssign[]      = {';', ')', EQ, NEQ, LEQ, GEQ, LSS, GRT, '+', '-', OR, KW_BOOLEAN, KW_INT, KW_FLOAT, EOF, '*', '/', AND};
int followExpr[]        = {';', ')', EQ, NEQ, LEQ, GEQ, LSS, GRT, '+', '-', OR, KW_BOOLEAN, KW_INT, KW_FLOAT, EOF, '*', '/', AND};
int followSimplExpr[]   = {';', ')', EQ, NEQ, LEQ, GEQ, LSS, GRT, '+', '-', OR, KW_BOOLEAN, KW_INT, KW_FLOAT, EOF, '*', '/', AND};
int followTerm[]        = {';', ')', EQ, NEQ, LEQ, GEQ, LSS, GRT, '+', '-', OR, KW_BOOLEAN, KW_INT, KW_FLOAT, EOF, '*', '/', AND};
int followFactor[]      = {';', ')', EQ, NEQ, LEQ, GEQ, LSS, GRT, '+', '-', OR, KW_BOOLEAN, KW_INT, KW_FLOAT, EOF, '*', '/', AND};

int main(int argc, char* argv[])
{
    if (argc != 2) {
		yyin = stdin;
	} else {
		yyin = fopen(argv[1], "r");
		if (yyin == 0) {
			fprintf(stderr, "Fehler: Konnte Datei %s nicht zum lesen oeffnen.\n", argv[1]);
			exit(-1);
		}
	}

    currentToken = yylex();
    nextToken = yylex();

    Programm();
	return 0;
}

void eat() {
    currentToken = nextToken;
    nextToken = yylex();
}

int isToken(int token) {
    return token == currentToken;
}

void isTokenAndEat(int token) {
    if(isToken(token)) {
        eat();
    } else {
        PRINTERROR
    }
}

int elementOf(int token, int* set, int length) {
    for(int i = 0; i < length; i++) {
        if(token == set[i]) {return 1;}
    }
    return 0;
}

void Programm() {
    while(elementOf(currentToken, firstFuncDef, NOFELEMS(firstFuncDef))) {
        FuncDef();
    } 
    isTokenAndEat(EOF);
    exit(0);
}

void FuncDef() {
    Type();
    isTokenAndEat(ID);
    isTokenAndEat('(');
    isTokenAndEat(')');
    isTokenAndEat('{');
    StmtList();
    isTokenAndEat('}');
}

void FuncCall() {
    isTokenAndEat(ID);
    isTokenAndEat('(');
    isTokenAndEat(')');   
}

void StmtList() {  
    while(elementOf(currentToken, firstBlock, NOFELEMS(firstBlock))) {
        Block();
    }
    if(elementOf(currentToken, followStmtList, NOFELEMS(followStmtList))) {
    } else {
        PRINTERROR
    }    
}

void Block() {     
    if(isToken('{')) {
        isTokenAndEat('{');
        StmtList();
        isTokenAndEat('}');
    } else {
        Stmt();
    }   
}

void Stmt() {    
    if(elementOf(currentToken, firstIfStmt, NOFELEMS(firstIfStmt))) {
        IfStmt();
    } else if(elementOf(currentToken, firstReturnStmt, NOFELEMS(firstReturnStmt))) {
        ReturnStmt();
        isTokenAndEat(';');
    } else if(elementOf(currentToken, firstPrintf, NOFELEMS(firstPrintf))) {
        Printf();
        isTokenAndEat(';');
    } else if(elementOf(currentToken, firstStatAssign, NOFELEMS(firstStatAssign))) {
        StatAssign();
        isTokenAndEat(';');
    } else if(elementOf(currentToken, firstFuncCall, NOFELEMS(firstFuncCall))) {
        FuncCall();
        isTokenAndEat(';');
    } else {
        PRINTERROR
    }     
}

void IfStmt() {   
    isTokenAndEat(KW_IF);
    isTokenAndEat('(');
    Assign();
    isTokenAndEat(')');
    Block();    
}

void ReturnStmt() {   
    isTokenAndEat(KW_RETURN);
    if(elementOf(currentToken, firstAssign, NOFELEMS(firstAssign))) {
        Assign();
    } else if(elementOf(currentToken, followReturnStmt, NOFELEMS(followReturnStmt))) {
    } else {
        PRINTERROR
    }   
}

void Printf() {   
    isTokenAndEat(KW_PRINTF);
    isTokenAndEat('(');
    Assign();
    isTokenAndEat(')');   
}

void Type() {    
    switch(currentToken) {
        case KW_BOOLEAN: isTokenAndEat(KW_BOOLEAN); break;
        case KW_FLOAT: isTokenAndEat(KW_FLOAT); break;
        case KW_INT: isTokenAndEat(KW_INT); break;
        case KW_VOID: isTokenAndEat(KW_VOID); break;
        default: PRINTERROR;
    }    
}

void StatAssign() {   
    isTokenAndEat(ID);
    isTokenAndEat('=');
    if(elementOf(currentToken, firstAssign, NOFELEMS(firstAssign))) {
        Assign();
    } else {PRINTERROR }    
}

void Assign() {    
    if(isToken(ID) && nextToken == '=') {
        isTokenAndEat(ID);
        isTokenAndEat('=');
        Assign();
    } else if(elementOf(currentToken, firstExpr, NOFELEMS(firstExpr))) {
        Expr();
    } else {PRINTERROR }    
}

void Expr() {  
    SimplExpr();
    switch(currentToken) {
        case EQ: isTokenAndEat(EQ);   SimplExpr(); break;
        case NEQ: isTokenAndEat(NEQ); SimplExpr(); break;
        case LEQ: isTokenAndEat(LEQ); SimplExpr(); break;
        case GEQ: isTokenAndEat(GEQ); SimplExpr(); break;
        case LSS: isTokenAndEat(LSS); SimplExpr(); break;
        case GRT: isTokenAndEat(GRT); SimplExpr(); break;
        default: break;
    }   
}

void SimplExpr() {   
    if(isToken('-')) {
        isTokenAndEat('-');
    }
    Term();
    if(isToken('+') || isToken('-') || isToken(OR)) {
        do {              
            switch(currentToken) {
                case '+': isTokenAndEat('+'); break;
                case '-': isTokenAndEat('-'); break;
                case OR : isTokenAndEat(OR); break;
                default: PRINTERROR
            }   
            Term();
        } while(isToken('+') || isToken('-') || isToken(OR));
    } else if(elementOf(currentToken, followTerm, NOFELEMS(followTerm))) {
    } else {PRINTERROR}   
}

void Term() {   
    Factor();
    if(isToken('*') || isToken('/') || isToken(AND)) {
        do {
            switch(currentToken) {
                case '*': isTokenAndEat('*'); break;
                case '/': isTokenAndEat('/'); break;
                case AND : isTokenAndEat(AND); break;
                default: PRINTERROR
            }   
            Factor();
        } while(isToken('*') || isToken('/') || isToken(AND));
    }   
}

void Factor() {  
    switch(currentToken) {
        case CONST_INT:         isTokenAndEat(CONST_INT); break;
        case CONST_FLOAT:       isTokenAndEat(CONST_FLOAT); break;
        case CONST_BOOLEAN:     isTokenAndEat(CONST_BOOLEAN); break;
        case ID:                if(elementOf(nextToken, followFactor, NOFELEMS(followFactor))) {
                                    isTokenAndEat(ID);
                                } else if(nextToken == '(') {
                                    FuncCall();
                                } else {PRINTERROR}
                                break;
        case '(':               isTokenAndEat('(');
                                Assign();
                                isTokenAndEat(')');
                                break;
        default: PRINTERROR
    }
}