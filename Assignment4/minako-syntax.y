//%define parse.error verbose 
//%define parse.trace 

%code requires {
	#include <stdio.h>
	#include <stdlib.h>
	extern void yyerror(const char*);
	extern FILE* yyin;
}

%code {
	extern int yylex();
	extern int yylineno;
}
%error-verbose

%union {
	char* string;
	double floatValue;
	int intValue;
}

%start program

%token EQ            "=="
%right ASSIGN	     '='
%token NEQ           "!="
%right LEQ           "<="
%left GEQ            ">="
%nonassoc LSS        "<"
%nonassoc GRT        ">"
%left  PLUS 	     "+"
%left  MINUS	     "-"
%nonassoc OR         "||"
%right MUL 	     "*"
%right DIV	     "/"
%token AND           "&&"
%token KW_BOOLEAN    "bool"
%token KW_DO         "do"
%token KW_FLOAT      "float"
%token KW_FOR        "for"
%token KW_IF         "if"
%nonassoc KW_ELSE    "else"
%token KW_INT        "int"
%token KW_PRINTF     "printf"
%token KW_RETURN     "return"
%token KW_VOID       "void"
%token KW_WHILE      "while"
%token CONST_INT     "integer literal"
%token CONST_FLOAT   "float literal"
%token CONST_BOOLEAN "boolean literal"
%token CONST_STRING  "string literal"
%token ID            "identifier"
%token UMINUS	     

%% 	

id : ID

factor	: CONST_INT 
	| CONST_FLOAT 
	| CONST_BOOLEAN 
	| functioncall 
	| id 
	| '(' assignment ')'
	;

term : factor 
	| term '*' factor 
	| term  '/' factor 
	| term "&&" factor
	;

simpexpr : simpexpr '+' term 
		| simpexpr '-' term
		| simpexpr OR term
		| term
		| '-' term %prec UMINUS
		;

expr : simpexpr 
	| expr "==" simpexpr 
	| expr "!=" simpexpr 
	| expr "<=" simpexpr 
	| expr ">=" simpexpr 
	| expr '<' simpexpr 
	| expr '>' simpexpr
	;

assignment : id '=' assignment 
		| expr
		;

statassignment : id '=' assignment
		;

type : KW_BOOLEAN 
	| KW_FLOAT 
	| KW_INT 
	| KW_VOID
	;

declassignment : type id 
		| type id '=' assignment
		;

printf : KW_PRINTF '(' CONST_STRING ')'
		|  KW_PRINTF '(' assignment ')' 
		;

returnstatement : KW_RETURN 
		| KW_RETURN assignment
		;

whilestatement : KW_WHILE '(' assignment ')' block
		;

dowhilestatement : KW_DO block KW_WHILE '(' assignment ')'
		;

forstatement : KW_FOR '(' statassignment 
		| declassignment ';' expr ';' statassignment ')' block
		;

ifstatement : KW_IF '(' assignment ')' block 
		| ifstatement KW_ELSE block %prec KW_ELSE
		;

statement : ifstatement 
		| forstatement 
		| whilestatement 
		| returnstatement ';'
		| dowhilestatement ';' 
		| printf ';' 
		| declassignment ';' 
		| statassignment ';'
		| functioncall ';' 
		;

block : '{' statementlist '}' 
		| statement
		;

statementlist : /*empty*/ 
		| statementlist block 
		; 

A : /*empty*/ 
	| A assignment ','
	;

functioncall : id '(' ')' 
		| id '(' A assignment ')'
		;

parameterlist : type id 
		| parameterlist ',' type id
		;

functiondefinition : type id '('/*empty*/')''{' statementlist '}' 
			|  type id '('parameterlist')' '{' statementlist '}'
			; 

B : /* empty */
	| B declassignment ';'
	| B functiondefinition 
	;

program : declassignment ';' B {return 0;}
	| functiondefinition B  {return 0;}
	;
 
%%

int main(int argc, char **argv)
{
	//yydebug=1;
	if (argc == 1) {yyin = stdin; return yyparse();}
  	FILE* f = fopen(argv[1], "r");
    	  if (f != NULL)
	  {
	    yyin = f;
	    return yyparse();
	  }
	  else 
	  {
	    fprintf(stderr, "File (%s) not found, exiting...\n",argv[1]); 
	    exit(1);
	  }
	return 1;
}

void  yyerror(const char* s)
{
  extern int yylineno;
  extern char *yytext;	
  
  fprintf(stderr, "%s \n",s);
  fprintf(stderr, "On line %d: \"%s\" \n" ,yylineno,yytext);
  exit(1);
}

