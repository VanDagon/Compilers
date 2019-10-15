

%code requires {
	#include <stdlib.h>
	#include <stdarg.h>
	#include "symtab.h"
	#include "syntree.h"
	
	extern void yyerror(const char*, ...);
	extern int yylex();
	extern int yylineno;
	extern FILE* yyin;
}

%code provides {
	extern symtab_t* tab;
	extern syntree_t* ast;
}

%code {
	/* globale Zeiger auf Symboltabelle und abstrakten Syntaxbaum */
	symtab_t* tab;
	syntree_t* ast;
	
	/* interner (globaler) Zeiger auf die aktuell geparste Funktion */
	static symtab_symbol_t* func;
	
	/**@brief Kombiniert zwei Ausdrücke einer binären Operation und stellt
	 * sicher, dass sie auf deren Typden definiert ist.
	 * @param lhs  linke Seite
	 * @param rhs  rechte Seite
	 * @param op   Operator
	 * @return ID des Operatorknoten
	 */
	static syntree_nid
	combine(syntree_nid id1, syntree_nid id2, syntree_node_tag op);
	
	/* Hilfsfunktionen */
	
	/**@brief Gibt den Zeiger auf einen Knoten der entsprechenden ID zurück.
	 */
	static inline syntree_node_t*
	nodePtr(syntree_nid id) { return syntreeNodePtr(ast, id); }
	
	/**@brief Gibt den Knotentyp zurück.
	 */
	static inline syntree_node_type
	nodeType(syntree_nid id) { return nodePtr(id)->type; }
	
	/**@brief Gibt den Wert eines Knotens zurück.
	 */
	static inline union syntree_node_value_u*
	nodeValue(syntree_nid id) { return &nodePtr(id)->value; }
	
	/**@brief Gibt den ersten Kindknoten eines Containers zurück.
	 */
	static inline syntree_nid
	nodeFirst(syntree_nid id) { return nodePtr(id)->value.container.first; }
	
	/**@brief Gibt den Folgeknoten eines Knotens zurück.
	 */
	static inline syntree_nid
	nodeNext(syntree_nid id) { return nodePtr(id)->next; }
}
//%error-verbose
%union {
	char* string;
	double floatValue;
	int intValue;
	
	symtab_symbol_t* symbol;
	syntree_nid node;
	syntree_node_type type;
}

%printer { fprintf(yyoutput, "\"%s\"", $$); } <string>
%printer { fprintf(yyoutput, "%g", $$); } <floatValue>
%printer { fprintf(yyoutput, "%i", $$); } <intValue>
%printer {
	if ($$ != 0)
	{
		putc('\n', yyoutput);
		syntreePrint(ast, $$, yyoutput, 1);
	}
} <node>
%printer {
	putc('\n', yyoutput);
	symtabPrint(tab, yyoutput);
} declassignment functiondefinition

%destructor { free($$); } <string>

/* used tokens (KW is abbreviation for keyword) */
%token AND
%token OR
%token EQ
%token NEQ
%token LEQ
%token GEQ
%token LSS
%token GRT
%token KW_BOOLEAN
%token KW_DO
%token KW_ELSE
%token KW_FLOAT
%token KW_FOR
%token KW_IF
%token KW_INT
%token KW_PRINTF
%token KW_RETURN
%token KW_VOID
%token KW_WHILE
%token <intValue>   CONST_INT
%token <floatValue> CONST_FLOAT
%token <intValue>   CONST_BOOLEAN
%token <string>     CONST_STRING
%token <string>     ID

/* definition of association and precedence of operators */
%left '+' '-' OR
%left '*' '/' AND
%nonassoc UMINUS
%nonassoc ASSIGN_PREC

/* workaround for handling dangling else */
/* LOWER_THAN_ELSE stands for a not existing else */
%nonassoc LOWER_THAN_ELSE
%nonassoc KW_ELSE

%type <node> program declassignment
%type <node> opt_argumentlist argumentlist
%type <node> statementlist statement block
%type <node> ifstatement forstatement dowhilestatement whilestatement opt_else
%type <node> returnstatement printf statassignment
%type <node> expr simpexpr functioncall assignment

%type <type> type
%type <symbol> parameter

%%

start:
	program { 
		symtab_symbol_t* entry = symtabLookup(tab, "main");
		if (entry == NULL || entry->is_function != 1) {yyerror("main function not found");exit(1);}
		if (entry->type != SYNTREE_TYPE_Void) {yyerror("main type is not void");exit(1);}
		if (symtabParamFirst(entry)!=NULL){yyerror("main function has parameter(s)");exit(1);}
		nodeValue(0)->program.body = syntreeNodeAppend(ast, $program, entry->body);
		nodeValue(0)->program.globals = symtabMaxGlobals(tab);
	}
	;

/* see EBNF grammar for further information */
program:
	/* empty */
		{ $$ = syntreeNodeEmpty(ast, SYNTREE_TAG_Sequence); }
	| program[prog] declassignment[decl] ';'
		{ syntreeNodeAppend(ast, $prog, $decl); }
	| program functiondefinition
	;

functiondefinition:
	type ID[name] {
		/* globale Zeiger auf das aktuelle Funktionssymbol */
		func = symtabSymbol($name, $type);
		func->is_function = 1;
		func->body = syntreeNodeEmpty(ast, SYNTREE_TAG_Function);
		
		if (symtabInsert(tab, func) != 0)
			yyerror("double declaration of function %s.", $name);
	}
	'(' opt_parameterlist ')' {symtabEnter(tab);} '{' statementlist[body] '}' {
		syntreeNodeAppend(ast, func->body, $body);
		nodeValue(func->body)->function.locals = symtabMaxLocals(tab);
		symtabLeave(tab);
	}
	;

opt_parameterlist:
	/* empty */
	| parameterlist
	;

parameterlist:
	parameter
	| parameter ',' parameterlist
	;

parameter:
	type ID[name] {
		symtab_symbol_t* newParam = symtabSymbol($name,$1);
		symtabParam(func,newParam); 
		if (symtabInsert(tab, newParam) != 0){ 
			yyerror("double declaration of variable %s in this scope.", $name);exit(1);}
	}
	;

functioncall:
	ID[name] '(' opt_argumentlist[args] ')' { 
		symtab_symbol_t* fn = symtabLookup(tab, $name);
		symtab_symbol_t* par = symtabParamFirst(fn);
		int paramCounter= 0;
		int argCounter = 0;
		int * paramArray = malloc(sizeof(int));
		syntree_node_t* arg = nodePtr($args);
		syntree_node_t* arg1 = nodePtr(arg->value.container.first);
		while (par != NULL)
		{
			paramArray = realloc(paramArray,sizeof(int)*(paramCounter+1));
			paramArray[paramCounter] = par->type;
			par = par->par_next;
			paramCounter++;
		}
		if (arg1->type==0 && paramCounter>0){yyerror("too few arguments");exit(1);}
		while (1)
		{
			if (++argCounter>paramCounter){yyerror("too many arguments");exit(1);}
			if (arg1->type!=paramArray[paramCounter-argCounter]){yyerror("argument type does not match the parameters");exit(1);}
			if (arg1 == nodePtr(arg->value.container.last)){break;}
			arg1 = nodePtr(arg1->next);	

		}
		if (paramCounter > argCounter) {yyerror("too few arguments");exit(1);}

		if (fn == NULL)
			yyerror("unknown symbol '%s'", $name);
	

		$$ = syntreeNodePair(ast, SYNTREE_TAG_Call, $args, fn->body);
		nodePtr($$)->type = fn->type;
		
	}
	;

opt_argumentlist:
	/* empty */
		{ $$ = syntreeNodeEmpty(ast, SYNTREE_TAG_Sequence); }
	| argumentlist
	;

argumentlist:
	assignment[expr]
		{ $$ = syntreeNodeTag(ast, SYNTREE_TAG_Sequence, $expr); }
	| argumentlist[list] ',' assignment[elem]
		{ $$ = syntreeNodeAppend(ast, $list, $elem); }
	;

statementlist:
	/* empty */
		{ $$ = syntreeNodeEmpty(ast, SYNTREE_TAG_Sequence); }
	| statementlist[list] statement[elem]
		{ $$ = syntreeNodeAppend(ast, $list, $elem); }
	;

block:
	'{'
		statementlist[body]
	'}' { $$ = $body; }
	;

statement:
	  ifstatement
	| forstatement
	| whilestatement
	| returnstatement ';'
	| dowhilestatement ';'
	| printf ';'
	| declassignment ';'
	| statassignment ';'
	| functioncall ';'
	| block
	;

ifstatement:
	KW_IF '(' assignment[cond] ')' statement[then] opt_else[else] {
		if (nodePtr($cond)->type != SYNTREE_TYPE_Boolean && nodePtr($cond)->type != SYNTREE_TYPE_Integer) ///
		{yyerror("condition in if statement is not of type boolean");exit(1);} 
		$$ = syntreeNodePair(ast, SYNTREE_TAG_If, $cond, $then);
		$$ = syntreeNodeAppend(ast, $$, $else);
	}
	;

/* KW_ELSE has higher precedence, so an occuring 'else' will cause the */
/* execution of the second rule */
opt_else:
	/* empty */ %prec LOWER_THAN_ELSE
		{ $$ = 0; }
	| KW_ELSE statement[else]
		{ $$ = $else; }
	;

forstatement:
	KW_FOR '(' declassignment[init] ';' expr[cond] ';' statassignment[step] ')' {symtabEnter(tab);} statement[body] {
		if (nodePtr($cond)->type != SYNTREE_TYPE_Boolean && nodePtr($cond)->type != SYNTREE_TYPE_Integer) ///
		{yyerror("condition in for statement is not of type boolean");exit(1);} 
		$$ = syntreeNodePair(ast, SYNTREE_TAG_For, $init, $cond);
		$$ = syntreeNodeAppend(ast, $$, $step);
		$$ = syntreeNodeAppend(ast, $$, $body);
		symtabLeave(tab);
	}
	| KW_FOR '(' statassignment[init] ';' expr[cond] ';' statassignment[step] ')' {	symtabEnter(tab);} statement[body] {

		if (nodePtr($cond)->type != SYNTREE_TYPE_Boolean) {yyerror("condition in for statement is not of type boolean");exit(1);} ///
		$$ = syntreeNodePair(ast, SYNTREE_TAG_For, $init, $cond);
		$$ = syntreeNodeAppend(ast, $$, $step);
		$$ = syntreeNodeAppend(ast, $$, $body);
		symtabLeave(tab);
	}
	;

dowhilestatement:
	KW_DO {symtabEnter(tab);} statement[body] {symtabLeave(tab);} KW_WHILE '(' assignment[cond] ')' {
		if (nodePtr($cond)->type != SYNTREE_TYPE_Boolean && nodePtr($cond)->type != SYNTREE_TYPE_Integer)  
		{yyerror("condition in do-while statement is not of type boolean");exit(1);} 
		$$ = syntreeNodePair(ast, SYNTREE_TAG_DoWhile, $cond, $body);
	}
	;

whilestatement:	
	KW_WHILE '(' assignment[cond] ')' {symtabEnter(tab);} statement[body] {
		if (nodePtr($cond)->type != SYNTREE_TYPE_Boolean && nodePtr($cond)->type != SYNTREE_TYPE_Integer) 
		{yyerror("condition in while statement is not of type boolean");exit(1);} 
		$$ = syntreeNodePair(ast, SYNTREE_TAG_While, $cond, $body);
		symtabLeave(tab);
	}
	;

returnstatement:
	KW_RETURN {
		if (func->type != SYNTREE_TYPE_Void){yyerror("invalid return type for function %s ", func->name);exit(1);} 
		$$ = syntreeNodeEmpty(ast, SYNTREE_TAG_Return);
	}
	| KW_RETURN assignment[expr] {
		int floatTest = (func->type == SYNTREE_TYPE_Float && nodePtr($expr)->type == SYNTREE_TYPE_Integer); 
		if (func->type != nodePtr($expr)->type && !floatTest) {yyerror("invalid return type for function %s ", func->name);exit(1);} 
		if (func->type != nodeType($expr))
			$expr = syntreeNodeCast(ast, func->type, $expr);
		
		$$ = syntreeNodeTag(ast, SYNTREE_TAG_Return, $expr);
	}
	;

printf:
	KW_PRINTF '(' assignment[arg] ')'
		{ $$ = syntreeNodeTag(ast, SYNTREE_TAG_Print, $arg); }
	| KW_PRINTF '(' CONST_STRING[arg] ')'
		{ $$ = syntreeNodeTag(ast, SYNTREE_TAG_Print, syntreeNodeString(ast, $arg)); }
	;

declassignment:
	type ID[decName] {
		symtab_symbol_t* newVar = symtabSymbol($decName,$1);
		if (symtabInsert(tab, newVar) != 0){ 
			yyerror("double declaration of variable %s in this scope.", $decName);exit(1);}
		$$ = 0;
	}
	| type ID[name] '=' assignment[expr] {
		symtab_symbol_t* newVar = symtabSymbol($name,$1);
		if (symtabInsert(tab, newVar) != 0){ 
		yyerror("double declaration of variable %s in this scope.", $name);exit(1);}
		symtab_symbol_t* sym = symtabSymbol($name, $type);
		int floatNInt = 0;
		if (sym->type == SYNTREE_TYPE_Float && nodePtr($expr)->type==SYNTREE_TYPE_Integer){floatNInt = 1;}
		if (sym->type != nodePtr($expr)->type && !floatNInt ) {yyerror("type mismatch in assignment");exit(1);}
		 if (sym->type != nodeType($expr))
		 	$expr = syntreeNodeCast(ast, sym->type, $expr);
		
		$$ = syntreeNodePair(ast, SYNTREE_TAG_Assign,
		                     syntreeNodeVariable(ast, sym), $expr);


	}
	;

type:
	KW_BOOLEAN { $$ = SYNTREE_TYPE_Boolean; }
	| KW_FLOAT { $$ = SYNTREE_TYPE_Float; }
	| KW_INT   { $$ = SYNTREE_TYPE_Integer; }
	| KW_VOID  { $$ = SYNTREE_TYPE_Void; }
	;

statassignment:
	ID[name] '=' assignment[expr] {
		symtab_symbol_t* sym = symtabLookup(tab, $name);
		int floatNInt = 0; 
		if (sym->type == SYNTREE_TYPE_Float && nodePtr($expr)->type==SYNTREE_TYPE_Integer){floatNInt = 1;}
		if (sym->type != nodePtr($expr)->type && !floatNInt) {yyerror("type mismatch in assignment");exit(1);}
		if (sym==NULL){yyerror("undeclared variable %s ",$name);exit(1);} 

		if (sym->type != nodeType($expr))
			$expr = syntreeNodeCast(ast, sym->type, $expr);
		
		$$ = syntreeNodePair(ast, SYNTREE_TAG_Assign,
		                     syntreeNodeVariable(ast, sym), $expr);
		
	}
	;

assignment:
	ID[name] '=' assignment[expr] {
		symtab_symbol_t* sym = symtabLookup(tab, $name);
		int floatNInt = 0; 
		if (sym->type == SYNTREE_TYPE_Float && nodePtr($expr)->type==SYNTREE_TYPE_Integer){floatNInt = 1;} 
		if (sym->type != nodePtr($expr)->type && !floatNInt) {yyerror("type mismatch in assignment");exit(1);}
		if (sym==NULL){yyerror("undeclared variable %s ",$name);exit(1);} 

		if (sym->type != nodeType($expr))
			$expr = syntreeNodeCast(ast, sym->type, $expr);
		
		$$ = syntreeNodePair(ast, SYNTREE_TAG_Assign,
		                     syntreeNodeVariable(ast, sym), $expr);
		nodePtr($$)->type = sym->type;

	}
	| expr
	;

expr:
	simpexpr
	| simpexpr[lhs] EQ  simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_Eqt); }
	| simpexpr[lhs] NEQ simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_Neq); }
	| simpexpr[lhs] LEQ simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_Leq); }
	| simpexpr[lhs] GEQ simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_Geq); }
	| simpexpr[lhs] LSS simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_Lst); }
	| simpexpr[lhs] GRT simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_Grt); }
	;

simpexpr:
	simpexpr[lhs] '+' simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_Plus); }
	| simpexpr[lhs] '-' simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_Minus); } 
	| simpexpr[lhs] OR simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_LogOr); }
	| simpexpr[lhs] '*' simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_Times); }
	| simpexpr[lhs] '/' simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_Divide); }
	| simpexpr[lhs] AND simpexpr[rhs]
		{ $$ = combine($lhs, $rhs, SYNTREE_TAG_LogAnd); }
	| '-' simpexpr[operand] %prec UMINUS {
		$$ = syntreeNodeTag(ast, SYNTREE_TAG_Uminus, $operand);
		nodePtr($$)->type = nodeType($operand);
	}
	| CONST_INT[val]
		{ $$ = syntreeNodeInteger(ast, $val); }
	| CONST_FLOAT[val]
		{ $$ = syntreeNodeFloat(ast, $val); }
	| CONST_BOOLEAN[val]
		{ $$ = syntreeNodeBoolean(ast, $val); }
	| functioncall
	| ID[name] {
		symtab_symbol_t* sym = symtabLookup(tab, $name);
		if (sym==NULL){yyerror("undeclared variable %s ",$name);exit(1);}  

		$$ = syntreeNodeVariable(ast, sym);
	}
	| '(' assignment ')' 
		{ $$ = $assignment; }
	;

%%

int main(int argc, const char* argv[])
{
	symtab_t symtab;
	syntree_t syntree;
	int rc;
	
	/* belege die globalen Zeiger mit den lokalen Werten */
	tab = &symtab;
	ast = &syntree;
	
	/* versuche die Datei aus der Kommandozeile zu öffnen
	 * oder lies aus der Standardeingabe */
	yyin = (argc != 2) ? stdin : fopen(argv[1], "r");
	
	if (yyin == NULL)
		yyerror("couldn't open file %s\n", argv[1]);
	
	/* initialisiere die Hilfsstrukturen */
	if (symtabInit(tab))
	{
		fputs("out-of-memory error\n", stderr);
		exit(-1);
	}
	
	if (syntreeInit(ast))
	{
		fputs("out-of-memory error\n", stderr);
		exit(-1);
	}
	
	/* parse das Programm */
	//yydebug = 1;
	rc = yyparse();
	
	/* gib' Symboltabelle und Syntaxbaum wieder frei */
	symtabRelease(&symtab);
	syntreeRelease(&syntree);
	
	return rc;
}

/**@brief Gibt eine Fehlermeldung aus und beendet das Programm mit Exitcode -1.
 * Die Funktion akzeptiert eine variable Argumentliste und nutzt die Syntax von
 * printf.
 * @param msg  die Fehlermeldung
 * @param ...  variable Argumentliste für die Formatierung von \p msg
 */
void yyerror(const char* msg, ...)
{
	va_list args;
	
	va_start(args, msg);
	fprintf(stderr, "Error in line %d: ", yylineno);
	vfprintf(stderr, msg, args);
	fprintf(stderr, "\n");
	va_end(args);
}

/**@brief Testet, ob eine binäre Operation zwischen zwei getypten Ausdrücken
 * definiert ist und bricht mit einem Fehler ab, falls nicht.
 * @param lhs  Typ des Operanden auf der linken Seite
 * @param rhs  Typ des Operanden auf der rechten Seite
 * @param op   Operator
 * @return resultierender Typ aus der Operation
 */	
static syntree_node_type
combineTypes(syntree_node_type lhs, syntree_node_type rhs, syntree_node_tag op)
{
	/* lhs und rhs können sein:
	 * SYNTREE_TYPE_Void,
	 * SYNTREE_TYPE_Boolean,
	 * SYNTREE_TYPE_Integer,
	 * SYNTREE_TYPE_Float
	 */
	
	int floatTest = 0;
	if (lhs == SYNTREE_TYPE_Float || rhs == SYNTREE_TYPE_Float) {floatTest = 1;}
	switch (op)  
	{
	case SYNTREE_TAG_Eqt: return SYNTREE_TYPE_Boolean;
	case SYNTREE_TAG_Neq: return SYNTREE_TYPE_Boolean;
	case SYNTREE_TAG_Leq: return SYNTREE_TYPE_Boolean;
	case SYNTREE_TAG_Geq: return SYNTREE_TYPE_Boolean;
	case SYNTREE_TAG_Lst: return SYNTREE_TYPE_Boolean;
	case SYNTREE_TAG_Grt: return SYNTREE_TYPE_Boolean;
	case SYNTREE_TAG_LogOr: return SYNTREE_TYPE_Boolean;
	case SYNTREE_TAG_LogAnd: return SYNTREE_TYPE_Boolean;
	case SYNTREE_TAG_Plus: if (floatTest){return SYNTREE_TYPE_Float;}else{return SYNTREE_TYPE_Integer;}
	case SYNTREE_TAG_Minus: if (floatTest){return SYNTREE_TYPE_Float;}else{return SYNTREE_TYPE_Integer;}
	case SYNTREE_TAG_Times: if (floatTest){return SYNTREE_TYPE_Float;}else{return SYNTREE_TYPE_Integer;}
	case SYNTREE_TAG_Divide: if (floatTest){return SYNTREE_TYPE_Float;}else{return SYNTREE_TYPE_Integer;}
		break;
		
	default:
	 	yyerror("unknown operation (internal error)");
	}
	

	return SYNTREE_TYPE_Integer;
}

syntree_nid
combine(syntree_nid lhs, syntree_nid rhs, syntree_node_tag op)
{
	syntree_nid res;
	syntree_node_type lhs_type = nodeType(lhs);
	syntree_node_type rhs_type = nodeType(rhs);
	syntree_node_type type = combineTypes(lhs_type, rhs_type, op);
	int floatTest = ( lhs_type == SYNTREE_TYPE_Float || rhs_type == SYNTREE_TYPE_Float);
	
	if ((lhs_type != rhs_type) && (floatTest))
	{
		/* Situation: ein Integer und ein Float (impliziter Cast) */
		if (lhs_type != SYNTREE_TYPE_Float)
			lhs = syntreeNodeCast(ast, SYNTREE_TYPE_Float, lhs);
		else if (rhs_type != SYNTREE_TYPE_Float)
			rhs = syntreeNodeCast(ast, SYNTREE_TYPE_Float, rhs);
	}

	res = syntreeNodePair(ast, op, lhs, rhs);
	nodePtr(res)->type = type;	

	switch (op)  
	{
	case SYNTREE_TAG_Eqt: {nodePtr(res)->value.boolean = (nodePtr(lhs)->value.boolean == nodePtr(rhs)->value.boolean); break;}
	case SYNTREE_TAG_Neq: {nodePtr(res)->value.boolean = (nodePtr(lhs)->value.boolean != nodePtr(rhs)->value.boolean); break;}
	case SYNTREE_TAG_Leq: {nodePtr(res)->value.boolean = (nodePtr(lhs)->value.boolean <= nodePtr(rhs)->value.boolean); break;}
	case SYNTREE_TAG_Geq: {nodePtr(res)->value.boolean = (nodePtr(lhs)->value.boolean >= nodePtr(rhs)->value.boolean); break;}
	case SYNTREE_TAG_Lst: {nodePtr(res)->value.boolean = (nodePtr(lhs)->value.boolean < nodePtr(rhs)->value.boolean); break;}
	case SYNTREE_TAG_Grt: {nodePtr(res)->value.boolean = (nodePtr(lhs)->value.boolean > nodePtr(rhs)->value.boolean); break;}
	case SYNTREE_TAG_LogOr: { nodePtr(res)->value.boolean = (nodePtr(lhs)->value.boolean || nodePtr(rhs)->value.boolean); break; }
	case SYNTREE_TAG_LogAnd: { nodePtr(res)->value.boolean = (nodePtr(lhs)->value.boolean && nodePtr(rhs)->value.boolean); break; }
	case SYNTREE_TAG_Plus: {if (floatTest){nodePtr(res)->value.real = nodePtr(lhs)->value.real + nodePtr(rhs)->value.real;}
				else{nodePtr(res)->value.integer=nodePtr(lhs)->value.integer + nodePtr(rhs)->value.integer;} break; }
	case SYNTREE_TAG_Minus: {if (floatTest){nodePtr(res)->value.real = nodePtr(lhs)->value.real - nodePtr(rhs)->value.real;}
				else{nodePtr(res)->value.integer=nodePtr(lhs)->value.integer - nodePtr(rhs)->value.integer;} break; } 
	case SYNTREE_TAG_Times: {if (floatTest){nodePtr(res)->value.real = nodePtr(lhs)->value.real * nodePtr(rhs)->value.real;}
				else{nodePtr(res)->value.integer=nodePtr(lhs)->value.integer * nodePtr(rhs)->value.integer;} break; } 
	case SYNTREE_TAG_Divide: 
		
	default: break;
	 	
	}

	return res;
}
