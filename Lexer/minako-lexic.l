%{

#include <stdio.h>
#include <stdlib.h>
#include "minako.h"
#include <math.h>
#include <string.h>
yystype yylval;
int fileno(FILE *stream);

%}
DIGIT [0-9]
LETTER [a-zA-Z]
INTEGER {DIGIT}+
FLOAT ({INTEGER}"."{INTEGER})|"."{INTEGER} 
%option noyywrap
%option nounput
%option nodefault
%option yylineno	
%option noinput			
%%

"/*"[^*]*[*]+([^*/][^*]*[*]+)*"/" {} /* skip multi-line comment */

"//"[^\n]*  {} /* skip comment line */

[\"][^\n\"]*[\"] { char* newString = yytext; newString++; newString[strlen(newString)-1] = '\0'; yylval.string = newString; return CONST_STRING;} /* STRING */  

{INTEGER}    { yylval.intValue = atoi(yytext); return CONST_INT; } /*INTEGER */  

{FLOAT}([eE]([-+])?{INTEGER})?|{INTEGER}[eE]([-+])?{INTEGER} {yylval.floatValue = atof(yytext); return CONST_FLOAT; } /*FLOAT */ 

"true"|"false" { yylval.intValue = (yytext[0] == 't');  return CONST_BOOLEAN;} /*BOOLEAN */

"bool" {return KW_BOOLEAN;} /* KEYWORDS */
"do" {return KW_DO;}
"else" {return KW_ELSE;}
"float" {return KW_FLOAT;}
"for" {return KW_FOR;}
"if" {return KW_IF;}
"int" {return KW_INT;}
"printf" {return KW_PRINTF;}
"return" {return KW_RETURN;}
"void" {return KW_VOID;}
"while" {return KW_WHILE;}

"+"|"-"|"*"|"/"|"=" { yylval.intValue = yytext[0];  return yylval.intValue;} /*OPERATORS (single char)*/

"&&" {return AND;} /* OPERATORS (255+) */
"||" {return OR;}
"==" {return EQ;}
"!=" {return NEQ;}
"<=" {return LEQ;}
">=" {return GEQ;}
"<" {return LSS;}
">" {return GRT;}


","|";"|"("|")"|"{"|"}" { yylval.intValue = yytext[0];  return yylval.intValue;} /*OTHER TOKENS */

({LETTER})+({DIGIT}|{LETTER})* { yylval.string = yytext; return ID;} /*ID */

[ \t\n\r]+          /* skip whitespace */

.   { printf("Unrecognized Token: \"%s\" , exiting\n", yytext); return -1;}  // exit upon unrecognized token

<<EOF>> {return -1;}

%%

