%{

#include <stdio.h>
#include <stdlib.h>
#include "urlscanner.h"
#include <math.h>
#include <string.h>
yystype yylval;
int fileno(FILE *stream);
int href = 0;

%}

%option noyywrap
%option nounput
%option yylineno
%option noinput			
%%

"href=\""[^\"]* {href ++; char * newString = yytext; newString += 6; yylval = newString; return TOKEN_URL;} 

">"[^>]*"</a" {if (href>0){href--; char * newString = yytext; newString ++; newString[strlen(newString)-3] = '\0'; yylval = newString; return TOKEN_TEXT;} }

[ \t\n\r]+          /* skip whitespace */

. {}

<<EOF>> {return -1;}

%%

