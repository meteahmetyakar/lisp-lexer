    /* DEFINITION PART */

%{  
%}

    /* Definition of comment,values and idendifier */

COMMENT [;]{2}.*
VALUEF [0-9]*+[b]*[0-9]*
IDENTIFIER [a-zA-Z][a-zA-Z0-9]*

    /* END OF DEFINITION PART */


    /* RULE PART */
%%

    /* OPERATORS */

[+]     {printf("OP_PLUS\n");}
[-]     {printf("OP_MINUS\n");}
[/]     {printf("OP_DIV\n");}
[*]     {printf("OP_MULT\n");}
[(]     {printf("OP_OP\n");}
[)]     {printf("OP_CP\n");}
[,]     {printf("OP_COMMA\n");}

    /* KEYWORDS */

and     {printf("KW_AND\n");}
or      {printf("KW_OR\n");}
not     {printf("KW_NOT\n");}
equal   {printf("KW_EQUAL\n");}
less    {printf("KW_LESS\n");}
nil     {printf("KW_NIL\n");}
list    {printf("KW_LIST\n");}
append  {printf("KW_APPEND\n");}
concat  {printf("KW_CONCAT\n");}
set     {printf("KW_SET\n");}
def     {printf("KW_DEF\n");}
for     {printf("KW_FOR\n");}
if      {printf("KW_IF\n");}
exit    {printf("KW_EXIT\n"); return 0;}
load    {printf("KW_LOAD\n");}
display {printf("KW_DISPLAY\n");}
true    {printf("KW_TRUE\n");}
false   {printf("KW_FALSE\n");}


and[^\n\t ]*      {printf("Lexically incorrect usage of KW_AND : '%s'.\n", yytext);}
or[^\n\t ]*       {printf("Lexically incorrect usage of KW_OR : '%s'.\n", yytext);}
not[^\n\t ]*      {printf("Lexically incorrect usage of KW_NOT : '%s'.\n", yytext);}
equal[^\n\t ]*    {printf("Lexically incorrect usage of KW_EQUAL: '%s'.\n", yytext);}
less[^\n\t ]*     {printf("Lexically incorrect usage of KW_LESS : '%s'.\n", yytext);}
nil[^\n\t ]*      {printf("Lexically incorrect usage of KW_NIL : '%s'.\n", yytext);}
list[^\n\t ]*     {printf("Lexically incorrect usage of KW_LIST : '%s'.\n", yytext);}
append[^\n\t ]*   {printf("Lexically incorrect usage of KW_APPEND : '%s'.\n", yytext);}
concat[^\n\t ]*   {printf("Lexically incorrect usage of KW_CONCAT : '%s'.\n", yytext);}
set[^\n\t ]*      {printf("Lexically incorrect usage of KW_SET : '%s'.\n", yytext);}
def[^\n\t ]*      {printf("Lexically incorrect usage of KW_DEF : '%s'.\n", yytext);}
for[^\n\t ]*      {printf("Lexically incorrect usage of KW_FOR : '%s'.\n", yytext);}
if[^\n\t ]*       {printf("Lexically incorrect usage of KW_IF : '%s'.\n", yytext);}
exit[^\n\t ]*     {printf("Lexically incorrect usage of KW_EXIT : '%s'.\n", yytext);}
load[^\n\t ]*     {printf("Lexically incorrect usage of KW_LOAD : '%s'.\n", yytext);}
display[^\n\t ]*  {printf("Lexically incorrect usage of KW_DISPLAY : '%s'.\n", yytext);}
true[^\n\t ]*     {printf("Lexically incorrect usage of KW_TRUE : '%s'.\n", yytext);}
false[^\n\t ]*    {printf("Lexically incorrect usage of KW_AND : '%s'.\n", yytext);}


    /* rules for comment,values and identifier */

{COMMENT}       {printf("COMMENT\n");}
{VALUEF}        {printf("VALUEF\n");}
{IDENTIFIER}    {printf("IDENTIFIER\n");}

    /* rules for white spaces */
[\t]    {;}
[\n]    {;}
[ ]     {;}

    /* Error check for undefined statements */
[!\"#$%&'.:<=>?@[\]^_`{|}~] { printf("Error!! Undefined statement: '%s'\n",yytext);}

%%
    /* END OF RULE */


int yywrap() {}
int main(int argc , char** argv)
{
    if(argc == 1)
    {
        printf("Type exit for exit\n");
        printf("Lexer started:\n");
    }

    else if (argc == 2)
    {
        FILE *fp;
        fp = fopen(argv[1], "r");
        yyin = fp;
    }

    yylex();
    return 0;
}