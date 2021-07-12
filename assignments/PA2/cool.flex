/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;
int string_index;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int comment_depth = 0;
bool null_in_string = false;

%}

/*
 * Define names for regular expressions here.
 */

%x COMMENT_LINE
%x COMMENT_BLOCK
%x STRING
%x STRING_ESCAPE
%x STR_NUL_ERROR

/*
 * keywords
 */
DARROW    =>
CLASS     [Cc][Ll][Aa][Ss][Ss]
ELSE      [Ee][Ll][Ss][Ee]
FI        [Ff][Ii]
IF        [Ii][Ff]
IN        [Ii][Nn]
INHERITS  [Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]
LET       [Ll][Ee][Tt]
LOOP      [Ll][Oo][Oo][Pp]
POOL      [Pp][Oo][Oo][Ll]
THEN      [Tt][Hh][Ee][Nn]
WHILE     [Ww][Hh][Ii][Ll][Ee]
CASE      [Cc][Aa][Ss][Ee]
ESAC      [Ee][Ss][Aa][Cc]
OF        [Oo][Ff]
NEW       [Nn][Ee][Ww]
NOT       [Nn][Oo][Tt]
ISVOID    [Ii][Ss][Vv][Oo][Ii][Dd]
ASSIGN    <-
LE        <=


%%

static int stringCaller, commentCaller;



 /*
  *  Nested comments
  */

"--" {
  BEGIN(COMMENT_LINE);
}
<COMMENT_LINE>[^\n]* {}
<COMMENT_LINE>\n {
  ++curr_lineno;
  BEGIN(INITIAL);
}


"(*" {
  commentCaller = INITIAL;
  comment_depth = 1;
  BEGIN(COMMENT_BLOCK);
}
"*)" {
  cool_yylval.error_msg = "Unmatched *)";
  return (ERROR);
}
<COMMENT_BLOCK>"(*" {
  ++comment_depth;
}
<COMMENT_BLOCK>"*)" {
  if (comment_depth == 1) BEGIN(commentCaller);
  else --comment_depth;
}
<COMMENT_BLOCK>\n {
  ++curr_lineno;
}
<COMMENT_BLOCK><<EOF>> {
  cool_yylval.error_msg = "EOF in comment";
  BEGIN(commentCaller);
  return (ERROR);
}
<COMMENT_BLOCK>[^\n] {}


  /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{DARROW} { return (DARROW); }
{CLASS} { return (CLASS); }
{ELSE} { return (ELSE); }
{FI} { return (FI); }
{IF} { return (IF); }
{IN} { return (IN); }
{INHERITS} { return (INHERITS); }
{LET} { return (LET); }
{LOOP} { return (LOOP); }
{POOL} { return (POOL); }
{THEN} { return (THEN); }
{WHILE} { return (WHILE); }
{CASE} { return (CASE); }
{ESAC} { return (ESAC); }
{OF} { return (OF); }
{NEW} { return (NEW); }
{NOT} { return (NOT); }
{ISVOID} { return (ISVOID); }
{ASSIGN} { return (ASSIGN); }
{LE} { return (LE); }


 /*
  *  The multiple-character operators.
  */
 
[t][Rr][Uu][Ee] {
  cool_yylval.boolean = true;
  return (BOOL_CONST);
}
[f][Aa][Ll][Ss][Ee] {
  cool_yylval.boolean = false;
  return (BOOL_CONST);
}
[A-Z][A-Za-z0-9_]* {
  cool_yylval.symbol = idtable.add_string(yytext, yyleng);
  return (TYPEID);
}
[a-z][A-Za-z0-9_]* {
  cool_yylval.symbol = idtable.add_string(yytext, yyleng);
  return (OBJECTID);
}
[0-9]+ {
  cool_yylval.symbol = inttable.add_string(yytext, yyleng);
  return (INT_CONST);
}
[-.(){}:@,;+*/~<=] {
  return (int) yytext[0];
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\" {
  stringCaller = INITIAL;
  string_index = 0;
  BEGIN(STRING);
}
<STRING>\0 {
  null_in_string = true;
}
<STRING>\n {
  ++curr_lineno;
  cool_yylval.error_msg = "Unterminated string constant";
  BEGIN(stringCaller);
  return (ERROR);
}
<STRING>[^\\\n\"\0]*[\"\\] {
  int len = 0;
  while (len < yyleng - 1) {
    string_buf[string_index++] = yytext[len++];
  }
  if (yytext[yyleng-1] == '\"') {
    BEGIN(stringCaller);
    cool_yylval.symbol = stringtable.add_string(string_buf, string_index);
    if (string_index >= MAX_STR_CONST) {
      cool_yylval.error_msg = "String constant too long";
      return (ERROR);
    }
    else if (null_in_string) {
      cool_yylval.error_msg = "String contains null character";
      return (ERROR);
    }
    else return (STR_CONST);
  } 
  else BEGIN(STRING_ESCAPE);
}
<STRING><<EOF>> {
  cool_yylval.error_msg = "EOF in string constant";
  BEGIN(stringCaller);
  return (ERROR);
}
<STRING>[^\\\n\"\0] {
  string_buf[string_index++] = yytext[0];
}


<STRING_ESCAPE>[bntf\0\n] {
  switch (yytext[0]) {
    case '\n':
      ++curr_lineno;
    case 'n':
      string_buf[string_index++] = '\n';
      break;
    case 'b':
      string_buf[string_index++] = '\b';
      break;
    case 't':
      string_buf[string_index++] = '\t';
      break;
    case 'f':
      string_buf[string_index++] = '\f';
      break;
    case '\0':
      null_in_string = true;
      break;
    default:
      break;
  }
  BEGIN(STRING);
}
<STRING_ESCAPE><<EOF>> {
  cool_yylval.error_msg = "EOF in string";
  BEGIN(stringCaller);
  return (ERROR);
}
<STRING_ESCAPE>. {
  string_buf[string_index++] = yytext[0];
  BEGIN(STRING);
}


[ \t\f\r\v] { }
\n { ++curr_lineno; }

. {
  cool_yylval.error_msg = yytext;
  return (ERROR);
}

%%
