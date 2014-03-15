%include {
	#include <cassert>
	#include <cstring>

	#include "babb/utils/memory.h"

	#include "bcplus/parser/BCParser.h"
	#include "bcplus/parser/Token.h"
	#include "bcplus/parser/detail/lemon_parser.h"
	#include "bcplus/statements/Statement.h"
	#include "bcplus/statements/IncludeStatement.h"

	#define UNUSED void*

	using namespace babb::utils;
	using namespace bcplus::parser;
	using namespace bcplus::statements;
	using namespace bcplus::elements;
	using namespace bcplus::languages;


}

%name 					lemon_parser
%token_prefix 			T_
%extra_argument 		{ BCParser* parser						}


%nonassoc INTEGER.         // Any positive integer.
%nonassoc IDENTIFIER.      // Any identifier starting with a letter or
                           // with underscore(s) and a letter/number, & optionally
                           // followed by letters/numbers/underscores,
%nonassoc STRING_LITERAL.  // A quoted string containing arbitrary text.
%nonassoc ASP_GR.          // Raw ASP code from input using gringo style '#begin_asp' and '#end_asp.'.
%nonassoc ASP_CP.          // Raw ASP code from input using C+ style ':- begin asp.' and ':- end asp.'.
%nonassoc LUA_GR.          // Raw LUA code from input using gringo style '#begin_lua' and '#end_lua.'.
%nonassoc LUA_CP.          // Raw LUA code from input using C+ style ':- begin lua.' and ':- end lua.'
%nonassoc F2LP_GR.         // Raw F2LP code from input using gringo style '#begin_f2lp' and '#end_f2lp.'.
%nonassoc F2LP_CP.         // Raw F2LP code from input using C+ style ':- begin f2lp.' and ':- end f2lp.'.
%nonassoc COMMENT.         // Raw text from input that was commented out.

%nonassoc CONSTANTS.       // constants
%nonassoc INCLUDE.         // include
%nonassoc MACROS.          // macros
%nonassoc OBJECTS.         // objects
%nonassoc QUERY.           // query
%nonassoc SHOW.            // show
%nonassoc SORTS.           // sorts
%nonassoc VARIABLES.       // variables

%nonassoc ABACTION.        // abAction
%nonassoc ACTION.          // action
%nonassoc ADDITIVEACTION.  // additiveAction
%nonassoc ADDITIVEFLUENT.  // additiveFluent
%nonassoc AFTER.           // after
%nonassoc ALWAYS.          // always
%nonassoc ASSUMING.        // assuming             -- "if2" construct
%nonassoc ATTRIBUTE.       // attribute
%nonassoc BY.              // by
%nonassoc CAUSED.          // caused
%nonassoc CAUSES.          // causes
%nonassoc IMPOSSIBLE.      // impossible
%nonassoc CONSTRAINT.      // constraint
%nonassoc DECREMENTS.      // decrements
%nonassoc DEFAULT.         // default
%nonassoc EXTERNALACTION.      // dynamicAbnormality   -- aC+ construct
%nonassoc EXOGENOUS.       // exogenous
%nonassoc EXOGENOUSACTION. // exogenousAction
%nonassoc IF.              // if
%nonassoc IFCONS.
%nonassoc INCREMENTS.      // increments
%nonassoc INERTIAL.        // inertial
%nonassoc INERTIALFLUENT.  // inertialFluent
%nonassoc LABEL.           // label
%nonassoc MAY_CAUSE.       // may cause
%nonassoc MAXADDITIVE.     // maxAdditive
%nonassoc MAXAFVALUE.      // maxAFValue
%nonassoc MAXSTEP.         // maxstep
%nonassoc NEVER.           // never
%nonassoc NOCONCURRENCY.   // noconcurrency
%nonassoc STRONG_NOCONCURRENCY.// strong_noconcurrency
%nonassoc NONEXECUTABLE.   // onexecutable
%nonassoc OF.              // of
%nonassoc POSSIBLY_CAUSED. // possibly caused
%nonassoc RIGID.           // rigid
%nonassoc SDFLUENT.        // sdFluent
%nonassoc SIMPLEFLUENT.    // simpleFluent
%nonassoc EXTERNALFLUENT.          // staticAbnormality    -- aC+ construct
%nonassoc UNLESS.          // unless
%nonassoc WHERE.           // where

%nonassoc FALSE.           // false
%nonassoc NONE.            // none
%nonassoc TRUE.            // true

%nonassoc ABS.             // abs
%nonassoc AT.              // @
%nonassoc BRACKET_L.       // [
%nonassoc BRACKET_R.       // ]
%nonassoc COLON_DASH.      // :-
%nonassoc CBRACKET_L.      // {
%nonassoc CBRACKET_R.      // }
%nonassoc PAREN_L.         // (
%nonassoc PAREN_R.         // )
%nonassoc PERIOD.          // .
%nonassoc PIPE.            // |

/* operators */
%nonassoc TILDE.           // ~
%nonassoc DBL_COLON.       // ::
%nonassoc ARROW_LEQ ARROW_REQ ARROW_LDASH.      // <= => <-
%nonassoc COLON.           // :
%nonassoc EQ.              // =
%nonassoc DBL_EQ NEQ NOT_EQ LTHAN GTHAN LTHAN_EQ GTHAN_EQ.    // ==, \= or <> or neq, <, >, =<, >=
%nonassoc DBL_PERIOD.      // ..
%nonassoc BIG_CONJ BIG_DISJ.// /\ \/
%nonassoc POUND.           // #

%right    SEMICOLON.       // ;
%right    EQUIV.           // <->
%right    IMPL ARROW_RDASH.// ->>
%left     DBL_PLUS.        // ++
%left     AMP DBL_AMP COMMA.// &, &&, ,
%left     NOT.             // not
%left     DASH PLUS.       // -, +
%right    DBL_GTHAN.       // >>
%left     STAR INT_DIV MOD.// *, //, mod
%left     UMINUS.          // Pseudo-token, unary minus

// errors
%nonassoc 				EOF.
%nonassoc				ERR_IO ERR_UNKNOWN_SYMBOL.
%nonassoc				ERR_UNTERMINATED_STRING ERR_UNTERMINATED_ASP ERR_UNTERMINATED_LUA. 
%nonassoc				ERR_UNTERMINATED_F2LP ERR_UNTERMINATED_BLK_COMMENT.

%token_type 						{ Token const* 								}
%token_destructor 					{ delete $$;								}

%type 		start					{ UNUSED									}			// start symbol for file parsing
%destructor start					{ /* Intentionally left blank */			}
%type 		statement_list			{ UNUSED									}			// list of BC+ statements
%destructor statement_list			{ /* Intentionally left blank */			}

%type 		statement				{ Statement const*							}			// A single Statement
%destructor	statement				{ delete $$;								}
%type 		incl_stmt				{ Statement const*							}			// ':- include <file>;...;<file>.'
%destructor incl_stmt				{ delete $$;								}


%type 		incl_lst				{ IncludeStatement::FilenameList*			}			// File list: '<file>;...;<file>.'
%destructor incl_lst				{ delete $$;								}
%type 		incl_item				{ Token const*								}			// A single file name
%destructor incl_item				{ delete $$;								}
%type		incl_sep				{ UNUSED									}			// include list separators: ';' or ','
%destructor	incl_sep				{ /* Intentionally left blank */			}

%syntax_error 						{ parser->_parse_error("Syntax error.");	}



// ---------------------------------------------------------------------------------

start ::= statement_list EOF.

statement_list ::= .
statement_list ::= error.
statement_list ::= statement_list statement(stmt).
		{
			ref_ptr<const Statement> ptr = stmt;
			parser->_handle_stmt(stmt);
		}



/*************************************************************************************************/
/* Statements */
/*************************************************************************************************/

statement(stmt) ::= incl_stmt(incl).
		{
			stmt = incl;
		}

/*************************************************************************************************/
/* Include Statement */
/*************************************************************************************************/

incl_stmt(incl) ::= COLON_DASH(cd) INCLUDE incl_lst(lst) PERIOD(p).
		{
			ref_ptr<IncludeStatement::FilenameList> lst_ptr = lst;
			incl = new IncludeStatement(lst, cd->begin(), p->end() );
			delete cd;
			delete p;

			if (parser->lang()->support(Language::Feature::DECL_INCLUDE)) {
				parser->_feature_error(Language::Feature::DECL_INCLUDE);
				YYERROR;
			}
		}

incl_lst(lst) ::= incl_item(item).
		{
			lst = new IncludeStatement::FilenameList();
			lst->push_back(item->str());
			delete item;
		}

incl_lst(new_lst) ::= incl_lst(lst) incl_sep incl_item(item).
		{
			new_lst = lst;
			new_lst->push_back(item->str());
			delete item;
		}

incl_item(item) ::= STRING_LITERAL(str).
		{
			item = str;
		}
incl_item(item) ::= IDENTIFIER(id).
		{
			item = id;
		}
incl_item(item) ::= INTEGER(i).
		{
			item = i;
		}

incl_sep ::= SEMICOLON.
incl_sep ::= COMMA.
