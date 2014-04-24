%include {
			#include <cassert>
			#include <cstring>

			#include <boost/foreach.hpp>

			#include "babb/utils/memory.h"

			#include "bcplus/Location.h"
			#include "bcplus/parser/BCParser.h"
			#include "bcplus/parser/Token.h"
			#include "bcplus/parser/detail/lemon_parser.h"
			#include "bcplus/parser/detail/Number.h"
			#include "bcplus/parser/detail/NumberRange.h"
			#include "bcplus/statements/Statement.h"
			#include "bcplus/statements/declarations.h"
			#include "bcplus/statements/QueryStatement.h"
			#include "bcplus/statements/blocks.h"
			#include "bcplus/statements/laws.h"
			#include "bcplus/elements/Element.h"
			#include "bcplus/elements/terms.h"
			#include "bcplus/elements/formulas.h"
			#include "bcplus/symbols/Symbol.h"
			#include "bcplus/symbols/MacroSymbol.h"
			#include "bcplus/symbols/ConstantSymbol.h"
			#include "bcplus/symbols/AttributeSymbol.h"

			#define UNUSED void*

			using namespace bcplus;
			using namespace babb::utils;
			using namespace bcplus::parser;
			using namespace bcplus::statements;
			using namespace bcplus::elements;
			using namespace bcplus::languages;
			using namespace bcplus::symbols;
			using namespace bcplus::parser::detail;
			

			/// A list of terms
			typedef ReferencedVector<ref_ptr<const Term> >::type TermList;

			/// A list of sorts
			typedef ReferencedVector<ref_ptr<const SortSymbol> >::type SortList;


			typedef ReferencedVector<ref_ptr<const Token> >::type TokenList;

			/// Helper for deallocating things
			#define DEALLOC(x)	{ if (x) delete x; x = NULL; }

			/// A list of name sortlist pairs for declaring identifiers
			typedef std::pair<ref_ptr<const Token>, ref_ptr<SortList> > IdentifierDecl;
			typedef ReferencedVector<IdentifierDecl>::type IdentifierDeclList;

		}

%name 					lemon_parser
%token_prefix 			T_
%extra_argument 		{ BCParser* parser						}


%nonassoc INTEGER.         // Any positive integer.
%nonassoc IDENTIFIER.      // Any identifier starting with a letter or
                           // with underscore(s) and a letter/number, & optionally
                           // followed by letters/numbers/underscores,
%nonassoc POUND_IDENTIFIER. // #abc, #def, etc
%nonassoc POUND_INTEGER.	// #1, #2, etc
%nonassoc AT_IDENTIFIER.	// @abc, @def...
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
%nonassoc ALL.             // all
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

%nonassoc AT.              // @
%nonassoc BRACKET_L.       // [
%nonassoc BRACKET_R.       // ]
%nonassoc COLON_DASH.      // :-
%nonassoc CBRACKET_L.      // {
%nonassoc CBRACKET_R.      // }
%nonassoc PAREN_L.         // (
%nonassoc PAREN_R.         // )
%nonassoc PERIOD.          // .
%nonassoc MACRO_STRING.		// A macro definition string.



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
%nonassoc SEMICOLON.       // ;

%left     EQUIV.           // <->
%left     IMPL ARROW_RDASH.// ->> ->
%left     DBL_PLUS PIPE.   // ++
%right    DBL_GTHAN DBL_LTHAN.       // >> <<
%left     AMP COMMA DBL_AMP.// &, ,, &&
%left     NOT.             // not
%left     DASH PLUS.       // -, +
%left     STAR INT_DIV MOD.	// *, //, mod
%left     ABS.             // abs
%right    CARROT.		   // ^

%left     UMINUS.          // Pseudo-token, unary minus


%nonassoc PREC_4.
%nonassoc PREC_3.
%nonassoc PREC_2.
%nonassoc PREC_1.
%nonassoc PREC_0.



// errors
%nonassoc EOF.
%nonassoc ERR_IO ERR_UNKNOWN_SYMBOL.
%nonassoc ERR_UNTERMINATED_STRING ERR_UNTERMINATED_ASP ERR_UNTERMINATED_LUA. 
%nonassoc ERR_UNTERMINATED_F2LP ERR_UNTERMINATED_BLK_COMMENT ERR_SYNTAX ERR_PAREN_MISMATCH.

// place holders and pseudo-tokens
%nonassoc ARG.
%nonassoc NOOP.				// pseudo-token used to force statements to reduce without looking ahead.
%nonassoc CONSTANT_ID.		// identifier that has been resolved as a constant by the macro parser.
%nonassoc VARIABLE_ID.		// identifier that has been resolved as a variable by the macro parser.
%nonassoc OBJECT_ID.		// identifier that has been resolved as an object by the macro parser.

%fallback IDENTIFIER OBJECT_ID CONSTANT_ID VARIABLE_ID.


%token_type 								{ Token const* 								}
%token_destructor 							{ DEALLOC($$);								}
%syntax_error 								{ parser->_parse_error("Syntax error.");	}



/*************************************************************************************************/
/* Start */
/*************************************************************************************************/

%type 		start							{ UNUSED									}			// start symbol for file parsing
%destructor start							{ /* Intentionally left blank */			}
%type 		statement_lst					{ UNUSED									}			// list of BC+ statements
%destructor statement_lst					{ /* Intentionally left blank */			}
%type 		statement						{ Statement*								}			// A single Statement
%destructor	statement						{ DEALLOC($$);								}

start ::= statement_lst EOF.

statement_lst ::= .
statement_lst ::= statement_lst error.
statement_lst ::= statement_lst statement(stmt).
		{
			ref_ptr<Statement> ptr = stmt;
			stmt  = NULL;
			parser->_handle_stmt(ptr);
		}




/*************************************************************************************************/
/* Statements */
/*************************************************************************************************/

%type       stmt_macro_def					{ MacroDeclaration*							}			// macro definition statement
%destructor stmt_macro_def					{ DEALLOC($$);								}
%type       stmt_constant_def				{ ConstantDeclaration*						}			// constant definition statement ":- constants ..."}
%destructor stmt_constant_def				{ DEALLOC($$);								}
%type       stmt_object_def					{ ObjectDeclaration*						}			// object definition statement
%destructor stmt_object_def					{ DEALLOC($$);								}
%type       stmt_variable_def				{ VariableDeclaration*						}			// variable definition statement
%destructor stmt_variable_def               { DEALLOC($$);								}
%type       stmt_sort_def					{ SortDeclaration*							}			// sort definition statement
%destructor stmt_sort_def					{ DEALLOC($$);								}
%type       stmt_code_blk					{ Statement*								}			// code blocks to be passed through to another program.
%destructor stmt_code_blk					{ DEALLOC($$);								}
%type       stmt_law						{ Statement*								}			// program laws
%destructor stmt_law						{ DEALLOC($$);								}
%type       stmt_show						{ Statement*								}			// Statement to show one or more atoms
%destructor stmt_show						{ DEALLOC($$);								}
%type       stmt_hide						{ Statement*								}			// Statement to hide one or more atoms
%destructor stmt_hide						{ DEALLOC($$);								}
%type       stmt_noconcurrency				{ NCStatement*								}			// noconcurrency statement
%destructor stmt_noconcurrency				{ DEALLOC($$);								}
%type       stmt_strong_noconcurrency		{ StrongNCStatement*						}			// strong noconcurrency statement
%destructor stmt_strong_noconcurrency		{ DEALLOC($$);								}
%type       stmt_maxafvalue					{ Statement*								}			// maxAFValue definition
%destructor stmt_maxafvalue					{ DEALLOC($$);								}
%type       stmt_maxadditive				{ Statement*								}			// maxAdditve definition
%destructor stmt_maxadditive				{ DEALLOC($$);								}
%type       stmt_query						{ QueryStatement*							}			// query definitions
%destructor stmt_query						{ DEALLOC($$);								}


statement(stmt) ::= stmt_macro_def(m).				{ stmt = m; }
statement(stmt) ::= stmt_constant_def(c).			{ stmt = c; }
statement(stmt) ::= stmt_object_def(o).				{ stmt = o; }
statement(stmt) ::= stmt_variable_def(v).			{ stmt = v; }
statement(stmt) ::= stmt_sort_def(s).				{ stmt = s; }
statement(stmt) ::= stmt_code_blk(c).				{ stmt = c; }
statement(stmt) ::= stmt_law(l).					{ stmt = l; }
statement(stmt) ::= stmt_show(s).					{ stmt = s; }
statement(stmt) ::= stmt_hide(h).					{ stmt = h; }
statement(stmt) ::= stmt_noconcurrency(nc).			{ stmt = nc; }
statement(stmt) ::= stmt_strong_noconcurrency(nc). 	{ stmt = nc; }
statement(stmt) ::= stmt_maxafvalue(v).				{ stmt = v; }
statement(stmt) ::= stmt_maxadditive(v).			{ stmt = v; }
statement(stmt) ::= stmt_query(q).					{ stmt = q; }



/********************************************************************************************************************************/
/********************************************************************************************************************************/
/********************************************************************************************************************************/
/********************************************************************************************************************************/
/********************************************************************************************************************************/


/********************************************************************************************************************************/
/*************************************************************************************************/
/* base elements for formulas */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       base_elem						{ Term*										}			// constant, object, or variable.
%destructor base_elem						{ DEALLOC($$);								}
%type       base_elem_no_const				{ Term*										}			// object or variable.
%destructor base_elem_no_const				{ DEALLOC($$);								}
%type		constant						{ Constant*									}			// pre-declared constant
%destructor constant						{ DEALLOC($$);								}
%type		object							{ Object*									}			// pre-declared object
%destructor object							{ DEALLOC($$);								}
%type		object_nullary					{ Object*									}			// pre-declared nullary object
%destructor object_nullary					{ DEALLOC($$);								}
%type	    variable						{ Variable*									}			// pre-declared variable
%destructor variable						{ DEALLOC($$);								}
%type	    lua								{ LuaTerm*									}			// external lua call
%destructor lua								{ DEALLOC($$);								}
%type       undeclared						{ UNUSED									}			// undeclared element (to generate an error)
%destructor undeclared						{ /* Intentionally left blank */			}			
%type		term_lst						{ TermList*									}			// list of terms
%destructor term_lst						{ DEALLOC($$);								}
%type		term							{ Term*										}			// A (possibly compound) term
%destructor term							{ DEALLOC($$);								}
%type		constant_one_const				{ Constant*									}			// pre-declared constant whose arguments contain no other constants
%destructor constant_one_const				{ DEALLOC($$);								}
%type		term_no_const_lst				{ TermList*									}			// list of terms
%destructor term_no_const_lst				{ DEALLOC($$);								}
%type		term_no_const					{ Term*										}			// A term without constants
%destructor term_no_const					{ DEALLOC($$);								}

base_elem(elem) ::= constant(c).			{ elem = c; }
base_elem(elem) ::= base_elem_no_const(c).	{ elem = c; }

base_elem_no_const(elem) ::= object(o).		{ elem = o;	}
base_elem_no_const(elem) ::= variable(v).	{ elem = v; }
base_elem_no_const(elem) ::= lua(l).		{ elem = l; }
//base_elem_no_const(elem) ::= undeclared.	{ /* This should never be called*/ elem = NULL; }
%include {
	#define BASE_ELEM_DEF(elem, id, lparen, args, rparen, symtype, class, symclass)											\
		elem = NULL;																										\
		ref_ptr<const Token> id_ptr = id;																					\
		ref_ptr<const Token> lparen_ptr = lparen;																			\
		ref_ptr<TermList> args_ptr = args;																					\
		ref_ptr<const Token> rparen_ptr = rparen;																			\
		size_t arity = (args_ptr ? args_ptr->size() : 0);																	\
																															\
		Symbol const* sym = parser->symtab()->resolve(symtype, *id_ptr->str(), arity);										\
		if (sym && sym->type() == symtype) {																				\
			elem = new class((symclass*)sym, args, id_ptr->beginLoc(), (arity ? rparen_ptr->endLoc() : id_ptr->endLoc()));	\
		} else {																											\
			/* The preprocessor indicated this was a constant and it's not... oops. */										\
			parser->_parse_error(std::string("INTERNAL ERROR: Could not locate symbol table entry for ")					\
				+ Symbol::Type::cstr(symtype) + " \"" + Symbol::genName(*id_ptr->str(), arity)		 						\
				+ "\".", &id_ptr->beginLoc());																				\
			YYERROR;																										\
		}	
	
	#define BASE_ELEM_BARE_DEF(elem, id, symtype, class, symclass)															\
		elem = NULL;																										\
		ref_ptr<const Token> id_ptr = id;																					\
																															\
		Symbol const* sym = parser->symtab()->resolve(symtype, *id_ptr->str());												\
		if (sym && sym->type() == symtype) {																				\
			elem = new class((symclass*)sym, id_ptr->beginLoc(), id_ptr->endLoc());											\
		} else {																											\
			/* The preprocessor indicated this was a constant and it's not... oops. */										\
			parser->_parse_error(std::string("INTERNAL ERROR: Could not locate symbol table entry for ")					\
				+ Symbol::Type::cstr(symtype) + " \"" + Symbol::genName(*id_ptr->str(), 0)			 						\
				+ "\".", &id_ptr->beginLoc());																				\
			YYERROR;																										\
		}	
		
	#define BASE_LUA_ELEM(elem, id, lparen, args, rparen)																	\
		ref_ptr<const Token> id_ptr = id;																					\
		ref_ptr<const Token> lparen_ptr = lparen;																			\
		ref_ptr<TermList> args_ptr = args;																					\
		ref_ptr<const Token> rparen_ptr = rparen;																			\
		elem = new LuaTerm(id_ptr->str(), args, id_ptr->beginLoc(), (args ? rparen_ptr->endLoc() : id_ptr->endLoc()));

	#define UNDECLARED(elem, id, args)																						\
		elem = NULL;																										\
		ref_ptr<const Token> id_ptr = id;																					\
		ref_ptr<TermList> args_ptr = args;																					\
		parser->_parse_error("Encountered undeclared identifier \"" 														\
			+ Symbol::genName(*id->str(), (args_ptr ? args_ptr->size() : 0)) + "\".", &id_ptr->beginLoc());					\
		YYERROR;		

}

constant(c) ::= CONSTANT_ID(id) PAREN_L(pl) term_lst(args) PAREN_R(pr).			{ BASE_ELEM_DEF(c, id, pl, args, pr, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
constant(c) ::= CONSTANT_ID(id).												{ BASE_ELEM_DEF(c, id, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
object(o)   ::= OBJECT_ID(id) PAREN_L(pl) term_lst(args) PAREN_R(pr).			{ BASE_ELEM_DEF(o, id, pl, args, pr, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
object(new_o)	::= object_nullary(o).											{ new_o = o; }
object_nullary(o)   ::= OBJECT_ID(id).											{ BASE_ELEM_DEF(o, id, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }

variable(o) ::= VARIABLE_ID(id).												
	{ 
		o = NULL;
		ref_ptr<const Token> id_ptr = id;
		
		if (!parser->lang()->support(Language::Feature::VARIABLE)) {
			parser->_feature_error(Language::Feature::VARIABLE, &id->beginLoc());
			YYERROR;
		} else {
			BASE_ELEM_BARE_DEF(o, id, Symbol::Type::VARIABLE, Variable, VariableSymbol); 
		}
	}
lua(l) ::= AT_IDENTIFIER(id) PAREN_L(pl) term_lst(args) PAREN_R(pr).			{ BASE_LUA_ELEM(l, id, pl, args, pr); }
lua(l) ::= AT_IDENTIFIER(id).													{ BASE_LUA_ELEM(l, id, NULL, NULL, NULL); }
//undeclared(u) ::= IDENTIFIER(id) PAREN_L term_lst(args) PAREN_R.				{ UNDECLARED(u, id, args); }
//undeclared(u) ::= IDENTIFIER(id).												{ UNDECLARED(u, id, NULL); }

term_lst(lst) ::= term(t).
		{
			lst = new TermList();
			lst->push_back(t);
		}

term_lst(new_lst) ::= term_lst(lst) COMMA term(t).
		{
			new_lst = lst;
			lst->push_back(t);
		}

constant_one_const(c) ::= CONSTANT_ID(id) PAREN_L(pl) term_no_const_lst(args) PAREN_R(pr).			{ BASE_ELEM_DEF(c, id, pl, args, pr, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
constant_one_const(c) ::= CONSTANT_ID(id).															{ BASE_ELEM_DEF(c, id, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }


term_no_const_lst(lst) ::= term_no_const(t).
		{
			lst = new TermList();
			lst->push_back(t);
		}

term_no_const_lst(new_lst) ::= term_no_const_lst(lst) COMMA term_no_const(t).
		{
			new_lst = lst;
			lst->push_back(t);
		}



/********************************************************************************************************************************/
/*************************************************************************************************/
/* Terms */
/*************************************************************************************************/
/********************************************************************************************************************************/
%type		term_strong							{ Term*										}			// A stronger definition of term for the LHS of operators in a formula
																										// in order to disambiguate the grammar.
%destructor term_strong							{ DEALLOC($$);								}
%type		term_strong_candidate				{ Term*										}			// A term we think may be a strong term, depending on what's following it.
%destructor term_strong_candidate				{ DEALLOC($$);								}
%type		term_no_const_strong				{ Term*										}			// A stronger definition of term for the LHS of operators in a formula
																										// in order to disambiguate the grammar.
%destructor term_no_const_strong				{ DEALLOC($$);								}

%include {
	#define BASIC_TERM(term, id)																							\
		term = NULL;																										\
		ref_ptr<const Token> id_ptr = id;																					\
		Symbol const* sym = parser->symtab()->resolveOrCreate(new ObjectSymbol(id->str()));									\
		if (!sym) {																											\
			parser->_parse_error("An error occurred creating symbol \"" + *(id->str()) + "/0\".", &id->beginLoc());			\
			YYERROR;																										\
		} else term = new Object((ObjectSymbol const*)sym, NULL, id->beginLoc(), id->endLoc());

	#define TERM_PARENS(term, lparen, sub, rparen)																			\
		ref_ptr<const Token> lparen_ptr = lparen;																			\
		ref_ptr<const Token> rparen_ptr = rparen;																			\
		term = sub;																											\
		term->parens(true);																									\
		term->beginLoc(lparen->beginLoc());																					\
		term->endLoc(rparen->endLoc());


	#define UNARY_ARITH(term, op, sub, operator)																			\
		term = NULL;																										\
		ref_ptr<const Token> op_ptr = op;																					\
		ref_ptr<Term> sub_ptr = sub;																						\
																															\
		if (sub->domainType() != DomainType::INTEGRAL && sub->domainType() != DomainType::UNKNOWN) {						\
			parser->_parse_error("Malformed arithmetic operation. Operand is not numeric.", &sub->beginLoc());				\
			YYERROR;																										\
		} else term = new UnaryTerm(operator, sub, sub->beginLoc(), sub->endLoc());

	#define BINARY_ARITH(term, lhs, op, rhs, operator)																		\
		term = NULL;																										\
		ref_ptr<Term> lhs_ptr = lhs;																						\
		ref_ptr<Term> rhs_ptr = rhs;																						\
		ref_ptr<const Token> op_ptr = op;																					\
																															\
		bool good = true;																									\
		if (lhs->domainType() != DomainType::INTEGRAL && lhs->domainType() != DomainType::UNKNOWN) {						\
			good = false;																									\
			parser->_parse_error("Malformed arithmetic operation. Left Operand is not numeric.", &op->beginLoc());			\
			YYERROR;																										\
		}																													\
		if (rhs->domainType() != DomainType::INTEGRAL && rhs->domainType() != DomainType::UNKNOWN) {						\
			good = false;																									\
			parser->_parse_error("Malformed arithmetic operation. Right Operand is not numeric.", &rhs->beginLoc());		\
			YYERROR;																										\
		}																													\
		if (good) term = new BinaryTerm(operator, lhs, rhs, lhs->beginLoc(), rhs->endLoc());

	#define NULLARY_TERM(term, op, feature, operator)																		\
		term = NULL;																										\
		ref_ptr<const Token> op_ptr = op;																					\
																															\
		if (!parser->lang()->support(feature)) {																			\
			parser->_feature_error(feature, &op->beginLoc());																\
		} else {																											\
			term = new NullaryTerm(operator, op->beginLoc(), op->endLoc());													\
		}

}


term(t) ::= base_elem(e).										{ t = e; }
term(t) ::= INTEGER(i).											{ BASIC_TERM(t, i);	}
term(t) ::= STRING_LITERAL(s).									{ BASIC_TERM(t, s); }
term(t) ::= PAREN_L(pl) term(sub) PAREN_R(pr).					{ TERM_PARENS(t, pl, sub, pr); }
term(t) ::= TRUE(e).											{ BASIC_TERM(t, e); }
term(t) ::= FALSE(e).											{ BASIC_TERM(t, e); }
term(t) ::= MAXSTEP(e).											{ NULLARY_TERM(t, e, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
term(t) ::= MAXADDITIVE(e).										{ NULLARY_TERM(t, e, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
term(t) ::= MAXAFVALUE(e).										{ NULLARY_TERM(t, e, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }

// unary terms

term(t_new) ::= DASH(d) term(t). [UMINUS]						{ UNARY_ARITH(t_new, d, t, UnaryTerm::Operator::NEGATIVE); }
term(t_new) ::= ABS(a) term(t).									{ UNARY_ARITH(t_new, a, t, UnaryTerm::Operator::ABS); }

// binary terms

term(t) ::= term(l) DASH(o) term(r).							{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MINUS); }
term(t) ::= term(l) PLUS(o) term(r).							{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::PLUS); }
term(t) ::= term(l) STAR(o) term(r).							{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::TIMES); }
term(t) ::= term(l) INT_DIV(o) term(r).							{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::DIVIDE); }
term(t) ::= term(l) MOD(o) term(r).								{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MOD); }

// strong terms

term_strong(t) ::= base_elem_no_const(e).						{ t = e; }
term_strong(t) ::= INTEGER(i).									{ BASIC_TERM(t, i);	}
term_strong(t) ::= STRING_LITERAL(s).							{ BASIC_TERM(t, s); }
term_strong(t) ::= PAREN_L(pl) term_strong(sub) PAREN_R(pr).	{ TERM_PARENS(t, pl, sub, pr); }
term_strong(t) ::= MAXSTEP(e).									{ NULLARY_TERM(t, e, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
term_strong(t) ::= MAXADDITIVE(e).								{ NULLARY_TERM(t, e, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
term_strong(t) ::= MAXAFVALUE(e).								{ NULLARY_TERM(t, e, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }

// unary strong terms

term_strong(t_new) ::= DASH(d) term_strong(t). [UMINUS]			{ UNARY_ARITH(t_new, d, t, UnaryTerm::Operator::NEGATIVE); }
term_strong(t_new) ::= ABS(a) term(t).							{ UNARY_ARITH(t_new, a, t, UnaryTerm::Operator::ABS); }

// unary terms we think may be strong terms

term_strong_candidate(t_new) ::= DASH(d) constant(t). [UMINUS]	{ UNARY_ARITH(t_new, d, t, UnaryTerm::Operator::NEGATIVE); }

// binary strong terms

term_strong(t) ::= term_strong_candidate(l) DASH(o) term(r).	{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MINUS); }
term_strong(t) ::= term_strong_candidate(l) PLUS(o) term(r).	{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::PLUS); }
term_strong(t) ::= term_strong_candidate(l) STAR(o) term(r).	{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::TIMES); }
term_strong(t) ::= term_strong_candidate(l) INT_DIV(o) term(r).	{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::DIVIDE); }
term_strong(t) ::= term_strong_candidate(l) MOD(o) term(r).		{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MOD); }
term_strong(t) ::= constant(l) DASH(o) term(r).					{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MINUS); }
term_strong(t) ::= constant(l) PLUS(o) term(r).					{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::PLUS); }
term_strong(t) ::= constant(l) STAR(o) term(r).					{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::TIMES); }
term_strong(t) ::= constant(l) INT_DIV(o) term(r).				{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::DIVIDE); }
term_strong(t) ::= constant(l) MOD(o) term(r).					{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MOD); }
term_strong(t) ::= term_strong(l) DASH(o) term(r).				{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MINUS); }
term_strong(t) ::= term_strong(l) PLUS(o) term(r).				{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::PLUS); }
term_strong(t) ::= term_strong(l) STAR(o) term(r).				{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::TIMES); }
term_strong(t) ::= term_strong(l) INT_DIV(o) term(r).			{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::DIVIDE); }
term_strong(t) ::= term_strong(l) MOD(o) term(r).				{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MOD); }


/*************************************************************************************************/
/* terms without constants */
/*************************************************************************************************/

// strong terms

term_no_const_strong(t) ::= base_elem_no_const(e).									{ t = e; }
term_no_const_strong(t) ::= INTEGER(i).												{ BASIC_TERM(t, i);	}
term_no_const_strong(t) ::= STRING_LITERAL(s).										{ BASIC_TERM(t, s); }
term_no_const_strong(t) ::= PAREN_L(pl) term_no_const_strong(sub) PAREN_R(pr).		{ TERM_PARENS(t, pl, sub, pr); }
term_no_const_strong(t) ::= MAXSTEP(e).												{ NULLARY_TERM(t, e, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
term_no_const_strong(t) ::= MAXADDITIVE(e).											{ NULLARY_TERM(t, e, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
term_no_const_strong(t) ::= MAXAFVALUE(e).											{ NULLARY_TERM(t, e, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
term_no_const_strong(t) ::= constant(c).
	{
		// error handling for constants so they don't default to undeclared identifiers
		t = NULL;
		ref_ptr<const Referenced> c_ptr = c;
		parser->_parse_error("Encountered unexpected constant symbol.", &c->beginLoc());
		YYERROR;
	}

// unary term_no_const_strongs

term_no_const_strong(t_new) ::= DASH(d) term_no_const_strong(t). [UMINUS]			{ UNARY_ARITH(t_new, d, t, UnaryTerm::Operator::NEGATIVE); }
term_no_const_strong(t_new) ::= ABS(a) term_no_const(t).							{ UNARY_ARITH(t_new, a, t, UnaryTerm::Operator::ABS); }

// binary term_no_const_strongs

term_no_const_strong(t) ::= term_no_const_strong(l) DASH(o) term_no_const(r).		{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MINUS); }
term_no_const_strong(t) ::= term_no_const_strong(l) PLUS(o) term_no_const(r).		{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::PLUS); }
term_no_const_strong(t) ::= term_no_const_strong(l) STAR(o) term_no_const(r).		{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::TIMES); }
term_no_const_strong(t) ::= term_no_const_strong(l) INT_DIV(o) term_no_const(r).	{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::DIVIDE); }
term_no_const_strong(t) ::= term_no_const_strong(l) MOD(o) term_no_const(r).		{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MOD); }

// Terms with no constants

term_no_const(t) ::= base_elem_no_const(e).						{ t = e; }
term_no_const(t) ::= INTEGER(i).								{ BASIC_TERM(t, i);	}
term_no_const(t) ::= STRING_LITERAL(s).							{ BASIC_TERM(t, s); }
term_no_const(t) ::= PAREN_L(pl) term_no_const(sub) PAREN_R(pr).{ TERM_PARENS(t, pl, sub, pr); }
term_no_const(t) ::= TRUE(e).									{ BASIC_TERM(t, e); }
term_no_const(t) ::= FALSE(e).									{ BASIC_TERM(t, e); }
term_no_const(t) ::= constant(c).
	{
		// error handline for constants so they don't default to undeclared identifiers
		t = NULL;
		ref_ptr<const Referenced> c_ptr = c;
		parser->_parse_error("Encountered unexpected constant symbol.", &c->beginLoc());
		YYERROR;
	}

// unary terms with no constants

term_no_const(t_new) ::= DASH(d) term_no_const(t). [UMINUS]		{ UNARY_ARITH(t_new, d, t, UnaryTerm::Operator::NEGATIVE); }
term_no_const(t_new) ::= ABS(a) term_no_const(t).				{ UNARY_ARITH(t_new, a, t, UnaryTerm::Operator::ABS); }

// binary terms with no constants

term_no_const(t) ::= term_no_const(l) DASH(o) term_no_const(r).	{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MINUS); }
term_no_const(t) ::= term_no_const(l) PLUS(o) term_no_const(r).	{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::PLUS); }
term_no_const(t) ::= term_no_const(l) STAR(o) term_no_const(r).	{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::TIMES); }
term_no_const(t) ::= term_no_const(l) INT_DIV(o) term_no_const(r).{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::DIVIDE); }
term_no_const(t) ::= term_no_const(l) MOD(o) term_no_const(r).	{ BINARY_ARITH(t, l, o, r, BinaryTerm::Operator::MOD); }

/*************************************************************************************************/
/* Numeric Ranges */
/*************************************************************************************************/
%type		num_range						{ NumberRange*								}		// A range between two numbers
%destructor num_range						{ DEALLOC($$);								}
%type		term_numeric					{ Number*									}		// A term which contains only integers
%destructor term_numeric					{ DEALLOC($$);								}


num_range(nr) ::= term_numeric(l) DBL_PERIOD(s) term_numeric(r). {
	ref_ptr<const Referenced> l_ptr = l, r_ptr = r, s_ptr = s;

	nr = new NumberRange(l->val(), r->val(), l->beginLoc(), r->endLoc());

}


term_numeric(t) ::= INTEGER(i). {
	ref_ptr<const Referenced> i_ptr = i;

	t = 0;
	try {
		t = new Number(boost::lexical_cast<int>(*i->str()), i->beginLoc());

	} catch (boost::bad_lexical_cast const& e) {
		parser->_parse_error("INTERNAL ERROR: Failed to parse integer \"" + *i->str() + "\".", &i->beginLoc());
		YYERROR;
	}
}

term_numeric(t) ::= PAREN_L(pl) term_numeric(sub) PAREN_R(pr). { 
	ref_ptr<const Referenced> pl_ptr = pl, pr_ptr = pr;
	t = sub;  
	t->beginLoc(pl->beginLoc());
	t->endLoc(pr->endLoc());
}

%include {
	#define NUM_UOP(t_new, t, val)																				\
		ref_ptr<const Referenced> t_ptr = t;																			\
		t_new = new Number(val, t->beginLoc(), t->endLoc());

	
	#define NUM_BOP(t_new, l, r, val)																			\
		ref_ptr<const Referenced> l_ptr = l, r_ptr = r;																\
		t_new = new Number(val, l->beginLoc(), r->endLoc());

}


term_numeric(t_new) ::= DASH term_numeric(t). [UMINUS]					{ NUM_UOP(t_new, t, -1 * t->val()); }
term_numeric(t_new) ::= ABS  term_numeric(t).							{ NUM_UOP(t_new, t, t->val() < 0 ? - t->val() : t->val()); }

term_numeric(t) ::= term_numeric(l) DASH term_numeric(r).				{ NUM_BOP(t, l, r, l->val() - r->val()); }
term_numeric(t) ::= term_numeric(l) PLUS term_numeric(r).				{ NUM_BOP(t, l, r, l->val() + r->val()); }
term_numeric(t) ::= term_numeric(l) STAR term_numeric(r).				{ NUM_BOP(t, l, r, l->val() * r->val()); }
term_numeric(t) ::= term_numeric(l) INT_DIV term_numeric(r).			{ NUM_BOP(t, l, r, l->val() / r->val()); }
term_numeric(t) ::= term_numeric(l) MOD term_numeric(r).				{ NUM_BOP(t, l, r, l->val() % r->val()); }


/********************************************************************************************************************************/
/*************************************************************************************************/
/* Nested Body Formulas */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       formula							{ Formula*									}		// An arbitrary formula (either a conjunctive list or nested formula)
%destructor formula							{ DEALLOC($$);								}
%type       formula_base					{ Formula*									}		// A single formula element
%destructor formula_base					{ DEALLOC($$);								}
%type       comparison						{ Formula*									}		// comparison between 2 terms
%destructor comparison						{ DEALLOC($$);								}
%type		atomic_formula					{ AtomicFormula*							}		// c=v
%destructor atomic_formula					{ DEALLOC($$);								}
%type       formula_quant					{ QuantifierFormula*						}		// a quantifier formula
%destructor formula_quant					{ DEALLOC($$);								}
%type       formula_card					{ Formula*									}		// cardinality formula
%destructor formula_card					{ DEALLOC($$);								}

%include {
	#define NESTED_BOP(new_f, lhs, op, rhs, operator)															\
		new_f = NULL;																							\
		ref_ptr<Formula> lhs_ptr = lhs;																			\
		ref_ptr<const Token> op_ptr = op;																		\
		ref_ptr<Formula> rhs_ptr = rhs;																			\
																												\
		if (!parser->lang()->support(Language::Feature::FORMULA_NESTED)) {										\
			parser->_feature_error(Language::Feature::FORMULA_NESTED, &op->beginLoc());							\
			YYERROR;																							\
		}																										\
		new_f = new BinaryFormula(operator, lhs, rhs, lhs->beginLoc(), rhs->endLoc());  						\

	#define NESTED_UOP(new_f, op, rhs, operator, feature)														\
		new_f = NULL;																							\
		ref_ptr<const Token> op_ptr = op;																		\
		ref_ptr<Formula> rhs_ptr = rhs;																			\
																												\
		/* Check to ensure that the operator is supported */													\
		if (!parser->lang()->support(feature)) {																\
			parser->_feature_error(feature);																	\
			YYERROR;																							\
		} else if (!parser->lang()->support(Language::Feature::FORMULA_NESTED)) {								\
			/* Check to ensure that this isn't nested */														\
			if (rhs->subType() == Formula::Type::BINARY || rhs->subType() == Formula::Type::UNARY) {			\
				parser->_feature_error(Language::Feature::FORMULA_NESTED, &rhs->beginLoc());					\
				YYERROR;																						\
			}																									\
		}																										\
		else new_f = new UnaryFormula (UnaryFormula:: Operator::NOT, rhs, op->beginLoc(), rhs->endLoc());

}


formula(new_f) ::= formula_base(f).													{ new_f = f;				}
formula(new_f) ::= PAREN_L formula(f) PAREN_R.										{ new_f = f; new_f->parens(true); 	}
formula(new_f) ::= NOT(op) formula(f).												{ NESTED_UOP(new_f, op, f, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
formula(new_f) ::= DASH(op) formula(f).												{ NESTED_UOP(new_f, op, f, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
formula(new_f) ::= formula(lhs) AMP formula(rhs).									{ new_f = new BinaryFormula(BinaryFormula::Operator::AND, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
formula(new_f) ::= formula(lhs) DBL_PLUS(op) formula(rhs).							{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::OR); }
formula(new_f) ::= formula(lhs) PIPE(op) formula(rhs).								{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::OR); }
formula(new_f) ::= formula(lhs) EQUIV(op) formula(rhs).								{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::EQUIV); }
formula(new_f) ::= formula(lhs) IMPL(op) formula(rhs).								{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::IMPL); }
formula(new_f) ::= formula(lhs) ARROW_RDASH(op) formula(rhs).						{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::IMPL); }

formula_base(f) ::= comparison(c).													{ f = c; }
formula_base(f) ::= atomic_formula(l).												{ f = l; }
formula_base(f) ::= formula_quant(q).												{ f = q; }
formula_base(f) ::= formula_card(c).
	{ 
		f = c;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &c->beginLoc());
			YYERROR;
		}
	}
formula_base(f) ::= TRUE(e).														{ f = new NullaryFormula(NullaryFormula::Operator::TRUE, e->beginLoc(), e->endLoc()); }
formula_base(f) ::= FALSE(e).														{ f = new NullaryFormula(NullaryFormula::Operator::FALSE, e->beginLoc(), e->endLoc()); }

comparison(c) ::= term_strong(lhs) EQ term(rhs).									{ c = new ComparisonFormula(ComparisonFormula::Operator::EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong(lhs) DBL_EQ term(rhs).								{ c = new ComparisonFormula(ComparisonFormula::Operator::EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong(lhs) NEQ term(rhs).									{ c = new ComparisonFormula(ComparisonFormula::Operator::NEQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong(lhs) LTHAN term(rhs).									{ c = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong(lhs) GTHAN term(rhs).									{ c = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong(lhs) LTHAN_EQ term(rhs).								{ c = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong(lhs) GTHAN_EQ term(rhs).								{ c = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong_candidate(lhs) EQ term(rhs).							{ c = new ComparisonFormula(ComparisonFormula::Operator::EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong_candidate(lhs) DBL_EQ term(rhs).						{ c = new ComparisonFormula(ComparisonFormula::Operator::EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong_candidate(lhs) NEQ term(rhs).							{ c = new ComparisonFormula(ComparisonFormula::Operator::NEQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong_candidate(lhs) LTHAN term(rhs).						{ c = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong_candidate(lhs) GTHAN term(rhs).						{ c = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong_candidate(lhs) LTHAN_EQ term(rhs).					{ c = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= term_strong_candidate(lhs) GTHAN_EQ term(rhs).					{ c = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= constant(lhs) DBL_EQ term(rhs).									{ c = new ComparisonFormula(ComparisonFormula::Operator::EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= constant(lhs) NEQ term(rhs).										{ c = new ComparisonFormula(ComparisonFormula::Operator::NEQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= constant(lhs) LTHAN term(rhs).									{ c = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= constant(lhs) GTHAN term(rhs).									{ c = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= constant(lhs) LTHAN_EQ term(rhs).									{ c = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison(c) ::= constant(lhs) GTHAN_EQ term(rhs).									{ c = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }

%include {

	#define ATOMIC_FORMULA(af, constant, valuestring) 													\
		af = NULL;																						\
		ref_ptr<Constant> c_ptr = constant;																\
																										\
		/* If this is a boolean constant we can interpret it as a shortcut for c=true */				\
		if (constant->domainType() != DomainType::BOOLEAN) {											\
			parser->_parse_error("\"" + *constant->symbol()->name() 									\
				+ "\" is not a boolean valued constant and therefore "									\
				"cannot be used in a bare atomic formula.", &constant->beginLoc());						\
			YYERROR;																					\
		} else {																						\
			ref_ptr<ObjectSymbol> t = (ObjectSymbol*)parser->symtab()->resolveOrCreate(					\
				new ObjectSymbol(new ReferencedString(valuestring)));									\
			if (!t) {																					\
				parser->_parse_error("INTERNAL ERROR: Could not resolve \"" 							\
					+ Symbol::genName(valuestring,0) + "\".", &constant->beginLoc());					\
				YYERROR;																				\
			} else af = new AtomicFormula(constant,														\
				new Object(t, NULL, constant->beginLoc(), constant->endLoc()), 							\
					constant->beginLoc(), constant->endLoc());											\
		}
	


}

atomic_formula(af) ::= constant(c).										{ ATOMIC_FORMULA(af, c, "true"); }
atomic_formula(af) ::= TILDE constant(c).								{ ATOMIC_FORMULA(af, c, "false"); }
atomic_formula(af) ::= constant(c) EQ term(t). 							{ af = new AtomicFormula(c, t, c->beginLoc(), t->endLoc());	}

/*************************************************************************************************/
/* Formulas without constants */
/*************************************************************************************************/

%type       formula_no_const				{ Formula*									}		// An arbitrary formula with no constants (either a conjunctive list or nested formula_no_const)
%destructor formula_no_const				{ DEALLOC($$);								}
%type       formula_no_const_base			{ Formula*									}		// A single formula with no constants
%destructor formula_no_const_base			{ DEALLOC($$);								}
%type       comparison_no_const				{ Formula*									}		// comparison between 2 terms
%destructor comparison_no_const				{ DEALLOC($$);								}
%type       atomic_formula_one_const		{ AtomicFormula*							}		// Atomic formulas which contain only the one required constant
%destructor atomic_formula_one_const		{ DEALLOC($$);								}

formula_no_const(new_f) ::= formula_no_const_base(f).											{ new_f = f;				}
formula_no_const(new_f) ::= PAREN_L formula_no_const(f) PAREN_R.								{ new_f = f; new_f->parens(true); 	}
formula_no_const(new_f) ::= NOT(op) formula_no_const(f).										{ NESTED_UOP(new_f, op, f, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
formula_no_const(new_f) ::= DASH(op) formula_no_const(f).										{ NESTED_UOP(new_f, op, f, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
formula_no_const(new_f) ::= formula_no_const(lhs) AMP formula_no_const(rhs).					{ new_f = new BinaryFormula(BinaryFormula::Operator::AND, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
formula_no_const(new_f) ::= formula_no_const(lhs) DBL_PLUS(op) formula_no_const(rhs).			{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::OR); }
formula_no_const(new_f) ::= formula_no_const(lhs) PIPE(op) formula_no_const(rhs).				{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::OR); }
formula_no_const(new_f) ::= formula_no_const(lhs) EQUIV(op) formula_no_const(rhs).				{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::EQUIV); }
formula_no_const(new_f) ::= formula_no_const(lhs) IMPL(op) formula_no_const(rhs).				{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::IMPL); }
formula_no_const(new_f) ::= formula_no_const(lhs) ARROW_RDASH(op) formula_no_const(rhs).		{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::IMPL); }

formula_no_const_base(f) ::= comparison_no_const(c).											{ f = c; }
formula_no_const_base(f) ::= TRUE(e).															{ f = new NullaryFormula(NullaryFormula::Operator::TRUE, e->beginLoc(), e->endLoc()); }
formula_no_const_base(f) ::= FALSE(e).															{ f = new NullaryFormula(NullaryFormula::Operator::FALSE, e->beginLoc(), e->endLoc()); }

comparison_no_const(c) ::= term_no_const_strong(lhs) EQ term_no_const(rhs).						{ c = new ComparisonFormula(ComparisonFormula::Operator::EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison_no_const(c) ::= term_no_const_strong(lhs) DBL_EQ term_no_const(rhs).					{ c = new ComparisonFormula(ComparisonFormula::Operator::EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison_no_const(c) ::= term_no_const_strong(lhs) NEQ term_no_const(rhs).					{ c = new ComparisonFormula(ComparisonFormula::Operator::NEQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison_no_const(c) ::= term_no_const_strong(lhs) LTHAN term_no_const(rhs).					{ c = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison_no_const(c) ::= term_no_const_strong(lhs) GTHAN term_no_const(rhs).					{ c = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison_no_const(c) ::= term_no_const_strong(lhs) LTHAN_EQ term_no_const(rhs).				{ c = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
comparison_no_const(c) ::= term_no_const_strong(lhs) GTHAN_EQ term_no_const(rhs).				{ c = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }

atomic_formula_one_const(af) ::= constant_one_const(c).											{ ATOMIC_FORMULA(af, c, "true"); }
atomic_formula_one_const(af) ::= TILDE constant_one_const(c).									{ ATOMIC_FORMULA(af, c, "false"); }
atomic_formula_one_const(af) ::= constant_one_const(c) EQ term_no_const(t). 					{ af = new AtomicFormula(c, t, c->beginLoc(), t->endLoc());	}

/*************************************************************************************************/
/* Formulas with temporal bindings */
/*************************************************************************************************/

%include {
	#define BINDING(new_f, lhs, op, rhs)																		\
		new_f = NULL;																							\
		ref_ptr<Term> lhs_ptr = lhs;																			\
		ref_ptr<const Token> op_ptr = op;																		\
		ref_ptr<Formula> rhs_ptr = rhs;																			\
																												\
		if (!parser->lang()->support(Language::Feature::QUERY_BIND_STEP)) {										\
			parser->_feature_error(Language::Feature::QUERY_BIND_STEP, &op->beginLoc());						\
			YYERROR;																							\
		} else {																								\
			new_f = new BindingFormula(lhs, rhs, lhs->beginLoc(), rhs->endLoc());  								\
		}

}

%type       formula_temporal				{ Formula*									}
%destructor formula_temporal				{ DEALLOC($$);								}

formula_temporal(new_f) ::= formula_base(f).															{ new_f = f;				}
formula_temporal(new_f) ::= PAREN_L formula_temporal(f) PAREN_R.										{ new_f = f; new_f->parens(true); 	}
formula_temporal(new_f) ::= NOT(op) formula_temporal(f).												{ NESTED_UOP(new_f, op, f, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
formula_temporal(new_f) ::= DASH(op) formula_temporal(f).												{ NESTED_UOP(new_f, op, f, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
formula_temporal(new_f) ::= formula_temporal(lhs) AMP formula_temporal(rhs).							{ new_f = new BinaryFormula(BinaryFormula::Operator::AND, lhs, rhs, lhs->beginLoc(), rhs->endLoc()); }
formula_temporal(new_f) ::= formula_temporal(lhs) DBL_PLUS(op) formula_temporal(rhs).					{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::OR); }
formula_temporal(new_f) ::= formula_temporal(lhs) PIPE(op) formula_temporal(rhs).						{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::OR); }
formula_temporal(new_f) ::= formula_temporal(lhs) EQUIV(op) formula_temporal(rhs).						{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::EQUIV); }
formula_temporal(new_f) ::= formula_temporal(lhs) IMPL(op) formula_temporal(rhs).						{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::IMPL); }
formula_temporal(new_f) ::= formula_temporal(lhs) ARROW_RDASH(op) formula_temporal(rhs).				{ NESTED_BOP(new_f, lhs, op, rhs, BinaryFormula::Operator::IMPL); }
formula_temporal(new_f) ::= term_strong(lhs) COLON(op) formula_temporal(rhs).							{ BINDING(new_f, lhs, op, rhs); }

/*************************************************************************************************/
/* Quantifiers */
/*************************************************************************************************/

%type       quant_lst						{ QuantifierFormula::QuantifierList*		}		// a list of quantifier operators and variables
%destructor quant_lst						{ DEALLOC($$);								}
%type       quant_op						{ QuantifierFormula::Operator::type			}		// a single quantifier operator
%destructor quant_op						{ /* Intentionally left blank */			}

formula_quant(quant) ::= BRACKET_L(bl) quant_lst(lst) PIPE formula(sub) BRACKET_R(br).
	{
		quant=NULL;
		ref_ptr<const Token> bl_ptr = bl;
		ref_ptr<QuantifierFormula::QuantifierList> lst_ptr = lst;
		ref_ptr<Formula> sub_ptr = sub;
		ref_ptr<const Token> br_ptr = br;

		if (!parser->lang()->support(Language::Feature::FORMULA_QUANTIFIER)) {
			parser->_feature_error(Language::Feature::FORMULA_QUANTIFIER, &bl->beginLoc());
			YYERROR;
		} else quant = new QuantifierFormula(lst, sub, bl->beginLoc(), br->endLoc());
	}

quant_lst(lst) ::= quant_op(op) variable(var).
	{
		lst = new QuantifierFormula::QuantifierList();
		lst->push_back(QuantifierFormula::Quantifier(op, var));
	}

quant_lst(new_lst) ::= quant_lst(lst) quant_op(op) variable(var).
	{
		new_lst = lst;
		new_lst->push_back(QuantifierFormula::Quantifier(op, var));
	}

quant_op(op) ::= BIG_CONJ.												{ op = QuantifierFormula::Operator::CONJ; }
quant_op(op) ::= BIG_DISJ.												{ op = QuantifierFormula::Operator::DISJ; }



/*************************************************************************************************/
/* Cardinality constraints */
/*************************************************************************************************/
%type		card_var_lst					{ CardinalityFormula::VariableList*			}		// optional list of local variables for cardinality formula followed by |.
%destructor card_var_lst					{ DEALLOC($$);								}
%type		card_var_lst_inner				{ CardinalityFormula::VariableList*			}		// list of local variables for cardinality formula.
%destructor card_var_lst_inner				{ DEALLOC($$);								}


%include {
	#define CARD_FORMULA(card, min, lbrack, vars, af, rbrack, max)																	\
		card = NULL;																												\
		ref_ptr<const Referenced> vars_ptr = vars, af_ptr = af;																	\
		ref_ptr<const Term> min_ptr = min, max_ptr = max;																		\
		ref_ptr<const Token> lbrack_ptr = lbrack, rbrack_ptr = rbrack;															\
																																	\
		bool good = true;																										\
		if (min && min_ptr->domainType() != DomainType::INTEGRAL && min_ptr->domainType() != DomainType::UNKNOWN) {				\
			parser->_parse_error("Invalid lower cardinality bound. Expected an integral expression.", &min_ptr->beginLoc());		\
			good = false;																										\
			YYERROR;																											\
		}																														\
																																	\
		if (max && max_ptr->domainType() != DomainType::INTEGRAL && max_ptr->domainType() != DomainType::UNKNOWN) {				\
			parser->_parse_error("Invalid upper cardinality bound. Expected an integral expression.", &max_ptr->beginLoc());		\
			good = false;																										\
			YYERROR;																											\
		}																														\
																																\
		if (good) {																												\
			/* hopefully good to go. */																							\
			card = new CardinalityFormula(vars, af, min, max, 																	\
				(min ? min_ptr->beginLoc() : lbrack_ptr->beginLoc()), 															\
				(max ? max_ptr->endLoc() : rbrack_ptr->endLoc()));																\
		}																														\



}


//formula_card(new_card) ::= formula_smpl_card(card). { new_card = card; }
formula_card(card) ::= 					CBRACKET_L(bl) card_var_lst(vars) formula(f) CBRACKET_R(br).				[PREC_1]	{ CARD_FORMULA(card, NULL, bl, vars, f, br, NULL);  }
formula_card(card) ::= term_strong(min) CBRACKET_L(bl) card_var_lst(vars) formula(f) CBRACKET_R(br).				[PREC_1]	{ CARD_FORMULA(card, min, bl, vars, f,  br, NULL);  }
formula_card(card) ::= 					CBRACKET_L(bl) card_var_lst(vars) formula(f) CBRACKET_R(br) term(max).		[PREC_1]	{ CARD_FORMULA(card, NULL, bl, vars, f, br, max); }
formula_card(card) ::= term_strong(min) CBRACKET_L(bl) card_var_lst(vars) formula(f) CBRACKET_R(br) term(max).		[PREC_1]	{ CARD_FORMULA(card, min, bl, vars, f,  br, max); }


card_var_lst(new_vars) ::= card_var_lst_inner(vars) PIPE.
	{
		new_vars = vars;
	}
card_var_lst(new_vars) ::= PIPE.
	{
		new_vars = new CardinalityFormula::VariableList();
	}

card_var_lst_inner(new_vars) ::= variable(v).
	{
		ref_ptr<const Referenced> v_ptr = v;
		new_vars = new CardinalityFormula::VariableList();
		new_vars->push_back(v->symbol());
	}

card_var_lst_inner(new_vars) ::= card_var_lst_inner(vars) COMMA variable(v).
	{
		ref_ptr<const Referenced> v_ptr = v;
		new_vars = vars;
		new_vars->push_back(v->symbol());
	}

/********************************************************************************************************************************/
/*************************************************************************************************/
/* Head formulas */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       head_formula 					{ Formula* 									}			// A formula in the head of a law.
%destructor head_formula					{ DEALLOC($$);								}
%type		formula_smpl_card				{ CardinalityFormula*						}			// cardinality formula in the head of a law
%destructor formula_smpl_card				{ DEALLOC($$);								}

head_formula(f) ::= comparison(c).													{ f = c; }
head_formula(f) ::= atomic_formula(l).												{ f = l; }
head_formula(f) ::= formula_smpl_card(c).
	{ 
		f = c;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &c->beginLoc());
			YYERROR;
		}
	}
head_formula(f) ::= TRUE(e).														{ f = new NullaryFormula(NullaryFormula::Operator::TRUE, e->beginLoc(), e->endLoc()); }
head_formula(f) ::= FALSE(e).														{ f = new NullaryFormula(NullaryFormula::Operator::FALSE, e->beginLoc(), e->endLoc()); }
head_formula(f) ::= DASH(d) constant(c).												
	{ 
		f = NULL;
		ref_ptr<const Token> d_ptr = d;
		ref_ptr<Constant> c_ptr = c;

		if (!parser->lang()->support(Language::Feature::FORMULA_NOT_DASH_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_NOT_DASH_HEAD);
			YYERROR;
		} else {
			ATOMIC_FORMULA(f, c, "false"); 
		}
	}

formula_smpl_card(card) ::= 					CBRACKET_L(bl) card_var_lst(vars) atomic_formula_one_const(f) CBRACKET_R(br).			[PREC_0]		{ CARD_FORMULA(card, NULL, bl, vars, f, br, NULL);  }
formula_smpl_card(card) ::= term_strong(min) 	CBRACKET_L(bl) card_var_lst(vars) atomic_formula_one_const(f) CBRACKET_R(br).			[PREC_0]		{ CARD_FORMULA(card, min, bl, vars, f,  br, NULL);  }
formula_smpl_card(card) ::= 					CBRACKET_L(bl) card_var_lst(vars) atomic_formula_one_const(f) CBRACKET_R(br) term(max).	[PREC_0]		{ CARD_FORMULA(card, NULL, bl, vars, f, br, max); }
formula_smpl_card(card) ::= term_strong(min) 	CBRACKET_L(bl) card_var_lst(vars) atomic_formula_one_const(f) CBRACKET_R(br) term(max).	[PREC_0]		{ CARD_FORMULA(card, min, bl, vars, f,  br, max); }

/********************************************************************************************************************************/
/*************************************************************************************************/
/* Macro Statements */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       macro_def_lst                   { MacroDeclaration::ElementList*            }           // macro definition list "macro1 -> def1; macro2 -> def2; ..."
%destructor macro_def_lst                   { DEALLOC($$);                              }
%type       macro_bnd                       { MacroSymbol*                              }           // a single macro binding "macro1 -> def1"
%destructor macro_bnd                       { DEALLOC($$);                              }
%type       macro_args                      { MacroSymbol::ArgumentList*                }           // argument list for a macro
%destructor macro_args                      { DEALLOC($$);                              }
%type       macro_arg                       { Token const*                              }           // a single argument for the macro
%destructor macro_arg                       { DEALLOC($$);                              }


stmt_macro_def(stmt) ::= COLON_DASH(cd) MACROS(kw) macro_def_lst(l) PERIOD(p).
    {
		stmt = NULL;
        ref_ptr<const Token> cd_ptr = cd;
        ref_ptr<const Token> kw_ptr = kw;
        ref_ptr<MacroDeclaration::ElementList> l_ptr = l;
        ref_ptr<const Token> p_ptr = p;

        if (!parser->lang()->support(Language::Feature::DECL_MACRO)) {
            parser->_feature_error(Language::Feature::DECL_MACRO, &kw->beginLoc());
            YYERROR;
        } else {
		    BOOST_FOREACH(symbols::MacroSymbol* m, *l) {
			    if (!parser->symtab()->create(m)) {
	    	        // Check if it's a duplicate
	    	        symbols::MacroSymbol* m2 = (symbols::MacroSymbol*)parser->symtab()->resolve(symbols::Symbol::Type::MACRO, *m->base(), m->arity());
		            if (!m2 || m2 != m) {
		                parser->_parse_error("Detected conflicting definition of symbol \"" + *m->name() + "\".", &stmt->beginLoc());
		            } else {
		                parser->_parse_error("Detected a duplicate definition of symbol \"" + *m->name() + "\".", &stmt->beginLoc());
		            }
		        }
		    }

			stmt = new MacroDeclaration(l, cd->beginLoc(), p->endLoc());
        }
    }

macro_def_lst(lst) ::= macro_bnd(bnd).
    {
        lst = new MacroDeclaration::ElementList();
        lst->push_back(bnd);
    }

macro_def_lst(new_lst) ::= macro_def_lst(lst) SEMICOLON macro_bnd(bnd).
    {
        new_lst = lst;
        new_lst->push_back(bnd);
    }

macro_bnd(bnd) ::= IDENTIFIER(id) PAREN_L macro_args(args) PAREN_R ARROW_RDASH MACRO_STRING(def).
    {
        ref_ptr<const Token> id_ptr = id;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = args;
        ref_ptr<const Token> def_ptr = def;

        bnd = new MacroSymbol(id->str(), def->str(), args);
    }

macro_bnd(bnd) ::= IDENTIFIER(id) ARROW_RDASH MACRO_STRING(def).
    {
        ref_ptr<const Token> id_ptr = id;
        ref_ptr<const Token> def_ptr = def;

        bnd = new MacroSymbol(id->str(), def->str());
    }

macro_args(args) ::= macro_arg(arg).
    {
        args = new MacroSymbol::ArgumentList();
        args->push_back(arg->str());
        delete arg;
    }
macro_args(new_args) ::= macro_args(args) COMMA macro_arg(arg).
    {
        new_args = args;
        new_args->push_back(arg->str());
        delete arg;
    }

macro_arg(arg) ::= POUND_INTEGER(pi).
    {
        arg = pi;
    }
macro_arg(arg) ::= POUND_IDENTIFIER(pid).
    {
        arg = pid;
    }


/********************************************************************************************************************************/
/*************************************************************************************************/
/* Sorts */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type		sort_lst					{ ConstantSymbol::SortList*				}					// A list of comma separated sort names
%destructor sort_lst					{ DEALLOC($$);							}
%type       sort						{ SortSymbol*							}					// A single sort name
%destructor sort						{ /* Intentionally left blank */		}
%type		sort_id_nr					{ SortSymbol*							}					// A single sort or number range
%destructor sort_id_nr					{ /* Intentionally left blank */		}
%type		sort_nr						{ SortSymbol*							}					// A dynamic number range sort
%destructor sort_nr						{ /* Intentionally left blank */		}
%type       sort_id						{ SortSymbol*							}					// A single sort that's not a dynamic number range
%destructor sort_id						{ /* Intentionally left blank */		}

sort_lst(lst) ::= sort(s).
	{
		lst = new ConstantSymbol::SortList();
		lst->push_back(s);
	}
sort_lst(new_lst) ::= sort_lst(lst) COMMA sort(s).
	{
		new_lst = lst;
		new_lst->push_back(s);
	}


%include {
	#define DYNAMIC_SORT_SYM(sort, subsort, sym, feature, sortname, objectname)																						\
		sort = NULL;																																				\
		ref_ptr<SortSymbol> subsort_ptr = subsort;																													\
		ref_ptr<const Token> sym_ptr = sym;																															\
																																									\
		if (!parser->lang()->support(feature)) {																													\
			parser->_feature_error(feature, &sym->beginLoc());																										\
			YYERROR;																																				\
		} else {																																					\
			std::string name = sortname;																															\
			sort = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name)));														\
			if (!sort) {																																			\
				parser->_parse_error("An error occurred creating sort \"" + name + "\".", &sym->beginLoc());														\
				YYERROR;																																			\
			} else {																																				\
				sort->addSubSort(subsort);																															\
																																									\
				/* Get the additional object and add it */																											\
				ref_ptr<ObjectSymbol> obj = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(objectname)));					\
				if (!obj) {																																			\
					parser->_parse_error("An error occurred creating object symbol \"" + std::string(objectname) + "/0\".", &sym->beginLoc());						\
					YYERROR;																																		\
				} else {																																			\
					sort->add(obj);																																	\
				}																																					\
			}																																						\
		}
		
	#define DYNAMIC_SORT_PLUS(new_s, s, op, o)																														\
		new_s = NULL;																																				\
		ref_ptr<const Referenced> s_ptr = s, op_ptr = op, o_ptr = o;																								\
																																									\
																																									\
		if (!parser->lang()->support(Language::Feature::SORT_PLUS)) {																								\
			parser->_feature_error(Language::Feature::SORT_PLUS, &op->beginLoc());																					\
			YYERROR;																																				\
		} else {																																					\
			std::string name = *s->base() + "__" + *o->symbol()->base() + "_" + boost::lexical_cast<std::string>(o->arity()) + "__";								\
			new_s = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name)));														\
			if (!new_s) {																																			\
				parser->_parse_error("An error occurred creating sort \"" + name + "\".", &op->beginLoc());															\
				YYERROR;																																			\
			} else {																																				\
				new_s->addSubSort(s);																																\
				new_s->add(o->symbol());																															\
			}																																						\
		}																																						
}
sort(new_s) ::= sort_id_nr(s).					{ new_s = s; }
sort(new_s) ::= sort_id_nr(s) STAR(sym).		{ DYNAMIC_SORT_SYM(new_s, s, sym, Language::Feature::STAR_SORT, *s->base() + "__plus_none_0", "none"); }
sort(new_s) ::= sort_id_nr(s) CARROT(sym).		{ DYNAMIC_SORT_SYM(new_s, s, sym, Language::Feature::CARROT_SORT, *s->base() + "__plus_unknown_0__", "unknown"); }
sort(new_s) ::= sort_nr(s) PLUS(op) object_nullary(o).
												{ DYNAMIC_SORT_PLUS(new_s, s, op, o); }
sort(new_s) ::= sort_id(s) PLUS(op) object_nullary(o).
												{ DYNAMIC_SORT_PLUS(new_s, s, op, o); }
sort(new_s) ::= sort_id(s) PLUS(op) INTEGER(i). { ref_ptr<const Referenced> i_ptr = i; DYNAMIC_SORT_SYM(new_s, s, op, Language::Feature::SORT_PLUS, *s->base() + "__plus_" + *i->str(), (*((std::string const*)i->str()))); }


sort_id_nr(new_s) ::= sort_id(s).				{ new_s = s; }
sort_id_nr(new_s) ::= sort_nr(nr).				{ new_s = nr; }

sort_nr(s) ::= num_range(nr).
	{
		ref_ptr<const Referenced> nr_ptr = nr;

		s = NULL;

		if (!parser->lang()->support(Language::Feature::NUMRANGE_SORT)) {
			parser->_feature_error(Language::Feature::NUMRANGE_SORT, &nr->beginLoc());
			YYERROR;
		}

		// X..Y becomes __sort_X_Y__
		std::string name = "__sort_" + boost::lexical_cast<std::string>(nr->min()) + "__" + boost::lexical_cast<std::string>(nr->max()) + "__";

		ref_ptr<SortSymbol::ObjectList> objs = new SortSymbol::ObjectList();

		// Generate the objects that it will have
		for (int i = nr->min(); i <= nr->max(); i++) {
			std::string obj_name = boost::lexical_cast<std::string>(i);
			ObjectSymbol const* sym = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(obj_name)));

			if (!sym) {
				s = NULL;
				parser->_parse_error("An error occurred creating symbol \"" + obj_name + "/0\".", &nr->beginLoc());
				YYERROR;
			}
			objs->insert(sym);
		}		

		// dynamically declare the sort
		s = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name), objs));
		if (!s) {
				parser->_parse_error("An error occurred creating symbol \"" + name + "/0\".", &nr->beginLoc());
				YYERROR;
		} 
	}

sort_id(s) ::= IDENTIFIER(id).
	{
		// dynamically declare the sort
		s = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *id->str());
		if (!s) {
			parser->_parse_error("\"" + Symbol::genName(*id->str(),0) + "\" is not a declared sort.", &id->beginLoc());
			YYERROR;
		}
		delete id;
	}




/********************************************************************************************************************************/
/*************************************************************************************************/
/* Constant Definitions */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       constant_bnd_lst			{ ConstantDeclaration::ElementList*				}			// A list of constant bindings
%destructor constant_bnd_lst			{ DEALLOC($$);									}
%type       constant_bnd				{ ConstantDeclaration::ElementList*				}			// A single constant binding "c1,c2,c3 :: type(sort)"
%destructor constant_bnd				{ DEALLOC($$);									}
%type		constant_dcl_lst			{ IdentifierDeclList*							}			// A list of comma separated identifier declarations (<id>(<sort>,<sort>...))
%destructor constant_dcl_lst			{ DEALLOC($$);									}
%type       constant_dcl_type			{ ConstantSymbol::Type::type					}			// constant type keywords ("simpleFluent", "inertialFluent",...)
%destructor constant_dcl_type			{ /* Intentionally left blank */				}
%type       attrib_spec					{ SortSymbol const*								}			// an attribute/sort specifier
%destructor attrib_spec					{ /* Intentionally left blank */				}

stmt_constant_def(stmt) ::= COLON_DASH(cd) CONSTANTS(kw) constant_bnd_lst(l) PERIOD(p).
	{
		ref_ptr<const Token> cd_ptr = cd;
		ref_ptr<const Token> kw_ptr = kw;
		ref_ptr<ConstantDeclaration::ElementList> l_ptr = l;
		ref_ptr<const Token> p_ptr = p;
		
		if (!parser->lang()->support(Language::Feature::DECL_CONSTANT)) {
			stmt = NULL;
			parser->_feature_error(Language::Feature::DECL_CONSTANT, &kw->beginLoc());
			YYERROR;
		} else {
			stmt = new ConstantDeclaration(l, cd->beginLoc(), p->endLoc());

		}
	}

constant_bnd_lst(lst) ::= constant_bnd(bnd).
	{
		lst = bnd;
	}

constant_bnd_lst(new_lst) ::= constant_bnd_lst(lst) SEMICOLON constant_bnd(bnd).
	{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = bnd;
		new_lst = lst;
		new_lst->splice(new_lst->end(), *bnd);
	}

%include {
	#define CONSTANT_DECL(c, loc)																							\
		if (!parser->symtab()->create(c)) {																					\
			/* Check if it's a duplicate */																					\
			ConstantSymbol* c2 = (ConstantSymbol*)parser->symtab()->resolve(Symbol::Type::CONSTANT, *c->base(), c->arity());\
			if (!c2 || c2 != c) {																							\
				parser->_parse_error("Detected conflicting definition of symbol \"" + *c->name() + "\".", &loc);			\
			} else {																										\
				parser->_parse_error("Detected a duplicate definition of symbol \"" + *c->name() + "\".", &loc);			\
			}																												\
		}
}

constant_bnd(bnd) ::= constant_dcl_lst(names) DBL_COLON constant_dcl_type(type) PAREN_L sort(s) PAREN_R.
	{
		ref_ptr<const Referenced> names_ptr = names, s_ptr = s;
		bnd = new ConstantDeclaration::ElementList();


		BOOST_FOREACH(IdentifierDecl& decl, *names) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(type, decl.first->str(), s, decl.second);
			bnd->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	}
constant_bnd(bnd) ::= constant_dcl_lst(names) DBL_COLON sort(s).
	{
		ref_ptr<const Referenced> names_ptr = names, s_ptr = s;
		bnd = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *names) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(ConstantSymbol::Type::RIGID, decl.first->str(), s, decl.second);
			bnd->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	}
constant_bnd(bnd) ::= constant_dcl_lst(names) DBL_COLON constant_dcl_type(type).
	{
		ref_ptr<const Referenced> names_ptr = names;
		bnd = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *names) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(type, decl.first->str(), parser->symtab()->boolsort(), decl.second);
			bnd->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	}
constant_bnd(bnd) ::= constant_dcl_lst(names) DBL_COLON attrib_spec(s) OF IDENTIFIER(id).
	{
		bnd = NULL;
		ref_ptr<const Referenced> names_ptr = names, s_ptr = s, id_ptr = id;


		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *id->str());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*id->str(), 0) + "\" is not a valid constant symbol.", &id->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*id->str(), 0) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &id->beginLoc());
			YYERROR;
		} else {
			bnd = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *names) {
				ref_ptr<ConstantSymbol> c= new AttributeSymbol(decl.first->str(), s, c, decl.second);
				bnd->push_back(c);
				CONSTANT_DECL(c, decl.first->beginLoc());
			}
		}
	}
constant_bnd(bnd) ::= constant_dcl_lst(names) DBL_COLON attrib_spec(s) OF IDENTIFIER(id) PAREN_L sort_lst(lst) PAREN_R.
	{
		bnd = NULL;
		ref_ptr<const Referenced> names_ptr = names, s_ptr = s, id_ptr = id, lst_ptr = lst;

		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *id->str(), lst->size());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*id->str(), lst->size()) + "\" is not a valid constant symbol.", &id->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*id->str(), lst->size()) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &id->beginLoc());
			YYERROR;
		} else {
			// ensure that the sorts match the declaration of the symbol
			SortList::const_iterator it = lst->begin();
			BOOST_FOREACH(SortSymbol const* sort, *c) {
				if (*it != sort) {
					// check to see if it's a subsort, which is also permissable
					bool found = false;
					for (SortSymbol::SortList::const_iterator it2 = sort->beginSubSorts(); it2 != sort->endSubSorts(); it2++) {
						if (*it == *it2) {
							found = true;
							break;
						}
					}

					if (!found) {
						parser->_parse_error("Detected a sort mismatch in an attribute parent declaration. \"" + *(*it)->base() + "\" is not an explicit subsort of \"" + *sort->base() + "\".", &id->beginLoc());
						YYERROR;
					}
				}
				it++;
			}

			bnd = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *names) {
				// ensure that the sorts match the start of the sort list for each of the symbols
				if (decl.second->size() < lst->size()) {
					parser->_parse_error("Detected a malformed attribute declaration. An attribute must duplicate its parent's parameters.", &decl.first->beginLoc());
					YYERROR;
				} else {
					bool good_sort = true;		
					it = decl.second->begin();
					BOOST_FOREACH(SortSymbol const* sort, *lst) {
						if (*it != sort) {
							// check to see if it's a subsort, which is also permissable
							bool found = false;
							for (SortSymbol::SortList::const_iterator it2 = sort->beginSubSorts(); it2 != sort->endSubSorts(); it2++) {
								if (*it == *it2) {
									found = true;
									break;
								}
							}
							if (!found) {
								good_sort = false;
								parser->_parse_error("Detected a sort mismatch in an attribute declaration. \"" + *(*it)->base() + "\" is not an explicit subsort of \"" + *sort->base() + "\".", &decl.first->beginLoc());
								YYERROR;
							}
						}
						it++;	
					}

					if (good_sort) {
						ref_ptr<ConstantSymbol> sym = new AttributeSymbol(decl.first->str(), s, c, decl.second);
						bnd->push_back(sym);
						CONSTANT_DECL(sym, decl.first->beginLoc());

					}
				}
			}
		}
	}


constant_dcl_lst(names) ::= IDENTIFIER(id).
	{
		names = new IdentifierDeclList();
		names->push_back(IdentifierDecl(id, NULL));
	}
constant_dcl_lst(names) ::= IDENTIFIER(id) PAREN_L sort_lst(lst) PAREN_R.
	{
		names = new IdentifierDeclList();
		names->push_back(IdentifierDecl(id, lst));
	}
constant_dcl_lst(new_names) ::= constant_dcl_lst(names) COMMA IDENTIFIER(id).
	{
		new_names = names;
		new_names->push_back(IdentifierDecl(id, NULL));
	}
constant_dcl_lst(new_names) ::= constant_dcl_lst(names) COMMA IDENTIFIER(id) PAREN_L sort_lst(lst) PAREN_R.
	{
		new_names = names;
		new_names->push_back(IdentifierDecl(id, lst));
	}


constant_dcl_type(t) ::= ABACTION(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &tok->beginLoc());
			YYERROR;
		}
	}
constant_dcl_type(t) ::= ACTION(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &tok->beginLoc());
			YYERROR;
		}
	}
constant_dcl_type(t) ::= ADDITIVEACTION(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &tok->beginLoc());
			YYERROR;
		}
	}
constant_dcl_type(t) ::= ADDITIVEFLUENT(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &tok->beginLoc());
			YYERROR;
		}
	}
constant_dcl_type(t) ::= EXTERNALACTION(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &tok->beginLoc());
			YYERROR;
		}
	}
constant_dcl_type(t) ::= EXTERNALFLUENT(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &tok->beginLoc());
			YYERROR;
		}
	}
constant_dcl_type(t) ::= EXOGENOUSACTION(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &tok->beginLoc());
			YYERROR;
		}
	}
constant_dcl_type(t) ::= INERTIALFLUENT(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &tok->beginLoc());
			YYERROR;
		}
	}
constant_dcl_type(t) ::= RIGID(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &tok->beginLoc());
			YYERROR;
		}
	}
constant_dcl_type(t) ::= SIMPLEFLUENT(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &tok->beginLoc());
			YYERROR;
		}
	}

constant_dcl_type(t) ::= SDFLUENT(tok).						
	{ 
		ref_ptr<const Token> tok_ptr = tok;
		t = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &tok->beginLoc());
			YYERROR;
		}
	}

attrib_spec(attr) ::= ATTRIBUTE(kw).
	{
		attr = NULL;
		ref_ptr<const Referenced> kw_ptr = kw;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &kw->beginLoc());
			YYERROR;
		} else {
			// grab the boolean sort and provide it
			attr = parser->symtab()->boolsort();
		}
	}

attrib_spec(attr) ::= ATTRIBUTE(kw) PAREN_L sort(s) PAREN_R.
	{
		attr = NULL;
		ref_ptr<const Referenced> kw_ptr = kw, s_ptr = s;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &kw->beginLoc());
			YYERROR;
		} else {
			attr = s;
		}
	}


/********************************************************************************************************************************/
/*************************************************************************************************/
/* Object Definitions */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       object_bnd_lst			{ ObjectDeclaration::ElementList*				}			// A list of object bindings
%destructor object_bnd_lst			{ DEALLOC($$);									}
%type       object_bnd				{ ObjectDeclaration::Element*				}			// A single object binding "o1,o2,o3 :: sort"
%destructor object_bnd				{ DEALLOC($$);									}
%type       object_lst				{ ObjectDeclaration::Element::ObjectList*	}			// A list of objects
%destructor object_lst				{ DEALLOC($$);									}
%type       object_spec				{ ObjectDeclaration::Element::ObjectList*   }			// A single object specification (or numerical range).
%destructor object_spec				{ DEALLOC($$);									}

stmt_object_def(stmt) ::= COLON_DASH(cd) OBJECTS(kw) object_bnd_lst(l) PERIOD(p).
	{
		ref_ptr<const Token> cd_ptr = cd;
		ref_ptr<const Token> p_ptr = p;
		ref_ptr<const Token> kw_ptr = kw;
		ref_ptr<ObjectDeclaration::ElementList> l_ptr = l;
		
		if (!parser->lang()->support(Language::Feature::DECL_OBJECT)) {
			stmt = NULL;
			parser->_feature_error(Language::Feature::DECL_OBJECT, &kw->beginLoc());
			YYERROR;
		} else {
			stmt = new ObjectDeclaration(l, cd->beginLoc(), p->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ObjectDeclaration::Element* bnd, *l) {
				BOOST_FOREACH(ObjectSymbol const* o, *bnd) {
						bnd->sort()->add(o);
				}
			}
		}
	}

object_bnd_lst(lst) ::= object_bnd(bnd).
	{
		lst = new ObjectDeclaration::ElementList();
		lst->push_back(bnd);
	}

object_bnd_lst(new_lst) ::= object_bnd_lst(lst) SEMICOLON object_bnd(bnd).
	{
		new_lst = lst;
		new_lst->push_back(bnd);
	}

object_bnd(bnd) ::= object_lst(objs) DBL_COLON sort_id(s).
	{
		bnd = new ObjectDeclaration::Element(s, objs);
	}

object_lst(lst) ::= object_spec(obj).
	{
		lst = obj;
	}
object_lst(new_lst) ::= object_lst(lst) COMMA object_spec(obj).
	{
		new_lst = lst;
		new_lst->splice(new_lst->end(), *obj);
		delete obj;
	}



object_spec(obj) ::= IDENTIFIER(id).
	{
		ref_ptr<const Token> id_ptr = id;
		obj = NULL;
		ref_ptr<const ObjectSymbol> o = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(id->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*id->str(),0) + "\".", &id->beginLoc());
			YYERROR;
		} else {
			obj = new ObjectDeclaration::Element::ObjectList();
			obj->push_back(o);
		}
	}
object_spec(obj) ::= IDENTIFIER(id) PAREN_L sort_lst(lst) PAREN_R.
	{
		obj = NULL;
		ref_ptr<ObjectSymbol::SortList> lst_ptr = lst;
		ref_ptr<const Token> id_ptr = id;
		ref_ptr<const ObjectSymbol> o = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(id->str(), lst));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*id->str(),lst->size()) + "\".", &id->beginLoc());
			YYERROR;
		} else {
			obj = new  ObjectDeclaration::Element::ObjectList();
			obj->push_back(o);
		}
	}
object_spec(obj) ::= num_range(nr). 
	{
		obj = new ObjectDeclaration::Element::ObjectList();
		ref_ptr<const Referenced> nr_ptr = nr;

		// iterate over the range and add it to the list
		for (int i = nr->min(); i <= nr->max(); i++) {
			std::string name = boost::lexical_cast<std::string>(i);
			ref_ptr<const ObjectSymbol> o = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(name)));
			if (!o) {
				parser->_parse_error("INTERNAL ERROR: Could not create object symbol \"" + Symbol::genName(name, 0) + "\".", &nr->beginLoc());
				YYERROR;
			} else {
				obj->push_back(o);
			}
		}
	}


/********************************************************************************************************************************/
/*************************************************************************************************/
/* Variable Definitions */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       variable_bnd_lst			{ VariableDeclaration::ElementList*				}			// A list of variable bindings
%destructor variable_bnd_lst			{ DEALLOC($$);									}
%type       variable_bnd				{ VariableDeclaration::ElementList*				}			// A single variable binding "v1,v2,v3 :: sort"
%destructor variable_bnd				{ DEALLOC($$);									}
%type       variable_lst				{ TokenList*									}			// A list of variables
%destructor variable_lst				{ DEALLOC($$);									}

stmt_variable_def(stmt) ::= COLON_DASH(cd) VARIABLES(kw) variable_bnd_lst(l) PERIOD(p).
	{
		ref_ptr<const Token> cd_ptr = cd;
		ref_ptr<const Token> p_ptr = p;
		ref_ptr<const Token> kw_ptr = kw;
		ref_ptr<VariableDeclaration::ElementList> l_ptr = l;
		
		if (!parser->lang()->support(Language::Feature::DECL_VARIABLE)) {
			stmt = NULL;
			parser->_feature_error(Language::Feature::DECL_VARIABLE, &kw->beginLoc());
			YYERROR;
		} else {
			stmt = new VariableDeclaration(l, cd->beginLoc(), p->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(VariableSymbol* v, *l) {
				if (!parser->symtab()->create(v)) {
					// Check if it's a duplicate
					VariableSymbol* v2 = (VariableSymbol*)parser->symtab()->resolve(Symbol::Type::VARIABLE, *v->base());
					if (!v2 || v2 != v) {
						parser->_parse_error("Detected conflicting definition of symbol \"" + *v->name() + "\".", &cd->beginLoc());
					} else {
						parser->_parse_error("Detected a duplicate definition of symbol \"" + *v->name() + "\".", &cd->beginLoc());
					}
				}
			}
		}
	}

variable_bnd_lst(lst) ::= variable_bnd(bnd).
	{
		lst = bnd;
	}

variable_bnd_lst(new_lst) ::= variable_bnd_lst(lst) SEMICOLON variable_bnd(bnd).
	{
		new_lst = lst;
		new_lst->splice(new_lst->end(), *bnd);
		delete bnd;
	}

variable_bnd(bnd) ::= variable_lst(vars) DBL_COLON sort_id(s).
	{
		bnd = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *vars) {
			bnd->push_back(new VariableSymbol(tok->str(), s));
		}
		delete vars;
	}

variable_lst(vars) ::= IDENTIFIER(id).
	{
		vars = new TokenList();
		vars->push_back(id);
	}
variable_lst(new_vars) ::= variable_lst(vars) COMMA IDENTIFIER(id).
	{
		new_vars = vars;
		new_vars->push_back(id);
	}



/********************************************************************************************************************************/
/*************************************************************************************************/
/* Sort Definitions */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       sort_bnd_lst			{ SortDeclaration::ElementList*					}			// A list of sort bindings
%destructor sort_bnd_lst			{ DEALLOC($$);									}
%type       sort_bnd				{ SortDeclaration::ElementList*					}			// A single sort binding "v1,v2,v3 :: sort"
%destructor sort_bnd				{ DEALLOC($$);									}
%type       sort_dcl_lst			{ SortDeclaration::ElementList*					}			// A list of sorts
%destructor sort_dcl_lst			{ DEALLOC($$);									}

stmt_sort_def(stmt) ::= COLON_DASH(cd) SORTS(kw) sort_bnd_lst(l) PERIOD(p).
	{
		ref_ptr<const Token> cd_ptr = cd;
		ref_ptr<const Token> p_ptr = p;
		ref_ptr<const Token> kw_ptr = kw;
		ref_ptr<SortDeclaration::ElementList> l_ptr = l;
		
		if (!parser->lang()->support(Language::Feature::DECL_SORT)) {
			stmt = NULL;
			parser->_feature_error(Language::Feature::DECL_SORT, &kw->beginLoc());
			YYERROR;
		} else {
			stmt = new SortDeclaration(l, cd->beginLoc(), p->endLoc());
		}
	}

sort_bnd_lst(lst) ::= sort_bnd(bnd).
	{
		lst = bnd;
	}

sort_bnd_lst(new_lst) ::= sort_bnd_lst(lst) SEMICOLON sort_bnd(bnd).
	{
		new_lst = lst;
		new_lst->splice(new_lst->end(), *bnd);
		delete bnd;
	}

sort_bnd(bnd) ::= sort_dcl_lst(sorts).
	{
		bnd = sorts;
	}

sort_bnd(bnd) ::= sort_bnd(lhs) DBL_LTHAN sort_bnd(rhs).
	{
		BOOST_FOREACH(SortSymbol* sym, *lhs) {
			BOOST_FOREACH(SortSymbol* sym2, *rhs) {
				sym2->addSubSort(sym);
			}
		}
		bnd = lhs;
		bnd->splice(lhs->end(), *rhs);
		delete rhs;

	}
sort_bnd(bnd) ::= sort_bnd(lhs) DBL_GTHAN sort_bnd(rhs).
	{
		BOOST_FOREACH(SortSymbol* sym, *lhs) {
			BOOST_FOREACH(SortSymbol* sym2, *rhs) {
				sym->addSubSort(sym2);
			}
		}
		bnd = lhs;
		bnd->splice(lhs->end(), *rhs);
		delete rhs;
	}
sort_bnd(new_bnd) ::= PAREN_L sort_bnd(bnd) PAREN_R.
	{
		new_bnd = bnd;
	}

sort_dcl_lst(lst) ::= IDENTIFIER(id).
	{
		ref_ptr<SortSymbol> s = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(id->str()));
		if (!s) {
			lst = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*id->str(),0) + "\".", &id->beginLoc());
			YYERROR;
		} else {
			lst = new SortDeclaration::ElementList();
			lst->push_back(s);
		}

		delete id;
	}
sort_dcl_lst(new_lst) ::= sort_dcl_lst(lst) COMMA IDENTIFIER(id).
	{
		new_lst = lst;
		ref_ptr<SortSymbol> s = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(id->str()));
		if (!s) {
			lst = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*id->str(),0) + "\".", &id->beginLoc());
			YYERROR;
		} else {
			lst->push_back(s);
		}

		delete id;

	}

/********************************************************************************************************************************/
/*************************************************************************************************/
/* Show/hide statements */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       show_lst				{ ShowStatement::ElementList*					}
%destructor show_lst				{ DEALLOC($$);									}
%type       show_elem				{ AtomicFormula*								}
%destructor show_elem				{ DEALLOC($$);									}

stmt_show(stmt) ::= COLON_DASH(cd) SHOW(kw) show_lst(lst) PERIOD(p).
	{
		stmt = NULL;
		ref_ptr<const Token> cd_ptr = cd, kw_ptr = kw, p_ptr = p;
		ref_ptr<ShowStatement::ElementList> lst_ptr = lst;

		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &kw->beginLoc());
			YYERROR;
		} else {
			stmt = new ShowStatement(lst, cd->beginLoc(), p->endLoc());
		}
	}

stmt_show(stmt) ::= COLON_DASH(cd) SHOW(kw) ALL(all) PERIOD(p).
	{
		stmt = NULL;
		ref_ptr<const Token> cd_ptr = cd, kw_ptr = kw, p_ptr = p, all_ptr = all;
		
		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &kw->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_SHOW_ALL)) {
			parser->_feature_error(Language::Feature::DECL_SHOW_ALL, &all->beginLoc());
			YYERROR;
		} else {
			stmt = new ShowAllStatement(cd->beginLoc(), p->endLoc());
		}
	}


stmt_hide(stmt) ::= COLON_DASH(cd) HIDE(kw) show_lst(lst) PERIOD(p).
	{
		stmt = NULL;
		ref_ptr<const Token> cd_ptr = cd, kw_ptr = kw, p_ptr = p;
		ref_ptr<HideStatement::ElementList> lst_ptr = lst;

		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &kw->beginLoc());
			YYERROR;
		} else {
			stmt = new HideStatement(lst, cd->beginLoc(), p->endLoc());
		}
	}

stmt_hide(stmt) ::= COLON_DASH(cd) HIDE(kw) ALL(all) PERIOD(p).
	{
		stmt = NULL;
		ref_ptr<const Token> cd_ptr = cd, kw_ptr = kw, p_ptr = p, all_ptr = all;
		
		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &kw->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_HIDE_ALL)) {
			parser->_feature_error(Language::Feature::DECL_HIDE_ALL, &all->beginLoc());
			YYERROR;
		} else {
			stmt = new HideAllStatement(cd->beginLoc(), p->endLoc());
		}
	}



show_lst(lst) ::= show_elem(elem). 
	{
		lst = new ShowStatement::ElementList();
		lst->push_back(elem);
	}
show_lst(new_lst) ::= show_lst(lst) COMMA show_elem(elem).
	{
		new_lst = lst;
		new_lst->push_back(elem);
	}

show_elem(elem) ::= atomic_formula_one_const(af).	{ elem = af; }


/********************************************************************************************************************************/
/*************************************************************************************************/
/* Noconcurrency */
/*************************************************************************************************/
/********************************************************************************************************************************/

%include {
	#define NC_STATEMENT(stmt, kw, feature, class)													\
		stmt = NULL;																				\
		ref_ptr<const Token> kw_ptr = kw;															\
																									\
		if (!parser->lang()->support(feature)) {													\
			parser->_feature_error(feature, &kw->beginLoc());										\
			YYERROR;																				\
		} else {																					\
			stmt = new class(kw->beginLoc(), kw->endLoc());											\
		}																							

}

stmt_noconcurrency(stmt) ::= NOCONCURRENCY(kw).								{ NC_STATEMENT(stmt, kw, Language::Feature::NOCONCURRENCY, NCStatement); }
stmt_strong_noconcurrency(stmt) ::= STRONG_NOCONCURRENCY(kw).				{ NC_STATEMENT(stmt, kw, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }

/********************************************************************************************************************************/
/*************************************************************************************************/
/* value declarations */
/*************************************************************************************************/
/********************************************************************************************************************************/

%include {
	#define VALUE_DECL(stmt, cd, kw, val_obj, p, feature, class)									\
		stmt = NULL;																				\
		ref_ptr<const Referenced> cd_ptr = cd, kw_ptr = kw, val_ptr = val_obj, p_ptr = p;			\
																									\
		if (!parser->lang()->support(feature)) {													\
			parser->_feature_error(feature, &kw->beginLoc());										\
			YYERROR;																				\
		} else { 																					\
			int value = val_obj->val();																\
			if (value < 0) {																		\
				parser->_parse_error("ERROR: Expected a positive integer.", &val_obj->beginLoc());	\
			} else {																				\
				stmt = new class(value, cd->beginLoc(), p->endLoc());								\
			}																						\
		}
}

stmt_maxafvalue(stmt) ::= COLON_DASH(cd) MAXAFVALUE(kw) EQ term_numeric(i) PERIOD(p).	{ VALUE_DECL(stmt, cd, kw, i, p, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement); }
stmt_maxadditive(stmt) ::= COLON_DASH(cd) MAXADDITIVE(kw) EQ term_numeric(i) PERIOD(p).	{ VALUE_DECL(stmt, cd, kw, i, p, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement); }

/********************************************************************************************************************************/
/*************************************************************************************************/
/* Queries */
/*************************************************************************************************/
/********************************************************************************************************************************/

%include {
	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRange const* maxstep;
		Token const* label;
	};

}

%type       query_lst				{ QueryData													}
%destructor query_lst				{ DEALLOC($$.l); DEALLOC($$.maxstep); DEALLOC($$.label);	}
%type       query_maxstep_decl		{ NumberRange const*										}
%destructor query_maxstep_decl		{ DEALLOC($$);												}
%type       query_label_decl		{ Token const*												}
%destructor query_label_Decl		{ DEALLOC($$);												}					

stmt_query(stmt) ::= COLON_DASH(cd) QUERY(kw) query_lst(data) PERIOD(p).
	{
		stmt = NULL;
		ref_ptr<const Referenced> cd_ptr = cd, kw_ptr = kw, data_l_ptr = data.l, p_ptr = p;
		ref_ptr<const Referenced> data_maxstep_ptr = data.maxstep, data_label_ptr = data.label;

		int min = -1, max = -1;
		if (data.maxstep) {
			min = data.maxstep->min();
			max = data.maxstep->max();
		}

		if (!parser->lang()->support(Language::Feature::DECL_QUERY)) {
			parser->_feature_error(Language::Feature::DECL_QUERY, &kw->beginLoc());
			YYERROR;
		} else {
			bool good = true;

			// resolve the query label
			ref_ptr<QuerySymbol> sym = new QuerySymbol(data.label->str(), min, max);
			if (!parser->symtab()->create(sym)) {
				parser->_parse_error("Could not create query, the label \"" + *data.label->str() + "\" already exists.", &data.label->beginLoc());
				good = false;
				YYERROR;
			}


			if (good) stmt = new QueryStatement(sym, data.l, cd->beginLoc(), p->endLoc());
		}
	}


query_lst(lst) ::= formula_temporal(elem).
	{
		lst.l = new QueryStatement::FormulaList();
		lst.maxstep = NULL;
		lst.label = NULL;

		lst.l->push_back(elem);
	}

query_lst(lst) ::= query_maxstep_decl(elem).
	{
		lst.l = new QueryStatement::FormulaList();
		lst.maxstep = elem;
		lst.label = NULL;
	}

query_lst(lst) ::= query_label_decl(elem).
	{
		lst.l = new QueryStatement::FormulaList();
		lst.maxstep = NULL;
		lst.label = elem;
	}

query_lst(new_lst) ::= query_lst(lst) SEMICOLON formula_temporal(elem).
	{ 
		new_lst = lst;
		lst.l->push_back(elem);
	}

query_lst(new_lst) ::= query_lst(lst) SEMICOLON query_maxstep_decl(elem).
	{
		new_lst = lst;

		if (new_lst.maxstep) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &elem->beginLoc());
			delete elem;
			YYERROR;
		} else {
			new_lst.maxstep = elem;
		}
	}
	
query_lst(new_lst) ::= query_lst(lst) SEMICOLON query_label_decl(elem).
	{
		new_lst = lst;
		if (new_lst.label) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &elem->beginLoc());
			delete elem;
			YYERROR;

		} else {
			new_lst.label = elem;
		}
	}

%include {
	#define QUERY_DECL(decl, kw, val, feature)																\
		decl = NULL;																						\
		ref_ptr<const Token> kw_ptr = kw, val_ptr = val;													\
																											\
		if (!parser->lang()->support(feature)) {															\
			parser->_feature_error(feature, &kw->beginLoc());												\
			YYERROR;																						\
		} else {																							\
			decl = val;																						\
		}

}

query_maxstep_decl(decl) ::= MAXSTEP(kw) DBL_COLON INTEGER(i).			{ 
	decl = NULL;
	ref_ptr<const Referenced> kw_ptr = kw, i_ptr = i;


	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &kw->beginLoc());
		YYERROR;
	} else {

		int max = -1;
		try {
			max = boost::lexical_cast<int>(*i->str());
			decl = new NumberRange(-1, max, i->beginLoc(), i->endLoc());
		} catch (boost::bad_lexical_cast const& e) {
			parser->_parse_error("INTERNAL ERROR: An error occurred extracting an integer from \"" + *i->str() + "\".", &i->beginLoc());
			YYERROR;
		}
	}
}

query_maxstep_decl(decl) ::= MAXSTEP(kw) DBL_COLON num_range(nr). {
	decl = NULL;
	ref_ptr<const Referenced> kw_ptr = kw, nr_ptr = nr;

	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &kw->beginLoc());
		YYERROR;
	} else {
		decl = nr;
		nr_ptr.release();
	}
}


query_label_decl(decl) ::= LABEL(kw) DBL_COLON INTEGER(i).				{ QUERY_DECL(decl, kw, i, Language::Feature::QUERY_LABEL); }
query_label_decl(decl) ::= LABEL(kw) DBL_COLON IDENTIFIER(i).			{ QUERY_DECL(decl, kw, i, Language::Feature::QUERY_LABEL); }



/********************************************************************************************************************************/
/*************************************************************************************************/
/* Clauses */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       clause_if				{ Formula*										}
%destructor clause_if				{ DEALLOC($$);									}
%type       clause_after			{ Formula*										}
%destructor clause_after			{ DEALLOC($$);									}
%type       clause_ifcons			{ Formula*										}
%destructor clause_ifcons			{ DEALLOC($$);									}
%type       clause_unless			{ AtomicFormula*								}
%destructor clause_unless			{ DEALLOC($$);									}
%type       clause_where			{ Formula*										}
%destructor clause_where			{ DEALLOC($$);									}


%include {
	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
}



clause_if(new_f) ::= IF(kw) formula(f).					{ CLAUSE(new_f, kw, f, Language::Feature::CLAUSE_IF); 		}
clause_if(new_f) ::= .									{ new_f = NULL; }
clause_after(new_f) ::= AFTER(kw) formula(f).			{ CLAUSE(new_f, kw, f, Language::Feature::CLAUSE_AFTER);	}
clause_after(new_f) ::= .								{ new_f = NULL; }
clause_ifcons(new_f) ::= IFCONS(kw) formula(f).			{ CLAUSE(new_f, kw, f, Language::Feature::CLAUSE_IFCONS); 	}
clause_ifcons(new_f) ::= .								{ new_f = NULL; }
clause_unless(new_f) ::= UNLESS(kw) atomic_formula(f).	{ CLAUSE(new_f, kw, f, Language::Feature::CLAUSE_UNLESS); 	}
clause_unless(new_f) ::= .								{ new_f = NULL; }
clause_where(new_f) ::= WHERE(kw) formula_no_const(f).	{ CLAUSE(new_f, kw, f, Language::Feature::CLAUSE_WHERE); 	}
clause_where(new_f) ::= .								{ new_f = NULL; }

/********************************************************************************************************************************/
/*************************************************************************************************/
/* Laws */
/*************************************************************************************************/
/********************************************************************************************************************************/

%type       law_basic				{ Statement*									}			// F if G...
%destructor law_basic				{ DEALLOC($$);									}
%type       law_caused				{ Statement*									}			// caused F if G...
%destructor law_caused				{ DEALLOC($$);									}
%type       law_pcaused				{ Statement*									}			// possibly caused F if G...
%destructor law_pcaused				{ DEALLOC($$);									}
%type       law_impl				{ Statement*									}			// F <- G
%destructor law_impl				{ DEALLOC($$);									}			
%type       law_causes				{ Statement*									}			// G causes F...
%destructor law_causes				{ DEALLOC($$);									}			
%type       law_increments			{ Statement*									}			// G increment F by I...
%destructor law_increments			{ DEALLOC($$);									}			
%type       law_mcause				{ Statement*									}			// G may cause F...
%destructor law_mcause				{ DEALLOC($$);									}			
%type       law_always				{ Statement*									}			// always G...
%destructor law_always				{ DEALLOC($$);									}			
%type       law_constraint			{ Statement*									}			// constraint G...
%destructor law_constraint			{ DEALLOC($$);									}			
%type       law_impossible			{ Statement*									}			// impossible G...
%destructor law_impossible			{ DEALLOC($$);									}			
%type       law_never				{ Statement*									}			// never G...
%destructor law_never				{ DEALLOC($$);									}			
%type       law_default				{ Statement*									}			// default F if G...
%destructor law_default				{ DEALLOC($$);									}			
%type       law_exogenous			{ Statement*									}			// exogenous c if G...
%destructor law_exogenous			{ DEALLOC($$);									}			
%type       law_inertial			{ Statement*									}			// inertial c if G...
%destructor law_inertial			{ DEALLOC($$);									}			
%type       law_nonexecutable		{ Statement*									}			// nonexecutable F if G...
%destructor law_nonexecutable		{ DEALLOC($$);									}			
%type       law_rigid				{ Statement*									}			// rigid c...
%destructor law_rigid				{ DEALLOC($$);									}			
%type       law_observed			{ Statement*									}			// observed c=v at v2
%destructor law_observed			{ DEALLOC($$);									}			

stmt_law(stmt) ::= law_basic(law).					{stmt = law;}
stmt_law(stmt) ::= law_caused(law).					{stmt = law;}
stmt_law(stmt) ::= law_pcaused(law).				{stmt = law;}
stmt_law(stmt) ::= law_impl(law).					{stmt = law;}
stmt_law(stmt) ::= law_causes(law).					{stmt = law;}
stmt_law(stmt) ::= law_increments(law).				{stmt = law;}
stmt_law(stmt) ::= law_mcause(law).					{stmt = law;}
stmt_law(stmt) ::= law_always(law).					{stmt = law;}
stmt_law(stmt) ::= law_constraint(law).				{stmt = law;}
stmt_law(stmt) ::= law_impossible(law).				{stmt = law;}
stmt_law(stmt) ::= law_never(law).					{stmt = law;}
stmt_law(stmt) ::= law_default(law).				{stmt = law;}
stmt_law(stmt) ::= law_exogenous(law).				{stmt = law;}
stmt_law(stmt) ::= law_inertial(law).				{stmt = law;}
stmt_law(stmt) ::= law_nonexecutable(law).			{stmt = law;}
stmt_law(stmt) ::= law_rigid(law).					{stmt = law;}
stmt_law(stmt) ::= law_observed(law).				{stmt = law;}


%include {
	#define LAW_BASIC_FORM(law, kw, head, ifbody, ifcons, after, unless, where, p, static, dynamic, class)											\
		law = NULL;																																	\
		ref_ptr<Element> head_ptr = head, if_ptr = ifbody, ifcons_ptr = ifcons, after_ptr = after, unless_ptr = unless, where_ptr = where;		\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
		Language::Feature::type feature = ((after) ? (dynamic) : (static));																			\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, ((kw) ? &(kw_ptr)->beginLoc() : &(head_ptr)->beginLoc()));												\
			YYERROR;																																\
		} else {																																	\
			law = new class(head, ifbody, ifcons, after, unless, where, ((kw) ? (kw_ptr)->beginLoc() : (head_ptr)->beginLoc()), (p_ptr)->endLoc());	\
		}																																			

	#define LAW_IMPL_FORM(law, head, kw, body, where, p, feature, class)																			\
		law = NULL;																																	\
		ref_ptr<Element> head_ptr = head, body_ptr = body, where_ptr = where;																	\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, &kw->beginLoc());																						\
			YYERROR;																																\
		} else {																																	\
			law = new class(head, body, where, head->beginLoc(), p->endLoc());																		\
		}


	#define LAW_DYNAMIC_FORM(law, body, kw, head, ifbody, unless, where, p, feature, class)															\
		law = NULL;																																	\
		ref_ptr<Element> body_ptr = body, head_ptr = head, if_ptr = ifbody, unless_ptr = unless, where_ptr = where;								\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, &kw->beginLoc());																						\
			YYERROR;																																\
		} else {																																	\
			law = new class(body, head, ifbody, unless, where, body->beginLoc(), p->endLoc());														\
		}
	
	#define LAW_INCREMENTAL_FORM(law, body, kw, head, by, ifbody, unless, where, p, feature, class)													\
		law = NULL;																																	\
		ref_ptr<Element> body_ptr = body, head_ptr = head, if_ptr = ifbody, unless_ptr = unless, where_ptr = where;								\
		ref_ptr<Element> by_ptr = by;																											\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, &kw->beginLoc());																						\
			YYERROR;																																\
		} else {																																	\
			law = new class(body, head, by, ifbody, unless, where, body->beginLoc(), p->endLoc());													\
		}

		
	#define LAW_CONSTRAINT_FORM(law, kw, body, after, unless, where, p, static, dynamic, class)														\
		law = NULL;																																	\
		ref_ptr<Element> body_ptr = body, after_ptr = after, unless_ptr = unless, where_ptr = where;												\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		Language::Feature::type feature = (after ? dynamic : static);																				\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, (kw ? &kw_ptr->beginLoc() : &body_ptr->beginLoc()));													\
			YYERROR;																																\
		} else {																																	\
			law = new class(body, after, unless, where, (kw ? kw_ptr->beginLoc() : body_ptr->beginLoc()), p_ptr->endLoc());							\
		}

	#define LAW_DYNAMIC_CONSTRAINT_FORM(law, kw, body, ifbody, unless, where, p, feature, class)													\
		law = NULL;																																	\
		ref_ptr<Element> body_ptr = body, if_ptr = ifbody, unless_ptr = unless, where_ptr = where;													\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, (kw ? &kw_ptr->beginLoc() : &body_ptr->beginLoc()));													\
			YYERROR;																																\
		} else {																																	\
			law = new class(body, ifbody, unless, where, (kw ? kw_ptr->beginLoc() : body_ptr->beginLoc()), p_ptr->endLoc());						\
		}
	
	#define LAW_SIMPLE_FORM(law, kw, head, where, p, feature, class)																				\
		law = NULL;																																	\
		ref_ptr<Element> head_ptr = head, where_ptr = where;																						\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, (kw ? &kw_ptr->beginLoc() : &head_ptr->beginLoc()));													\
			YYERROR;																																\
		} else {																																	\
			law = new class(head, where, (kw ? kw_ptr->beginLoc() : head_ptr->beginLoc()), p_ptr->endLoc());										\
		}
		

}



law_basic(law) 			::= head_formula(head) clause_if(ifbody) clause_ifcons(ifcons) clause_after(after) clause_unless(unless) clause_where(where) PERIOD(p).						{ LAW_BASIC_FORM(law, NULL, head, ifbody, ifcons, after, 
																																														unless, where, p, Language::Feature::LAW_BASIC_S, 
																																															Language::Feature::LAW_BASIC_D, BasicLaw); }

law_caused(law)			::= CAUSED(kw) head_formula(head) clause_if(ifbody) clause_ifcons(ifcons) clause_after(after) clause_unless(unless) clause_where(where) PERIOD(p).			{ LAW_BASIC_FORM(law, kw, head, ifbody, ifcons, after, 
																																														unless, where, p, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }

law_pcaused(law) 		::= POSSIBLY_CAUSED(kw) head_formula(head) clause_if(ifbody) clause_ifcons(ifcons) clause_after(after) clause_unless(unless) clause_where(where) PERIOD(p).	{ LAW_BASIC_FORM(law, kw, head, ifbody, ifcons, after, 
																																														unless, where, p, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }

law_impl(law)			::= head_formula(head) ARROW_LDASH(kw) formula(body) clause_where(where) PERIOD(p).																			{ LAW_IMPL_FORM(law, head, kw, body, where, p, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }

law_causes(law)			::= atomic_formula(body) CAUSES(kw) head_formula(head) clause_if(ifbody) clause_unless(unless) clause_where(where) PERIOD(p).								{ LAW_DYNAMIC_FORM(law, body, kw, head, ifbody, unless, where, p,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }


law_increments(law)		::= atomic_formula(body) INCREMENTS(kw) constant(head) BY term(v) clause_if(ifbody) clause_unless(unless) clause_where(where) PERIOD(p).					{ LAW_INCREMENTAL_FORM(law, body, kw, head, v, ifbody, unless, where, p,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw); }


law_mcause(law)			::= atomic_formula(body) MAY_CAUSE(kw) head_formula(head) clause_if(ifbody) clause_unless(unless) clause_where(where) PERIOD(p).							{ LAW_DYNAMIC_FORM(law, body, kw, head, ifbody, unless, where, p,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }


law_always(law)			::= ALWAYS(kw) formula(body) clause_after(after) clause_unless(unless) clause_where(where) PERIOD(p).														{ LAW_CONSTRAINT_FORM(law, kw, body, after, unless, where, p,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }

law_constraint(law)		::= CONSTRAINT(kw) formula(body) clause_after(after) clause_unless(unless) clause_where(where) PERIOD(p).													{ LAW_CONSTRAINT_FORM(law, kw, body, after, unless, where, p,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }

law_impossible(law) 	::= IMPOSSIBLE(kw) formula(body) clause_after(after) clause_unless(unless) clause_where(where) PERIOD(p).													{ LAW_CONSTRAINT_FORM(law, kw, body, after, unless, where, p,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }

law_never(law) 			::= NEVER(kw) formula(body) clause_after(after) clause_unless(unless) clause_where(where) PERIOD(p).														{ LAW_CONSTRAINT_FORM(law, kw, body, after, unless, where, p,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }

law_default(law) 		::= DEFAULT(kw) atomic_formula(head) clause_if(ifbody) clause_ifcons(ifcons) clause_after(after) clause_unless(unless) clause_where(where) PERIOD(p).		{ LAW_BASIC_FORM(law, kw, head, ifbody, ifcons, after, 
																																														unless, where, p, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }

law_exogenous(law)		::= EXOGENOUS(kw) constant(head) clause_if(ifbody) clause_ifcons(ifcons) clause_after(after) clause_unless(unless) clause_where(where) PERIOD(p).			{ LAW_BASIC_FORM(law, kw, head, ifbody, ifcons, after, 
																																														unless, where, p, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }

law_inertial(law) 		::= INERTIAL(kw) constant(head) clause_if(ifbody) clause_ifcons(ifcons) clause_after(after) clause_unless(unless) clause_where(where) PERIOD(p).			{ LAW_BASIC_FORM(law, kw, head, ifbody, ifcons, after, 
																																														unless, where, p, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }

law_nonexecutable(law) 	::= NONEXECUTABLE(kw) formula(body) clause_if(ifbody) clause_unless(unless) clause_where(where) PERIOD(p).													{ LAW_DYNAMIC_CONSTRAINT_FORM(law, kw, body, ifbody, unless, where,
																																														p, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }


law_rigid(law) 			::= RIGID(kw) constant(head) clause_where(where) PERIOD(p).																									{ LAW_SIMPLE_FORM(law, kw, head, where, p,
																																														Language::Feature::LAW_RIGID, RigidLaw); }


law_observed(law) 		::= OBSERVED(kw) atomic_formula(head) AT term_no_const(t) PERIOD(p).																						
		{ 
			law = NULL;
			ref_ptr<const Token> kw_ptr = kw, p_ptr = p;
			ref_ptr<AtomicFormula> head_ptr = head;
			ref_ptr<Term> t_ptr = t;

			// make sure that the At clause is integral
			if (t->domainType() != DomainType::INTEGRAL) {
				parser->_parse_error("Expected an integral expression.", &t->beginLoc());
				YYERROR;
			} else {
				LAW_SIMPLE_FORM(law, kw, head, t, p, Language::Feature::LAW_OBSERVED, ObservedLaw); 
			}
		}

/********************************************************************************************************************************/
/*************************************************************************************************/
/* Code Blocks */
/*************************************************************************************************/
/********************************************************************************************************************************/

%include {
	#define CODE_BLK(stmt, code, feature, type) 												\
		ref_ptr<const Token> code_ptr = code;													\
		if (!parser->lang()->support(feature)) {												\
			stmt = NULL;																		\
			parser->_feature_error(feature, &code->beginLoc());									\
			YYERROR;																			\
		} else {																				\
			stmt = new type(code, code->beginLoc(), code->endLoc());							\
		}
}


stmt_code_blk(stmt) ::= ASP_GR(code). 				{ CODE_BLK(stmt, code, Language::Feature::CODE_ASP_GR, ASPBlock);	}
stmt_code_blk(stmt) ::= ASP_CP(code).				{ CODE_BLK(stmt, code, Language::Feature::CODE_ASP_CP, ASPBlock);	}
stmt_code_blk(stmt) ::= F2LP_GR(code).				{ CODE_BLK(stmt, code, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
stmt_code_blk(stmt) ::= F2LP_CP(code).				{ CODE_BLK(stmt, code, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
stmt_code_blk(stmt) ::= LUA_GR(code).				{ CODE_BLK(stmt, code, Language::Feature::CODE_LUA_GR, LUABlock);   }
stmt_code_blk(stmt) ::= LUA_CP(code).				{ CODE_BLK(stmt, code, Language::Feature::CODE_LUA_CP, LUABlock);   }
