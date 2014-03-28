/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is included that follows the "include" declaration
** in the input grammar file. */
#include <stdio.h>
#line 1 "bcplus/parser/detail/lemon_parser.y"

			#include <cassert>
			#include <cstring>

			#include <boost/foreach.hpp>

			#include "babb/utils/memory.h"

			#include "bcplus/Location.h"
			#include "bcplus/parser/BCParser.h"
			#include "bcplus/parser/Token.h"
			#include "bcplus/parser/detail/lemon_parser.h"
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

			#define UNUSED void*

			using namespace bcplus;
			using namespace babb::utils;
			using namespace bcplus::parser;
			using namespace bcplus::statements;
			using namespace bcplus::elements;
			using namespace bcplus::languages;
			using namespace bcplus::symbols;
			

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

		
#line 309 "bcplus/parser/detail/lemon_parser.y"

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

#line 483 "bcplus/parser/detail/lemon_parser.y"

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

#line 714 "bcplus/parser/detail/lemon_parser.y"

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

#line 794 "bcplus/parser/detail/lemon_parser.y"


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
	


#line 870 "bcplus/parser/detail/lemon_parser.y"

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

#line 951 "bcplus/parser/detail/lemon_parser.y"

	#define CARD_FORMULA(card, min, lbrack, af, binds, rbrack, max)																	\
		card = NULL;																												\
		ref_ptr<const Term> min_ptr = min;																							\
		ref_ptr<const Token> lbrack_ptr = lbrack;																					\
		ref_ptr<AtomicFormula> af_ptr = af;																							\
		ref_ptr<CardinalityFormula::BindingList> binds_ptr = binds;																	\
		ref_ptr<const Token> rbrack_ptr = rbrack;																					\
		ref_ptr<const Term> max_ptr = max;																							\
																																	\
		/* TODO: Check bindings */																									\
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
			card = new CardinalityFormula(af, binds, min, max, 																	\
				(min ? min_ptr->beginLoc() : lbrack_ptr->beginLoc()), 															\
				(max ? max_ptr->endLoc() : rbrack_ptr->endLoc()));																\
		}																														\



#line 1170 "bcplus/parser/detail/lemon_parser.y"

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
				sort->addSubsort(subsort);																															\
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
#line 1843 "bcplus/parser/detail/lemon_parser.y"

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

#line 1866 "bcplus/parser/detail/lemon_parser.y"

	#define VALUE_DECL(stmt, cd, kw, val, p, feature, class)										\
		stmt = NULL;																				\
		ref_ptr<const Token> cd_ptr = cd, kw_ptr = kw, val_ptr = val, p_ptr = p;					\
																									\
		if (!parser->lang()->support(feature)) {													\
			parser->_feature_error(feature, &kw->beginLoc());										\
			YYERROR;																				\
		} else { 																					\
			int value;																				\
			/* Ensure val is a positive integer */													\
			if (sscanf(p->str()->c_str(), "%d", &value) != 1) {										\
				parser->_parse_error("INTERNAL ERROR: Could not extract an integer from \"" 		\
					+ *val->str() + "\".", &val->beginLoc());										\
				YYERROR;																			\
			} else {																				\
				if (value < 0) {																	\
					parser->_parse_error("ERROR: Expected a positive integer.", &val->beginLoc());	\
				} else {																			\
					stmt = new class(value, cd->beginLoc(), p->endLoc());							\
				}																					\
			}																						\
		}
#line 1900 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		Token const* maxstep;
		Token const* label;
	};

#line 2025 "bcplus/parser/detail/lemon_parser.y"

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
#line 2063 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2146 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2329 "bcplus/parser/detail/lemon_parser.y"

	#define CODE_BLK(stmt, code, feature, type) 												\
		ref_ptr<const Token> code_ptr = code;													\
		if (!parser->lang()->support(feature)) {												\
			stmt = NULL;																		\
			parser->_feature_error(feature, &code->beginLoc());									\
			YYERROR;																			\
		} else {																				\
			stmt = new type(code, code->beginLoc(), code->endLoc());							\
		}
#line 478 "bcplus/parser/detail/lemon_parser.c"
/* Next is all token values, in a form suitable for use by makeheaders.
** This section will be null unless lemon is run with the -m switch.
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
/* Make sure the INTERFACE macro is defined.
*/
#ifndef INTERFACE
# define INTERFACE 1
#endif
/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 terminals
**                       and nonterminals.  "int" is used otherwise.
**    YYNOCODE           is a number of type YYCODETYPE which corresponds
**                       to no legal terminal or nonterminal number.  This
**                       number is used to fill in empty slots of the hash 
**                       table.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       have fall-back values which should be used if the
**                       original value of the token will not parse.
**    YYACTIONTYPE       is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 rules and
**                       states combined.  "int" is used otherwise.
**    lemon_parserTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is lemon_parserTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    lemon_parserARG_SDECL     A static variable declaration for the %extra_argument
**    lemon_parserARG_PDECL     A parameter declaration for the %extra_argument
**    lemon_parserARG_STORE     Code to store %extra_argument into yypParser
**    lemon_parserARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
#define YYCODETYPE unsigned char
#define YYNOCODE 241
#define YYACTIONTYPE unsigned short int
#define lemon_parserTOKENTYPE  Token const* 								
typedef union {
  int yyinit;
  lemon_parserTOKENTYPE yy0;
  Variable* yy13;
  SortDeclaration* yy45;
  MacroDeclaration::ElementList* yy49;
  MacroSymbol::ArgumentList* yy50;
  NCStatement* yy57;
  QuantifierFormula::Operator::type yy81;
  Constant* yy97;
  QuantifierFormula* yy101;
  MacroDeclaration* yy111;
  ConstantDeclaration::ElementList* yy145;
  MacroSymbol* yy163;
  QueryStatement* yy170;
  ConstantDeclaration* yy175;
  QueryData yy181;
  AtomicFormula* yy202;
  Token const* yy203;
  TokenList* yy208;
  ObjectDeclaration::Element::ObjectList* yy237;
  CardinalityFormula::BindingList* yy240;
  SortSymbol* yy265;
  VariableDeclaration* yy267;
  Statement* yy280;
  ObjectDeclaration* yy288;
  SortDeclaration::ElementList* yy295;
  StrongNCStatement* yy298;
  Term* yy307;
  ConstantSymbol::SortList* yy315;
  QuantifierFormula::QuantifierList* yy317;
  VariableDeclaration::ElementList* yy318;
  LocalVariable* yy356;
  Formula* yy393;
  ShowStatement::ElementList* yy395;
  UNUSED yy433;
  LuaTerm* yy434;
  Object* yy438;
  ObjectDeclaration::ElementList* yy454;
  TermList* yy467;
  ConstantSymbol::Type::type yy469;
  ObjectDeclaration::Element* yy470;
  IdentifierDeclList* yy474;
  int yy481;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define lemon_parserARG_SDECL  BCParser* parser						;
#define lemon_parserARG_PDECL , BCParser* parser						
#define lemon_parserARG_FETCH  BCParser* parser						 = yypParser->parser						
#define lemon_parserARG_STORE yypParser->parser						 = parser						
#define YYNSTATE 694
#define YYNRULE 363
#define YYERRORSYMBOL 132
#define YYERRSYMDT yy481
#define YYFALLBACK 1
#define YY_NO_ACTION      (YYNSTATE+YYNRULE+2)
#define YY_ACCEPT_ACTION  (YYNSTATE+YYNRULE+1)
#define YY_ERROR_ACTION   (YYNSTATE+YYNRULE)

/* The yyzerominor constant is used to initialize instances of
** YYMINORTYPE objects to zero. */
static const YYMINORTYPE yyzerominor = { 0 };

/* Define the yytestcase() macro to be a no-op if is not already defined
** otherwise.
**
** Applications can choose to define yytestcase() in the %include section
** to a macro that can assist in verifying code coverage.  For production
** code the yytestcase() macro should be turned off.  But it is useful
** for testing.
*/
#ifndef yytestcase
# define yytestcase(X)
#endif


/* Next are the tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == YYNSTATE+YYNRULE              A syntax error has occurred.
**
**   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
**
**   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as
**
**      yy_action[ yy_shift_ofst[S] + X ]
**
** If the index value yy_shift_ofst[S]+X is out of range or if the value
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
** is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
** and that yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
**
**  yy_action[]        A single table containing all actions.
**  yy_lookahead[]     A table containing the lookahead for each entry in
**                     yy_action.  Used to detect hash collisions.
**  yy_shift_ofst[]    For each state, the offset into yy_action for
**                     shifting terminals.
**  yy_reduce_ofst[]   For each state, the offset into yy_action for
**                     shifting non-terminals after a reduce.
**  yy_default[]       Default action for each state.
*/
#define YY_ACTTAB_COUNT (2525)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   693,  159,  160,  692,  691,  690,  689,  688,  687,  686,
 /*    10 */   685,  684,  683,  682,  681,  680,  679,  678,  644,  654,
 /*    20 */   251,  677,  676,  675,  590,  589,  636,  646,  647,  645,
 /*    30 */   643,  642,   24,  370,  311,  105,  103,  102,  166,  249,
 /*    40 */   252,  651,  255,  602,  261,  260,  597,  259,  166,   20,
 /*    50 */    96,   95,   94,  115,   93,   92,   91,   90,  178,   20,
 /*    60 */    96,   95,   94,  153,   93,   92,   91,   90,  694,  654,
 /*    70 */   308,  677,  676,  675,  101,  100,   99,   98,   97,  109,
 /*    80 */   107,  105,  103,  102,  101,  100,   99,   98,   97,  307,
 /*    90 */   309,  469,  468,  467,  466,  465,  464,  463,  462,  461,
 /*   100 */   460,  459,  458,  457,  456,  455,  454,  453,  653,   55,
 /*   110 */    53,   52,  429,  652,  438,  437,  434,  433,  436,  435,
 /*   120 */   653,  644,   29,   28,  429,  652,   30,  543,  480,  636,
 /*   130 */   646,  647,  645,  643,  642,    8,  369,  311,   37,   35,
 /*   140 */   241,    6,    7,   63,  157,  672,  197,  228,  234,   45,
 /*   150 */    44,  191,   36,   46,   62,  621,  620,  622,    5,  471,
 /*   160 */   470,    4,  141,   34,  183,  388,  671,  621,  620,  389,
 /*   170 */   257,   61,  258,  531,  165,  110,  254,  164,   40,   33,
 /*   180 */   366,  607,  594,  240,  595,  410,  165,  609,  408,  164,
 /*   190 */   428,   21,  608,  653,  162,  240,  110,  429,  652,  405,
 /*   200 */    27,   26,   25,   29,   28,  227,  669,   30,  357,  508,
 /*   210 */   356,  509,   31,   23,   26,   25,   29,   28,   89,  411,
 /*   220 */    30,  432,  226,  534,   18,   19,   42,   41,   45,   44,
 /*   230 */    89,  179,   46,  431,  670,  430,  110,  641,  156,  400,
 /*   240 */   621,  620,  622,  168,  499,  431,  670,  430,  404,  654,
 /*   250 */   251,  677,  676,  675,  495,  594,  479,  595,  399,  165,
 /*   260 */    39,  653,  164,  493,   33,  429,  652,   60,  240,  246,
 /*   270 */   252,  159,  160,  485,  599,  598,  597,  596,  520,  644,
 /*   280 */   400,  640,  269,  148,  359,  366,  607,  636,  646,  647,
 /*   290 */   645,  643,  642,  494,  368,  311,  112,   31,   32,   13,
 /*   300 */    12,   16,   15,   89,  493,   17,  153,  225,  621,  620,
 /*   310 */   622,   60,  343,  473,  472,  361,  516,  360,  431,  670,
 /*   320 */   430,  425,  497,  594,  924,  595,  510,  165,  366,  607,
 /*   330 */   164,  868,   21,  346,  491,   38,  240,  166,  515,  924,
 /*   340 */   924,  868,  653,  475,  474,  868,  429,  652,  240,   96,
 /*   350 */    95,   94,  119,   93,   92,   91,   90,  401,  150,  924,
 /*   360 */   924,  507,  520,   16,   15,   18,   19,   17,  358,  366,
 /*   370 */   607,   89,  924,  101,  100,   99,   98,   97,  924,  501,
 /*   380 */   635,  167,  639,  638,  426,  634,  431,  670,  430,  621,
 /*   390 */   620,  622,  400,  868,  868,  868,  868,  868,  431,  161,
 /*   400 */   868,  520,  610,  637,  581,  478,  582,  355,  366,  607,
 /*   410 */    51,  164,  490,  114,   30,  578,  493,  240,  166,  429,
 /*   420 */   577,  651,  135,  133,  132,    3,  225,  629,  628,  630,
 /*   430 */    96,   95,   94,   60,   93,   92,   91,   90,  514,  356,
 /*   440 */   509,  148,  631,  181,  632,  345,  491,  111,  354,  502,
 /*   450 */   353,   58,   89,  592,  101,  100,   99,   98,   97,  352,
 /*   460 */   348,  403,  559,  558,  560,  639,  638,  431,  670,  430,
 /*   470 */    72,  654,  251,  677,  676,  675,  401,  549,   22,  550,
 /*   480 */    27,   26,   25,   29,   28,   56,   49,   30,  398,  605,
 /*   490 */    54,  246,  252,  409,  413,  485,  599,  598,  597,  596,
 /*   500 */   366,  607,  519,   46,  268,  428,  670,  427,  667,  366,
 /*   510 */   607,   64,  429,  666,  538,  539,  546,    9,  518,   47,
 /*   520 */    48,  586,  517,  511,  180,  120,  668,  673,  674,  677,
 /*   530 */   676,  675,  545,  373,  320,  477,  476,  225,  423,   10,
 /*   540 */   431,  670,  430,   27,   26,   25,   29,   28,  151,  576,
 /*   550 */    30,  544,  151,  151,  541,  661,  660,  662,  127,  126,
 /*   560 */   125,  243,  124,  123,  122,  121,  547,  412,  364,  535,
 /*   570 */   663,  152,  664,  654,  251,  677,  676,  675,  536,  108,
 /*   580 */   533,  360,  140,  131,  130,  129,  128,  401,   43,   42,
 /*   590 */    41,   45,   44,  247,  252,   46,  263,  602,  599,  598,
 /*   600 */   597,  596,  423,  654,  253,  677,  676,  675,   27,   26,
 /*   610 */    25,   29,   28,  106,  532,   30,  606,   22,  104,   50,
 /*   620 */   506,  353,  407,  603,  252,  242,  587,  602,  599,  598,
 /*   630 */   597,  596,  147,  431,  670,  430,  498,  654,  251,  677,
 /*   640 */   676,  675,  177,  238,  590,  589,  504,  654,  251,  677,
 /*   650 */   676,  675,   27,   26,   25,   29,   28,  249,  252,   30,
 /*   660 */   601,  602,  599,  598,  597,  596,  402,  249,  252,  500,
 /*   670 */   600,  602,  599,  598,  597,  596,  492,  654,  251,  677,
 /*   680 */   676,  675,  109,  107,  105,  103,  102,  654,  251,  677,
 /*   690 */   676,  675,   27,   26,   25,   29,   28,  249,  252,   30,
 /*   700 */   422,  602,  599,  598,  597,  596,  158,  249,  252,  481,
 /*   710 */   421,  602,  599,  598,  597,  596,  654,  251,  677,  676,
 /*   720 */   675,  146,  420,  362,  537,  654,  251,  677,  676,  675,
 /*   730 */   489,   14,   13,   12,   16,   15,  249,  252,   17,  291,
 /*   740 */   602,  599,  598,  597,  596,  249,  252,  580,  331,  602,
 /*   750 */   599,  598,  597,  596,  144,  654,  251,  677,  676,  675,
 /*   760 */    22,  351,  403, 1058,    1,  654,  251,  677,  676,  675,
 /*   770 */    43,   42,   41,   45,   44,  249,  252,   46,  330,  602,
 /*   780 */   599,  598,  597,  596,  116,  249,  252,  118,  264,  602,
 /*   790 */   599,  598,  597,  596,  350,  403,  349,  403,  396,  654,
 /*   800 */   244,  677,  676,  675,  323,  403,  420,  395,  488,  654,
 /*   810 */   251,  677,  676,  675,   27,   26,   25,   29,   28,  603,
 /*   820 */   252,   30,  587,  602,  599,  598,  597,  596,  393,  249,
 /*   830 */   252,  202,  273,  602,  599,  598,  597,  596,  392,  654,
 /*   840 */   251,  677,  676,  675,   59,   57,   55,   53,   52,  654,
 /*   850 */   251,  677,  676,  675,   14,   13,   12,   16,   15,  245,
 /*   860 */   252,   17,  487,  485,  599,  598,  597,  596,   17,  246,
 /*   870 */   252,  344,  256,  485,  599,  598,  597,  596,  342,  452,
 /*   880 */   117,  420,  270,  654,  308,  677,  676,  675,  654,  253,
 /*   890 */   677,  676,  675,   83,   82,   81,   80,   79,  654,  251,
 /*   900 */   677,  676,  675,  275,  309,  439,  374,  451,  486,  252,
 /*   910 */   450,  367,  485,  599,  598,  597,  596,  449,  246,  252,
 /*   920 */   448,  482,  485,  599,  598,  597,  596,  447,  446,  445,
 /*   930 */   444,  484,  654,  251,  677,  676,  675,  139,  137,  135,
 /*   940 */   133,  132,  654,  251,  677,  676,  675,  604,  665,  365,
 /*   950 */   443,  593,  246,  252,  442,  428,  485,  599,  598,  597,
 /*   960 */   596,  441,  246,  252,  440,  483,  485,  599,  598,  597,
 /*   970 */   596,  431,  591,  588,  670,  391,  654,  251,  677,  676,
 /*   980 */   675,  109,  107,  105,  103,  102,  654,  251,  677,  676,
 /*   990 */   675,   88,   87,   86,   85,   84,  246,  252,  236,   22,
 /*  1000 */   485,  599,  598,  597,  596,  235,  246,  252,  163,  390,
 /*  1010 */   485,  599,  598,  597,  596,  417,   50,  416,  232,  278,
 /*  1020 */   415,  654,  251,  677,  676,  675,  230,  644,  579,  557,
 /*  1030 */   677,  676,  675,  633,  229,  636,  646,  647,  645,  643,
 /*  1040 */   642,  246,  252,  310,  414,  485,  599,  598,  597,  596,
 /*  1050 */   250,  542,  363,  605,  322,  513,  496,  271,  554,  551,
 /*  1060 */   654,  251,  677,  676,  675,  505,   59,   57,   55,   53,
 /*  1070 */    52,  654,  251,  677,  676,  675,  651,  401,   11,  222,
 /*  1080 */   246,  252,  223,   10,  485,  599,  598,  597,  596,  387,
 /*  1090 */   221,  249,  252,  321,  267,  602,  599,  598,  597,  596,
 /*  1100 */   219,  218,  217,  212,  654,  251,  677,  676,  675,  101,
 /*  1110 */   100,   99,   98,   97,  654,  251,  677,  676,  675,  216,
 /*  1120 */   572,  385,  214,  213,  249,  252,  211,  266,  602,  599,
 /*  1130 */   598,  597,  596,  384,  249,  252,  209,  175,  602,  599,
 /*  1140 */   598,  597,  596,  383,  207,  201,  382,  205,  654,  251,
 /*  1150 */   677,  676,  675,  139,  137,  135,  133,  132,  654,  251,
 /*  1160 */   677,  676,  675,  381,  203,  200,  380,  199,  249,  252,
 /*  1170 */   198,  174,  602,  599,  598,  597,  596,  379,  249,  252,
 /*  1180 */   196,  173,  602,  599,  598,  597,  596,  924,  575,  569,
 /*  1190 */   677,  676,  675,  654,  251,  677,  676,  675,  329,  195,
 /*  1200 */   194,  193,  924,  924,  192,  190,  654,  251,  677,  676,
 /*  1210 */   675,  378,  189,  249,  252,  188,  172,  602,  599,  598,
 /*  1220 */   597,  596,  924,  924,  187,  186,  249,  252,  377,  171,
 /*  1230 */   602,  599,  598,  597,  596,  924,  654,  251,  677,  676,
 /*  1240 */   675,  771,  771,  771,  184,  771,  771,  771,  771,  376,
 /*  1250 */   654,  617,  677,  676,  675,  182,  249,  252,  424,  170,
 /*  1260 */   602,  599,  598,  597,  596,  771,  771,  771,  771,  771,
 /*  1270 */   619,  309,  540,  512,  375,  239,   71,   70,   69,  224,
 /*  1280 */    68,   67,   66,   65,  233,  386,  210,  406,  208,   72,
 /*  1290 */    78,   77,  206,   76,   75,   74,   73,  503,  204,  185,
 /*  1300 */    83,   82,   81,   80,   79, 1059,  574, 1059, 1059, 1059,
 /*  1310 */   429,  573, 1059,   88,   87,   86,   85,   84,  605,  127,
 /*  1320 */   126,  125, 1059,  124,  123,  122,  121, 1059,  579,  557,
 /*  1330 */   677,  676,  675,  101,  100,   99,   98,   97,  530,  529,
 /*  1340 */   528,  527, 1059,  140,  131,  130,  129,  128,  653, 1059,
 /*  1350 */   248, 1059,  429,  652,  526, 1059,  524,  262,  554,  551,
 /*  1360 */  1059,  523,  654,  251,  677,  676,  675, 1059,  570, 1059,
 /*  1370 */   571, 1059, 1059,  522, 1059,  521,  525,  138, 1059, 1059,
 /*  1380 */  1059, 1059,  249,  252, 1059, 1059, 1059,  585,  584, 1059,
 /*  1390 */   583, 1059, 1059, 1059, 1059,  621,  620,  622, 1059, 1059,
 /*  1400 */  1059,  237,  155, 1059,  169,  145,    2,  143,  149,  154,
 /*  1410 */  1059,  136, 1059, 1059, 1059, 1059,  134, 1059, 1059,  114,
 /*  1420 */  1059,  654,  251,  677,  676,  675,  139,  137,  135,  133,
 /*  1430 */   132,  431,  670,  430, 1059, 1059,  394,  397, 1059, 1059,
 /*  1440 */  1059,  249,  252, 1059, 1059,  606,  585,  584, 1059,  583,
 /*  1450 */  1059, 1059, 1059,  113, 1059, 1059, 1059, 1059,   89, 1059,
 /*  1460 */   231, 1059,  654,  251,  677,  676,  675, 1059, 1059, 1059,
 /*  1470 */  1059, 1059, 1059,  431,  670,  430, 1059,  654,  251,  677,
 /*  1480 */   676,  675,  249,  252, 1059, 1059, 1059,  585,  584, 1059,
 /*  1490 */   583,  579,  557,  677,  676,  675, 1059,  249,  252, 1059,
 /*  1500 */  1059,  220,  585,  584, 1059,  583,  579,  557,  677,  676,
 /*  1510 */   675, 1059, 1059,  556, 1059, 1059,  215, 1059,  142, 1059,
 /*  1520 */   548,  554,  551, 1059, 1059, 1059, 1059, 1059,  250,  579,
 /*  1530 */   557,  677,  676,  675, 1059,  553,  554,  551, 1059, 1059,
 /*  1540 */   579,  557,  677,  676,  675, 1059, 1059, 1059, 1059, 1059,
 /*  1550 */  1059,  250, 1059,  579,  557,  677,  676,  675,  552,  554,
 /*  1560 */   551, 1059,  250,  579,  557,  677,  676,  675, 1059,  419,
 /*  1570 */   554,  551, 1059, 1059, 1059,  250,  579,  557,  677,  676,
 /*  1580 */   675, 1059,  418,  554,  551,  250, 1059, 1059, 1059, 1059,
 /*  1590 */  1059, 1059,  282,  554,  551, 1059, 1059, 1059,  250,  579,
 /*  1600 */   557,  677,  676,  675, 1059,  325,  554,  551, 1059,  668,
 /*  1610 */   673,  674,  677,  676,  675, 1059,  372,  320, 1059, 1059,
 /*  1620 */  1059,  250, 1059, 1059, 1059, 1059, 1059, 1059,  324,  554,
 /*  1630 */   551,  668,  673,  674,  677,  676,  675,  644,  371,  320,
 /*  1640 */  1059, 1059, 1059, 1059, 1059,  636,  646,  647,  645,  643,
 /*  1650 */   642, 1059,  644,  336,  654,  308,  677,  676,  675, 1059,
 /*  1660 */   636,  646,  647,  645,  643,  642, 1059, 1059,  276, 1059,
 /*  1670 */  1059,  644, 1059, 1059,  290,  309, 1059, 1059, 1059,  636,
 /*  1680 */   646,  647,  645,  643,  642,  644, 1059,  337, 1059, 1059,
 /*  1690 */  1059, 1059, 1059,  636,  646,  647,  645,  643,  642,  644,
 /*  1700 */  1059,  627, 1059, 1059, 1059, 1059, 1059,  636,  646,  647,
 /*  1710 */   645,  643,  642,  644, 1059,  623,  654,  274,  677,  676,
 /*  1720 */   675,  636,  646,  647,  645,  643,  642, 1059, 1059,  626,
 /*  1730 */   644, 1059, 1059, 1059, 1059, 1059,  619,  309,  636,  646,
 /*  1740 */   647,  645,  643,  642, 1059, 1059,  625,  644, 1059,  919,
 /*  1750 */  1059,  920, 1059, 1059, 1059,  636,  646,  647,  645,  643,
 /*  1760 */   642,  644, 1059,  624,  919,  919,  920,  920, 1059,  636,
 /*  1770 */   646,  647,  645,  643,  642, 1059,  921,  306,  922, 1059,
 /*  1780 */  1059, 1059, 1059, 1059,  919,  919,  920,  920, 1059, 1059,
 /*  1790 */  1059,  921,  921,  922,  922, 1059, 1059,  919, 1059,  920,
 /*  1800 */   923, 1059, 1059,  919, 1059,  920, 1059, 1059, 1059, 1059,
 /*  1810 */  1059,  921,  921,  922,  922,  923,  923,  575,  569,  677,
 /*  1820 */   676,  675, 1059, 1059,  921, 1059,  922,  327, 1059, 1059,
 /*  1830 */   921, 1059,  922, 1059, 1059,  923,  923,  575,  569,  677,
 /*  1840 */   676,  675, 1059, 1059, 1059, 1059,  347,  280,  923, 1059,
 /*  1850 */  1059, 1059, 1059, 1059,  923, 1059, 1059, 1059,  668,  673,
 /*  1860 */   674,  677,  676,  675, 1059, 1059,  319,  668,  673,  674,
 /*  1870 */   677,  676,  675, 1059, 1059,  340,  668,  673,  674,  677,
 /*  1880 */   676,  675, 1059, 1059,  277, 1059, 1059,  668,  673,  674,
 /*  1890 */   677,  676,  675, 1059, 1059,  341, 1059,  668,  673,  674,
 /*  1900 */   677,  676,  675, 1059, 1059,  659,  668,  673,  674,  677,
 /*  1910 */   676,  675, 1059, 1059,  655,  668,  673,  674,  677,  676,
 /*  1920 */   675, 1059, 1059,  658, 1059,  668,  673,  674,  677,  676,
 /*  1930 */   675, 1059, 1059,  657, 1059,  668,  673,  674,  677,  676,
 /*  1940 */   675, 1059, 1059,  656,  668,  673,  674,  677,  676,  675,
 /*  1950 */  1059, 1059,  339, 1059,  668,  673,  674,  677,  676,  675,
 /*  1960 */  1059, 1059,  338,  668,  673,  674,  677,  676,  675, 1059,
 /*  1970 */  1059,  650, 1059, 1059, 1059,  668,  673,  674,  677,  676,
 /*  1980 */   675, 1059, 1059,  649, 1059, 1059,  668,  673,  674,  677,
 /*  1990 */   676,  675, 1059, 1059,  648, 1059, 1059,  668,  673,  674,
 /*  2000 */   677,  676,  675, 1059, 1059,  318, 1059, 1059, 1059,  668,
 /*  2010 */   673,  674,  677,  676,  675, 1059, 1059,  317,  668,  673,
 /*  2020 */   674,  677,  676,  675, 1059, 1059,  316,  668,  673,  674,
 /*  2030 */   677,  676,  675, 1059, 1059,  315, 1059, 1059,  668,  673,
 /*  2040 */   674,  677,  676,  675, 1059, 1059,  314, 1059,  668,  673,
 /*  2050 */   674,  677,  676,  675, 1059, 1059,  313,  668,  673,  674,
 /*  2060 */   677,  676,  675, 1059, 1059,  312,  668,  673,  674,  677,
 /*  2070 */   676,  675, 1059, 1059,  618, 1059,  668,  673,  674,  677,
 /*  2080 */   676,  675, 1059, 1059,  335, 1059,  668,  673,  674,  677,
 /*  2090 */   676,  675, 1059, 1059,  334,  668,  673,  674,  677,  676,
 /*  2100 */   675, 1059, 1059,  616, 1059,  668,  673,  674,  677,  676,
 /*  2110 */   675, 1059, 1059,  615,  668,  673,  674,  677,  676,  675,
 /*  2120 */  1059, 1059,  614, 1059, 1059, 1059,  668,  673,  674,  677,
 /*  2130 */   676,  675, 1059, 1059,  333, 1059, 1059,  668,  673,  674,
 /*  2140 */   677,  676,  675, 1059, 1059,  332, 1059, 1059,  668,  673,
 /*  2150 */   674,  677,  676,  675, 1059, 1059,  613, 1059, 1059, 1059,
 /*  2160 */   668,  673,  674,  677,  676,  675, 1059, 1059,  612,  668,
 /*  2170 */   673,  674,  677,  676,  675, 1059, 1059,  611,  668,  673,
 /*  2180 */   674,  677,  676,  675, 1059, 1059,  305, 1059, 1059,  668,
 /*  2190 */   673,  674,  677,  676,  675, 1059, 1059,  304, 1059,  668,
 /*  2200 */   673,  674,  677,  676,  675, 1059, 1059,  303,  668,  673,
 /*  2210 */   674,  677,  676,  675, 1059, 1059,  302,  668,  673,  674,
 /*  2220 */   677,  676,  675, 1059, 1059,  301, 1059,  668,  673,  674,
 /*  2230 */   677,  676,  675, 1059, 1059,  300, 1059,  668,  673,  674,
 /*  2240 */   677,  676,  675, 1059, 1059,  299,  668,  673,  674,  677,
 /*  2250 */   676,  675, 1059, 1059,  298, 1059,  668,  673,  674,  677,
 /*  2260 */   676,  675, 1059, 1059,  297,  668,  673,  674,  677,  676,
 /*  2270 */   675, 1059, 1059,  296, 1059, 1059, 1059,  668,  673,  674,
 /*  2280 */   677,  676,  675, 1059, 1059,  295, 1059, 1059,  668,  673,
 /*  2290 */   674,  677,  676,  675, 1059, 1059,  294, 1059, 1059,  668,
 /*  2300 */   673,  674,  677,  676,  675, 1059, 1059,  293, 1059, 1059,
 /*  2310 */  1059,  668,  673,  674,  677,  676,  675, 1059, 1059,  292,
 /*  2320 */   668,  673,  674,  677,  676,  675, 1059, 1059,  176,  575,
 /*  2330 */   569,  677,  676,  675,  575,  569,  677,  676,  675,  272,
 /*  2340 */  1059, 1059, 1059, 1059,  328,  575,  569,  677,  676,  675,
 /*  2350 */  1059, 1059, 1059, 1059, 1059,  568,  575,  569,  677,  676,
 /*  2360 */   675,  575,  569,  677,  676,  675,  564, 1059, 1059, 1059,
 /*  2370 */  1059,  567,  575,  569,  677,  676,  675,  575,  569,  677,
 /*  2380 */   676,  675,  566, 1059, 1059, 1059, 1059,  565, 1059,  575,
 /*  2390 */   569,  677,  676,  675,  575,  569,  677,  676,  675,  326,
 /*  2400 */  1059, 1059, 1059, 1059,  563, 1059,  575,  569,  677,  676,
 /*  2410 */   675,  575,  569,  677,  676,  675,  562, 1059, 1059, 1059,
 /*  2420 */  1059,  561,  575,  569,  677,  676,  675, 1059, 1059, 1059,
 /*  2430 */  1059, 1059,  289,  575,  569,  677,  676,  675, 1059, 1059,
 /*  2440 */  1059, 1059, 1059,  288,  575,  569,  677,  676,  675, 1059,
 /*  2450 */  1059, 1059, 1059, 1059,  287,  575,  569,  677,  676,  675,
 /*  2460 */  1059, 1059, 1059, 1059, 1059,  286,  575,  569,  677,  676,
 /*  2470 */   675,  575,  569,  677,  676,  675,  285, 1059, 1059, 1059,
 /*  2480 */  1059,  284,  575,  569,  677,  676,  675,  575,  569,  677,
 /*  2490 */   676,  675,  283, 1059, 1059, 1059, 1059,  555,  575,  569,
 /*  2500 */   677,  676,  675, 1059, 1059, 1059, 1059, 1059,  281,  575,
 /*  2510 */   569,  677,  676,  675,  575,  569,  677,  676,  675,  279,
 /*  2520 */  1059, 1059, 1059, 1059,  265,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   132,  100,  101,  135,  136,  137,  138,  139,  140,  141,
 /*    10 */   142,  143,  144,  145,  146,  147,  148,  149,  154,  151,
 /*    20 */   152,  153,  154,  155,   91,   92,  162,  163,  164,  165,
 /*    30 */   166,  167,   99,  169,  170,  108,  109,  110,   70,  171,
 /*    40 */   172,   73,  174,  175,  176,  177,  178,  179,   70,   81,
 /*    50 */    82,   83,   84,   71,   86,   87,   88,   89,  190,   81,
 /*    60 */    82,   83,   84,   81,   86,   87,   88,   89,    0,  151,
 /*    70 */   152,  153,  154,  155,  106,  107,  108,  109,  110,  106,
 /*    80 */   107,  108,  109,  110,  106,  107,  108,  109,  110,  171,
 /*    90 */   172,  223,  224,  225,  226,  227,  228,  229,  230,  231,
 /*   100 */   232,  233,  234,  235,  236,  237,  238,  239,    1,  108,
 /*   110 */   109,  110,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   120 */     1,  154,   98,   99,    5,    6,  102,   74,    1,  162,
 /*   130 */   163,  164,  165,  166,  167,   28,  169,  170,   33,   32,
 /*   140 */    76,   34,   35,   72,   37,   73,   39,   94,   43,   98,
 /*   150 */    99,   44,   47,  102,   72,   48,   49,   50,   51,   52,
 /*   160 */    53,   54,   77,   56,   57,   46,   73,   48,   49,   50,
 /*   170 */    63,   72,   65,  189,   67,  103,   69,   70,   72,   72,
 /*   180 */   196,  197,   63,   76,   65,  201,   67,  108,  103,   70,
 /*   190 */   126,   72,  113,    1,   72,   76,  103,    5,    6,    2,
 /*   200 */    95,   96,   97,   98,   99,   77,   73,  102,  202,  203,
 /*   210 */   204,  205,  105,  106,   96,   97,   98,   99,  111,   97,
 /*   220 */   102,  114,   77,   74,  105,  106,   96,   97,   98,   99,
 /*   230 */   111,  103,  102,  126,  127,  128,  103,   73,  131,  159,
 /*   240 */    48,   49,   50,   94,   73,  126,  127,  128,  103,  151,
 /*   250 */   152,  153,  154,  155,   74,   63,  129,   65,   27,   67,
 /*   260 */    72,    1,   70,  183,   72,    5,    6,  103,   76,  171,
 /*   270 */   172,  100,  101,  175,  176,  177,  178,  179,  189,  154,
 /*   280 */   159,   73,  184,  103,  195,  196,  197,  162,  163,  164,
 /*   290 */   165,  166,  167,  213,  169,  170,   71,  105,  106,   96,
 /*   300 */    97,   98,   99,  111,  183,  102,   81,   76,   48,   49,
 /*   310 */    50,  103,  214,  215,  216,  198,  199,  200,  126,  127,
 /*   320 */   128,  189,   73,   63,   26,   65,  129,   67,  196,  197,
 /*   330 */    70,   33,   72,  212,  213,   72,   76,   70,   74,   41,
 /*   340 */    42,   43,    1,    1,    2,   47,    5,    6,   76,   82,
 /*   350 */    83,   84,  103,   86,   87,   88,   89,  126,   94,   61,
 /*   360 */    62,   74,  189,   98,   99,  105,  106,  102,  195,  196,
 /*   370 */   197,  111,   74,  106,  107,  108,  109,  110,   80,   74,
 /*   380 */     1,   94,    3,    4,    5,    6,  126,  127,  128,   48,
 /*   390 */    49,   50,  159,   95,   96,   97,   98,   99,  126,   94,
 /*   400 */   102,  189,   73,   73,   63,   74,   65,  195,  196,  197,
 /*   410 */    82,   70,   74,   72,  102,    1,  183,   76,   70,    5,
 /*   420 */     6,   73,  108,  109,  110,   94,   76,   48,   49,   50,
 /*   430 */    82,   83,   84,  103,   86,   87,   88,   89,  203,  204,
 /*   440 */   205,  103,   63,   72,   65,  212,  213,  106,  206,  207,
 /*   450 */   208,   72,  111,   68,  106,  107,  108,  109,  110,  209,
 /*   460 */   210,  211,   48,   49,   50,    3,    4,  126,  127,  128,
 /*   470 */    82,  151,  152,  153,  154,  155,  126,   63,   41,   65,
 /*   480 */    95,   96,   97,   98,   99,  106,   72,  102,   27,    2,
 /*   490 */   111,  171,  172,  189,   73,  175,  176,  177,  178,  179,
 /*   500 */   196,  197,  189,  102,  184,  126,  127,  128,    1,  196,
 /*   510 */   197,   31,    5,    6,    3,    4,   74,   80,   73,  105,
 /*   520 */   106,   73,   73,   73,  103,  111,  150,  151,  152,  153,
 /*   530 */   154,  155,   74,  157,  158,  215,  216,   76,  164,   26,
 /*   540 */   126,  127,  128,   95,   96,   97,   98,   99,  103,   73,
 /*   550 */   102,   74,  103,  103,   75,   48,   49,   50,   82,   83,
 /*   560 */    84,  187,   86,   87,   88,   89,   73,   97,  191,  192,
 /*   570 */    63,   72,   65,  151,  152,  153,  154,  155,   75,   72,
 /*   580 */   199,  200,  106,  107,  108,  109,  110,  126,   95,   96,
 /*   590 */    97,   98,   99,  171,  172,  102,  174,  175,  176,  177,
 /*   600 */   178,  179,  164,  151,  152,  153,  154,  155,   95,   96,
 /*   610 */    97,   98,   99,  106,   73,  102,  129,   41,  111,   62,
 /*   620 */   207,  208,    2,  171,  172,  187,  174,  175,  176,  177,
 /*   630 */   178,  179,   72,  126,  127,  128,    2,  151,  152,  153,
 /*   640 */   154,  155,  185,  186,   91,   92,    2,  151,  152,  153,
 /*   650 */   154,  155,   95,   96,   97,   98,   99,  171,  172,  102,
 /*   660 */   174,  175,  176,  177,  178,  179,  103,  171,  172,    2,
 /*   670 */   174,  175,  176,  177,  178,  179,   74,  151,  152,  153,
 /*   680 */   154,  155,  106,  107,  108,  109,  110,  151,  152,  153,
 /*   690 */   154,  155,   95,   96,   97,   98,   99,  171,  172,  102,
 /*   700 */   174,  175,  176,  177,  178,  179,   72,  171,  172,   73,
 /*   710 */   174,  175,  176,  177,  178,  179,  151,  152,  153,  154,
 /*   720 */   155,   72,  152,  193,  194,  151,  152,  153,  154,  155,
 /*   730 */    74,   95,   96,   97,   98,   99,  171,  172,  102,  174,
 /*   740 */   175,  176,  177,  178,  179,  171,  172,  177,  174,  175,
 /*   750 */   176,  177,  178,  179,   72,  151,  152,  153,  154,  155,
 /*   760 */    41,  210,  211,  133,  134,  151,  152,  153,  154,  155,
 /*   770 */    95,   96,   97,   98,   99,  171,  172,  102,  174,  175,
 /*   780 */   176,  177,  178,  179,   72,  171,  172,   82,  174,  175,
 /*   790 */   176,  177,  178,  179,  210,  211,  210,  211,   82,  151,
 /*   800 */   152,  153,  154,  155,  210,  211,  152,    1,   74,  151,
 /*   810 */   152,  153,  154,  155,   95,   96,   97,   98,   99,  171,
 /*   820 */   172,  102,  174,  175,  176,  177,  178,  179,   82,  171,
 /*   830 */   172,  177,  174,  175,  176,  177,  178,  179,    1,  151,
 /*   840 */   152,  153,  154,  155,  106,  107,  108,  109,  110,  151,
 /*   850 */   152,  153,  154,  155,   95,   96,   97,   98,   99,  171,
 /*   860 */   172,  102,   74,  175,  176,  177,  178,  179,  102,  171,
 /*   870 */   172,   77,  184,  175,  176,  177,  178,  179,   77,   74,
 /*   880 */    66,  152,  184,  151,  152,  153,  154,  155,  151,  152,
 /*   890 */   153,  154,  155,  106,  107,  108,  109,  110,  151,  152,
 /*   900 */   153,  154,  155,  171,  172,   74,  177,   74,  171,  172,
 /*   910 */    74,  188,  175,  176,  177,  178,  179,   74,  171,  172,
 /*   920 */    74,  184,  175,  176,  177,  178,  179,   74,   74,   74,
 /*   930 */    74,  184,  151,  152,  153,  154,  155,  106,  107,  108,
 /*   940 */   109,  110,  151,  152,  153,  154,  155,  164,   73,  188,
 /*   950 */    74,  152,  171,  172,   74,  126,  175,  176,  177,  178,
 /*   960 */   179,   74,  171,  172,   74,  184,  175,  176,  177,  178,
 /*   970 */   179,  126,  154,  154,  127,  184,  151,  152,  153,  154,
 /*   980 */   155,  106,  107,  108,  109,  110,  151,  152,  153,  154,
 /*   990 */   155,  106,  107,  108,  109,  110,  171,  172,  218,   41,
 /*  1000 */   175,  176,  177,  178,  179,  221,  171,  172,   61,  184,
 /*  1010 */   175,  176,  177,  178,  179,  222,   62,  152,  221,  184,
 /*  1020 */   222,  151,  152,  153,  154,  155,  218,  154,  151,  152,
 /*  1030 */   153,  154,  155,   73,  221,  162,  163,  164,  165,  166,
 /*  1040 */   167,  171,  172,  170,  222,  175,  176,  177,  178,  179,
 /*  1050 */   173,  192,    2,    2,  184,  197,  159,  180,  181,  182,
 /*  1060 */   151,  152,  153,  154,  155,  197,  106,  107,  108,  109,
 /*  1070 */   110,  151,  152,  153,  154,  155,   73,  126,   42,  219,
 /*  1080 */   171,  172,  220,   26,  175,  176,  177,  178,  179,  222,
 /*  1090 */   221,  171,  172,  184,  174,  175,  176,  177,  178,  179,
 /*  1100 */   218,  220,  219,  219,  151,  152,  153,  154,  155,  106,
 /*  1110 */   107,  108,  109,  110,  151,  152,  153,  154,  155,  221,
 /*  1120 */    73,  222,  218,  220,  171,  172,  221,  174,  175,  176,
 /*  1130 */   177,  178,  179,  222,  171,  172,  221,  174,  175,  176,
 /*  1140 */   177,  178,  179,  222,  221,  218,  222,  221,  151,  152,
 /*  1150 */   153,  154,  155,  106,  107,  108,  109,  110,  151,  152,
 /*  1160 */   153,  154,  155,  222,  221,  220,  222,  219,  171,  172,
 /*  1170 */   221,  174,  175,  176,  177,  178,  179,  222,  171,  172,
 /*  1180 */   152,  174,  175,  176,  177,  178,  179,   26,  151,  152,
 /*  1190 */   153,  154,  155,  151,  152,  153,  154,  155,  161,  218,
 /*  1200 */   220,  219,   41,   42,  221,  152,  151,  152,  153,  154,
 /*  1210 */   155,  222,  218,  171,  172,  220,  174,  175,  176,  177,
 /*  1220 */   178,  179,   61,   62,  219,  221,  171,  172,  222,  174,
 /*  1230 */   175,  176,  177,  178,  179,   74,  151,  152,  153,  154,
 /*  1240 */   155,   82,   83,   84,  221,   86,   87,   88,   89,  222,
 /*  1250 */   151,  152,  153,  154,  155,  152,  171,  172,  166,  174,
 /*  1260 */   175,  176,  177,  178,  179,  106,  107,  108,  109,  110,
 /*  1270 */   171,  172,  194,  205,  222,  186,   82,   83,   84,  218,
 /*  1280 */    86,   87,   88,   89,  218,  222,  219,    2,  219,   82,
 /*  1290 */    83,   84,  219,   86,   87,   88,   89,    2,  219,  218,
 /*  1300 */   106,  107,  108,  109,  110,  240,    1,  240,  240,  240,
 /*  1310 */     5,    6,  240,  106,  107,  108,  109,  110,    2,   82,
 /*  1320 */    83,   84,  240,   86,   87,   88,   89,  240,  151,  152,
 /*  1330 */   153,  154,  155,  106,  107,  108,  109,  110,   22,   23,
 /*  1340 */    24,   25,  240,  106,  107,  108,  109,  110,    1,  240,
 /*  1350 */   173,  240,    5,    6,   38,  240,   40,  180,  181,  182,
 /*  1360 */   240,   45,  151,  152,  153,  154,  155,  240,   63,  240,
 /*  1370 */    65,  240,  240,   57,  240,   59,   60,   72,  240,  240,
 /*  1380 */   240,  240,  171,  172,  240,  240,  240,  176,  177,  240,
 /*  1390 */   179,  240,  240,  240,  240,   48,   49,   50,  240,  240,
 /*  1400 */   240,  190,   14,  240,   16,   17,   18,   19,   20,   21,
 /*  1410 */   240,  106,  240,  240,  240,  240,  111,  240,  240,   72,
 /*  1420 */   240,  151,  152,  153,  154,  155,  106,  107,  108,  109,
 /*  1430 */   110,  126,  127,  128,  240,  240,   48,   49,  240,  240,
 /*  1440 */   240,  171,  172,  240,  240,  129,  176,  177,  240,  179,
 /*  1450 */   240,  240,  240,  106,  240,  240,  240,  240,  111,  240,
 /*  1460 */   190,  240,  151,  152,  153,  154,  155,  240,  240,  240,
 /*  1470 */   240,  240,  240,  126,  127,  128,  240,  151,  152,  153,
 /*  1480 */   154,  155,  171,  172,  240,  240,  240,  176,  177,  240,
 /*  1490 */   179,  151,  152,  153,  154,  155,  240,  171,  172,  240,
 /*  1500 */   240,  190,  176,  177,  240,  179,  151,  152,  153,  154,
 /*  1510 */   155,  240,  240,  173,  240,  240,  190,  240,  130,  240,
 /*  1520 */   180,  181,  182,  240,  240,  240,  240,  240,  173,  151,
 /*  1530 */   152,  153,  154,  155,  240,  180,  181,  182,  240,  240,
 /*  1540 */   151,  152,  153,  154,  155,  240,  240,  240,  240,  240,
 /*  1550 */   240,  173,  240,  151,  152,  153,  154,  155,  180,  181,
 /*  1560 */   182,  240,  173,  151,  152,  153,  154,  155,  240,  180,
 /*  1570 */   181,  182,  240,  240,  240,  173,  151,  152,  153,  154,
 /*  1580 */   155,  240,  180,  181,  182,  173,  240,  240,  240,  240,
 /*  1590 */   240,  240,  180,  181,  182,  240,  240,  240,  173,  151,
 /*  1600 */   152,  153,  154,  155,  240,  180,  181,  182,  240,  150,
 /*  1610 */   151,  152,  153,  154,  155,  240,  157,  158,  240,  240,
 /*  1620 */   240,  173,  240,  240,  240,  240,  240,  240,  180,  181,
 /*  1630 */   182,  150,  151,  152,  153,  154,  155,  154,  157,  158,
 /*  1640 */   240,  240,  240,  240,  240,  162,  163,  164,  165,  166,
 /*  1650 */   167,  240,  154,  170,  151,  152,  153,  154,  155,  240,
 /*  1660 */   162,  163,  164,  165,  166,  167,  240,  240,  170,  240,
 /*  1670 */   240,  154,  240,  240,  171,  172,  240,  240,  240,  162,
 /*  1680 */   163,  164,  165,  166,  167,  154,  240,  170,  240,  240,
 /*  1690 */   240,  240,  240,  162,  163,  164,  165,  166,  167,  154,
 /*  1700 */   240,  170,  240,  240,  240,  240,  240,  162,  163,  164,
 /*  1710 */   165,  166,  167,  154,  240,  170,  151,  152,  153,  154,
 /*  1720 */   155,  162,  163,  164,  165,  166,  167,  240,  240,  170,
 /*  1730 */   154,  240,  240,  240,  240,  240,  171,  172,  162,  163,
 /*  1740 */   164,  165,  166,  167,  240,  240,  170,  154,  240,   26,
 /*  1750 */   240,   26,  240,  240,  240,  162,  163,  164,  165,  166,
 /*  1760 */   167,  154,  240,  170,   41,   42,   41,   42,  240,  162,
 /*  1770 */   163,  164,  165,  166,  167,  240,   26,  170,   26,  240,
 /*  1780 */   240,  240,  240,  240,   61,   62,   61,   62,  240,  240,
 /*  1790 */   240,   41,   42,   41,   42,  240,  240,   74,  240,   74,
 /*  1800 */    26,  240,  240,   80,  240,   80,  240,  240,  240,  240,
 /*  1810 */   240,   61,   62,   61,   62,   41,   42,  151,  152,  153,
 /*  1820 */   154,  155,  240,  240,   74,  240,   74,  161,  240,  240,
 /*  1830 */    80,  240,   80,  240,  240,   61,   62,  151,  152,  153,
 /*  1840 */   154,  155,  240,  240,  240,  240,  160,  161,   74,  240,
 /*  1850 */   240,  240,  240,  240,   80,  240,  240,  240,  150,  151,
 /*  1860 */   152,  153,  154,  155,  240,  240,  158,  150,  151,  152,
 /*  1870 */   153,  154,  155,  240,  240,  158,  150,  151,  152,  153,
 /*  1880 */   154,  155,  240,  240,  158,  240,  240,  150,  151,  152,
 /*  1890 */   153,  154,  155,  240,  240,  158,  240,  150,  151,  152,
 /*  1900 */   153,  154,  155,  240,  240,  158,  150,  151,  152,  153,
 /*  1910 */   154,  155,  240,  240,  158,  150,  151,  152,  153,  154,
 /*  1920 */   155,  240,  240,  158,  240,  150,  151,  152,  153,  154,
 /*  1930 */   155,  240,  240,  158,  240,  150,  151,  152,  153,  154,
 /*  1940 */   155,  240,  240,  158,  150,  151,  152,  153,  154,  155,
 /*  1950 */   240,  240,  158,  240,  150,  151,  152,  153,  154,  155,
 /*  1960 */   240,  240,  158,  150,  151,  152,  153,  154,  155,  240,
 /*  1970 */   240,  158,  240,  240,  240,  150,  151,  152,  153,  154,
 /*  1980 */   155,  240,  240,  158,  240,  240,  150,  151,  152,  153,
 /*  1990 */   154,  155,  240,  240,  158,  240,  240,  150,  151,  152,
 /*  2000 */   153,  154,  155,  240,  240,  158,  240,  240,  240,  150,
 /*  2010 */   151,  152,  153,  154,  155,  240,  240,  158,  150,  151,
 /*  2020 */   152,  153,  154,  155,  240,  240,  158,  150,  151,  152,
 /*  2030 */   153,  154,  155,  240,  240,  158,  240,  240,  150,  151,
 /*  2040 */   152,  153,  154,  155,  240,  240,  158,  240,  150,  151,
 /*  2050 */   152,  153,  154,  155,  240,  240,  158,  150,  151,  152,
 /*  2060 */   153,  154,  155,  240,  240,  158,  150,  151,  152,  153,
 /*  2070 */   154,  155,  240,  240,  158,  240,  150,  151,  152,  153,
 /*  2080 */   154,  155,  240,  240,  158,  240,  150,  151,  152,  153,
 /*  2090 */   154,  155,  240,  240,  158,  150,  151,  152,  153,  154,
 /*  2100 */   155,  240,  240,  158,  240,  150,  151,  152,  153,  154,
 /*  2110 */   155,  240,  240,  158,  150,  151,  152,  153,  154,  155,
 /*  2120 */   240,  240,  158,  240,  240,  240,  150,  151,  152,  153,
 /*  2130 */   154,  155,  240,  240,  158,  240,  240,  150,  151,  152,
 /*  2140 */   153,  154,  155,  240,  240,  158,  240,  240,  150,  151,
 /*  2150 */   152,  153,  154,  155,  240,  240,  158,  240,  240,  240,
 /*  2160 */   150,  151,  152,  153,  154,  155,  240,  240,  158,  150,
 /*  2170 */   151,  152,  153,  154,  155,  240,  240,  158,  150,  151,
 /*  2180 */   152,  153,  154,  155,  240,  240,  158,  240,  240,  150,
 /*  2190 */   151,  152,  153,  154,  155,  240,  240,  158,  240,  150,
 /*  2200 */   151,  152,  153,  154,  155,  240,  240,  158,  150,  151,
 /*  2210 */   152,  153,  154,  155,  240,  240,  158,  150,  151,  152,
 /*  2220 */   153,  154,  155,  240,  240,  158,  240,  150,  151,  152,
 /*  2230 */   153,  154,  155,  240,  240,  158,  240,  150,  151,  152,
 /*  2240 */   153,  154,  155,  240,  240,  158,  150,  151,  152,  153,
 /*  2250 */   154,  155,  240,  240,  158,  240,  150,  151,  152,  153,
 /*  2260 */   154,  155,  240,  240,  158,  150,  151,  152,  153,  154,
 /*  2270 */   155,  240,  240,  158,  240,  240,  240,  150,  151,  152,
 /*  2280 */   153,  154,  155,  240,  240,  158,  240,  240,  150,  151,
 /*  2290 */   152,  153,  154,  155,  240,  240,  158,  240,  240,  150,
 /*  2300 */   151,  152,  153,  154,  155,  240,  240,  158,  240,  240,
 /*  2310 */   240,  150,  151,  152,  153,  154,  155,  240,  240,  158,
 /*  2320 */   150,  151,  152,  153,  154,  155,  240,  240,  158,  151,
 /*  2330 */   152,  153,  154,  155,  151,  152,  153,  154,  155,  161,
 /*  2340 */   240,  240,  240,  240,  161,  151,  152,  153,  154,  155,
 /*  2350 */   240,  240,  240,  240,  240,  161,  151,  152,  153,  154,
 /*  2360 */   155,  151,  152,  153,  154,  155,  161,  240,  240,  240,
 /*  2370 */   240,  161,  151,  152,  153,  154,  155,  151,  152,  153,
 /*  2380 */   154,  155,  161,  240,  240,  240,  240,  161,  240,  151,
 /*  2390 */   152,  153,  154,  155,  151,  152,  153,  154,  155,  161,
 /*  2400 */   240,  240,  240,  240,  161,  240,  151,  152,  153,  154,
 /*  2410 */   155,  151,  152,  153,  154,  155,  161,  240,  240,  240,
 /*  2420 */   240,  161,  151,  152,  153,  154,  155,  240,  240,  240,
 /*  2430 */   240,  240,  161,  151,  152,  153,  154,  155,  240,  240,
 /*  2440 */   240,  240,  240,  161,  151,  152,  153,  154,  155,  240,
 /*  2450 */   240,  240,  240,  240,  161,  151,  152,  153,  154,  155,
 /*  2460 */   240,  240,  240,  240,  240,  161,  151,  152,  153,  154,
 /*  2470 */   155,  151,  152,  153,  154,  155,  161,  240,  240,  240,
 /*  2480 */   240,  161,  151,  152,  153,  154,  155,  151,  152,  153,
 /*  2490 */   154,  155,  161,  240,  240,  240,  240,  161,  151,  152,
 /*  2500 */   153,  154,  155,  240,  240,  240,  240,  240,  161,  151,
 /*  2510 */   152,  153,  154,  155,  151,  152,  153,  154,  155,  161,
 /*  2520 */   240,  240,  240,  240,  161,
};
#define YY_SHIFT_USE_DFLT (-100)
#define YY_SHIFT_COUNT (432)
#define YY_SHIFT_MIN   (-99)
#define YY_SHIFT_MAX   (1774)
static const short yy_shift_ofst[] = {
 /*     0 */  -100,  107,  119,  119,  192,  192,  192,  192,  192,  192,
 /*    10 */   192,  192,  260,  260,  260,  260,  260,  260,  260,  260,
 /*    20 */   260,  260,  192,  192,  192,  192,  192,  192,  192,  192,
 /*    30 */   192,  192,  192,  192,  341,  341,  341,  341,  379,  379,
 /*    40 */   379,  414,  414,  414,  414,  414,  414,  414,  414,  414,
 /*    50 */   414,  379,  379,  379,  379,  379,  379,  379,  379,  379,
 /*    60 */   379,  507,  507,  507,  507,  507,  507,  507,  507,  507,
 /*    70 */   507,  507,  507,  507,  507,  507,  507,  507,  507,  507,
 /*    80 */   507,  507,  507,  507,  507,  507,  507,  507,  507,  507,
 /*    90 */   507,  507,  507,  507,  507,  507,  507,  507,  507,  507,
 /*   100 */   507,  507,  507,  507,  507,  507,  507,  507,  507,  507,
 /*   110 */   507, 1347, 1347, 1347, 1347, 1347, 1305, 1305, 1305, 1305,
 /*   120 */  1305, 1305, 1305, 1305, 1305, 1305, 1305, 1305, 1305, 1305,
 /*   130 */  1305, 1305, 1305, 1305, 1305, 1305, 1305, 1305, 1305, 1305,
 /*   140 */  1305, 1316,  461,  231,  487,  197,  487,  487,  350,  634,
 /*   150 */   197,  487,  487,  487, 1295, 1285,  272,  272,  634,  634,
 /*   160 */   634,  634,  511,  272,   64,  553,   64, 1295, 1285, 1050,
 /*   170 */   719,  513,  513,  513,  513,  557,  576,  -67,  437,  197,
 /*   180 */   511,  462,  954,  845,  954,  947,  954,  947, 1057, 1036,
 /*   190 */   958,  845,  954,  947, 1057, 1036,  958,  845,  954,  947,
 /*   200 */  1057, 1036,  958,  954,  947,  954,  947,  954,  947,  954,
 /*   210 */   947,  954,  947, 1057, 1036,  958,  954,  947, 1057, 1036,
 /*   220 */   958,  954,  947, 1057, 1036,  951, 1051, 1051, 1050,  954,
 /*   230 */   947,  958,  954,  947,  845,  954,  947,  958,  847,  847,
 /*   240 */   845,  829, -100, -100,  298,  -32,  -22,  348,  476,  267,
 /*   250 */  1237, 1207, 1194, 1159, 1388,  105,  636, 1774, 1752, 1750,
 /*   260 */  1725, 1723,  493,  448,  385,  831,  597,  597,  759,  759,
 /*   270 */   759,  675, 1047,  597, 1161, 1003,  960,  875,  203, 1320,
 /*   280 */  1320, 1320,  130, 1320, 1320, 1320, 1320, 1320, 1320, 1320,
 /*   290 */  1227,  118,  -27,  -27,  -27,  -27,  -27,  -27,  -27,  -27,
 /*   300 */   -27,  -27,  -27,  -27,  -27,  -27,  738, 1227,  885,  787,
 /*   310 */   738,  738,  -27,  -27,  -27,  -27,  -27,  -27,  -27,  -27,
 /*   320 */   -27,  265,  265,  171,   51,   51,  314,  314,  314,  314,
 /*   330 */    24,   24,  -73,  -73,  -73,  -73,    1,    1,  -73,  -73,
 /*   340 */   -73,  -73,  342,  331,  127,  338,  180,  249,  -99,  -99,
 /*   350 */   -99,  -99,  305,  145,  287,  450,  128,  264,  449,  445,
 /*   360 */    85,  149,  421,  122,   53,  225,   79,  -18,  330,  208,
 /*   370 */   164,  133,   93,   72,  814,  890,  887,  880,  876,  856,
 /*   380 */   855,  854,  853,  846,  843,  836,  833,  805,  801,  794,
 /*   390 */   766,  766,  788,  837,  746,  734,  806,  716,  656,  602,
 /*   400 */   705,  712,  667,  563,  644,  682,  649,  560,  620,  541,
 /*   410 */   499,  503,  479,  470,  477,  458,  480,  442,  401,  401,
 /*   420 */   388,  312,  312,  328,  329,  371,  263,  188,  106,   99,
 /*   430 */    82,   71,   68,
};
#define YY_REDUCE_USE_DFLT (-137)
#define YY_REDUCE_COUNT (243)
#define YY_REDUCE_MIN   (-136)
#define YY_REDUCE_MAX   (2363)
static const short yy_reduce_ofst[] = {
 /*     0 */   630, -132,   98,  320, 1085, 1055, 1042, 1007,  997,  963,
 /*    10 */   953,  920,  909,  870,  835,  825,  791,  781,  747,  737,
 /*    20 */   698,  688,  658,  648,  614,  604,  574,  565,  536,  526,
 /*    30 */   496,  486,  452,  422, 1326, 1311, 1270, 1211,  125,  -33,
 /*    40 */  -136, 1448, 1425, 1412, 1402, 1389, 1378, 1355, 1340, 1177,
 /*    50 */   877, 1607, 1593, 1576, 1559, 1545, 1531, 1517, 1498, 1483,
 /*    60 */   873, 1481, 1459,  376, 2170, 2161, 2149, 2138, 2127, 2115,
 /*    70 */  2106, 2096, 2087, 2077, 2067, 2058, 2049, 2039, 2028, 2019,
 /*    80 */  2010, 1998, 1987, 1976, 1964, 1955, 1945, 1936, 1926, 1916,
 /*    90 */  1907, 1898, 1888, 1877, 1868, 1859, 1847, 1836, 1825, 1813,
 /*   100 */  1804, 1794, 1785, 1775, 1765, 1756, 1747, 1737, 1726, 1717,
 /*   110 */  1708, 1565, 1503, 1099,  732,  -82, 1686, 2363, 2358, 2347,
 /*   120 */  2336, 2331, 2320, 2315, 2304, 2293, 2282, 2271, 2260, 2255,
 /*   130 */  2243, 2238, 2226, 2221, 2210, 2205, 2194, 2183, 2178, 1666,
 /*   140 */  1037,  -16,  233,  121,  212,    6,  173,   89,   80,  250,
 /*   150 */   235,  313,  304,  132,  242,  117,  729,  654,  594,  586,
 /*   160 */   584,  551,  530,  570,  438,  457,  374,  413,  381,  377,
 /*   170 */  1081, 1079, 1073, 1069, 1067, 1063, 1066, 1089, 1061, 1068,
 /*   180 */  1078, 1092, 1052, 1103, 1027, 1023, 1006, 1004, 1005,  995,
 /*   190 */   994, 1053,  989,  983,  982,  980,  981, 1028,  955,  949,
 /*   200 */   948,  945,  927,  944,  943,  941,  926,  924,  923,  921,
 /*   210 */   915,  911,  905,  884,  903,  904,  899,  898,  883,  881,
 /*   220 */   882,  867,  869,  860,  862,  897,  868,  858,  859,  822,
 /*   230 */   813,  808,  798,  797,  865,  793,  784,  780,  819,  818,
 /*   240 */   799,  783,  761,  723,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   695, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*    10 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*    20 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*    30 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*    40 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*    50 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*    60 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*    70 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*    80 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*    90 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*   100 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*   110 */  1057, 1057,  910, 1057, 1057,  911, 1057, 1057, 1057, 1057,
 /*   120 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*   130 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*   140 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*   150 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*   160 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*   170 */  1008, 1010, 1010, 1010, 1010, 1016, 1008, 1057, 1008, 1057,
 /*   180 */  1057, 1057, 1016, 1057, 1016, 1014, 1016, 1014, 1010, 1012,
 /*   190 */  1008, 1057, 1016, 1014, 1010, 1012, 1008, 1057, 1016, 1014,
 /*   200 */  1010, 1012, 1008, 1016, 1014, 1016, 1014, 1016, 1014, 1016,
 /*   210 */  1014, 1016, 1014, 1010, 1012, 1008, 1016, 1014, 1010, 1012,
 /*   220 */  1008, 1016, 1014, 1010, 1012, 1057, 1057, 1057, 1057, 1016,
 /*   230 */  1014, 1008, 1016, 1014, 1057, 1016, 1014, 1008, 1057, 1057,
 /*   240 */  1057, 1057,  917,  917,  771, 1057, 1057, 1057, 1057, 1057,
 /*   250 */  1057,  868, 1057,  868, 1057, 1057, 1057,  847,  846,  845,
 /*   260 */   843,  842, 1057, 1057, 1057, 1057, 1009, 1011, 1000,  997,
 /*   270 */   904, 1015, 1057, 1007,  771, 1057, 1057, 1057,  901,  893,
 /*   280 */   728,  729,  878,  890,  889,  888,  887,  886,  885,  884,
 /*   290 */   912,  839,  861,  860,  859,  858,  857,  856,  855,  870,
 /*   300 */   867,  866,  865,  864,  863,  862,  916,  913, 1057, 1057,
 /*   310 */   745,  744,  854,  853,  852,  851,  850,  849,  848,  725,
 /*   320 */   724,  903,  902, 1057,  880,  879,  814,  827,  828,  813,
 /*   330 */   841,  840,  773,  772,  778,  777,  798,  799,  783,  782,
 /*   340 */   757,  758, 1057, 1057, 1057, 1057, 1057, 1057,  977,  981,
 /*   350 */   980,  978, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*   360 */  1057, 1057, 1057, 1057, 1057, 1057,  938, 1057, 1057, 1057,
 /*   370 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*   380 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,  766,
 /*   390 */   900,  899, 1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,
 /*   400 */   891,  727, 1057,  979, 1057,  967,  947,  949, 1057, 1057,
 /*   410 */  1057, 1057, 1057, 1057, 1057, 1057, 1057, 1057,  877,  876,
 /*   420 */   868,  838,  837,  914, 1057, 1057,  743,  739,  737,  723,
 /*   430 */   720,  718, 1057, 1056, 1055, 1054, 1053, 1052, 1051, 1050,
 /*   440 */  1049, 1048, 1047, 1046, 1045, 1044, 1043, 1042, 1041, 1036,
 /*   450 */  1035, 1037, 1034, 1033, 1032, 1031, 1030, 1029, 1028, 1027,
 /*   460 */  1026, 1025, 1024, 1023, 1022, 1021, 1020, 1019, 1018, 1017,
 /*   470 */   993,  992,  999,  998, 1006, 1005, 1002, 1001,  996, 1004,
 /*   480 */  1003,  895,  897,  898,  896,  894,  769,  995,  994,  988,
 /*   490 */   987,  989,  986,  991,  990,  985,  892,  726,  983,  982,
 /*   500 */   984,  976,  971,  974,  975,  973,  972,  970,  962,  965,
 /*   510 */   969,  968,  966,  964,  963,  961,  943,  948,  950,  935,
 /*   520 */   934,  960,  959,  958,  957,  956,  955,  954,  953,  952,
 /*   530 */   951,  946,  945,  944,  942,  926,  929,  930,  933,  932,
 /*   540 */   931,  928,  927,  925, 1040, 1039, 1038,  872,  874,  883,
 /*   550 */   882,  881,  875,  873,  871,  812,  811,  810,  809,  808,
 /*   560 */   807,  817,  816,  815,  829,  831,  830,  826,  825,  824,
 /*   570 */   823,  822,  821,  820,  819,  818,  806,  805,  804,  803,
 /*   580 */  1013,  923,  922,  921,  920,  919,  833,  835,  906,  909,
 /*   590 */   908,  907,  905,  869,  847,  846,  845,  844,  843,  842,
 /*   600 */   836,  834,  832,  769,  915,  941,  940,  939,  937,  936,
 /*   610 */   918,  776,  775,  774,  781,  780,  779,  771,  770,  769,
 /*   620 */   768,  767,  766,  800,  802,  801,  797,  796,  795,  794,
 /*   630 */   793,  792,  791,  790,  789,  788,  787,  742,  741,  740,
 /*   640 */   738,  736,  735,  734,  733,  732,  731,  730,  786,  785,
 /*   650 */   784,  765,  764,  763,  762,  759,  761,  760,  756,  755,
 /*   660 */   754,  753,  752,  751,  750,  749,  748,  747,  746,  722,
 /*   670 */   721,  719,  717,  713,  712,  716,  715,  714,  711,  710,
 /*   680 */   709,  708,  707,  706,  705,  704,  703,  702,  701,  700,
 /*   690 */   699,  698,  697,  696,
};

/* The next table maps tokens into fallback tokens.  If a construct
** like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammar, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
*/
#ifdef YYFALLBACK
static const YYCODETYPE yyFallback[] = {
    0,  /*          $ => nothing */
    0,  /*    INTEGER => nothing */
    0,  /* IDENTIFIER => nothing */
    0,  /* POUND_IDENTIFIER => nothing */
    0,  /* POUND_INTEGER => nothing */
    0,  /* AT_IDENTIFIER => nothing */
    0,  /* STRING_LITERAL => nothing */
    0,  /*     ASP_GR => nothing */
    0,  /*     ASP_CP => nothing */
    0,  /*     LUA_GR => nothing */
    0,  /*     LUA_CP => nothing */
    0,  /*    F2LP_GR => nothing */
    0,  /*    F2LP_CP => nothing */
    0,  /*    COMMENT => nothing */
    0,  /*  CONSTANTS => nothing */
    0,  /*    INCLUDE => nothing */
    0,  /*     MACROS => nothing */
    0,  /*    OBJECTS => nothing */
    0,  /*      QUERY => nothing */
    0,  /*       SHOW => nothing */
    0,  /*      SORTS => nothing */
    0,  /*  VARIABLES => nothing */
    0,  /*   ABACTION => nothing */
    0,  /*     ACTION => nothing */
    0,  /* ADDITIVEACTION => nothing */
    0,  /* ADDITIVEFLUENT => nothing */
    0,  /*      AFTER => nothing */
    0,  /*        ALL => nothing */
    0,  /*     ALWAYS => nothing */
    0,  /*   ASSUMING => nothing */
    0,  /*  ATTRIBUTE => nothing */
    0,  /*         BY => nothing */
    0,  /*     CAUSED => nothing */
    0,  /*     CAUSES => nothing */
    0,  /* IMPOSSIBLE => nothing */
    0,  /* CONSTRAINT => nothing */
    0,  /* DECREMENTS => nothing */
    0,  /*    DEFAULT => nothing */
    0,  /* EXTERNALACTION => nothing */
    0,  /*  EXOGENOUS => nothing */
    0,  /* EXOGENOUSACTION => nothing */
    0,  /*         IF => nothing */
    0,  /*     IFCONS => nothing */
    0,  /* INCREMENTS => nothing */
    0,  /*   INERTIAL => nothing */
    0,  /* INERTIALFLUENT => nothing */
    0,  /*      LABEL => nothing */
    0,  /*  MAY_CAUSE => nothing */
    0,  /* MAXADDITIVE => nothing */
    0,  /* MAXAFVALUE => nothing */
    0,  /*    MAXSTEP => nothing */
    0,  /*      NEVER => nothing */
    0,  /* NOCONCURRENCY => nothing */
    0,  /* STRONG_NOCONCURRENCY => nothing */
    0,  /* NONEXECUTABLE => nothing */
    0,  /*         OF => nothing */
    0,  /* POSSIBLY_CAUSED => nothing */
    0,  /*      RIGID => nothing */
    0,  /*   SDFLUENT => nothing */
    0,  /* SIMPLEFLUENT => nothing */
    0,  /* EXTERNALFLUENT => nothing */
    0,  /*     UNLESS => nothing */
    0,  /*      WHERE => nothing */
    0,  /*      FALSE => nothing */
    0,  /*       NONE => nothing */
    0,  /*       TRUE => nothing */
    0,  /*         AT => nothing */
    0,  /*  BRACKET_L => nothing */
    0,  /*  BRACKET_R => nothing */
    0,  /* COLON_DASH => nothing */
    0,  /* CBRACKET_L => nothing */
    0,  /* CBRACKET_R => nothing */
    0,  /*    PAREN_L => nothing */
    0,  /*    PAREN_R => nothing */
    0,  /*     PERIOD => nothing */
    0,  /* MACRO_STRING => nothing */
    0,  /*      TILDE => nothing */
    0,  /*  DBL_COLON => nothing */
    0,  /*  ARROW_LEQ => nothing */
    0,  /*  ARROW_REQ => nothing */
    0,  /* ARROW_LDASH => nothing */
    0,  /*      COLON => nothing */
    0,  /*         EQ => nothing */
    0,  /*     DBL_EQ => nothing */
    0,  /*        NEQ => nothing */
    0,  /*     NOT_EQ => nothing */
    0,  /*      LTHAN => nothing */
    0,  /*      GTHAN => nothing */
    0,  /*   LTHAN_EQ => nothing */
    0,  /*   GTHAN_EQ => nothing */
    0,  /* DBL_PERIOD => nothing */
    0,  /*   BIG_CONJ => nothing */
    0,  /*   BIG_DISJ => nothing */
    0,  /*      POUND => nothing */
    0,  /*  SEMICOLON => nothing */
    0,  /*      EQUIV => nothing */
    0,  /*       IMPL => nothing */
    0,  /* ARROW_RDASH => nothing */
    0,  /*   DBL_PLUS => nothing */
    0,  /*       PIPE => nothing */
    0,  /*  DBL_GTHAN => nothing */
    0,  /*  DBL_LTHAN => nothing */
    0,  /*        AMP => nothing */
    0,  /*      COMMA => nothing */
    0,  /*    DBL_AMP => nothing */
    0,  /*        NOT => nothing */
    0,  /*       DASH => nothing */
    0,  /*       PLUS => nothing */
    0,  /*       STAR => nothing */
    0,  /*    INT_DIV => nothing */
    0,  /*        MOD => nothing */
    0,  /*        ABS => nothing */
    0,  /*     UMINUS => nothing */
    0,  /*     CARROT => nothing */
    0,  /*        EOF => nothing */
    0,  /*     ERR_IO => nothing */
    0,  /* ERR_UNKNOWN_SYMBOL => nothing */
    0,  /* ERR_UNTERMINATED_STRING => nothing */
    0,  /* ERR_UNTERMINATED_ASP => nothing */
    0,  /* ERR_UNTERMINATED_LUA => nothing */
    0,  /* ERR_UNTERMINATED_F2LP => nothing */
    0,  /* ERR_UNTERMINATED_BLK_COMMENT => nothing */
    0,  /* ERR_SYNTAX => nothing */
    0,  /* ERR_PAREN_MISMATCH => nothing */
    0,  /*        ARG => nothing */
    0,  /*       NOOP => nothing */
    2,  /* CONSTANT_ID => IDENTIFIER */
    2,  /* VARIABLE_ID => IDENTIFIER */
    2,  /*  OBJECT_ID => IDENTIFIER */
};
#endif /* YYFALLBACK */

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
*/
struct yyStackEntry {
  YYACTIONTYPE stateno;  /* The state-number */
  YYCODETYPE major;      /* The major token value.  This is the code
                         ** number for the token at this stack level */
  YYMINORTYPE minor;     /* The user-supplied minor token value.  This
                         ** is the value of the token  */
};
typedef struct yyStackEntry yyStackEntry;

/* The state of the parser is completely contained in an instance of
** the following structure */
struct yyParser {
  int yyidx;                    /* Index of top element in stack */
#ifdef YYTRACKMAXSTACKDEPTH
  int yyidxMax;                 /* Maximum value of yyidx */
#endif
  int yyerrcnt;                 /* Shifts left before out of the error */
  lemon_parserARG_SDECL                /* A place to hold %extra_argument */
#if YYSTACKDEPTH<=0
  int yystksz;                  /* Current side of the stack */
  yyStackEntry *yystack;        /* The parser's stack */
#else
  yyStackEntry yystack[YYSTACKDEPTH];  /* The parser's stack */
#endif

  int yylookmajor;				/* major token type for the lookahead token */
  YYMINORTYPE yylookminor;		/* minor token type for the lookahead token */
  int yysyntaxerr;				/* a flag used to trigger a syntax error */

};
typedef struct yyParser yyParser;

#ifndef NDEBUG
#include <stdio.h>
static FILE *yyTraceFILE = 0;
static char const*yyTracePrompt = 0;
#endif /* NDEBUG */

#ifndef NDEBUG
/* 
** Turn parser tracing on by giving a stream to which to write the trace
** and a prompt to preface each trace message.  Tracing is turned off
** by making either argument NULL 
**
** Inputs:
** <ul>
** <li> A FILE* to which trace output should be written.
**      If NULL, then tracing is turned off.
** <li> A prefix string written at the beginning of every
**      line of trace output.  If NULL, then tracing is
**      turned off.
** </ul>
**
** Outputs:
** None.
*/
void lemon_parserTrace(FILE *TraceFILE, char const*zTracePrompt){
  yyTraceFILE = TraceFILE;
  yyTracePrompt = zTracePrompt;
  if( yyTraceFILE==0 ) yyTracePrompt = 0;
  else if( yyTracePrompt==0 ) yyTraceFILE = 0;
}
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
static const char *const yyTokenName[] = { 
  "$",             "INTEGER",       "IDENTIFIER",    "POUND_IDENTIFIER",
  "POUND_INTEGER",  "AT_IDENTIFIER",  "STRING_LITERAL",  "ASP_GR",      
  "ASP_CP",        "LUA_GR",        "LUA_CP",        "F2LP_GR",     
  "F2LP_CP",       "COMMENT",       "CONSTANTS",     "INCLUDE",     
  "MACROS",        "OBJECTS",       "QUERY",         "SHOW",        
  "SORTS",         "VARIABLES",     "ABACTION",      "ACTION",      
  "ADDITIVEACTION",  "ADDITIVEFLUENT",  "AFTER",         "ALL",         
  "ALWAYS",        "ASSUMING",      "ATTRIBUTE",     "BY",          
  "CAUSED",        "CAUSES",        "IMPOSSIBLE",    "CONSTRAINT",  
  "DECREMENTS",    "DEFAULT",       "EXTERNALACTION",  "EXOGENOUS",   
  "EXOGENOUSACTION",  "IF",            "IFCONS",        "INCREMENTS",  
  "INERTIAL",      "INERTIALFLUENT",  "LABEL",         "MAY_CAUSE",   
  "MAXADDITIVE",   "MAXAFVALUE",    "MAXSTEP",       "NEVER",       
  "NOCONCURRENCY",  "STRONG_NOCONCURRENCY",  "NONEXECUTABLE",  "OF",          
  "POSSIBLY_CAUSED",  "RIGID",         "SDFLUENT",      "SIMPLEFLUENT",
  "EXTERNALFLUENT",  "UNLESS",        "WHERE",         "FALSE",       
  "NONE",          "TRUE",          "AT",            "BRACKET_L",   
  "BRACKET_R",     "COLON_DASH",    "CBRACKET_L",    "CBRACKET_R",  
  "PAREN_L",       "PAREN_R",       "PERIOD",        "MACRO_STRING",
  "TILDE",         "DBL_COLON",     "ARROW_LEQ",     "ARROW_REQ",   
  "ARROW_LDASH",   "COLON",         "EQ",            "DBL_EQ",      
  "NEQ",           "NOT_EQ",        "LTHAN",         "GTHAN",       
  "LTHAN_EQ",      "GTHAN_EQ",      "DBL_PERIOD",    "BIG_CONJ",    
  "BIG_DISJ",      "POUND",         "SEMICOLON",     "EQUIV",       
  "IMPL",          "ARROW_RDASH",   "DBL_PLUS",      "PIPE",        
  "DBL_GTHAN",     "DBL_LTHAN",     "AMP",           "COMMA",       
  "DBL_AMP",       "NOT",           "DASH",          "PLUS",        
  "STAR",          "INT_DIV",       "MOD",           "ABS",         
  "UMINUS",        "CARROT",        "EOF",           "ERR_IO",      
  "ERR_UNKNOWN_SYMBOL",  "ERR_UNTERMINATED_STRING",  "ERR_UNTERMINATED_ASP",  "ERR_UNTERMINATED_LUA",
  "ERR_UNTERMINATED_F2LP",  "ERR_UNTERMINATED_BLK_COMMENT",  "ERR_SYNTAX",    "ERR_PAREN_MISMATCH",
  "ARG",           "NOOP",          "CONSTANT_ID",   "VARIABLE_ID", 
  "OBJECT_ID",     "NUMBER_RANGE",  "HIDE",          "OBSERVED",    
  "error",         "start",         "statement_lst",  "statement",   
  "stmt_macro_def",  "stmt_constant_def",  "stmt_object_def",  "stmt_variable_def",
  "stmt_sort_def",  "stmt_code_blk",  "stmt_law",      "stmt_show",   
  "stmt_hide",     "stmt_noconcurrency",  "stmt_strong_noconcurrency",  "stmt_maxafvalue",
  "stmt_maxadditive",  "stmt_query",    "base_elem",     "base_elem_no_const",
  "constant",      "object",        "variable",      "lua",         
  "undeclared",    "term_lst",      "term",          "constant_one_const",
  "term_no_const_lst",  "term_no_const",  "base_elem_local",  "base_elem_local_no_const",
  "constant_local",  "object_local",  "variable_local",  "lua_local",   
  "undeclared_local",  "term_local_lst",  "term_local",    "term_strong", 
  "term_strong_candidate",  "term_no_const_strong",  "formula",       "formula_base",
  "comparison",    "atomic_formula",  "formula_quant",  "formula_card",
  "formula_no_const",  "formula_no_const_base",  "comparison_no_const",  "atomic_formula_one_const",
  "formula_temporal",  "quant_lst",     "quant_op",      "card_af",     
  "card_sort_bnd_lst",  "sort",          "head_formula",  "macro_def_lst",
  "macro_bnd",     "macro_args",    "macro_arg",     "sort_lst",    
  "sort_id_nr",    "sort_id",       "constant_bnd_lst",  "constant_bnd",
  "constant_dcl_lst",  "constant_dcl_type",  "object_bnd_lst",  "object_bnd",  
  "object_lst",    "object_spec",   "variable_bnd_lst",  "variable_bnd",
  "variable_lst",  "sort_bnd_lst",  "sort_bnd",      "sort_dcl_lst",
  "show_lst",      "show_elem",     "query_lst",     "query_maxstep_decl",
  "query_label_decl",  "query_label_Decl",  "clause_if",     "clause_after",
  "clause_ifcons",  "clause_unless",  "clause_where",  "law_basic",   
  "law_caused",    "law_pcaused",   "law_impl",      "law_causes",  
  "law_increments",  "law_mcause",    "law_always",    "law_constraint",
  "law_impossible",  "law_never",     "law_default",   "law_exogenous",
  "law_inertial",  "law_nonexecutable",  "law_rigid",     "law_observed",
};
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
 /*   0 */ "start ::= statement_lst EOF",
 /*   1 */ "statement_lst ::=",
 /*   2 */ "statement_lst ::= statement_lst error",
 /*   3 */ "statement_lst ::= statement_lst statement",
 /*   4 */ "statement ::= stmt_macro_def",
 /*   5 */ "statement ::= stmt_constant_def",
 /*   6 */ "statement ::= stmt_object_def",
 /*   7 */ "statement ::= stmt_variable_def",
 /*   8 */ "statement ::= stmt_sort_def",
 /*   9 */ "statement ::= stmt_code_blk",
 /*  10 */ "statement ::= stmt_law",
 /*  11 */ "statement ::= stmt_show",
 /*  12 */ "statement ::= stmt_hide",
 /*  13 */ "statement ::= stmt_noconcurrency",
 /*  14 */ "statement ::= stmt_strong_noconcurrency",
 /*  15 */ "statement ::= stmt_maxafvalue",
 /*  16 */ "statement ::= stmt_maxadditive",
 /*  17 */ "statement ::= stmt_query",
 /*  18 */ "base_elem ::= constant",
 /*  19 */ "base_elem ::= base_elem_no_const",
 /*  20 */ "base_elem_no_const ::= object",
 /*  21 */ "base_elem_no_const ::= variable",
 /*  22 */ "base_elem_no_const ::= lua",
 /*  23 */ "constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R",
 /*  24 */ "constant ::= CONSTANT_ID",
 /*  25 */ "object ::= OBJECT_ID PAREN_L term_lst PAREN_R",
 /*  26 */ "object ::= OBJECT_ID",
 /*  27 */ "variable ::= VARIABLE_ID",
 /*  28 */ "lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  29 */ "lua ::= AT_IDENTIFIER",
 /*  30 */ "term_lst ::= term",
 /*  31 */ "term_lst ::= term_lst COMMA term",
 /*  32 */ "constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R",
 /*  33 */ "constant_one_const ::= CONSTANT_ID",
 /*  34 */ "term_no_const_lst ::= term_no_const",
 /*  35 */ "term_no_const_lst ::= term_no_const_lst COMMA term_no_const",
 /*  36 */ "base_elem_local ::= constant_local",
 /*  37 */ "base_elem_local ::= base_elem_local_no_const",
 /*  38 */ "base_elem_local_no_const ::= object_local",
 /*  39 */ "base_elem_local_no_const ::= variable",
 /*  40 */ "base_elem_local_no_const ::= variable_local",
 /*  41 */ "base_elem_local_no_const ::= lua_local",
 /*  42 */ "constant_local ::= CONSTANT_ID PAREN_L term_local_lst PAREN_R",
 /*  43 */ "constant_local ::= CONSTANT_ID",
 /*  44 */ "object_local ::= OBJECT_ID PAREN_L term_local_lst PAREN_R",
 /*  45 */ "object_local ::= OBJECT_ID",
 /*  46 */ "variable_local ::= POUND_IDENTIFIER",
 /*  47 */ "variable_local ::= POUND_INTEGER",
 /*  48 */ "lua_local ::= AT_IDENTIFIER PAREN_L term_local_lst PAREN_R",
 /*  49 */ "lua_local ::= AT_IDENTIFIER",
 /*  50 */ "term_local_lst ::= term_local",
 /*  51 */ "term_local_lst ::= term_local_lst COMMA term_local",
 /*  52 */ "term ::= base_elem",
 /*  53 */ "term ::= INTEGER",
 /*  54 */ "term ::= STRING_LITERAL",
 /*  55 */ "term ::= PAREN_L term PAREN_R",
 /*  56 */ "term ::= TRUE",
 /*  57 */ "term ::= FALSE",
 /*  58 */ "term ::= MAXSTEP",
 /*  59 */ "term ::= MAXADDITIVE",
 /*  60 */ "term ::= MAXAFVALUE",
 /*  61 */ "term ::= DASH term",
 /*  62 */ "term ::= ABS term",
 /*  63 */ "term ::= term DASH term",
 /*  64 */ "term ::= term PLUS term",
 /*  65 */ "term ::= term STAR term",
 /*  66 */ "term ::= term INT_DIV term",
 /*  67 */ "term ::= term MOD term",
 /*  68 */ "term_strong ::= base_elem_no_const",
 /*  69 */ "term_strong ::= INTEGER",
 /*  70 */ "term_strong ::= STRING_LITERAL",
 /*  71 */ "term_strong ::= PAREN_L term_strong PAREN_R",
 /*  72 */ "term_strong ::= MAXSTEP",
 /*  73 */ "term_strong ::= MAXADDITIVE",
 /*  74 */ "term_strong ::= MAXAFVALUE",
 /*  75 */ "term_strong ::= DASH term_strong",
 /*  76 */ "term_strong ::= ABS term",
 /*  77 */ "term_strong_candidate ::= DASH constant",
 /*  78 */ "term_strong ::= term_strong_candidate DASH term",
 /*  79 */ "term_strong ::= term_strong_candidate PLUS term",
 /*  80 */ "term_strong ::= term_strong_candidate STAR term",
 /*  81 */ "term_strong ::= term_strong_candidate INT_DIV term",
 /*  82 */ "term_strong ::= term_strong_candidate MOD term",
 /*  83 */ "term_strong ::= constant DASH term",
 /*  84 */ "term_strong ::= constant PLUS term",
 /*  85 */ "term_strong ::= constant STAR term",
 /*  86 */ "term_strong ::= constant INT_DIV term",
 /*  87 */ "term_strong ::= constant MOD term",
 /*  88 */ "term_strong ::= term_strong DASH term",
 /*  89 */ "term_strong ::= term_strong PLUS term",
 /*  90 */ "term_strong ::= term_strong STAR term",
 /*  91 */ "term_strong ::= term_strong INT_DIV term",
 /*  92 */ "term_strong ::= term_strong MOD term",
 /*  93 */ "term_local ::= base_elem_local",
 /*  94 */ "term_local ::= INTEGER",
 /*  95 */ "term_local ::= STRING_LITERAL",
 /*  96 */ "term_local ::= PAREN_L term_local PAREN_R",
 /*  97 */ "term_local ::= TRUE",
 /*  98 */ "term_local ::= FALSE",
 /*  99 */ "term_local ::= MAXSTEP",
 /* 100 */ "term_local ::= MAXADDITIVE",
 /* 101 */ "term_local ::= MAXAFVALUE",
 /* 102 */ "term_local ::= DASH term_local",
 /* 103 */ "term_local ::= ABS term_local",
 /* 104 */ "term_local ::= term_local DASH term_local",
 /* 105 */ "term_local ::= term_local PLUS term_local",
 /* 106 */ "term_local ::= term_local STAR term_local",
 /* 107 */ "term_local ::= term_local INT_DIV term_local",
 /* 108 */ "term_local ::= term_local MOD term_local",
 /* 109 */ "term_no_const_strong ::= base_elem_no_const",
 /* 110 */ "term_no_const_strong ::= INTEGER",
 /* 111 */ "term_no_const_strong ::= STRING_LITERAL",
 /* 112 */ "term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R",
 /* 113 */ "term_no_const_strong ::= MAXSTEP",
 /* 114 */ "term_no_const_strong ::= MAXADDITIVE",
 /* 115 */ "term_no_const_strong ::= MAXAFVALUE",
 /* 116 */ "term_no_const_strong ::= constant",
 /* 117 */ "term_no_const_strong ::= DASH term_no_const_strong",
 /* 118 */ "term_no_const_strong ::= ABS term_no_const",
 /* 119 */ "term_no_const_strong ::= term_no_const_strong DASH term_no_const",
 /* 120 */ "term_no_const_strong ::= term_no_const_strong PLUS term_no_const",
 /* 121 */ "term_no_const_strong ::= term_no_const_strong STAR term_no_const",
 /* 122 */ "term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const",
 /* 123 */ "term_no_const_strong ::= term_no_const_strong MOD term_no_const",
 /* 124 */ "term_no_const ::= base_elem_no_const",
 /* 125 */ "term_no_const ::= INTEGER",
 /* 126 */ "term_no_const ::= STRING_LITERAL",
 /* 127 */ "term_no_const ::= PAREN_L term_no_const PAREN_R",
 /* 128 */ "term_no_const ::= TRUE",
 /* 129 */ "term_no_const ::= FALSE",
 /* 130 */ "term_no_const ::= constant",
 /* 131 */ "term_no_const ::= DASH term_no_const",
 /* 132 */ "term_no_const ::= ABS term_no_const",
 /* 133 */ "term_no_const ::= term_no_const DASH term_no_const",
 /* 134 */ "term_no_const ::= term_no_const PLUS term_no_const",
 /* 135 */ "term_no_const ::= term_no_const STAR term_no_const",
 /* 136 */ "term_no_const ::= term_no_const INT_DIV term_no_const",
 /* 137 */ "term_no_const ::= term_no_const MOD term_no_const",
 /* 138 */ "formula ::= formula_base",
 /* 139 */ "formula ::= PAREN_L formula PAREN_R",
 /* 140 */ "formula ::= NOT formula",
 /* 141 */ "formula ::= DASH formula",
 /* 142 */ "formula ::= formula AMP formula",
 /* 143 */ "formula ::= formula DBL_PLUS formula",
 /* 144 */ "formula ::= formula PIPE formula",
 /* 145 */ "formula ::= formula EQUIV formula",
 /* 146 */ "formula ::= formula IMPL formula",
 /* 147 */ "formula ::= formula ARROW_RDASH formula",
 /* 148 */ "formula_base ::= comparison",
 /* 149 */ "formula_base ::= atomic_formula",
 /* 150 */ "formula_base ::= formula_quant",
 /* 151 */ "formula_base ::= formula_card",
 /* 152 */ "formula_base ::= TRUE",
 /* 153 */ "formula_base ::= FALSE",
 /* 154 */ "comparison ::= term_strong EQ term",
 /* 155 */ "comparison ::= term_strong DBL_EQ term",
 /* 156 */ "comparison ::= term_strong NEQ term",
 /* 157 */ "comparison ::= term_strong LTHAN term",
 /* 158 */ "comparison ::= term_strong GTHAN term",
 /* 159 */ "comparison ::= term_strong LTHAN_EQ term",
 /* 160 */ "comparison ::= term_strong GTHAN_EQ term",
 /* 161 */ "comparison ::= term_strong_candidate EQ term",
 /* 162 */ "comparison ::= term_strong_candidate DBL_EQ term",
 /* 163 */ "comparison ::= term_strong_candidate NEQ term",
 /* 164 */ "comparison ::= term_strong_candidate LTHAN term",
 /* 165 */ "comparison ::= term_strong_candidate GTHAN term",
 /* 166 */ "comparison ::= term_strong_candidate LTHAN_EQ term",
 /* 167 */ "comparison ::= term_strong_candidate GTHAN_EQ term",
 /* 168 */ "comparison ::= constant DBL_EQ term",
 /* 169 */ "comparison ::= constant NEQ term",
 /* 170 */ "comparison ::= constant LTHAN term",
 /* 171 */ "comparison ::= constant GTHAN term",
 /* 172 */ "comparison ::= constant LTHAN_EQ term",
 /* 173 */ "comparison ::= constant GTHAN_EQ term",
 /* 174 */ "atomic_formula ::= constant",
 /* 175 */ "atomic_formula ::= TILDE constant",
 /* 176 */ "atomic_formula ::= constant EQ term",
 /* 177 */ "formula_no_const ::= formula_no_const_base",
 /* 178 */ "formula_no_const ::= PAREN_L formula_no_const PAREN_R",
 /* 179 */ "formula_no_const ::= NOT formula_no_const",
 /* 180 */ "formula_no_const ::= DASH formula_no_const",
 /* 181 */ "formula_no_const ::= formula_no_const AMP formula_no_const",
 /* 182 */ "formula_no_const ::= formula_no_const DBL_PLUS formula_no_const",
 /* 183 */ "formula_no_const ::= formula_no_const PIPE formula_no_const",
 /* 184 */ "formula_no_const ::= formula_no_const EQUIV formula_no_const",
 /* 185 */ "formula_no_const ::= formula_no_const IMPL formula_no_const",
 /* 186 */ "formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const",
 /* 187 */ "formula_no_const_base ::= comparison_no_const",
 /* 188 */ "formula_no_const_base ::= TRUE",
 /* 189 */ "formula_no_const_base ::= FALSE",
 /* 190 */ "comparison_no_const ::= term_no_const_strong EQ term_no_const",
 /* 191 */ "comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const",
 /* 192 */ "comparison_no_const ::= term_no_const_strong NEQ term_no_const",
 /* 193 */ "comparison_no_const ::= term_no_const_strong LTHAN term_no_const",
 /* 194 */ "comparison_no_const ::= term_no_const_strong GTHAN term_no_const",
 /* 195 */ "comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const",
 /* 196 */ "comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const",
 /* 197 */ "atomic_formula_one_const ::= constant_one_const",
 /* 198 */ "atomic_formula_one_const ::= TILDE constant_one_const",
 /* 199 */ "atomic_formula_one_const ::= constant_one_const EQ term_no_const",
 /* 200 */ "formula_temporal ::= formula_base",
 /* 201 */ "formula_temporal ::= PAREN_L formula_temporal PAREN_R",
 /* 202 */ "formula_temporal ::= NOT formula_temporal",
 /* 203 */ "formula_temporal ::= DASH formula_temporal",
 /* 204 */ "formula_temporal ::= formula_temporal AMP formula_temporal",
 /* 205 */ "formula_temporal ::= formula_temporal DBL_PLUS formula_temporal",
 /* 206 */ "formula_temporal ::= formula_temporal PIPE formula_temporal",
 /* 207 */ "formula_temporal ::= formula_temporal EQUIV formula_temporal",
 /* 208 */ "formula_temporal ::= formula_temporal IMPL formula_temporal",
 /* 209 */ "formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal",
 /* 210 */ "formula_temporal ::= term_strong COLON formula_temporal",
 /* 211 */ "formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R",
 /* 212 */ "quant_lst ::= quant_op variable",
 /* 213 */ "quant_lst ::= quant_lst quant_op variable",
 /* 214 */ "quant_op ::= BIG_CONJ",
 /* 215 */ "quant_op ::= BIG_DISJ",
 /* 216 */ "formula_card ::= CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R",
 /* 217 */ "formula_card ::= term_strong CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R",
 /* 218 */ "formula_card ::= CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R term_strong",
 /* 219 */ "formula_card ::= term_strong CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R term_strong",
 /* 220 */ "card_af ::= constant_local",
 /* 221 */ "card_af ::= TILDE constant_local",
 /* 222 */ "card_af ::= constant_local EQ term_local",
 /* 223 */ "card_sort_bnd_lst ::=",
 /* 224 */ "card_sort_bnd_lst ::= card_sort_bnd_lst COLON sort PAREN_L variable_local PAREN_R",
 /* 225 */ "head_formula ::= comparison",
 /* 226 */ "head_formula ::= atomic_formula",
 /* 227 */ "head_formula ::= formula_card",
 /* 228 */ "head_formula ::= TRUE",
 /* 229 */ "head_formula ::= FALSE",
 /* 230 */ "head_formula ::= DASH constant",
 /* 231 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 232 */ "macro_def_lst ::= macro_bnd",
 /* 233 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 234 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 235 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 236 */ "macro_args ::= macro_arg",
 /* 237 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 238 */ "macro_arg ::= POUND_INTEGER",
 /* 239 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 240 */ "sort_lst ::= sort",
 /* 241 */ "sort_lst ::= sort_lst COMMA sort",
 /* 242 */ "sort ::= sort_id_nr STAR",
 /* 243 */ "sort ::= sort_id_nr CARROT",
 /* 244 */ "sort ::= sort_id_nr",
 /* 245 */ "sort_id_nr ::= sort_id",
 /* 246 */ "sort_id_nr ::= NUMBER_RANGE",
 /* 247 */ "sort_id ::= IDENTIFIER",
 /* 248 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 249 */ "constant_bnd_lst ::= constant_bnd",
 /* 250 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 251 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 252 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 253 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 254 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 255 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 256 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 257 */ "constant_dcl_type ::= ABACTION",
 /* 258 */ "constant_dcl_type ::= ACTION",
 /* 259 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 260 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 261 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 262 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 263 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 264 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 265 */ "constant_dcl_type ::= RIGID",
 /* 266 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 267 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 268 */ "object_bnd_lst ::= object_bnd",
 /* 269 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 270 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 271 */ "object_lst ::= object_spec",
 /* 272 */ "object_lst ::= object_lst COMMA object_spec",
 /* 273 */ "object_spec ::= IDENTIFIER",
 /* 274 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 275 */ "object_spec ::= NUMBER_RANGE",
 /* 276 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 277 */ "variable_bnd_lst ::= variable_bnd",
 /* 278 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 279 */ "variable_bnd ::= variable_lst DBL_COLON sort_id",
 /* 280 */ "variable_lst ::= IDENTIFIER",
 /* 281 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 282 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 283 */ "sort_bnd_lst ::= sort_bnd",
 /* 284 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 285 */ "sort_bnd ::= sort_dcl_lst",
 /* 286 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 287 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 288 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 289 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 290 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 291 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 292 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 293 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 294 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 295 */ "show_lst ::= show_elem",
 /* 296 */ "show_lst ::= show_lst COMMA show_elem",
 /* 297 */ "show_elem ::= atomic_formula_one_const",
 /* 298 */ "stmt_noconcurrency ::= NOCONCURRENCY",
 /* 299 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY",
 /* 300 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ INTEGER PERIOD",
 /* 301 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ INTEGER PERIOD",
 /* 302 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 303 */ "query_lst ::= formula_temporal",
 /* 304 */ "query_lst ::= query_maxstep_decl",
 /* 305 */ "query_lst ::= query_label_decl",
 /* 306 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 307 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 308 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 309 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 310 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON NUMBER_RANGE",
 /* 311 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 312 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 313 */ "clause_if ::= IF formula",
 /* 314 */ "clause_if ::=",
 /* 315 */ "clause_after ::= AFTER formula",
 /* 316 */ "clause_after ::=",
 /* 317 */ "clause_ifcons ::= IFCONS formula",
 /* 318 */ "clause_ifcons ::=",
 /* 319 */ "clause_unless ::= UNLESS atomic_formula",
 /* 320 */ "clause_unless ::=",
 /* 321 */ "clause_where ::= WHERE formula_no_const",
 /* 322 */ "clause_where ::=",
 /* 323 */ "stmt_law ::= law_basic",
 /* 324 */ "stmt_law ::= law_caused",
 /* 325 */ "stmt_law ::= law_pcaused",
 /* 326 */ "stmt_law ::= law_impl",
 /* 327 */ "stmt_law ::= law_causes",
 /* 328 */ "stmt_law ::= law_increments",
 /* 329 */ "stmt_law ::= law_mcause",
 /* 330 */ "stmt_law ::= law_always",
 /* 331 */ "stmt_law ::= law_constraint",
 /* 332 */ "stmt_law ::= law_impossible",
 /* 333 */ "stmt_law ::= law_never",
 /* 334 */ "stmt_law ::= law_default",
 /* 335 */ "stmt_law ::= law_exogenous",
 /* 336 */ "stmt_law ::= law_inertial",
 /* 337 */ "stmt_law ::= law_nonexecutable",
 /* 338 */ "stmt_law ::= law_rigid",
 /* 339 */ "stmt_law ::= law_observed",
 /* 340 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 341 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 342 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 343 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 344 */ "law_causes ::= formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 345 */ "law_increments ::= formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 346 */ "law_mcause ::= formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 347 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 348 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 349 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 350 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 351 */ "law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 352 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 353 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 354 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 355 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 356 */ "law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD",
 /* 357 */ "stmt_code_blk ::= ASP_GR",
 /* 358 */ "stmt_code_blk ::= ASP_CP",
 /* 359 */ "stmt_code_blk ::= F2LP_GR",
 /* 360 */ "stmt_code_blk ::= F2LP_CP",
 /* 361 */ "stmt_code_blk ::= LUA_GR",
 /* 362 */ "stmt_code_blk ::= LUA_CP",
};
#endif /* NDEBUG */


#if YYSTACKDEPTH<=0
/*
** Try to increase the size of the parser stack.
*/
static void yyGrowStack(yyParser *p){
  int newSize;
  yyStackEntry *pNew;

  newSize = p->yystksz*2 + 100;
  pNew = realloc(p->yystack, newSize*sizeof(pNew[0]));
  if( pNew ){
    p->yystack = pNew;
    p->yystksz = newSize;
#ifndef NDEBUG
    if( yyTraceFILE ){
      fprintf(yyTraceFILE,"%sStack grows to %d entries!\n",
              yyTracePrompt, p->yystksz);
    }
#endif
  }
}
#endif

/* 
** This function allocates a new parser.
** The only argument is a pointer to a function which works like
** malloc.
**
** Inputs:
** A pointer to the function used to allocate memory.
**
** Outputs:
** A pointer to a parser.  This pointer is used in subsequent calls
** to lemon_parser and lemon_parserFree.
*/
void *lemon_parserAlloc(void *(*mallocProc)(size_t)){
  yyParser *pParser;
  pParser = (yyParser*)(*mallocProc)( (size_t)sizeof(yyParser) );
  if( pParser ){
    pParser->yyidx = -1;
#ifdef YYTRACKMAXSTACKDEPTH
    pParser->yyidxMax = 0;
#endif
#if YYSTACKDEPTH<=0
    pParser->yystack = NULL;
    pParser->yystksz = 0;
    yyGrowStack(pParser);
#endif
	pParser->yylookmajor = YYNOCODE;
	pParser->yylookminor = yyzerominor;
	pParser->yysyntaxerr = 0;
  }
  return pParser;
}

/* The following function deletes the value associated with a
** symbol.  The symbol can be either a terminal or nonterminal.
** "yymajor" is the symbol code, and "yypminor" is a pointer to
** the value.
*/
static void yy_destructor(
  yyParser *yypParser,    /* The parser */
  YYCODETYPE yymajor,     /* Type code for object to destroy */
  YYMINORTYPE *yypminor   /* The object to be destroyed */
){
  lemon_parserARG_FETCH;
  switch( yymajor ){
    /* Here is inserted the actions which take place when a
    ** terminal or non-terminal is destroyed.  This can happen
    ** when the symbol is popped from the stack during a
    ** reduce or during error processing or when a parser is 
    ** being destroyed before it is finished parsing.
    **
    ** Note: during a reduce, the only symbols destroyed are those
    ** which appear on the RHS of the rule, but which are not used
    ** inside the C code.
    */
      /* TERMINAL Destructor */
    case 1: /* INTEGER */
    case 2: /* IDENTIFIER */
    case 3: /* POUND_IDENTIFIER */
    case 4: /* POUND_INTEGER */
    case 5: /* AT_IDENTIFIER */
    case 6: /* STRING_LITERAL */
    case 7: /* ASP_GR */
    case 8: /* ASP_CP */
    case 9: /* LUA_GR */
    case 10: /* LUA_CP */
    case 11: /* F2LP_GR */
    case 12: /* F2LP_CP */
    case 13: /* COMMENT */
    case 14: /* CONSTANTS */
    case 15: /* INCLUDE */
    case 16: /* MACROS */
    case 17: /* OBJECTS */
    case 18: /* QUERY */
    case 19: /* SHOW */
    case 20: /* SORTS */
    case 21: /* VARIABLES */
    case 22: /* ABACTION */
    case 23: /* ACTION */
    case 24: /* ADDITIVEACTION */
    case 25: /* ADDITIVEFLUENT */
    case 26: /* AFTER */
    case 27: /* ALL */
    case 28: /* ALWAYS */
    case 29: /* ASSUMING */
    case 30: /* ATTRIBUTE */
    case 31: /* BY */
    case 32: /* CAUSED */
    case 33: /* CAUSES */
    case 34: /* IMPOSSIBLE */
    case 35: /* CONSTRAINT */
    case 36: /* DECREMENTS */
    case 37: /* DEFAULT */
    case 38: /* EXTERNALACTION */
    case 39: /* EXOGENOUS */
    case 40: /* EXOGENOUSACTION */
    case 41: /* IF */
    case 42: /* IFCONS */
    case 43: /* INCREMENTS */
    case 44: /* INERTIAL */
    case 45: /* INERTIALFLUENT */
    case 46: /* LABEL */
    case 47: /* MAY_CAUSE */
    case 48: /* MAXADDITIVE */
    case 49: /* MAXAFVALUE */
    case 50: /* MAXSTEP */
    case 51: /* NEVER */
    case 52: /* NOCONCURRENCY */
    case 53: /* STRONG_NOCONCURRENCY */
    case 54: /* NONEXECUTABLE */
    case 55: /* OF */
    case 56: /* POSSIBLY_CAUSED */
    case 57: /* RIGID */
    case 58: /* SDFLUENT */
    case 59: /* SIMPLEFLUENT */
    case 60: /* EXTERNALFLUENT */
    case 61: /* UNLESS */
    case 62: /* WHERE */
    case 63: /* FALSE */
    case 64: /* NONE */
    case 65: /* TRUE */
    case 66: /* AT */
    case 67: /* BRACKET_L */
    case 68: /* BRACKET_R */
    case 69: /* COLON_DASH */
    case 70: /* CBRACKET_L */
    case 71: /* CBRACKET_R */
    case 72: /* PAREN_L */
    case 73: /* PAREN_R */
    case 74: /* PERIOD */
    case 75: /* MACRO_STRING */
    case 76: /* TILDE */
    case 77: /* DBL_COLON */
    case 78: /* ARROW_LEQ */
    case 79: /* ARROW_REQ */
    case 80: /* ARROW_LDASH */
    case 81: /* COLON */
    case 82: /* EQ */
    case 83: /* DBL_EQ */
    case 84: /* NEQ */
    case 85: /* NOT_EQ */
    case 86: /* LTHAN */
    case 87: /* GTHAN */
    case 88: /* LTHAN_EQ */
    case 89: /* GTHAN_EQ */
    case 90: /* DBL_PERIOD */
    case 91: /* BIG_CONJ */
    case 92: /* BIG_DISJ */
    case 93: /* POUND */
    case 94: /* SEMICOLON */
    case 95: /* EQUIV */
    case 96: /* IMPL */
    case 97: /* ARROW_RDASH */
    case 98: /* DBL_PLUS */
    case 99: /* PIPE */
    case 100: /* DBL_GTHAN */
    case 101: /* DBL_LTHAN */
    case 102: /* AMP */
    case 103: /* COMMA */
    case 104: /* DBL_AMP */
    case 105: /* NOT */
    case 106: /* DASH */
    case 107: /* PLUS */
    case 108: /* STAR */
    case 109: /* INT_DIV */
    case 110: /* MOD */
    case 111: /* ABS */
    case 112: /* UMINUS */
    case 113: /* CARROT */
    case 114: /* EOF */
    case 115: /* ERR_IO */
    case 116: /* ERR_UNKNOWN_SYMBOL */
    case 117: /* ERR_UNTERMINATED_STRING */
    case 118: /* ERR_UNTERMINATED_ASP */
    case 119: /* ERR_UNTERMINATED_LUA */
    case 120: /* ERR_UNTERMINATED_F2LP */
    case 121: /* ERR_UNTERMINATED_BLK_COMMENT */
    case 122: /* ERR_SYNTAX */
    case 123: /* ERR_PAREN_MISMATCH */
    case 124: /* ARG */
    case 125: /* NOOP */
    case 126: /* CONSTANT_ID */
    case 127: /* VARIABLE_ID */
    case 128: /* OBJECT_ID */
    case 129: /* NUMBER_RANGE */
    case 130: /* HIDE */
    case 131: /* OBSERVED */
{
#line 183 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));								
#line 2192 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 133: /* start */
    case 134: /* statement_lst */
    case 156: /* undeclared */
    case 168: /* undeclared_local */
{
#line 193 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2202 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 135: /* statement */
    case 141: /* stmt_code_blk */
    case 142: /* stmt_law */
    case 143: /* stmt_show */
    case 144: /* stmt_hide */
    case 147: /* stmt_maxafvalue */
    case 148: /* stmt_maxadditive */
{
#line 197 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy280));								
#line 2215 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 136: /* stmt_macro_def */
{
#line 218 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy111));								
#line 2222 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 137: /* stmt_constant_def */
{
#line 220 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy175));								
#line 2229 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 138: /* stmt_object_def */
{
#line 222 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy288));								
#line 2236 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 139: /* stmt_variable_def */
{
#line 224 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy267));								
#line 2243 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_sort_def */
{
#line 226 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy45));								
#line 2250 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 145: /* stmt_noconcurrency */
{
#line 236 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy57));								
#line 2257 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 146: /* stmt_strong_noconcurrency */
{
#line 238 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy298));								
#line 2264 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_query */
{
#line 244 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy170));								
#line 2271 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 150: /* base_elem */
    case 151: /* base_elem_no_const */
    case 158: /* term */
    case 161: /* term_no_const */
    case 162: /* base_elem_local */
    case 163: /* base_elem_local_no_const */
    case 170: /* term_local */
    case 171: /* term_strong */
    case 172: /* term_strong_candidate */
    case 173: /* term_no_const_strong */
{
#line 278 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy307));								
#line 2287 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 152: /* constant */
    case 159: /* constant_one_const */
    case 164: /* constant_local */
{
#line 282 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy97));								
#line 2296 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* object */
    case 165: /* object_local */
{
#line 284 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy438));								
#line 2304 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 154: /* variable */
{
#line 286 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy13));								
#line 2311 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 155: /* lua */
    case 167: /* lua_local */
{
#line 288 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy434));								
#line 2319 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 157: /* term_lst */
    case 160: /* term_no_const_lst */
    case 169: /* term_local_lst */
{
#line 292 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy467));								
#line 2328 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 166: /* variable_local */
{
#line 425 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy356));								
#line 2335 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 174: /* formula */
    case 175: /* formula_base */
    case 176: /* comparison */
    case 179: /* formula_card */
    case 180: /* formula_no_const */
    case 181: /* formula_no_const_base */
    case 182: /* comparison_no_const */
    case 184: /* formula_temporal */
    case 190: /* head_formula */
{
#line 702 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy393));								
#line 2350 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 177: /* atomic_formula */
    case 183: /* atomic_formula_one_const */
    case 187: /* card_af */
{
#line 708 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy202));								
#line 2359 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 178: /* formula_quant */
{
#line 710 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy101));								
#line 2366 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_lst */
{
#line 906 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy317));								
#line 2373 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* quant_op */
{
#line 908 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2380 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 188: /* card_sort_bnd_lst */
{
#line 947 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy240));								
#line 2387 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 189: /* sort */
    case 196: /* sort_id_nr */
    case 197: /* sort_id */
{
#line 1152 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2396 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 191: /* macro_def_lst */
{
#line 1054 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy49));                              
#line 2403 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 192: /* macro_bnd */
{
#line 1056 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy163));                              
#line 2410 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 193: /* macro_args */
{
#line 1058 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy50));                              
#line 2417 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 194: /* macro_arg */
{
#line 1060 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy203));                              
#line 2424 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 195: /* sort_lst */
{
#line 1150 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy315));							
#line 2431 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 198: /* constant_bnd_lst */
    case 199: /* constant_bnd */
{
#line 1269 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy145));									
#line 2439 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* constant_dcl_lst */
{
#line 1273 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy474));									
#line 2446 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* constant_dcl_type */
{
#line 1275 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2453 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* object_bnd_lst */
{
#line 1455 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy454));									
#line 2460 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* object_bnd */
{
#line 1457 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy470));									
#line 2467 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* object_lst */
    case 205: /* object_spec */
{
#line 1459 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy237));									
#line 2475 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 206: /* variable_bnd_lst */
    case 207: /* variable_bnd */
{
#line 1575 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy318));									
#line 2483 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 208: /* variable_lst */
{
#line 1579 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy208));									
#line 2490 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 209: /* sort_bnd_lst */
    case 210: /* sort_bnd */
    case 211: /* sort_dcl_lst */
{
#line 1652 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy295));									
#line 2499 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* show_lst */
{
#line 1756 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy395));									
#line 2506 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* show_elem */
    case 221: /* clause_unless */
{
#line 1758 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy202));									
#line 2514 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* query_lst */
{
#line 1910 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy181).l); DEALLOC((yypminor->yy181).maxstep); DEALLOC((yypminor->yy181).label);	
#line 2521 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* query_maxstep_decl */
{
#line 1912 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy203));												
#line 2528 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* query_label_Decl */
{
#line 1914 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2535 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 218: /* clause_if */
    case 219: /* clause_after */
    case 220: /* clause_ifcons */
    case 222: /* clause_where */
{
#line 2052 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy393));									
#line 2545 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 223: /* law_basic */
    case 224: /* law_caused */
    case 225: /* law_pcaused */
    case 226: /* law_impl */
    case 227: /* law_causes */
    case 228: /* law_increments */
    case 229: /* law_mcause */
    case 230: /* law_always */
    case 231: /* law_constraint */
    case 232: /* law_impossible */
    case 233: /* law_never */
    case 234: /* law_default */
    case 235: /* law_exogenous */
    case 236: /* law_inertial */
    case 237: /* law_nonexecutable */
    case 238: /* law_rigid */
    case 239: /* law_observed */
{
#line 2093 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy280));									
#line 2568 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    default:  break;   /* If no destructor action specified: do nothing */
  }
  lemon_parserARG_STORE;
}

/*
** Pop the parser's stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
**
** Return the major token number for the symbol popped.
*/
static int yy_pop_parser_stack(yyParser *pParser){
  YYCODETYPE yymajor;
  yyStackEntry *yytos = &pParser->yystack[pParser->yyidx];

  if( pParser->yyidx<0 ) return 0;
#ifndef NDEBUG
  if( yyTraceFILE && pParser->yyidx>=0 ){
    fprintf(yyTraceFILE,"%sPopping %s\n",
      yyTracePrompt,
      yyTokenName[yytos->major]);
  }
#endif
  yymajor = yytos->major;
  yy_destructor(pParser, yymajor, &yytos->minor);
  pParser->yyidx--;
  return yymajor;
}

/* 
** Deallocate and destroy a parser.  Destructors are all called for
** all stack elements before shutting the parser down.
**
** Inputs:
** <ul>
** <li>  A pointer to the parser.  This should be a pointer
**       obtained from lemon_parserAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
void lemon_parserFree(
  void *p,                    /* The parser to be deleted */
  void (*freeProc)(void*)     /* Function used to reclaim memory */
){
  yyParser *pParser = (yyParser*)p;
  if( pParser==0 ) return;
  if( pParser->yylookmajor != YYNOCODE ) yy_destructor(pParser, (YYCODETYPE)pParser->yylookmajor, &pParser->yylookminor);
  while( pParser->yyidx>=0 ) yy_pop_parser_stack(pParser);
#if YYSTACKDEPTH<=0
  free(pParser->yystack);
#endif
  (*freeProc)((void*)pParser);
}

/*
** Return the peak depth of the stack for a parser.
*/
#ifdef YYTRACKMAXSTACKDEPTH
int lemon_parserStackPeak(void *p){
  yyParser *pParser = (yyParser*)p;
  return pParser->yyidxMax;
}
#endif

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_shift_action(
  yyParser *pParser,        /* The parser */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
  int stateno = pParser->yystack[pParser->yyidx].stateno;
 
  if( stateno>YY_SHIFT_COUNT
   || (i = yy_shift_ofst[stateno])==YY_SHIFT_USE_DFLT ){
    return yy_default[stateno];
  }
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
  if( i<0 || i>=YY_ACTTAB_COUNT || yy_lookahead[i]!=iLookAhead ){
    if( iLookAhead>0 ){
#ifdef YYFALLBACK
      YYCODETYPE iFallback;            /* Fallback token */
      if( iLookAhead<sizeof(yyFallback)/sizeof(yyFallback[0])
             && (iFallback = yyFallback[iLookAhead])!=0 ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE, "%sFALLBACK %s => %s\n",
             yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[iFallback]);
        }
#endif
        return yy_find_shift_action(pParser, iFallback);
      }
#endif
#ifdef YYWILDCARD
      {
        int j = i - iLookAhead + YYWILDCARD;
        if( 
#if YY_SHIFT_MIN+YYWILDCARD<0
          j>=0 &&
#endif
#if YY_SHIFT_MAX+YYWILDCARD>=YY_ACTTAB_COUNT
          j<YY_ACTTAB_COUNT &&
#endif
          yy_lookahead[j]==YYWILDCARD
        ){
#ifndef NDEBUG
          if( yyTraceFILE ){
            fprintf(yyTraceFILE, "%sWILDCARD %s => %s\n",
               yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[YYWILDCARD]);
          }
#endif /* NDEBUG */
          return yy_action[j];
        }
      }
#endif /* YYWILDCARD */
    }
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_reduce_action(
  int stateno,              /* Current state number */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
#ifdef YYERRORSYMBOL
  if( stateno>YY_REDUCE_COUNT ){
    return yy_default[stateno];
  }
#else
  assert( stateno<=YY_REDUCE_COUNT );
#endif
  i = yy_reduce_ofst[stateno];
  assert( i!=YY_REDUCE_USE_DFLT );
  if (iLookAhead == YYNOCODE) return YY_NO_ACTION;
  assert( iLookAhead!=YYNOCODE );
  i += iLookAhead;
#ifdef YYERRORSYMBOL
  if( i<0 || i>=YY_ACTTAB_COUNT || yy_lookahead[i]!=iLookAhead ){
    return yy_default[stateno];
  }
#else
  assert( i>=0 && i<YY_ACTTAB_COUNT );
  assert( yy_lookahead[i]==iLookAhead );
#endif
  return yy_action[i];
}

/*
** The following routine is called if the stack overflows.
*/
static void yyStackOverflow(yyParser *yypParser /*, YYMINORTYPE *yypMinor */){
   lemon_parserARG_FETCH;
   yypParser->yyidx--;
#ifndef NDEBUG
   if( yyTraceFILE ){
     fprintf(yyTraceFILE,"%sStack Overflow!\n",yyTracePrompt);
   }
#endif
   while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
   lemon_parserARG_STORE; /* Suppress warning about unused %extra_argument var */
}

/*
** Perform a shift action.
*/
static void yy_shift(
  yyParser *yypParser,          /* The parser to be shifted */
  int yyNewState,               /* The new state to shift in */
  int yyMajor,                  /* The major token to shift in */
  YYMINORTYPE *yypMinor         /* Pointer to the minor token to shift in */
){
  yyStackEntry *yytos;
  yypParser->yyidx++;
#ifdef YYTRACKMAXSTACKDEPTH
  if( yypParser->yyidx>yypParser->yyidxMax ){
    yypParser->yyidxMax = yypParser->yyidx;
  }
#endif
#if YYSTACKDEPTH>0 
  if( yypParser->yyidx>=YYSTACKDEPTH ){
    yyStackOverflow(yypParser/*, yypMinor */);
    return;
  }
#else
  if( yypParser->yyidx>=yypParser->yystksz ){
    yyGrowStack(yypParser);
    if( yypParser->yyidx>=yypParser->yystksz ){
      yyStackOverflow(yypParser/*, yypMinor */);
      return;
    }
  }
#endif
  yytos = &yypParser->yystack[yypParser->yyidx];
  yytos->stateno = (YYACTIONTYPE)yyNewState;
  yytos->major = (YYCODETYPE)yyMajor;
  yytos->minor = *yypMinor;
#ifndef NDEBUG
  if( yyTraceFILE && yypParser->yyidx>0 ){
    int i;
    fprintf(yyTraceFILE,"%sShift %d\n",yyTracePrompt,yyNewState);
    fprintf(yyTraceFILE,"%sStack:",yyTracePrompt);
    for(i=1; i<=yypParser->yyidx; i++)
      fprintf(yyTraceFILE," %s",yyTokenName[yypParser->yystack[i].major]);
    fprintf(yyTraceFILE,"\n");
  }
#endif
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
static const struct {
  YYCODETYPE lhs;         /* Symbol on the left-hand side of the rule */
  unsigned char nrhs;     /* Number of right-hand side symbols in the rule */
} yyRuleInfo[] = {
  { 133, 2 },
  { 134, 0 },
  { 134, 2 },
  { 134, 2 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 135, 1 },
  { 150, 1 },
  { 150, 1 },
  { 151, 1 },
  { 151, 1 },
  { 151, 1 },
  { 152, 4 },
  { 152, 1 },
  { 153, 4 },
  { 153, 1 },
  { 154, 1 },
  { 155, 4 },
  { 155, 1 },
  { 157, 1 },
  { 157, 3 },
  { 159, 4 },
  { 159, 1 },
  { 160, 1 },
  { 160, 3 },
  { 162, 1 },
  { 162, 1 },
  { 163, 1 },
  { 163, 1 },
  { 163, 1 },
  { 163, 1 },
  { 164, 4 },
  { 164, 1 },
  { 165, 4 },
  { 165, 1 },
  { 166, 1 },
  { 166, 1 },
  { 167, 4 },
  { 167, 1 },
  { 169, 1 },
  { 169, 3 },
  { 158, 1 },
  { 158, 1 },
  { 158, 1 },
  { 158, 3 },
  { 158, 1 },
  { 158, 1 },
  { 158, 1 },
  { 158, 1 },
  { 158, 1 },
  { 158, 2 },
  { 158, 2 },
  { 158, 3 },
  { 158, 3 },
  { 158, 3 },
  { 158, 3 },
  { 158, 3 },
  { 171, 1 },
  { 171, 1 },
  { 171, 1 },
  { 171, 3 },
  { 171, 1 },
  { 171, 1 },
  { 171, 1 },
  { 171, 2 },
  { 171, 2 },
  { 172, 2 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 170, 1 },
  { 170, 1 },
  { 170, 1 },
  { 170, 3 },
  { 170, 1 },
  { 170, 1 },
  { 170, 1 },
  { 170, 1 },
  { 170, 1 },
  { 170, 2 },
  { 170, 2 },
  { 170, 3 },
  { 170, 3 },
  { 170, 3 },
  { 170, 3 },
  { 170, 3 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 173, 3 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 173, 2 },
  { 173, 2 },
  { 173, 3 },
  { 173, 3 },
  { 173, 3 },
  { 173, 3 },
  { 173, 3 },
  { 161, 1 },
  { 161, 1 },
  { 161, 1 },
  { 161, 3 },
  { 161, 1 },
  { 161, 1 },
  { 161, 1 },
  { 161, 2 },
  { 161, 2 },
  { 161, 3 },
  { 161, 3 },
  { 161, 3 },
  { 161, 3 },
  { 161, 3 },
  { 174, 1 },
  { 174, 3 },
  { 174, 2 },
  { 174, 2 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 175, 1 },
  { 175, 1 },
  { 175, 1 },
  { 175, 1 },
  { 175, 1 },
  { 175, 1 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 176, 3 },
  { 177, 1 },
  { 177, 2 },
  { 177, 3 },
  { 180, 1 },
  { 180, 3 },
  { 180, 2 },
  { 180, 2 },
  { 180, 3 },
  { 180, 3 },
  { 180, 3 },
  { 180, 3 },
  { 180, 3 },
  { 180, 3 },
  { 181, 1 },
  { 181, 1 },
  { 181, 1 },
  { 182, 3 },
  { 182, 3 },
  { 182, 3 },
  { 182, 3 },
  { 182, 3 },
  { 182, 3 },
  { 182, 3 },
  { 183, 1 },
  { 183, 2 },
  { 183, 3 },
  { 184, 1 },
  { 184, 3 },
  { 184, 2 },
  { 184, 2 },
  { 184, 3 },
  { 184, 3 },
  { 184, 3 },
  { 184, 3 },
  { 184, 3 },
  { 184, 3 },
  { 184, 3 },
  { 178, 5 },
  { 185, 2 },
  { 185, 3 },
  { 186, 1 },
  { 186, 1 },
  { 179, 4 },
  { 179, 5 },
  { 179, 5 },
  { 179, 6 },
  { 187, 1 },
  { 187, 2 },
  { 187, 3 },
  { 188, 0 },
  { 188, 6 },
  { 190, 1 },
  { 190, 1 },
  { 190, 1 },
  { 190, 1 },
  { 190, 1 },
  { 190, 2 },
  { 136, 4 },
  { 191, 1 },
  { 191, 3 },
  { 192, 6 },
  { 192, 3 },
  { 193, 1 },
  { 193, 3 },
  { 194, 1 },
  { 194, 1 },
  { 195, 1 },
  { 195, 3 },
  { 189, 2 },
  { 189, 2 },
  { 189, 1 },
  { 196, 1 },
  { 196, 1 },
  { 197, 1 },
  { 137, 4 },
  { 198, 1 },
  { 198, 3 },
  { 199, 6 },
  { 199, 3 },
  { 200, 1 },
  { 200, 4 },
  { 200, 3 },
  { 200, 6 },
  { 201, 1 },
  { 201, 1 },
  { 201, 1 },
  { 201, 1 },
  { 201, 1 },
  { 201, 1 },
  { 201, 1 },
  { 201, 1 },
  { 201, 1 },
  { 201, 1 },
  { 138, 4 },
  { 202, 1 },
  { 202, 3 },
  { 203, 3 },
  { 204, 1 },
  { 204, 3 },
  { 205, 1 },
  { 205, 4 },
  { 205, 1 },
  { 139, 4 },
  { 206, 1 },
  { 206, 3 },
  { 207, 3 },
  { 208, 1 },
  { 208, 3 },
  { 140, 4 },
  { 209, 1 },
  { 209, 3 },
  { 210, 1 },
  { 210, 3 },
  { 210, 3 },
  { 210, 3 },
  { 211, 1 },
  { 211, 3 },
  { 143, 4 },
  { 143, 4 },
  { 144, 4 },
  { 144, 4 },
  { 212, 1 },
  { 212, 3 },
  { 213, 1 },
  { 145, 1 },
  { 146, 1 },
  { 147, 5 },
  { 148, 5 },
  { 149, 4 },
  { 214, 1 },
  { 214, 1 },
  { 214, 1 },
  { 214, 3 },
  { 214, 3 },
  { 214, 3 },
  { 215, 3 },
  { 215, 3 },
  { 216, 3 },
  { 216, 3 },
  { 218, 2 },
  { 218, 0 },
  { 219, 2 },
  { 219, 0 },
  { 220, 2 },
  { 220, 0 },
  { 221, 2 },
  { 221, 0 },
  { 222, 2 },
  { 222, 0 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 142, 1 },
  { 223, 7 },
  { 224, 8 },
  { 225, 8 },
  { 226, 5 },
  { 227, 7 },
  { 228, 9 },
  { 229, 7 },
  { 230, 6 },
  { 231, 6 },
  { 232, 6 },
  { 233, 6 },
  { 234, 8 },
  { 235, 8 },
  { 236, 8 },
  { 237, 6 },
  { 238, 4 },
  { 239, 5 },
  { 141, 1 },
  { 141, 1 },
  { 141, 1 },
  { 141, 1 },
  { 141, 1 },
  { 141, 1 },
};

/*
** Flags that a syntax error has occurred. 
*/
#define YYERROR { yypParser->yysyntaxerr = 1; yypParser->yyerrcnt = 3; }

static void yy_accept(yyParser*);  /* Forward Declaration */


/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
static void yy_reduce(
  yyParser *yypParser,         /* The parser */
  int yyruleno                 /* Number of the rule by which to reduce */
){
  int yygoto;                     /* The next state */
  int yyact;                      /* The next action */
  YYMINORTYPE yygotominor;        /* The LHS of the rule reduced */
  yyStackEntry *yymsp;            /* The top of the parser's stack */
  int yysize;                     /* Amount to pop the stack */
  lemon_parserARG_FETCH;
  yymsp = &yypParser->yystack[yypParser->yyidx];
#ifndef NDEBUG
  if( yyTraceFILE && yyruleno>=0 
        && yyruleno<(int)(sizeof(yyRuleName)/sizeof(yyRuleName[0])) ){
    fprintf(yyTraceFILE, "%sReduce [%s].\n", yyTracePrompt,
      yyRuleName[yyruleno]);
  }
#endif /* NDEBUG */

  /* Silence complaints from purify about yygotominor being uninitialized
  ** in some cases when it is copied into the stack after the following
  ** switch.  yygotominor is uninitialized when a rule reduces that does
  ** not set the value of its left-hand side nonterminal.  Leaving the
  ** value of the nonterminal uninitialized is utterly harmless as long
  ** as the value is never used.  So really the only thing this code
  ** accomplishes is to quieten purify.  
  **
  ** 2007-01-16:  The wireshark project (www.wireshark.org) reports that
  ** without this code, their parser segfaults.  I'm not sure what there
  ** parser is doing to make this happen.  This is the second bug report
  ** from wireshark this week.  Clearly they are stressing Lemon in ways
  ** that it has not been previously stressed...  (SQLite ticket #2172)
  */
  /*memset(&yygotominor, 0, sizeof(yygotominor));*/
  yygotominor = yyzerominor;

  switch( yyruleno ){
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line <lineno> <grammarfile>
  **     { ... }           // User supplied code
  **  #line <lineno> <thisfile>
  **     break;
  */
      case 0: /* start ::= statement_lst EOF */
#line 199 "bcplus/parser/detail/lemon_parser.y"
{
  yy_destructor(yypParser,114,&yymsp[0].minor);
}
#line 3236 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 204 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy280;
			yymsp[0].minor.yy280  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3245 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 247 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy280 = yymsp[0].minor.yy111; }
#line 3250 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 248 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy280 = yymsp[0].minor.yy175; }
#line 3255 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 249 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy280 = yymsp[0].minor.yy288; }
#line 3260 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 250 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy280 = yymsp[0].minor.yy267; }
#line 3265 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 251 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy280 = yymsp[0].minor.yy45; }
#line 3270 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 252 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy280 = yymsp[0].minor.yy280; }
#line 3280 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 256 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy280 = yymsp[0].minor.yy57; }
#line 3285 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 257 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy280 = yymsp[0].minor.yy298; }
#line 3290 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 260 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy280 = yymsp[0].minor.yy170; }
#line 3295 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
      case 36: /* base_elem_local ::= constant_local */ yytestcase(yyruleno==36);
#line 302 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy307 = yymsp[0].minor.yy97; }
#line 3301 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 37: /* base_elem_local ::= base_elem_local_no_const */ yytestcase(yyruleno==37);
      case 52: /* term ::= base_elem */ yytestcase(yyruleno==52);
      case 68: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==68);
      case 93: /* term_local ::= base_elem_local */ yytestcase(yyruleno==93);
      case 109: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==109);
      case 124: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==124);
#line 303 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy307 = yymsp[0].minor.yy307; }
#line 3312 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
      case 38: /* base_elem_local_no_const ::= object_local */ yytestcase(yyruleno==38);
#line 305 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy307 = yymsp[0].minor.yy438;	}
#line 3318 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
      case 39: /* base_elem_local_no_const ::= variable */ yytestcase(yyruleno==39);
#line 306 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy307 = yymsp[0].minor.yy13; }
#line 3324 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
      case 41: /* base_elem_local_no_const ::= lua_local */ yytestcase(yyruleno==41);
#line 307 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy307 = yymsp[0].minor.yy434; }
#line 3330 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 32: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==32);
      case 42: /* constant_local ::= CONSTANT_ID PAREN_L term_local_lst PAREN_R */ yytestcase(yyruleno==42);
#line 361 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy97, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy467, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3337 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 33: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==33);
      case 43: /* constant_local ::= CONSTANT_ID */ yytestcase(yyruleno==43);
#line 362 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy97, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3344 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
      case 44: /* object_local ::= OBJECT_ID PAREN_L term_local_lst PAREN_R */ yytestcase(yyruleno==44);
#line 363 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy438, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy467, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3350 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* object ::= OBJECT_ID */
      case 45: /* object_local ::= OBJECT_ID */ yytestcase(yyruleno==45);
#line 364 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy438, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3356 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* variable ::= VARIABLE_ID */
#line 366 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy13 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::VARIABLE)) {
			parser->_feature_error(Language::Feature::VARIABLE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			BASE_ELEM_BARE_DEF(yygotominor.yy13, yymsp[0].minor.yy0, Symbol::Type::VARIABLE, Variable, VariableSymbol); 
		}
	}
#line 3371 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
      case 48: /* lua_local ::= AT_IDENTIFIER PAREN_L term_local_lst PAREN_R */ yytestcase(yyruleno==48);
#line 377 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy434, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy467, yymsp[0].minor.yy0); }
#line 3377 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* lua ::= AT_IDENTIFIER */
      case 49: /* lua_local ::= AT_IDENTIFIER */ yytestcase(yyruleno==49);
#line 378 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy434, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3383 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* term_lst ::= term */
      case 34: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==34);
      case 50: /* term_local_lst ::= term_local */ yytestcase(yyruleno==50);
#line 383 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy467 = new TermList();
			yygotominor.yy467->push_back(yymsp[0].minor.yy307);
		}
#line 3393 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* term_lst ::= term_lst COMMA term */
      case 35: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==35);
      case 51: /* term_local_lst ::= term_local_lst COMMA term_local */ yytestcase(yyruleno==51);
#line 389 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy467 = yymsp[-2].minor.yy467;
			yymsp[-2].minor.yy467->push_back(yymsp[0].minor.yy307);
		  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 3404 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 40: /* base_elem_local_no_const ::= variable_local */
#line 439 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy307 = yymsp[0].minor.yy356; }
#line 3409 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 46: /* variable_local ::= POUND_IDENTIFIER */
      case 47: /* variable_local ::= POUND_INTEGER */ yytestcase(yyruleno==47);
#line 448 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy356 = new LocalVariable(yymsp[0].minor.yy0->str(), yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); delete yymsp[0].minor.yy0; }
#line 3415 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= INTEGER */
      case 69: /* term_strong ::= INTEGER */ yytestcase(yyruleno==69);
      case 94: /* term_local ::= INTEGER */ yytestcase(yyruleno==94);
      case 110: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==110);
      case 125: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==125);
#line 545 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy307, yymsp[0].minor.yy0);	}
#line 3424 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= STRING_LITERAL */
      case 56: /* term ::= TRUE */ yytestcase(yyruleno==56);
      case 57: /* term ::= FALSE */ yytestcase(yyruleno==57);
      case 70: /* term_strong ::= STRING_LITERAL */ yytestcase(yyruleno==70);
      case 95: /* term_local ::= STRING_LITERAL */ yytestcase(yyruleno==95);
      case 97: /* term_local ::= TRUE */ yytestcase(yyruleno==97);
      case 98: /* term_local ::= FALSE */ yytestcase(yyruleno==98);
      case 111: /* term_no_const_strong ::= STRING_LITERAL */ yytestcase(yyruleno==111);
      case 126: /* term_no_const ::= STRING_LITERAL */ yytestcase(yyruleno==126);
      case 128: /* term_no_const ::= TRUE */ yytestcase(yyruleno==128);
      case 129: /* term_no_const ::= FALSE */ yytestcase(yyruleno==129);
#line 546 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy307, yymsp[0].minor.yy0); }
#line 3439 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= PAREN_L term PAREN_R */
      case 71: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==71);
      case 96: /* term_local ::= PAREN_L term_local PAREN_R */ yytestcase(yyruleno==96);
      case 112: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==112);
      case 127: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==127);
#line 547 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy307, yymsp[-2].minor.yy0, yymsp[-1].minor.yy307, yymsp[0].minor.yy0); }
#line 3448 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 58: /* term ::= MAXSTEP */
      case 72: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==72);
      case 99: /* term_local ::= MAXSTEP */ yytestcase(yyruleno==99);
      case 113: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==113);
#line 550 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy307, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3456 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 59: /* term ::= MAXADDITIVE */
      case 73: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==73);
      case 100: /* term_local ::= MAXADDITIVE */ yytestcase(yyruleno==100);
      case 114: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==114);
#line 551 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy307, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3464 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 60: /* term ::= MAXAFVALUE */
      case 74: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==74);
      case 101: /* term_local ::= MAXAFVALUE */ yytestcase(yyruleno==101);
      case 115: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==115);
#line 552 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy307, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3472 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 61: /* term ::= DASH term */
      case 75: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==75);
      case 102: /* term_local ::= DASH term_local */ yytestcase(yyruleno==102);
      case 117: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==117);
      case 131: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==131);
#line 556 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy307, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, UnaryTerm::Operator::NEGATIVE); }
#line 3481 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 62: /* term ::= ABS term */
      case 76: /* term_strong ::= ABS term */ yytestcase(yyruleno==76);
      case 103: /* term_local ::= ABS term_local */ yytestcase(yyruleno==103);
      case 118: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==118);
      case 132: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==132);
#line 557 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy307, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, UnaryTerm::Operator::ABS); }
#line 3490 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 63: /* term ::= term DASH term */
      case 78: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==78);
      case 88: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==88);
      case 104: /* term_local ::= term_local DASH term_local */ yytestcase(yyruleno==104);
      case 119: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==119);
      case 133: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==133);
#line 561 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy307, yymsp[-2].minor.yy307, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, BinaryTerm::Operator::MINUS); }
#line 3500 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 64: /* term ::= term PLUS term */
      case 79: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==79);
      case 89: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==89);
      case 105: /* term_local ::= term_local PLUS term_local */ yytestcase(yyruleno==105);
      case 120: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==120);
      case 134: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==134);
#line 562 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy307, yymsp[-2].minor.yy307, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, BinaryTerm::Operator::PLUS); }
#line 3510 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 65: /* term ::= term STAR term */
      case 80: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==80);
      case 90: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==90);
      case 106: /* term_local ::= term_local STAR term_local */ yytestcase(yyruleno==106);
      case 121: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==121);
      case 135: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==135);
#line 563 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy307, yymsp[-2].minor.yy307, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, BinaryTerm::Operator::TIMES); }
#line 3520 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 66: /* term ::= term INT_DIV term */
      case 81: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==81);
      case 91: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==91);
      case 107: /* term_local ::= term_local INT_DIV term_local */ yytestcase(yyruleno==107);
      case 122: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==122);
      case 136: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==136);
#line 564 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy307, yymsp[-2].minor.yy307, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, BinaryTerm::Operator::DIVIDE); }
#line 3530 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 67: /* term ::= term MOD term */
      case 82: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==82);
      case 92: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==92);
      case 108: /* term_local ::= term_local MOD term_local */ yytestcase(yyruleno==108);
      case 123: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==123);
      case 137: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==137);
#line 565 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy307, yymsp[-2].minor.yy307, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, BinaryTerm::Operator::MOD); }
#line 3540 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong_candidate ::= DASH constant */
#line 584 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy307, yymsp[-1].minor.yy0, yymsp[0].minor.yy97, UnaryTerm::Operator::NEGATIVE); }
#line 3545 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 83: /* term_strong ::= constant DASH term */
#line 593 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy307, yymsp[-2].minor.yy97, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, BinaryTerm::Operator::MINUS); }
#line 3550 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 84: /* term_strong ::= constant PLUS term */
#line 594 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy307, yymsp[-2].minor.yy97, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, BinaryTerm::Operator::PLUS); }
#line 3555 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 85: /* term_strong ::= constant STAR term */
#line 595 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy307, yymsp[-2].minor.yy97, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, BinaryTerm::Operator::TIMES); }
#line 3560 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 86: /* term_strong ::= constant INT_DIV term */
#line 596 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy307, yymsp[-2].minor.yy97, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, BinaryTerm::Operator::DIVIDE); }
#line 3565 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 87: /* term_strong ::= constant MOD term */
#line 597 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy307, yymsp[-2].minor.yy97, yymsp[-1].minor.yy0, yymsp[0].minor.yy307, BinaryTerm::Operator::MOD); }
#line 3570 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 116: /* term_no_const_strong ::= constant */
      case 130: /* term_no_const ::= constant */ yytestcase(yyruleno==130);
#line 643 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy307 default to undeclared identifiers
		yygotominor.yy307 = NULL;
		ref_ptr<Referenced> c_ptr = yymsp[0].minor.yy97;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy97->beginLoc());
		YYERROR;
	}
#line 3582 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* formula ::= formula_base */
      case 177: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==177);
      case 200: /* formula_temporal ::= formula_base */ yytestcase(yyruleno==200);
#line 748 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = yymsp[0].minor.yy393;				}
#line 3589 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* formula ::= PAREN_L formula PAREN_R */
      case 178: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==178);
      case 201: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==201);
#line 749 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = yymsp[-1].minor.yy393; yygotominor.yy393->parens(true); 	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3598 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* formula ::= NOT formula */
      case 179: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==179);
      case 202: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==202);
#line 750 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy393, yymsp[-1].minor.yy0, yymsp[0].minor.yy393, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 3605 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* formula ::= DASH formula */
      case 180: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==180);
      case 203: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==203);
#line 751 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy393, yymsp[-1].minor.yy0, yymsp[0].minor.yy393, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 3612 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* formula ::= formula AMP formula */
      case 181: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==181);
      case 204: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==204);
#line 752 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy393, yymsp[0].minor.yy393, yymsp[-2].minor.yy393->beginLoc(), yymsp[0].minor.yy393->endLoc());   yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3620 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* formula ::= formula DBL_PLUS formula */
      case 144: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==144);
      case 182: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==182);
      case 183: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==183);
      case 205: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==205);
      case 206: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==206);
#line 753 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy393, yymsp[-2].minor.yy393, yymsp[-1].minor.yy0, yymsp[0].minor.yy393, BinaryFormula::Operator::OR); }
#line 3630 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 145: /* formula ::= formula EQUIV formula */
      case 184: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==184);
      case 207: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==207);
#line 755 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy393, yymsp[-2].minor.yy393, yymsp[-1].minor.yy0, yymsp[0].minor.yy393, BinaryFormula::Operator::EQUIV); }
#line 3637 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 146: /* formula ::= formula IMPL formula */
      case 147: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==147);
      case 185: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==185);
      case 186: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==186);
      case 208: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==208);
      case 209: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==209);
#line 756 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy393, yymsp[-2].minor.yy393, yymsp[-1].minor.yy0, yymsp[0].minor.yy393, BinaryFormula::Operator::IMPL); }
#line 3647 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 148: /* formula_base ::= comparison */
      case 187: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==187);
      case 225: /* head_formula ::= comparison */ yytestcase(yyruleno==225);
#line 759 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = yymsp[0].minor.yy393; }
#line 3654 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 149: /* formula_base ::= atomic_formula */
      case 226: /* head_formula ::= atomic_formula */ yytestcase(yyruleno==226);
#line 760 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = yymsp[0].minor.yy202; }
#line 3660 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 150: /* formula_base ::= formula_quant */
#line 761 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = yymsp[0].minor.yy101; }
#line 3665 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 151: /* formula_base ::= formula_card */
#line 763 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy393 = yymsp[0].minor.yy393;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy393->beginLoc());
			YYERROR;
		}
	}
#line 3676 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* formula_base ::= TRUE */
      case 188: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==188);
      case 228: /* head_formula ::= TRUE */ yytestcase(yyruleno==228);
#line 770 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3683 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* formula_base ::= FALSE */
      case 189: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==189);
      case 229: /* head_formula ::= FALSE */ yytestcase(yyruleno==229);
#line 771 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3690 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* comparison ::= term_strong EQ term */
      case 161: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==161);
      case 190: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==190);
#line 773 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy307, yymsp[0].minor.yy307, yymsp[-2].minor.yy307->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3698 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* comparison ::= term_strong DBL_EQ term */
      case 162: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==162);
      case 191: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==191);
#line 774 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy307, yymsp[0].minor.yy307, yymsp[-2].minor.yy307->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3706 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* comparison ::= term_strong NEQ term */
      case 163: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==163);
      case 192: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==192);
#line 775 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy307, yymsp[0].minor.yy307, yymsp[-2].minor.yy307->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3714 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= term_strong LTHAN term */
      case 164: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==164);
      case 193: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==193);
#line 776 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy307, yymsp[0].minor.yy307, yymsp[-2].minor.yy307->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3722 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* comparison ::= term_strong GTHAN term */
      case 165: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==165);
      case 194: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==194);
#line 777 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy307, yymsp[0].minor.yy307, yymsp[-2].minor.yy307->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3730 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* comparison ::= term_strong LTHAN_EQ term */
      case 166: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==166);
      case 195: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==195);
#line 778 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy307, yymsp[0].minor.yy307, yymsp[-2].minor.yy307->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3738 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* comparison ::= term_strong GTHAN_EQ term */
      case 167: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==167);
      case 196: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==196);
#line 779 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy307, yymsp[0].minor.yy307, yymsp[-2].minor.yy307->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 3746 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 168: /* comparison ::= constant DBL_EQ term */
#line 787 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy97, yymsp[0].minor.yy307, yymsp[-2].minor.yy97->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3752 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 169: /* comparison ::= constant NEQ term */
#line 788 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy97, yymsp[0].minor.yy307, yymsp[-2].minor.yy97->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3758 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 170: /* comparison ::= constant LTHAN term */
#line 789 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy97, yymsp[0].minor.yy307, yymsp[-2].minor.yy97->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3764 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 171: /* comparison ::= constant GTHAN term */
#line 790 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy97, yymsp[0].minor.yy307, yymsp[-2].minor.yy97->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3770 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 172: /* comparison ::= constant LTHAN_EQ term */
#line 791 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy97, yymsp[0].minor.yy307, yymsp[-2].minor.yy97->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3776 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 173: /* comparison ::= constant GTHAN_EQ term */
#line 792 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy97, yymsp[0].minor.yy307, yymsp[-2].minor.yy97->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 3782 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 174: /* atomic_formula ::= constant */
      case 197: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==197);
      case 220: /* card_af ::= constant_local */ yytestcase(yyruleno==220);
#line 822 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy202, yymsp[0].minor.yy97, "true"); }
#line 3789 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 175: /* atomic_formula ::= TILDE constant */
      case 198: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==198);
      case 221: /* card_af ::= TILDE constant_local */ yytestcase(yyruleno==221);
#line 823 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy202, yymsp[0].minor.yy97, "false");   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 3797 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 176: /* atomic_formula ::= constant EQ term */
      case 199: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==199);
#line 824 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy202 = new AtomicFormula(yymsp[-2].minor.yy97, yymsp[0].minor.yy307, yymsp[-2].minor.yy97->beginLoc(), yymsp[0].minor.yy307->endLoc());	  yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3804 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* formula_temporal ::= term_strong COLON formula_temporal */
#line 899 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy393, yymsp[-2].minor.yy307, yymsp[-1].minor.yy0, yymsp[0].minor.yy393); }
#line 3809 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
#line 911 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy101=NULL;
		ref_ptr<const Token> bl_ptr = yymsp[-4].minor.yy0;
		ref_ptr<QuantifierFormula::QuantifierList> lst_ptr = yymsp[-3].minor.yy317;
		ref_ptr<Formula> sub_ptr = yymsp[-1].minor.yy393;
		ref_ptr<const Token> br_ptr = yymsp[0].minor.yy0;

		if (!parser->lang()->support(Language::Feature::FORMULA_QUANTIFIER)) {
			parser->_feature_error(Language::Feature::FORMULA_QUANTIFIER, &yymsp[-4].minor.yy0->beginLoc());
			YYERROR;
		} else yygotominor.yy101 = new QuantifierFormula(yymsp[-3].minor.yy317, yymsp[-1].minor.yy393, yymsp[-4].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
	  yy_destructor(yypParser,99,&yymsp[-2].minor);
}
#line 3826 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* quant_lst ::= quant_op variable */
#line 925 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy317 = new QuantifierFormula::QuantifierList();
		yygotominor.yy317->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy81, yymsp[0].minor.yy13));
	}
#line 3834 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* quant_lst ::= quant_lst quant_op variable */
#line 931 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy317 = yymsp[-2].minor.yy317;
		yygotominor.yy317->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy81, yymsp[0].minor.yy13));
	}
#line 3842 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* quant_op ::= BIG_CONJ */
#line 936 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy81 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 3848 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 215: /* quant_op ::= BIG_DISJ */
#line 937 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy81 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,92,&yymsp[0].minor);
}
#line 3854 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* formula_card ::= CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R */
#line 988 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy393, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy202, yymsp[-1].minor.yy240, yymsp[0].minor.yy0, NULL); }
#line 3859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* formula_card ::= term_strong CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R */
#line 989 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy393, yymsp[-4].minor.yy307, yymsp[-3].minor.yy0, yymsp[-2].minor.yy202, yymsp[-1].minor.yy240, yymsp[0].minor.yy0, NULL);  }
#line 3864 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 218: /* formula_card ::= CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R term_strong */
#line 990 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy393, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy202, yymsp[-2].minor.yy240, yymsp[-1].minor.yy0, yymsp[0].minor.yy307); }
#line 3869 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 219: /* formula_card ::= term_strong CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R term_strong */
#line 991 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy393, yymsp[-5].minor.yy307, yymsp[-4].minor.yy0, yymsp[-3].minor.yy202, yymsp[-2].minor.yy240, yymsp[-1].minor.yy0, yymsp[0].minor.yy307); }
#line 3874 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* card_af ::= constant_local EQ term_local */
#line 999 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy202 = new AtomicFormula(yymsp[-2].minor.yy97, yymsp[0].minor.yy307, yymsp[-2].minor.yy97->beginLoc(), yymsp[0].minor.yy307->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3880 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 223: /* card_sort_bnd_lst ::= */
#line 1002 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy240 = new CardinalityFormula::BindingList();	}
#line 3885 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 224: /* card_sort_bnd_lst ::= card_sort_bnd_lst COLON sort PAREN_L variable_local PAREN_R */
#line 1004 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy240 = yymsp[-5].minor.yy240;
		yygotominor.yy240->push_back(CardinalityFormula::Binding(yymsp[-3].minor.yy265, yymsp[-1].minor.yy356));
	  yy_destructor(yypParser,81,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3896 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 227: /* head_formula ::= formula_card */
#line 1022 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy393 = yymsp[0].minor.yy393;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy393->beginLoc());
			YYERROR;
		}
	}
#line 3907 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 230: /* head_formula ::= DASH constant */
#line 1032 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy393 = NULL;
		ref_ptr<const Token> d_ptr = yymsp[-1].minor.yy0;
		ref_ptr<Constant> c_ptr = yymsp[0].minor.yy97;

		if (!parser->lang()->support(Language::Feature::FORMULA_NOT_DASH_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_NOT_DASH_HEAD);
			YYERROR;
		} else {
			ATOMIC_FORMULA(yygotominor.yy393, yymsp[0].minor.yy97, "false"); 
		}
	}
#line 3923 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 231: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1064 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy111 = NULL;
        ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
        ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
        ref_ptr<MacroDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy49;
        ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;

        if (!parser->lang()->support(Language::Feature::DECL_MACRO)) {
            parser->_feature_error(Language::Feature::DECL_MACRO, &yymsp[-2].minor.yy0->beginLoc());
            YYERROR;
        } else {
		    BOOST_FOREACH(symbols::MacroSymbol* m, *yymsp[-1].minor.yy49) {
			    if (!parser->symtab()->create(m)) {
	    	        // Check if it's a duplicate
	    	        symbols::MacroSymbol* m2 = (symbols::MacroSymbol*)parser->symtab()->resolve(symbols::Symbol::Type::MACRO, *m->base(), m->arity());
		            if (!m2 || m2 != m) {
		                parser->_parse_error("Detected conflicting definition of symbol \"" + *m->name() + "\".", &yygotominor.yy111->beginLoc());
		            } else {
		                parser->_parse_error("Detected a duplicate definition of symbol \"" + *m->name() + "\".", &yygotominor.yy111->beginLoc());
		            }
		        }
		    }

			yygotominor.yy111 = new MacroDeclaration(yymsp[-1].minor.yy49, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
        }
    }
#line 3953 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* macro_def_lst ::= macro_bnd */
#line 1092 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy49 = new MacroDeclaration::ElementList();
        yygotominor.yy49->push_back(yymsp[0].minor.yy163);
    }
#line 3961 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 233: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1098 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy49 = yymsp[-2].minor.yy49;
        yygotominor.yy49->push_back(yymsp[0].minor.yy163);
      yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 3970 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 234: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1104 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy50;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy163 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy50);
      yy_destructor(yypParser,72,&yymsp[-4].minor);
  yy_destructor(yypParser,73,&yymsp[-2].minor);
  yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 3984 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1113 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy163 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 3995 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 236: /* macro_args ::= macro_arg */
#line 1121 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy50 = new MacroSymbol::ArgumentList();
        yygotominor.yy50->push_back(yymsp[0].minor.yy203->str());
        delete yymsp[0].minor.yy203;
    }
#line 4004 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 237: /* macro_args ::= macro_args COMMA macro_arg */
#line 1127 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy50 = yymsp[-2].minor.yy50;
        yygotominor.yy50->push_back(yymsp[0].minor.yy203->str());
        delete yymsp[0].minor.yy203;
      yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4014 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 238: /* macro_arg ::= POUND_INTEGER */
      case 239: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==239);
#line 1134 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy203 = yymsp[0].minor.yy0;
    }
#line 4022 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 240: /* sort_lst ::= sort */
#line 1159 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy315 = new ConstantSymbol::SortList();
		yygotominor.yy315->push_back(yymsp[0].minor.yy265);
	}
#line 4030 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 241: /* sort_lst ::= sort_lst COMMA sort */
#line 1164 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy315 = yymsp[-2].minor.yy315;
		yygotominor.yy315->push_back(yymsp[0].minor.yy265);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4039 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 242: /* sort ::= sort_id_nr STAR */
#line 1200 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_SYM(yygotominor.yy265, yymsp[-1].minor.yy265, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, *yymsp[-1].minor.yy265->base() + "_star", "none"); }
#line 4044 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 243: /* sort ::= sort_id_nr CARROT */
#line 1201 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_SYM(yygotominor.yy265, yymsp[-1].minor.yy265, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, *yymsp[-1].minor.yy265->base() + "_carrot", "unknown"); }
#line 4049 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 244: /* sort ::= sort_id_nr */
      case 245: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==245);
#line 1202 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy265 = yymsp[0].minor.yy265; }
#line 4055 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 246: /* sort_id_nr ::= NUMBER_RANGE */
#line 1206 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy265 = NULL;
		// dynamic sort declaration using a numerical range
		// parse the string for min and max
		int min, max;
		if (sscanf(yymsp[0].minor.yy0->str()->c_str(), "%d..%d", &min, &max) != 2) {
			parser->_parse_error("INTERNAL ERROR: failed parsing mumber range \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}


		if (!parser->lang()->support(Language::Feature::NUMRANGE_SORT)) {
			parser->_feature_error(Language::Feature::NUMRANGE_SORT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}

		// X..Y becomes __sort_X_Y__
		std::string name = "__sort_" + boost::lexical_cast<std::string>(min) + "__" + boost::lexical_cast<std::string>(max) + "__";

		ref_ptr<SortSymbol::ObjectList> objs = new SortSymbol::ObjectList();

		// Generate the objects that it will have
		for (int i = min; i <= max; i++) {
			std::string obj_name = boost::lexical_cast<std::string>(i);
			ObjectSymbol const* sym = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(obj_name)));

			if (!sym) {
				yygotominor.yy265 = NULL;
				parser->_parse_error("An error occurred creating symbol \"" + obj_name + "/0\".", &yymsp[0].minor.yy0->beginLoc());
				YYERROR;
			}
			objs->insert(sym);
		}		

		// dynamically declare the sort
		yygotominor.yy265 = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name), objs));
		if (!yygotominor.yy265) {
				parser->_parse_error("An error occurred creating symbol \"" + name + "/0\".", &yymsp[0].minor.yy0->beginLoc());
				YYERROR;
		} 
	}
#line 4100 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 247: /* sort_id ::= IDENTIFIER */
#line 1249 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy265 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy265) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4113 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 248: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1278 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ConstantDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy145;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_CONSTANT)) {
			yygotominor.yy175 = NULL;
			parser->_feature_error(Language::Feature::DECL_CONSTANT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy175 = new ConstantDeclaration(yymsp[-1].minor.yy145, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ConstantSymbol* c, *yymsp[-1].minor.yy145) {
				if (!parser->symtab()->create(c)) {
					// Check if it's a duplicate
					ConstantSymbol* c2 = (ConstantSymbol*)parser->symtab()->resolve(Symbol::Type::CONSTANT, *c->base(), c->arity());
					if (!c2 || c2 != c) {
						parser->_parse_error("Detected conflicting definition of symbol \"" + *c->name() + "\".", &yymsp[-3].minor.yy0->beginLoc());
					} else {
						parser->_parse_error("Detected a duplicate definition of symbol \"" + *c->name() + "\".", &yymsp[-3].minor.yy0->beginLoc());
					}
				}
			}
		}
	}
#line 4144 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 249: /* constant_bnd_lst ::= constant_bnd */
#line 1307 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy145 = yymsp[0].minor.yy145;
	}
#line 4151 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 250: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1312 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy145;
		yygotominor.yy145 = yymsp[-2].minor.yy145;
		yygotominor.yy145->splice(yygotominor.yy145->end(), *yymsp[0].minor.yy145);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4161 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 251: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1319 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy145 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-5].minor.yy474) {
			yygotominor.yy145->push_back(new ConstantSymbol(yymsp[-3].minor.yy469, decl.first->str(), yymsp[-1].minor.yy265, decl.second));
		}
		delete yymsp[-5].minor.yy474;
	  yy_destructor(yypParser,77,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4175 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 252: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1327 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy145 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy474) {
			yygotominor.yy145->push_back(new ConstantSymbol(ConstantSymbol::Type::RIGID, decl.first->str(), yymsp[0].minor.yy265, decl.second));
		}
		delete yymsp[-2].minor.yy474;
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4187 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 253: /* constant_dcl_lst ::= IDENTIFIER */
#line 1336 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy474 = new IdentifierDeclList();
		yygotominor.yy474->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 4195 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 254: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1341 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy474 = new IdentifierDeclList();
		yygotominor.yy474->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy315));
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4205 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 255: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1346 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy474 = yymsp[-2].minor.yy474;
		yygotominor.yy474->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4214 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 256: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1351 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy474 = yymsp[-5].minor.yy474;
		yygotominor.yy474->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy315));
	  yy_destructor(yypParser,103,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4225 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 257: /* constant_dcl_type ::= ABACTION */
#line 1357 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy469 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4237 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 258: /* constant_dcl_type ::= ACTION */
#line 1366 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy469 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4249 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 259: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1375 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy469 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4261 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 260: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1384 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy469 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4273 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 261: /* constant_dcl_type ::= EXTERNALACTION */
#line 1393 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy469 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4285 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 262: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1402 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy469 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4297 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 263: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1411 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy469 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4309 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 264: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1420 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy469 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4321 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* constant_dcl_type ::= RIGID */
#line 1429 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy469 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4333 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1438 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy469 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4345 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 267: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1464 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ObjectDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy454;
		
		if (!parser->lang()->support(Language::Feature::DECL_OBJECT)) {
			yygotominor.yy288 = NULL;
			parser->_feature_error(Language::Feature::DECL_OBJECT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy288 = new ObjectDeclaration(yymsp[-1].minor.yy454, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ObjectDeclaration::Element* bnd, *yymsp[-1].minor.yy454) {
				BOOST_FOREACH(ObjectSymbol const* o, *bnd) {
						bnd->sort()->add(o);
				}
			}
		}
	}
#line 4370 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* object_bnd_lst ::= object_bnd */
#line 1487 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy454 = new ObjectDeclaration::ElementList();
		yygotominor.yy454->push_back(yymsp[0].minor.yy470);
	}
#line 4378 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 269: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 1493 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy454 = yymsp[-2].minor.yy454;
		yygotominor.yy454->push_back(yymsp[0].minor.yy470);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4387 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 270: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 1499 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy470 = new ObjectDeclaration::Element(yymsp[0].minor.yy265, yymsp[-2].minor.yy237);
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4395 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 271: /* object_lst ::= object_spec */
#line 1504 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy237 = yymsp[0].minor.yy237;
	}
#line 4402 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 272: /* object_lst ::= object_lst COMMA object_spec */
#line 1508 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy237 = yymsp[-2].minor.yy237;
		yygotominor.yy237->splice(yygotominor.yy237->end(), *yymsp[0].minor.yy237);
		delete yymsp[0].minor.yy237;
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4412 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 273: /* object_spec ::= IDENTIFIER */
#line 1517 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy237 = NULL;
		ref_ptr<const ObjectSymbol> o = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy237 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy237->push_back(o);
		}
	}
#line 4427 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 274: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1529 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy237 = NULL;
		ref_ptr<ObjectSymbol::SortList> lst_ptr = yymsp[-1].minor.yy315;
		ref_ptr<const ObjectSymbol> o = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy315));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(),yymsp[-1].minor.yy315->size()) + "\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy237 = new  ObjectDeclaration::Element::ObjectList();
			yygotominor.yy237->push_back(o);
		}
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4445 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 275: /* object_spec ::= NUMBER_RANGE */
#line 1542 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy237 = new ObjectDeclaration::Element::ObjectList();
		ref_ptr<const Token> nr_ptr = yymsp[0].minor.yy0;

		// dynamic sort declaration using a numerical range
		// parse the string for min and max
		int min, max;
		if (sscanf(yymsp[0].minor.yy0->str()->c_str(), "%d..%d", &min, &max) != 2) {
			parser->_parse_error("INTERNAL ERROR: failed parsing mumber range \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}

		// iterate over the range and add it to the list
		for (int i = min; i <= max; i++) {
			std::string name = boost::lexical_cast<std::string>(i);
			ref_ptr<const ObjectSymbol> o = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(name)));
			if (!o) {
				parser->_parse_error("INTERNAL ERROR: Could not create object symbol \"" + Symbol::genName(name, 0) + "\".", &yymsp[0].minor.yy0->beginLoc());
				YYERROR;
			} else {
				yygotominor.yy237->push_back(o);
			}
		}
	}
#line 4473 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 276: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 1582 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<VariableDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy318;
		
		if (!parser->lang()->support(Language::Feature::DECL_VARIABLE)) {
			yygotominor.yy267 = NULL;
			parser->_feature_error(Language::Feature::DECL_VARIABLE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy267 = new VariableDeclaration(yymsp[-1].minor.yy318, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(VariableSymbol* v, *yymsp[-1].minor.yy318) {
				if (!parser->symtab()->create(v)) {
					// Check if it's a duplicate
					VariableSymbol* v2 = (VariableSymbol*)parser->symtab()->resolve(Symbol::Type::VARIABLE, *v->base());
					if (!v2 || v2 != v) {
						parser->_parse_error("Detected conflicting definition of symbol \"" + *v->name() + "\".", &yymsp[-3].minor.yy0->beginLoc());
					} else {
						parser->_parse_error("Detected a duplicate definition of symbol \"" + *v->name() + "\".", &yymsp[-3].minor.yy0->beginLoc());
					}
				}
			}
		}
	}
#line 4504 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 277: /* variable_bnd_lst ::= variable_bnd */
#line 1611 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy318 = yymsp[0].minor.yy318;
	}
#line 4511 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 278: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 1616 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy318 = yymsp[-2].minor.yy318;
		yygotominor.yy318->splice(yygotominor.yy318->end(), *yymsp[0].minor.yy318);
		delete yymsp[0].minor.yy318;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4521 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 279: /* variable_bnd ::= variable_lst DBL_COLON sort_id */
#line 1623 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy318 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy208) {
			yygotominor.yy318->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy265));
		}
		delete yymsp[-2].minor.yy208;
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4534 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 280: /* variable_lst ::= IDENTIFIER */
#line 1633 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy208 = new TokenList();
		yygotominor.yy208->push_back(yymsp[0].minor.yy0);
	}
#line 4542 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 281: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 1638 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy208 = yymsp[-2].minor.yy208;
		yygotominor.yy208->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4551 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 282: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 1659 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<SortDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy295;
		
		if (!parser->lang()->support(Language::Feature::DECL_SORT)) {
			yygotominor.yy45 = NULL;
			parser->_feature_error(Language::Feature::DECL_SORT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy45 = new SortDeclaration(yymsp[-1].minor.yy295, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4569 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 283: /* sort_bnd_lst ::= sort_bnd */
      case 285: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==285);
#line 1675 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy295 = yymsp[0].minor.yy295;
	}
#line 4577 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 284: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 1680 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy295 = yymsp[-2].minor.yy295;
		yygotominor.yy295->splice(yygotominor.yy295->end(), *yymsp[0].minor.yy295);
		delete yymsp[0].minor.yy295;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4587 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 286: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 1692 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy295) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy295) {
				sym2->addSubsort(sym);
			}
		}
		yygotominor.yy295 = yymsp[-2].minor.yy295;
		yygotominor.yy295->splice(yymsp[-2].minor.yy295->end(), *yymsp[0].minor.yy295);
		delete yymsp[0].minor.yy295;

	  yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 4603 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 1704 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy295) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy295) {
				sym->addSubsort(sym2);
			}
		}
		yygotominor.yy295 = yymsp[-2].minor.yy295;
		yygotominor.yy295->splice(yymsp[-2].minor.yy295->end(), *yymsp[0].minor.yy295);
		delete yymsp[0].minor.yy295;
	  yy_destructor(yypParser,100,&yymsp[-1].minor);
}
#line 4618 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 1715 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy295 = yymsp[-1].minor.yy295;
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4627 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* sort_dcl_lst ::= IDENTIFIER */
#line 1720 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<SortSymbol> s = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yygotominor.yy295 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy295 = new SortDeclaration::ElementList();
			yygotominor.yy295->push_back(s);
		}

		delete yymsp[0].minor.yy0;
	}
#line 4644 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 1734 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy295 = yymsp[-2].minor.yy295;
		ref_ptr<SortSymbol> s = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yymsp[-2].minor.yy295 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yymsp[-2].minor.yy295->push_back(s);
		}

		delete yymsp[0].minor.yy0;

	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4663 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 1761 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy280 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<ShowStatement::ElementList> lst_ptr = yymsp[-1].minor.yy395;

		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy280 = new ShowStatement(yymsp[-1].minor.yy395, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4679 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 1775 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy280 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_SHOW_ALL)) {
			parser->_feature_error(Language::Feature::DECL_SHOW_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy280 = new ShowAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4697 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 1792 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy280 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<HideStatement::ElementList> lst_ptr = yymsp[-1].minor.yy395;

		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy280 = new HideStatement(yymsp[-1].minor.yy395, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4713 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 294: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 1806 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy280 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_HIDE_ALL)) {
			parser->_feature_error(Language::Feature::DECL_HIDE_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy280 = new HideAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4731 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* show_lst ::= show_elem */
#line 1824 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy395 = new ShowStatement::ElementList();
		yygotominor.yy395->push_back(yymsp[0].minor.yy202);
	}
#line 4739 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* show_lst ::= show_lst COMMA show_elem */
#line 1829 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy395 = yymsp[-2].minor.yy395;
		yygotominor.yy395->push_back(yymsp[0].minor.yy202);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4748 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* show_elem ::= atomic_formula_one_const */
#line 1834 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy202 = yymsp[0].minor.yy202; }
#line 4753 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* stmt_noconcurrency ::= NOCONCURRENCY */
#line 1857 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy57, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 4758 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY */
#line 1858 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy298, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 4763 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ INTEGER PERIOD */
#line 1891 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy280, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 4769 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ INTEGER PERIOD */
#line 1892 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy280, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 4775 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 1917 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy170 = NULL;
		ref_ptr<const Referenced> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, data_l_ptr = yymsp[-1].minor.yy181.l, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Referenced> data_maxstep_ptr = yymsp[-1].minor.yy181.maxstep, data_label_ptr = yymsp[-1].minor.yy181.label;

		if (!parser->lang()->support(Language::Feature::DECL_QUERY)) {
			parser->_feature_error(Language::Feature::DECL_QUERY, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			bool good = true;

			// decode the maxstep if it exists
			int min = -1, max = -1;
			if (yymsp[-1].minor.yy181.maxstep) {
				if (yymsp[-1].minor.yy181.maxstep->type() == T_INTEGER) {
					if (sscanf(yymsp[-1].minor.yy181.maxstep->str()->c_str(), "%d", &max) != 1) {
						parser->_parse_error("INTERNAL ERROR: Could not extract integer from \"" + *yymsp[-1].minor.yy181.maxstep->str() + "\".", &yymsp[-1].minor.yy181.maxstep->beginLoc());
						YYERROR;
						good = false;
					} else if (max < 0) {
						parser->_parse_error("Query maximum step definitions cannot be negative.", &yymsp[-1].minor.yy181.maxstep->beginLoc());
						YYERROR;
						good = false;
					} else min = max;
				} else if (yymsp[-1].minor.yy181.maxstep->type() == T_NUMBER_RANGE) {
					if (sscanf(yymsp[-1].minor.yy181.maxstep->str()->c_str(), "%d..%d", &min, &max) != 2) {
						parser->_parse_error("INTERNAL ERROR: Could not extract number range from \"" + *yymsp[-1].minor.yy181.maxstep->str() + "\".", &yymsp[-1].minor.yy181.maxstep->beginLoc());
						YYERROR;
						good = false;
					} else if (min < 0 || max < 0) {
						parser->_parse_error("Query maximum step definitions cannot be negative.", &yymsp[-1].minor.yy181.maxstep->beginLoc());
						YYERROR;
						good = false;
					}
				}

			}


			// resolve the query label
			ref_ptr<QuerySymbol> sym = new QuerySymbol(yymsp[-1].minor.yy181.label->str(), min, max);
			if (!parser->symtab()->create(sym)) {
				parser->_parse_error("Could not create query, the label \"" + *yymsp[-1].minor.yy181.label->str() + "\" already exists.", &yymsp[-1].minor.yy181.label->beginLoc());
				good = false;
				YYERROR;
			}


			if (good) yygotominor.yy170 = new QueryStatement(sym, yymsp[-1].minor.yy181.l, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4830 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* query_lst ::= formula_temporal */
#line 1971 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy181.l = new QueryStatement::FormulaList();
		yygotominor.yy181.maxstep = NULL;
		yygotominor.yy181.label = NULL;

		yygotominor.yy181.l->push_back(yymsp[0].minor.yy393);
	}
#line 4841 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* query_lst ::= query_maxstep_decl */
#line 1980 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy181.l = new QueryStatement::FormulaList();
		yygotominor.yy181.maxstep = yymsp[0].minor.yy203;
		yygotominor.yy181.label = NULL;
	}
#line 4850 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* query_lst ::= query_label_decl */
#line 1987 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy181.l = new QueryStatement::FormulaList();
		yygotominor.yy181.maxstep = NULL;
		yygotominor.yy181.label = yymsp[0].minor.yy203;
	}
#line 4859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 1994 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy181 = yymsp[-2].minor.yy181;
		yymsp[-2].minor.yy181.l->push_back(yymsp[0].minor.yy393);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4868 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2000 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy181 = yymsp[-2].minor.yy181;

		if (yygotominor.yy181.maxstep) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy203->beginLoc());
			delete yymsp[0].minor.yy203;
			YYERROR;
		} else {
			yygotominor.yy181.maxstep = yymsp[0].minor.yy203;
		}
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4884 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2013 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy181 = yymsp[-2].minor.yy181;
		if (yygotominor.yy181.label) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy203->beginLoc());
			delete yymsp[0].minor.yy203;
			YYERROR;

		} else {
			yygotominor.yy181.label = yymsp[0].minor.yy203;
		}
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4900 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
      case 310: /* query_maxstep_decl ::= MAXSTEP DBL_COLON NUMBER_RANGE */ yytestcase(yyruleno==310);
#line 2038 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy203, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_MAXSTEP);   yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4907 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 312: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==312);
#line 2040 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy203, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4914 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* clause_if ::= IF formula */
#line 2075 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy393, yymsp[-1].minor.yy0, yymsp[0].minor.yy393, Language::Feature::CLAUSE_IF); 		}
#line 4919 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* clause_if ::= */
      case 316: /* clause_after ::= */ yytestcase(yyruleno==316);
      case 318: /* clause_ifcons ::= */ yytestcase(yyruleno==318);
      case 322: /* clause_where ::= */ yytestcase(yyruleno==322);
#line 2076 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = NULL; }
#line 4927 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* clause_after ::= AFTER formula */
#line 2077 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy393, yymsp[-1].minor.yy0, yymsp[0].minor.yy393, Language::Feature::CLAUSE_AFTER);	}
#line 4932 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* clause_ifcons ::= IFCONS formula */
#line 2079 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy393, yymsp[-1].minor.yy0, yymsp[0].minor.yy393, Language::Feature::CLAUSE_IFCONS); 	}
#line 4937 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* clause_unless ::= UNLESS atomic_formula */
#line 2081 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy202, yymsp[-1].minor.yy0, yymsp[0].minor.yy202, Language::Feature::CLAUSE_UNLESS); 	}
#line 4942 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 320: /* clause_unless ::= */
#line 2082 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy202 = NULL; }
#line 4947 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* clause_where ::= WHERE formula_no_const */
#line 2083 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy393, yymsp[-1].minor.yy0, yymsp[0].minor.yy393, Language::Feature::CLAUSE_WHERE); 	}
#line 4952 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 323: /* stmt_law ::= law_basic */
      case 324: /* stmt_law ::= law_caused */ yytestcase(yyruleno==324);
      case 325: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==325);
      case 326: /* stmt_law ::= law_impl */ yytestcase(yyruleno==326);
      case 327: /* stmt_law ::= law_causes */ yytestcase(yyruleno==327);
      case 328: /* stmt_law ::= law_increments */ yytestcase(yyruleno==328);
      case 329: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==329);
      case 330: /* stmt_law ::= law_always */ yytestcase(yyruleno==330);
      case 331: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==331);
      case 332: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==332);
      case 333: /* stmt_law ::= law_never */ yytestcase(yyruleno==333);
      case 334: /* stmt_law ::= law_default */ yytestcase(yyruleno==334);
      case 335: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==335);
      case 336: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==336);
      case 337: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==337);
      case 338: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==338);
      case 339: /* stmt_law ::= law_observed */ yytestcase(yyruleno==339);
#line 2127 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy280 = yymsp[0].minor.yy280;}
#line 4973 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 340: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2242 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy280, NULL, yymsp[-6].minor.yy393, yymsp[-5].minor.yy393, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, 
																																														yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
																																															Language::Feature::LAW_BASIC_D, BasicLaw); }
#line 4980 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 341: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2247 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy280, yymsp[-7].minor.yy0, yymsp[-6].minor.yy393, yymsp[-5].minor.yy393, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, 
																																														yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 4987 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 342: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2251 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy280, yymsp[-7].minor.yy0, yymsp[-6].minor.yy393, yymsp[-5].minor.yy393, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, 
																																														yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 4994 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 343: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2255 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy280, yymsp[-4].minor.yy393, yymsp[-3].minor.yy0, yymsp[-2].minor.yy393, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 5000 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 344: /* law_causes ::= formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2259 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy280, yymsp[-6].minor.yy393, yymsp[-5].minor.yy0, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 5006 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 345: /* law_increments ::= formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2263 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy280, yymsp[-8].minor.yy393, yymsp[-7].minor.yy0, yymsp[-6].minor.yy97, yymsp[-4].minor.yy307, yymsp[-3].minor.yy393, yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5013 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* law_mcause ::= formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2267 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy280, yymsp[-6].minor.yy393, yymsp[-5].minor.yy0, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 5019 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
      case 348: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */ yytestcase(yyruleno==348);
#line 2271 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy280, yymsp[-5].minor.yy0, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 5027 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2279 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy280, yymsp[-5].minor.yy0, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 5034 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2283 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy280, yymsp[-5].minor.yy0, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 5041 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2287 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy280, yymsp[-7].minor.yy0, yymsp[-6].minor.yy202, yymsp[-5].minor.yy393, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, 
																																														yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 5048 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2291 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy280, yymsp[-7].minor.yy0, yymsp[-6].minor.yy97, yymsp[-5].minor.yy393, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, 
																																														yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 5055 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2295 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy280, yymsp[-7].minor.yy0, yymsp[-6].minor.yy97, yymsp[-5].minor.yy393, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, 
																																														yymsp[-2].minor.yy202, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 5062 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2299 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy280, yymsp[-5].minor.yy0, yymsp[-4].minor.yy393, yymsp[-3].minor.yy393, yymsp[-2].minor.yy202, yymsp[-1].minor.yy393,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 5068 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2303 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy280, yymsp[-3].minor.yy0, yymsp[-2].minor.yy97, yymsp[-1].minor.yy393, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 5074 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD */
#line 2308 "bcplus/parser/detail/lemon_parser.y"
{ 
			yygotominor.yy280 = NULL;
			ref_ptr<const Token> kw_ptr = yymsp[-4].minor.yy0, p_ptr = yymsp[0].minor.yy0;
			ref_ptr<AtomicFormula> head_ptr = yymsp[-3].minor.yy202;
			ref_ptr<Term> t_ptr = yymsp[-1].minor.yy307;

			// make sure that the At clause is integral
			if (yymsp[-1].minor.yy307->domainType() != DomainType::INTEGRAL) {
				parser->_parse_error("Expected an integral expression.", &yymsp[-1].minor.yy307->beginLoc());
				YYERROR;
			} else {
				LAW_SIMPLE_FORM(yygotominor.yy280, yymsp[-4].minor.yy0, yymsp[-3].minor.yy202, yymsp[-1].minor.yy307, yymsp[0].minor.yy0, Language::Feature::LAW_OBSERVED, ObservedLaw); 
			}
		  yy_destructor(yypParser,66,&yymsp[-2].minor);
}
#line 5093 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* stmt_code_blk ::= ASP_GR */
#line 2342 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy280, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 5098 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* stmt_code_blk ::= ASP_CP */
#line 2343 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy280, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 5103 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* stmt_code_blk ::= F2LP_GR */
#line 2344 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy280, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 5108 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* stmt_code_blk ::= F2LP_CP */
#line 2345 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy280, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 5113 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* stmt_code_blk ::= LUA_GR */
#line 2346 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy280, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 5118 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* stmt_code_blk ::= LUA_CP */
#line 2347 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy280, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 5123 "bcplus/parser/detail/lemon_parser.c"
        break;
      default:
      /* (1) statement_lst ::= */ yytestcase(yyruleno==1);
      /* (2) statement_lst ::= statement_lst error */ yytestcase(yyruleno==2);
        break;
  };
  yygoto = yyRuleInfo[yyruleno].lhs;
  yysize = yyRuleInfo[yyruleno].nrhs;
  yypParser->yyidx -= yysize;
  yyact = yy_find_reduce_action(yymsp[-yysize].stateno,(YYCODETYPE)yygoto);
  if( yyact < YYNSTATE ){
#ifdef NDEBUG
    /* If we are not debugging and the reduce action popped at least
    ** one element off the stack, then we can push the new element back
    ** onto the stack here, and skip the stack overflow test in yy_shift().
    ** That gives a significant speed improvement. */
    if( yysize ){
      yypParser->yyidx++;
      yymsp -= yysize-1;
      yymsp->stateno = (YYACTIONTYPE)yyact;
      yymsp->major = (YYCODETYPE)yygoto;
      yymsp->minor = yygotominor;
    }else
#endif
    {
      yy_shift(yypParser,yyact,yygoto,&yygotominor);
    }
  }else{
    assert( yyact == YYNSTATE + YYNRULE + 1 );
    yy_accept(yypParser);
  }
}

/*
** The following code executes when the parse fails
*/
#ifndef YYNOERRORRECOVERY
static void yy_parse_failed(
  yyParser *yypParser           /* The parser */
){
  lemon_parserARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sFail!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
  lemon_parserARG_STORE; /* Suppress warning about unused %extra_argument variable */
}
#endif /* YYNOERRORRECOVERY */

/*
** The following code executes when a syntax error first occurs.
*/
static void yy_syntax_error(
  yyParser *yypParser,           /* The parser */
  int yymajor,                   /* The major type of the error token */
  YYMINORTYPE yyminor            /* The minor type of the error token */
){
  lemon_parserARG_FETCH;
#define TOKEN (yyminor.yy0)
#line 184 "bcplus/parser/detail/lemon_parser.y"
 parser->_parse_error("Syntax error.");	
#line 5189 "bcplus/parser/detail/lemon_parser.c"
  lemon_parserARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  lemon_parserARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sAccept!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
  lemon_parserARG_STORE; /* Suppress warning about unused %extra_argument variable */
}


/*
** Handles a syntax error within the parser.
*/
static void yy_handle_err(yyParser* yypParser, int* yyerrorhit) {
      int yyact;
#ifdef YYERRORSYMBOL
  int yymx;
#endif
#ifndef NDEBUG
      if( yyTraceFILE ){
        fprintf(yyTraceFILE,"%sSyntax Error!\n",yyTracePrompt);
      }
#endif
#ifdef YYERRORSYMBOL
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token "ERROR".  
      **
      ** This is what we do if the grammar does define ERROR:
      **
      **  * Call the %syntax_error function.
      **
      **  * Begin popping the stack until we enter a state where
      **    it is legal to shift the error symbol, then shift
      **    the error symbol.
      **
      **  * Set the error count to three.
      **
      **  * Begin accepting and shifting new tokens.  No new error
      **    processing will occur until three tokens have been
      **    shifted successfully.
      **
      */
	  yyact = YY_ERROR_ACTION;
      if( yypParser->yyerrcnt<0 ){
        yy_syntax_error(yypParser,yypParser->yylookmajor,yypParser->yylookminor);
      }
      yymx = yypParser->yystack[yypParser->yyidx].major;
      if( yymx==YYERRORSYMBOL || *yyerrorhit ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE,"%sDiscard input token %s\n",
             yyTracePrompt,yyTokenName[yypParser->yylookmajor]);
        }
#endif
        yy_destructor(yypParser, (YYCODETYPE)yypParser->yylookmajor,&yypParser->yylookminor);
        yypParser->yylookmajor = YYNOCODE;
        yypParser->yylookminor = yyzerominor;
      }else{
         while(
          yypParser->yyidx >= 0 &&
          yymx != YYERRORSYMBOL &&
          (yyact = yy_find_reduce_action(
                        yypParser->yystack[yypParser->yyidx].stateno,
                        YYERRORSYMBOL)) >= YYNSTATE
        ){
          yy_pop_parser_stack(yypParser);
        }
        if( yypParser->yyidx < 0 || yypParser->yylookmajor==0 ){
          yy_destructor(yypParser,(YYCODETYPE)yypParser->yylookmajor,&yypParser->yylookminor);
          yy_parse_failed(yypParser);
          yypParser->yylookmajor = YYNOCODE;
          yypParser->yylookminor = yyzerominor;
        }else if( yymx!=YYERRORSYMBOL ){
          YYMINORTYPE u2;
          u2.YYERRSYMDT = 0;
          yy_shift(yypParser,yyact,YYERRORSYMBOL,&u2);
        }
      }
      yypParser->yyerrcnt = 3;
      *yyerrorhit = 1;
#elif defined(YYNOERRORRECOVERY)
      /* If the YYNOERRORRECOVERY macro is defined, then do not attempt to
      ** do any kind of error recovery.  Instead, simply invoke the syntax
      ** error routine and continue going as if nothing had happened.
      **
      ** Applications can set this macro (for example inside %include) if
      ** they intend to abandon the parse upon the first syntax error seen.
      */
      yy_syntax_error(yypParser,yypParser->yylookmajor,yypParser->yylookminor);
      yy_destructor(yypParser,(YYCODETYPE)yypParser->yylookmajor,&yypParser->yylookminor);
      yypParser->yylookmajor = YYNOCODE;
      yypParser->yylookminor = yyzerominor;
      
#else  /* YYERRORSYMBOL is not defined */
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if( yypParser->yyerrcnt<=0 ){
        yy_syntax_error(yypParser,yypParser->yylookmajor,yypParser->yylookminor);
      }
      yypParser->yyerrcnt = 3;
      yy_destructor(yypParser,(YYCODETYPE)yypParser->yylookmajor,&yypParser->yylookminor);
      if( yyendofinput ){
        yy_parse_failed(yypParser);
      }
      yypParser->yylookmajor = YYNOCODE;
      yypParser->yylookminor = yyzerominor;
#endif
      yypParser->yysyntaxerr = 0;
}


/*
** Prepares the parser to accept tokens injected at the current
** location by extracting the lookahead token so that it can be
** reintroduced into the stream.
** Also pops the latest symbol off the parser's stack if the pop
** option is asserted.
** 
** returns the major type of the lookahead token that has been 
** cleared from the parser or YYNOCODE and sets the lookahead minor
** type appropriately.
*/
int lemon_parserPreInject(void* yyp, int pop, lemon_parserTOKENTYPE* lookahead) {
	yyParser* pParser = (yyParser*)yyp;
	int code = pParser->yylookmajor;
	if (pop && pParser->yyidx) yy_pop_parser_stack(pParser);
	if (code != YYNOCODE) {
		*lookahead = pParser->yylookminor.yy0;
		pParser->yylookmajor = YYNOCODE;
	    pParser->yylookminor = yyzerominor;
		return code;
	} else {
		*lookahead = yyzerominor.yy0;
		return 0;

	}
}

/*
** Gets the name of the provided token.
** Primarily for debugging purposes.
**
*/
char const* lemon_parserTokenName(int tok) {
	if (tok < 1) return "<INVALID_TOKEN>";
	else if (tok == YYNOCODE) return "<NOCODE_TOKEN>";
#ifdef YYERRORSYMBOL
	else if (tok == YYERRORSYMBOL) return "<ERROR_TOKEN>";
#endif
	return yyTokenName[tok];
}


/*
** Checks to see if there is a next-token independent reduction rule
** and executes it.
*/
void lemon_parserAttemptReduce(void* yyp lemon_parserARG_PDECL) {
	yyParser* yypParser = (yyParser*)yyp;
	lemon_parserARG_STORE;
	int act = 0;
	int yyerrorhit = 0;
	do {
		yypParser->yysyntaxerr = 0;
		act = yy_find_reduce_action(yypParser->yystack[yypParser->yyidx].stateno, YYNOCODE);
		if (act >= YYNSTATE && act < YYNSTATE + YYNRULE) {
			// There is a reduce action. Do it.
			yy_reduce(yypParser, act-YYNSTATE);	
		}

		if (yypParser->yysyntaxerr) {
			yy_handle_err(yypParser, &yyerrorhit);
		}

	} while (act >= YYNSTATE && act < YYNSTATE + YYNRULE);
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "lemon_parserAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
void lemon_parser(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  lemon_parserTOKENTYPE yyminor       /* The value for the token */
  lemon_parserARG_PDECL               /* Optional %extra_argument parameter */
){
  int yyact;            /* The parser action. */
  int yyendofinput;     /* True if we are at the end of input */
  int yyerrorhit = 0;
  yyParser *yypParser;  /* The parser */

  /* (re)initialize the parser, if necessary */
  yypParser = (yyParser*)yyp;
  if( yypParser->yyidx<0 ){
#if YYSTACKDEPTH<=0
    if( yypParser->yystksz <=0 ){
      /*memset(&yyminorunion, 0, sizeof(yyminorunion));*/
      yypParser->yylookmajor = YYNOCODE;
      yypParser->yylookminor = yyzerominor;
      yyStackOverflow(yypParser/*, &yyminorunion */);
      return;
    }
#endif
    yypParser->yyidx = 0;
    yypParser->yyerrcnt = -1;
    yypParser->yystack[0].stateno = 0;
    yypParser->yystack[0].major = 0;
  }
  yypParser->yylookmajor = yymajor;
  yypParser->yylookminor.yy0 = yyminor;
  yyendofinput = (yypParser->yylookmajor==0);
  lemon_parserARG_STORE;

#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sInput %s\n",yyTracePrompt,yyTokenName[yypParser->yylookmajor]);
  }
#endif

  do{
    yyact = yy_find_shift_action(yypParser,(YYCODETYPE)yypParser->yylookmajor);
    if( yyact<YYNSTATE ){
      assert( !yyendofinput );  /* Impossible to shift the $ token */
      yy_shift(yypParser,yyact,yypParser->yylookmajor,&yypParser->yylookminor);
      yypParser->yyerrcnt--;
      yypParser->yylookmajor = YYNOCODE;
      yypParser->yylookminor = yyzerominor;
    }else if( yyact < YYNSTATE + YYNRULE ) {
      yy_reduce(yypParser,yyact-YYNSTATE);
    } else {
      	assert( yyact == YY_ERROR_ACTION );
		yypParser->yysyntaxerr = 1; 
	}
	
	if (yypParser->yysyntaxerr) {
		yy_handle_err(yypParser, &yyerrorhit);
    }
  }while( yypParser->yylookmajor!=YYNOCODE && yypParser->yyidx>=0 );
  return;
}
