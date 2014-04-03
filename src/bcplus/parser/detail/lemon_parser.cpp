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

		
#line 311 "bcplus/parser/detail/lemon_parser.y"

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

#line 485 "bcplus/parser/detail/lemon_parser.y"

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

#line 716 "bcplus/parser/detail/lemon_parser.y"

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

#line 796 "bcplus/parser/detail/lemon_parser.y"


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
	


#line 872 "bcplus/parser/detail/lemon_parser.y"

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

#line 953 "bcplus/parser/detail/lemon_parser.y"

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



#line 1172 "bcplus/parser/detail/lemon_parser.y"

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
#line 1310 "bcplus/parser/detail/lemon_parser.y"

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
#line 2004 "bcplus/parser/detail/lemon_parser.y"

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

#line 2027 "bcplus/parser/detail/lemon_parser.y"

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
#line 2061 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		Token const* maxstep;
		Token const* label;
	};

#line 2186 "bcplus/parser/detail/lemon_parser.y"

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
#line 2224 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2307 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2490 "bcplus/parser/detail/lemon_parser.y"

	#define CODE_BLK(stmt, code, feature, type) 												\
		ref_ptr<const Token> code_ptr = code;													\
		if (!parser->lang()->support(feature)) {												\
			stmt = NULL;																		\
			parser->_feature_error(feature, &code->beginLoc());									\
			YYERROR;																			\
		} else {																				\
			stmt = new type(code, code->beginLoc(), code->endLoc());							\
		}
#line 492 "bcplus/parser/detail/lemon_parser.c"
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
#define YYNOCODE 242
#define YYACTIONTYPE unsigned short int
#define lemon_parserTOKENTYPE  Token const* 								
typedef union {
  int yyinit;
  lemon_parserTOKENTYPE yy0;
  AtomicFormula* yy6;
  VariableDeclaration* yy43;
  UNUSED yy49;
  TokenList* yy58;
  TermList* yy67;
  ConstantDeclaration::ElementList* yy77;
  MacroSymbol* yy83;
  ObjectDeclaration::ElementList* yy84;
  LuaTerm* yy105;
  Term* yy113;
  QueryStatement* yy134;
  ConstantSymbol::SortList* yy151;
  QuantifierFormula* yy155;
  QueryData yy165;
  ObjectDeclaration* yy172;
  ObjectDeclaration::Element* yy180;
  MacroSymbol::ArgumentList* yy198;
  SortDeclaration::ElementList* yy233;
  ObjectDeclaration::Element::ObjectList* yy299;
  SortSymbol const* yy310;
  Variable* yy313;
  IdentifierDeclList* yy316;
  SortSymbol* yy325;
  VariableDeclaration::ElementList* yy326;
  LocalVariable* yy328;
  Statement* yy334;
  SortDeclaration* yy339;
  Token const* yy340;
  ConstantSymbol::Type::type yy341;
  ShowStatement::ElementList* yy345;
  StrongNCStatement* yy352;
  ConstantDeclaration* yy353;
  Constant* yy357;
  QuantifierFormula::QuantifierList* yy358;
  MacroDeclaration* yy373;
  Formula* yy374;
  MacroDeclaration::ElementList* yy381;
  Object* yy398;
  QuantifierFormula::Operator::type yy423;
  CardinalityFormula::BindingList* yy430;
  NCStatement* yy451;
  int yy483;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define lemon_parserARG_SDECL  BCParser* parser						;
#define lemon_parserARG_PDECL , BCParser* parser						
#define lemon_parserARG_FETCH  BCParser* parser						 = yypParser->parser						
#define lemon_parserARG_STORE yypParser->parser						 = parser						
#define YYNSTATE 705
#define YYNRULE 369
#define YYERRORSYMBOL 132
#define YYERRSYMDT yy483
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
#define YY_ACTTAB_COUNT (2459)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   704,  161,  162,  703,  702,  701,  700,  699,  698,  697,
 /*    10 */   696,  695,  694,  693,  692,  691,  690,  689,  655,  665,
 /*    20 */   253,  688,  687,  686,  601,  600,  647,  657,  658,  656,
 /*    30 */   654,  653,   24,  373,  313,  105,  103,  102,  168,  251,
 /*    40 */   254,  662,  257,  613,  263,  262,  608,  261,  168,   20,
 /*    50 */    96,   95,   94,  115,   93,   92,   91,   90,  180,   20,
 /*    60 */    96,   95,   94,  155,   93,   92,   91,   90,  408,  665,
 /*    70 */   310,  688,  687,  686,  101,  100,   99,   98,   97,  109,
 /*    80 */   107,  105,  103,  102,  101,  100,   99,   98,   97,  309,
 /*    90 */   311,  705,  477,  476,  475,  474,  473,  472,  471,  470,
 /*   100 */   469,  468,  467,  466,  465,  464,  463,  462,  461,  664,
 /*   110 */   364,  524,  363,  437,  663,  446,  445,  442,  441,  444,
 /*   120 */   443,  664,   55,   53,   52,  437,  663,  679,  684,  685,
 /*   130 */   688,  687,  686,  683,  376,  322,    8,   29,   28,   37,
 /*   140 */    35,   30,    6,    7,   63,  159,  682,  199,   62,  236,
 /*   150 */    45,   44,  193,   36,   46,   61,  632,  631,  633,    5,
 /*   160 */   479,  478,    4,  110,   34,  185,  391,  680,  632,  631,
 /*   170 */   392,  259,   40,  260,  433,  167,  110,  256,  166,   39,
 /*   180 */    33,  369,  618,  605,  242,  606,  620,  167,  650,  649,
 /*   190 */   166,  619,   21,   38,  664,  518,  242,  110,  437,  663,
 /*   200 */   403,   27,   26,   25,   29,   28,   51,  652,   30,  359,
 /*   210 */   516,  358,  517,   31,   23,   26,   25,   29,   28,   89,
 /*   220 */   554,   30,  440,  183,  501,   18,   19,   42,   41,   45,
 /*   230 */    44,   89,  164,   46,  439,  681,  438,   60,  651,  158,
 /*   240 */   230,  632,  631,  633,  621,  507,  439,  681,  438,  112,
 /*   250 */   665,  253,  688,  687,  686,  502,  605,  419,  606,  155,
 /*   260 */   167,  545,  664,  166,  648,   33,  437,  663,   60,  242,
 /*   270 */   248,  254,  161,  162,  493,  610,  609,  608,  607,  541,
 /*   280 */   655,  170,  141,  271,   30,  362,  369,  618,  647,  657,
 /*   290 */   658,  656,  654,  653,   60,  372,  313,   72,   31,   32,
 /*   300 */    13,   12,   16,   15,   89,  542,   17,  523,  411,  632,
 /*   310 */   631,  633,  369,  618,  345,  481,  480,  418,  416,  439,
 /*   320 */   681,  438,  417,  421,  605,  935,  606,  151,  167,  369,
 /*   330 */   618,  166,  879,   21,  135,  133,  132,  242,  168,  515,
 /*   340 */   935,  935,  879,  664,  483,  482,  879,  437,  663,  488,
 /*   350 */    96,   95,   94,  182,   93,   92,   91,   90,   22,  169,
 /*   360 */   935,  935,   46,  541,   16,   15,   18,   19,   17,  361,
 /*   370 */   369,  618,   89,  935,  101,  100,   99,   98,   97,  935,
 /*   380 */   229,  646,  228,  650,  649,  434,  645,  439,  681,  438,
 /*   390 */   632,  631,  633,  509,  879,  879,  879,  879,  879,  549,
 /*   400 */   550,  879,  557,  541,  539,  592,  181,  593,  407,  360,
 /*   410 */   369,  618,  166,  163,  114,  503,  589,  526,  242,  168,
 /*   420 */   437,  588,  662,  109,  107,  105,  103,  102,  640,  639,
 /*   430 */   641,   96,   95,   94,  153,   93,   92,   91,   90,  356,
 /*   440 */   510,  355,  540,  642,  149,  643,  486,  153,  111,  369,
 /*   450 */   618,   64,   58,   89,  603,  101,  100,   99,   98,   97,
 /*   460 */   525,  367,  546,  570,  569,  571,    3,  556,  439,  681,
 /*   470 */   438,  431,  665,  253,  688,  687,  686,  487,  560,  506,
 /*   480 */   561,   27,   26,   25,   29,   28,   56,   49,   30,  616,
 /*   490 */   153,   54,  248,  254,  245,  541,  493,  610,  609,  608,
 /*   500 */   607,  357,  369,  618,  555,  270,  436,  681,  435,  538,
 /*   510 */   537,  536,  535,  420,  678,  552,  412,  413,  437,  677,
 /*   520 */    47,   48,  676,  369,  618,  534,  120,  532,  354,  350,
 /*   530 */   406,   50,  531,  522,  358,  517,  403,  485,  484,  519,
 /*   540 */   547,  439,  681,  438,  530,  528,  529,  533,  402,  160,
 /*   550 */   590,  568,  688,  687,  686,  109,  107,  105,  103,  102,
 /*   560 */   501,  672,  671,  673,   27,   26,   25,   29,   28,  153,
 /*   570 */   597,   30,  252,  587,  544,  363,  674,  154,  675,  273,
 /*   580 */   565,  562,  127,  126,  125,  108,  124,  123,  122,  121,
 /*   590 */   348,  499,   27,   26,   25,   29,   28,  227,  157,   30,
 /*   600 */   171,  145,    2,  143,  150,  156,  140,  131,  130,  129,
 /*   610 */   128,  586,  580,  688,  687,  686,  617,  514,  355,  106,
 /*   620 */   349,  282,  543,   22,  104,  665,  253,  688,  687,  686,
 /*   630 */   179,  240,  397,  400,  665,  310,  688,  687,  686,  439,
 /*   640 */   681,  438,  431,  601,  600,  249,  254,  404,  265,  613,
 /*   650 */   610,  609,  608,  607,  277,  311,  415,  665,  255,  688,
 /*   660 */   687,  686,    9,  655,  414,  244,  665,  253,  688,  687,
 /*   670 */   686,  647,  657,  658,  656,  654,  653,  614,  254,  312,
 /*   680 */   598,  613,  610,  609,  608,  607,  251,  254,  428,  612,
 /*   690 */   613,  610,  609,  608,  607,  665,  253,  688,  687,  686,
 /*   700 */   505,  148,  590,  568,  688,  687,  686,  586,  580,  688,
 /*   710 */   687,  686,  152,  591,  142,  251,  254,  331,  611,  613,
 /*   720 */   610,  609,  608,  607,  250,  665,  253,  688,  687,  686,
 /*   730 */   119,  264,  565,  562,  665,  253,  688,  687,  686,  679,
 /*   740 */   684,  685,  688,  687,  686,  251,  254,  321,  430,  613,
 /*   750 */   610,  609,  608,  607,  251,  254,  243,  429,  613,  610,
 /*   760 */   609,  608,  607,  586,  580,  688,  687,  686,  665,  253,
 /*   770 */   688,  687,  686,  329,  655,  353,  406,  665,  253,  688,
 /*   780 */   687,  686,  647,  657,  658,  656,  654,  653,  251,  254,
 /*   790 */   338,  293,  613,  610,  609,  608,  607,  251,  254,  616,
 /*   800 */   333,  613,  610,  609,  608,  607,  436,  365,  548,  665,
 /*   810 */   253,  688,  687,  686,  410,  655,  352,  406,  665,  253,
 /*   820 */   688,  687,  686,  647,  657,  658,  656,  654,  653,  251,
 /*   830 */   254,  278,  332,  613,  610,  609,  608,  607,  251,  254,
 /*   840 */   527,  266,  613,  610,  609,  608,  607,  665,  246,  688,
 /*   850 */   687,  686,  147,  655,  590,  568,  688,  687,  686,  146,
 /*   860 */   401,  647,  657,  658,  656,  654,  653,  614,  254,  339,
 /*   870 */   598,  613,  610,  609,  608,  607,  567,  665,  253,  688,
 /*   880 */   687,  686,  144,  559,  565,  562,  665,  253,  688,  687,
 /*   890 */   686,  679,  684,  685,  688,  687,  686,  251,  254,  342,
 /*   900 */   275,  613,  610,  609,  608,  607,  247,  254,  498,  227,
 /*   910 */   493,  610,  609,  608,  607,  351,  406,  325,  406,  258,
 /*   920 */   665,  253,  688,  687,  686,   22,  617,  428, 1075,    1,
 /*   930 */   665,  255,  688,  687,  686,  428,  644,  149,  512,  405,
 /*   940 */   248,  254,  508,  242,  493,  610,  609,  608,  607,  116,
 /*   950 */   494,  254,  204,  272,  493,  610,  609,  608,  607,  404,
 /*   960 */   377,  118,  500,  490,  665,  253,  688,  687,  686,   59,
 /*   970 */    57,   55,   53,   52,  665,  253,  688,  687,  686,   27,
 /*   980 */    26,   25,   29,   28,  248,  254,   30,  227,  493,  610,
 /*   990 */   609,  608,  607,  439,  248,  254,  399,  492,  493,  610,
 /*  1000 */   609,  608,  607,  497,  398,  496,  396,  491,  679,  684,
 /*  1010 */   685,  688,  687,  686,  403,  375,  322,  395,  495,  665,
 /*  1020 */   253,  688,  687,  686,  586,  580,  688,  687,  686,  665,
 /*  1030 */   253,  688,  687,  686,  274,   17,  346,  404,  501,  248,
 /*  1040 */   254,  344,  460,  493,  610,  609,  608,  607,  459,  248,
 /*  1050 */   254,  458,  394,  493,  610,  609,  608,  607,  457,  456,
 /*  1060 */   455,  454,  393,  665,  253,  688,  687,  686,  347,  499,
 /*  1070 */   117,  453,  452,  665,  253,  688,  687,  686,   27,   26,
 /*  1080 */    25,   29,   28,  248,  254,   30,  451,  493,  610,  609,
 /*  1090 */   608,  607,  450,  248,  254,  449,  280,  493,  610,  609,
 /*  1100 */   608,  607,  448,  370,  368,  615,  324,  665,  253,  688,
 /*  1110 */   687,  686,  604,  238,  436,  439,  602,  681,  665,  253,
 /*  1120 */   688,  687,  686,  599,   22,  237,  165,  248,  254,  425,
 /*  1130 */    50,  493,  610,  609,  608,  607,  423,   10,  251,  254,
 /*  1140 */   323,  269,  613,  610,  609,  608,  607,  665,  253,  688,
 /*  1150 */   687,  686,   59,   57,   55,   53,   52,  424,  232,  665,
 /*  1160 */   253,  688,  687,  686,  447,  234,  366,  251,  254,  231,
 /*  1170 */   268,  613,  610,  609,  608,  607,  422,  521,  616,  251,
 /*  1180 */   254,  504,  177,  613,  610,  609,  608,  607,  513,  404,
 /*  1190 */    11,  665,  253,  688,  687,  686,  139,  137,  135,  133,
 /*  1200 */   132,  665,  253,  688,  687,  686,   27,   26,   25,   29,
 /*  1210 */    28,  251,  254,   30,  176,  613,  610,  609,  608,  607,
 /*  1220 */   225,  251,  254,  553,  175,  613,  610,  609,  608,  607,
 /*  1230 */   586,  580,  688,  687,  686,  665,  253,  688,  687,  686,
 /*  1240 */   330,   10,  223,  224,  221,  665,  253,  688,  687,  686,
 /*  1250 */    83,   82,   81,   80,   79,  251,  254,  390,  174,  613,
 /*  1260 */   610,  609,  608,  607,  220,  251,  254,  219,  173,  613,
 /*  1270 */   610,  609,  608,  607,  665,  253,  688,  687,  686,  218,
 /*  1280 */   662,  388,  215,  216,  782,  782,  782,  214,  782,  782,
 /*  1290 */   782,  782,  213,  211,  251,  254,  387,  172,  613,  610,
 /*  1300 */   609,  608,  607,   88,   87,   86,   85,   84,  782,  782,
 /*  1310 */   782,  782,  782,  101,  100,   99,   98,   97,   71,   70,
 /*  1320 */    69,  386,   68,   67,   66,   65,  209,  207,   72,   78,
 /*  1330 */    77,  385,   76,   75,   74,   73,  585,  384,  205,  203,
 /*  1340 */   437,  584,   83,   82,   81,   80,   79,  665,  253,  688,
 /*  1350 */   687,  686,   88,   87,   86,   85,   84,  127,  126,  125,
 /*  1360 */   383,  124,  123,  122,  121,  664,  202,  251,  254,  437,
 /*  1370 */   663,  201,  596,  595,  200,  594,  665,  253,  688,  687,
 /*  1380 */   686,  140,  131,  130,  129,  128,  239,  679,  684,  685,
 /*  1390 */   688,  687,  686,  382,  374,  322,  251,  254,  581,  198,
 /*  1400 */   582,  596,  595,  197,  594,  194,  196,  138,  195,  381,
 /*  1410 */   192,  191,  632,  631,  633,  233,  190,  665,  253,  688,
 /*  1420 */   687,  686,  188,  189,  935,  184,  186,  665,  253,  688,
 /*  1430 */   687,  686,  380,  379,  378,  432,  114,  251,  254,  935,
 /*  1440 */   935,  136,  596,  595,  551,  594,  134,  251,  254,  520,
 /*  1450 */   226,  241,  596,  595,  235,  594,  222,  389,  212,  935,
 /*  1460 */   935,  439,  681,  438,  210,  208,  217,  409,  206,  511,
 /*  1470 */   113,  187,  935, 1076, 1076,   89,  590,  568,  688,  687,
 /*  1480 */   686,  590,  568,  688,  687,  686, 1076, 1076, 1076, 1076,
 /*  1490 */   439,  681,  438,  590,  568,  688,  687,  686,  252, 1076,
 /*  1500 */  1076, 1076, 1076,  252, 1076,  564,  565,  562, 1076, 1076,
 /*  1510 */   563,  565,  562, 1076, 1076,  252,  590,  568,  688,  687,
 /*  1520 */   686, 1076,  427,  565,  562, 1076, 1076,  590,  568,  688,
 /*  1530 */   687,  686,  590,  568,  688,  687,  686, 1076,  252,  665,
 /*  1540 */   628,  688,  687,  686, 1076,  426,  565,  562, 1076,  252,
 /*  1550 */  1076, 1076, 1076, 1076,  252, 1076,  284,  565,  562,  630,
 /*  1560 */   311,  327,  565,  562,  590,  568,  688,  687,  686,  655,
 /*  1570 */  1076, 1076, 1076, 1076,  930, 1076, 1076,  647,  657,  658,
 /*  1580 */   656,  654,  653, 1076,  371,  313,  252, 1076, 1076,  930,
 /*  1590 */   930,  655, 1076,  326,  565,  562, 1076, 1076, 1076,  647,
 /*  1600 */   657,  658,  656,  654,  653, 1076, 1076,  638,  655,  930,
 /*  1610 */   930, 1076, 1076, 1076, 1076, 1076,  647,  657,  658,  656,
 /*  1620 */   654,  653,  930,  583,  634,  655, 1076, 1076,  930, 1076,
 /*  1630 */  1076, 1076, 1076,  647,  657,  658,  656,  654,  653,  655,
 /*  1640 */  1076,  637,  101,  100,   99,   98,   97,  647,  657,  658,
 /*  1650 */   656,  654,  653,  655, 1076,  636,  139,  137,  135,  133,
 /*  1660 */   132,  647,  657,  658,  656,  654,  653,  655,  558,  635,
 /*  1670 */   139,  137,  135,  133,  132,  647,  657,  658,  656,  654,
 /*  1680 */   653, 1076, 1076,  308, 1076, 1076, 1076, 1076,  931, 1076,
 /*  1690 */    43,   42,   41,   45,   44,  932, 1076,   46, 1076, 1076,
 /*  1700 */  1076, 1076,  933,  931,  931,  934, 1076, 1076, 1076, 1076,
 /*  1710 */   932,  932,  665,  310,  688,  687,  686,  933,  933, 1076,
 /*  1720 */   934,  934,  489,  931,  931, 1076, 1076, 1076, 1076, 1076,
 /*  1730 */   932,  932,  292,  311, 1076, 1076,  931,  933,  933, 1076,
 /*  1740 */   934,  934,  931,  932,   14,   13,   12,   16,   15,  932,
 /*  1750 */   933,   17, 1076,  934, 1076, 1076,  933, 1076, 1076,  934,
 /*  1760 */   665,  276,  688,  687,  686, 1076, 1076,  679,  684,  685,
 /*  1770 */   688,  687,  686, 1076, 1076,  279, 1076, 1076, 1076, 1076,
 /*  1780 */   630,  311, 1076, 1076,  679,  684,  685,  688,  687,  686,
 /*  1790 */  1076, 1076,  343, 1076, 1076, 1076, 1076, 1076,  679,  684,
 /*  1800 */   685,  688,  687,  686, 1076, 1076,  670, 1076, 1076, 1076,
 /*  1810 */  1076, 1076, 1076,  679,  684,  685,  688,  687,  686, 1076,
 /*  1820 */  1076,  666, 1076, 1076, 1076, 1076, 1076,  679,  684,  685,
 /*  1830 */   688,  687,  686, 1076, 1076,  669, 1076, 1076, 1076,  679,
 /*  1840 */   684,  685,  688,  687,  686, 1076, 1076,  668,  679,  684,
 /*  1850 */   685,  688,  687,  686, 1076, 1076,  667, 1076, 1076, 1076,
 /*  1860 */  1076,  679,  684,  685,  688,  687,  686, 1076, 1076,  341,
 /*  1870 */  1076,  679,  684,  685,  688,  687,  686, 1076, 1076,  340,
 /*  1880 */  1076,  679,  684,  685,  688,  687,  686, 1076, 1076,  661,
 /*  1890 */  1076,  679,  684,  685,  688,  687,  686, 1076, 1076,  660,
 /*  1900 */   679,  684,  685,  688,  687,  686, 1076, 1076,  659, 1076,
 /*  1910 */   679,  684,  685,  688,  687,  686, 1076, 1076,  320,  679,
 /*  1920 */   684,  685,  688,  687,  686, 1076, 1076,  319, 1076, 1076,
 /*  1930 */  1076,  679,  684,  685,  688,  687,  686, 1076, 1076,  318,
 /*  1940 */   679,  684,  685,  688,  687,  686, 1076, 1076,  317,  679,
 /*  1950 */   684,  685,  688,  687,  686, 1076, 1076,  316, 1076, 1076,
 /*  1960 */  1076, 1076, 1076, 1076,  679,  684,  685,  688,  687,  686,
 /*  1970 */  1076, 1076,  315, 1076, 1076, 1076, 1076, 1076,  679,  684,
 /*  1980 */   685,  688,  687,  686, 1076, 1076,  314, 1076, 1076, 1076,
 /*  1990 */   679,  684,  685,  688,  687,  686, 1076, 1076,  629,  679,
 /*  2000 */   684,  685,  688,  687,  686, 1076, 1076,  337, 1076, 1076,
 /*  2010 */  1076, 1076,  679,  684,  685,  688,  687,  686, 1076, 1076,
 /*  2020 */   336, 1076,  679,  684,  685,  688,  687,  686, 1076, 1076,
 /*  2030 */   627, 1076,  679,  684,  685,  688,  687,  686, 1076, 1076,
 /*  2040 */   626, 1076,  679,  684,  685,  688,  687,  686, 1076, 1076,
 /*  2050 */   625,  679,  684,  685,  688,  687,  686, 1076, 1076,  335,
 /*  2060 */  1076,  679,  684,  685,  688,  687,  686, 1076, 1076,  334,
 /*  2070 */   679,  684,  685,  688,  687,  686, 1076, 1076,  624, 1076,
 /*  2080 */  1076, 1076,  679,  684,  685,  688,  687,  686, 1076, 1076,
 /*  2090 */   623,  679,  684,  685,  688,  687,  686, 1076, 1076,  622,
 /*  2100 */   679,  684,  685,  688,  687,  686, 1076, 1076,  307, 1076,
 /*  2110 */  1076, 1076, 1076, 1076, 1076,  679,  684,  685,  688,  687,
 /*  2120 */   686, 1076, 1076,  306, 1076, 1076, 1076, 1076, 1076,  679,
 /*  2130 */   684,  685,  688,  687,  686, 1076, 1076,  305, 1076, 1076,
 /*  2140 */  1076,  679,  684,  685,  688,  687,  686, 1076, 1076,  304,
 /*  2150 */   679,  684,  685,  688,  687,  686, 1076, 1076,  303, 1076,
 /*  2160 */  1076, 1076, 1076,  679,  684,  685,  688,  687,  686, 1076,
 /*  2170 */  1076,  302, 1076,  679,  684,  685,  688,  687,  686, 1076,
 /*  2180 */  1076,  301, 1076,  679,  684,  685,  688,  687,  686, 1076,
 /*  2190 */  1076,  300, 1076,  679,  684,  685,  688,  687,  686, 1076,
 /*  2200 */  1076,  299,  679,  684,  685,  688,  687,  686, 1076, 1076,
 /*  2210 */   298, 1076,  679,  684,  685,  688,  687,  686, 1076, 1076,
 /*  2220 */   297,  679,  684,  685,  688,  687,  686, 1076, 1076,  296,
 /*  2230 */  1076, 1076, 1076,  679,  684,  685,  688,  687,  686, 1076,
 /*  2240 */  1076,  295,  679,  684,  685,  688,  687,  686, 1076, 1076,
 /*  2250 */   294,  679,  684,  685,  688,  687,  686, 1076, 1076,  178,
 /*  2260 */    43,   42,   41,   45,   44, 1076, 1076,   46, 1076,   14,
 /*  2270 */    13,   12,   16,   15, 1076, 1076,   17, 1076, 1076, 1076,
 /*  2280 */  1076,  586,  580,  688,  687,  686,  586,  580,  688,  687,
 /*  2290 */   686,  579, 1076, 1076, 1076, 1076,  575,  586,  580,  688,
 /*  2300 */   687,  686,  586,  580,  688,  687,  686,  578, 1076, 1076,
 /*  2310 */  1076, 1076,  577, 1076, 1076,  586,  580,  688,  687,  686,
 /*  2320 */   586,  580,  688,  687,  686,  576, 1076, 1076, 1076, 1076,
 /*  2330 */   328, 1076, 1076,  586,  580,  688,  687,  686,  586,  580,
 /*  2340 */   688,  687,  686,  574, 1076, 1076, 1076, 1076,  573,  586,
 /*  2350 */   580,  688,  687,  686,  586,  580,  688,  687,  686,  572,
 /*  2360 */  1076, 1076, 1076, 1076,  291,  586,  580,  688,  687,  686,
 /*  2370 */  1076, 1076, 1076, 1076, 1076,  290,  586,  580,  688,  687,
 /*  2380 */   686,  586,  580,  688,  687,  686,  289, 1076, 1076, 1076,
 /*  2390 */  1076,  288, 1076,  586,  580,  688,  687,  686,  586,  580,
 /*  2400 */   688,  687,  686,  287, 1076, 1076, 1076, 1076,  286, 1076,
 /*  2410 */  1076,  586,  580,  688,  687,  686,  586,  580,  688,  687,
 /*  2420 */   686,  285, 1076, 1076, 1076, 1076,  566, 1076,  586,  580,
 /*  2430 */   688,  687,  686,  586,  580,  688,  687,  686,  283, 1076,
 /*  2440 */  1076, 1076, 1076,  281, 1076, 1076, 1076, 1076,  586,  580,
 /*  2450 */   688,  687,  686, 1076, 1076, 1076, 1076, 1076,  267,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   132,  100,  101,  135,  136,  137,  138,  139,  140,  141,
 /*    10 */   142,  143,  144,  145,  146,  147,  148,  149,  154,  151,
 /*    20 */   152,  153,  154,  155,   91,   92,  162,  163,  164,  165,
 /*    30 */   166,  167,   99,  169,  170,  108,  109,  110,   70,  171,
 /*    40 */   172,   73,  174,  175,  176,  177,  178,  179,   70,   81,
 /*    50 */    82,   83,   84,   71,   86,   87,   88,   89,  190,   81,
 /*    60 */    82,   83,   84,   81,   86,   87,   88,   89,    2,  151,
 /*    70 */   152,  153,  154,  155,  106,  107,  108,  109,  110,  106,
 /*    80 */   107,  108,  109,  110,  106,  107,  108,  109,  110,  171,
 /*    90 */   172,    0,  224,  225,  226,  227,  228,  229,  230,  231,
 /*   100 */   232,  233,  234,  235,  236,  237,  238,  239,  240,    1,
 /*   110 */   198,  199,  200,    5,    6,    7,    8,    9,   10,   11,
 /*   120 */    12,    1,  108,  109,  110,    5,    6,  150,  151,  152,
 /*   130 */   153,  154,  155,   73,  157,  158,   28,   98,   99,   33,
 /*   140 */    32,  102,   34,   35,   72,   37,   73,   39,   72,   43,
 /*   150 */    98,   99,   44,   47,  102,   72,   48,   49,   50,   51,
 /*   160 */    52,   53,   54,  103,   56,   57,   46,   73,   48,   49,
 /*   170 */    50,   63,   72,   65,  189,   67,  103,   69,   70,   72,
 /*   180 */    72,  196,  197,   63,   76,   65,  108,   67,    3,    4,
 /*   190 */    70,  113,   72,   72,    1,  129,   76,  103,    5,    6,
 /*   200 */   159,   95,   96,   97,   98,   99,   82,   73,  102,  203,
 /*   210 */   204,  205,  206,  105,  106,   96,   97,   98,   99,  111,
 /*   220 */    74,  102,  114,   72,  183,  105,  106,   96,   97,   98,
 /*   230 */    99,  111,   72,  102,  126,  127,  128,  103,   73,  131,
 /*   240 */    94,   48,   49,   50,   73,   73,  126,  127,  128,   71,
 /*   250 */   151,  152,  153,  154,  155,  214,   63,   97,   65,   81,
 /*   260 */    67,   74,    1,   70,   73,   72,    5,    6,  103,   76,
 /*   270 */   171,  172,  100,  101,  175,  176,  177,  178,  179,  189,
 /*   280 */   154,   94,   77,  184,  102,  195,  196,  197,  162,  163,
 /*   290 */   164,  165,  166,  167,  103,  169,  170,   82,  105,  106,
 /*   300 */    96,   97,   98,   99,  111,  189,  102,   74,  103,   48,
 /*   310 */    49,   50,  196,  197,  215,  216,  217,  201,  202,  126,
 /*   320 */   127,  128,  189,   73,   63,   26,   65,   94,   67,  196,
 /*   330 */   197,   70,   33,   72,  108,  109,  110,   76,   70,   74,
 /*   340 */    41,   42,   43,    1,    1,    2,   47,    5,    6,    1,
 /*   350 */    82,   83,   84,  103,   86,   87,   88,   89,   41,   94,
 /*   360 */    61,   62,  102,  189,   98,   99,  105,  106,  102,  195,
 /*   370 */   196,  197,  111,   74,  106,  107,  108,  109,  110,   80,
 /*   380 */    77,    1,   77,    3,    4,    5,    6,  126,  127,  128,
 /*   390 */    48,   49,   50,   74,   95,   96,   97,   98,   99,    3,
 /*   400 */     4,  102,   74,  189,   73,   63,  103,   65,  103,  195,
 /*   410 */   196,  197,   70,   94,   72,   74,    1,   73,   76,   70,
 /*   420 */     5,    6,   73,  106,  107,  108,  109,  110,   48,   49,
 /*   430 */    50,   82,   83,   84,  103,   86,   87,   88,   89,  207,
 /*   440 */   208,  209,  189,   63,  103,   65,   74,  103,  106,  196,
 /*   450 */   197,   31,   72,  111,   68,  106,  107,  108,  109,  110,
 /*   460 */    73,  191,  192,   48,   49,   50,   94,   74,  126,  127,
 /*   470 */   128,  164,  151,  152,  153,  154,  155,  129,   63,    2,
 /*   480 */    65,   95,   96,   97,   98,   99,  106,   72,  102,    2,
 /*   490 */   103,  111,  171,  172,  187,  189,  175,  176,  177,  178,
 /*   500 */   179,  195,  196,  197,   74,  184,  126,  127,  128,   22,
 /*   510 */    23,   24,   25,   97,    1,   75,  189,   30,    5,    6,
 /*   520 */   105,  106,   73,  196,  197,   38,  111,   40,  210,  211,
 /*   530 */   212,   62,   45,  204,  205,  206,  159,  216,  217,   73,
 /*   540 */    75,  126,  127,  128,   57,   58,   59,   60,   27,   72,
 /*   550 */   151,  152,  153,  154,  155,  106,  107,  108,  109,  110,
 /*   560 */   183,   48,   49,   50,   95,   96,   97,   98,   99,  103,
 /*   570 */    73,  102,  173,   73,  199,  200,   63,   72,   65,  180,
 /*   580 */   181,  182,   82,   83,   84,   72,   86,   87,   88,   89,
 /*   590 */   213,  214,   95,   96,   97,   98,   99,   76,   14,  102,
 /*   600 */    16,   17,   18,   19,   20,   21,  106,  107,  108,  109,
 /*   610 */   110,  151,  152,  153,  154,  155,  129,  208,  209,  106,
 /*   620 */   160,  161,   73,   41,  111,  151,  152,  153,  154,  155,
 /*   630 */   185,  186,   48,   49,  151,  152,  153,  154,  155,  126,
 /*   640 */   127,  128,  164,   91,   92,  171,  172,  126,  174,  175,
 /*   650 */   176,  177,  178,  179,  171,  172,   55,  151,  152,  153,
 /*   660 */   154,  155,   80,  154,    2,  187,  151,  152,  153,  154,
 /*   670 */   155,  162,  163,  164,  165,  166,  167,  171,  172,  170,
 /*   680 */   174,  175,  176,  177,  178,  179,  171,  172,  152,  174,
 /*   690 */   175,  176,  177,  178,  179,  151,  152,  153,  154,  155,
 /*   700 */    73,   72,  151,  152,  153,  154,  155,  151,  152,  153,
 /*   710 */   154,  155,   72,  177,  130,  171,  172,  161,  174,  175,
 /*   720 */   176,  177,  178,  179,  173,  151,  152,  153,  154,  155,
 /*   730 */   103,  180,  181,  182,  151,  152,  153,  154,  155,  150,
 /*   740 */   151,  152,  153,  154,  155,  171,  172,  158,  174,  175,
 /*   750 */   176,  177,  178,  179,  171,  172,   76,  174,  175,  176,
 /*   760 */   177,  178,  179,  151,  152,  153,  154,  155,  151,  152,
 /*   770 */   153,  154,  155,  161,  154,  211,  212,  151,  152,  153,
 /*   780 */   154,  155,  162,  163,  164,  165,  166,  167,  171,  172,
 /*   790 */   170,  174,  175,  176,  177,  178,  179,  171,  172,    2,
 /*   800 */   174,  175,  176,  177,  178,  179,  126,  193,  194,  151,
 /*   810 */   152,  153,  154,  155,    2,  154,  211,  212,  151,  152,
 /*   820 */   153,  154,  155,  162,  163,  164,  165,  166,  167,  171,
 /*   830 */   172,  170,  174,  175,  176,  177,  178,  179,  171,  172,
 /*   840 */    73,  174,  175,  176,  177,  178,  179,  151,  152,  153,
 /*   850 */   154,  155,   72,  154,  151,  152,  153,  154,  155,   72,
 /*   860 */    27,  162,  163,  164,  165,  166,  167,  171,  172,  170,
 /*   870 */   174,  175,  176,  177,  178,  179,  173,  151,  152,  153,
 /*   880 */   154,  155,   72,  180,  181,  182,  151,  152,  153,  154,
 /*   890 */   155,  150,  151,  152,  153,  154,  155,  171,  172,  158,
 /*   900 */   174,  175,  176,  177,  178,  179,  171,  172,   74,   76,
 /*   910 */   175,  176,  177,  178,  179,  211,  212,  211,  212,  184,
 /*   920 */   151,  152,  153,  154,  155,   41,  129,  152,  133,  134,
 /*   930 */   151,  152,  153,  154,  155,  152,   73,  103,    2,  103,
 /*   940 */   171,  172,    2,   76,  175,  176,  177,  178,  179,   72,
 /*   950 */   171,  172,  177,  184,  175,  176,  177,  178,  179,  126,
 /*   960 */   177,   82,   74,  184,  151,  152,  153,  154,  155,  106,
 /*   970 */   107,  108,  109,  110,  151,  152,  153,  154,  155,   95,
 /*   980 */    96,   97,   98,   99,  171,  172,  102,   76,  175,  176,
 /*   990 */   177,  178,  179,  126,  171,  172,   82,  184,  175,  176,
 /*  1000 */   177,  178,  179,   74,    1,   74,   82,  184,  150,  151,
 /*  1010 */   152,  153,  154,  155,  159,  157,  158,    1,   74,  151,
 /*  1020 */   152,  153,  154,  155,  151,  152,  153,  154,  155,  151,
 /*  1030 */   152,  153,  154,  155,  161,  102,   77,  126,  183,  171,
 /*  1040 */   172,   77,   74,  175,  176,  177,  178,  179,   74,  171,
 /*  1050 */   172,   74,  184,  175,  176,  177,  178,  179,   74,   74,
 /*  1060 */    74,   74,  184,  151,  152,  153,  154,  155,  213,  214,
 /*  1070 */    66,   74,   74,  151,  152,  153,  154,  155,   95,   96,
 /*  1080 */    97,   98,   99,  171,  172,  102,   74,  175,  176,  177,
 /*  1090 */   178,  179,   74,  171,  172,   74,  184,  175,  176,  177,
 /*  1100 */   178,  179,   74,  188,  188,  164,  184,  151,  152,  153,
 /*  1110 */   154,  155,  152,  219,  126,  126,  154,  127,  151,  152,
 /*  1120 */   153,  154,  155,  154,   41,  222,   61,  171,  172,  223,
 /*  1130 */    62,  175,  176,  177,  178,  179,  223,   26,  171,  172,
 /*  1140 */   184,  174,  175,  176,  177,  178,  179,  151,  152,  153,
 /*  1150 */   154,  155,  106,  107,  108,  109,  110,  152,  219,  151,
 /*  1160 */   152,  153,  154,  155,   74,  222,    2,  171,  172,  222,
 /*  1170 */   174,  175,  176,  177,  178,  179,  223,  197,    2,  171,
 /*  1180 */   172,  159,  174,  175,  176,  177,  178,  179,  197,  126,
 /*  1190 */    42,  151,  152,  153,  154,  155,  106,  107,  108,  109,
 /*  1200 */   110,  151,  152,  153,  154,  155,   95,   96,   97,   98,
 /*  1210 */    99,  171,  172,  102,  174,  175,  176,  177,  178,  179,
 /*  1220 */   221,  171,  172,  192,  174,  175,  176,  177,  178,  179,
 /*  1230 */   151,  152,  153,  154,  155,  151,  152,  153,  154,  155,
 /*  1240 */   161,   26,  222,  220,  219,  151,  152,  153,  154,  155,
 /*  1250 */   106,  107,  108,  109,  110,  171,  172,  223,  174,  175,
 /*  1260 */   176,  177,  178,  179,  221,  171,  172,  220,  174,  175,
 /*  1270 */   176,  177,  178,  179,  151,  152,  153,  154,  155,  222,
 /*  1280 */    73,  223,  221,  219,   82,   83,   84,  220,   86,   87,
 /*  1290 */    88,   89,  222,  222,  171,  172,  223,  174,  175,  176,
 /*  1300 */   177,  178,  179,  106,  107,  108,  109,  110,  106,  107,
 /*  1310 */   108,  109,  110,  106,  107,  108,  109,  110,   82,   83,
 /*  1320 */    84,  223,   86,   87,   88,   89,  222,  222,   82,   83,
 /*  1330 */    84,  223,   86,   87,   88,   89,    1,  223,  222,  219,
 /*  1340 */     5,    6,  106,  107,  108,  109,  110,  151,  152,  153,
 /*  1350 */   154,  155,  106,  107,  108,  109,  110,   82,   83,   84,
 /*  1360 */   223,   86,   87,   88,   89,    1,  221,  171,  172,    5,
 /*  1370 */     6,  220,  176,  177,  222,  179,  151,  152,  153,  154,
 /*  1380 */   155,  106,  107,  108,  109,  110,  190,  150,  151,  152,
 /*  1390 */   153,  154,  155,  223,  157,  158,  171,  172,   63,  152,
 /*  1400 */    65,  176,  177,  219,  179,  222,  221,   72,  220,  223,
 /*  1410 */   152,  219,   48,   49,   50,  190,  221,  151,  152,  153,
 /*  1420 */   154,  155,  222,  220,   26,  152,  222,  151,  152,  153,
 /*  1430 */   154,  155,  223,  223,  223,  166,   72,  171,  172,   41,
 /*  1440 */    42,  106,  176,  177,  194,  179,  111,  171,  172,  206,
 /*  1450 */   219,  186,  176,  177,  219,  179,  190,  223,  220,   61,
 /*  1460 */    62,  126,  127,  128,  220,  220,  190,    2,  220,    2,
 /*  1470 */   106,  219,   74,  241,  241,  111,  151,  152,  153,  154,
 /*  1480 */   155,  151,  152,  153,  154,  155,  241,  241,  241,  241,
 /*  1490 */   126,  127,  128,  151,  152,  153,  154,  155,  173,  241,
 /*  1500 */   241,  241,  241,  173,  241,  180,  181,  182,  241,  241,
 /*  1510 */   180,  181,  182,  241,  241,  173,  151,  152,  153,  154,
 /*  1520 */   155,  241,  180,  181,  182,  241,  241,  151,  152,  153,
 /*  1530 */   154,  155,  151,  152,  153,  154,  155,  241,  173,  151,
 /*  1540 */   152,  153,  154,  155,  241,  180,  181,  182,  241,  173,
 /*  1550 */   241,  241,  241,  241,  173,  241,  180,  181,  182,  171,
 /*  1560 */   172,  180,  181,  182,  151,  152,  153,  154,  155,  154,
 /*  1570 */   241,  241,  241,  241,   26,  241,  241,  162,  163,  164,
 /*  1580 */   165,  166,  167,  241,  169,  170,  173,  241,  241,   41,
 /*  1590 */    42,  154,  241,  180,  181,  182,  241,  241,  241,  162,
 /*  1600 */   163,  164,  165,  166,  167,  241,  241,  170,  154,   61,
 /*  1610 */    62,  241,  241,  241,  241,  241,  162,  163,  164,  165,
 /*  1620 */   166,  167,   74,   73,  170,  154,  241,  241,   80,  241,
 /*  1630 */   241,  241,  241,  162,  163,  164,  165,  166,  167,  154,
 /*  1640 */   241,  170,  106,  107,  108,  109,  110,  162,  163,  164,
 /*  1650 */   165,  166,  167,  154,  241,  170,  106,  107,  108,  109,
 /*  1660 */   110,  162,  163,  164,  165,  166,  167,  154,   73,  170,
 /*  1670 */   106,  107,  108,  109,  110,  162,  163,  164,  165,  166,
 /*  1680 */   167,  241,  241,  170,  241,  241,  241,  241,   26,  241,
 /*  1690 */    95,   96,   97,   98,   99,   26,  241,  102,  241,  241,
 /*  1700 */   241,  241,   26,   41,   42,   26,  241,  241,  241,  241,
 /*  1710 */    41,   42,  151,  152,  153,  154,  155,   41,   42,  241,
 /*  1720 */    41,   42,   73,   61,   62,  241,  241,  241,  241,  241,
 /*  1730 */    61,   62,  171,  172,  241,  241,   74,   61,   62,  241,
 /*  1740 */    61,   62,   80,   74,   95,   96,   97,   98,   99,   80,
 /*  1750 */    74,  102,  241,   74,  241,  241,   80,  241,  241,   80,
 /*  1760 */   151,  152,  153,  154,  155,  241,  241,  150,  151,  152,
 /*  1770 */   153,  154,  155,  241,  241,  158,  241,  241,  241,  241,
 /*  1780 */   171,  172,  241,  241,  150,  151,  152,  153,  154,  155,
 /*  1790 */   241,  241,  158,  241,  241,  241,  241,  241,  150,  151,
 /*  1800 */   152,  153,  154,  155,  241,  241,  158,  241,  241,  241,
 /*  1810 */   241,  241,  241,  150,  151,  152,  153,  154,  155,  241,
 /*  1820 */   241,  158,  241,  241,  241,  241,  241,  150,  151,  152,
 /*  1830 */   153,  154,  155,  241,  241,  158,  241,  241,  241,  150,
 /*  1840 */   151,  152,  153,  154,  155,  241,  241,  158,  150,  151,
 /*  1850 */   152,  153,  154,  155,  241,  241,  158,  241,  241,  241,
 /*  1860 */   241,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  1870 */   241,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  1880 */   241,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  1890 */   241,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  1900 */   150,  151,  152,  153,  154,  155,  241,  241,  158,  241,
 /*  1910 */   150,  151,  152,  153,  154,  155,  241,  241,  158,  150,
 /*  1920 */   151,  152,  153,  154,  155,  241,  241,  158,  241,  241,
 /*  1930 */   241,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  1940 */   150,  151,  152,  153,  154,  155,  241,  241,  158,  150,
 /*  1950 */   151,  152,  153,  154,  155,  241,  241,  158,  241,  241,
 /*  1960 */   241,  241,  241,  241,  150,  151,  152,  153,  154,  155,
 /*  1970 */   241,  241,  158,  241,  241,  241,  241,  241,  150,  151,
 /*  1980 */   152,  153,  154,  155,  241,  241,  158,  241,  241,  241,
 /*  1990 */   150,  151,  152,  153,  154,  155,  241,  241,  158,  150,
 /*  2000 */   151,  152,  153,  154,  155,  241,  241,  158,  241,  241,
 /*  2010 */   241,  241,  150,  151,  152,  153,  154,  155,  241,  241,
 /*  2020 */   158,  241,  150,  151,  152,  153,  154,  155,  241,  241,
 /*  2030 */   158,  241,  150,  151,  152,  153,  154,  155,  241,  241,
 /*  2040 */   158,  241,  150,  151,  152,  153,  154,  155,  241,  241,
 /*  2050 */   158,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  2060 */   241,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  2070 */   150,  151,  152,  153,  154,  155,  241,  241,  158,  241,
 /*  2080 */   241,  241,  150,  151,  152,  153,  154,  155,  241,  241,
 /*  2090 */   158,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  2100 */   150,  151,  152,  153,  154,  155,  241,  241,  158,  241,
 /*  2110 */   241,  241,  241,  241,  241,  150,  151,  152,  153,  154,
 /*  2120 */   155,  241,  241,  158,  241,  241,  241,  241,  241,  150,
 /*  2130 */   151,  152,  153,  154,  155,  241,  241,  158,  241,  241,
 /*  2140 */   241,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  2150 */   150,  151,  152,  153,  154,  155,  241,  241,  158,  241,
 /*  2160 */   241,  241,  241,  150,  151,  152,  153,  154,  155,  241,
 /*  2170 */   241,  158,  241,  150,  151,  152,  153,  154,  155,  241,
 /*  2180 */   241,  158,  241,  150,  151,  152,  153,  154,  155,  241,
 /*  2190 */   241,  158,  241,  150,  151,  152,  153,  154,  155,  241,
 /*  2200 */   241,  158,  150,  151,  152,  153,  154,  155,  241,  241,
 /*  2210 */   158,  241,  150,  151,  152,  153,  154,  155,  241,  241,
 /*  2220 */   158,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  2230 */   241,  241,  241,  150,  151,  152,  153,  154,  155,  241,
 /*  2240 */   241,  158,  150,  151,  152,  153,  154,  155,  241,  241,
 /*  2250 */   158,  150,  151,  152,  153,  154,  155,  241,  241,  158,
 /*  2260 */    95,   96,   97,   98,   99,  241,  241,  102,  241,   95,
 /*  2270 */    96,   97,   98,   99,  241,  241,  102,  241,  241,  241,
 /*  2280 */   241,  151,  152,  153,  154,  155,  151,  152,  153,  154,
 /*  2290 */   155,  161,  241,  241,  241,  241,  161,  151,  152,  153,
 /*  2300 */   154,  155,  151,  152,  153,  154,  155,  161,  241,  241,
 /*  2310 */   241,  241,  161,  241,  241,  151,  152,  153,  154,  155,
 /*  2320 */   151,  152,  153,  154,  155,  161,  241,  241,  241,  241,
 /*  2330 */   161,  241,  241,  151,  152,  153,  154,  155,  151,  152,
 /*  2340 */   153,  154,  155,  161,  241,  241,  241,  241,  161,  151,
 /*  2350 */   152,  153,  154,  155,  151,  152,  153,  154,  155,  161,
 /*  2360 */   241,  241,  241,  241,  161,  151,  152,  153,  154,  155,
 /*  2370 */   241,  241,  241,  241,  241,  161,  151,  152,  153,  154,
 /*  2380 */   155,  151,  152,  153,  154,  155,  161,  241,  241,  241,
 /*  2390 */   241,  161,  241,  151,  152,  153,  154,  155,  151,  152,
 /*  2400 */   153,  154,  155,  161,  241,  241,  241,  241,  161,  241,
 /*  2410 */   241,  151,  152,  153,  154,  155,  151,  152,  153,  154,
 /*  2420 */   155,  161,  241,  241,  241,  241,  161,  241,  151,  152,
 /*  2430 */   153,  154,  155,  151,  152,  153,  154,  155,  161,  241,
 /*  2440 */   241,  241,  241,  161,  241,  241,  241,  241,  151,  152,
 /*  2450 */   153,  154,  155,  241,  241,  241,  241,  241,  161,
};
#define YY_SHIFT_USE_DFLT (-100)
#define YY_SHIFT_COUNT (440)
#define YY_SHIFT_MIN   (-99)
#define YY_SHIFT_MAX   (2174)
static const short yy_shift_ofst[] = {
 /*     0 */  -100,  108,  120,  120,  193,  193,  193,  193,  193,  193,
 /*    10 */   193,  193,  261,  261,  261,  261,  261,  261,  261,  261,
 /*    20 */   261,  261,  193,  193,  193,  193,  193,  193,  193,  193,
 /*    30 */   193,  193,  193,  193,  342,  342,  342,  342,  380,  380,
 /*    40 */   380,  415,  415,  415,  415,  415,  415,  415,  415,  415,
 /*    50 */   415,  380,  380,  380,  380,  380,  380,  380,  380,  380,
 /*    60 */   380,  513,  513,  513,  513,  513,  513,  513,  513,  513,
 /*    70 */   513,  513,  513,  513,  513,  513,  513,  513,  513,  513,
 /*    80 */   513,  513,  513,  513,  513,  513,  513,  513,  513,  513,
 /*    90 */   513,  513,  513,  513,  513,  513,  513,  513,  513,  513,
 /*   100 */   513,  513,  513,  513,  513,  513,  513,  513,  513,  513,
 /*   110 */   513, 1364, 1364, 1364, 1364, 1364, 1335, 1335, 1335, 1335,
 /*   120 */  1335, 1335, 1335, 1335, 1335, 1335, 1335, 1335, 1335, 1335,
 /*   130 */  1335, 1335, 1335, 1335, 1335, 1335, 1335, 1335, 1335, 1335,
 /*   140 */  1335,  487,  833,  521,  797,   66,  797,  797,  797,  911,
 /*   150 */   477,   66,  797,  797,  797,  797, 1467, 1465,  867,  867,
 /*   160 */   477,  477,  477,  477,  396,  867,  680,  552,  680, 1467,
 /*   170 */  1465, 1164,  884, 1111, 1111, 1111, 1111,  469,  317,  -67,
 /*   180 */   582,   66,  396,  185, 1068,  989, 1068, 1065, 1068, 1065,
 /*   190 */  1215, 1148, 1083,  989, 1068, 1065, 1215, 1148, 1083,  989,
 /*   200 */  1068, 1065, 1215, 1148, 1083, 1068, 1065, 1068, 1065, 1068,
 /*   210 */  1065, 1068, 1065, 1068, 1065, 1215, 1148, 1083, 1068, 1065,
 /*   220 */  1215, 1148, 1083, 1068, 1065, 1215, 1148, 1063, 1176, 1176,
 /*   230 */  1164, 1068, 1065, 1083, 1068, 1065,  989, 1068, 1065, 1083,
 /*   240 */   990,  990,  989,  988, -100, -100,  299,  -32,  -22,  349,
 /*   250 */   500,  268, 1275, 1246, 1236, 1202,  584,  106, 1649, 1679,
 /*   260 */  1676, 1669, 1662, 1548, 1595,  497,  386, 1090,  983,  983,
 /*   270 */  2174, 2174, 2174, 2165, 1550,  983, 1398, 1207,  863,  449,
 /*   280 */   204, 1564, 1564, 1564,  131, 1564, 1564, 1564, 1564, 1564,
 /*   290 */  1564, 1564, 1536,  119,  -27,  -27,  -27,  -27,  -27,  -27,
 /*   300 */   -27,  -27,  -27,  -27,  -27,  -27,  -27,  -27, 1046, 1536,
 /*   310 */  1197, 1144, 1046, 1046,  -27,  -27,  -27,  -27,  -27,  -27,
 /*   320 */   -27,  -27,  -27,  266,  266,  172,   52,   52,  226,  226,
 /*   330 */   226,  226,   39,   39,  -73,  -73,  -73,  -73,   14,   14,
 /*   340 */   -73,  -73,  -73,  -73,  343,  372,  348,  834,  341,  627,
 /*   350 */   -99,  -99,  -99,  -99,  319,  305,  265,  466,  303,  233,
 /*   360 */   387,  344,  331,  205,  187,  250,  160,  146,  178,   78,
 /*   370 */   -18,  191,  165,  134,   94,   73,   60, 1004, 1028, 1021,
 /*   380 */  1018, 1012,  998,  997,  987,  986,  985,  984,  977,  974,
 /*   390 */   968,  964,  959,  933,  933,  944, 1016,  924,  931, 1003,
 /*   400 */   914,  929,  888,  879,  877,  940,  836,  936,  810,  787,
 /*   410 */   780,  812,  767,  640,  629,  662,  601,  549,  505,  465,
 /*   420 */   440,  416,  430,  393,  420,  328,  260,  260,  215,  182,
 /*   430 */   182,  124,  171,  151,  121,  107,  100,   83,   76,   72,
 /*   440 */    91,
};
#define YY_REDUCE_USE_DFLT (-137)
#define YY_REDUCE_COUNT (245)
#define YY_REDUCE_MIN   (-136)
#define YY_REDUCE_MAX   (2297)
static const short yy_reduce_ofst[] = {
 /*     0 */   795, -132,   99,  321, 1123, 1094, 1084, 1050, 1040, 1008,
 /*    10 */   996,  967,  956,  922,  912,  878,  868,  823,  813,  779,
 /*    20 */   769,  735,  726,  696,  667,  658,  626,  617,  583,  574,
 /*    30 */   544,  515,  506,  474, 1276, 1266, 1225, 1196, 1415,  126,
 /*    40 */  -136, 1413, 1381, 1376, 1365, 1342, 1330, 1325,  703,  551,
 /*    50 */   399, 1513, 1499, 1485, 1471, 1454, 1437,  699,  661,  620,
 /*    60 */   509, 1237,  858,  -23, 2101, 2092, 2083, 2071, 2062, 2052,
 /*    70 */  2043, 2033, 2023, 2013, 2000, 1991, 1979, 1965, 1950, 1941,
 /*    80 */  1932, 1920, 1911, 1901, 1892, 1882, 1872, 1862, 1849, 1840,
 /*    90 */  1828, 1814, 1799, 1790, 1781, 1769, 1760, 1750, 1741, 1731,
 /*   100 */  1721, 1711, 1698, 1689, 1677, 1663, 1648, 1634, 1617,  741,
 /*   110 */   589, 1609, 1561, 1388,  483,  -82,  460, 2297, 2282, 2277,
 /*   120 */  2265, 2260, 2247, 2242, 2230, 2225, 2214, 2203, 2198, 2187,
 /*   130 */  2182, 2169, 2164, 2151, 2146, 2135, 2130, 1079,  873,  612,
 /*   140 */   556,  116,  855,  377,  306,    6,  214,  174,   90,   41,
 /*   150 */   318,  329,  327,  253,  133,  -15,  232,  -88,  783,  775,
 /*   160 */   706,  704,  605,  564,  614,  536,  478,  445,  307,  409,
 /*   170 */   375,  270, 1252, 1248, 1245, 1244, 1238, 1234, 1235, 1265,
 /*   180 */  1231, 1243, 1250, 1269, 1211, 1273, 1210, 1204, 1209, 1200,
 /*   190 */  1203, 1195, 1192, 1258, 1186, 1183, 1188, 1185, 1184, 1247,
 /*   200 */  1170, 1152, 1151, 1145, 1120, 1137, 1116, 1114, 1105, 1108,
 /*   210 */  1104, 1098, 1071, 1073, 1070, 1067, 1061, 1064, 1058, 1057,
 /*   220 */  1047, 1043, 1025, 1034, 1020, 1023,  999, 1022,  991,  980,
 /*   230 */  1031,  953,  947,  939,  913,  943, 1005,  906,  903,  894,
 /*   240 */   969,  962,  960,  941,  916,  915,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   706, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*    10 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*    20 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*    30 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*    40 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*    50 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*    60 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*    70 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*    80 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*    90 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   100 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   110 */  1074, 1074,  921, 1074, 1074,  922, 1074, 1074, 1074, 1074,
 /*   120 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   130 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   140 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   150 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   160 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   170 */  1074, 1074, 1025, 1027, 1027, 1027, 1027, 1033, 1025, 1074,
 /*   180 */  1025, 1074, 1074, 1074, 1033, 1074, 1033, 1031, 1033, 1031,
 /*   190 */  1027, 1029, 1025, 1074, 1033, 1031, 1027, 1029, 1025, 1074,
 /*   200 */  1033, 1031, 1027, 1029, 1025, 1033, 1031, 1033, 1031, 1033,
 /*   210 */  1031, 1033, 1031, 1033, 1031, 1027, 1029, 1025, 1033, 1031,
 /*   220 */  1027, 1029, 1025, 1033, 1031, 1027, 1029, 1074, 1074, 1074,
 /*   230 */  1074, 1033, 1031, 1025, 1033, 1031, 1074, 1033, 1031, 1025,
 /*   240 */  1074, 1074, 1074, 1074,  928,  928,  782, 1074, 1074, 1074,
 /*   250 */  1074, 1074, 1074,  879, 1074,  879, 1074, 1074, 1074,  858,
 /*   260 */   857,  856,  854,  853, 1074, 1074, 1074, 1074, 1026, 1028,
 /*   270 */  1017, 1014,  915, 1032, 1074, 1024,  782, 1074, 1074, 1074,
 /*   280 */   912,  904,  739,  740,  889,  901,  900,  899,  898,  897,
 /*   290 */   896,  895,  923,  850,  872,  871,  870,  869,  868,  867,
 /*   300 */   866,  881,  878,  877,  876,  875,  874,  873,  927,  924,
 /*   310 */  1074, 1074,  756,  755,  865,  864,  863,  862,  861,  860,
 /*   320 */   859,  736,  735,  914,  913, 1074,  891,  890,  825,  838,
 /*   330 */   839,  824,  852,  851,  784,  783,  789,  788,  809,  810,
 /*   340 */   794,  793,  768,  769, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   350 */   994,  998,  997,  995, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   360 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,  949,
 /*   370 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   380 */  1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074, 1074,
 /*   390 */  1074, 1074,  777,  911,  910, 1074, 1074, 1074, 1074, 1074,
 /*   400 */  1074, 1074, 1074,  902,  738, 1074,  996, 1074,  984,  961,
 /*   410 */   963, 1074, 1074,  976,  959, 1074, 1074, 1074,  958, 1074,
 /*   420 */  1074, 1074, 1074, 1074, 1074, 1074,  888,  887,  879,  849,
 /*   430 */   848,  925, 1074, 1074,  754,  750,  748,  734,  731,  729,
 /*   440 */  1074, 1073, 1072, 1071, 1070, 1069, 1068, 1067, 1066, 1065,
 /*   450 */  1064, 1063, 1062, 1061, 1060, 1059, 1058, 1053, 1052, 1054,
 /*   460 */  1051, 1050, 1049, 1048, 1047, 1046, 1045, 1044, 1043, 1042,
 /*   470 */  1041, 1040, 1039, 1038, 1037, 1036, 1035, 1034, 1010, 1009,
 /*   480 */  1016, 1015, 1023, 1022, 1019, 1018, 1013, 1021, 1020,  906,
 /*   490 */   908,  909,  907,  905,  780, 1012, 1011, 1005, 1004, 1006,
 /*   500 */  1003, 1008, 1007, 1002,  903,  737, 1000,  999, 1001,  993,
 /*   510 */   988,  991,  992,  990,  989,  987,  979,  982,  986,  985,
 /*   520 */   983,  981,  980,  978,  954,  962,  964,  977,  975,  974,
 /*   530 */   973,  972,  971,  970,  969,  968,  967,  966,  965,  960,
 /*   540 */   946,  945,  957,  956,  955,  953,  937,  940,  941,  944,
 /*   550 */   943,  942,  939,  938,  936, 1057, 1056, 1055,  883,  885,
 /*   560 */   894,  893,  892,  886,  884,  882,  823,  822,  821,  820,
 /*   570 */   819,  818,  828,  827,  826,  840,  842,  841,  837,  836,
 /*   580 */   835,  834,  833,  832,  831,  830,  829,  817,  816,  815,
 /*   590 */   814, 1030,  934,  933,  932,  931,  930,  844,  846,  917,
 /*   600 */   920,  919,  918,  916,  880,  858,  857,  856,  855,  854,
 /*   610 */   853,  847,  845,  843,  780,  926,  952,  951,  950,  948,
 /*   620 */   947,  929,  787,  786,  785,  792,  791,  790,  782,  781,
 /*   630 */   780,  779,  778,  777,  811,  813,  812,  808,  807,  806,
 /*   640 */   805,  804,  803,  802,  801,  800,  799,  798,  753,  752,
 /*   650 */   751,  749,  747,  746,  745,  744,  743,  742,  741,  797,
 /*   660 */   796,  795,  776,  775,  774,  773,  770,  772,  771,  767,
 /*   670 */   766,  765,  764,  763,  762,  761,  760,  759,  758,  757,
 /*   680 */   733,  732,  730,  728,  724,  723,  727,  726,  725,  722,
 /*   690 */   721,  720,  719,  718,  717,  716,  715,  714,  713,  712,
 /*   700 */   711,  710,  709,  708,  707,
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

//#ifndef NDEBUG
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
  "constant_dcl_lst",  "constant_dcl_type",  "attrib_spec",   "object_bnd_lst",
  "object_bnd",    "object_lst",    "object_spec",   "variable_bnd_lst",
  "variable_bnd",  "variable_lst",  "sort_bnd_lst",  "sort_bnd",    
  "sort_dcl_lst",  "show_lst",      "show_elem",     "query_lst",   
  "query_maxstep_decl",  "query_label_decl",  "query_label_Decl",  "clause_if",   
  "clause_after",  "clause_ifcons",  "clause_unless",  "clause_where",
  "law_basic",     "law_caused",    "law_pcaused",   "law_impl",    
  "law_causes",    "law_increments",  "law_mcause",    "law_always",  
  "law_constraint",  "law_impossible",  "law_never",     "law_default", 
  "law_exogenous",  "law_inertial",  "law_nonexecutable",  "law_rigid",   
  "law_observed",
};
//#endif /* NDEBUG */

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
 /* 253 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 254 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 255 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 256 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 257 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 258 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 259 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 260 */ "constant_dcl_type ::= ABACTION",
 /* 261 */ "constant_dcl_type ::= ACTION",
 /* 262 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 263 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 264 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 265 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 266 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 267 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 268 */ "constant_dcl_type ::= RIGID",
 /* 269 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 270 */ "constant_dcl_type ::= SDFLUENT",
 /* 271 */ "attrib_spec ::= ATTRIBUTE",
 /* 272 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 273 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 274 */ "object_bnd_lst ::= object_bnd",
 /* 275 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 276 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 277 */ "object_lst ::= object_spec",
 /* 278 */ "object_lst ::= object_lst COMMA object_spec",
 /* 279 */ "object_spec ::= IDENTIFIER",
 /* 280 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 281 */ "object_spec ::= NUMBER_RANGE",
 /* 282 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 283 */ "variable_bnd_lst ::= variable_bnd",
 /* 284 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 285 */ "variable_bnd ::= variable_lst DBL_COLON sort_id",
 /* 286 */ "variable_lst ::= IDENTIFIER",
 /* 287 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 288 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 289 */ "sort_bnd_lst ::= sort_bnd",
 /* 290 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 291 */ "sort_bnd ::= sort_dcl_lst",
 /* 292 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 293 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 294 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 295 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 296 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 297 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 298 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 299 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 300 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 301 */ "show_lst ::= show_elem",
 /* 302 */ "show_lst ::= show_lst COMMA show_elem",
 /* 303 */ "show_elem ::= atomic_formula_one_const",
 /* 304 */ "stmt_noconcurrency ::= NOCONCURRENCY",
 /* 305 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY",
 /* 306 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ INTEGER PERIOD",
 /* 307 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ INTEGER PERIOD",
 /* 308 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 309 */ "query_lst ::= formula_temporal",
 /* 310 */ "query_lst ::= query_maxstep_decl",
 /* 311 */ "query_lst ::= query_label_decl",
 /* 312 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 313 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 314 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 315 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 316 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON NUMBER_RANGE",
 /* 317 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 318 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 319 */ "clause_if ::= IF formula",
 /* 320 */ "clause_if ::=",
 /* 321 */ "clause_after ::= AFTER formula",
 /* 322 */ "clause_after ::=",
 /* 323 */ "clause_ifcons ::= IFCONS formula",
 /* 324 */ "clause_ifcons ::=",
 /* 325 */ "clause_unless ::= UNLESS atomic_formula",
 /* 326 */ "clause_unless ::=",
 /* 327 */ "clause_where ::= WHERE formula_no_const",
 /* 328 */ "clause_where ::=",
 /* 329 */ "stmt_law ::= law_basic",
 /* 330 */ "stmt_law ::= law_caused",
 /* 331 */ "stmt_law ::= law_pcaused",
 /* 332 */ "stmt_law ::= law_impl",
 /* 333 */ "stmt_law ::= law_causes",
 /* 334 */ "stmt_law ::= law_increments",
 /* 335 */ "stmt_law ::= law_mcause",
 /* 336 */ "stmt_law ::= law_always",
 /* 337 */ "stmt_law ::= law_constraint",
 /* 338 */ "stmt_law ::= law_impossible",
 /* 339 */ "stmt_law ::= law_never",
 /* 340 */ "stmt_law ::= law_default",
 /* 341 */ "stmt_law ::= law_exogenous",
 /* 342 */ "stmt_law ::= law_inertial",
 /* 343 */ "stmt_law ::= law_nonexecutable",
 /* 344 */ "stmt_law ::= law_rigid",
 /* 345 */ "stmt_law ::= law_observed",
 /* 346 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 347 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 348 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 349 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 350 */ "law_causes ::= formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 351 */ "law_increments ::= formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 352 */ "law_mcause ::= formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 353 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 354 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 355 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 356 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 357 */ "law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 358 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 359 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 360 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 361 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 362 */ "law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD",
 /* 363 */ "stmt_code_blk ::= ASP_GR",
 /* 364 */ "stmt_code_blk ::= ASP_CP",
 /* 365 */ "stmt_code_blk ::= F2LP_GR",
 /* 366 */ "stmt_code_blk ::= F2LP_CP",
 /* 367 */ "stmt_code_blk ::= LUA_GR",
 /* 368 */ "stmt_code_blk ::= LUA_CP",
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
#line 185 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));								
#line 2202 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 133: /* start */
    case 134: /* statement_lst */
    case 156: /* undeclared */
    case 168: /* undeclared_local */
{
#line 195 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2212 "bcplus/parser/detail/lemon_parser.c"
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
#line 199 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy334));								
#line 2225 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 136: /* stmt_macro_def */
{
#line 220 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy373));								
#line 2232 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 137: /* stmt_constant_def */
{
#line 222 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy353));								
#line 2239 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 138: /* stmt_object_def */
{
#line 224 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy172));								
#line 2246 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 139: /* stmt_variable_def */
{
#line 226 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy43));								
#line 2253 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_sort_def */
{
#line 228 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy339));								
#line 2260 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 145: /* stmt_noconcurrency */
{
#line 238 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy451));								
#line 2267 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 146: /* stmt_strong_noconcurrency */
{
#line 240 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy352));								
#line 2274 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_query */
{
#line 246 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy134));								
#line 2281 "bcplus/parser/detail/lemon_parser.c"
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
#line 280 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy113));								
#line 2297 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 152: /* constant */
    case 159: /* constant_one_const */
    case 164: /* constant_local */
{
#line 284 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy357));								
#line 2306 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* object */
    case 165: /* object_local */
{
#line 286 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy398));								
#line 2314 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 154: /* variable */
{
#line 288 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy313));								
#line 2321 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 155: /* lua */
    case 167: /* lua_local */
{
#line 290 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy105));								
#line 2329 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 157: /* term_lst */
    case 160: /* term_no_const_lst */
    case 169: /* term_local_lst */
{
#line 294 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy67));								
#line 2338 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 166: /* variable_local */
{
#line 427 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy328));								
#line 2345 "bcplus/parser/detail/lemon_parser.c"
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
#line 704 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy374));								
#line 2360 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 177: /* atomic_formula */
    case 183: /* atomic_formula_one_const */
    case 187: /* card_af */
{
#line 710 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy6));								
#line 2369 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 178: /* formula_quant */
{
#line 712 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy155));								
#line 2376 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_lst */
{
#line 908 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy358));								
#line 2383 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* quant_op */
{
#line 910 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2390 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 188: /* card_sort_bnd_lst */
{
#line 949 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy430));								
#line 2397 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 189: /* sort */
    case 196: /* sort_id_nr */
    case 197: /* sort_id */
{
#line 1154 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2406 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 191: /* macro_def_lst */
{
#line 1056 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy381));                              
#line 2413 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 192: /* macro_bnd */
{
#line 1058 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy83));                              
#line 2420 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 193: /* macro_args */
{
#line 1060 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy198));                              
#line 2427 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 194: /* macro_arg */
{
#line 1062 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy340));                              
#line 2434 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 195: /* sort_lst */
{
#line 1152 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy151));							
#line 2441 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 198: /* constant_bnd_lst */
    case 199: /* constant_bnd */
{
#line 1271 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy77));									
#line 2449 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* constant_dcl_lst */
{
#line 1275 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy316));									
#line 2456 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* constant_dcl_type */
{
#line 1277 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2463 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* attrib_spec */
{
#line 1279 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2470 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* object_bnd_lst */
{
#line 1614 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy84));									
#line 2477 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* object_bnd */
{
#line 1616 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy180));									
#line 2484 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* object_lst */
    case 206: /* object_spec */
{
#line 1618 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy299));									
#line 2492 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 207: /* variable_bnd_lst */
    case 208: /* variable_bnd */
{
#line 1736 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy326));									
#line 2500 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 209: /* variable_lst */
{
#line 1740 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy58));									
#line 2507 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* sort_bnd_lst */
    case 211: /* sort_bnd */
    case 212: /* sort_dcl_lst */
{
#line 1813 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy233));									
#line 2516 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* show_lst */
{
#line 1917 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy345));									
#line 2523 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* show_elem */
    case 222: /* clause_unless */
{
#line 1919 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy6));									
#line 2531 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* query_lst */
{
#line 2071 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy165).l); DEALLOC((yypminor->yy165).maxstep); DEALLOC((yypminor->yy165).label);	
#line 2538 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* query_maxstep_decl */
{
#line 2073 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy340));												
#line 2545 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 218: /* query_label_Decl */
{
#line 2075 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2552 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* clause_if */
    case 220: /* clause_after */
    case 221: /* clause_ifcons */
    case 223: /* clause_where */
{
#line 2213 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy374));									
#line 2562 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 224: /* law_basic */
    case 225: /* law_caused */
    case 226: /* law_pcaused */
    case 227: /* law_impl */
    case 228: /* law_causes */
    case 229: /* law_increments */
    case 230: /* law_mcause */
    case 231: /* law_always */
    case 232: /* law_constraint */
    case 233: /* law_impossible */
    case 234: /* law_never */
    case 235: /* law_default */
    case 236: /* law_exogenous */
    case 237: /* law_inertial */
    case 238: /* law_nonexecutable */
    case 239: /* law_rigid */
    case 240: /* law_observed */
{
#line 2254 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy334));									
#line 2585 "bcplus/parser/detail/lemon_parser.c"
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
  { 199, 3 },
  { 199, 5 },
  { 199, 8 },
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
  { 201, 1 },
  { 202, 1 },
  { 202, 4 },
  { 138, 4 },
  { 203, 1 },
  { 203, 3 },
  { 204, 3 },
  { 205, 1 },
  { 205, 3 },
  { 206, 1 },
  { 206, 4 },
  { 206, 1 },
  { 139, 4 },
  { 207, 1 },
  { 207, 3 },
  { 208, 3 },
  { 209, 1 },
  { 209, 3 },
  { 140, 4 },
  { 210, 1 },
  { 210, 3 },
  { 211, 1 },
  { 211, 3 },
  { 211, 3 },
  { 211, 3 },
  { 212, 1 },
  { 212, 3 },
  { 143, 4 },
  { 143, 4 },
  { 144, 4 },
  { 144, 4 },
  { 213, 1 },
  { 213, 3 },
  { 214, 1 },
  { 145, 1 },
  { 146, 1 },
  { 147, 5 },
  { 148, 5 },
  { 149, 4 },
  { 215, 1 },
  { 215, 1 },
  { 215, 1 },
  { 215, 3 },
  { 215, 3 },
  { 215, 3 },
  { 216, 3 },
  { 216, 3 },
  { 217, 3 },
  { 217, 3 },
  { 219, 2 },
  { 219, 0 },
  { 220, 2 },
  { 220, 0 },
  { 221, 2 },
  { 221, 0 },
  { 222, 2 },
  { 222, 0 },
  { 223, 2 },
  { 223, 0 },
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
  { 224, 7 },
  { 225, 8 },
  { 226, 8 },
  { 227, 5 },
  { 228, 7 },
  { 229, 9 },
  { 230, 7 },
  { 231, 6 },
  { 232, 6 },
  { 233, 6 },
  { 234, 6 },
  { 235, 8 },
  { 236, 8 },
  { 237, 8 },
  { 238, 6 },
  { 239, 4 },
  { 240, 5 },
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
#line 201 "bcplus/parser/detail/lemon_parser.y"
{
  yy_destructor(yypParser,114,&yymsp[0].minor);
}
#line 3259 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 206 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy334;
			yymsp[0].minor.yy334  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3268 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 249 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy373; }
#line 3273 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 250 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy353; }
#line 3278 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 251 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy172; }
#line 3283 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 252 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy43; }
#line 3288 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 253 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy339; }
#line 3293 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 254 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy334; }
#line 3303 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 258 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy451; }
#line 3308 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 259 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy352; }
#line 3313 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 262 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy134; }
#line 3318 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
      case 36: /* base_elem_local ::= constant_local */ yytestcase(yyruleno==36);
#line 304 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy357; }
#line 3324 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 37: /* base_elem_local ::= base_elem_local_no_const */ yytestcase(yyruleno==37);
      case 52: /* term ::= base_elem */ yytestcase(yyruleno==52);
      case 68: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==68);
      case 93: /* term_local ::= base_elem_local */ yytestcase(yyruleno==93);
      case 109: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==109);
      case 124: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==124);
#line 305 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy113; }
#line 3335 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
      case 38: /* base_elem_local_no_const ::= object_local */ yytestcase(yyruleno==38);
#line 307 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy398;	}
#line 3341 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
      case 39: /* base_elem_local_no_const ::= variable */ yytestcase(yyruleno==39);
#line 308 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy313; }
#line 3347 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
      case 41: /* base_elem_local_no_const ::= lua_local */ yytestcase(yyruleno==41);
#line 309 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy105; }
#line 3353 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 32: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==32);
      case 42: /* constant_local ::= CONSTANT_ID PAREN_L term_local_lst PAREN_R */ yytestcase(yyruleno==42);
#line 363 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy357, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3360 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 33: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==33);
      case 43: /* constant_local ::= CONSTANT_ID */ yytestcase(yyruleno==43);
#line 364 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy357, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3367 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
      case 44: /* object_local ::= OBJECT_ID PAREN_L term_local_lst PAREN_R */ yytestcase(yyruleno==44);
#line 365 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy398, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3373 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* object ::= OBJECT_ID */
      case 45: /* object_local ::= OBJECT_ID */ yytestcase(yyruleno==45);
#line 366 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy398, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3379 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* variable ::= VARIABLE_ID */
#line 368 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy313 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::VARIABLE)) {
			parser->_feature_error(Language::Feature::VARIABLE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			BASE_ELEM_BARE_DEF(yygotominor.yy313, yymsp[0].minor.yy0, Symbol::Type::VARIABLE, Variable, VariableSymbol); 
		}
	}
#line 3394 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
      case 48: /* lua_local ::= AT_IDENTIFIER PAREN_L term_local_lst PAREN_R */ yytestcase(yyruleno==48);
#line 379 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy105, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0); }
#line 3400 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* lua ::= AT_IDENTIFIER */
      case 49: /* lua_local ::= AT_IDENTIFIER */ yytestcase(yyruleno==49);
#line 380 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy105, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3406 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* term_lst ::= term */
      case 34: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==34);
      case 50: /* term_local_lst ::= term_local */ yytestcase(yyruleno==50);
#line 385 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy67 = new TermList();
			yygotominor.yy67->push_back(yymsp[0].minor.yy113);
		}
#line 3416 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* term_lst ::= term_lst COMMA term */
      case 35: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==35);
      case 51: /* term_local_lst ::= term_local_lst COMMA term_local */ yytestcase(yyruleno==51);
#line 391 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy67 = yymsp[-2].minor.yy67;
			yymsp[-2].minor.yy67->push_back(yymsp[0].minor.yy113);
		  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 3427 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 40: /* base_elem_local_no_const ::= variable_local */
#line 441 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy328; }
#line 3432 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 46: /* variable_local ::= POUND_IDENTIFIER */
      case 47: /* variable_local ::= POUND_INTEGER */ yytestcase(yyruleno==47);
#line 450 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy328 = new LocalVariable(yymsp[0].minor.yy0->str(), yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); delete yymsp[0].minor.yy0; }
#line 3438 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= INTEGER */
      case 69: /* term_strong ::= INTEGER */ yytestcase(yyruleno==69);
      case 94: /* term_local ::= INTEGER */ yytestcase(yyruleno==94);
      case 110: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==110);
      case 125: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==125);
#line 547 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy113, yymsp[0].minor.yy0);	}
#line 3447 "bcplus/parser/detail/lemon_parser.c"
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
#line 548 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy113, yymsp[0].minor.yy0); }
#line 3462 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= PAREN_L term PAREN_R */
      case 71: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==71);
      case 96: /* term_local ::= PAREN_L term_local PAREN_R */ yytestcase(yyruleno==96);
      case 112: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==112);
      case 127: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==127);
#line 549 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy113, yymsp[-2].minor.yy0, yymsp[-1].minor.yy113, yymsp[0].minor.yy0); }
#line 3471 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 58: /* term ::= MAXSTEP */
      case 72: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==72);
      case 99: /* term_local ::= MAXSTEP */ yytestcase(yyruleno==99);
      case 113: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==113);
#line 552 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3479 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 59: /* term ::= MAXADDITIVE */
      case 73: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==73);
      case 100: /* term_local ::= MAXADDITIVE */ yytestcase(yyruleno==100);
      case 114: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==114);
#line 553 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3487 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 60: /* term ::= MAXAFVALUE */
      case 74: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==74);
      case 101: /* term_local ::= MAXAFVALUE */ yytestcase(yyruleno==101);
      case 115: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==115);
#line 554 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3495 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 61: /* term ::= DASH term */
      case 75: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==75);
      case 102: /* term_local ::= DASH term_local */ yytestcase(yyruleno==102);
      case 117: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==117);
      case 131: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==131);
#line 558 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, UnaryTerm::Operator::NEGATIVE); }
#line 3504 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 62: /* term ::= ABS term */
      case 76: /* term_strong ::= ABS term */ yytestcase(yyruleno==76);
      case 103: /* term_local ::= ABS term_local */ yytestcase(yyruleno==103);
      case 118: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==118);
      case 132: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==132);
#line 559 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, UnaryTerm::Operator::ABS); }
#line 3513 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 63: /* term ::= term DASH term */
      case 78: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==78);
      case 88: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==88);
      case 104: /* term_local ::= term_local DASH term_local */ yytestcase(yyruleno==104);
      case 119: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==119);
      case 133: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==133);
#line 563 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MINUS); }
#line 3523 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 64: /* term ::= term PLUS term */
      case 79: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==79);
      case 89: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==89);
      case 105: /* term_local ::= term_local PLUS term_local */ yytestcase(yyruleno==105);
      case 120: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==120);
      case 134: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==134);
#line 564 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::PLUS); }
#line 3533 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 65: /* term ::= term STAR term */
      case 80: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==80);
      case 90: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==90);
      case 106: /* term_local ::= term_local STAR term_local */ yytestcase(yyruleno==106);
      case 121: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==121);
      case 135: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==135);
#line 565 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::TIMES); }
#line 3543 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 66: /* term ::= term INT_DIV term */
      case 81: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==81);
      case 91: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==91);
      case 107: /* term_local ::= term_local INT_DIV term_local */ yytestcase(yyruleno==107);
      case 122: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==122);
      case 136: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==136);
#line 566 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::DIVIDE); }
#line 3553 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 67: /* term ::= term MOD term */
      case 82: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==82);
      case 92: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==92);
      case 108: /* term_local ::= term_local MOD term_local */ yytestcase(yyruleno==108);
      case 123: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==123);
      case 137: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==137);
#line 567 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MOD); }
#line 3563 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong_candidate ::= DASH constant */
#line 586 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy357, UnaryTerm::Operator::NEGATIVE); }
#line 3568 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 83: /* term_strong ::= constant DASH term */
#line 595 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MINUS); }
#line 3573 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 84: /* term_strong ::= constant PLUS term */
#line 596 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::PLUS); }
#line 3578 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 85: /* term_strong ::= constant STAR term */
#line 597 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::TIMES); }
#line 3583 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 86: /* term_strong ::= constant INT_DIV term */
#line 598 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::DIVIDE); }
#line 3588 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 87: /* term_strong ::= constant MOD term */
#line 599 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MOD); }
#line 3593 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 116: /* term_no_const_strong ::= constant */
      case 130: /* term_no_const ::= constant */ yytestcase(yyruleno==130);
#line 645 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy113 default to undeclared identifiers
		yygotominor.yy113 = NULL;
		ref_ptr<Referenced> c_ptr = yymsp[0].minor.yy357;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy357->beginLoc());
		YYERROR;
	}
#line 3605 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* formula ::= formula_base */
      case 177: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==177);
      case 200: /* formula_temporal ::= formula_base */ yytestcase(yyruleno==200);
#line 750 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy374;				}
#line 3612 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* formula ::= PAREN_L formula PAREN_R */
      case 178: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==178);
      case 201: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==201);
#line 751 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[-1].minor.yy374; yygotominor.yy374->parens(true); 	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3621 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* formula ::= NOT formula */
      case 179: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==179);
      case 202: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==202);
#line 752 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 3628 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* formula ::= DASH formula */
      case 180: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==180);
      case 203: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==203);
#line 753 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 3635 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* formula ::= formula AMP formula */
      case 181: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==181);
      case 204: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==204);
#line 754 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy374, yymsp[0].minor.yy374, yymsp[-2].minor.yy374->beginLoc(), yymsp[0].minor.yy374->endLoc());   yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3643 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* formula ::= formula DBL_PLUS formula */
      case 144: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==144);
      case 182: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==182);
      case 183: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==183);
      case 205: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==205);
      case 206: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==206);
#line 755 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::OR); }
#line 3653 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 145: /* formula ::= formula EQUIV formula */
      case 184: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==184);
      case 207: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==207);
#line 757 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::EQUIV); }
#line 3660 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 146: /* formula ::= formula IMPL formula */
      case 147: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==147);
      case 185: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==185);
      case 186: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==186);
      case 208: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==208);
      case 209: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==209);
#line 758 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::IMPL); }
#line 3670 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 148: /* formula_base ::= comparison */
      case 187: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==187);
      case 225: /* head_formula ::= comparison */ yytestcase(yyruleno==225);
#line 761 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy374; }
#line 3677 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 149: /* formula_base ::= atomic_formula */
      case 226: /* head_formula ::= atomic_formula */ yytestcase(yyruleno==226);
#line 762 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy6; }
#line 3683 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 150: /* formula_base ::= formula_quant */
#line 763 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy155; }
#line 3688 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 151: /* formula_base ::= formula_card */
#line 765 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy374 = yymsp[0].minor.yy374;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy374->beginLoc());
			YYERROR;
		}
	}
#line 3699 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* formula_base ::= TRUE */
      case 188: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==188);
      case 228: /* head_formula ::= TRUE */ yytestcase(yyruleno==228);
#line 772 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3706 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* formula_base ::= FALSE */
      case 189: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==189);
      case 229: /* head_formula ::= FALSE */ yytestcase(yyruleno==229);
#line 773 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3713 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* comparison ::= term_strong EQ term */
      case 161: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==161);
      case 190: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==190);
#line 775 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3721 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* comparison ::= term_strong DBL_EQ term */
      case 162: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==162);
      case 191: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==191);
#line 776 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3729 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* comparison ::= term_strong NEQ term */
      case 163: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==163);
      case 192: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==192);
#line 777 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3737 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= term_strong LTHAN term */
      case 164: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==164);
      case 193: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==193);
#line 778 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3745 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* comparison ::= term_strong GTHAN term */
      case 165: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==165);
      case 194: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==194);
#line 779 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3753 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* comparison ::= term_strong LTHAN_EQ term */
      case 166: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==166);
      case 195: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==195);
#line 780 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3761 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* comparison ::= term_strong GTHAN_EQ term */
      case 167: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==167);
      case 196: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==196);
#line 781 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 3769 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 168: /* comparison ::= constant DBL_EQ term */
#line 789 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3775 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 169: /* comparison ::= constant NEQ term */
#line 790 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3781 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 170: /* comparison ::= constant LTHAN term */
#line 791 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3787 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 171: /* comparison ::= constant GTHAN term */
#line 792 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3793 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 172: /* comparison ::= constant LTHAN_EQ term */
#line 793 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3799 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 173: /* comparison ::= constant GTHAN_EQ term */
#line 794 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 3805 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 174: /* atomic_formula ::= constant */
      case 197: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==197);
      case 220: /* card_af ::= constant_local */ yytestcase(yyruleno==220);
#line 824 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy6, yymsp[0].minor.yy357, "true"); }
#line 3812 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 175: /* atomic_formula ::= TILDE constant */
      case 198: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==198);
      case 221: /* card_af ::= TILDE constant_local */ yytestcase(yyruleno==221);
#line 825 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy6, yymsp[0].minor.yy357, "false");   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 3820 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 176: /* atomic_formula ::= constant EQ term */
      case 199: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==199);
#line 826 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = new AtomicFormula(yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());	  yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3827 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* formula_temporal ::= term_strong COLON formula_temporal */
#line 901 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy374, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy374); }
#line 3832 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
#line 913 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy155=NULL;
		ref_ptr<const Token> bl_ptr = yymsp[-4].minor.yy0;
		ref_ptr<QuantifierFormula::QuantifierList> lst_ptr = yymsp[-3].minor.yy358;
		ref_ptr<Formula> sub_ptr = yymsp[-1].minor.yy374;
		ref_ptr<const Token> br_ptr = yymsp[0].minor.yy0;

		if (!parser->lang()->support(Language::Feature::FORMULA_QUANTIFIER)) {
			parser->_feature_error(Language::Feature::FORMULA_QUANTIFIER, &yymsp[-4].minor.yy0->beginLoc());
			YYERROR;
		} else yygotominor.yy155 = new QuantifierFormula(yymsp[-3].minor.yy358, yymsp[-1].minor.yy374, yymsp[-4].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
	  yy_destructor(yypParser,99,&yymsp[-2].minor);
}
#line 3849 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* quant_lst ::= quant_op variable */
#line 927 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy358 = new QuantifierFormula::QuantifierList();
		yygotominor.yy358->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy423, yymsp[0].minor.yy313));
	}
#line 3857 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* quant_lst ::= quant_lst quant_op variable */
#line 933 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy358 = yymsp[-2].minor.yy358;
		yygotominor.yy358->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy423, yymsp[0].minor.yy313));
	}
#line 3865 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* quant_op ::= BIG_CONJ */
#line 938 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 3871 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 215: /* quant_op ::= BIG_DISJ */
#line 939 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,92,&yymsp[0].minor);
}
#line 3877 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* formula_card ::= CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R */
#line 990 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy6, yymsp[-1].minor.yy430, yymsp[0].minor.yy0, NULL); }
#line 3882 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* formula_card ::= term_strong CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R */
#line 991 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, yymsp[-4].minor.yy113, yymsp[-3].minor.yy0, yymsp[-2].minor.yy6, yymsp[-1].minor.yy430, yymsp[0].minor.yy0, NULL);  }
#line 3887 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 218: /* formula_card ::= CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R term_strong */
#line 992 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy6, yymsp[-2].minor.yy430, yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 3892 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 219: /* formula_card ::= term_strong CBRACKET_L card_af card_sort_bnd_lst CBRACKET_R term_strong */
#line 993 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, yymsp[-5].minor.yy113, yymsp[-4].minor.yy0, yymsp[-3].minor.yy6, yymsp[-2].minor.yy430, yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 3897 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* card_af ::= constant_local EQ term_local */
#line 1001 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = new AtomicFormula(yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3903 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 223: /* card_sort_bnd_lst ::= */
#line 1004 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy430 = new CardinalityFormula::BindingList();	}
#line 3908 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 224: /* card_sort_bnd_lst ::= card_sort_bnd_lst COLON sort PAREN_L variable_local PAREN_R */
#line 1006 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy430 = yymsp[-5].minor.yy430;
		yygotominor.yy430->push_back(CardinalityFormula::Binding(yymsp[-3].minor.yy325, yymsp[-1].minor.yy328));
	  yy_destructor(yypParser,81,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3919 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 227: /* head_formula ::= formula_card */
#line 1024 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy374 = yymsp[0].minor.yy374;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy374->beginLoc());
			YYERROR;
		}
	}
#line 3930 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 230: /* head_formula ::= DASH constant */
#line 1034 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy374 = NULL;
		ref_ptr<const Token> d_ptr = yymsp[-1].minor.yy0;
		ref_ptr<Constant> c_ptr = yymsp[0].minor.yy357;

		if (!parser->lang()->support(Language::Feature::FORMULA_NOT_DASH_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_NOT_DASH_HEAD);
			YYERROR;
		} else {
			ATOMIC_FORMULA(yygotominor.yy374, yymsp[0].minor.yy357, "false"); 
		}
	}
#line 3946 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 231: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1066 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy373 = NULL;
        ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
        ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
        ref_ptr<MacroDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy381;
        ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;

        if (!parser->lang()->support(Language::Feature::DECL_MACRO)) {
            parser->_feature_error(Language::Feature::DECL_MACRO, &yymsp[-2].minor.yy0->beginLoc());
            YYERROR;
        } else {
		    BOOST_FOREACH(symbols::MacroSymbol* m, *yymsp[-1].minor.yy381) {
			    if (!parser->symtab()->create(m)) {
	    	        // Check if it's a duplicate
	    	        symbols::MacroSymbol* m2 = (symbols::MacroSymbol*)parser->symtab()->resolve(symbols::Symbol::Type::MACRO, *m->base(), m->arity());
		            if (!m2 || m2 != m) {
		                parser->_parse_error("Detected conflicting definition of symbol \"" + *m->name() + "\".", &yygotominor.yy373->beginLoc());
		            } else {
		                parser->_parse_error("Detected a duplicate definition of symbol \"" + *m->name() + "\".", &yygotominor.yy373->beginLoc());
		            }
		        }
		    }

			yygotominor.yy373 = new MacroDeclaration(yymsp[-1].minor.yy381, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
        }
    }
#line 3976 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* macro_def_lst ::= macro_bnd */
#line 1094 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy381 = new MacroDeclaration::ElementList();
        yygotominor.yy381->push_back(yymsp[0].minor.yy83);
    }
#line 3984 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 233: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1100 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy381 = yymsp[-2].minor.yy381;
        yygotominor.yy381->push_back(yymsp[0].minor.yy83);
      yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 3993 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 234: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1106 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy198;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy83 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy198);
      yy_destructor(yypParser,72,&yymsp[-4].minor);
  yy_destructor(yypParser,73,&yymsp[-2].minor);
  yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4007 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1115 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy83 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4018 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 236: /* macro_args ::= macro_arg */
#line 1123 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy198 = new MacroSymbol::ArgumentList();
        yygotominor.yy198->push_back(yymsp[0].minor.yy340->str());
        delete yymsp[0].minor.yy340;
    }
#line 4027 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 237: /* macro_args ::= macro_args COMMA macro_arg */
#line 1129 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy198 = yymsp[-2].minor.yy198;
        yygotominor.yy198->push_back(yymsp[0].minor.yy340->str());
        delete yymsp[0].minor.yy340;
      yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4037 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 238: /* macro_arg ::= POUND_INTEGER */
      case 239: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==239);
#line 1136 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy340 = yymsp[0].minor.yy0;
    }
#line 4045 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 240: /* sort_lst ::= sort */
#line 1161 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy151 = new ConstantSymbol::SortList();
		yygotominor.yy151->push_back(yymsp[0].minor.yy325);
	}
#line 4053 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 241: /* sort_lst ::= sort_lst COMMA sort */
#line 1166 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy151 = yymsp[-2].minor.yy151;
		yygotominor.yy151->push_back(yymsp[0].minor.yy325);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4062 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 242: /* sort ::= sort_id_nr STAR */
#line 1202 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-1].minor.yy325, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, *yymsp[-1].minor.yy325->base() + "_star", "none"); }
#line 4067 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 243: /* sort ::= sort_id_nr CARROT */
#line 1203 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-1].minor.yy325, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, *yymsp[-1].minor.yy325->base() + "_carrot", "unknown"); }
#line 4072 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 244: /* sort ::= sort_id_nr */
      case 245: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==245);
#line 1204 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy325 = yymsp[0].minor.yy325; }
#line 4078 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 246: /* sort_id_nr ::= NUMBER_RANGE */
#line 1208 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy325 = NULL;
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
				yygotominor.yy325 = NULL;
				parser->_parse_error("An error occurred creating symbol \"" + obj_name + "/0\".", &yymsp[0].minor.yy0->beginLoc());
				YYERROR;
			}
			objs->insert(sym);
		}		

		// dynamically declare the sort
		yygotominor.yy325 = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name), objs));
		if (!yygotominor.yy325) {
				parser->_parse_error("An error occurred creating symbol \"" + name + "/0\".", &yymsp[0].minor.yy0->beginLoc());
				YYERROR;
		} 
	}
#line 4123 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 247: /* sort_id ::= IDENTIFIER */
#line 1251 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy325 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy325) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4136 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 248: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1282 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ConstantDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy77;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_CONSTANT)) {
			yygotominor.yy353 = NULL;
			parser->_feature_error(Language::Feature::DECL_CONSTANT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy353 = new ConstantDeclaration(yymsp[-1].minor.yy77, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

		}
	}
#line 4155 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 249: /* constant_bnd_lst ::= constant_bnd */
#line 1299 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy77 = yymsp[0].minor.yy77;
	}
#line 4162 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 250: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1304 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy77;
		yygotominor.yy77 = yymsp[-2].minor.yy77;
		yygotominor.yy77->splice(yygotominor.yy77->end(), *yymsp[0].minor.yy77);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4172 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 251: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1324 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-5].minor.yy316, s_ptr = yymsp[-1].minor.yy325;
		yygotominor.yy77 = new ConstantDeclaration::ElementList();


		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-5].minor.yy316) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[-3].minor.yy341, decl.first->str(), yymsp[-1].minor.yy325, decl.second);
			yygotominor.yy77->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,77,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4191 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 252: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1337 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy316, s_ptr = yymsp[0].minor.yy325;
		yygotominor.yy77 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy316) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(ConstantSymbol::Type::RIGID, decl.first->str(), yymsp[0].minor.yy325, decl.second);
			yygotominor.yy77->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4206 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 253: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1348 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy316;
		yygotominor.yy77 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy316) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[0].minor.yy341, decl.first->str(), parser->symtab()->boolsort(), decl.second);
			yygotominor.yy77->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4221 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 254: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1359 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy77 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-4].minor.yy316, s_ptr = yymsp[-2].minor.yy310, id_ptr = yymsp[0].minor.yy0;


		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[0].minor.yy0->str());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\" is not a valid constant symbol.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy77 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-4].minor.yy316) {
				ref_ptr<ConstantSymbol> c= new AttributeSymbol(decl.first->str(), yymsp[-2].minor.yy310, c, decl.second);
				yygotominor.yy77->push_back(c);
				CONSTANT_DECL(c, decl.first->beginLoc());
			}
		}
	  yy_destructor(yypParser,77,&yymsp[-3].minor);
  yy_destructor(yypParser,55,&yymsp[-1].minor);
}
#line 4250 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 255: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1383 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy77 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-7].minor.yy316, s_ptr = yymsp[-5].minor.yy310, id_ptr = yymsp[-3].minor.yy0, lst_ptr = yymsp[-1].minor.yy151;

		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy151->size());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy151->size()) + "\" is not a valid constant symbol.", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy151->size()) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// ensure that the sorts match the declaration of the symbol
			SortList::const_iterator it = yymsp[-1].minor.yy151->begin();
			BOOST_FOREACH(SortSymbol const* sort, *c) {
				if (*it != sort) {
					// check to see if it'yymsp[-5].minor.yy310 a subsort, which is also permissable
					bool found = false;
					for (SortSymbol::SortList::const_iterator it2 = sort->beginSubSorts(); it2 != sort->endSubSorts(); it2++) {
						if (*it == *it2) {
							found = true;
							break;
						}
					}

					if (!found) {
						parser->_parse_error("Detected a sort mismatch in an attribute parent declaration. \"" + *(*it)->base() + "\" is not an explicit subsort of \"" + *sort->base() + "\".", &yymsp[-3].minor.yy0->beginLoc());
						YYERROR;
					}
				}
				it++;
			}

			yygotominor.yy77 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-7].minor.yy316) {
				// ensure that the sorts match the start of the sort list for each of the symbols
				if (decl.second->size() < yymsp[-1].minor.yy151->size()) {
					parser->_parse_error("Detected a malformed attribute declaration. An attribute must duplicate its parent'yymsp[-5].minor.yy310 parameters.", &decl.first->beginLoc());
					YYERROR;
				} else {
					bool good_sort = true;		
					it = decl.second->begin();
					BOOST_FOREACH(SortSymbol const* sort, *yymsp[-1].minor.yy151) {
						if (*it != sort) {
							// check to see if it'yymsp[-5].minor.yy310 a subsort, which is also permissable
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
						ref_ptr<ConstantSymbol> sym = new AttributeSymbol(decl.first->str(), yymsp[-5].minor.yy310, c, decl.second);
						yygotominor.yy77->push_back(sym);
						CONSTANT_DECL(sym, decl.first->beginLoc());

					}
				}
			}
		}
	  yy_destructor(yypParser,77,&yymsp[-6].minor);
  yy_destructor(yypParser,55,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4331 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 256: /* constant_dcl_lst ::= IDENTIFIER */
#line 1459 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = new IdentifierDeclList();
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 4339 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 257: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1464 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = new IdentifierDeclList();
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy151));
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4349 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 258: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1469 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = yymsp[-2].minor.yy316;
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4358 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 259: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1474 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = yymsp[-5].minor.yy316;
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy151));
	  yy_destructor(yypParser,103,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4369 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 260: /* constant_dcl_type ::= ABACTION */
#line 1481 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4381 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 261: /* constant_dcl_type ::= ACTION */
#line 1490 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4393 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 262: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1499 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4405 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 263: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1508 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4417 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 264: /* constant_dcl_type ::= EXTERNALACTION */
#line 1517 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4429 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1526 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4441 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1535 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4453 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 267: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1544 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4465 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* constant_dcl_type ::= RIGID */
#line 1553 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4477 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 269: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1562 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4489 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 270: /* constant_dcl_type ::= SDFLUENT */
#line 1572 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4501 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 271: /* attrib_spec ::= ATTRIBUTE */
#line 1582 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy310 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[0].minor.yy0;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// grab the boolean sort and provide it
			yygotominor.yy310 = parser->symtab()->boolsort();
		}
	}
#line 4516 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 272: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1595 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy310 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[-3].minor.yy0, s_ptr = yymsp[-1].minor.yy325;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy310 = yymsp[-1].minor.yy325;
		}
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4532 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 273: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1623 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ObjectDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy84;
		
		if (!parser->lang()->support(Language::Feature::DECL_OBJECT)) {
			yygotominor.yy172 = NULL;
			parser->_feature_error(Language::Feature::DECL_OBJECT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy172 = new ObjectDeclaration(yymsp[-1].minor.yy84, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ObjectDeclaration::Element* bnd, *yymsp[-1].minor.yy84) {
				BOOST_FOREACH(ObjectSymbol const* o, *bnd) {
						bnd->sort()->add(o);
				}
			}
		}
	}
#line 4557 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 274: /* object_bnd_lst ::= object_bnd */
#line 1646 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy84 = new ObjectDeclaration::ElementList();
		yygotominor.yy84->push_back(yymsp[0].minor.yy180);
	}
#line 4565 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 275: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 1652 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy84 = yymsp[-2].minor.yy84;
		yygotominor.yy84->push_back(yymsp[0].minor.yy180);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4574 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 276: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 1658 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy180 = new ObjectDeclaration::Element(yymsp[0].minor.yy325, yymsp[-2].minor.yy299);
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4582 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 277: /* object_lst ::= object_spec */
#line 1663 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = yymsp[0].minor.yy299;
	}
#line 4589 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 278: /* object_lst ::= object_lst COMMA object_spec */
#line 1667 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = yymsp[-2].minor.yy299;
		yygotominor.yy299->splice(yygotominor.yy299->end(), *yymsp[0].minor.yy299);
		delete yymsp[0].minor.yy299;
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4599 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 279: /* object_spec ::= IDENTIFIER */
#line 1676 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		yygotominor.yy299 = NULL;
		ref_ptr<const ObjectSymbol> o = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy299 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy299->push_back(o);
		}
	}
#line 4615 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 280: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1689 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = NULL;
		ref_ptr<ObjectSymbol::SortList> lst_ptr = yymsp[-1].minor.yy151;
		ref_ptr<const Token> id_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const ObjectSymbol> o = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy151));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(),yymsp[-1].minor.yy151->size()) + "\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy299 = new  ObjectDeclaration::Element::ObjectList();
			yygotominor.yy299->push_back(o);
		}
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4634 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 281: /* object_spec ::= NUMBER_RANGE */
#line 1703 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = new ObjectDeclaration::Element::ObjectList();
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
				yygotominor.yy299->push_back(o);
			}
		}
	}
#line 4662 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 282: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 1743 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<VariableDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy326;
		
		if (!parser->lang()->support(Language::Feature::DECL_VARIABLE)) {
			yygotominor.yy43 = NULL;
			parser->_feature_error(Language::Feature::DECL_VARIABLE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy43 = new VariableDeclaration(yymsp[-1].minor.yy326, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(VariableSymbol* v, *yymsp[-1].minor.yy326) {
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
#line 4693 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 283: /* variable_bnd_lst ::= variable_bnd */
#line 1772 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = yymsp[0].minor.yy326;
	}
#line 4700 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 284: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 1777 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = yymsp[-2].minor.yy326;
		yygotominor.yy326->splice(yygotominor.yy326->end(), *yymsp[0].minor.yy326);
		delete yymsp[0].minor.yy326;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4710 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 285: /* variable_bnd ::= variable_lst DBL_COLON sort_id */
#line 1784 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy58) {
			yygotominor.yy326->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy325));
		}
		delete yymsp[-2].minor.yy58;
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4723 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 286: /* variable_lst ::= IDENTIFIER */
#line 1794 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy58 = new TokenList();
		yygotominor.yy58->push_back(yymsp[0].minor.yy0);
	}
#line 4731 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 1799 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy58 = yymsp[-2].minor.yy58;
		yygotominor.yy58->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4740 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 1820 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<SortDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy233;
		
		if (!parser->lang()->support(Language::Feature::DECL_SORT)) {
			yygotominor.yy339 = NULL;
			parser->_feature_error(Language::Feature::DECL_SORT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy339 = new SortDeclaration(yymsp[-1].minor.yy233, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4758 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* sort_bnd_lst ::= sort_bnd */
      case 291: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==291);
#line 1836 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[0].minor.yy233;
	}
#line 4766 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 1841 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[-2].minor.yy233;
		yygotominor.yy233->splice(yygotominor.yy233->end(), *yymsp[0].minor.yy233);
		delete yymsp[0].minor.yy233;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4776 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 1853 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy233) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy233) {
				sym2->addSubSort(sym);
			}
		}
		yygotominor.yy233 = yymsp[-2].minor.yy233;
		yygotominor.yy233->splice(yymsp[-2].minor.yy233->end(), *yymsp[0].minor.yy233);
		delete yymsp[0].minor.yy233;

	  yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 4792 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 1865 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy233) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy233) {
				sym->addSubSort(sym2);
			}
		}
		yygotominor.yy233 = yymsp[-2].minor.yy233;
		yygotominor.yy233->splice(yymsp[-2].minor.yy233->end(), *yymsp[0].minor.yy233);
		delete yymsp[0].minor.yy233;
	  yy_destructor(yypParser,100,&yymsp[-1].minor);
}
#line 4807 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 294: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 1876 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[-1].minor.yy233;
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4816 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* sort_dcl_lst ::= IDENTIFIER */
#line 1881 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<SortSymbol> s = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yygotominor.yy233 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy233 = new SortDeclaration::ElementList();
			yygotominor.yy233->push_back(s);
		}

		delete yymsp[0].minor.yy0;
	}
#line 4833 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 1895 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[-2].minor.yy233;
		ref_ptr<SortSymbol> s = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yymsp[-2].minor.yy233 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yymsp[-2].minor.yy233->push_back(s);
		}

		delete yymsp[0].minor.yy0;

	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4852 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 1922 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy334 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<ShowStatement::ElementList> lst_ptr = yymsp[-1].minor.yy345;

		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy334 = new ShowStatement(yymsp[-1].minor.yy345, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4868 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 1936 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy334 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_SHOW_ALL)) {
			parser->_feature_error(Language::Feature::DECL_SHOW_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy334 = new ShowAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4886 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 1953 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy334 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<HideStatement::ElementList> lst_ptr = yymsp[-1].minor.yy345;

		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy334 = new HideStatement(yymsp[-1].minor.yy345, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4902 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 1967 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy334 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_HIDE_ALL)) {
			parser->_feature_error(Language::Feature::DECL_HIDE_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy334 = new HideAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 4920 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* show_lst ::= show_elem */
#line 1985 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy345 = new ShowStatement::ElementList();
		yygotominor.yy345->push_back(yymsp[0].minor.yy6);
	}
#line 4928 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* show_lst ::= show_lst COMMA show_elem */
#line 1990 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy345 = yymsp[-2].minor.yy345;
		yygotominor.yy345->push_back(yymsp[0].minor.yy6);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4937 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* show_elem ::= atomic_formula_one_const */
#line 1995 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = yymsp[0].minor.yy6; }
#line 4942 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* stmt_noconcurrency ::= NOCONCURRENCY */
#line 2018 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy451, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 4947 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY */
#line 2019 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy352, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 4952 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ INTEGER PERIOD */
#line 2052 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy334, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 4958 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ INTEGER PERIOD */
#line 2053 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy334, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 4964 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2078 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy134 = NULL;
		ref_ptr<const Referenced> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, data_l_ptr = yymsp[-1].minor.yy165.l, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Referenced> data_maxstep_ptr = yymsp[-1].minor.yy165.maxstep, data_label_ptr = yymsp[-1].minor.yy165.label;

		if (!parser->lang()->support(Language::Feature::DECL_QUERY)) {
			parser->_feature_error(Language::Feature::DECL_QUERY, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			bool good = true;

			// decode the maxstep if it exists
			int min = -1, max = -1;
			if (yymsp[-1].minor.yy165.maxstep) {
				if (yymsp[-1].minor.yy165.maxstep->type() == T_INTEGER) {
					if (sscanf(yymsp[-1].minor.yy165.maxstep->str()->c_str(), "%d", &max) != 1) {
						parser->_parse_error("INTERNAL ERROR: Could not extract integer from \"" + *yymsp[-1].minor.yy165.maxstep->str() + "\".", &yymsp[-1].minor.yy165.maxstep->beginLoc());
						YYERROR;
						good = false;
					} else if (max < 0) {
						parser->_parse_error("Query maximum step definitions cannot be negative.", &yymsp[-1].minor.yy165.maxstep->beginLoc());
						YYERROR;
						good = false;
					} else min = max;
				} else if (yymsp[-1].minor.yy165.maxstep->type() == T_NUMBER_RANGE) {
					if (sscanf(yymsp[-1].minor.yy165.maxstep->str()->c_str(), "%d..%d", &min, &max) != 2) {
						parser->_parse_error("INTERNAL ERROR: Could not extract number range from \"" + *yymsp[-1].minor.yy165.maxstep->str() + "\".", &yymsp[-1].minor.yy165.maxstep->beginLoc());
						YYERROR;
						good = false;
					} else if (min < 0 || max < 0) {
						parser->_parse_error("Query maximum step definitions cannot be negative.", &yymsp[-1].minor.yy165.maxstep->beginLoc());
						YYERROR;
						good = false;
					}
				}

			}


			// resolve the query label
			ref_ptr<QuerySymbol> sym = new QuerySymbol(yymsp[-1].minor.yy165.label->str(), min, max);
			if (!parser->symtab()->create(sym)) {
				parser->_parse_error("Could not create query, the label \"" + *yymsp[-1].minor.yy165.label->str() + "\" already exists.", &yymsp[-1].minor.yy165.label->beginLoc());
				good = false;
				YYERROR;
			}


			if (good) yygotominor.yy134 = new QueryStatement(sym, yymsp[-1].minor.yy165.l, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5019 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* query_lst ::= formula_temporal */
#line 2132 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = NULL;
		yygotominor.yy165.label = NULL;

		yygotominor.yy165.l->push_back(yymsp[0].minor.yy374);
	}
#line 5030 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* query_lst ::= query_maxstep_decl */
#line 2141 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = yymsp[0].minor.yy340;
		yygotominor.yy165.label = NULL;
	}
#line 5039 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* query_lst ::= query_label_decl */
#line 2148 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = NULL;
		yygotominor.yy165.label = yymsp[0].minor.yy340;
	}
#line 5048 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2155 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy165 = yymsp[-2].minor.yy165;
		yymsp[-2].minor.yy165.l->push_back(yymsp[0].minor.yy374);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5057 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2161 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165 = yymsp[-2].minor.yy165;

		if (yygotominor.yy165.maxstep) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy340->beginLoc());
			delete yymsp[0].minor.yy340;
			YYERROR;
		} else {
			yygotominor.yy165.maxstep = yymsp[0].minor.yy340;
		}
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5073 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2174 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165 = yymsp[-2].minor.yy165;
		if (yygotominor.yy165.label) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy340->beginLoc());
			delete yymsp[0].minor.yy340;
			YYERROR;

		} else {
			yygotominor.yy165.label = yymsp[0].minor.yy340;
		}
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5089 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
      case 316: /* query_maxstep_decl ::= MAXSTEP DBL_COLON NUMBER_RANGE */ yytestcase(yyruleno==316);
#line 2199 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy340, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_MAXSTEP);   yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5096 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 318: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==318);
#line 2201 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy340, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5103 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* clause_if ::= IF formula */
#line 2236 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_IF); 		}
#line 5108 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 320: /* clause_if ::= */
      case 322: /* clause_after ::= */ yytestcase(yyruleno==322);
      case 324: /* clause_ifcons ::= */ yytestcase(yyruleno==324);
      case 328: /* clause_where ::= */ yytestcase(yyruleno==328);
#line 2237 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = NULL; }
#line 5116 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* clause_after ::= AFTER formula */
#line 2238 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_AFTER);	}
#line 5121 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 323: /* clause_ifcons ::= IFCONS formula */
#line 2240 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_IFCONS); 	}
#line 5126 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 325: /* clause_unless ::= UNLESS atomic_formula */
#line 2242 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy6, yymsp[-1].minor.yy0, yymsp[0].minor.yy6, Language::Feature::CLAUSE_UNLESS); 	}
#line 5131 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 326: /* clause_unless ::= */
#line 2243 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = NULL; }
#line 5136 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 327: /* clause_where ::= WHERE formula_no_const */
#line 2244 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_WHERE); 	}
#line 5141 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* stmt_law ::= law_basic */
      case 330: /* stmt_law ::= law_caused */ yytestcase(yyruleno==330);
      case 331: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==331);
      case 332: /* stmt_law ::= law_impl */ yytestcase(yyruleno==332);
      case 333: /* stmt_law ::= law_causes */ yytestcase(yyruleno==333);
      case 334: /* stmt_law ::= law_increments */ yytestcase(yyruleno==334);
      case 335: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==335);
      case 336: /* stmt_law ::= law_always */ yytestcase(yyruleno==336);
      case 337: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==337);
      case 338: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==338);
      case 339: /* stmt_law ::= law_never */ yytestcase(yyruleno==339);
      case 340: /* stmt_law ::= law_default */ yytestcase(yyruleno==340);
      case 341: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==341);
      case 342: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==342);
      case 343: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==343);
      case 344: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==344);
      case 345: /* stmt_law ::= law_observed */ yytestcase(yyruleno==345);
#line 2288 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy334 = yymsp[0].minor.yy334;}
#line 5162 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2403 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, NULL, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
																																															Language::Feature::LAW_BASIC_D, BasicLaw); }
#line 5169 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2408 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 5176 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 348: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2412 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 5183 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2416 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy334, yymsp[-4].minor.yy374, yymsp[-3].minor.yy0, yymsp[-2].minor.yy374, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 5189 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* law_causes ::= formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2420 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy334, yymsp[-6].minor.yy374, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 5195 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* law_increments ::= formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2424 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy334, yymsp[-8].minor.yy374, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-4].minor.yy113, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5202 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* law_mcause ::= formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2428 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy334, yymsp[-6].minor.yy374, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 5208 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
      case 354: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */ yytestcase(yyruleno==354);
#line 2432 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 5216 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2440 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 5223 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2444 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 5230 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2448 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy6, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 5237 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2452 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 5244 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2456 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 5251 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2460 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 5257 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2464 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy334, yymsp[-3].minor.yy0, yymsp[-2].minor.yy357, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 5263 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD */
#line 2469 "bcplus/parser/detail/lemon_parser.y"
{ 
			yygotominor.yy334 = NULL;
			ref_ptr<const Token> kw_ptr = yymsp[-4].minor.yy0, p_ptr = yymsp[0].minor.yy0;
			ref_ptr<AtomicFormula> head_ptr = yymsp[-3].minor.yy6;
			ref_ptr<Term> t_ptr = yymsp[-1].minor.yy113;

			// make sure that the At clause is integral
			if (yymsp[-1].minor.yy113->domainType() != DomainType::INTEGRAL) {
				parser->_parse_error("Expected an integral expression.", &yymsp[-1].minor.yy113->beginLoc());
				YYERROR;
			} else {
				LAW_SIMPLE_FORM(yygotominor.yy334, yymsp[-4].minor.yy0, yymsp[-3].minor.yy6, yymsp[-1].minor.yy113, yymsp[0].minor.yy0, Language::Feature::LAW_OBSERVED, ObservedLaw); 
			}
		  yy_destructor(yypParser,66,&yymsp[-2].minor);
}
#line 5282 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* stmt_code_blk ::= ASP_GR */
#line 2503 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 5287 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* stmt_code_blk ::= ASP_CP */
#line 2504 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 5292 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 365: /* stmt_code_blk ::= F2LP_GR */
#line 2505 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 5297 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* stmt_code_blk ::= F2LP_CP */
#line 2506 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 5302 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* stmt_code_blk ::= LUA_GR */
#line 2507 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 5307 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* stmt_code_blk ::= LUA_CP */
#line 2508 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 5312 "bcplus/parser/detail/lemon_parser.c"
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
#line 186 "bcplus/parser/detail/lemon_parser.y"
 parser->_parse_error("Syntax error.");	
#line 5378 "bcplus/parser/detail/lemon_parser.c"
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
