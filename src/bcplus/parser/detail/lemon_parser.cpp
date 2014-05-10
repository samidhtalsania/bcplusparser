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

		
#line 326 "bcplus/parser/detail/lemon_parser.y"

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

#line 445 "bcplus/parser/detail/lemon_parser.y"

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

#line 669 "bcplus/parser/detail/lemon_parser.y"

	#define NUM_UOP(t_new, t, val)																				\
		ref_ptr<const Referenced> t_ptr = t;																			\
		t_new = new Number(val, t->beginLoc(), t->endLoc());

	
	#define NUM_BOP(t_new, l, r, val)																			\
		ref_ptr<const Referenced> l_ptr = l, r_ptr = r;																\
		t_new = new Number(val, l->beginLoc(), r->endLoc());

#line 711 "bcplus/parser/detail/lemon_parser.y"

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

#line 791 "bcplus/parser/detail/lemon_parser.y"


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
	


#line 867 "bcplus/parser/detail/lemon_parser.y"

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

#line 947 "bcplus/parser/detail/lemon_parser.y"

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



#line 1176 "bcplus/parser/detail/lemon_parser.y"

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
#line 1335 "bcplus/parser/detail/lemon_parser.y"

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
#line 2021 "bcplus/parser/detail/lemon_parser.y"

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

#line 2044 "bcplus/parser/detail/lemon_parser.y"

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
#line 2071 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRange const* maxstep;
		Token const* label;
	};

#line 2174 "bcplus/parser/detail/lemon_parser.y"

	#define QUERY_DECL(decl, kw, val, feature)																\
		decl = NULL;																						\
		ref_ptr<const Token> kw_ptr = kw, val_ptr = val;													\
																											\
		if (!parser->lang()->support(feature)) {															\
			parser->_feature_error(feature, &kw->beginLoc());												\
			YYERROR;																						\
		} else {																							\
			decl = val_ptr.release();																		\
		}

#line 2246 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2329 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2510 "bcplus/parser/detail/lemon_parser.y"

	#define CODE_BLK(stmt, code, feature, type) 												\
		ref_ptr<const Token> code_ptr = code;													\
		if (!parser->lang()->support(feature)) {												\
			stmt = NULL;																		\
			parser->_feature_error(feature, &code->beginLoc());									\
			YYERROR;																			\
		} else {																				\
			stmt = new type(code, code->beginLoc(), code->endLoc());							\
		}
#line 515 "bcplus/parser/detail/lemon_parser.c"
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
  CardinalityFormula* yy73;
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
  NumberRange const* yy234;
  CardinalityFormula::VariableList* yy279;
  ObjectDeclaration::Element::ObjectList* yy299;
  SortSymbol const* yy310;
  Variable* yy313;
  IdentifierDeclList* yy316;
  SortSymbol* yy325;
  VariableDeclaration::ElementList* yy326;
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
  NCStatement* yy451;
  Number* yy468;
  NumberRange* yy475;
  int yy483;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define lemon_parserARG_SDECL  BCParser* parser						;
#define lemon_parserARG_PDECL , BCParser* parser						
#define lemon_parserARG_FETCH  BCParser* parser						 = yypParser->parser						
#define lemon_parserARG_STORE yypParser->parser						 = parser						
#define YYNSTATE 686
#define YYNRULE 355
#define YYERRORSYMBOL 136
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
#define YY_ACTTAB_COUNT (2637)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   685,  536,  277,  684,  683,  682,  681,  680,  679,  678,
 /*    10 */   677,  676,  675,  674,  673,  672,  671,  670,  686,  645,
 /*    20 */   258,  669,  662,  668,  667,  411,  366,  414,  413,  147,
 /*    30 */   560,  255,  259,   98,   96,   95,   34,  184,  607,  337,
 /*    40 */    20,   64,   63,   62,  551,   61,   60,   59,   58,  159,
 /*    50 */   235,  186,  605,  183,  181,  179,  177,  176,  645,  258,
 /*    60 */   669,  662,  668,  667,  165,   94,   93,   92,   91,   90,
 /*    70 */   251,  259,  509,  277,  415,    9,  486,  598,  597,  596,
 /*    80 */   595,   25,   24,   28,   27,  272,  122,   29,  471,  470,
 /*    90 */   469,  468,  467,  466,  465,  464,  463,  462,  461,  460,
 /*   100 */   459,  458,  457,  456,  455,  644,  513,  361,  508,  431,
 /*   110 */   643,  440,  439,  436,  435,  438,  437,  644,  349,  475,
 /*   120 */   474,  431,  643,  509,  277,  659,  665,  666,  669,  662,
 /*   130 */   668,  667,    8,  376,  325,   51,   36,   50,    6,    7,
 /*   140 */   626,  154,  248,  204,  249,  620,  429,  664,  198,  431,
 /*   150 */   619,   49,  637,  636,  638,    5,  473,  472,    4,  511,
 /*   160 */    35,  190,  391,  494,  637,  636,  392,  603,   38,  604,
 /*   170 */   126,  124,  123,  260,  148,  107,  106,  103,  241,  593,
 /*   180 */   247,  594,   37,  161,  509,  277,  146,  623,   21,   28,
 /*   190 */    27,  398,  247,   29,  644,  352,  492,  430,  431,  643,
 /*   200 */    57,  621,  615,  669,  662,  668,  667,  616,  514,  617,
 /*   210 */   104,  372,  299,  163,  373,   89,  129,  362,  507,  361,
 /*   220 */   508,   18,   19,  434,   56,  644,   29,   89,  140,  431,
 /*   230 */   643,  179,  177,  176,  498,  433,  661,  432,  663,  153,
 /*   240 */   248,  637,  636,  638,  357,  353,  400,  433,  661,  432,
 /*   250 */   127,   40,   39,   43,   42,  125,  593,   44,  594,  506,
 /*   260 */   161,  156,  157,  146,   52,   33,   43,   42,  103,  247,
 /*   270 */    44,  233,  637,  636,  638,  433,  661,  432,  151,  164,
 /*   280 */   166,  136,    2,  142,  145,  150,   65,  593,  542,  594,
 /*   290 */   405,  161,  541,   44,  146,  430,   21,  143,   30,   31,
 /*   300 */   247,  660,  549,  402,   89,  659,  665,  666,  669,  662,
 /*   310 */   668,  667,  395,  396,  324,  368,  515,  367,  584,  574,
 /*   320 */   669,  662,  668,  667,  433,  661,  432,  536,  277,   18,
 /*   330 */    19,  103,  256,  563,  549,   89,  623,  549,  535,   53,
 /*   340 */   644,  278,  571,  568,  431,  643,  359,  501,  358,  894,
 /*   350 */   365,  532,  366,  414,  413,  433,  661,  432,  529,  528,
 /*   360 */   527,  526,  162,  373,  894,  894,  407,  645,  322,  669,
 /*   370 */   662,  668,  667,  182,  525,  416,  523,  156,  157,  281,
 /*   380 */   323,  522,   16,   15,  894,  894,   17,  637,  636,  638,
 /*   390 */   500,  590,  589,  521,  519,  520,  524,  894,  141,   23,
 /*   400 */   536,  277,  603,  894,  604,  182,  558,  180,  182,  148,
 /*   410 */   158,  106,  178,  429,  623,  247,  645,  258,  669,  662,
 /*   420 */   668,  667,  232,  537,  533,  366,  414,  413,  251,  259,
 /*   430 */   428,  412,  410,  397,  486,  598,  597,  596,  595,  180,
 /*   440 */    32,  373,  180,  271,  178,  104,  496,  178,  401,  644,
 /*   450 */    89,  562,  583,  431,  643,  561,  431,  582,  622,  659,
 /*   460 */   665,  666,  669,  662,  668,  667,  491,  375,  325,  234,
 /*   470 */   433,  661,  432,  147,  987,  144,  642,  479,  478,  536,
 /*   480 */   277,  623,  248,  553,   20,   64,   63,   62,  131,   61,
 /*   490 */    60,   59,   58,  624,  987,  144,  637,  636,  638,  576,
 /*   500 */   575,  577,  364,  532,  366,  414,  413,   22,  373,   94,
 /*   510 */    93,   92,   91,   90,  566,  429,  567,  424,  480,  187,
 /*   520 */   106,  371,  552,   47,  536,  277,  661,  147,  417,  658,
 /*   530 */   642,  530,  427,  431,  657,   48,  585,  430,    3,   64,
 /*   540 */    63,   62,  139,   61,   60,   59,   58,  363,  532,  366,
 /*   550 */   414,  413,  477,  476,  105,  540,   45,   46,  188,   89,
 /*   560 */   534,  138,  109,   94,   93,   92,   91,   90,   26,   25,
 /*   570 */    24,   28,   27,  555,  556,   29,  652,  651,  653,  433,
 /*   580 */   661,  432,  433,  661,  432,  645,  258,  669,  662,  668,
 /*   590 */   667,  654,  409,  655,  517,  550,  367,  254,  259,  408,
 /*   600 */   101,  247,  279,  601,  598,  597,  596,  595,  645,  258,
 /*   610 */   669,  662,  668,  667,  516,  135,  536,  277,  656,  137,
 /*   620 */   252,  259,  185,  244,  138,  263,  601,  598,  597,  596,
 /*   630 */   595,  505,  358,  518,   99,  590,  589,  356,  400,   97,
 /*   640 */   531,  366,  414,  413,  138,  645,  258,  669,  662,  668,
 /*   650 */   667,  102,  100,   98,   96,   95,  433,  254,  259,  433,
 /*   660 */   661,  432,  264,  601,  598,  597,  596,  595,  645,  257,
 /*   670 */   669,  662,  668,  667,  429,  592,   13,   12,   16,   15,
 /*   680 */   602,  259,   17,  369,  554,  587,  601,  598,  597,  596,
 /*   690 */   595,  494,  355,  400,  404,  645,  258,  669,  662,  668,
 /*   700 */   667,   55,   26,   25,   24,   28,   27,  254,  259,   29,
 /*   710 */   354,  400,  600,  601,  598,  597,  596,  595,  645,  258,
 /*   720 */   669,  662,  668,  667,  495,   26,   25,   24,   28,   27,
 /*   730 */   254,  259,   29,  134,  497,  599,  601,  598,  597,  596,
 /*   740 */   595,  645,  258,  669,  662,  668,  667,   54,   26,   25,
 /*   750 */    24,   28,   27,  254,  259,   29, 1042,    1,  426,  601,
 /*   760 */   598,  597,  596,  595,  645,  258,  669,  662,  668,  667,
 /*   770 */   429,   26,   25,   24,   28,   27,  254,  259,   29,  328,
 /*   780 */   400,  425,  601,  598,  597,  596,  595,  494,  481,  277,
 /*   790 */   645,  258,  669,  662,  668,  667,  102,  100,   98,   96,
 /*   800 */    95,   10,  254,  259,  155,  424,  133,  295,  601,  598,
 /*   810 */   597,  596,  595,  645,  258,  669,  662,  668,  667,  351,
 /*   820 */   492,  586,  510,  424,  209,  254,  259,  132,  503,  399,
 /*   830 */   336,  601,  598,  597,  596,  595,  645,  258,  669,  662,
 /*   840 */   668,  667,  377,   26,   25,   24,   28,   27,  254,  259,
 /*   850 */    29,  493,  138,  335,  601,  598,  597,  596,  595,  642,
 /*   860 */   499,  490,  175,  645,  258,  669,  662,  668,  667,   17,
 /*   870 */    26,   25,   24,   28,   27,  254,  259,   29,  174,  152,
 /*   880 */   266,  601,  598,  597,  596,  595,  645,  258,  669,  662,
 /*   890 */   668,  667,   94,   93,   92,   91,   90,  348,  254,  259,
 /*   900 */   108,  454,  453,  265,  601,  598,  597,  596,  595,  645,
 /*   910 */   258,  669,  662,  668,  667,  452,  451,  450,   34,  449,
 /*   920 */   448,  250,  259,  625,  447,  446,  445,  486,  598,  597,
 /*   930 */   596,  595,  661,  444,  443,  442,  261,  645,  258,  669,
 /*   940 */   662,  668,  667,  609,   41,   40,   39,   43,   42,  251,
 /*   950 */   259,   44,  430,  608,  433,  486,  598,  597,  596,  595,
 /*   960 */   243,   34,  591,   34,  273,  645,  257,  669,  662,  668,
 /*   970 */   667,  588,   26,   25,   24,   28,   27,  487,  259,   29,
 /*   980 */   242,   48,  160,  486,  598,  597,  596,  595,  421,  420,
 /*   990 */   239,  237,  483,  645,  258,  669,  662,  668,  667,  621,
 /*  1000 */   615,  669,  662,  668,  667,  251,  259,  419,  418,  559,
 /*  1010 */   300,  486,  598,  597,  596,  595,  618,  236,  370,  540,
 /*  1020 */   485,  645,  258,  669,  662,  668,  667,  539,  102,  100,
 /*  1030 */    98,   96,   95,  251,  259,  535,  512,  536,  277,  486,
 /*  1040 */   598,  597,  596,  595,  504,  230,   11,   10,  484,  130,
 /*  1050 */   128,  126,  124,  123,  645,  258,  669,  662,  668,  667,
 /*  1060 */   360,  532,  366,  414,  413,  203,  251,  259,  564,  538,
 /*  1070 */   228,  284,  486,  598,  597,  596,  595,  229,  390,  388,
 /*  1080 */   557,  394,  226,  645,  258,  669,  662,  668,  667,  225,
 /*  1090 */    41,   40,   39,   43,   42,  251,  259,   44,  224,  221,
 /*  1100 */   220,  486,  598,  597,  596,  595,  223,  219,  387,  208,
 /*  1110 */   393,  645,  258,  669,  662,  668,  667,  218,   14,   13,
 /*  1120 */    12,   16,   15,  251,  259,   17,  216,  207,  386,  486,
 /*  1130 */   598,  597,  596,  595,  214,  202,  385,  212,  283,  645,
 /*  1140 */   258,  669,  662,  668,  667,  201,  384,  210,  205,  536,
 /*  1150 */   277,  251,  259,  383,  206,  382,  200,  486,  598,  597,
 /*  1160 */   596,  595,  199,  197,  381,  196,  327,  645,  258,  669,
 /*  1170 */   662,  668,  667,  406,  366,  414,  413,  193,  195,  251,
 /*  1180 */   259,  194,  380,  379,  191,  486,  598,  597,  596,  595,
 /*  1190 */   189,  378,  231,  245,  326,  645,  258,  669,  662,  668,
 /*  1200 */   667,   83,   82,   81,   80,   79,  329,  254,  259,  276,
 /*  1210 */   330,  240,  270,  601,  598,  597,  596,  595,  547,  543,
 /*  1220 */   546,  545,  645,  258,  669,  662,  668,  667,   88,   87,
 /*  1230 */    86,   85,   84,  544,  254,  259,  275,  274,  389,  269,
 /*  1240 */   601,  598,  597,  596,  595,  130,  128,  126,  124,  123,
 /*  1250 */   217,  645,  258,  669,  662,  668,  667,  183,  181,  179,
 /*  1260 */   177,  176,  215,  254,  259,  213,  211,  403,  172,  601,
 /*  1270 */   598,  597,  596,  595,  192,  502, 1043, 1043, 1043,  645,
 /*  1280 */   258,  669,  662,  668,  667, 1043, 1043, 1043, 1043, 1043,
 /*  1290 */  1043,  254,  259, 1043, 1043, 1043,  171,  601,  598,  597,
 /*  1300 */   596,  595, 1043, 1043, 1043, 1043, 1043,  645,  258,  669,
 /*  1310 */   662,  668,  667, 1043, 1043, 1043, 1043, 1043, 1043,  254,
 /*  1320 */   259, 1043, 1043, 1043,  170,  601,  598,  597,  596,  595,
 /*  1330 */  1043, 1043, 1043, 1043, 1043,  645,  258,  669,  662,  668,
 /*  1340 */   667, 1043, 1043, 1043, 1043, 1043, 1043,  254,  259, 1043,
 /*  1350 */  1043, 1043,  169,  601,  598,  597,  596,  595, 1043, 1043,
 /*  1360 */  1043, 1043, 1043,  645,  258,  669,  662,  668,  667, 1043,
 /*  1370 */  1043, 1043, 1043, 1043, 1043,  254,  259, 1043, 1043, 1043,
 /*  1380 */   168,  601,  598,  597,  596,  595, 1043,  549,  535, 1043,
 /*  1390 */   645,  258,  669,  662,  668,  667, 1043, 1043, 1043, 1043,
 /*  1400 */  1043, 1043,  254,  259, 1043, 1043,  149,  167,  601,  598,
 /*  1410 */   597,  596,  595, 1043, 1043, 1043, 1043, 1043,   64,   63,
 /*  1420 */    62, 1043,   61,   60,   59,   58, 1043, 1043,  659,  665,
 /*  1430 */   666,  669,  662,  668,  667,  147,  374,  325, 1043, 1043,
 /*  1440 */  1043, 1043,   94,   93,   92,   91,   90,   64,   63,   62,
 /*  1450 */   581,   61,   60,   59,   58, 1043, 1043, 1043,  182,  116,
 /*  1460 */   115,  114, 1043,  113,  112,  111,  110, 1043, 1043, 1043,
 /*  1470 */  1043,   94,   93,   92,   91,   90, 1043, 1043, 1043, 1043,
 /*  1480 */  1043, 1043, 1043,  121,  120,  119,  118,  117, 1043,   78,
 /*  1490 */    77,   76,  180,   75,   74,   73,   72,  178, 1043, 1043,
 /*  1500 */  1043,   65,   71,   70, 1043,   69,   68,   67,   66, 1043,
 /*  1510 */  1043, 1043, 1043,   83,   82,   81,   80,   79, 1043, 1043,
 /*  1520 */  1043, 1043, 1043, 1043, 1043,   88,   87,   86,   85,   84,
 /*  1530 */   748,  748,  748, 1043,  748,  748,  748,  748, 1043, 1043,
 /*  1540 */   548, 1043,  116,  115,  114, 1043,  113,  112,  111,  110,
 /*  1550 */  1043,  350, 1043, 1043,  748,  748,  748,  748,  748,  645,
 /*  1560 */   258,  669,  662,  668,  667, 1043,  121,  120,  119,  118,
 /*  1570 */   117,  255,  259,  183,  181,  179,  177,  176,  607,  606,
 /*  1580 */  1043,  645,  258,  669,  662,  668,  667, 1043, 1043, 1043,
 /*  1590 */  1043,  246,  605,  255,  259, 1043, 1043, 1043, 1043, 1043,
 /*  1600 */   607,  606, 1043, 1043,  645,  258,  669,  662,  668,  667,
 /*  1610 */  1043, 1043, 1043,  238,  605, 1043,  255,  259, 1043, 1043,
 /*  1620 */  1043, 1043,  182,  607,  606, 1043, 1043,  645,  258,  669,
 /*  1630 */   662,  668,  667, 1043, 1043, 1043,  227,  605, 1043,  255,
 /*  1640 */   259, 1043, 1043, 1043, 1043, 1043,  607,  606, 1043,  584,
 /*  1650 */   574,  669,  662,  668,  667, 1043,  180, 1043,  489,  222,
 /*  1660 */   605,  178, 1043,  253,  584,  574,  669,  662,  668,  667,
 /*  1670 */  1043, 1043,  262,  571,  568, 1043, 1043, 1043,  573, 1043,
 /*  1680 */  1043,  584,  574,  669,  662,  668,  667,  565,  571,  568,
 /*  1690 */   183,  181,  179,  177,  176,  256, 1043, 1043,  584,  574,
 /*  1700 */   669,  662,  668,  667,  570,  571,  568, 1043, 1043, 1043,
 /*  1710 */  1043,  488,  256,  584,  574,  669,  662,  668,  667, 1043,
 /*  1720 */  1043,  569,  571,  568, 1043, 1043, 1043,  256,  584,  574,
 /*  1730 */   669,  662,  668,  667, 1043, 1043,  423,  571,  568,  441,
 /*  1740 */  1043, 1043,  256,  183,  181,  179,  177,  176, 1043, 1043,
 /*  1750 */  1043,  422,  571,  568, 1043, 1043,  584,  574,  669,  662,
 /*  1760 */   668,  667,  584,  574,  669,  662,  668,  667, 1043, 1043,
 /*  1770 */   256,  130,  128,  126,  124,  123,  256, 1043, 1043,  285,
 /*  1780 */   571,  568, 1043, 1043, 1043,  332,  571,  568,  584,  574,
 /*  1790 */   669,  662,  668,  667, 1043,  645,  633,  669,  662,  668,
 /*  1800 */   667, 1043,  256, 1043, 1043, 1043, 1043,  635,  323, 1043,
 /*  1810 */  1043,  331,  571,  568, 1043,  645,  267,  669,  662,  668,
 /*  1820 */   667, 1043, 1043, 1043, 1043, 1043, 1043,  635,  323,  659,
 /*  1830 */   665,  666,  669,  662,  668,  667, 1043, 1043,  346,  659,
 /*  1840 */   665,  666,  669,  662,  668,  667, 1043, 1043,  282, 1043,
 /*  1850 */   659,  665,  666,  669,  662,  668,  667, 1043, 1043,  347,
 /*  1860 */   659,  665,  666,  669,  662,  668,  667, 1043, 1043,  650,
 /*  1870 */   659,  665,  666,  669,  662,  668,  667, 1043, 1043,  646,
 /*  1880 */  1043, 1043,  659,  665,  666,  669,  662,  668,  667, 1043,
 /*  1890 */  1043,  649,  659,  665,  666,  669,  662,  668,  667, 1043,
 /*  1900 */  1043,  648, 1043, 1043, 1043, 1043,  659,  665,  666,  669,
 /*  1910 */   662,  668,  667, 1043, 1043,  647,  659,  665,  666,  669,
 /*  1920 */   662,  668,  667, 1043, 1043,  345, 1043, 1043, 1043, 1043,
 /*  1930 */  1043,  659,  665,  666,  669,  662,  668,  667, 1043, 1043,
 /*  1940 */   344,  659,  665,  666,  669,  662,  668,  667, 1043, 1043,
 /*  1950 */   641,  659,  665,  666,  669,  662,  668,  667, 1043, 1043,
 /*  1960 */   640, 1043,  659,  665,  666,  669,  662,  668,  667, 1043,
 /*  1970 */  1043,  639,  659,  665,  666,  669,  662,  668,  667, 1043,
 /*  1980 */  1043,  634,  659,  665,  666,  669,  662,  668,  667, 1043,
 /*  1990 */  1043,  343, 1043,  659,  665,  666,  669,  662,  668,  667,
 /*  2000 */  1043, 1043,  342,  659,  665,  666,  669,  662,  668,  667,
 /*  2010 */  1043, 1043,  632, 1043,  659,  665,  666,  669,  662,  668,
 /*  2020 */   667, 1043, 1043,  631,  659,  665,  666,  669,  662,  668,
 /*  2030 */   667, 1043, 1043,  630,  659,  665,  666,  669,  662,  668,
 /*  2040 */   667, 1043, 1043,  341, 1043, 1043,  659,  665,  666,  669,
 /*  2050 */   662,  668,  667, 1043, 1043,  340,  659,  665,  666,  669,
 /*  2060 */   662,  668,  667, 1043, 1043,  629,  659,  665,  666,  669,
 /*  2070 */   662,  668,  667, 1043, 1043,  628,  659,  665,  666,  669,
 /*  2080 */   662,  668,  667, 1043, 1043,  627,  659,  665,  666,  669,
 /*  2090 */   662,  668,  667, 1043, 1043,  321,  659,  665,  666,  669,
 /*  2100 */   662,  668,  667, 1043, 1043,  320,  659,  665,  666,  669,
 /*  2110 */   662,  668,  667, 1043, 1043,  319, 1043,  659,  665,  666,
 /*  2120 */   669,  662,  668,  667, 1043, 1043,  318,  659,  665,  666,
 /*  2130 */   669,  662,  668,  667, 1043, 1043,  317,  659,  665,  666,
 /*  2140 */   669,  662,  668,  667, 1043, 1043,  316, 1043,  659,  665,
 /*  2150 */   666,  669,  662,  668,  667, 1043, 1043,  315,  659,  665,
 /*  2160 */   666,  669,  662,  668,  667, 1043, 1043,  314, 1043,  659,
 /*  2170 */   665,  666,  669,  662,  668,  667, 1043, 1043,  313,  659,
 /*  2180 */   665,  666,  669,  662,  668,  667, 1043, 1043,  312,  659,
 /*  2190 */   665,  666,  669,  662,  668,  667, 1043, 1043,  311, 1043,
 /*  2200 */  1043,  659,  665,  666,  669,  662,  668,  667, 1043, 1043,
 /*  2210 */   310,  659,  665,  666,  669,  662,  668,  667, 1043, 1043,
 /*  2220 */   309,  659,  665,  666,  669,  662,  668,  667, 1043, 1043,
 /*  2230 */   308,  659,  665,  666,  669,  662,  668,  667, 1043, 1043,
 /*  2240 */   307,  659,  665,  666,  669,  662,  668,  667, 1043, 1043,
 /*  2250 */   306,  659,  665,  666,  669,  662,  668,  667, 1043, 1043,
 /*  2260 */   305,  659,  665,  666,  669,  662,  668,  667, 1043, 1043,
 /*  2270 */   304, 1043,  659,  665,  666,  669,  662,  668,  667, 1043,
 /*  2280 */  1043,  303,  659,  665,  666,  669,  662,  668,  667, 1043,
 /*  2290 */  1043,  302,  659,  665,  666,  669,  662,  668,  667, 1043,
 /*  2300 */  1043,  301, 1043,  659,  665,  666,  669,  662,  668,  667,
 /*  2310 */  1043, 1043,  297,  659,  665,  666,  669,  662,  668,  667,
 /*  2320 */  1043, 1043,  296, 1043,  659,  665,  666,  669,  662,  668,
 /*  2330 */   667, 1043, 1043,  294,  659,  665,  666,  669,  662,  668,
 /*  2340 */   667,  482, 1043,  293,  659,  665,  666,  669,  662,  668,
 /*  2350 */   667, 1043, 1043,  173, 1043, 1043, 1043,  621,  615,  669,
 /*  2360 */   662,  668,  667,   14,   13,   12,   16,   15,  338, 1043,
 /*  2370 */    17,  621,  615,  669,  662,  668,  667, 1043, 1043, 1043,
 /*  2380 */  1043, 1043,  280,  621,  615,  669,  662,  668,  667, 1043,
 /*  2390 */  1043, 1043, 1043, 1043,  339, 1043,  621,  615,  669,  662,
 /*  2400 */   668,  667, 1043, 1043, 1043, 1043, 1043,  614,  621,  615,
 /*  2410 */   669,  662,  668,  667, 1043, 1043, 1043, 1043, 1043,  610,
 /*  2420 */   621,  615,  669,  662,  668,  667, 1043, 1043, 1043, 1043,
 /*  2430 */  1043,  613,  621,  615,  669,  662,  668,  667, 1043, 1043,
 /*  2440 */  1043, 1043, 1043,  612,  621,  615,  669,  662,  668,  667,
 /*  2450 */  1043, 1043, 1043, 1043, 1043,  611, 1043,  621,  615,  669,
 /*  2460 */   662,  668,  667, 1043, 1043, 1043, 1043, 1043,  298,  621,
 /*  2470 */   615,  669,  662,  668,  667, 1043, 1043, 1043, 1043, 1043,
 /*  2480 */   334,  621,  615,  669,  662,  668,  667, 1043, 1043, 1043,
 /*  2490 */  1043, 1043,  333,  621,  615,  669,  662,  668,  667, 1043,
 /*  2500 */  1043, 1043, 1043, 1043,  580,  621,  615,  669,  662,  668,
 /*  2510 */   667, 1043, 1043, 1043, 1043, 1043,  579,  621,  615,  669,
 /*  2520 */   662,  668,  667, 1043, 1043, 1043, 1043, 1043,  578,  621,
 /*  2530 */   615,  669,  662,  668,  667, 1043, 1043, 1043, 1043, 1043,
 /*  2540 */   292,  621,  615,  669,  662,  668,  667, 1043, 1043, 1043,
 /*  2550 */  1043, 1043,  291,  621,  615,  669,  662,  668,  667, 1043,
 /*  2560 */  1043, 1043, 1043, 1043,  290,  621,  615,  669,  662,  668,
 /*  2570 */   667, 1043, 1043, 1043, 1043, 1043,  289,  621,  615,  669,
 /*  2580 */   662,  668,  667, 1043, 1043, 1043, 1043, 1043,  288,  621,
 /*  2590 */   615,  669,  662,  668,  667, 1043, 1043, 1043, 1043, 1043,
 /*  2600 */   287,  621,  615,  669,  662,  668,  667, 1043, 1043, 1043,
 /*  2610 */  1043, 1043,  286,  621,  615,  669,  662,  668,  667, 1043,
 /*  2620 */  1043, 1043, 1043, 1043,  572,  621,  615,  669,  662,  668,
 /*  2630 */   667, 1043, 1043, 1043, 1043, 1043,  268,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   136,  170,  171,  139,  140,  141,  142,  143,  144,  145,
 /*    10 */   146,  147,  148,  149,  150,  151,  152,  153,    0,  155,
 /*    20 */   156,  157,  158,  159,  160,  194,  195,  196,  197,   70,
 /*    30 */    74,  167,  168,  108,  109,  110,   41,   90,  174,  175,
 /*    40 */    81,   82,   83,   84,   74,   86,   87,   88,   89,   72,
 /*    50 */    94,  187,  188,  106,  107,  108,  109,  110,  155,  156,
 /*    60 */   157,  158,  159,  160,   94,  106,  107,  108,  109,  110,
 /*    70 */   167,  168,  170,  171,   97,   80,  173,  174,  175,  176,
 /*    80 */   177,   96,   97,   98,   99,  182,   82,  102,  224,  225,
 /*    90 */   226,  227,  228,  229,  230,  231,  232,  233,  234,  235,
 /*   100 */   236,  237,  238,  239,  240,    1,  204,  205,  206,    5,
 /*   110 */     6,    7,    8,    9,   10,   11,   12,    1,  215,  216,
 /*   120 */   217,    5,    6,  170,  171,  154,  155,  156,  157,  158,
 /*   130 */   159,  160,   28,  162,  163,   72,   32,   72,   34,   35,
 /*   140 */    99,   37,   76,   39,  103,    1,  164,   73,   44,    5,
 /*   150 */     6,   72,   48,   49,   50,   51,   52,   53,   54,  206,
 /*   160 */    56,   57,   46,  181,   48,   49,   50,   63,   33,   65,
 /*   170 */   108,  109,  110,   69,   70,   72,   72,  103,   43,   63,
 /*   180 */    76,   65,   47,   67,  170,  171,   70,  159,   72,   98,
 /*   190 */    99,   27,   76,  102,    1,  213,  214,  131,    5,    6,
 /*   200 */    71,  155,  156,  157,  158,  159,  160,   63,   74,   65,
 /*   210 */   106,  165,  166,  185,  186,  111,   72,  203,  204,  205,
 /*   220 */   206,  105,  106,  119,   71,    1,  102,  111,   94,    5,
 /*   230 */     6,  108,  109,  110,   73,  131,  132,  133,   73,  135,
 /*   240 */    76,   48,   49,   50,  210,  211,  212,  131,  132,  133,
 /*   250 */   106,   96,   97,   98,   99,  111,   63,  102,   65,   74,
 /*   260 */    67,  100,  101,   70,   77,   72,   98,   99,  103,   76,
 /*   270 */   102,   77,   48,   49,   50,  131,  132,  133,   14,   94,
 /*   280 */    16,   17,   18,   19,   20,   21,   82,   63,  108,   65,
 /*   290 */   103,   67,  112,  102,   70,  131,   72,  103,  105,  106,
 /*   300 */    76,   73,    1,    2,  111,  154,  155,  156,  157,  158,
 /*   310 */   159,  160,   48,   49,  163,  198,  199,  200,  155,  156,
 /*   320 */   157,  158,  159,  160,  131,  132,  133,  170,  171,  105,
 /*   330 */   106,  103,  169,   74,    1,  111,  159,    1,    2,   31,
 /*   340 */     1,  178,  179,  180,    5,    6,  207,  208,  209,   26,
 /*   350 */   193,  194,  195,  196,  197,  131,  132,  133,   22,   23,
 /*   360 */    24,   25,  185,  186,   41,   42,   30,  155,  156,  157,
 /*   370 */   158,  159,  160,   72,   38,   97,   40,  100,  101,  167,
 /*   380 */   168,   45,   98,   99,   61,   62,  102,   48,   49,   50,
 /*   390 */    74,   91,   92,   57,   58,   59,   60,   74,  134,   99,
 /*   400 */   170,  171,   63,   80,   65,   72,   75,  106,   72,   70,
 /*   410 */    94,   72,  111,  164,  159,   76,  155,  156,  157,  158,
 /*   420 */   159,  160,   77,    1,  194,  195,  196,  197,  167,  168,
 /*   430 */   181,  201,  202,   27,  173,  174,  175,  176,  177,  106,
 /*   440 */   185,  186,  106,  182,  111,  106,   74,  111,  103,    1,
 /*   450 */   111,   74,    1,    5,    6,   74,    5,    6,   73,  154,
 /*   460 */   155,  156,  157,  158,  159,  160,   74,  162,  163,  107,
 /*   470 */   131,  132,  133,   70,   74,  103,   73,  216,  217,  170,
 /*   480 */   171,  159,   76,   75,   81,   82,   83,   84,  103,   86,
 /*   490 */    87,   88,   89,   99,   94,  103,   48,   49,   50,   48,
 /*   500 */    49,   50,  193,  194,  195,  196,  197,  185,  186,  106,
 /*   510 */   107,  108,  109,  110,   63,  164,   65,  156,   74,  107,
 /*   520 */    72,  189,  190,   72,  170,  171,  132,   70,   73,    1,
 /*   530 */    73,   73,  181,    5,    6,   62,  175,  131,   94,   82,
 /*   540 */    83,   84,   72,   86,   87,   88,   89,  193,  194,  195,
 /*   550 */   196,  197,    1,    2,  106,  133,  105,  106,  103,  111,
 /*   560 */    73,  103,  111,  106,  107,  108,  109,  110,   95,   96,
 /*   570 */    97,   98,   99,    3,    4,  102,   48,   49,   50,  131,
 /*   580 */   132,  133,  131,  132,  133,  155,  156,  157,  158,  159,
 /*   590 */   160,   63,   55,   65,   73,  199,  200,  167,  168,    2,
 /*   600 */    72,   76,  172,  173,  174,  175,  176,  177,  155,  156,
 /*   610 */   157,  158,  159,  160,   73,   72,  170,  171,   73,   72,
 /*   620 */   167,  168,  183,  184,  103,  172,  173,  174,  175,  176,
 /*   630 */   177,  208,  209,   73,  106,   91,   92,  211,  212,  111,
 /*   640 */   194,  195,  196,  197,  103,  155,  156,  157,  158,  159,
 /*   650 */   160,  106,  107,  108,  109,  110,  131,  167,  168,  131,
 /*   660 */   132,  133,  172,  173,  174,  175,  176,  177,  155,  156,
 /*   670 */   157,  158,  159,  160,  164,   68,   96,   97,   98,   99,
 /*   680 */   167,  168,  102,  191,  192,  172,  173,  174,  175,  176,
 /*   690 */   177,  181,  211,  212,    2,  155,  156,  157,  158,  159,
 /*   700 */   160,   71,   95,   96,   97,   98,   99,  167,  168,  102,
 /*   710 */   211,  212,  172,  173,  174,  175,  176,  177,  155,  156,
 /*   720 */   157,  158,  159,  160,  214,   95,   96,   97,   98,   99,
 /*   730 */   167,  168,  102,   72,    2,  172,  173,  174,  175,  176,
 /*   740 */   177,  155,  156,  157,  158,  159,  160,   71,   95,   96,
 /*   750 */    97,   98,   99,  167,  168,  102,  137,  138,  172,  173,
 /*   760 */   174,  175,  176,  177,  155,  156,  157,  158,  159,  160,
 /*   770 */   164,   95,   96,   97,   98,   99,  167,  168,  102,  211,
 /*   780 */   212,  172,  173,  174,  175,  176,  177,  181,  170,  171,
 /*   790 */   155,  156,  157,  158,  159,  160,  106,  107,  108,  109,
 /*   800 */   110,   26,  167,  168,   72,  156,   72,  172,  173,  174,
 /*   810 */   175,  176,  177,  155,  156,  157,  158,  159,  160,  213,
 /*   820 */   214,   73,   73,  156,  175,  167,  168,   72,    2,  103,
 /*   830 */   172,  173,  174,  175,  176,  177,  155,  156,  157,  158,
 /*   840 */   159,  160,  175,   95,   96,   97,   98,   99,  167,  168,
 /*   850 */   102,   74,  103,  172,  173,  174,  175,  176,  177,   73,
 /*   860 */     2,   74,   82,  155,  156,  157,  158,  159,  160,  102,
 /*   870 */    95,   96,   97,   98,   99,  167,  168,  102,   82,   77,
 /*   880 */   172,  173,  174,  175,  176,  177,  155,  156,  157,  158,
 /*   890 */   159,  160,  106,  107,  108,  109,  110,   77,  167,  168,
 /*   900 */    66,   74,   74,  172,  173,  174,  175,  176,  177,  155,
 /*   910 */   156,  157,  158,  159,  160,   74,   74,   74,   41,   74,
 /*   920 */    74,  167,  168,  159,   74,   74,   74,  173,  174,  175,
 /*   930 */   176,  177,  132,   74,   74,   74,  182,  155,  156,  157,
 /*   940 */   158,  159,  160,  164,   95,   96,   97,   98,   99,  167,
 /*   950 */   168,  102,  131,  156,  131,  173,  174,  175,  176,  177,
 /*   960 */   219,   41,  159,   41,  182,  155,  156,  157,  158,  159,
 /*   970 */   160,  159,   95,   96,   97,   98,   99,  167,  168,  102,
 /*   980 */   222,   62,   61,  173,  174,  175,  176,  177,  223,  156,
 /*   990 */   222,  219,  182,  155,  156,  157,  158,  159,  160,  155,
 /*  1000 */   156,  157,  158,  159,  160,  167,  168,  223,  223,  190,
 /*  1010 */   166,  173,  174,  175,  176,  177,   73,  222,    2,  133,
 /*  1020 */   182,  155,  156,  157,  158,  159,  160,  158,  106,  107,
 /*  1030 */   108,  109,  110,  167,  168,    2,  197,  170,  171,  173,
 /*  1040 */   174,  175,  176,  177,  197,  221,   42,   26,  182,  106,
 /*  1050 */   107,  108,  109,  110,  155,  156,  157,  158,  159,  160,
 /*  1060 */   193,  194,  195,  196,  197,  156,  167,  168,   73,  158,
 /*  1070 */   222,  171,  173,  174,  175,  176,  177,  220,  223,  223,
 /*  1080 */   192,  182,  219,  155,  156,  157,  158,  159,  160,  221,
 /*  1090 */    95,   96,   97,   98,   99,  167,  168,  102,  220,  219,
 /*  1100 */   221,  173,  174,  175,  176,  177,  222,  220,  223,  219,
 /*  1110 */   182,  155,  156,  157,  158,  159,  160,  222,   95,   96,
 /*  1120 */    97,   98,   99,  167,  168,  102,  222,  221,  223,  173,
 /*  1130 */   174,  175,  176,  177,  222,  219,  223,  222,  182,  155,
 /*  1140 */   156,  157,  158,  159,  160,  221,  223,  222,  222,  170,
 /*  1150 */   171,  167,  168,  223,  220,  223,  220,  173,  174,  175,
 /*  1160 */   176,  177,  222,  156,  223,  219,  182,  155,  156,  157,
 /*  1170 */   158,  159,  160,  194,  195,  196,  197,  222,  221,  167,
 /*  1180 */   168,  220,  223,  223,  222,  173,  174,  175,  176,  177,
 /*  1190 */   156,  223,  219,  184,  182,  155,  156,  157,  158,  159,
 /*  1200 */   160,  106,  107,  108,  109,  110,  171,  167,  168,  171,
 /*  1210 */   171,  219,  172,  173,  174,  175,  176,  177,  171,  171,
 /*  1220 */   171,  171,  155,  156,  157,  158,  159,  160,  106,  107,
 /*  1230 */   108,  109,  110,  171,  167,  168,  171,  171,  223,  172,
 /*  1240 */   173,  174,  175,  176,  177,  106,  107,  108,  109,  110,
 /*  1250 */   220,  155,  156,  157,  158,  159,  160,  106,  107,  108,
 /*  1260 */   109,  110,  220,  167,  168,  220,  220,    2,  172,  173,
 /*  1270 */   174,  175,  176,  177,  219,    2,  241,  241,  241,  155,
 /*  1280 */   156,  157,  158,  159,  160,  241,  241,  241,  241,  241,
 /*  1290 */   241,  167,  168,  241,  241,  241,  172,  173,  174,  175,
 /*  1300 */   176,  177,  241,  241,  241,  241,  241,  155,  156,  157,
 /*  1310 */   158,  159,  160,  241,  241,  241,  241,  241,  241,  167,
 /*  1320 */   168,  241,  241,  241,  172,  173,  174,  175,  176,  177,
 /*  1330 */   241,  241,  241,  241,  241,  155,  156,  157,  158,  159,
 /*  1340 */   160,  241,  241,  241,  241,  241,  241,  167,  168,  241,
 /*  1350 */   241,  241,  172,  173,  174,  175,  176,  177,  241,  241,
 /*  1360 */   241,  241,  241,  155,  156,  157,  158,  159,  160,  241,
 /*  1370 */   241,  241,  241,  241,  241,  167,  168,  241,  241,  241,
 /*  1380 */   172,  173,  174,  175,  176,  177,  241,    1,    2,  241,
 /*  1390 */   155,  156,  157,  158,  159,  160,  241,  241,  241,  241,
 /*  1400 */   241,  241,  167,  168,  241,  241,   70,  172,  173,  174,
 /*  1410 */   175,  176,  177,  241,  241,  241,  241,  241,   82,   83,
 /*  1420 */    84,  241,   86,   87,   88,   89,  241,  241,  154,  155,
 /*  1430 */   156,  157,  158,  159,  160,   70,  162,  163,  241,  241,
 /*  1440 */   241,  241,  106,  107,  108,  109,  110,   82,   83,   84,
 /*  1450 */    73,   86,   87,   88,   89,  241,  241,  241,   72,   82,
 /*  1460 */    83,   84,  241,   86,   87,   88,   89,  241,  241,  241,
 /*  1470 */   241,  106,  107,  108,  109,  110,  241,  241,  241,  241,
 /*  1480 */   241,  241,  241,  106,  107,  108,  109,  110,  241,   82,
 /*  1490 */    83,   84,  106,   86,   87,   88,   89,  111,  241,  241,
 /*  1500 */   241,   82,   83,   84,  241,   86,   87,   88,   89,  241,
 /*  1510 */   241,  241,  241,  106,  107,  108,  109,  110,  241,  241,
 /*  1520 */   241,  241,  241,  241,  241,  106,  107,  108,  109,  110,
 /*  1530 */    82,   83,   84,  241,   86,   87,   88,   89,  241,  241,
 /*  1540 */    73,  241,   82,   83,   84,  241,   86,   87,   88,   89,
 /*  1550 */   241,    1,  241,  241,  106,  107,  108,  109,  110,  155,
 /*  1560 */   156,  157,  158,  159,  160,  241,  106,  107,  108,  109,
 /*  1570 */   110,  167,  168,  106,  107,  108,  109,  110,  174,  175,
 /*  1580 */   241,  155,  156,  157,  158,  159,  160,  241,  241,  241,
 /*  1590 */   241,  187,  188,  167,  168,  241,  241,  241,  241,  241,
 /*  1600 */   174,  175,  241,  241,  155,  156,  157,  158,  159,  160,
 /*  1610 */   241,  241,  241,  187,  188,  241,  167,  168,  241,  241,
 /*  1620 */   241,  241,   72,  174,  175,  241,  241,  155,  156,  157,
 /*  1630 */   158,  159,  160,  241,  241,  241,  187,  188,  241,  167,
 /*  1640 */   168,  241,  241,  241,  241,  241,  174,  175,  241,  155,
 /*  1650 */   156,  157,  158,  159,  160,  241,  106,  241,   74,  187,
 /*  1660 */   188,  111,  241,  169,  155,  156,  157,  158,  159,  160,
 /*  1670 */   241,  241,  178,  179,  180,  241,  241,  241,  169,  241,
 /*  1680 */   241,  155,  156,  157,  158,  159,  160,  178,  179,  180,
 /*  1690 */   106,  107,  108,  109,  110,  169,  241,  241,  155,  156,
 /*  1700 */   157,  158,  159,  160,  178,  179,  180,  241,  241,  241,
 /*  1710 */   241,   74,  169,  155,  156,  157,  158,  159,  160,  241,
 /*  1720 */   241,  178,  179,  180,  241,  241,  241,  169,  155,  156,
 /*  1730 */   157,  158,  159,  160,  241,  241,  178,  179,  180,   74,
 /*  1740 */   241,  241,  169,  106,  107,  108,  109,  110,  241,  241,
 /*  1750 */   241,  178,  179,  180,  241,  241,  155,  156,  157,  158,
 /*  1760 */   159,  160,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  1770 */   169,  106,  107,  108,  109,  110,  169,  241,  241,  178,
 /*  1780 */   179,  180,  241,  241,  241,  178,  179,  180,  155,  156,
 /*  1790 */   157,  158,  159,  160,  241,  155,  156,  157,  158,  159,
 /*  1800 */   160,  241,  169,  241,  241,  241,  241,  167,  168,  241,
 /*  1810 */   241,  178,  179,  180,  241,  155,  156,  157,  158,  159,
 /*  1820 */   160,  241,  241,  241,  241,  241,  241,  167,  168,  154,
 /*  1830 */   155,  156,  157,  158,  159,  160,  241,  241,  163,  154,
 /*  1840 */   155,  156,  157,  158,  159,  160,  241,  241,  163,  241,
 /*  1850 */   154,  155,  156,  157,  158,  159,  160,  241,  241,  163,
 /*  1860 */   154,  155,  156,  157,  158,  159,  160,  241,  241,  163,
 /*  1870 */   154,  155,  156,  157,  158,  159,  160,  241,  241,  163,
 /*  1880 */   241,  241,  154,  155,  156,  157,  158,  159,  160,  241,
 /*  1890 */   241,  163,  154,  155,  156,  157,  158,  159,  160,  241,
 /*  1900 */   241,  163,  241,  241,  241,  241,  154,  155,  156,  157,
 /*  1910 */   158,  159,  160,  241,  241,  163,  154,  155,  156,  157,
 /*  1920 */   158,  159,  160,  241,  241,  163,  241,  241,  241,  241,
 /*  1930 */   241,  154,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  1940 */   163,  154,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  1950 */   163,  154,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  1960 */   163,  241,  154,  155,  156,  157,  158,  159,  160,  241,
 /*  1970 */   241,  163,  154,  155,  156,  157,  158,  159,  160,  241,
 /*  1980 */   241,  163,  154,  155,  156,  157,  158,  159,  160,  241,
 /*  1990 */   241,  163,  241,  154,  155,  156,  157,  158,  159,  160,
 /*  2000 */   241,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2010 */   241,  241,  163,  241,  154,  155,  156,  157,  158,  159,
 /*  2020 */   160,  241,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  2030 */   160,  241,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  2040 */   160,  241,  241,  163,  241,  241,  154,  155,  156,  157,
 /*  2050 */   158,  159,  160,  241,  241,  163,  154,  155,  156,  157,
 /*  2060 */   158,  159,  160,  241,  241,  163,  154,  155,  156,  157,
 /*  2070 */   158,  159,  160,  241,  241,  163,  154,  155,  156,  157,
 /*  2080 */   158,  159,  160,  241,  241,  163,  154,  155,  156,  157,
 /*  2090 */   158,  159,  160,  241,  241,  163,  154,  155,  156,  157,
 /*  2100 */   158,  159,  160,  241,  241,  163,  154,  155,  156,  157,
 /*  2110 */   158,  159,  160,  241,  241,  163,  241,  154,  155,  156,
 /*  2120 */   157,  158,  159,  160,  241,  241,  163,  154,  155,  156,
 /*  2130 */   157,  158,  159,  160,  241,  241,  163,  154,  155,  156,
 /*  2140 */   157,  158,  159,  160,  241,  241,  163,  241,  154,  155,
 /*  2150 */   156,  157,  158,  159,  160,  241,  241,  163,  154,  155,
 /*  2160 */   156,  157,  158,  159,  160,  241,  241,  163,  241,  154,
 /*  2170 */   155,  156,  157,  158,  159,  160,  241,  241,  163,  154,
 /*  2180 */   155,  156,  157,  158,  159,  160,  241,  241,  163,  154,
 /*  2190 */   155,  156,  157,  158,  159,  160,  241,  241,  163,  241,
 /*  2200 */   241,  154,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  2210 */   163,  154,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  2220 */   163,  154,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  2230 */   163,  154,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  2240 */   163,  154,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  2250 */   163,  154,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  2260 */   163,  154,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  2270 */   163,  241,  154,  155,  156,  157,  158,  159,  160,  241,
 /*  2280 */   241,  163,  154,  155,  156,  157,  158,  159,  160,  241,
 /*  2290 */   241,  163,  154,  155,  156,  157,  158,  159,  160,  241,
 /*  2300 */   241,  163,  241,  154,  155,  156,  157,  158,  159,  160,
 /*  2310 */   241,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2320 */   241,  241,  163,  241,  154,  155,  156,  157,  158,  159,
 /*  2330 */   160,  241,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  2340 */   160,   73,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  2350 */   160,  241,  241,  163,  241,  241,  241,  155,  156,  157,
 /*  2360 */   158,  159,  160,   95,   96,   97,   98,   99,  166,  241,
 /*  2370 */   102,  155,  156,  157,  158,  159,  160,  241,  241,  241,
 /*  2380 */   241,  241,  166,  155,  156,  157,  158,  159,  160,  241,
 /*  2390 */   241,  241,  241,  241,  166,  241,  155,  156,  157,  158,
 /*  2400 */   159,  160,  241,  241,  241,  241,  241,  166,  155,  156,
 /*  2410 */   157,  158,  159,  160,  241,  241,  241,  241,  241,  166,
 /*  2420 */   155,  156,  157,  158,  159,  160,  241,  241,  241,  241,
 /*  2430 */   241,  166,  155,  156,  157,  158,  159,  160,  241,  241,
 /*  2440 */   241,  241,  241,  166,  155,  156,  157,  158,  159,  160,
 /*  2450 */   241,  241,  241,  241,  241,  166,  241,  155,  156,  157,
 /*  2460 */   158,  159,  160,  241,  241,  241,  241,  241,  166,  155,
 /*  2470 */   156,  157,  158,  159,  160,  241,  241,  241,  241,  241,
 /*  2480 */   166,  155,  156,  157,  158,  159,  160,  241,  241,  241,
 /*  2490 */   241,  241,  166,  155,  156,  157,  158,  159,  160,  241,
 /*  2500 */   241,  241,  241,  241,  166,  155,  156,  157,  158,  159,
 /*  2510 */   160,  241,  241,  241,  241,  241,  166,  155,  156,  157,
 /*  2520 */   158,  159,  160,  241,  241,  241,  241,  241,  166,  155,
 /*  2530 */   156,  157,  158,  159,  160,  241,  241,  241,  241,  241,
 /*  2540 */   166,  155,  156,  157,  158,  159,  160,  241,  241,  241,
 /*  2550 */   241,  241,  166,  155,  156,  157,  158,  159,  160,  241,
 /*  2560 */   241,  241,  241,  241,  166,  155,  156,  157,  158,  159,
 /*  2570 */   160,  241,  241,  241,  241,  241,  166,  155,  156,  157,
 /*  2580 */   158,  159,  160,  241,  241,  241,  241,  241,  166,  155,
 /*  2590 */   156,  157,  158,  159,  160,  241,  241,  241,  241,  241,
 /*  2600 */   166,  155,  156,  157,  158,  159,  160,  241,  241,  241,
 /*  2610 */   241,  241,  166,  155,  156,  157,  158,  159,  160,  241,
 /*  2620 */   241,  241,  241,  241,  166,  155,  156,  157,  158,  159,
 /*  2630 */   160,  241,  241,  241,  241,  241,  166,
};
#define YY_SHIFT_USE_DFLT (-76)
#define YY_SHIFT_COUNT (434)
#define YY_SHIFT_MIN   (-75)
#define YY_SHIFT_MAX   (2268)
static const short yy_shift_ofst[] = {
 /*     0 */   -76,  104,  116,  116,  193,  193,  193,  193,  193,  193,
 /*    10 */   193,  193,  224,  224,  224,  224,  224,  224,  224,  224,
 /*    20 */   224,  224,  193,  193,  193,  193,  193,  193,  193,  193,
 /*    30 */   193,  193,  193,  193,  193,  339,  339,  339,  339,  451,
 /*    40 */   451,  451,  451,  451,  451,  451,  451,  451,  451,  528,
 /*    50 */   528,  528,  336,  528,  528,  528,  528,  528,  528,  528,
 /*    60 */   528,  528,  528,  528,  528,  528,  528,  528,  528,  528,
 /*    70 */   528,  528,  528,  528,  528,  528,  528,  528,  528,  528,
 /*    80 */   528,  528,  528,  528,  528,  528,  528,  528,  528,  528,
 /*    90 */   528,  528,  528,  528,  528,  528,  528,  528,  528,  528,
 /*   100 */   528,  528,  528,  528,  448,  448,  448,  144,  144,  144,
 /*   110 */   144,  144,  144,  144,  144,  144,  144,  144,  144,  144,
 /*   120 */   144,  144,  144,  144,  144,  144,  144,  144,  144,  144,
 /*   130 */   144,  144, 1386, 1386, 1386, 1386,  301, 1386, 1386, 1386,
 /*   140 */   301,  406,  164,  301,   66,  732,  394,  394,  394,  394,
 /*   150 */  1273, 1265, 1550,  525,  525,  732,  732,  732,  732,  570,
 /*   160 */   525,  544,   66,   66, 1273, 1265, 1016,  877,  775,  775,
 /*   170 */   775,  775,  473,  922,  333,  333,  333,  333,  333,  333,
 /*   180 */   333,  333,  333,  333,  333,  300,   -5,  422,  570,  919,
 /*   190 */   823,  919,  921,  919,  921, 1021, 1004,  920,  823,  919,
 /*   200 */   921, 1021, 1004,  920,  823,  919,  921, 1021, 1004,  920,
 /*   210 */   919,  921,  919,  921,  919,  921,  919,  921,  919,  921,
 /*   220 */  1021, 1004,  920,  919,  921, 1021, 1004,  920,  919,  921,
 /*   230 */  1021, 1004, 1033, 1033,  886, 1016,  919,  921,  920,  919,
 /*   240 */   921,  823,  919,  921,  800,  800,  920,  823,  821,  800,
 /*   250 */   403,  -41,  457, 1377, 1365, 1336, 1460, 1448, 1419, 1407,
 /*   260 */   264, 2268,  995,  748,  676,  630,  607,  323, 1665,  653,
 /*   270 */   653, 1023, 1023, 1023, 1637, 1584, 1467,  -53,  849,  653,
 /*   280 */   943,  786,  545,  580, 1151,  155, 1139, 1139, 1139, 1139,
 /*   290 */  1139, 1139, 1139,  690,  690,  -15,  690,  690, 1139, 1139,
 /*   300 */  1139,  690,  690,  690,  690,  690,  690,  690,  690,  690,
 /*   310 */   690,  690,  690,  690,  690,  690,  690,  690,  690,  690,
 /*   320 */   690,  690, 1122, 1095,  690,  690,  284,  284,  161,  123,
 /*   330 */   123,  168,  168,   62,   62,   91,   91,  135,   62,   62,
 /*   340 */   -75,  -75,  -75,  -75,  -75,  -75,  -75,  -75,  551,  444,
 /*   350 */   400,  392,  372,  277,  277,  277,  277,  316,  345,  185,
 /*   360 */   749,  194,  134,  541,  521,  458,  180,  187,  -30,  455,
 /*   370 */   -23,  -44,  385,   41,  228,  165,   74,  834,  861,  860,
 /*   380 */   859,  852,  851,  850,  846,  845,  843,  842,  841,  828,
 /*   390 */   827,  820,  802,  767,  767,  796,  780,  787,  777,  858,
 /*   400 */   726,  826,  755,  734,  661,  692,  560,  547,  543,  597,
 /*   410 */   537,  487,  470,  412,  362,  408,  331,  278,  381,  377,
 /*   420 */   308,  259,  191,  191,  204,  124,  124,  153,  129,    4,
 /*   430 */   103,   79,   65,   63,   18,
};
#define YY_REDUCE_USE_DFLT (-170)
#define YY_REDUCE_COUNT (249)
#define YY_REDUCE_MIN   (-169)
#define YY_REDUCE_MAX   (2470)
static const short yy_reduce_ofst[] = {
 /*     0 */   619, -136,  -97,  261, 1235, 1208, 1180, 1152, 1124, 1096,
 /*    10 */  1067, 1040, 1012,  984,  956,  928,  899,  866,  838,  810,
 /*    20 */   782,  754,  731,  708,  681,  658,  635,  609,  586,  563,
 /*    30 */   540,  513,  490,  453,  430, 1472, 1449, 1426, 1404, 1633,
 /*    40 */  1607, 1601, 1573, 1558, 1543, 1526, 1509, 1494,  163, 1274,
 /*    50 */   305,  -29,  230, 2190, 2180, 2170, 2159, 2149, 2138, 2128,
 /*    60 */  2118, 2107, 2097, 2087, 2077, 2067, 2057, 2047, 2035, 2025,
 /*    70 */  2015, 2004, 1994, 1983, 1973, 1963, 1952, 1942, 1932, 1922,
 /*    80 */  1912, 1902, 1892, 1880, 1870, 1860, 1849, 1839, 1828, 1818,
 /*    90 */  1808, 1797, 1787, 1777, 1762, 1752, 1738, 1728, 1716, 1706,
 /*   100 */  1696, 1685, 1675,  151, 1660, 1640,  212,   46, 2470, 2458,
 /*   110 */  2446, 2434, 2422, 2410, 2398, 2386, 2374, 2362, 2350, 2338,
 /*   120 */  2326, 2314, 2302, 2289, 2277, 2265, 2253, 2241, 2228, 2216,
 /*   130 */  2202,  844,  867,  354,  309,  157,   14,  979,  446, -169,
 /*   140 */   -98,  606,  -18,  -47,  510,   34,  322,  255,  177,   28,
 /*   150 */   139,  117,  618,  667,  649,  568,  499,  481,  426,  492,
 /*   160 */   361,  439,  351,  249,  423,  396,  332, 1055, 1046, 1045,
 /*   170 */  1042, 1030, 1015,  992, 1066, 1065, 1062, 1050, 1049, 1048,
 /*   180 */  1047, 1039, 1038, 1035,  900, 1009,  973,  911,  888,  968,
 /*   190 */  1034,  960,  962,  959,  955,  961,  957,  946, 1007,  941,
 /*   200 */   940,  936,  924,  916,  909,  932,  926,  934,  906,  890,
 /*   210 */   930,  925,  923,  915,  913,  912,  905,  904,  885,  895,
 /*   220 */   887,  879,  880,  856,  884,  878,  868,  863,  855,  848,
 /*   230 */   857,  824,  847,  839,  869,  819,  785,  795,  772,  784,
 /*   240 */   768,  833,  765,  758,  812,  803,  741,  797,  779,  764,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   687, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*    10 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*    20 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*    30 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*    40 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*    50 */  1041, 1041, 1041, 1041,  882,  881,  895,  896, 1041, 1041,
 /*    60 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*    70 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*    80 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*    90 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*   100 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*   110 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*   120 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*   130 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*   140 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*   150 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*   160 */  1041, 1041, 1041, 1041, 1041, 1041, 1041,  992,  994,  994,
 /*   170 */   994,  994, 1000,  992, 1041, 1041, 1041, 1041, 1041, 1041,
 /*   180 */  1041, 1041, 1041, 1041, 1041, 1041,  992, 1041, 1041, 1000,
 /*   190 */  1041, 1000,  998, 1000,  998,  994,  996,  992, 1041, 1000,
 /*   200 */   998,  994,  996,  992, 1041, 1000,  998,  994,  996,  992,
 /*   210 */  1000,  998, 1000,  998, 1000,  998, 1000,  998, 1000,  998,
 /*   220 */   994,  996,  992, 1000,  998,  994,  996,  992, 1000,  998,
 /*   230 */   994,  996, 1041, 1041, 1041, 1041, 1000,  998,  992, 1000,
 /*   240 */   998, 1041, 1000,  998, 1041, 1041,  992, 1041, 1041, 1041,
 /*   250 */  1041, 1041, 1041, 1041, 1041, 1041, 1041,  839,  839, 1041,
 /*   260 */  1041, 1041, 1041, 1041, 1041, 1041, 1041,  748, 1041,  993,
 /*   270 */   995,  984,  981,  875, 1041, 1041, 1041, 1041,  999,  991,
 /*   280 */  1041, 1041, 1041,  872,  793,  849,  861,  860,  859,  858,
 /*   290 */   857,  856,  855,  884,  883,  810,  897,  898,  864,  721,
 /*   300 */   722,  825,  824,  823,  822,  821,  820,  819,  841,  838,
 /*   310 */   837,  836,  835,  834,  833,  832,  831,  830,  829,  828,
 /*   320 */   827,  826, 1041, 1041,  718,  717,  874,  873, 1041,  798,
 /*   330 */   799,  851,  850,  775,  774,  812,  811,  890,  788,  789,
 /*   340 */   750,  749,  755,  754,  760,  759,  734,  735, 1041, 1041,
 /*   350 */   794, 1041, 1041,  961,  965,  964,  962, 1041, 1041, 1041,
 /*   360 */  1041, 1041, 1041, 1041, 1041, 1041,  910, 1041, 1041, 1041,
 /*   370 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*   380 */  1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041, 1041,
 /*   390 */  1041, 1041,  743,  871,  870, 1041, 1041, 1041, 1041, 1041,
 /*   400 */   963, 1041,  951,  928,  930, 1041, 1041,  943,  926, 1041,
 /*   410 */  1041, 1041,  925,  916,  917, 1041, 1041, 1041, 1041, 1041,
 /*   420 */  1041, 1041,  848,  847,  839,  809,  808, 1041, 1041,  862,
 /*   430 */   720,  716,  713,  710, 1041, 1040, 1039, 1038, 1037, 1036,
 /*   440 */  1035, 1034, 1033, 1032, 1031, 1030, 1029, 1028, 1027, 1026,
 /*   450 */  1025, 1020, 1019, 1021, 1018, 1017, 1016, 1015, 1014, 1013,
 /*   460 */  1012, 1011, 1010, 1009, 1008, 1007, 1006, 1005, 1004, 1003,
 /*   470 */  1002, 1001,  977,  976,  983,  982,  990,  989,  986,  985,
 /*   480 */   980,  988,  866,  868,  869,  867,  865,  746,  979,  978,
 /*   490 */   972,  971,  973,  970,  975,  974,  969,  967,  966,  968,
 /*   500 */   960,  955,  958,  959,  957,  956,  954,  946,  949,  953,
 /*   510 */   952,  950,  948,  947,  945,  921,  929,  931,  944,  942,
 /*   520 */   941,  940,  939,  938,  937,  936,  935,  934,  933,  932,
 /*   530 */   927,  909,  908,  924,  923,  919,  918,  915,  914,  913,
 /*   540 */   713,  912,  911,  800,  802,  801,  797,  796,  795,  794,
 /*   550 */   922,  920,  900,  903,  904,  907,  906,  905,  902,  901,
 /*   560 */   899, 1024, 1023, 1022,  843,  845,  854,  853,  852,  846,
 /*   570 */   844,  842,  773,  772,  771,  770,  769,  768,  778,  777,
 /*   580 */   776,  767,  766,  765,  764,  997,  804,  806,  877,  880,
 /*   590 */   879,  878,  876,  818,  817,  816,  815,  814,  813,  807,
 /*   600 */   805,  803,  746,  893,  892,  891,  890,  889,  840,  863,
 /*   610 */   790,  792,  791,  787,  786,  785,  784,  783,  782,  781,
 /*   620 */   780,  779,  719,  887,  886,  888,  885,  753,  752,  751,
 /*   630 */   758,  757,  756,  748,  747,  746,  745,  744,  743,  763,
 /*   640 */   762,  761,  742,  741,  740,  739,  736,  738,  737,  733,
 /*   650 */   732,  731,  730,  729,  728,  727,  726,  725,  724,  723,
 /*   660 */   715,  714,  712,  711,  709,  705,  704,  708,  707,  706,
 /*   670 */   703,  702,  701,  700,  699,  698,  697,  696,  695,  694,
 /*   680 */   693,  692,  691,  690,  689,  688,
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
    0,  /*     CARROT => nothing */
    0,  /*     UMINUS => nothing */
    0,  /*     PREC_4 => nothing */
    0,  /*     PREC_3 => nothing */
    0,  /*     PREC_2 => nothing */
    0,  /*     PREC_1 => nothing */
    0,  /*     PREC_0 => nothing */
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
  "CARROT",        "UMINUS",        "PREC_4",        "PREC_3",      
  "PREC_2",        "PREC_1",        "PREC_0",        "EOF",         
  "ERR_IO",        "ERR_UNKNOWN_SYMBOL",  "ERR_UNTERMINATED_STRING",  "ERR_UNTERMINATED_ASP",
  "ERR_UNTERMINATED_LUA",  "ERR_UNTERMINATED_F2LP",  "ERR_UNTERMINATED_BLK_COMMENT",  "ERR_SYNTAX",  
  "ERR_PAREN_MISMATCH",  "ARG",           "NOOP",          "CONSTANT_ID", 
  "VARIABLE_ID",   "OBJECT_ID",     "HIDE",          "OBSERVED",    
  "error",         "start",         "statement_lst",  "statement",   
  "stmt_macro_def",  "stmt_constant_def",  "stmt_object_def",  "stmt_variable_def",
  "stmt_sort_def",  "stmt_code_blk",  "stmt_law",      "stmt_show",   
  "stmt_hide",     "stmt_noconcurrency",  "stmt_strong_noconcurrency",  "stmt_maxafvalue",
  "stmt_maxadditive",  "stmt_query",    "base_elem",     "base_elem_no_const",
  "constant",      "object",        "object_nullary",  "variable",    
  "lua",           "undeclared",    "term_lst",      "term",        
  "constant_one_const",  "term_no_const_lst",  "term_no_const",  "term_strong", 
  "term_strong_candidate",  "term_no_const_strong",  "num_range",     "term_numeric",
  "formula",       "formula_base",  "comparison",    "atomic_formula",
  "formula_quant",  "formula_card",  "formula_no_const",  "formula_no_const_base",
  "comparison_no_const",  "atomic_formula_one_const",  "formula_temporal",  "quant_lst",   
  "quant_op",      "card_var_lst",  "card_var_lst_inner",  "head_formula",
  "formula_smpl_card",  "macro_def_lst",  "macro_bnd",     "macro_args",  
  "macro_arg",     "sort_lst",      "sort",          "sort_id_nr",  
  "sort_nr",       "sort_id",       "constant_bnd_lst",  "constant_bnd",
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
 /*  26 */ "object ::= object_nullary",
 /*  27 */ "object_nullary ::= OBJECT_ID",
 /*  28 */ "variable ::= VARIABLE_ID",
 /*  29 */ "lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  30 */ "lua ::= AT_IDENTIFIER",
 /*  31 */ "term_lst ::= term",
 /*  32 */ "term_lst ::= term_lst COMMA term",
 /*  33 */ "constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R",
 /*  34 */ "constant_one_const ::= CONSTANT_ID",
 /*  35 */ "term_no_const_lst ::= term_no_const",
 /*  36 */ "term_no_const_lst ::= term_no_const_lst COMMA term_no_const",
 /*  37 */ "term ::= base_elem",
 /*  38 */ "term ::= INTEGER",
 /*  39 */ "term ::= STRING_LITERAL",
 /*  40 */ "term ::= PAREN_L term PAREN_R",
 /*  41 */ "term ::= TRUE",
 /*  42 */ "term ::= FALSE",
 /*  43 */ "term ::= MAXSTEP",
 /*  44 */ "term ::= MAXADDITIVE",
 /*  45 */ "term ::= MAXAFVALUE",
 /*  46 */ "term ::= DASH term",
 /*  47 */ "term ::= ABS term",
 /*  48 */ "term ::= term DASH term",
 /*  49 */ "term ::= term PLUS term",
 /*  50 */ "term ::= term STAR term",
 /*  51 */ "term ::= term INT_DIV term",
 /*  52 */ "term ::= term MOD term",
 /*  53 */ "term_strong ::= base_elem_no_const",
 /*  54 */ "term_strong ::= INTEGER",
 /*  55 */ "term_strong ::= STRING_LITERAL",
 /*  56 */ "term_strong ::= PAREN_L term_strong PAREN_R",
 /*  57 */ "term_strong ::= MAXSTEP",
 /*  58 */ "term_strong ::= MAXADDITIVE",
 /*  59 */ "term_strong ::= MAXAFVALUE",
 /*  60 */ "term_strong ::= DASH term_strong",
 /*  61 */ "term_strong ::= ABS term",
 /*  62 */ "term_strong_candidate ::= DASH constant",
 /*  63 */ "term_strong ::= term_strong_candidate DASH term",
 /*  64 */ "term_strong ::= term_strong_candidate PLUS term",
 /*  65 */ "term_strong ::= term_strong_candidate STAR term",
 /*  66 */ "term_strong ::= term_strong_candidate INT_DIV term",
 /*  67 */ "term_strong ::= term_strong_candidate MOD term",
 /*  68 */ "term_strong ::= constant DASH term",
 /*  69 */ "term_strong ::= constant PLUS term",
 /*  70 */ "term_strong ::= constant STAR term",
 /*  71 */ "term_strong ::= constant INT_DIV term",
 /*  72 */ "term_strong ::= constant MOD term",
 /*  73 */ "term_strong ::= term_strong DASH term",
 /*  74 */ "term_strong ::= term_strong PLUS term",
 /*  75 */ "term_strong ::= term_strong STAR term",
 /*  76 */ "term_strong ::= term_strong INT_DIV term",
 /*  77 */ "term_strong ::= term_strong MOD term",
 /*  78 */ "term_no_const_strong ::= base_elem_no_const",
 /*  79 */ "term_no_const_strong ::= INTEGER",
 /*  80 */ "term_no_const_strong ::= STRING_LITERAL",
 /*  81 */ "term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R",
 /*  82 */ "term_no_const_strong ::= MAXSTEP",
 /*  83 */ "term_no_const_strong ::= MAXADDITIVE",
 /*  84 */ "term_no_const_strong ::= MAXAFVALUE",
 /*  85 */ "term_no_const_strong ::= constant",
 /*  86 */ "term_no_const_strong ::= DASH term_no_const_strong",
 /*  87 */ "term_no_const_strong ::= ABS term_no_const",
 /*  88 */ "term_no_const_strong ::= term_no_const_strong DASH term_no_const",
 /*  89 */ "term_no_const_strong ::= term_no_const_strong PLUS term_no_const",
 /*  90 */ "term_no_const_strong ::= term_no_const_strong STAR term_no_const",
 /*  91 */ "term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const",
 /*  92 */ "term_no_const_strong ::= term_no_const_strong MOD term_no_const",
 /*  93 */ "term_no_const ::= base_elem_no_const",
 /*  94 */ "term_no_const ::= INTEGER",
 /*  95 */ "term_no_const ::= STRING_LITERAL",
 /*  96 */ "term_no_const ::= PAREN_L term_no_const PAREN_R",
 /*  97 */ "term_no_const ::= TRUE",
 /*  98 */ "term_no_const ::= FALSE",
 /*  99 */ "term_no_const ::= constant",
 /* 100 */ "term_no_const ::= DASH term_no_const",
 /* 101 */ "term_no_const ::= ABS term_no_const",
 /* 102 */ "term_no_const ::= term_no_const DASH term_no_const",
 /* 103 */ "term_no_const ::= term_no_const PLUS term_no_const",
 /* 104 */ "term_no_const ::= term_no_const STAR term_no_const",
 /* 105 */ "term_no_const ::= term_no_const INT_DIV term_no_const",
 /* 106 */ "term_no_const ::= term_no_const MOD term_no_const",
 /* 107 */ "num_range ::= term_numeric DBL_PERIOD term_numeric",
 /* 108 */ "term_numeric ::= INTEGER",
 /* 109 */ "term_numeric ::= PAREN_L term_numeric PAREN_R",
 /* 110 */ "term_numeric ::= DASH term_numeric",
 /* 111 */ "term_numeric ::= ABS term_numeric",
 /* 112 */ "term_numeric ::= term_numeric DASH term_numeric",
 /* 113 */ "term_numeric ::= term_numeric PLUS term_numeric",
 /* 114 */ "term_numeric ::= term_numeric STAR term_numeric",
 /* 115 */ "term_numeric ::= term_numeric INT_DIV term_numeric",
 /* 116 */ "term_numeric ::= term_numeric MOD term_numeric",
 /* 117 */ "formula ::= formula_base",
 /* 118 */ "formula ::= PAREN_L formula PAREN_R",
 /* 119 */ "formula ::= NOT formula",
 /* 120 */ "formula ::= DASH formula",
 /* 121 */ "formula ::= formula AMP formula",
 /* 122 */ "formula ::= formula DBL_PLUS formula",
 /* 123 */ "formula ::= formula PIPE formula",
 /* 124 */ "formula ::= formula EQUIV formula",
 /* 125 */ "formula ::= formula IMPL formula",
 /* 126 */ "formula ::= formula ARROW_RDASH formula",
 /* 127 */ "formula_base ::= comparison",
 /* 128 */ "formula_base ::= atomic_formula",
 /* 129 */ "formula_base ::= formula_quant",
 /* 130 */ "formula_base ::= formula_card",
 /* 131 */ "formula_base ::= TRUE",
 /* 132 */ "formula_base ::= FALSE",
 /* 133 */ "comparison ::= term_strong EQ term",
 /* 134 */ "comparison ::= term_strong DBL_EQ term",
 /* 135 */ "comparison ::= term_strong NEQ term",
 /* 136 */ "comparison ::= term_strong LTHAN term",
 /* 137 */ "comparison ::= term_strong GTHAN term",
 /* 138 */ "comparison ::= term_strong LTHAN_EQ term",
 /* 139 */ "comparison ::= term_strong GTHAN_EQ term",
 /* 140 */ "comparison ::= term_strong_candidate EQ term",
 /* 141 */ "comparison ::= term_strong_candidate DBL_EQ term",
 /* 142 */ "comparison ::= term_strong_candidate NEQ term",
 /* 143 */ "comparison ::= term_strong_candidate LTHAN term",
 /* 144 */ "comparison ::= term_strong_candidate GTHAN term",
 /* 145 */ "comparison ::= term_strong_candidate LTHAN_EQ term",
 /* 146 */ "comparison ::= term_strong_candidate GTHAN_EQ term",
 /* 147 */ "comparison ::= constant DBL_EQ term",
 /* 148 */ "comparison ::= constant NEQ term",
 /* 149 */ "comparison ::= constant LTHAN term",
 /* 150 */ "comparison ::= constant GTHAN term",
 /* 151 */ "comparison ::= constant LTHAN_EQ term",
 /* 152 */ "comparison ::= constant GTHAN_EQ term",
 /* 153 */ "atomic_formula ::= constant",
 /* 154 */ "atomic_formula ::= TILDE constant",
 /* 155 */ "atomic_formula ::= constant EQ term",
 /* 156 */ "formula_no_const ::= formula_no_const_base",
 /* 157 */ "formula_no_const ::= PAREN_L formula_no_const PAREN_R",
 /* 158 */ "formula_no_const ::= NOT formula_no_const",
 /* 159 */ "formula_no_const ::= DASH formula_no_const",
 /* 160 */ "formula_no_const ::= formula_no_const AMP formula_no_const",
 /* 161 */ "formula_no_const ::= formula_no_const DBL_PLUS formula_no_const",
 /* 162 */ "formula_no_const ::= formula_no_const PIPE formula_no_const",
 /* 163 */ "formula_no_const ::= formula_no_const EQUIV formula_no_const",
 /* 164 */ "formula_no_const ::= formula_no_const IMPL formula_no_const",
 /* 165 */ "formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const",
 /* 166 */ "formula_no_const_base ::= comparison_no_const",
 /* 167 */ "formula_no_const_base ::= TRUE",
 /* 168 */ "formula_no_const_base ::= FALSE",
 /* 169 */ "comparison_no_const ::= term_no_const_strong EQ term_no_const",
 /* 170 */ "comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const",
 /* 171 */ "comparison_no_const ::= term_no_const_strong NEQ term_no_const",
 /* 172 */ "comparison_no_const ::= term_no_const_strong LTHAN term_no_const",
 /* 173 */ "comparison_no_const ::= term_no_const_strong GTHAN term_no_const",
 /* 174 */ "comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const",
 /* 175 */ "comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const",
 /* 176 */ "atomic_formula_one_const ::= constant_one_const",
 /* 177 */ "atomic_formula_one_const ::= TILDE constant_one_const",
 /* 178 */ "atomic_formula_one_const ::= constant_one_const EQ term_no_const",
 /* 179 */ "formula_temporal ::= formula_base",
 /* 180 */ "formula_temporal ::= PAREN_L formula_temporal PAREN_R",
 /* 181 */ "formula_temporal ::= NOT formula_temporal",
 /* 182 */ "formula_temporal ::= DASH formula_temporal",
 /* 183 */ "formula_temporal ::= formula_temporal AMP formula_temporal",
 /* 184 */ "formula_temporal ::= formula_temporal DBL_PLUS formula_temporal",
 /* 185 */ "formula_temporal ::= formula_temporal PIPE formula_temporal",
 /* 186 */ "formula_temporal ::= formula_temporal EQUIV formula_temporal",
 /* 187 */ "formula_temporal ::= formula_temporal IMPL formula_temporal",
 /* 188 */ "formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal",
 /* 189 */ "formula_temporal ::= term_strong COLON formula_temporal",
 /* 190 */ "formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R",
 /* 191 */ "quant_lst ::= quant_op variable",
 /* 192 */ "quant_lst ::= quant_lst quant_op variable",
 /* 193 */ "quant_op ::= BIG_CONJ",
 /* 194 */ "quant_op ::= BIG_DISJ",
 /* 195 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 196 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 197 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 198 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 199 */ "card_var_lst ::= card_var_lst_inner PIPE",
 /* 200 */ "card_var_lst ::= PIPE",
 /* 201 */ "card_var_lst_inner ::= variable",
 /* 202 */ "card_var_lst_inner ::= card_var_lst_inner COMMA variable",
 /* 203 */ "head_formula ::= comparison",
 /* 204 */ "head_formula ::= atomic_formula",
 /* 205 */ "head_formula ::= formula_smpl_card",
 /* 206 */ "head_formula ::= TRUE",
 /* 207 */ "head_formula ::= FALSE",
 /* 208 */ "head_formula ::= DASH constant",
 /* 209 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 210 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 211 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 212 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 213 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 214 */ "macro_def_lst ::= macro_bnd",
 /* 215 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 216 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 217 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 218 */ "macro_args ::= macro_arg",
 /* 219 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 220 */ "macro_arg ::= POUND_INTEGER",
 /* 221 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 222 */ "sort_lst ::= sort",
 /* 223 */ "sort_lst ::= sort_lst COMMA sort",
 /* 224 */ "sort ::= sort_id_nr",
 /* 225 */ "sort ::= sort_id_nr STAR",
 /* 226 */ "sort ::= sort_id_nr CARROT",
 /* 227 */ "sort ::= sort_nr PLUS object_nullary",
 /* 228 */ "sort ::= sort_id PLUS object_nullary",
 /* 229 */ "sort ::= sort_id PLUS INTEGER",
 /* 230 */ "sort_id_nr ::= sort_id",
 /* 231 */ "sort_id_nr ::= sort_nr",
 /* 232 */ "sort_nr ::= num_range",
 /* 233 */ "sort_id ::= IDENTIFIER",
 /* 234 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 235 */ "constant_bnd_lst ::= constant_bnd",
 /* 236 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 237 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 238 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 239 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 240 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 241 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 242 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 243 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 244 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 245 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 246 */ "constant_dcl_type ::= ABACTION",
 /* 247 */ "constant_dcl_type ::= ACTION",
 /* 248 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 249 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 250 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 251 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 252 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 253 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 254 */ "constant_dcl_type ::= RIGID",
 /* 255 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 256 */ "constant_dcl_type ::= SDFLUENT",
 /* 257 */ "attrib_spec ::= ATTRIBUTE",
 /* 258 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 259 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 260 */ "object_bnd_lst ::= object_bnd",
 /* 261 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 262 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 263 */ "object_lst ::= object_spec",
 /* 264 */ "object_lst ::= object_lst COMMA object_spec",
 /* 265 */ "object_spec ::= IDENTIFIER",
 /* 266 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 267 */ "object_spec ::= num_range",
 /* 268 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 269 */ "variable_bnd_lst ::= variable_bnd",
 /* 270 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 271 */ "variable_bnd ::= variable_lst DBL_COLON sort_id",
 /* 272 */ "variable_lst ::= IDENTIFIER",
 /* 273 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 274 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 275 */ "sort_bnd_lst ::= sort_bnd",
 /* 276 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 277 */ "sort_bnd ::= sort_dcl_lst",
 /* 278 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 279 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 280 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 281 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 282 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 283 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 284 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 285 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 286 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 287 */ "show_lst ::= show_elem",
 /* 288 */ "show_lst ::= show_lst COMMA show_elem",
 /* 289 */ "show_elem ::= atomic_formula_one_const",
 /* 290 */ "stmt_noconcurrency ::= NOCONCURRENCY",
 /* 291 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY",
 /* 292 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD",
 /* 293 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD",
 /* 294 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 295 */ "query_lst ::= formula_temporal",
 /* 296 */ "query_lst ::= query_maxstep_decl",
 /* 297 */ "query_lst ::= query_label_decl",
 /* 298 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 299 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 300 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 301 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 302 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range",
 /* 303 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 304 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 305 */ "clause_if ::= IF formula",
 /* 306 */ "clause_if ::=",
 /* 307 */ "clause_after ::= AFTER formula",
 /* 308 */ "clause_after ::=",
 /* 309 */ "clause_ifcons ::= IFCONS formula",
 /* 310 */ "clause_ifcons ::=",
 /* 311 */ "clause_unless ::= UNLESS atomic_formula",
 /* 312 */ "clause_unless ::=",
 /* 313 */ "clause_where ::= WHERE formula_no_const",
 /* 314 */ "clause_where ::=",
 /* 315 */ "stmt_law ::= law_basic",
 /* 316 */ "stmt_law ::= law_caused",
 /* 317 */ "stmt_law ::= law_pcaused",
 /* 318 */ "stmt_law ::= law_impl",
 /* 319 */ "stmt_law ::= law_causes",
 /* 320 */ "stmt_law ::= law_increments",
 /* 321 */ "stmt_law ::= law_mcause",
 /* 322 */ "stmt_law ::= law_always",
 /* 323 */ "stmt_law ::= law_constraint",
 /* 324 */ "stmt_law ::= law_impossible",
 /* 325 */ "stmt_law ::= law_never",
 /* 326 */ "stmt_law ::= law_default",
 /* 327 */ "stmt_law ::= law_exogenous",
 /* 328 */ "stmt_law ::= law_inertial",
 /* 329 */ "stmt_law ::= law_nonexecutable",
 /* 330 */ "stmt_law ::= law_rigid",
 /* 331 */ "stmt_law ::= law_observed",
 /* 332 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 333 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 334 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 335 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 336 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 337 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 338 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 339 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 340 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 341 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 342 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 343 */ "law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 344 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 345 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 346 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 347 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 348 */ "law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD",
 /* 349 */ "stmt_code_blk ::= ASP_GR",
 /* 350 */ "stmt_code_blk ::= ASP_CP",
 /* 351 */ "stmt_code_blk ::= F2LP_GR",
 /* 352 */ "stmt_code_blk ::= F2LP_CP",
 /* 353 */ "stmt_code_blk ::= LUA_GR",
 /* 354 */ "stmt_code_blk ::= LUA_CP",
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
    case 112: /* CARROT */
    case 113: /* UMINUS */
    case 114: /* PREC_4 */
    case 115: /* PREC_3 */
    case 116: /* PREC_2 */
    case 117: /* PREC_1 */
    case 118: /* PREC_0 */
    case 119: /* EOF */
    case 120: /* ERR_IO */
    case 121: /* ERR_UNKNOWN_SYMBOL */
    case 122: /* ERR_UNTERMINATED_STRING */
    case 123: /* ERR_UNTERMINATED_ASP */
    case 124: /* ERR_UNTERMINATED_LUA */
    case 125: /* ERR_UNTERMINATED_F2LP */
    case 126: /* ERR_UNTERMINATED_BLK_COMMENT */
    case 127: /* ERR_SYNTAX */
    case 128: /* ERR_PAREN_MISMATCH */
    case 129: /* ARG */
    case 130: /* NOOP */
    case 131: /* CONSTANT_ID */
    case 132: /* VARIABLE_ID */
    case 133: /* OBJECT_ID */
    case 134: /* HIDE */
    case 135: /* OBSERVED */
{
#line 198 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));								
#line 2256 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 137: /* start */
    case 138: /* statement_lst */
    case 161: /* undeclared */
{
#line 208 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2265 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 139: /* statement */
    case 145: /* stmt_code_blk */
    case 146: /* stmt_law */
    case 147: /* stmt_show */
    case 148: /* stmt_hide */
    case 151: /* stmt_maxafvalue */
    case 152: /* stmt_maxadditive */
{
#line 212 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy334));								
#line 2278 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_macro_def */
{
#line 233 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy373));								
#line 2285 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_constant_def */
{
#line 235 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy353));								
#line 2292 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_object_def */
{
#line 237 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy172));								
#line 2299 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_variable_def */
{
#line 239 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy43));								
#line 2306 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 144: /* stmt_sort_def */
{
#line 241 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy339));								
#line 2313 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_noconcurrency */
{
#line 251 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy451));								
#line 2320 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 150: /* stmt_strong_noconcurrency */
{
#line 253 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy352));								
#line 2327 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* stmt_query */
{
#line 259 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy134));								
#line 2334 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 154: /* base_elem */
    case 155: /* base_elem_no_const */
    case 163: /* term */
    case 166: /* term_no_const */
    case 167: /* term_strong */
    case 168: /* term_strong_candidate */
    case 169: /* term_no_const_strong */
{
#line 293 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy113));								
#line 2347 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* constant */
    case 164: /* constant_one_const */
{
#line 297 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy357));								
#line 2355 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 157: /* object */
    case 158: /* object_nullary */
{
#line 299 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy398));								
#line 2363 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* variable */
{
#line 303 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy313));								
#line 2370 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 160: /* lua */
{
#line 305 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy105));								
#line 2377 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 162: /* term_lst */
    case 165: /* term_no_const_lst */
{
#line 309 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy67));								
#line 2385 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 170: /* num_range */
{
#line 636 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy475));								
#line 2392 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* term_numeric */
{
#line 638 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy468));								
#line 2399 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 172: /* formula */
    case 173: /* formula_base */
    case 174: /* comparison */
    case 177: /* formula_card */
    case 178: /* formula_no_const */
    case 179: /* formula_no_const_base */
    case 180: /* comparison_no_const */
    case 182: /* formula_temporal */
    case 187: /* head_formula */
{
#line 699 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy374));								
#line 2414 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 175: /* atomic_formula */
    case 181: /* atomic_formula_one_const */
{
#line 705 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy6));								
#line 2422 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 176: /* formula_quant */
{
#line 707 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy155));								
#line 2429 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 183: /* quant_lst */
{
#line 903 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy358));								
#line 2436 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 184: /* quant_op */
{
#line 905 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2443 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* card_var_lst */
    case 186: /* card_var_lst_inner */
{
#line 942 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy279));								
#line 2451 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 188: /* formula_smpl_card */
{
#line 1018 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy73));								
#line 2458 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 189: /* macro_def_lst */
{
#line 1058 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy381));                              
#line 2465 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 190: /* macro_bnd */
{
#line 1060 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy83));                              
#line 2472 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 191: /* macro_args */
{
#line 1062 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy198));                              
#line 2479 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 192: /* macro_arg */
{
#line 1064 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy340));                              
#line 2486 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 193: /* sort_lst */
{
#line 1154 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy151));							
#line 2493 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 194: /* sort */
    case 195: /* sort_id_nr */
    case 196: /* sort_nr */
    case 197: /* sort_id */
{
#line 1156 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2503 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 198: /* constant_bnd_lst */
    case 199: /* constant_bnd */
{
#line 1296 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy77));									
#line 2511 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* constant_dcl_lst */
{
#line 1300 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy316));									
#line 2518 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* constant_dcl_type */
{
#line 1302 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2525 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* attrib_spec */
{
#line 1304 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2532 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* object_bnd_lst */
{
#line 1639 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy84));									
#line 2539 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* object_bnd */
{
#line 1641 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy180));									
#line 2546 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* object_lst */
    case 206: /* object_spec */
{
#line 1643 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy299));									
#line 2554 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 207: /* variable_bnd_lst */
    case 208: /* variable_bnd */
{
#line 1753 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy326));									
#line 2562 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 209: /* variable_lst */
{
#line 1757 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy58));									
#line 2569 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* sort_bnd_lst */
    case 211: /* sort_bnd */
    case 212: /* sort_dcl_lst */
{
#line 1830 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy233));									
#line 2578 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* show_lst */
{
#line 1934 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy345));									
#line 2585 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* show_elem */
    case 222: /* clause_unless */
{
#line 1936 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy6));									
#line 2593 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* query_lst */
{
#line 2081 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy165).l); DEALLOC((yypminor->yy165).maxstep); DEALLOC((yypminor->yy165).label);	
#line 2600 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* query_maxstep_decl */
{
#line 2083 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy234));												
#line 2607 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 218: /* query_label_Decl */
{
#line 2085 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2614 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* clause_if */
    case 220: /* clause_after */
    case 221: /* clause_ifcons */
    case 223: /* clause_where */
{
#line 2235 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy374));									
#line 2624 "bcplus/parser/detail/lemon_parser.c"
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
#line 2276 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy334));									
#line 2647 "bcplus/parser/detail/lemon_parser.c"
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
  { 137, 2 },
  { 138, 0 },
  { 138, 2 },
  { 138, 2 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 139, 1 },
  { 154, 1 },
  { 154, 1 },
  { 155, 1 },
  { 155, 1 },
  { 155, 1 },
  { 156, 4 },
  { 156, 1 },
  { 157, 4 },
  { 157, 1 },
  { 158, 1 },
  { 159, 1 },
  { 160, 4 },
  { 160, 1 },
  { 162, 1 },
  { 162, 3 },
  { 164, 4 },
  { 164, 1 },
  { 165, 1 },
  { 165, 3 },
  { 163, 1 },
  { 163, 1 },
  { 163, 1 },
  { 163, 3 },
  { 163, 1 },
  { 163, 1 },
  { 163, 1 },
  { 163, 1 },
  { 163, 1 },
  { 163, 2 },
  { 163, 2 },
  { 163, 3 },
  { 163, 3 },
  { 163, 3 },
  { 163, 3 },
  { 163, 3 },
  { 167, 1 },
  { 167, 1 },
  { 167, 1 },
  { 167, 3 },
  { 167, 1 },
  { 167, 1 },
  { 167, 1 },
  { 167, 2 },
  { 167, 2 },
  { 168, 2 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 167, 3 },
  { 169, 1 },
  { 169, 1 },
  { 169, 1 },
  { 169, 3 },
  { 169, 1 },
  { 169, 1 },
  { 169, 1 },
  { 169, 1 },
  { 169, 2 },
  { 169, 2 },
  { 169, 3 },
  { 169, 3 },
  { 169, 3 },
  { 169, 3 },
  { 169, 3 },
  { 166, 1 },
  { 166, 1 },
  { 166, 1 },
  { 166, 3 },
  { 166, 1 },
  { 166, 1 },
  { 166, 1 },
  { 166, 2 },
  { 166, 2 },
  { 166, 3 },
  { 166, 3 },
  { 166, 3 },
  { 166, 3 },
  { 166, 3 },
  { 170, 3 },
  { 171, 1 },
  { 171, 3 },
  { 171, 2 },
  { 171, 2 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 171, 3 },
  { 172, 1 },
  { 172, 3 },
  { 172, 2 },
  { 172, 2 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 173, 1 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 174, 3 },
  { 175, 1 },
  { 175, 2 },
  { 175, 3 },
  { 178, 1 },
  { 178, 3 },
  { 178, 2 },
  { 178, 2 },
  { 178, 3 },
  { 178, 3 },
  { 178, 3 },
  { 178, 3 },
  { 178, 3 },
  { 178, 3 },
  { 179, 1 },
  { 179, 1 },
  { 179, 1 },
  { 180, 3 },
  { 180, 3 },
  { 180, 3 },
  { 180, 3 },
  { 180, 3 },
  { 180, 3 },
  { 180, 3 },
  { 181, 1 },
  { 181, 2 },
  { 181, 3 },
  { 182, 1 },
  { 182, 3 },
  { 182, 2 },
  { 182, 2 },
  { 182, 3 },
  { 182, 3 },
  { 182, 3 },
  { 182, 3 },
  { 182, 3 },
  { 182, 3 },
  { 182, 3 },
  { 176, 5 },
  { 183, 2 },
  { 183, 3 },
  { 184, 1 },
  { 184, 1 },
  { 177, 4 },
  { 177, 5 },
  { 177, 5 },
  { 177, 6 },
  { 185, 2 },
  { 185, 1 },
  { 186, 1 },
  { 186, 3 },
  { 187, 1 },
  { 187, 1 },
  { 187, 1 },
  { 187, 1 },
  { 187, 1 },
  { 187, 2 },
  { 188, 4 },
  { 188, 5 },
  { 188, 5 },
  { 188, 6 },
  { 140, 4 },
  { 189, 1 },
  { 189, 3 },
  { 190, 6 },
  { 190, 3 },
  { 191, 1 },
  { 191, 3 },
  { 192, 1 },
  { 192, 1 },
  { 193, 1 },
  { 193, 3 },
  { 194, 1 },
  { 194, 2 },
  { 194, 2 },
  { 194, 3 },
  { 194, 3 },
  { 194, 3 },
  { 195, 1 },
  { 195, 1 },
  { 196, 1 },
  { 197, 1 },
  { 141, 4 },
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
  { 142, 4 },
  { 203, 1 },
  { 203, 3 },
  { 204, 3 },
  { 205, 1 },
  { 205, 3 },
  { 206, 1 },
  { 206, 4 },
  { 206, 1 },
  { 143, 4 },
  { 207, 1 },
  { 207, 3 },
  { 208, 3 },
  { 209, 1 },
  { 209, 3 },
  { 144, 4 },
  { 210, 1 },
  { 210, 3 },
  { 211, 1 },
  { 211, 3 },
  { 211, 3 },
  { 211, 3 },
  { 212, 1 },
  { 212, 3 },
  { 147, 4 },
  { 147, 4 },
  { 148, 4 },
  { 148, 4 },
  { 213, 1 },
  { 213, 3 },
  { 214, 1 },
  { 149, 1 },
  { 150, 1 },
  { 151, 5 },
  { 152, 5 },
  { 153, 4 },
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
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
  { 146, 1 },
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
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
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
#line 214 "bcplus/parser/detail/lemon_parser.y"
{
  yy_destructor(yypParser,119,&yymsp[0].minor);
}
#line 3307 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 219 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy334;
			yymsp[0].minor.yy334  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3316 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 262 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy373; }
#line 3321 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy353; }
#line 3326 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy172; }
#line 3331 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy43; }
#line 3336 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy339; }
#line 3341 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy334; }
#line 3351 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 271 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy451; }
#line 3356 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy352; }
#line 3361 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 275 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy134; }
#line 3366 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 319 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy357; }
#line 3371 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 37: /* term ::= base_elem */ yytestcase(yyruleno==37);
      case 53: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==53);
      case 78: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==78);
      case 93: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==93);
#line 320 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy113; }
#line 3380 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy398;	}
#line 3385 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 323 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy313; }
#line 3390 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 324 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy105; }
#line 3395 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 33: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==33);
#line 378 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy357, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3401 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 34: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==34);
#line 379 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy357, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3407 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 380 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy398, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3412 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* object ::= object_nullary */
#line 381 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy398 = yymsp[0].minor.yy398; }
#line 3417 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object_nullary ::= OBJECT_ID */
#line 382 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy398, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3422 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* variable ::= VARIABLE_ID */
#line 385 "bcplus/parser/detail/lemon_parser.y"
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
#line 3437 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 396 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy105, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0); }
#line 3442 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* lua ::= AT_IDENTIFIER */
#line 397 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy105, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3447 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* term_lst ::= term */
      case 35: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==35);
#line 402 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy67 = new TermList();
			yygotominor.yy67->push_back(yymsp[0].minor.yy113);
		}
#line 3456 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* term_lst ::= term_lst COMMA term */
      case 36: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==36);
#line 408 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy67 = yymsp[-2].minor.yy67;
			yymsp[-2].minor.yy67->push_back(yymsp[0].minor.yy113);
		  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 3466 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 38: /* term ::= INTEGER */
      case 54: /* term_strong ::= INTEGER */ yytestcase(yyruleno==54);
      case 79: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==79);
      case 94: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==94);
#line 507 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy113, yymsp[0].minor.yy0);	}
#line 3474 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 39: /* term ::= STRING_LITERAL */
      case 41: /* term ::= TRUE */ yytestcase(yyruleno==41);
      case 42: /* term ::= FALSE */ yytestcase(yyruleno==42);
      case 55: /* term_strong ::= STRING_LITERAL */ yytestcase(yyruleno==55);
      case 80: /* term_no_const_strong ::= STRING_LITERAL */ yytestcase(yyruleno==80);
      case 95: /* term_no_const ::= STRING_LITERAL */ yytestcase(yyruleno==95);
      case 97: /* term_no_const ::= TRUE */ yytestcase(yyruleno==97);
      case 98: /* term_no_const ::= FALSE */ yytestcase(yyruleno==98);
#line 508 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy113, yymsp[0].minor.yy0); }
#line 3486 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 40: /* term ::= PAREN_L term PAREN_R */
      case 56: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==56);
      case 81: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==81);
      case 96: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==96);
#line 509 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy113, yymsp[-2].minor.yy0, yymsp[-1].minor.yy113, yymsp[0].minor.yy0); }
#line 3494 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 43: /* term ::= MAXSTEP */
      case 57: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==57);
      case 82: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==82);
#line 512 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3501 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 44: /* term ::= MAXADDITIVE */
      case 58: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==58);
      case 83: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==83);
#line 513 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3508 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 45: /* term ::= MAXAFVALUE */
      case 59: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==84);
#line 514 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3515 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 46: /* term ::= DASH term */
      case 60: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==60);
      case 86: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==86);
      case 100: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==100);
#line 518 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, UnaryTerm::Operator::NEGATIVE); }
#line 3523 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 47: /* term ::= ABS term */
      case 61: /* term_strong ::= ABS term */ yytestcase(yyruleno==61);
      case 87: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==87);
      case 101: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==101);
#line 519 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, UnaryTerm::Operator::ABS); }
#line 3531 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 48: /* term ::= term DASH term */
      case 63: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==63);
      case 73: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==73);
      case 88: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==88);
      case 102: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==102);
#line 523 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MINUS); }
#line 3540 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= term PLUS term */
      case 64: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==64);
      case 74: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==74);
      case 89: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==89);
      case 103: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==103);
#line 524 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::PLUS); }
#line 3549 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= term STAR term */
      case 65: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==65);
      case 75: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==75);
      case 90: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==90);
      case 104: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==104);
#line 525 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::TIMES); }
#line 3558 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= term INT_DIV term */
      case 66: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==66);
      case 76: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==76);
      case 91: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==91);
      case 105: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==105);
#line 526 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::DIVIDE); }
#line 3567 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= term MOD term */
      case 67: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==67);
      case 77: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==77);
      case 92: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==92);
      case 106: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==106);
#line 527 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MOD); }
#line 3576 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 62: /* term_strong_candidate ::= DASH constant */
#line 546 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy357, UnaryTerm::Operator::NEGATIVE); }
#line 3581 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 68: /* term_strong ::= constant DASH term */
#line 555 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MINUS); }
#line 3586 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 69: /* term_strong ::= constant PLUS term */
#line 556 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::PLUS); }
#line 3591 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 70: /* term_strong ::= constant STAR term */
#line 557 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::TIMES); }
#line 3596 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 71: /* term_strong ::= constant INT_DIV term */
#line 558 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::DIVIDE); }
#line 3601 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 72: /* term_strong ::= constant MOD term */
#line 559 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MOD); }
#line 3606 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 85: /* term_no_const_strong ::= constant */
#line 581 "bcplus/parser/detail/lemon_parser.y"
{
		// error handling for constants so they don'yygotominor.yy113 default to undeclared identifiers
		yygotominor.yy113 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy357;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy357->beginLoc());
		YYERROR;
	}
#line 3617 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 99: /* term_no_const ::= constant */
#line 611 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy113 default to undeclared identifiers
		yygotominor.yy113 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy357;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy357->beginLoc());
		YYERROR;
	}
#line 3628 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 107: /* num_range ::= term_numeric DBL_PERIOD term_numeric */
#line 641 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy468, r_ptr = yymsp[0].minor.yy468, s_ptr = yymsp[-1].minor.yy0;

	yygotominor.yy475 = new NumberRange(yymsp[-2].minor.yy468->val(), yymsp[0].minor.yy468->val(), yymsp[-2].minor.yy468->beginLoc(), yymsp[0].minor.yy468->endLoc());

}
#line 3638 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 108: /* term_numeric ::= INTEGER */
#line 649 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> i_ptr = yymsp[0].minor.yy0;

	yygotominor.yy468 = 0;
	try {
		yygotominor.yy468 = new Number(boost::lexical_cast<int>(*yymsp[0].minor.yy0->str()), yymsp[0].minor.yy0->beginLoc());

	} catch (boost::bad_lexical_cast const& e) {
		parser->_parse_error("INTERNAL ERROR: Failed to parse integer \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
		YYERROR;
	}
}
#line 3654 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 109: /* term_numeric ::= PAREN_L term_numeric PAREN_R */
#line 662 "bcplus/parser/detail/lemon_parser.y"
{ 
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy468 = yymsp[-1].minor.yy468;  
	yygotominor.yy468->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy468->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 3664 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 110: /* term_numeric ::= DASH term_numeric */
#line 682 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy468, yymsp[0].minor.yy468, -1 * yymsp[0].minor.yy468->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3670 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 111: /* term_numeric ::= ABS term_numeric */
#line 683 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy468, yymsp[0].minor.yy468, yymsp[0].minor.yy468->val() < 0 ? - yymsp[0].minor.yy468->val() : yymsp[0].minor.yy468->val());   yy_destructor(yypParser,111,&yymsp[-1].minor);
}
#line 3676 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 112: /* term_numeric ::= term_numeric DASH term_numeric */
#line 685 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() - yymsp[0].minor.yy468->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3682 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 113: /* term_numeric ::= term_numeric PLUS term_numeric */
#line 686 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() + yymsp[0].minor.yy468->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 3688 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 114: /* term_numeric ::= term_numeric STAR term_numeric */
#line 687 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() * yymsp[0].minor.yy468->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 3694 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 115: /* term_numeric ::= term_numeric INT_DIV term_numeric */
#line 688 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() / yymsp[0].minor.yy468->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 3700 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 116: /* term_numeric ::= term_numeric MOD term_numeric */
#line 689 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() % yymsp[0].minor.yy468->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 3706 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 117: /* formula ::= formula_base */
      case 156: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==156);
      case 179: /* formula_temporal ::= formula_base */ yytestcase(yyruleno==179);
#line 745 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy374;				}
#line 3713 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 118: /* formula ::= PAREN_L formula PAREN_R */
      case 157: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==157);
      case 180: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==180);
#line 746 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[-1].minor.yy374; yygotominor.yy374->parens(true); 	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3722 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 119: /* formula ::= NOT formula */
      case 158: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==158);
      case 181: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==181);
#line 747 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 3729 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 120: /* formula ::= DASH formula */
      case 159: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==159);
      case 182: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==182);
#line 748 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 3736 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 121: /* formula ::= formula AMP formula */
      case 160: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==160);
      case 183: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==183);
#line 749 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy374, yymsp[0].minor.yy374, yymsp[-2].minor.yy374->beginLoc(), yymsp[0].minor.yy374->endLoc());   yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3744 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 122: /* formula ::= formula DBL_PLUS formula */
      case 123: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==123);
      case 161: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==161);
      case 162: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==162);
      case 184: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==184);
      case 185: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==185);
#line 750 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::OR); }
#line 3754 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 124: /* formula ::= formula EQUIV formula */
      case 163: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==163);
      case 186: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==186);
#line 752 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::EQUIV); }
#line 3761 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 125: /* formula ::= formula IMPL formula */
      case 126: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==126);
      case 164: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==164);
      case 165: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==165);
      case 187: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==187);
      case 188: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==188);
#line 753 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::IMPL); }
#line 3771 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 127: /* formula_base ::= comparison */
      case 166: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==166);
      case 203: /* head_formula ::= comparison */ yytestcase(yyruleno==203);
#line 756 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy374; }
#line 3778 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 128: /* formula_base ::= atomic_formula */
      case 204: /* head_formula ::= atomic_formula */ yytestcase(yyruleno==204);
#line 757 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy6; }
#line 3784 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 129: /* formula_base ::= formula_quant */
#line 758 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy155; }
#line 3789 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* formula_base ::= formula_card */
#line 760 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy374 = yymsp[0].minor.yy374;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy374->beginLoc());
			YYERROR;
		}
	}
#line 3800 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 131: /* formula_base ::= TRUE */
      case 167: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==167);
      case 206: /* head_formula ::= TRUE */ yytestcase(yyruleno==206);
#line 767 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3807 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* formula_base ::= FALSE */
      case 168: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==168);
      case 207: /* head_formula ::= FALSE */ yytestcase(yyruleno==207);
#line 768 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3814 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* comparison ::= term_strong EQ term */
      case 140: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==140);
      case 169: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==169);
#line 770 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3822 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* comparison ::= term_strong DBL_EQ term */
      case 141: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==141);
      case 170: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==170);
#line 771 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3830 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* comparison ::= term_strong NEQ term */
      case 142: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==142);
      case 171: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==171);
#line 772 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3838 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* comparison ::= term_strong LTHAN term */
      case 143: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==143);
      case 172: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==172);
#line 773 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3846 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* comparison ::= term_strong GTHAN term */
      case 144: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==144);
      case 173: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==173);
#line 774 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3854 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* comparison ::= term_strong LTHAN_EQ term */
      case 145: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==145);
      case 174: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==174);
#line 775 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3862 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* comparison ::= term_strong GTHAN_EQ term */
      case 146: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==146);
      case 175: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==175);
#line 776 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 3870 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 147: /* comparison ::= constant DBL_EQ term */
#line 784 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3876 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 148: /* comparison ::= constant NEQ term */
#line 785 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3882 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 149: /* comparison ::= constant LTHAN term */
#line 786 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3888 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 150: /* comparison ::= constant GTHAN term */
#line 787 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3894 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 151: /* comparison ::= constant LTHAN_EQ term */
#line 788 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3900 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* comparison ::= constant GTHAN_EQ term */
#line 789 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 3906 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* atomic_formula ::= constant */
      case 176: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==176);
#line 819 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy6, yymsp[0].minor.yy357, "true"); }
#line 3912 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* atomic_formula ::= TILDE constant */
      case 177: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==177);
#line 820 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy6, yymsp[0].minor.yy357, "false");   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 3919 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* atomic_formula ::= constant EQ term */
      case 178: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==178);
#line 821 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = new AtomicFormula(yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());	  yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3926 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 189: /* formula_temporal ::= term_strong COLON formula_temporal */
#line 896 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy374, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy374); }
#line 3931 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 190: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
#line 908 "bcplus/parser/detail/lemon_parser.y"
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
#line 3948 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 191: /* quant_lst ::= quant_op variable */
#line 922 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy358 = new QuantifierFormula::QuantifierList();
		yygotominor.yy358->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy423, yymsp[0].minor.yy313));
	}
#line 3956 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 192: /* quant_lst ::= quant_lst quant_op variable */
#line 928 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy358 = yymsp[-2].minor.yy358;
		yygotominor.yy358->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy423, yymsp[0].minor.yy313));
	}
#line 3964 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 193: /* quant_op ::= BIG_CONJ */
#line 933 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 3970 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 194: /* quant_op ::= BIG_DISJ */
#line 934 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,92,&yymsp[0].minor);
}
#line 3976 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 195: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
#line 980 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, NULL);  }
#line 3981 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 196: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
#line 981 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, yymsp[-4].minor.yy113, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy374,  yymsp[0].minor.yy0, NULL);  }
#line 3986 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 197: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 982 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 3991 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 198: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 983 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, yymsp[-5].minor.yy113, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy374,  yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 3996 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 199: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 987 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy279 = yymsp[-1].minor.yy279;
	  yy_destructor(yypParser,99,&yymsp[0].minor);
}
#line 4004 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 200: /* card_var_lst ::= PIPE */
#line 991 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy279 = new CardinalityFormula::VariableList();
	  yy_destructor(yypParser,99,&yymsp[0].minor);
}
#line 4012 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 201: /* card_var_lst_inner ::= variable */
#line 996 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy313;
		yygotominor.yy279 = new CardinalityFormula::VariableList();
		yygotominor.yy279->push_back(yymsp[0].minor.yy313->symbol());
	}
#line 4021 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 202: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1003 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy313;
		yygotominor.yy279 = yymsp[-2].minor.yy279;
		yygotominor.yy279->push_back(yymsp[0].minor.yy313->symbol());
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4031 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 205: /* head_formula ::= formula_smpl_card */
#line 1023 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy374 = yymsp[0].minor.yy73;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy73->beginLoc());
			YYERROR;
		}
	}
#line 4042 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* head_formula ::= DASH constant */
#line 1033 "bcplus/parser/detail/lemon_parser.y"
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
#line 4058 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 209: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1046 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy6, yymsp[0].minor.yy0, NULL);  }
#line 4063 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1047 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, yymsp[-4].minor.yy113, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy6,  yymsp[0].minor.yy0, NULL);  }
#line 4068 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1048 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy6, yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4073 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1049 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, yymsp[-5].minor.yy113, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy6,  yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4078 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1068 "bcplus/parser/detail/lemon_parser.y"
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
#line 4108 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* macro_def_lst ::= macro_bnd */
#line 1096 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy381 = new MacroDeclaration::ElementList();
        yygotominor.yy381->push_back(yymsp[0].minor.yy83);
    }
#line 4116 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 215: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1102 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy381 = yymsp[-2].minor.yy381;
        yygotominor.yy381->push_back(yymsp[0].minor.yy83);
      yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4125 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1108 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy198;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy83 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy198);
      yy_destructor(yypParser,72,&yymsp[-4].minor);
  yy_destructor(yypParser,73,&yymsp[-2].minor);
  yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4139 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1117 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy83 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4150 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 218: /* macro_args ::= macro_arg */
#line 1125 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy198 = new MacroSymbol::ArgumentList();
        yygotominor.yy198->push_back(yymsp[0].minor.yy340->str());
        delete yymsp[0].minor.yy340;
    }
#line 4159 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 219: /* macro_args ::= macro_args COMMA macro_arg */
#line 1131 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy198 = yymsp[-2].minor.yy198;
        yygotominor.yy198->push_back(yymsp[0].minor.yy340->str());
        delete yymsp[0].minor.yy340;
      yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4169 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 220: /* macro_arg ::= POUND_INTEGER */
      case 221: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==221);
#line 1138 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy340 = yymsp[0].minor.yy0;
    }
#line 4177 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* sort_lst ::= sort */
#line 1165 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy151 = new ConstantSymbol::SortList();
		yygotominor.yy151->push_back(yymsp[0].minor.yy325);
	}
#line 4185 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 223: /* sort_lst ::= sort_lst COMMA sort */
#line 1170 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy151 = yymsp[-2].minor.yy151;
		yygotominor.yy151->push_back(yymsp[0].minor.yy325);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4194 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 224: /* sort ::= sort_id_nr */
      case 230: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==230);
      case 231: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==231);
#line 1225 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy325 = yymsp[0].minor.yy325; }
#line 4201 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 225: /* sort ::= sort_id_nr STAR */
#line 1226 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-1].minor.yy325, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, *yymsp[-1].minor.yy325->base() + "__plus_none_0", "none"); }
#line 4206 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 226: /* sort ::= sort_id_nr CARROT */
#line 1227 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-1].minor.yy325, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, *yymsp[-1].minor.yy325->base() + "__plus_unknown_0__", "unknown"); }
#line 4211 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 227: /* sort ::= sort_nr PLUS object_nullary */
      case 228: /* sort ::= sort_id PLUS object_nullary */ yytestcase(yyruleno==228);
#line 1229 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy325, yymsp[-2].minor.yy325, yymsp[-1].minor.yy0, yymsp[0].minor.yy398); }
#line 4217 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 229: /* sort ::= sort_id PLUS INTEGER */
#line 1232 "bcplus/parser/detail/lemon_parser.y"
{ ref_ptr<const Referenced> i_ptr = yymsp[0].minor.yy0; DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-2].minor.yy325, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, *yymsp[-2].minor.yy325->base() + "__plus_" + *yymsp[0].minor.yy0->str(), (*((std::string const*)yymsp[0].minor.yy0->str()))); }
#line 4222 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* sort_nr ::= num_range */
#line 1239 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy475;

		yygotominor.yy325 = NULL;

		if (!parser->lang()->support(Language::Feature::NUMRANGE_SORT)) {
			parser->_feature_error(Language::Feature::NUMRANGE_SORT, &yymsp[0].minor.yy475->beginLoc());
			YYERROR;
		}

		// X..Y becomes __sort_X_Y__
		std::string name = "__sort_" + boost::lexical_cast<std::string>(yymsp[0].minor.yy475->min()) + "__" + boost::lexical_cast<std::string>(yymsp[0].minor.yy475->max()) + "__";

		ref_ptr<SortSymbol::ObjectList> objs = new SortSymbol::ObjectList();

		// Generate the objects that it will have
		for (int i = yymsp[0].minor.yy475->min(); i <= yymsp[0].minor.yy475->max(); i++) {
			std::string obj_name = boost::lexical_cast<std::string>(i);
			ObjectSymbol const* sym = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(obj_name)));

			if (!sym) {
				yygotominor.yy325 = NULL;
				parser->_parse_error("An error occurred creating symbol \"" + obj_name + "/0\".", &yymsp[0].minor.yy475->beginLoc());
				YYERROR;
			}
			objs->insert(sym);
		}		

		// dynamically declare the sort
		yygotominor.yy325 = (SortSymbol*)parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name), objs));
		if (!yygotominor.yy325) {
				parser->_parse_error("An error occurred creating symbol \"" + name + "/0\".", &yymsp[0].minor.yy475->beginLoc());
				YYERROR;
		} 
	}
#line 4261 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 233: /* sort_id ::= IDENTIFIER */
#line 1276 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy325 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy325) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4274 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 234: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1307 "bcplus/parser/detail/lemon_parser.y"
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
#line 4293 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* constant_bnd_lst ::= constant_bnd */
#line 1324 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy77 = yymsp[0].minor.yy77;
	}
#line 4300 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 236: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1329 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy77;
		yygotominor.yy77 = yymsp[-2].minor.yy77;
		yygotominor.yy77->splice(yygotominor.yy77->end(), *yymsp[0].minor.yy77);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4310 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 237: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1349 "bcplus/parser/detail/lemon_parser.y"
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
#line 4329 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 238: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1362 "bcplus/parser/detail/lemon_parser.y"
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
#line 4344 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 239: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1373 "bcplus/parser/detail/lemon_parser.y"
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
#line 4359 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 240: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1384 "bcplus/parser/detail/lemon_parser.y"
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
#line 4388 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 241: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1408 "bcplus/parser/detail/lemon_parser.y"
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
#line 4469 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 242: /* constant_dcl_lst ::= IDENTIFIER */
#line 1484 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = new IdentifierDeclList();
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 4477 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 243: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1489 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = new IdentifierDeclList();
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy151));
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4487 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 244: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1494 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = yymsp[-2].minor.yy316;
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4496 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 245: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1499 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = yymsp[-5].minor.yy316;
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy151));
	  yy_destructor(yypParser,103,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4507 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 246: /* constant_dcl_type ::= ABACTION */
#line 1506 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4519 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 247: /* constant_dcl_type ::= ACTION */
#line 1515 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4531 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 248: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1524 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4543 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 249: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1533 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4555 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 250: /* constant_dcl_type ::= EXTERNALACTION */
#line 1542 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4567 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 251: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1551 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4579 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 252: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1560 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4591 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 253: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1569 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4603 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 254: /* constant_dcl_type ::= RIGID */
#line 1578 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4615 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 255: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1587 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4627 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 256: /* constant_dcl_type ::= SDFLUENT */
#line 1597 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4639 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 257: /* attrib_spec ::= ATTRIBUTE */
#line 1607 "bcplus/parser/detail/lemon_parser.y"
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
#line 4654 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 258: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1620 "bcplus/parser/detail/lemon_parser.y"
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
#line 4670 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 259: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1648 "bcplus/parser/detail/lemon_parser.y"
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
#line 4695 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 260: /* object_bnd_lst ::= object_bnd */
#line 1671 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy84 = new ObjectDeclaration::ElementList();
		yygotominor.yy84->push_back(yymsp[0].minor.yy180);
	}
#line 4703 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 261: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 1677 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy84 = yymsp[-2].minor.yy84;
		yygotominor.yy84->push_back(yymsp[0].minor.yy180);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4712 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 262: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 1683 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy180 = new ObjectDeclaration::Element(yymsp[0].minor.yy325, yymsp[-2].minor.yy299);
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4720 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 263: /* object_lst ::= object_spec */
#line 1688 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = yymsp[0].minor.yy299;
	}
#line 4727 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 264: /* object_lst ::= object_lst COMMA object_spec */
#line 1692 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = yymsp[-2].minor.yy299;
		yygotominor.yy299->splice(yygotominor.yy299->end(), *yymsp[0].minor.yy299);
		delete yymsp[0].minor.yy299;
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4737 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* object_spec ::= IDENTIFIER */
#line 1701 "bcplus/parser/detail/lemon_parser.y"
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
#line 4753 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1714 "bcplus/parser/detail/lemon_parser.y"
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
#line 4772 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 267: /* object_spec ::= num_range */
#line 1728 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = new ObjectDeclaration::Element::ObjectList();
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy475;

		// iterate over the range and add it to the list
		for (int i = yymsp[0].minor.yy475->min(); i <= yymsp[0].minor.yy475->max(); i++) {
			std::string name = boost::lexical_cast<std::string>(i);
			ref_ptr<const ObjectSymbol> o = (ObjectSymbol*)parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(name)));
			if (!o) {
				parser->_parse_error("INTERNAL ERROR: Could not create object symbol \"" + Symbol::genName(name, 0) + "\".", &yymsp[0].minor.yy475->beginLoc());
				YYERROR;
			} else {
				yygotominor.yy299->push_back(o);
			}
		}
	}
#line 4792 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 1760 "bcplus/parser/detail/lemon_parser.y"
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
#line 4823 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 269: /* variable_bnd_lst ::= variable_bnd */
#line 1789 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = yymsp[0].minor.yy326;
	}
#line 4830 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 270: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 1794 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = yymsp[-2].minor.yy326;
		yygotominor.yy326->splice(yygotominor.yy326->end(), *yymsp[0].minor.yy326);
		delete yymsp[0].minor.yy326;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4840 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 271: /* variable_bnd ::= variable_lst DBL_COLON sort_id */
#line 1801 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy58) {
			yygotominor.yy326->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy325));
		}
		delete yymsp[-2].minor.yy58;
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4853 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 272: /* variable_lst ::= IDENTIFIER */
#line 1811 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy58 = new TokenList();
		yygotominor.yy58->push_back(yymsp[0].minor.yy0);
	}
#line 4861 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 273: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 1816 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy58 = yymsp[-2].minor.yy58;
		yygotominor.yy58->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4870 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 274: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 1837 "bcplus/parser/detail/lemon_parser.y"
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
#line 4888 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 275: /* sort_bnd_lst ::= sort_bnd */
      case 277: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==277);
#line 1853 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[0].minor.yy233;
	}
#line 4896 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 276: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 1858 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[-2].minor.yy233;
		yygotominor.yy233->splice(yygotominor.yy233->end(), *yymsp[0].minor.yy233);
		delete yymsp[0].minor.yy233;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4906 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 278: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 1870 "bcplus/parser/detail/lemon_parser.y"
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
#line 4922 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 279: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 1882 "bcplus/parser/detail/lemon_parser.y"
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
#line 4937 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 280: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 1893 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[-1].minor.yy233;
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4946 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 281: /* sort_dcl_lst ::= IDENTIFIER */
#line 1898 "bcplus/parser/detail/lemon_parser.y"
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
#line 4963 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 282: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 1912 "bcplus/parser/detail/lemon_parser.y"
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
#line 4982 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 283: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 1939 "bcplus/parser/detail/lemon_parser.y"
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
#line 4998 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 284: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 1953 "bcplus/parser/detail/lemon_parser.y"
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
#line 5016 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 285: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 1970 "bcplus/parser/detail/lemon_parser.y"
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
#line 5032 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 286: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 1984 "bcplus/parser/detail/lemon_parser.y"
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
#line 5050 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* show_lst ::= show_elem */
#line 2002 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy345 = new ShowStatement::ElementList();
		yygotominor.yy345->push_back(yymsp[0].minor.yy6);
	}
#line 5058 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* show_lst ::= show_lst COMMA show_elem */
#line 2007 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy345 = yymsp[-2].minor.yy345;
		yygotominor.yy345->push_back(yymsp[0].minor.yy6);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5067 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* show_elem ::= atomic_formula_one_const */
#line 2012 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = yymsp[0].minor.yy6; }
#line 5072 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* stmt_noconcurrency ::= NOCONCURRENCY */
#line 2035 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy451, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5077 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY */
#line 2036 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy352, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5082 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD */
#line 2062 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy334, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy468, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5088 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD */
#line 2063 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy334, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy468, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5094 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 294: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2088 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy134 = NULL;
		ref_ptr<const Referenced> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, data_l_ptr = yymsp[-1].minor.yy165.l, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Referenced> data_maxstep_ptr = yymsp[-1].minor.yy165.maxstep, data_label_ptr = yymsp[-1].minor.yy165.label;

		int min = -1, max = -1;
		if (yymsp[-1].minor.yy165.maxstep) {
			min = yymsp[-1].minor.yy165.maxstep->min();
			max = yymsp[-1].minor.yy165.maxstep->max();
		}

		if (!parser->lang()->support(Language::Feature::DECL_QUERY)) {
			parser->_feature_error(Language::Feature::DECL_QUERY, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			bool good = true;

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
#line 5127 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* query_lst ::= formula_temporal */
#line 2120 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = NULL;
		yygotominor.yy165.label = NULL;

		yygotominor.yy165.l->push_back(yymsp[0].minor.yy374);
	}
#line 5138 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* query_lst ::= query_maxstep_decl */
#line 2129 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = yymsp[0].minor.yy234;
		yygotominor.yy165.label = NULL;
	}
#line 5147 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* query_lst ::= query_label_decl */
#line 2136 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = NULL;
		yygotominor.yy165.label = yymsp[0].minor.yy340;
	}
#line 5156 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2143 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy165 = yymsp[-2].minor.yy165;
		yymsp[-2].minor.yy165.l->push_back(yymsp[0].minor.yy374);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5165 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2149 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165 = yymsp[-2].minor.yy165;

		if (yygotominor.yy165.maxstep) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy234->beginLoc());
			delete yymsp[0].minor.yy234;
			YYERROR;
		} else {
			yygotominor.yy165.maxstep = yymsp[0].minor.yy234;
		}
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5181 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2162 "bcplus/parser/detail/lemon_parser.y"
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
#line 5197 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2188 "bcplus/parser/detail/lemon_parser.y"
{ 
	yygotominor.yy234 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, i_ptr = yymsp[0].minor.yy0;


	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {

		int max = -1;
		try {
			max = boost::lexical_cast<int>(*yymsp[0].minor.yy0->str());
			yygotominor.yy234 = new NumberRange(-1, max, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		} catch (boost::bad_lexical_cast const& e) {
			parser->_parse_error("INTERNAL ERROR: An error occurred extracting an integer from \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5222 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range */
#line 2209 "bcplus/parser/detail/lemon_parser.y"
{
	yygotominor.yy234 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, nr_ptr = yymsp[0].minor.yy475;

	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {
		yygotominor.yy234 = yymsp[0].minor.yy475;
		nr_ptr.release();
	}
  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5239 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 304: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==304);
#line 2223 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy340, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5246 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* clause_if ::= IF formula */
#line 2258 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_IF); 		}
#line 5251 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* clause_if ::= */
      case 308: /* clause_after ::= */ yytestcase(yyruleno==308);
      case 310: /* clause_ifcons ::= */ yytestcase(yyruleno==310);
      case 314: /* clause_where ::= */ yytestcase(yyruleno==314);
#line 2259 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = NULL; }
#line 5259 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* clause_after ::= AFTER formula */
#line 2260 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_AFTER);	}
#line 5264 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* clause_ifcons ::= IFCONS formula */
#line 2262 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_IFCONS); 	}
#line 5269 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* clause_unless ::= UNLESS atomic_formula */
#line 2264 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy6, yymsp[-1].minor.yy0, yymsp[0].minor.yy6, Language::Feature::CLAUSE_UNLESS); 	}
#line 5274 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* clause_unless ::= */
#line 2265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = NULL; }
#line 5279 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* clause_where ::= WHERE formula_no_const */
#line 2266 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_WHERE); 	}
#line 5284 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* stmt_law ::= law_basic */
      case 316: /* stmt_law ::= law_caused */ yytestcase(yyruleno==316);
      case 317: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==317);
      case 318: /* stmt_law ::= law_impl */ yytestcase(yyruleno==318);
      case 319: /* stmt_law ::= law_causes */ yytestcase(yyruleno==319);
      case 320: /* stmt_law ::= law_increments */ yytestcase(yyruleno==320);
      case 321: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==321);
      case 322: /* stmt_law ::= law_always */ yytestcase(yyruleno==322);
      case 323: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==323);
      case 324: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==324);
      case 325: /* stmt_law ::= law_never */ yytestcase(yyruleno==325);
      case 326: /* stmt_law ::= law_default */ yytestcase(yyruleno==326);
      case 327: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==327);
      case 328: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==328);
      case 329: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==329);
      case 330: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==330);
      case 331: /* stmt_law ::= law_observed */ yytestcase(yyruleno==331);
#line 2310 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy334 = yymsp[0].minor.yy334;}
#line 5305 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 332: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2425 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, NULL, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
																																															Language::Feature::LAW_BASIC_D, BasicLaw); }
#line 5312 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 333: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2429 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 5319 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 334: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2433 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 5326 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 335: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2437 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy334, yymsp[-4].minor.yy374, yymsp[-3].minor.yy0, yymsp[-2].minor.yy374, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 5332 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 336: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2440 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy334, yymsp[-6].minor.yy6, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 5338 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 337: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2444 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy334, yymsp[-8].minor.yy6, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-4].minor.yy113, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5345 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 338: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2448 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy334, yymsp[-6].minor.yy6, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 5351 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 339: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
      case 340: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */ yytestcase(yyruleno==340);
#line 2452 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 5359 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 341: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2460 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 5366 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 342: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2464 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 5373 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 343: /* law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2468 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy6, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 5380 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 344: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2472 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 5387 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 345: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2476 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 5394 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2480 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 5400 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2484 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy334, yymsp[-3].minor.yy0, yymsp[-2].minor.yy357, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 5406 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 348: /* law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD */
#line 2489 "bcplus/parser/detail/lemon_parser.y"
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
#line 5425 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* stmt_code_blk ::= ASP_GR */
#line 2523 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 5430 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* stmt_code_blk ::= ASP_CP */
#line 2524 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 5435 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* stmt_code_blk ::= F2LP_GR */
#line 2525 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 5440 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* stmt_code_blk ::= F2LP_CP */
#line 2526 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 5445 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* stmt_code_blk ::= LUA_GR */
#line 2527 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 5450 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* stmt_code_blk ::= LUA_CP */
#line 2528 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 5455 "bcplus/parser/detail/lemon_parser.c"
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
#line 199 "bcplus/parser/detail/lemon_parser.y"
 parser->_parse_error("Syntax error.");	
#line 5521 "bcplus/parser/detail/lemon_parser.c"
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
