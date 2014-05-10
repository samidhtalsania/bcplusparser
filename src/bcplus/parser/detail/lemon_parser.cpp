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

#line 446 "bcplus/parser/detail/lemon_parser.y"

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

#line 670 "bcplus/parser/detail/lemon_parser.y"

	#define NUM_UOP(t_new, t, val)																				\
		ref_ptr<const Referenced> t_ptr = t;																			\
		t_new = new Number(val, t->beginLoc(), t->endLoc());

	
	#define NUM_BOP(t_new, l, r, val)																			\
		ref_ptr<const Referenced> l_ptr = l, r_ptr = r;																\
		t_new = new Number(val, l->beginLoc(), r->endLoc());

#line 712 "bcplus/parser/detail/lemon_parser.y"

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

#line 792 "bcplus/parser/detail/lemon_parser.y"


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
	


#line 868 "bcplus/parser/detail/lemon_parser.y"

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

#line 948 "bcplus/parser/detail/lemon_parser.y"

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



#line 1177 "bcplus/parser/detail/lemon_parser.y"

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
#line 1336 "bcplus/parser/detail/lemon_parser.y"

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
#line 2022 "bcplus/parser/detail/lemon_parser.y"

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

#line 2045 "bcplus/parser/detail/lemon_parser.y"

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
#line 2072 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRange const* maxstep;
		Token const* label;
	};

#line 2175 "bcplus/parser/detail/lemon_parser.y"

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

#line 2247 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2330 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2511 "bcplus/parser/detail/lemon_parser.y"

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
#define YYNSTATE 691
#define YYNRULE 358
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
#define YY_ACTTAB_COUNT (2656)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   690,  157,  158,  689,  688,  687,  686,  685,  684,  683,
 /*    10 */   682,  681,  680,  679,  678,  677,  676,  675,  108,  648,
 /*    20 */   259,  674,  667,  673,  672,  666,   98,   96,   95,  148,
 /*    30 */   563,  256,  260,  127,  125,  124,  540,  185,  610,  338,
 /*    40 */    20,   64,   63,   62,  407,   61,   60,   59,   58,  234,
 /*    50 */   236,  187,  608,  184,  182,  180,  178,  177,  648,  259,
 /*    60 */   674,  667,  673,  672,  666,   94,   93,   92,   91,   90,
 /*    70 */   252,  260,  180,  178,  177,  144,  489,  601,  600,  599,
 /*    80 */   598,   25,   24,   28,   27,  272,  691,   29,  474,  473,
 /*    90 */   472,  471,  470,  469,  468,  467,  466,  465,  464,  463,
 /*   100 */   462,  461,  460,  459,  458,  647,  433,  669,   52,  434,
 /*   110 */   646,  443,  442,  439,  438,  441,  440,  647,  433,  482,
 /*   120 */   481,  434,  646,  624,  618,  674,  667,  673,  672,  666,
 /*   130 */    38,   34,    8,  373,  300,  552,   36,  103,    6,    7,
 /*   140 */   242,  155,  400,  205,   37,  623,  433,  629,  199,  434,
 /*   150 */   622,  250,  640,  639,  641,    5,  476,  475,    4,  545,
 /*   160 */    35,  191,  393,  544,  640,  639,  394,  606,  543,  607,
 /*   170 */     9,   16,   15,  261,  149,   17,  106,   55,  123,  596,
 /*   180 */   248,  597,   51,  162,  512,  278,  147,   65,   21,   28,
 /*   190 */    27,  249,  248,   29,  647,  433,   43,   42,  434,  646,
 /*   200 */    44,   26,   25,   24,   28,   27,  183,  619,   29,  620,
 /*   210 */   104,  369,  518,  368,  431,   89,  130,  363,  510,  362,
 /*   220 */   511,   18,   19,  437,   50,  647,  433,   89,   49,  434,
 /*   230 */   646,  430,  512,  278,  501,  436,  665,  435,  107,  154,
 /*   240 */   181,  640,  639,  641,  626,  179,  432,  436,  665,  435,
 /*   250 */   128,   40,   39,   43,   42,  126,  596,   44,  597,  554,
 /*   260 */   162,  157,  158,  147,  233,   33,  516,  362,  511,  248,
 /*   270 */   164,  374,  640,  639,  641,  436,  665,  435,  431,  166,
 /*   280 */   648,  323,  674,  667,  673,  672,  666,  596,   57,  597,
 /*   290 */   403,  162,  282,  324,  147,  497,   21,   56,   30,   31,
 /*   300 */   248,  150,  372,  555,   89,  624,  618,  674,  667,  673,
 /*   310 */   672,  666,   29,   64,   63,   62,  301,   61,   60,   59,
 /*   320 */    58,  358,  354,  402,  436,  665,  435,  353,  495,   18,
 /*   330 */    19,  360,  504,  359,  668,   89,  499,   94,   93,   92,
 /*   340 */    91,   90,  627,  662,  670,  671,  674,  667,  673,  672,
 /*   350 */   666,  378,  326,   34,  566,  436,  665,  435,  184,  182,
 /*   360 */   180,  178,  177,   53,  103,  145,  648,  259,  674,  667,
 /*   370 */   673,  672,  666,  552,  538,  665,  647,  433,  252,  260,
 /*   380 */   434,  646,  494,  431,  489,  601,  600,  599,  598,  626,
 /*   390 */   593,  592,  431,  273,  532,  531,  530,  529,   23,   44,
 /*   400 */   497,  152,  409,  167,  137,    2,  143,  146,  151,  497,
 /*   410 */   528,  145,  526,  664,  160,  163,  374,  525,  102,  100,
 /*   420 */    98,   96,   95,  640,  639,  641,  350,  478,  477,  524,
 /*   430 */   522,  523,  527,  498,  565,  397,  398,  564,  606,  417,
 /*   440 */   607,  352,  495,  103,  183,  149,  595,  106,  561,  586,
 /*   450 */   433,  248,  148,  434,  585,  645,  624,  618,  674,  667,
 /*   460 */   673,  672,  666,   20,   64,   63,   62,  339,   61,   60,
 /*   470 */    59,   58,  663,   26,   25,   24,   28,   27,  181,  625,
 /*   480 */    29,  104,  659,  179,  419,  140,   89,  418,   94,   93,
 /*   490 */    92,   91,   90,  426,  512,  278,  579,  578,  580,  661,
 /*   500 */   433,  556,  103,  434,  660,  249,  436,  665,  435,  132,
 /*   510 */   626,  569,  588,  570,  189,  102,  100,   98,   96,   95,
 /*   520 */    47,  142,  648,  259,  674,  667,  673,  672,  666,  626,
 /*   530 */   514,  645,  533,  500,  255,  260,   32,  374,  517,  280,
 /*   540 */   604,  601,  600,  599,  598,  235,  655,  654,  656,  589,
 /*   550 */   480,  479,  431,   45,   46,   22,  374,  509,  141,  110,
 /*   560 */   432,  657,  139,  658,   94,   93,   92,   91,   90,  429,
 /*   570 */   101,   26,   25,   24,   28,   27,  188,  165,   29,  436,
 /*   580 */   665,  435,  662,  670,  671,  674,  667,  673,  672,  666,
 /*   590 */   377,  326,  558,  559,  648,  259,  674,  667,  673,  672,
 /*   600 */   666,  553,  368,  156,   99,   10,  253,  260,  410,   97,
 /*   610 */   537,  264,  604,  601,  600,  599,  598,  426,  508,  359,
 /*   620 */   648,  259,  674,  667,  673,  672,  666,  539,  278,  436,
 /*   630 */   665,  435,  255,  260,  186,  245,  210,  265,  604,  601,
 /*   640 */   600,  599,  598,  411,  648,  258,  674,  667,  673,  672,
 /*   650 */   666,  536,  367,  416,  415,  136,  605,  260,  414,  412,
 /*   660 */   521,  590,  604,  601,  600,  599,  598,  648,  259,  674,
 /*   670 */   667,  673,  672,  666,   26,   25,   24,   28,   27,  255,
 /*   680 */   260,   29,  406,  520,  603,  604,  601,  600,  599,  598,
 /*   690 */   648,  259,  674,  667,  673,  672,  666,  138,   54,  593,
 /*   700 */   592,  519,  255,  260,  552,  404,  135,  602,  604,  601,
 /*   710 */   600,  599,  598,  139,  134,  648,  259,  674,  667,  673,
 /*   720 */   672,  666,   26,   25,   24,   28,   27,  255,  260,   29,
 /*   730 */   513,  139,  428,  604,  601,  600,  599,  598,  662,  670,
 /*   740 */   671,  674,  667,  673,  672,  666,  376,  326,  370,  557,
 /*   750 */   648,  259,  674,  667,  673,  672,  666,  133,  357,  402,
 /*   760 */   139,  567,  255,  260,  356,  402,  399,  427,  604,  601,
 /*   770 */   600,  599,  598,  355,  402,  183,  648,  259,  674,  667,
 /*   780 */   673,  672,  666,   41,   40,   39,   43,   42,  255,  260,
 /*   790 */    44,  503,  506,  296,  604,  601,  600,  599,  598,  401,
 /*   800 */   648,  259,  674,  667,  673,  672,  666,  502,  485,  181,
 /*   810 */   496,  159,  255,  260,  179,  249,  493,  337,  604,  601,
 /*   820 */   600,  599,  598,  648,  259,  674,  667,  673,  672,  666,
 /*   830 */    14,   13,   12,   16,   15,  255,  260,   17, 1050,    1,
 /*   840 */   336,  604,  601,  600,  599,  598,  648,  259,  674,  667,
 /*   850 */   673,  672,  666,  102,  100,   98,   96,   95,  255,  260,
 /*   860 */   176,  995,  483,  267,  604,  601,  600,  599,  598,   17,
 /*   870 */   432,  648,  259,  674,  667,  673,  672,  666,  329,  402,
 /*   880 */    48,  995,    3,  255,  260,  175,  426,   34,  266,  604,
 /*   890 */   601,  600,  599,  598,  662,  670,  671,  674,  667,  673,
 /*   900 */   672,  666,  375,  326,  248,  379,  648,  259,  674,  667,
 /*   910 */   673,  672,  666,   26,   25,   24,   28,   27,  251,  260,
 /*   920 */    29,  153,  484,  278,  489,  601,  600,  599,  598,  349,
 /*   930 */   457,  456,  455,  262,  648,  259,  674,  667,  673,  672,
 /*   940 */   666,   26,   25,   24,   28,   27,  252,  260,   29,  454,
 /*   950 */   453,  452,  489,  601,  600,  599,  598,  451,  450,  436,
 /*   960 */   449,  274,  648,  258,  674,  667,  673,  672,  666,   26,
 /*   970 */    25,   24,   28,   27,  490,  260,   29,  665,  448,  447,
 /*   980 */   489,  601,  600,  599,  598,  446,  445,  109,  628,  486,
 /*   990 */   612,  648,  259,  674,  667,  673,  672,  666,   41,   40,
 /*  1000 */    39,   43,   42,  252,  260,   44,  611,   34,  244,  489,
 /*  1010 */   601,  600,  599,  598,  594,  243,  432,  161,  488,  648,
 /*  1020 */   259,  674,  667,  673,  672,  666,  436,  591,  422,  423,
 /*  1030 */    48,  252,  260,  240,  238,  421,  237,  489,  601,  600,
 /*  1040 */   599,  598,  420,  562,  371,  542,  487,  648,  259,  674,
 /*  1050 */   667,  673,  672,  666,  538,  543,  515,  507,  231,  252,
 /*  1060 */   260,   11,  230,   10,  392,  489,  601,  600,  599,  598,
 /*  1070 */   229,  224,  226,  227,  396,  648,  259,  674,  667,  673,
 /*  1080 */   672,  666,  225,  220,  390,  222,  221,  252,  260,  219,
 /*  1090 */   209,  208,  389,  489,  601,  600,  599,  598,  204,  203,
 /*  1100 */   217,  215,  395,  648,  259,  674,  667,  673,  672,  666,
 /*  1110 */   207,  388,  387,  213,  202,  252,  260,  386,  211,  201,
 /*  1120 */   385,  489,  601,  600,  599,  598,  198,  197,  206,  200,
 /*  1130 */   284,  648,  259,  674,  667,  673,  672,  666,  384,  383,
 /*  1140 */   196,  382,  190,  252,  260,  195,  194,  560,  192,  489,
 /*  1150 */   601,  600,  599,  598,  381,  380,  541,  232,  328,  648,
 /*  1160 */   259,  674,  667,  673,  672,  666,  246,  285,  330,  241,
 /*  1170 */   277,  252,  260,  331,  550,  391,  546,  489,  601,  600,
 /*  1180 */   599,  598,  549,  548,  547,  276,  327,  648,  259,  674,
 /*  1190 */   667,  673,  672,  666,  275,  218,  405,  216,  214,  255,
 /*  1200 */   260,  212,  193,  505,  271,  604,  601,  600,  599,  598,
 /*  1210 */    83,   82,   81,   80,   79,  648,  259,  674,  667,  673,
 /*  1220 */   672,  666, 1051, 1051, 1051, 1051, 1051,  255,  260, 1051,
 /*  1230 */  1051, 1051,  270,  604,  601,  600,  599,  598,   88,   87,
 /*  1240 */    86,   85,   84,  648,  259,  674,  667,  673,  672,  666,
 /*  1250 */  1051, 1051, 1051, 1051, 1051,  255,  260, 1051, 1051, 1051,
 /*  1260 */   173,  604,  601,  600,  599,  598,  131,  129,  127,  125,
 /*  1270 */   124,  648,  259,  674,  667,  673,  672,  666, 1051, 1051,
 /*  1280 */  1051, 1051, 1051,  255,  260, 1051, 1051, 1051,  172,  604,
 /*  1290 */   601,  600,  599,  598, 1051, 1051, 1051, 1051, 1051,  648,
 /*  1300 */   259,  674,  667,  673,  672,  666, 1051,   13,   12,   16,
 /*  1310 */    15,  255,  260,   17, 1051, 1051,  171,  604,  601,  600,
 /*  1320 */   599,  598, 1051, 1051, 1051, 1051, 1051,  648,  259,  674,
 /*  1330 */   667,  673,  672,  666, 1051, 1051, 1051, 1051, 1051,  255,
 /*  1340 */   260, 1051, 1051, 1051,  170,  604,  601,  600,  599,  598,
 /*  1350 */  1051, 1051, 1051, 1051, 1051,  648,  259,  674,  667,  673,
 /*  1360 */   672,  666, 1051, 1051, 1051, 1051, 1051,  255,  260, 1051,
 /*  1370 */  1051, 1051,  169,  604,  601,  600,  599,  598, 1051, 1051,
 /*  1380 */  1051, 1051, 1051,  648,  259,  674,  667,  673,  672,  666,
 /*  1390 */  1051, 1051, 1051, 1051, 1051,  255,  260, 1051, 1051, 1051,
 /*  1400 */   168,  604,  601,  600,  599,  598, 1051, 1051, 1051, 1051,
 /*  1410 */   148, 1051, 1051,  645, 1051,  539,  278, 1051,  148, 1051,
 /*  1420 */  1051, 1051,   64,   63,   62, 1051,   61,   60,   59,   58,
 /*  1430 */    64,   63,   62, 1051,   61,   60,   59,   58,  366,  535,
 /*  1440 */   367,  416,  415, 1051, 1051, 1051,   94,   93,   92,   91,
 /*  1450 */    90,  584, 1051, 1051,   94,   93,   92,   91,   90,  492,
 /*  1460 */   117,  116,  115, 1051,  114,  113,  112,  111,  647,  433,
 /*  1470 */  1051, 1051,  434,  646, 1051, 1051, 1051,  648,  259,  674,
 /*  1480 */   667,  673,  672,  666,  122,  121,  120,  119,  118,  256,
 /*  1490 */   260,  184,  182,  180,  178,  177,  610,  609, 1051, 1051,
 /*  1500 */   648,  259,  674,  667,  673,  672,  666, 1051, 1051,  247,
 /*  1510 */   608, 1051,  256,  260, 1051,  640,  639,  641, 1051,  610,
 /*  1520 */   609, 1051, 1051,  648,  259,  674,  667,  673,  672,  666,
 /*  1530 */  1051,  621,  239,  608, 1051,  256,  260, 1051, 1051,  106,
 /*  1540 */  1051, 1051,  610,  609,  662,  670,  671,  674,  667,  673,
 /*  1550 */   672,  666, 1051,  325, 1051,  228,  608,  648,  259,  674,
 /*  1560 */   667,  673,  672,  666,  131,  129,  127,  125,  124,  256,
 /*  1570 */   260, 1051, 1051,  105, 1051, 1051,  610,  609,   89,   78,
 /*  1580 */    77,   76, 1051,   75,   74,   73,   72, 1051, 1051,  223,
 /*  1590 */   608,  624,  618,  674,  667,  673,  672,  666,  436,  665,
 /*  1600 */   435, 1051,  281,   83,   82,   81,   80,   79,   65,   71,
 /*  1610 */    70,  351,   69,   68,   67,   66,  756,  756,  756, 1051,
 /*  1620 */   756,  756,  756,  756, 1051,  648,  636,  674,  667,  673,
 /*  1630 */   672,  666,   88,   87,   86,   85,   84,  638,  324, 1051,
 /*  1640 */   756,  756,  756,  756,  756,  117,  116,  115, 1051,  114,
 /*  1650 */   113,  112,  111,  587,  577,  674,  667,  673,  672,  666,
 /*  1660 */   587,  577,  674,  667,  673,  672,  666,  257, 1051,  122,
 /*  1670 */   121,  120,  119,  118,  254, 1051,  279,  574,  571, 1051,
 /*  1680 */   539,  278,  183,  263,  574,  571, 1051, 1051,  587,  577,
 /*  1690 */   674,  667,  673,  672,  666,  587,  577,  674,  667,  673,
 /*  1700 */   672,  666,  576,  365,  535,  367,  416,  415,  551,  257,
 /*  1710 */  1051,  568,  574,  571, 1051, 1051,  181, 1051,  573,  574,
 /*  1720 */   571,  179, 1051, 1051, 1051,  587,  577,  674,  667,  673,
 /*  1730 */   672,  666,  587,  577,  674,  667,  673,  672,  666,  257,
 /*  1740 */  1051,  184,  182,  180,  178,  177,  257, 1051,  572,  574,
 /*  1750 */   571,  539,  278, 1051, 1051,  425,  574,  571, 1051,  587,
 /*  1760 */   577,  674,  667,  673,  672,  666,  587,  577,  674,  667,
 /*  1770 */   673,  672,  666,  257,  364,  535,  367,  416,  415, 1051,
 /*  1780 */   257, 1051,  424,  574,  571,  539,  278, 1051, 1051,  286,
 /*  1790 */   574,  571,  587,  577,  674,  667,  673,  672,  666,  587,
 /*  1800 */   577,  674,  667,  673,  672,  666,  257, 1051,  361,  535,
 /*  1810 */   367,  416,  415,  257, 1051,  333,  574,  571, 1051, 1051,
 /*  1820 */  1051, 1051,  332,  574,  571,  648,  268,  674,  667,  673,
 /*  1830 */   672,  666, 1051, 1051, 1051,  539,  278,  638,  324,  662,
 /*  1840 */   670,  671,  674,  667,  673,  672,  666, 1051,  347,  662,
 /*  1850 */   670,  671,  674,  667,  673,  672,  666, 1051,  283,  413,
 /*  1860 */   367,  416,  415, 1051,  662,  670,  671,  674,  667,  673,
 /*  1870 */   672,  666, 1051,  348,  662,  670,  671,  674,  667,  673,
 /*  1880 */   672,  666, 1051,  653,  662,  670,  671,  674,  667,  673,
 /*  1890 */   672,  666, 1051,  649,  662,  670,  671,  674,  667,  673,
 /*  1900 */   672,  666, 1051,  652,  662,  670,  671,  674,  667,  673,
 /*  1910 */   672,  666, 1051,  651,  662,  670,  671,  674,  667,  673,
 /*  1920 */   672,  666, 1051,  650,  662,  670,  671,  674,  667,  673,
 /*  1930 */   672,  666, 1051,  346, 1051,  662,  670,  671,  674,  667,
 /*  1940 */   673,  672,  666, 1051,  345,  662,  670,  671,  674,  667,
 /*  1950 */   673,  672,  666, 1051,  644,  662,  670,  671,  674,  667,
 /*  1960 */   673,  672,  666, 1051,  643, 1051, 1051, 1051,  662,  670,
 /*  1970 */   671,  674,  667,  673,  672,  666, 1051,  642,  662,  670,
 /*  1980 */   671,  674,  667,  673,  672,  666, 1051,  637, 1051, 1051,
 /*  1990 */  1051, 1051,  662,  670,  671,  674,  667,  673,  672,  666,
 /*  2000 */  1051,  344, 1051,  662,  670,  671,  674,  667,  673,  672,
 /*  2010 */   666, 1051,  343,  662,  670,  671,  674,  667,  673,  672,
 /*  2020 */   666, 1051,  635,  662,  670,  671,  674,  667,  673,  672,
 /*  2030 */   666, 1051,  634,  662,  670,  671,  674,  667,  673,  672,
 /*  2040 */   666, 1051,  633,  662,  670,  671,  674,  667,  673,  672,
 /*  2050 */   666, 1051,  342,  662,  670,  671,  674,  667,  673,  672,
 /*  2060 */   666, 1051,  341,  662,  670,  671,  674,  667,  673,  672,
 /*  2070 */   666, 1051,  632,  662,  670,  671,  674,  667,  673,  672,
 /*  2080 */   666, 1051,  631,  662,  670,  671,  674,  667,  673,  672,
 /*  2090 */   666, 1051,  630,  662,  670,  671,  674,  667,  673,  672,
 /*  2100 */   666, 1051,  322,  662,  670,  671,  674,  667,  673,  672,
 /*  2110 */   666, 1051,  321,  662,  670,  671,  674,  667,  673,  672,
 /*  2120 */   666, 1051,  320,  662,  670,  671,  674,  667,  673,  672,
 /*  2130 */   666, 1051,  319,  662,  670,  671,  674,  667,  673,  672,
 /*  2140 */   666, 1051,  318, 1051, 1051, 1051, 1051,  662,  670,  671,
 /*  2150 */   674,  667,  673,  672,  666, 1051,  317, 1051,  662,  670,
 /*  2160 */   671,  674,  667,  673,  672,  666, 1051,  316,  662,  670,
 /*  2170 */   671,  674,  667,  673,  672,  666, 1051,  315,  662,  670,
 /*  2180 */   671,  674,  667,  673,  672,  666, 1051,  314,  662,  670,
 /*  2190 */   671,  674,  667,  673,  672,  666, 1051,  313,  662,  670,
 /*  2200 */   671,  674,  667,  673,  672,  666, 1051,  312,  662,  670,
 /*  2210 */   671,  674,  667,  673,  672,  666, 1051,  311,  662,  670,
 /*  2220 */   671,  674,  667,  673,  672,  666, 1051,  310,  662,  670,
 /*  2230 */   671,  674,  667,  673,  672,  666, 1051,  309,  662,  670,
 /*  2240 */   671,  674,  667,  673,  672,  666, 1051,  308,  662,  670,
 /*  2250 */   671,  674,  667,  673,  672,  666, 1051,  307,  662,  670,
 /*  2260 */   671,  674,  667,  673,  672,  666, 1051,  306,  662,  670,
 /*  2270 */   671,  674,  667,  673,  672,  666, 1051,  305,  662,  670,
 /*  2280 */   671,  674,  667,  673,  672,  666, 1051,  304,  662,  670,
 /*  2290 */   671,  674,  667,  673,  672,  666, 1051,  303, 1051, 1051,
 /*  2300 */  1051, 1051,  662,  670,  671,  674,  667,  673,  672,  666,
 /*  2310 */  1051,  302, 1051,  662,  670,  671,  674,  667,  673,  672,
 /*  2320 */   666, 1051,  298,  662,  670,  671,  674,  667,  673,  672,
 /*  2330 */   666, 1051,  297,  662,  670,  671,  674,  667,  673,  672,
 /*  2340 */   666, 1051,  295,  662,  670,  671,  674,  667,  673,  672,
 /*  2350 */   666, 1051,  294,  662,  670,  671,  674,  667,  673,  672,
 /*  2360 */   666, 1051,  174,  624,  618,  674,  667,  673,  672,  666,
 /*  2370 */   539,  278, 1051, 1051,  340,  624,  618,  674,  667,  673,
 /*  2380 */   672,  666,  539,  278, 1051, 1051,  617,  624,  618,  674,
 /*  2390 */   667,  673,  672,  666,  534,  367,  416,  415,  613,  624,
 /*  2400 */   618,  674,  667,  673,  672,  666,  408,  367,  416,  415,
 /*  2410 */   616,  624,  618,  674,  667,  673,  672,  666, 1051, 1051,
 /*  2420 */  1051, 1051,  615,  624,  618,  674,  667,  673,  672,  666,
 /*  2430 */  1051, 1051, 1051, 1051,  614,  624,  618,  674,  667,  673,
 /*  2440 */   672,  666, 1051, 1051, 1051, 1051,  299,  624,  618,  674,
 /*  2450 */   667,  673,  672,  666, 1051, 1051, 1051, 1051,  335,  624,
 /*  2460 */   618,  674,  667,  673,  672,  666, 1051, 1051, 1051, 1051,
 /*  2470 */   334,  624,  618,  674,  667,  673,  672,  666, 1051, 1051,
 /*  2480 */  1051, 1051,  583,  624,  618,  674,  667,  673,  672,  666,
 /*  2490 */  1051, 1051, 1051, 1051,  582,  624,  618,  674,  667,  673,
 /*  2500 */   672,  666, 1051, 1051, 1051, 1051,  581, 1051,  624,  618,
 /*  2510 */   674,  667,  673,  672,  666, 1051, 1051, 1051, 1051,  293,
 /*  2520 */   624,  618,  674,  667,  673,  672,  666, 1051,  552,  538,
 /*  2530 */  1051,  292,  624,  618,  674,  667,  673,  672,  666, 1051,
 /*  2540 */  1051, 1051, 1051,  291,  624,  618,  674,  667,  673,  672,
 /*  2550 */   666, 1051, 1051, 1051, 1051,  290,  624,  618,  674,  667,
 /*  2560 */   673,  672,  666, 1051, 1051, 1051, 1051,  289,  624,  618,
 /*  2570 */   674,  667,  673,  672,  666, 1051, 1051, 1051, 1051,  288,
 /*  2580 */   624,  618,  674,  667,  673,  672,  666,  491, 1051, 1051,
 /*  2590 */  1051,  287,  624,  618,  674,  667,  673,  672,  666,  183,
 /*  2600 */  1051,  902, 1051,  575,  624,  618,  674,  667,  673,  672,
 /*  2610 */   666,  444, 1051, 1051, 1051,  269,  902,  902, 1051,  184,
 /*  2620 */   182,  180,  178,  177, 1051, 1051, 1051,   14,   13,   12,
 /*  2630 */    16,   15, 1051,  181,   17, 1051,  902,  902,  179, 1051,
 /*  2640 */  1051, 1051, 1051,  131,  129,  127,  125,  124, 1051,  902,
 /*  2650 */  1051, 1051, 1051, 1051, 1051,  902,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   136,  100,  101,  139,  140,  141,  142,  143,  144,  145,
 /*    10 */   146,  147,  148,  149,  150,  151,  152,  153,   77,  155,
 /*    20 */   156,  157,  158,  159,  160,  161,  108,  109,  110,   70,
 /*    30 */    74,  167,  168,  108,  109,  110,    1,   90,  174,  175,
 /*    40 */    81,   82,   83,   84,  103,   86,   87,   88,   89,   77,
 /*    50 */    94,  187,  188,  106,  107,  108,  109,  110,  155,  156,
 /*    60 */   157,  158,  159,  160,  161,  106,  107,  108,  109,  110,
 /*    70 */   167,  168,  108,  109,  110,  103,  173,  174,  175,  176,
 /*    80 */   177,   96,   97,   98,   99,  182,    0,  102,  224,  225,
 /*    90 */   226,  227,  228,  229,  230,  231,  232,  233,  234,  235,
 /*   100 */   236,  237,  238,  239,  240,    1,    2,   73,   72,    5,
 /*   110 */     6,    7,    8,    9,   10,   11,   12,    1,    2,  216,
 /*   120 */   217,    5,    6,  155,  156,  157,  158,  159,  160,  161,
 /*   130 */    33,   41,   28,  165,  166,    1,   32,  103,   34,   35,
 /*   140 */    43,   37,   27,   39,   47,    1,    2,   99,   44,    5,
 /*   150 */     6,  103,   48,   49,   50,   51,   52,   53,   54,  108,
 /*   160 */    56,   57,   46,  112,   48,   49,   50,   63,  133,   65,
 /*   170 */    80,   98,   99,   69,   70,  102,   72,   71,   82,   63,
 /*   180 */    76,   65,   72,   67,  170,  171,   70,   82,   72,   98,
 /*   190 */    99,   76,   76,  102,    1,    2,   98,   99,    5,    6,
 /*   200 */   102,   95,   96,   97,   98,   99,   72,   63,  102,   65,
 /*   210 */   106,  198,  199,  200,  164,  111,   72,  203,  204,  205,
 /*   220 */   206,  105,  106,  119,   72,    1,    2,  111,   72,    5,
 /*   230 */     6,  181,  170,  171,   73,  131,  132,  133,   72,  135,
 /*   240 */   106,   48,   49,   50,  159,  111,  131,  131,  132,  133,
 /*   250 */   106,   96,   97,   98,   99,  111,   63,  102,   65,   74,
 /*   260 */    67,  100,  101,   70,   77,   72,  204,  205,  206,   76,
 /*   270 */   185,  186,   48,   49,   50,  131,  132,  133,  164,   94,
 /*   280 */   155,  156,  157,  158,  159,  160,  161,   63,   71,   65,
 /*   290 */   103,   67,  167,  168,   70,  181,   72,   71,  105,  106,
 /*   300 */    76,   70,  189,  190,  111,  155,  156,  157,  158,  159,
 /*   310 */   160,  161,  102,   82,   83,   84,  166,   86,   87,   88,
 /*   320 */    89,  210,  211,  212,  131,  132,  133,  213,  214,  105,
 /*   330 */   106,  207,  208,  209,   73,  111,   74,  106,  107,  108,
 /*   340 */   109,  110,   99,  154,  155,  156,  157,  158,  159,  160,
 /*   350 */   161,  162,  163,   41,   74,  131,  132,  133,  106,  107,
 /*   360 */   108,  109,  110,   31,  103,  103,  155,  156,  157,  158,
 /*   370 */   159,  160,  161,    1,    2,  132,    1,    2,  167,  168,
 /*   380 */     5,    6,   74,  164,  173,  174,  175,  176,  177,  159,
 /*   390 */    91,   92,  164,  182,   22,   23,   24,   25,   99,  102,
 /*   400 */   181,   14,   30,   16,   17,   18,   19,   20,   21,  181,
 /*   410 */    38,  103,   40,   73,   72,  185,  186,   45,  106,  107,
 /*   420 */   108,  109,  110,   48,   49,   50,  215,  216,  217,   57,
 /*   430 */    58,   59,   60,  214,   74,   48,   49,   74,   63,   97,
 /*   440 */    65,  213,  214,  103,   72,   70,   68,   72,   75,    1,
 /*   450 */     2,   76,   70,    5,    6,   73,  155,  156,  157,  158,
 /*   460 */   159,  160,  161,   81,   82,   83,   84,  166,   86,   87,
 /*   470 */    88,   89,   73,   95,   96,   97,   98,   99,  106,   73,
 /*   480 */   102,  106,   73,  111,   73,   72,  111,   97,  106,  107,
 /*   490 */   108,  109,  110,  156,  170,  171,   48,   49,   50,    1,
 /*   500 */     2,   75,  103,    5,    6,   76,  131,  132,  133,  103,
 /*   510 */   159,   63,  175,   65,  103,  106,  107,  108,  109,  110,
 /*   520 */    72,  134,  155,  156,  157,  158,  159,  160,  161,  159,
 /*   530 */   206,   73,   73,    2,  167,  168,  185,  186,   74,  172,
 /*   540 */   173,  174,  175,  176,  177,  107,   48,   49,   50,   73,
 /*   550 */     1,    2,  164,  105,  106,  185,  186,   74,   94,  111,
 /*   560 */   131,   63,  103,   65,  106,  107,  108,  109,  110,  181,
 /*   570 */    72,   95,   96,   97,   98,   99,  107,   94,  102,  131,
 /*   580 */   132,  133,  154,  155,  156,  157,  158,  159,  160,  161,
 /*   590 */   162,  163,    3,    4,  155,  156,  157,  158,  159,  160,
 /*   600 */   161,  199,  200,   72,  106,   26,  167,  168,    2,  111,
 /*   610 */    73,  172,  173,  174,  175,  176,  177,  156,  208,  209,
 /*   620 */   155,  156,  157,  158,  159,  160,  161,  170,  171,  131,
 /*   630 */   132,  133,  167,  168,  183,  184,  175,  172,  173,  174,
 /*   640 */   175,  176,  177,   55,  155,  156,  157,  158,  159,  160,
 /*   650 */   161,  194,  195,  196,  197,   72,  167,  168,  201,  202,
 /*   660 */    73,  172,  173,  174,  175,  176,  177,  155,  156,  157,
 /*   670 */   158,  159,  160,  161,   95,   96,   97,   98,   99,  167,
 /*   680 */   168,  102,    2,   73,  172,  173,  174,  175,  176,  177,
 /*   690 */   155,  156,  157,  158,  159,  160,  161,   72,   71,   91,
 /*   700 */    92,   73,  167,  168,    1,    2,   72,  172,  173,  174,
 /*   710 */   175,  176,  177,  103,   72,  155,  156,  157,  158,  159,
 /*   720 */   160,  161,   95,   96,   97,   98,   99,  167,  168,  102,
 /*   730 */    73,  103,  172,  173,  174,  175,  176,  177,  154,  155,
 /*   740 */   156,  157,  158,  159,  160,  161,  162,  163,  191,  192,
 /*   750 */   155,  156,  157,  158,  159,  160,  161,   72,  211,  212,
 /*   760 */   103,   73,  167,  168,  211,  212,   27,  172,  173,  174,
 /*   770 */   175,  176,  177,  211,  212,   72,  155,  156,  157,  158,
 /*   780 */   159,  160,  161,   95,   96,   97,   98,   99,  167,  168,
 /*   790 */   102,   74,    2,  172,  173,  174,  175,  176,  177,  103,
 /*   800 */   155,  156,  157,  158,  159,  160,  161,    2,   73,  106,
 /*   810 */    74,   94,  167,  168,  111,   76,   74,  172,  173,  174,
 /*   820 */   175,  176,  177,  155,  156,  157,  158,  159,  160,  161,
 /*   830 */    95,   96,   97,   98,   99,  167,  168,  102,  137,  138,
 /*   840 */   172,  173,  174,  175,  176,  177,  155,  156,  157,  158,
 /*   850 */   159,  160,  161,  106,  107,  108,  109,  110,  167,  168,
 /*   860 */    82,   74,   74,  172,  173,  174,  175,  176,  177,  102,
 /*   870 */   131,  155,  156,  157,  158,  159,  160,  161,  211,  212,
 /*   880 */    62,   94,   94,  167,  168,   82,  156,   41,  172,  173,
 /*   890 */   174,  175,  176,  177,  154,  155,  156,  157,  158,  159,
 /*   900 */   160,  161,  162,  163,   76,  175,  155,  156,  157,  158,
 /*   910 */   159,  160,  161,   95,   96,   97,   98,   99,  167,  168,
 /*   920 */   102,   77,  170,  171,  173,  174,  175,  176,  177,   77,
 /*   930 */    74,   74,   74,  182,  155,  156,  157,  158,  159,  160,
 /*   940 */   161,   95,   96,   97,   98,   99,  167,  168,  102,   74,
 /*   950 */    74,   74,  173,  174,  175,  176,  177,   74,   74,  131,
 /*   960 */    74,  182,  155,  156,  157,  158,  159,  160,  161,   95,
 /*   970 */    96,   97,   98,   99,  167,  168,  102,  132,   74,   74,
 /*   980 */   173,  174,  175,  176,  177,   74,   74,   66,  159,  182,
 /*   990 */   164,  155,  156,  157,  158,  159,  160,  161,   95,   96,
 /*  1000 */    97,   98,   99,  167,  168,  102,  156,   41,  219,  173,
 /*  1010 */   174,  175,  176,  177,  159,  222,  131,   61,  182,  155,
 /*  1020 */   156,  157,  158,  159,  160,  161,  131,  159,  156,  223,
 /*  1030 */    62,  167,  168,  222,  219,  223,  222,  173,  174,  175,
 /*  1040 */   176,  177,  223,  190,    2,  158,  182,  155,  156,  157,
 /*  1050 */   158,  159,  160,  161,    2,  133,  197,  197,  221,  167,
 /*  1060 */   168,   42,  220,   26,  223,  173,  174,  175,  176,  177,
 /*  1070 */   222,  222,  221,  219,  182,  155,  156,  157,  158,  159,
 /*  1080 */   160,  161,  220,  220,  223,  219,  221,  167,  168,  222,
 /*  1090 */   219,  221,  223,  173,  174,  175,  176,  177,  156,  219,
 /*  1100 */   222,  222,  182,  155,  156,  157,  158,  159,  160,  161,
 /*  1110 */   220,  223,  223,  222,  221,  167,  168,  223,  222,  220,
 /*  1120 */   223,  173,  174,  175,  176,  177,  156,  219,  222,  222,
 /*  1130 */   182,  155,  156,  157,  158,  159,  160,  161,  223,  223,
 /*  1140 */   221,  223,  156,  167,  168,  220,  222,  192,  222,  173,
 /*  1150 */   174,  175,  176,  177,  223,  223,  158,  219,  182,  155,
 /*  1160 */   156,  157,  158,  159,  160,  161,  184,  171,  171,  219,
 /*  1170 */   171,  167,  168,  171,  171,  223,  171,  173,  174,  175,
 /*  1180 */   176,  177,  171,  171,  171,  171,  182,  155,  156,  157,
 /*  1190 */   158,  159,  160,  161,  171,  220,    2,  220,  220,  167,
 /*  1200 */   168,  220,  219,    2,  172,  173,  174,  175,  176,  177,
 /*  1210 */   106,  107,  108,  109,  110,  155,  156,  157,  158,  159,
 /*  1220 */   160,  161,  241,  241,  241,  241,  241,  167,  168,  241,
 /*  1230 */   241,  241,  172,  173,  174,  175,  176,  177,  106,  107,
 /*  1240 */   108,  109,  110,  155,  156,  157,  158,  159,  160,  161,
 /*  1250 */   241,  241,  241,  241,  241,  167,  168,  241,  241,  241,
 /*  1260 */   172,  173,  174,  175,  176,  177,  106,  107,  108,  109,
 /*  1270 */   110,  155,  156,  157,  158,  159,  160,  161,  241,  241,
 /*  1280 */   241,  241,  241,  167,  168,  241,  241,  241,  172,  173,
 /*  1290 */   174,  175,  176,  177,  241,  241,  241,  241,  241,  155,
 /*  1300 */   156,  157,  158,  159,  160,  161,  241,   96,   97,   98,
 /*  1310 */    99,  167,  168,  102,  241,  241,  172,  173,  174,  175,
 /*  1320 */   176,  177,  241,  241,  241,  241,  241,  155,  156,  157,
 /*  1330 */   158,  159,  160,  161,  241,  241,  241,  241,  241,  167,
 /*  1340 */   168,  241,  241,  241,  172,  173,  174,  175,  176,  177,
 /*  1350 */   241,  241,  241,  241,  241,  155,  156,  157,  158,  159,
 /*  1360 */   160,  161,  241,  241,  241,  241,  241,  167,  168,  241,
 /*  1370 */   241,  241,  172,  173,  174,  175,  176,  177,  241,  241,
 /*  1380 */   241,  241,  241,  155,  156,  157,  158,  159,  160,  161,
 /*  1390 */   241,  241,  241,  241,  241,  167,  168,  241,  241,  241,
 /*  1400 */   172,  173,  174,  175,  176,  177,  241,  241,  241,  241,
 /*  1410 */    70,  241,  241,   73,  241,  170,  171,  241,   70,  241,
 /*  1420 */   241,  241,   82,   83,   84,  241,   86,   87,   88,   89,
 /*  1430 */    82,   83,   84,  241,   86,   87,   88,   89,  193,  194,
 /*  1440 */   195,  196,  197,  241,  241,  241,  106,  107,  108,  109,
 /*  1450 */   110,   73,  241,  241,  106,  107,  108,  109,  110,   74,
 /*  1460 */    82,   83,   84,  241,   86,   87,   88,   89,    1,    2,
 /*  1470 */   241,  241,    5,    6,  241,  241,  241,  155,  156,  157,
 /*  1480 */   158,  159,  160,  161,  106,  107,  108,  109,  110,  167,
 /*  1490 */   168,  106,  107,  108,  109,  110,  174,  175,  241,  241,
 /*  1500 */   155,  156,  157,  158,  159,  160,  161,  241,  241,  187,
 /*  1510 */   188,  241,  167,  168,  241,   48,   49,   50,  241,  174,
 /*  1520 */   175,  241,  241,  155,  156,  157,  158,  159,  160,  161,
 /*  1530 */   241,   73,  187,  188,  241,  167,  168,  241,  241,   72,
 /*  1540 */   241,  241,  174,  175,  154,  155,  156,  157,  158,  159,
 /*  1550 */   160,  161,  241,  163,  241,  187,  188,  155,  156,  157,
 /*  1560 */   158,  159,  160,  161,  106,  107,  108,  109,  110,  167,
 /*  1570 */   168,  241,  241,  106,  241,  241,  174,  175,  111,   82,
 /*  1580 */    83,   84,  241,   86,   87,   88,   89,  241,  241,  187,
 /*  1590 */   188,  155,  156,  157,  158,  159,  160,  161,  131,  132,
 /*  1600 */   133,  241,  166,  106,  107,  108,  109,  110,   82,   83,
 /*  1610 */    84,    1,   86,   87,   88,   89,   82,   83,   84,  241,
 /*  1620 */    86,   87,   88,   89,  241,  155,  156,  157,  158,  159,
 /*  1630 */   160,  161,  106,  107,  108,  109,  110,  167,  168,  241,
 /*  1640 */   106,  107,  108,  109,  110,   82,   83,   84,  241,   86,
 /*  1650 */    87,   88,   89,  155,  156,  157,  158,  159,  160,  161,
 /*  1660 */   155,  156,  157,  158,  159,  160,  161,  169,  241,  106,
 /*  1670 */   107,  108,  109,  110,  169,  241,  178,  179,  180,  241,
 /*  1680 */   170,  171,   72,  178,  179,  180,  241,  241,  155,  156,
 /*  1690 */   157,  158,  159,  160,  161,  155,  156,  157,  158,  159,
 /*  1700 */   160,  161,  169,  193,  194,  195,  196,  197,   73,  169,
 /*  1710 */   241,  178,  179,  180,  241,  241,  106,  241,  178,  179,
 /*  1720 */   180,  111,  241,  241,  241,  155,  156,  157,  158,  159,
 /*  1730 */   160,  161,  155,  156,  157,  158,  159,  160,  161,  169,
 /*  1740 */   241,  106,  107,  108,  109,  110,  169,  241,  178,  179,
 /*  1750 */   180,  170,  171,  241,  241,  178,  179,  180,  241,  155,
 /*  1760 */   156,  157,  158,  159,  160,  161,  155,  156,  157,  158,
 /*  1770 */   159,  160,  161,  169,  193,  194,  195,  196,  197,  241,
 /*  1780 */   169,  241,  178,  179,  180,  170,  171,  241,  241,  178,
 /*  1790 */   179,  180,  155,  156,  157,  158,  159,  160,  161,  155,
 /*  1800 */   156,  157,  158,  159,  160,  161,  169,  241,  193,  194,
 /*  1810 */   195,  196,  197,  169,  241,  178,  179,  180,  241,  241,
 /*  1820 */   241,  241,  178,  179,  180,  155,  156,  157,  158,  159,
 /*  1830 */   160,  161,  241,  241,  241,  170,  171,  167,  168,  154,
 /*  1840 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  154,
 /*  1850 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  194,
 /*  1860 */   195,  196,  197,  241,  154,  155,  156,  157,  158,  159,
 /*  1870 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  1880 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  1890 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  1900 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  1910 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  1920 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  1930 */   160,  161,  241,  163,  241,  154,  155,  156,  157,  158,
 /*  1940 */   159,  160,  161,  241,  163,  154,  155,  156,  157,  158,
 /*  1950 */   159,  160,  161,  241,  163,  154,  155,  156,  157,  158,
 /*  1960 */   159,  160,  161,  241,  163,  241,  241,  241,  154,  155,
 /*  1970 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  1980 */   156,  157,  158,  159,  160,  161,  241,  163,  241,  241,
 /*  1990 */   241,  241,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2000 */   241,  163,  241,  154,  155,  156,  157,  158,  159,  160,
 /*  2010 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2020 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2030 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2040 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2050 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2060 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2070 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2080 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2090 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2100 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2110 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2120 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2130 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2140 */   161,  241,  163,  241,  241,  241,  241,  154,  155,  156,
 /*  2150 */   157,  158,  159,  160,  161,  241,  163,  241,  154,  155,
 /*  2160 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2170 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2180 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2190 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2200 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2210 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2220 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2230 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2240 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2250 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2260 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2270 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2280 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2290 */   156,  157,  158,  159,  160,  161,  241,  163,  241,  241,
 /*  2300 */   241,  241,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2310 */   241,  163,  241,  154,  155,  156,  157,  158,  159,  160,
 /*  2320 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2330 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2340 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2350 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2360 */   161,  241,  163,  155,  156,  157,  158,  159,  160,  161,
 /*  2370 */   170,  171,  241,  241,  166,  155,  156,  157,  158,  159,
 /*  2380 */   160,  161,  170,  171,  241,  241,  166,  155,  156,  157,
 /*  2390 */   158,  159,  160,  161,  194,  195,  196,  197,  166,  155,
 /*  2400 */   156,  157,  158,  159,  160,  161,  194,  195,  196,  197,
 /*  2410 */   166,  155,  156,  157,  158,  159,  160,  161,  241,  241,
 /*  2420 */   241,  241,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2430 */   241,  241,  241,  241,  166,  155,  156,  157,  158,  159,
 /*  2440 */   160,  161,  241,  241,  241,  241,  166,  155,  156,  157,
 /*  2450 */   158,  159,  160,  161,  241,  241,  241,  241,  166,  155,
 /*  2460 */   156,  157,  158,  159,  160,  161,  241,  241,  241,  241,
 /*  2470 */   166,  155,  156,  157,  158,  159,  160,  161,  241,  241,
 /*  2480 */   241,  241,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2490 */   241,  241,  241,  241,  166,  155,  156,  157,  158,  159,
 /*  2500 */   160,  161,  241,  241,  241,  241,  166,  241,  155,  156,
 /*  2510 */   157,  158,  159,  160,  161,  241,  241,  241,  241,  166,
 /*  2520 */   155,  156,  157,  158,  159,  160,  161,  241,    1,    2,
 /*  2530 */   241,  166,  155,  156,  157,  158,  159,  160,  161,  241,
 /*  2540 */   241,  241,  241,  166,  155,  156,  157,  158,  159,  160,
 /*  2550 */   161,  241,  241,  241,  241,  166,  155,  156,  157,  158,
 /*  2560 */   159,  160,  161,  241,  241,  241,  241,  166,  155,  156,
 /*  2570 */   157,  158,  159,  160,  161,  241,  241,  241,  241,  166,
 /*  2580 */   155,  156,  157,  158,  159,  160,  161,   74,  241,  241,
 /*  2590 */   241,  166,  155,  156,  157,  158,  159,  160,  161,   72,
 /*  2600 */   241,   26,  241,  166,  155,  156,  157,  158,  159,  160,
 /*  2610 */   161,   74,  241,  241,  241,  166,   41,   42,  241,  106,
 /*  2620 */   107,  108,  109,  110,  241,  241,  241,   95,   96,   97,
 /*  2630 */    98,   99,  241,  106,  102,  241,   61,   62,  111,  241,
 /*  2640 */   241,  241,  241,  106,  107,  108,  109,  110,  241,   74,
 /*  2650 */   241,  241,  241,  241,  241,   80,
};
#define YY_SHIFT_USE_DFLT (-100)
#define YY_SHIFT_COUNT (437)
#define YY_SHIFT_MIN   (-99)
#define YY_SHIFT_MAX   (2575)
static const short yy_shift_ofst[] = {
 /*     0 */  -100,  104,  116,  116,  193,  193,  193,  193,  193,  193,
 /*    10 */   193,  193,  224,  224,  224,  224,  224,  224,  224,  224,
 /*    20 */   224,  224,  193,  193,  193,  193,  193,  193,  193,  193,
 /*    30 */   193,  193,  193,  193,  193,  375,  375,  375,  375,  448,
 /*    40 */   448,  448,  448,  448,  448,  448,  448,  448,  448,  498,
 /*    50 */   498,  498,  498,  498,  498,  498,  498,  498,  498,  498,
 /*    60 */   498,  498,  498,  498,  498,  498,  498,  498,  498,  498,
 /*    70 */   498,  498,  498,  498,  498,  498,  498,  498,  498,  498,
 /*    80 */   498,  498,  498,  498,  498,  498,  498,  498,  498,  498,
 /*    90 */   498,  498,  498,  498,  498,  498,  498,  498,  498,  498,
 /*   100 */   498,  498,  498,  498, 1467, 1467, 1467,  144,  372,  144,
 /*   110 */   144,  144,  144,  144,  144,  144,  144,  144,  144,  144,
 /*   120 */   144,  144,  144,  144,  144,  144,  144,  144,  144,  144,
 /*   130 */   144,  144,  144, 2527, 2527, 2527, 2527,  703, 2527, 2527,
 /*   140 */  2527,  703,  739,  115,  703,  429,  531,  243,  243,  243,
 /*   150 */   243, 1201, 1194, 1610,  828,  828,  531,  531,  531,  531,
 /*   160 */   589,  828,  608,  429,  429, 1201, 1194, 1042,  846,  579,
 /*   170 */   579,  579,  579,  818,  312,  134,  134,  134,  134,  134,
 /*   180 */   134,  134,  134,  134,  134,  134,  299,   90,   35,  589,
 /*   190 */   968,  895,  968,  956,  968,  956, 1037, 1019,  966,  895,
 /*   200 */   968,  956, 1037, 1019,  966,  895,  968,  956, 1037, 1019,
 /*   210 */   966,  968,  956,  968,  956,  968,  956,  968,  956,  968,
 /*   220 */   956, 1037, 1019,  966,  968,  956, 1037, 1019,  966,  968,
 /*   230 */   956, 1037, 1019, 1052, 1052,  922, 1042,  968,  956,  966,
 /*   240 */   968,  956,  895,  968,  956,  845,  845,  966,  895,  885,
 /*   250 */   845,  382,  -41, 1340, 1378, 1348,  231, 1563, 1534, 1526,
 /*   260 */  1497,  387,  735,  688,  476,  627,  106,  378, 2575, 2537,
 /*   270 */   874,  874, 2532, 2532, 2532, 2513, 1385, 1635,  -53,  903,
 /*   280 */   874, 1458,  458,  409, 1211,  252,  155, 1160, 1160, 1160,
 /*   290 */  1160, 1160, 1160, 1160,  747,  747,  -15,  747,  747, 1160,
 /*   300 */  1160, 1160,  747,  747,  747,  747,  747,  747,  747,  747,
 /*   310 */   747,  747,  747,  747,  747,  747,  747,  747,  747,  747,
 /*   320 */   747,  747,  747, 1132, 1104,  747,  747,   73,   73,  161,
 /*   330 */   -36,  -36,   98,   98,  -75,  -75,   91,   91,   97,  -75,
 /*   340 */   -75,  -82,  -82,  -82,  -82,  -82,  -82,  -82,  -82,  549,
 /*   350 */   788,  787,  308,  262,  -99,  -99,  -99,  -99,  717,  187,
 /*   360 */   483,  657,  -28,  464,  628,  610,  459,   51,  -59,  185,
 /*   370 */   411,  342,  -44,  406,   48,  399,  340,  261,   34,  921,
 /*   380 */   912,  911,  905,  904,  886,  884,  883,  877,  876,  875,
 /*   390 */   858,  857,  856,  852,  844,  767,  767,  803,  778,  742,
 /*   400 */   736,  805,  696,  790,  685,  642,  634,  680,  587,  625,
 /*   410 */   583,  606,  588,  537,  413,  469,  438,  426,  373,  390,
 /*   420 */   363,  360,  332,  280,  297,  297,  105,  210,  210,  226,
 /*   430 */   217,   96,  166,  156,  152,  110,   36,   86,
};
#define YY_REDUCE_USE_DFLT (-137)
#define YY_REDUCE_COUNT (250)
#define YY_REDUCE_MIN   (-136)
#define YY_REDUCE_MAX   (2449)
static const short yy_reduce_ofst[] = {
 /*     0 */   701, -136,  211,  -97, 1228, 1200, 1172, 1144, 1116, 1088,
 /*    10 */  1060, 1032, 1004,  976,  948,  920,  892,  864,  836,  807,
 /*    20 */   779,  751,  716,  691,  668,  645,  621,  595,  560,  535,
 /*    30 */   512,  489,  465,  439,  367, 1402, 1368, 1345, 1322, 1644,
 /*    40 */  1637, 1611, 1604, 1577, 1570, 1540, 1533, 1505, 1498,  740,
 /*    50 */   584,  428,  189, 2199, 2189, 2179, 2169, 2159, 2148, 2134,
 /*    60 */  2124, 2114, 2104, 2094, 2084, 2074, 2064, 2054, 2044, 2034,
 /*    70 */  2024, 2014, 2004, 1993, 1979, 1969, 1959, 1949, 1939, 1929,
 /*    80 */  1919, 1909, 1899, 1889, 1879, 1869, 1859, 1849, 1838, 1824,
 /*    90 */  1814, 1801, 1791, 1781, 1770, 1760, 1750, 1740, 1730, 1720,
 /*   100 */  1710, 1695, 1685, 1390, 1670, 1470,  125,  -32,  457, 2449,
 /*   110 */  2437, 2425, 2413, 2401, 2389, 2377, 2365, 2353, 2340, 2328,
 /*   120 */  2316, 2304, 2292, 2280, 2268, 2256, 2244, 2232, 2220, 2208,
 /*   130 */  1436,  301,  150, 1615, 1581, 1510, 1245,   14, 2212, 2200,
 /*   140 */  1665,   62,  228,  114,  324,  219,  111,  370,  351,  230,
 /*   150 */    85,  124,   13,  752,  730,  461,  667,  562,  553,  547,
 /*   160 */   557,  337,  451,  388,   50,  410,  402,  113,  983,  981,
 /*   170 */   978,  977,  975,  952,  950, 1023, 1014, 1013, 1012, 1011,
 /*   180 */  1005, 1003, 1002,  999,  997,  996,  982,  938,  998,  955,
 /*   190 */   932,  986,  931,  926,  918,  924,  925,  919,  908,  970,
 /*   200 */   916,  907,  899,  893,  880,  942,  915,  906,  890,  870,
 /*   210 */   871,  897,  896,  894,  891,  889,  879,  888,  878,  869,
 /*   220 */   867,  863,  865,  866,  861,  849,  862,  851,  854,  841,
 /*   230 */   848,  842,  837,  860,  859,  887,  853,  819,  814,  815,
 /*   240 */   812,  811,  872,  806,  793,  868,  855,  789,  850,  826,
 /*   250 */   829,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   692, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*    10 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*    20 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*    30 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*    40 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*    50 */  1049, 1049, 1049, 1049,  890,  889,  903,  904, 1049, 1049,
 /*    60 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*    70 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*    80 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*    90 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*   100 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*   110 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*   120 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*   130 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*   140 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*   150 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*   160 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1000, 1002,
 /*   170 */  1002, 1002, 1002, 1008, 1000, 1049, 1049, 1049, 1049, 1049,
 /*   180 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1000, 1049, 1049,
 /*   190 */  1008, 1049, 1008, 1006, 1008, 1006, 1002, 1004, 1000, 1049,
 /*   200 */  1008, 1006, 1002, 1004, 1000, 1049, 1008, 1006, 1002, 1004,
 /*   210 */  1000, 1008, 1006, 1008, 1006, 1008, 1006, 1008, 1006, 1008,
 /*   220 */  1006, 1002, 1004, 1000, 1008, 1006, 1002, 1004, 1000, 1008,
 /*   230 */  1006, 1002, 1004, 1049, 1049, 1049, 1049, 1008, 1006, 1000,
 /*   240 */  1008, 1006, 1049, 1008, 1006, 1049, 1049, 1000, 1049, 1049,
 /*   250 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,  847,  847,
 /*   260 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,  756, 1049,
 /*   270 */  1001, 1003,  992,  989,  883, 1049, 1049, 1049, 1049, 1007,
 /*   280 */   999, 1049, 1049, 1049,  880,  801,  857,  869,  868,  867,
 /*   290 */   866,  865,  864,  863,  892,  891,  818,  905,  906,  872,
 /*   300 */   729,  730,  833,  832,  831,  830,  829,  828,  827,  849,
 /*   310 */   846,  845,  844,  843,  842,  841,  840,  839,  838,  837,
 /*   320 */   836,  835,  834, 1049, 1049,  726,  725,  882,  881, 1049,
 /*   330 */   806,  807,  859,  858,  783,  782,  820,  819,  898,  796,
 /*   340 */   797,  758,  757,  763,  762,  768,  767,  742,  743, 1049,
 /*   350 */  1049,  802, 1049, 1049,  969,  973,  972,  970, 1049, 1049,
 /*   360 */  1049, 1049, 1049, 1049, 1049, 1049, 1049,  918, 1049, 1049,
 /*   370 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*   380 */  1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049, 1049,
 /*   390 */  1049, 1049, 1049, 1049,  751,  879,  878, 1049, 1049, 1049,
 /*   400 */  1049, 1049,  971, 1049,  959,  936,  938, 1049, 1049,  951,
 /*   410 */   934, 1049, 1049, 1049,  933,  924,  925, 1049, 1049, 1049,
 /*   420 */  1049, 1049, 1049, 1049,  856,  855,  847,  817,  816, 1049,
 /*   430 */  1049,  870,  728,  724,  722,  718,  715, 1049, 1048, 1047,
 /*   440 */  1046, 1045, 1044, 1043, 1042, 1041, 1040, 1039, 1038, 1037,
 /*   450 */  1036, 1035, 1034, 1033, 1028, 1027, 1029, 1026, 1025, 1024,
 /*   460 */  1023, 1022, 1021, 1020, 1019, 1018, 1017, 1016, 1015, 1014,
 /*   470 */  1013, 1012, 1011, 1010, 1009,  985,  984,  991,  990,  998,
 /*   480 */   997,  994,  993,  988,  996,  874,  876,  877,  875,  873,
 /*   490 */   754,  987,  986,  980,  979,  981,  978,  983,  982,  977,
 /*   500 */   975,  974,  976,  968,  963,  966,  967,  965,  964,  962,
 /*   510 */   954,  957,  961,  960,  958,  956,  955,  953,  929,  937,
 /*   520 */   939,  952,  950,  949,  948,  947,  946,  945,  944,  943,
 /*   530 */   942,  941,  940,  935,  917,  916,  932,  931,  927,  926,
 /*   540 */   923,  922,  921,  718,  920,  919,  808,  810,  809,  805,
 /*   550 */   804,  803,  802,  930,  928,  908,  911,  912,  915,  914,
 /*   560 */   913,  910,  909,  907, 1032, 1031, 1030,  851,  853,  862,
 /*   570 */   861,  860,  854,  852,  850,  781,  780,  779,  778,  777,
 /*   580 */   776,  786,  785,  784,  775,  774,  773,  772, 1005,  812,
 /*   590 */   814,  885,  888,  887,  886,  884,  826,  825,  824,  823,
 /*   600 */   822,  821,  815,  813,  811,  754,  901,  900,  899,  898,
 /*   610 */   897,  848,  871,  798,  800,  799,  795,  794,  793,  792,
 /*   620 */   791,  790,  789,  788,  787,  727,  895,  894,  896,  893,
 /*   630 */   761,  760,  759,  766,  765,  764,  756,  755,  754,  753,
 /*   640 */   752,  751,  771,  770,  769,  750,  749,  748,  747,  744,
 /*   650 */   746,  745,  741,  740,  739,  738,  737,  736,  735,  734,
 /*   660 */   733,  732,  731,  723,  721,  720,  719,  717,  716,  714,
 /*   670 */   710,  709,  713,  712,  711,  708,  707,  706,  705,  704,
 /*   680 */   703,  702,  701,  700,  699,  698,  697,  696,  695,  694,
 /*   690 */   693,
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
 /*  28 */ "object ::= undeclared",
 /*  29 */ "variable ::= VARIABLE_ID",
 /*  30 */ "lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  31 */ "lua ::= AT_IDENTIFIER",
 /*  32 */ "undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  33 */ "undeclared ::= IDENTIFIER",
 /*  34 */ "term_lst ::= term",
 /*  35 */ "term_lst ::= term_lst COMMA term",
 /*  36 */ "constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R",
 /*  37 */ "constant_one_const ::= CONSTANT_ID",
 /*  38 */ "term_no_const_lst ::= term_no_const",
 /*  39 */ "term_no_const_lst ::= term_no_const_lst COMMA term_no_const",
 /*  40 */ "term ::= base_elem",
 /*  41 */ "term ::= INTEGER",
 /*  42 */ "term ::= STRING_LITERAL",
 /*  43 */ "term ::= PAREN_L term PAREN_R",
 /*  44 */ "term ::= TRUE",
 /*  45 */ "term ::= FALSE",
 /*  46 */ "term ::= MAXSTEP",
 /*  47 */ "term ::= MAXADDITIVE",
 /*  48 */ "term ::= MAXAFVALUE",
 /*  49 */ "term ::= DASH term",
 /*  50 */ "term ::= ABS term",
 /*  51 */ "term ::= term DASH term",
 /*  52 */ "term ::= term PLUS term",
 /*  53 */ "term ::= term STAR term",
 /*  54 */ "term ::= term INT_DIV term",
 /*  55 */ "term ::= term MOD term",
 /*  56 */ "term_strong ::= base_elem_no_const",
 /*  57 */ "term_strong ::= INTEGER",
 /*  58 */ "term_strong ::= STRING_LITERAL",
 /*  59 */ "term_strong ::= PAREN_L term_strong PAREN_R",
 /*  60 */ "term_strong ::= MAXSTEP",
 /*  61 */ "term_strong ::= MAXADDITIVE",
 /*  62 */ "term_strong ::= MAXAFVALUE",
 /*  63 */ "term_strong ::= DASH term_strong",
 /*  64 */ "term_strong ::= ABS term",
 /*  65 */ "term_strong_candidate ::= DASH constant",
 /*  66 */ "term_strong ::= term_strong_candidate DASH term",
 /*  67 */ "term_strong ::= term_strong_candidate PLUS term",
 /*  68 */ "term_strong ::= term_strong_candidate STAR term",
 /*  69 */ "term_strong ::= term_strong_candidate INT_DIV term",
 /*  70 */ "term_strong ::= term_strong_candidate MOD term",
 /*  71 */ "term_strong ::= constant DASH term",
 /*  72 */ "term_strong ::= constant PLUS term",
 /*  73 */ "term_strong ::= constant STAR term",
 /*  74 */ "term_strong ::= constant INT_DIV term",
 /*  75 */ "term_strong ::= constant MOD term",
 /*  76 */ "term_strong ::= term_strong DASH term",
 /*  77 */ "term_strong ::= term_strong PLUS term",
 /*  78 */ "term_strong ::= term_strong STAR term",
 /*  79 */ "term_strong ::= term_strong INT_DIV term",
 /*  80 */ "term_strong ::= term_strong MOD term",
 /*  81 */ "term_no_const_strong ::= base_elem_no_const",
 /*  82 */ "term_no_const_strong ::= INTEGER",
 /*  83 */ "term_no_const_strong ::= STRING_LITERAL",
 /*  84 */ "term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R",
 /*  85 */ "term_no_const_strong ::= MAXSTEP",
 /*  86 */ "term_no_const_strong ::= MAXADDITIVE",
 /*  87 */ "term_no_const_strong ::= MAXAFVALUE",
 /*  88 */ "term_no_const_strong ::= constant",
 /*  89 */ "term_no_const_strong ::= DASH term_no_const_strong",
 /*  90 */ "term_no_const_strong ::= ABS term_no_const",
 /*  91 */ "term_no_const_strong ::= term_no_const_strong DASH term_no_const",
 /*  92 */ "term_no_const_strong ::= term_no_const_strong PLUS term_no_const",
 /*  93 */ "term_no_const_strong ::= term_no_const_strong STAR term_no_const",
 /*  94 */ "term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const",
 /*  95 */ "term_no_const_strong ::= term_no_const_strong MOD term_no_const",
 /*  96 */ "term_no_const ::= base_elem_no_const",
 /*  97 */ "term_no_const ::= INTEGER",
 /*  98 */ "term_no_const ::= STRING_LITERAL",
 /*  99 */ "term_no_const ::= PAREN_L term_no_const PAREN_R",
 /* 100 */ "term_no_const ::= TRUE",
 /* 101 */ "term_no_const ::= FALSE",
 /* 102 */ "term_no_const ::= constant",
 /* 103 */ "term_no_const ::= DASH term_no_const",
 /* 104 */ "term_no_const ::= ABS term_no_const",
 /* 105 */ "term_no_const ::= term_no_const DASH term_no_const",
 /* 106 */ "term_no_const ::= term_no_const PLUS term_no_const",
 /* 107 */ "term_no_const ::= term_no_const STAR term_no_const",
 /* 108 */ "term_no_const ::= term_no_const INT_DIV term_no_const",
 /* 109 */ "term_no_const ::= term_no_const MOD term_no_const",
 /* 110 */ "num_range ::= term_numeric DBL_PERIOD term_numeric",
 /* 111 */ "term_numeric ::= INTEGER",
 /* 112 */ "term_numeric ::= PAREN_L term_numeric PAREN_R",
 /* 113 */ "term_numeric ::= DASH term_numeric",
 /* 114 */ "term_numeric ::= ABS term_numeric",
 /* 115 */ "term_numeric ::= term_numeric DASH term_numeric",
 /* 116 */ "term_numeric ::= term_numeric PLUS term_numeric",
 /* 117 */ "term_numeric ::= term_numeric STAR term_numeric",
 /* 118 */ "term_numeric ::= term_numeric INT_DIV term_numeric",
 /* 119 */ "term_numeric ::= term_numeric MOD term_numeric",
 /* 120 */ "formula ::= formula_base",
 /* 121 */ "formula ::= PAREN_L formula PAREN_R",
 /* 122 */ "formula ::= NOT formula",
 /* 123 */ "formula ::= DASH formula",
 /* 124 */ "formula ::= formula AMP formula",
 /* 125 */ "formula ::= formula DBL_PLUS formula",
 /* 126 */ "formula ::= formula PIPE formula",
 /* 127 */ "formula ::= formula EQUIV formula",
 /* 128 */ "formula ::= formula IMPL formula",
 /* 129 */ "formula ::= formula ARROW_RDASH formula",
 /* 130 */ "formula_base ::= comparison",
 /* 131 */ "formula_base ::= atomic_formula",
 /* 132 */ "formula_base ::= formula_quant",
 /* 133 */ "formula_base ::= formula_card",
 /* 134 */ "formula_base ::= TRUE",
 /* 135 */ "formula_base ::= FALSE",
 /* 136 */ "comparison ::= term_strong EQ term",
 /* 137 */ "comparison ::= term_strong DBL_EQ term",
 /* 138 */ "comparison ::= term_strong NEQ term",
 /* 139 */ "comparison ::= term_strong LTHAN term",
 /* 140 */ "comparison ::= term_strong GTHAN term",
 /* 141 */ "comparison ::= term_strong LTHAN_EQ term",
 /* 142 */ "comparison ::= term_strong GTHAN_EQ term",
 /* 143 */ "comparison ::= term_strong_candidate EQ term",
 /* 144 */ "comparison ::= term_strong_candidate DBL_EQ term",
 /* 145 */ "comparison ::= term_strong_candidate NEQ term",
 /* 146 */ "comparison ::= term_strong_candidate LTHAN term",
 /* 147 */ "comparison ::= term_strong_candidate GTHAN term",
 /* 148 */ "comparison ::= term_strong_candidate LTHAN_EQ term",
 /* 149 */ "comparison ::= term_strong_candidate GTHAN_EQ term",
 /* 150 */ "comparison ::= constant DBL_EQ term",
 /* 151 */ "comparison ::= constant NEQ term",
 /* 152 */ "comparison ::= constant LTHAN term",
 /* 153 */ "comparison ::= constant GTHAN term",
 /* 154 */ "comparison ::= constant LTHAN_EQ term",
 /* 155 */ "comparison ::= constant GTHAN_EQ term",
 /* 156 */ "atomic_formula ::= constant",
 /* 157 */ "atomic_formula ::= TILDE constant",
 /* 158 */ "atomic_formula ::= constant EQ term",
 /* 159 */ "formula_no_const ::= formula_no_const_base",
 /* 160 */ "formula_no_const ::= PAREN_L formula_no_const PAREN_R",
 /* 161 */ "formula_no_const ::= NOT formula_no_const",
 /* 162 */ "formula_no_const ::= DASH formula_no_const",
 /* 163 */ "formula_no_const ::= formula_no_const AMP formula_no_const",
 /* 164 */ "formula_no_const ::= formula_no_const DBL_PLUS formula_no_const",
 /* 165 */ "formula_no_const ::= formula_no_const PIPE formula_no_const",
 /* 166 */ "formula_no_const ::= formula_no_const EQUIV formula_no_const",
 /* 167 */ "formula_no_const ::= formula_no_const IMPL formula_no_const",
 /* 168 */ "formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const",
 /* 169 */ "formula_no_const_base ::= comparison_no_const",
 /* 170 */ "formula_no_const_base ::= TRUE",
 /* 171 */ "formula_no_const_base ::= FALSE",
 /* 172 */ "comparison_no_const ::= term_no_const_strong EQ term_no_const",
 /* 173 */ "comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const",
 /* 174 */ "comparison_no_const ::= term_no_const_strong NEQ term_no_const",
 /* 175 */ "comparison_no_const ::= term_no_const_strong LTHAN term_no_const",
 /* 176 */ "comparison_no_const ::= term_no_const_strong GTHAN term_no_const",
 /* 177 */ "comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const",
 /* 178 */ "comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const",
 /* 179 */ "atomic_formula_one_const ::= constant_one_const",
 /* 180 */ "atomic_formula_one_const ::= TILDE constant_one_const",
 /* 181 */ "atomic_formula_one_const ::= constant_one_const EQ term_no_const",
 /* 182 */ "formula_temporal ::= formula_base",
 /* 183 */ "formula_temporal ::= PAREN_L formula_temporal PAREN_R",
 /* 184 */ "formula_temporal ::= NOT formula_temporal",
 /* 185 */ "formula_temporal ::= DASH formula_temporal",
 /* 186 */ "formula_temporal ::= formula_temporal AMP formula_temporal",
 /* 187 */ "formula_temporal ::= formula_temporal DBL_PLUS formula_temporal",
 /* 188 */ "formula_temporal ::= formula_temporal PIPE formula_temporal",
 /* 189 */ "formula_temporal ::= formula_temporal EQUIV formula_temporal",
 /* 190 */ "formula_temporal ::= formula_temporal IMPL formula_temporal",
 /* 191 */ "formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal",
 /* 192 */ "formula_temporal ::= term_strong COLON formula_temporal",
 /* 193 */ "formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R",
 /* 194 */ "quant_lst ::= quant_op variable",
 /* 195 */ "quant_lst ::= quant_lst quant_op variable",
 /* 196 */ "quant_op ::= BIG_CONJ",
 /* 197 */ "quant_op ::= BIG_DISJ",
 /* 198 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 199 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 200 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 201 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 202 */ "card_var_lst ::= card_var_lst_inner PIPE",
 /* 203 */ "card_var_lst ::= PIPE",
 /* 204 */ "card_var_lst_inner ::= variable",
 /* 205 */ "card_var_lst_inner ::= card_var_lst_inner COMMA variable",
 /* 206 */ "head_formula ::= comparison",
 /* 207 */ "head_formula ::= atomic_formula",
 /* 208 */ "head_formula ::= formula_smpl_card",
 /* 209 */ "head_formula ::= TRUE",
 /* 210 */ "head_formula ::= FALSE",
 /* 211 */ "head_formula ::= DASH constant",
 /* 212 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 213 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 214 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 215 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 216 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 217 */ "macro_def_lst ::= macro_bnd",
 /* 218 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 219 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 220 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 221 */ "macro_args ::= macro_arg",
 /* 222 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 223 */ "macro_arg ::= POUND_INTEGER",
 /* 224 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 225 */ "sort_lst ::= sort",
 /* 226 */ "sort_lst ::= sort_lst COMMA sort",
 /* 227 */ "sort ::= sort_id_nr",
 /* 228 */ "sort ::= sort_id_nr STAR",
 /* 229 */ "sort ::= sort_id_nr CARROT",
 /* 230 */ "sort ::= sort_nr PLUS object_nullary",
 /* 231 */ "sort ::= sort_id PLUS object_nullary",
 /* 232 */ "sort ::= sort_id PLUS INTEGER",
 /* 233 */ "sort_id_nr ::= sort_id",
 /* 234 */ "sort_id_nr ::= sort_nr",
 /* 235 */ "sort_nr ::= num_range",
 /* 236 */ "sort_id ::= IDENTIFIER",
 /* 237 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 238 */ "constant_bnd_lst ::= constant_bnd",
 /* 239 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 240 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 241 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 242 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 243 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 244 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 245 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 246 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 247 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 248 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 249 */ "constant_dcl_type ::= ABACTION",
 /* 250 */ "constant_dcl_type ::= ACTION",
 /* 251 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 252 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 253 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 254 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 255 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 256 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 257 */ "constant_dcl_type ::= RIGID",
 /* 258 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 259 */ "constant_dcl_type ::= SDFLUENT",
 /* 260 */ "attrib_spec ::= ATTRIBUTE",
 /* 261 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 262 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 263 */ "object_bnd_lst ::= object_bnd",
 /* 264 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 265 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 266 */ "object_lst ::= object_spec",
 /* 267 */ "object_lst ::= object_lst COMMA object_spec",
 /* 268 */ "object_spec ::= IDENTIFIER",
 /* 269 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 270 */ "object_spec ::= num_range",
 /* 271 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 272 */ "variable_bnd_lst ::= variable_bnd",
 /* 273 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 274 */ "variable_bnd ::= variable_lst DBL_COLON sort_id",
 /* 275 */ "variable_lst ::= IDENTIFIER",
 /* 276 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 277 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 278 */ "sort_bnd_lst ::= sort_bnd",
 /* 279 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 280 */ "sort_bnd ::= sort_dcl_lst",
 /* 281 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 282 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 283 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 284 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 285 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 286 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 287 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 288 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 289 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 290 */ "show_lst ::= show_elem",
 /* 291 */ "show_lst ::= show_lst COMMA show_elem",
 /* 292 */ "show_elem ::= atomic_formula_one_const",
 /* 293 */ "stmt_noconcurrency ::= NOCONCURRENCY",
 /* 294 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY",
 /* 295 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD",
 /* 296 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD",
 /* 297 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 298 */ "query_lst ::= formula_temporal",
 /* 299 */ "query_lst ::= query_maxstep_decl",
 /* 300 */ "query_lst ::= query_label_decl",
 /* 301 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 302 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 303 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 304 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 305 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range",
 /* 306 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 307 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 308 */ "clause_if ::= IF formula",
 /* 309 */ "clause_if ::=",
 /* 310 */ "clause_after ::= AFTER formula",
 /* 311 */ "clause_after ::=",
 /* 312 */ "clause_ifcons ::= IFCONS formula",
 /* 313 */ "clause_ifcons ::=",
 /* 314 */ "clause_unless ::= UNLESS atomic_formula",
 /* 315 */ "clause_unless ::=",
 /* 316 */ "clause_where ::= WHERE formula_no_const",
 /* 317 */ "clause_where ::=",
 /* 318 */ "stmt_law ::= law_basic",
 /* 319 */ "stmt_law ::= law_caused",
 /* 320 */ "stmt_law ::= law_pcaused",
 /* 321 */ "stmt_law ::= law_impl",
 /* 322 */ "stmt_law ::= law_causes",
 /* 323 */ "stmt_law ::= law_increments",
 /* 324 */ "stmt_law ::= law_mcause",
 /* 325 */ "stmt_law ::= law_always",
 /* 326 */ "stmt_law ::= law_constraint",
 /* 327 */ "stmt_law ::= law_impossible",
 /* 328 */ "stmt_law ::= law_never",
 /* 329 */ "stmt_law ::= law_default",
 /* 330 */ "stmt_law ::= law_exogenous",
 /* 331 */ "stmt_law ::= law_inertial",
 /* 332 */ "stmt_law ::= law_nonexecutable",
 /* 333 */ "stmt_law ::= law_rigid",
 /* 334 */ "stmt_law ::= law_observed",
 /* 335 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 336 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 337 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 338 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 339 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 340 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 341 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 342 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 343 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 344 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 345 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 346 */ "law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 347 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 348 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 349 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 350 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 351 */ "law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD",
 /* 352 */ "stmt_code_blk ::= ASP_GR",
 /* 353 */ "stmt_code_blk ::= ASP_CP",
 /* 354 */ "stmt_code_blk ::= F2LP_GR",
 /* 355 */ "stmt_code_blk ::= F2LP_CP",
 /* 356 */ "stmt_code_blk ::= LUA_GR",
 /* 357 */ "stmt_code_blk ::= LUA_CP",
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
#line 2265 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 137: /* start */
    case 138: /* statement_lst */
    case 161: /* undeclared */
{
#line 208 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2274 "bcplus/parser/detail/lemon_parser.c"
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
#line 2287 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_macro_def */
{
#line 233 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy373));								
#line 2294 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_constant_def */
{
#line 235 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy353));								
#line 2301 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_object_def */
{
#line 237 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy172));								
#line 2308 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_variable_def */
{
#line 239 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy43));								
#line 2315 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 144: /* stmt_sort_def */
{
#line 241 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy339));								
#line 2322 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_noconcurrency */
{
#line 251 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy451));								
#line 2329 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 150: /* stmt_strong_noconcurrency */
{
#line 253 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy352));								
#line 2336 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* stmt_query */
{
#line 259 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy134));								
#line 2343 "bcplus/parser/detail/lemon_parser.c"
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
#line 2356 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* constant */
    case 164: /* constant_one_const */
{
#line 297 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy357));								
#line 2364 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 157: /* object */
    case 158: /* object_nullary */
{
#line 299 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy398));								
#line 2372 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* variable */
{
#line 303 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy313));								
#line 2379 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 160: /* lua */
{
#line 305 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy105));								
#line 2386 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 162: /* term_lst */
    case 165: /* term_no_const_lst */
{
#line 309 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy67));								
#line 2394 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 170: /* num_range */
{
#line 637 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy475));								
#line 2401 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* term_numeric */
{
#line 639 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy468));								
#line 2408 "bcplus/parser/detail/lemon_parser.c"
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
#line 700 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy374));								
#line 2423 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 175: /* atomic_formula */
    case 181: /* atomic_formula_one_const */
{
#line 706 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy6));								
#line 2431 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 176: /* formula_quant */
{
#line 708 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy155));								
#line 2438 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 183: /* quant_lst */
{
#line 904 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy358));								
#line 2445 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 184: /* quant_op */
{
#line 906 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2452 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* card_var_lst */
    case 186: /* card_var_lst_inner */
{
#line 943 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy279));								
#line 2460 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 188: /* formula_smpl_card */
{
#line 1019 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy73));								
#line 2467 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 189: /* macro_def_lst */
{
#line 1059 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy381));                              
#line 2474 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 190: /* macro_bnd */
{
#line 1061 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy83));                              
#line 2481 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 191: /* macro_args */
{
#line 1063 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy198));                              
#line 2488 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 192: /* macro_arg */
{
#line 1065 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy340));                              
#line 2495 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 193: /* sort_lst */
{
#line 1155 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy151));							
#line 2502 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 194: /* sort */
    case 195: /* sort_id_nr */
    case 196: /* sort_nr */
    case 197: /* sort_id */
{
#line 1157 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2512 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 198: /* constant_bnd_lst */
    case 199: /* constant_bnd */
{
#line 1297 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy77));									
#line 2520 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* constant_dcl_lst */
{
#line 1301 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy316));									
#line 2527 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* constant_dcl_type */
{
#line 1303 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2534 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* attrib_spec */
{
#line 1305 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2541 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* object_bnd_lst */
{
#line 1640 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy84));									
#line 2548 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* object_bnd */
{
#line 1642 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy180));									
#line 2555 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* object_lst */
    case 206: /* object_spec */
{
#line 1644 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy299));									
#line 2563 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 207: /* variable_bnd_lst */
    case 208: /* variable_bnd */
{
#line 1754 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy326));									
#line 2571 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 209: /* variable_lst */
{
#line 1758 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy58));									
#line 2578 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* sort_bnd_lst */
    case 211: /* sort_bnd */
    case 212: /* sort_dcl_lst */
{
#line 1831 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy233));									
#line 2587 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* show_lst */
{
#line 1935 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy345));									
#line 2594 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* show_elem */
    case 222: /* clause_unless */
{
#line 1937 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy6));									
#line 2602 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* query_lst */
{
#line 2082 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy165).l); DEALLOC((yypminor->yy165).maxstep); DEALLOC((yypminor->yy165).label);	
#line 2609 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* query_maxstep_decl */
{
#line 2084 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy234));												
#line 2616 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 218: /* query_label_Decl */
{
#line 2086 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2623 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* clause_if */
    case 220: /* clause_after */
    case 221: /* clause_ifcons */
    case 223: /* clause_where */
{
#line 2236 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy374));									
#line 2633 "bcplus/parser/detail/lemon_parser.c"
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
#line 2277 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy334));									
#line 2656 "bcplus/parser/detail/lemon_parser.c"
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
  { 157, 1 },
  { 159, 1 },
  { 160, 4 },
  { 160, 1 },
  { 161, 4 },
  { 161, 1 },
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
#line 3319 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 219 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy334;
			yymsp[0].minor.yy334  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3328 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 262 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy373; }
#line 3333 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy353; }
#line 3338 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy172; }
#line 3343 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy43; }
#line 3348 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy339; }
#line 3353 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy334; }
#line 3363 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 271 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy451; }
#line 3368 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy352; }
#line 3373 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 275 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy134; }
#line 3378 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 319 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy357; }
#line 3383 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 40: /* term ::= base_elem */ yytestcase(yyruleno==40);
      case 56: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==56);
      case 81: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==81);
      case 96: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==96);
#line 320 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy113; }
#line 3392 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy398;	}
#line 3397 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 323 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy313; }
#line 3402 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 324 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy105; }
#line 3407 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 36: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==36);
#line 378 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy357, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3413 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 37: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==37);
#line 379 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy357, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3419 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 380 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy398, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3424 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* object ::= object_nullary */
#line 381 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy398 = yymsp[0].minor.yy398; }
#line 3429 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object_nullary ::= OBJECT_ID */
#line 382 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy398, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3434 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= undeclared */
#line 383 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3439 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* variable ::= VARIABLE_ID */
#line 386 "bcplus/parser/detail/lemon_parser.y"
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
#line 3454 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 397 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy105, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0); }
#line 3459 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* lua ::= AT_IDENTIFIER */
#line 398 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy105, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3464 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 399 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy49, yymsp[-3].minor.yy0, yymsp[-1].minor.yy67);   yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3471 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* undeclared ::= IDENTIFIER */
#line 400 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy49, yymsp[0].minor.yy0, NULL); }
#line 3476 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* term_lst ::= term */
      case 38: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==38);
#line 403 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy67 = new TermList();
			yygotominor.yy67->push_back(yymsp[0].minor.yy113);
		}
#line 3485 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* term_lst ::= term_lst COMMA term */
      case 39: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==39);
#line 409 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy67 = yymsp[-2].minor.yy67;
			yymsp[-2].minor.yy67->push_back(yymsp[0].minor.yy113);
		  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 3495 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 41: /* term ::= INTEGER */
      case 57: /* term_strong ::= INTEGER */ yytestcase(yyruleno==57);
      case 82: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==82);
      case 97: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==97);
#line 508 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy113, yymsp[0].minor.yy0);	}
#line 3503 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 42: /* term ::= STRING_LITERAL */
      case 44: /* term ::= TRUE */ yytestcase(yyruleno==44);
      case 45: /* term ::= FALSE */ yytestcase(yyruleno==45);
      case 58: /* term_strong ::= STRING_LITERAL */ yytestcase(yyruleno==58);
      case 83: /* term_no_const_strong ::= STRING_LITERAL */ yytestcase(yyruleno==83);
      case 98: /* term_no_const ::= STRING_LITERAL */ yytestcase(yyruleno==98);
      case 100: /* term_no_const ::= TRUE */ yytestcase(yyruleno==100);
      case 101: /* term_no_const ::= FALSE */ yytestcase(yyruleno==101);
#line 509 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy113, yymsp[0].minor.yy0); }
#line 3515 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 43: /* term ::= PAREN_L term PAREN_R */
      case 59: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==99);
#line 510 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy113, yymsp[-2].minor.yy0, yymsp[-1].minor.yy113, yymsp[0].minor.yy0); }
#line 3523 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 46: /* term ::= MAXSTEP */
      case 60: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==60);
      case 85: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==85);
#line 513 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3530 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 47: /* term ::= MAXADDITIVE */
      case 61: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==61);
      case 86: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==86);
#line 514 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3537 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 48: /* term ::= MAXAFVALUE */
      case 62: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==87);
#line 515 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3544 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= DASH term */
      case 63: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==63);
      case 89: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==89);
      case 103: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==103);
#line 519 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, UnaryTerm::Operator::NEGATIVE); }
#line 3552 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= ABS term */
      case 64: /* term_strong ::= ABS term */ yytestcase(yyruleno==64);
      case 90: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==90);
      case 104: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==104);
#line 520 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, UnaryTerm::Operator::ABS); }
#line 3560 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= term DASH term */
      case 66: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==66);
      case 76: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==76);
      case 91: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==91);
      case 105: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==105);
#line 524 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MINUS); }
#line 3569 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= term PLUS term */
      case 67: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==67);
      case 77: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==77);
      case 92: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==92);
      case 106: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==106);
#line 525 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::PLUS); }
#line 3578 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= term STAR term */
      case 68: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==68);
      case 78: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==78);
      case 93: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==93);
      case 107: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==107);
#line 526 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::TIMES); }
#line 3587 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= term INT_DIV term */
      case 69: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==69);
      case 79: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==79);
      case 94: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==94);
      case 108: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==108);
#line 527 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::DIVIDE); }
#line 3596 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= term MOD term */
      case 70: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==70);
      case 80: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==80);
      case 95: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==95);
      case 109: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==109);
#line 528 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MOD); }
#line 3605 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 65: /* term_strong_candidate ::= DASH constant */
#line 547 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy357, UnaryTerm::Operator::NEGATIVE); }
#line 3610 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 71: /* term_strong ::= constant DASH term */
#line 556 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MINUS); }
#line 3615 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 72: /* term_strong ::= constant PLUS term */
#line 557 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::PLUS); }
#line 3620 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 73: /* term_strong ::= constant STAR term */
#line 558 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::TIMES); }
#line 3625 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant INT_DIV term */
#line 559 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::DIVIDE); }
#line 3630 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant MOD term */
#line 560 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MOD); }
#line 3635 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 88: /* term_no_const_strong ::= constant */
#line 582 "bcplus/parser/detail/lemon_parser.y"
{
		// error handling for constants so they don'yygotominor.yy113 default to undeclared identifiers
		yygotominor.yy113 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy357;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy357->beginLoc());
		YYERROR;
	}
#line 3646 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 102: /* term_no_const ::= constant */
#line 612 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy113 default to undeclared identifiers
		yygotominor.yy113 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy357;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy357->beginLoc());
		YYERROR;
	}
#line 3657 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 110: /* num_range ::= term_numeric DBL_PERIOD term_numeric */
#line 642 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy468, r_ptr = yymsp[0].minor.yy468, s_ptr = yymsp[-1].minor.yy0;

	yygotominor.yy475 = new NumberRange(yymsp[-2].minor.yy468->val(), yymsp[0].minor.yy468->val(), yymsp[-2].minor.yy468->beginLoc(), yymsp[0].minor.yy468->endLoc());

}
#line 3667 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 111: /* term_numeric ::= INTEGER */
#line 650 "bcplus/parser/detail/lemon_parser.y"
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
#line 3683 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 112: /* term_numeric ::= PAREN_L term_numeric PAREN_R */
#line 663 "bcplus/parser/detail/lemon_parser.y"
{ 
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy468 = yymsp[-1].minor.yy468;  
	yygotominor.yy468->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy468->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 3693 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 113: /* term_numeric ::= DASH term_numeric */
#line 683 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy468, yymsp[0].minor.yy468, -1 * yymsp[0].minor.yy468->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3699 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 114: /* term_numeric ::= ABS term_numeric */
#line 684 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy468, yymsp[0].minor.yy468, yymsp[0].minor.yy468->val() < 0 ? - yymsp[0].minor.yy468->val() : yymsp[0].minor.yy468->val());   yy_destructor(yypParser,111,&yymsp[-1].minor);
}
#line 3705 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 115: /* term_numeric ::= term_numeric DASH term_numeric */
#line 686 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() - yymsp[0].minor.yy468->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3711 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 116: /* term_numeric ::= term_numeric PLUS term_numeric */
#line 687 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() + yymsp[0].minor.yy468->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 3717 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 117: /* term_numeric ::= term_numeric STAR term_numeric */
#line 688 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() * yymsp[0].minor.yy468->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 3723 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 118: /* term_numeric ::= term_numeric INT_DIV term_numeric */
#line 689 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() / yymsp[0].minor.yy468->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 3729 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 119: /* term_numeric ::= term_numeric MOD term_numeric */
#line 690 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() % yymsp[0].minor.yy468->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 3735 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 120: /* formula ::= formula_base */
      case 159: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==159);
      case 182: /* formula_temporal ::= formula_base */ yytestcase(yyruleno==182);
#line 746 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy374;				}
#line 3742 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 121: /* formula ::= PAREN_L formula PAREN_R */
      case 160: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==160);
      case 183: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==183);
#line 747 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[-1].minor.yy374; yygotominor.yy374->parens(true); 	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3751 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 122: /* formula ::= NOT formula */
      case 161: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==161);
      case 184: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==184);
#line 748 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 3758 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 123: /* formula ::= DASH formula */
      case 162: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==162);
      case 185: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==185);
#line 749 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 3765 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 124: /* formula ::= formula AMP formula */
      case 163: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==163);
      case 186: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==186);
#line 750 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy374, yymsp[0].minor.yy374, yymsp[-2].minor.yy374->beginLoc(), yymsp[0].minor.yy374->endLoc());   yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3773 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 125: /* formula ::= formula DBL_PLUS formula */
      case 126: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==126);
      case 164: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==164);
      case 165: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==165);
      case 187: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==187);
      case 188: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==188);
#line 751 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::OR); }
#line 3783 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 127: /* formula ::= formula EQUIV formula */
      case 166: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==166);
      case 189: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==189);
#line 753 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::EQUIV); }
#line 3790 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 128: /* formula ::= formula IMPL formula */
      case 129: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==129);
      case 167: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==167);
      case 168: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==168);
      case 190: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==190);
      case 191: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==191);
#line 754 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::IMPL); }
#line 3800 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* formula_base ::= comparison */
      case 169: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==169);
      case 206: /* head_formula ::= comparison */ yytestcase(yyruleno==206);
#line 757 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy374; }
#line 3807 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 131: /* formula_base ::= atomic_formula */
      case 207: /* head_formula ::= atomic_formula */ yytestcase(yyruleno==207);
#line 758 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy6; }
#line 3813 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* formula_base ::= formula_quant */
#line 759 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy155; }
#line 3818 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* formula_base ::= formula_card */
#line 761 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy374 = yymsp[0].minor.yy374;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy374->beginLoc());
			YYERROR;
		}
	}
#line 3829 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* formula_base ::= TRUE */
      case 170: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==170);
      case 209: /* head_formula ::= TRUE */ yytestcase(yyruleno==209);
#line 768 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3836 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* formula_base ::= FALSE */
      case 171: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==171);
      case 210: /* head_formula ::= FALSE */ yytestcase(yyruleno==210);
#line 769 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3843 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* comparison ::= term_strong EQ term */
      case 143: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==143);
      case 172: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==172);
#line 771 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3851 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* comparison ::= term_strong DBL_EQ term */
      case 144: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==144);
      case 173: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==173);
#line 772 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* comparison ::= term_strong NEQ term */
      case 145: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==145);
      case 174: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==174);
#line 773 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3867 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* comparison ::= term_strong LTHAN term */
      case 146: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==146);
      case 175: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==175);
#line 774 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3875 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* comparison ::= term_strong GTHAN term */
      case 147: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==147);
      case 176: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==176);
#line 775 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3883 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* comparison ::= term_strong LTHAN_EQ term */
      case 148: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==148);
      case 177: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==177);
#line 776 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3891 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* comparison ::= term_strong GTHAN_EQ term */
      case 149: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==149);
      case 178: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==178);
#line 777 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 3899 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 150: /* comparison ::= constant DBL_EQ term */
#line 785 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3905 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 151: /* comparison ::= constant NEQ term */
#line 786 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3911 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* comparison ::= constant LTHAN term */
#line 787 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3917 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* comparison ::= constant GTHAN term */
#line 788 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3923 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* comparison ::= constant LTHAN_EQ term */
#line 789 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3929 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* comparison ::= constant GTHAN_EQ term */
#line 790 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 3935 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* atomic_formula ::= constant */
      case 179: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==179);
#line 820 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy6, yymsp[0].minor.yy357, "true"); }
#line 3941 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* atomic_formula ::= TILDE constant */
      case 180: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==180);
#line 821 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy6, yymsp[0].minor.yy357, "false");   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 3948 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* atomic_formula ::= constant EQ term */
      case 181: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==181);
#line 822 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = new AtomicFormula(yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());	  yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3955 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 192: /* formula_temporal ::= term_strong COLON formula_temporal */
#line 897 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy374, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy374); }
#line 3960 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 193: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
#line 909 "bcplus/parser/detail/lemon_parser.y"
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
#line 3977 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 194: /* quant_lst ::= quant_op variable */
#line 923 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy358 = new QuantifierFormula::QuantifierList();
		yygotominor.yy358->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy423, yymsp[0].minor.yy313));
	}
#line 3985 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 195: /* quant_lst ::= quant_lst quant_op variable */
#line 929 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy358 = yymsp[-2].minor.yy358;
		yygotominor.yy358->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy423, yymsp[0].minor.yy313));
	}
#line 3993 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 196: /* quant_op ::= BIG_CONJ */
#line 934 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 3999 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 197: /* quant_op ::= BIG_DISJ */
#line 935 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,92,&yymsp[0].minor);
}
#line 4005 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 198: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
#line 981 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, NULL);  }
#line 4010 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 199: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
#line 982 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, yymsp[-4].minor.yy113, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy374,  yymsp[0].minor.yy0, NULL);  }
#line 4015 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 200: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 983 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4020 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 201: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 984 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, yymsp[-5].minor.yy113, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy374,  yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4025 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 202: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 988 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy279 = yymsp[-1].minor.yy279;
	  yy_destructor(yypParser,99,&yymsp[0].minor);
}
#line 4033 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 203: /* card_var_lst ::= PIPE */
#line 992 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy279 = new CardinalityFormula::VariableList();
	  yy_destructor(yypParser,99,&yymsp[0].minor);
}
#line 4041 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 204: /* card_var_lst_inner ::= variable */
#line 997 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy313;
		yygotominor.yy279 = new CardinalityFormula::VariableList();
		yygotominor.yy279->push_back(yymsp[0].minor.yy313->symbol());
	}
#line 4050 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 205: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1004 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy313;
		yygotominor.yy279 = yymsp[-2].minor.yy279;
		yygotominor.yy279->push_back(yymsp[0].minor.yy313->symbol());
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4060 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* head_formula ::= formula_smpl_card */
#line 1024 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy374 = yymsp[0].minor.yy73;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy73->beginLoc());
			YYERROR;
		}
	}
#line 4071 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* head_formula ::= DASH constant */
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
#line 4087 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1047 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy6, yymsp[0].minor.yy0, NULL);  }
#line 4092 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1048 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, yymsp[-4].minor.yy113, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy6,  yymsp[0].minor.yy0, NULL);  }
#line 4097 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1049 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy6, yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4102 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 215: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1050 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, yymsp[-5].minor.yy113, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy6,  yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4107 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1069 "bcplus/parser/detail/lemon_parser.y"
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
#line 4137 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* macro_def_lst ::= macro_bnd */
#line 1097 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy381 = new MacroDeclaration::ElementList();
        yygotominor.yy381->push_back(yymsp[0].minor.yy83);
    }
#line 4145 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 218: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1103 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy381 = yymsp[-2].minor.yy381;
        yygotominor.yy381->push_back(yymsp[0].minor.yy83);
      yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4154 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 219: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1109 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy198;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy83 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy198);
      yy_destructor(yypParser,72,&yymsp[-4].minor);
  yy_destructor(yypParser,73,&yymsp[-2].minor);
  yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4168 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 220: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1118 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy83 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4179 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 221: /* macro_args ::= macro_arg */
#line 1126 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy198 = new MacroSymbol::ArgumentList();
        yygotominor.yy198->push_back(yymsp[0].minor.yy340->str());
        delete yymsp[0].minor.yy340;
    }
#line 4188 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* macro_args ::= macro_args COMMA macro_arg */
#line 1132 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy198 = yymsp[-2].minor.yy198;
        yygotominor.yy198->push_back(yymsp[0].minor.yy340->str());
        delete yymsp[0].minor.yy340;
      yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4198 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 223: /* macro_arg ::= POUND_INTEGER */
      case 224: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==224);
#line 1139 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy340 = yymsp[0].minor.yy0;
    }
#line 4206 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 225: /* sort_lst ::= sort */
#line 1166 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy151 = new ConstantSymbol::SortList();
		yygotominor.yy151->push_back(yymsp[0].minor.yy325);
	}
#line 4214 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 226: /* sort_lst ::= sort_lst COMMA sort */
#line 1171 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy151 = yymsp[-2].minor.yy151;
		yygotominor.yy151->push_back(yymsp[0].minor.yy325);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4223 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 227: /* sort ::= sort_id_nr */
      case 233: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==233);
      case 234: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==234);
#line 1226 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy325 = yymsp[0].minor.yy325; }
#line 4230 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 228: /* sort ::= sort_id_nr STAR */
#line 1227 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-1].minor.yy325, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, *yymsp[-1].minor.yy325->base() + "__plus_none_0", "none"); }
#line 4235 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 229: /* sort ::= sort_id_nr CARROT */
#line 1228 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-1].minor.yy325, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, *yymsp[-1].minor.yy325->base() + "__plus_unknown_0__", "unknown"); }
#line 4240 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 230: /* sort ::= sort_nr PLUS object_nullary */
      case 231: /* sort ::= sort_id PLUS object_nullary */ yytestcase(yyruleno==231);
#line 1230 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy325, yymsp[-2].minor.yy325, yymsp[-1].minor.yy0, yymsp[0].minor.yy398); }
#line 4246 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* sort ::= sort_id PLUS INTEGER */
#line 1233 "bcplus/parser/detail/lemon_parser.y"
{ ref_ptr<const Referenced> i_ptr = yymsp[0].minor.yy0; DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-2].minor.yy325, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, *yymsp[-2].minor.yy325->base() + "__plus_" + *yymsp[0].minor.yy0->str(), (*((std::string const*)yymsp[0].minor.yy0->str()))); }
#line 4251 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* sort_nr ::= num_range */
#line 1240 "bcplus/parser/detail/lemon_parser.y"
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
#line 4290 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 236: /* sort_id ::= IDENTIFIER */
#line 1277 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy325 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy325) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4303 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 237: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1308 "bcplus/parser/detail/lemon_parser.y"
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
#line 4322 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 238: /* constant_bnd_lst ::= constant_bnd */
#line 1325 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy77 = yymsp[0].minor.yy77;
	}
#line 4329 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 239: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1330 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy77;
		yygotominor.yy77 = yymsp[-2].minor.yy77;
		yygotominor.yy77->splice(yygotominor.yy77->end(), *yymsp[0].minor.yy77);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4339 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 240: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1350 "bcplus/parser/detail/lemon_parser.y"
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
#line 4358 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 241: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1363 "bcplus/parser/detail/lemon_parser.y"
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
#line 4373 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 242: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1374 "bcplus/parser/detail/lemon_parser.y"
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
#line 4388 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 243: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1385 "bcplus/parser/detail/lemon_parser.y"
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
#line 4417 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 244: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1409 "bcplus/parser/detail/lemon_parser.y"
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
#line 4498 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 245: /* constant_dcl_lst ::= IDENTIFIER */
#line 1485 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = new IdentifierDeclList();
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 4506 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 246: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1490 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = new IdentifierDeclList();
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy151));
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4516 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 247: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1495 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = yymsp[-2].minor.yy316;
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4525 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 248: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1500 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = yymsp[-5].minor.yy316;
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy151));
	  yy_destructor(yypParser,103,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4536 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 249: /* constant_dcl_type ::= ABACTION */
#line 1507 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4548 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 250: /* constant_dcl_type ::= ACTION */
#line 1516 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4560 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 251: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1525 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4572 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 252: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1534 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4584 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 253: /* constant_dcl_type ::= EXTERNALACTION */
#line 1543 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4596 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 254: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1552 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4608 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 255: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1561 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4620 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 256: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1570 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4632 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 257: /* constant_dcl_type ::= RIGID */
#line 1579 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4644 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 258: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1588 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4656 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 259: /* constant_dcl_type ::= SDFLUENT */
#line 1598 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4668 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 260: /* attrib_spec ::= ATTRIBUTE */
#line 1608 "bcplus/parser/detail/lemon_parser.y"
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
#line 4683 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 261: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1621 "bcplus/parser/detail/lemon_parser.y"
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
#line 4699 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 262: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1649 "bcplus/parser/detail/lemon_parser.y"
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
#line 4724 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 263: /* object_bnd_lst ::= object_bnd */
#line 1672 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy84 = new ObjectDeclaration::ElementList();
		yygotominor.yy84->push_back(yymsp[0].minor.yy180);
	}
#line 4732 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 264: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 1678 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy84 = yymsp[-2].minor.yy84;
		yygotominor.yy84->push_back(yymsp[0].minor.yy180);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4741 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 1684 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy180 = new ObjectDeclaration::Element(yymsp[0].minor.yy325, yymsp[-2].minor.yy299);
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4749 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* object_lst ::= object_spec */
#line 1689 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = yymsp[0].minor.yy299;
	}
#line 4756 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 267: /* object_lst ::= object_lst COMMA object_spec */
#line 1693 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = yymsp[-2].minor.yy299;
		yygotominor.yy299->splice(yygotominor.yy299->end(), *yymsp[0].minor.yy299);
		delete yymsp[0].minor.yy299;
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4766 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* object_spec ::= IDENTIFIER */
#line 1702 "bcplus/parser/detail/lemon_parser.y"
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
#line 4782 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 269: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1715 "bcplus/parser/detail/lemon_parser.y"
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
#line 4801 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 270: /* object_spec ::= num_range */
#line 1729 "bcplus/parser/detail/lemon_parser.y"
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
#line 4821 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 271: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 1761 "bcplus/parser/detail/lemon_parser.y"
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
#line 4852 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 272: /* variable_bnd_lst ::= variable_bnd */
#line 1790 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = yymsp[0].minor.yy326;
	}
#line 4859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 273: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 1795 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = yymsp[-2].minor.yy326;
		yygotominor.yy326->splice(yygotominor.yy326->end(), *yymsp[0].minor.yy326);
		delete yymsp[0].minor.yy326;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4869 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 274: /* variable_bnd ::= variable_lst DBL_COLON sort_id */
#line 1802 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy58) {
			yygotominor.yy326->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy325));
		}
		delete yymsp[-2].minor.yy58;
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4882 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 275: /* variable_lst ::= IDENTIFIER */
#line 1812 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy58 = new TokenList();
		yygotominor.yy58->push_back(yymsp[0].minor.yy0);
	}
#line 4890 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 276: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 1817 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy58 = yymsp[-2].minor.yy58;
		yygotominor.yy58->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4899 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 277: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 1838 "bcplus/parser/detail/lemon_parser.y"
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
#line 4917 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 278: /* sort_bnd_lst ::= sort_bnd */
      case 280: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==280);
#line 1854 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[0].minor.yy233;
	}
#line 4925 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 279: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 1859 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[-2].minor.yy233;
		yygotominor.yy233->splice(yygotominor.yy233->end(), *yymsp[0].minor.yy233);
		delete yymsp[0].minor.yy233;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4935 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 281: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 1871 "bcplus/parser/detail/lemon_parser.y"
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
#line 4951 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 282: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 1883 "bcplus/parser/detail/lemon_parser.y"
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
#line 4966 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 283: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 1894 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[-1].minor.yy233;
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4975 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 284: /* sort_dcl_lst ::= IDENTIFIER */
#line 1899 "bcplus/parser/detail/lemon_parser.y"
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
#line 4992 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 285: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 1913 "bcplus/parser/detail/lemon_parser.y"
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
#line 5011 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 286: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 1940 "bcplus/parser/detail/lemon_parser.y"
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
#line 5027 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 1954 "bcplus/parser/detail/lemon_parser.y"
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
#line 5045 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 1971 "bcplus/parser/detail/lemon_parser.y"
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
#line 5061 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 1985 "bcplus/parser/detail/lemon_parser.y"
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
#line 5079 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* show_lst ::= show_elem */
#line 2003 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy345 = new ShowStatement::ElementList();
		yygotominor.yy345->push_back(yymsp[0].minor.yy6);
	}
#line 5087 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* show_lst ::= show_lst COMMA show_elem */
#line 2008 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy345 = yymsp[-2].minor.yy345;
		yygotominor.yy345->push_back(yymsp[0].minor.yy6);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5096 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* show_elem ::= atomic_formula_one_const */
#line 2013 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = yymsp[0].minor.yy6; }
#line 5101 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* stmt_noconcurrency ::= NOCONCURRENCY */
#line 2036 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy451, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5106 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 294: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY */
#line 2037 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy352, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5111 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD */
#line 2063 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy334, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy468, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5117 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD */
#line 2064 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy334, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy468, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5123 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2089 "bcplus/parser/detail/lemon_parser.y"
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
#line 5156 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* query_lst ::= formula_temporal */
#line 2121 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = NULL;
		yygotominor.yy165.label = NULL;

		yygotominor.yy165.l->push_back(yymsp[0].minor.yy374);
	}
#line 5167 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* query_lst ::= query_maxstep_decl */
#line 2130 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = yymsp[0].minor.yy234;
		yygotominor.yy165.label = NULL;
	}
#line 5176 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* query_lst ::= query_label_decl */
#line 2137 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = NULL;
		yygotominor.yy165.label = yymsp[0].minor.yy340;
	}
#line 5185 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2144 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy165 = yymsp[-2].minor.yy165;
		yymsp[-2].minor.yy165.l->push_back(yymsp[0].minor.yy374);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5194 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2150 "bcplus/parser/detail/lemon_parser.y"
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
#line 5210 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2163 "bcplus/parser/detail/lemon_parser.y"
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
#line 5226 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2189 "bcplus/parser/detail/lemon_parser.y"
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
#line 5251 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range */
#line 2210 "bcplus/parser/detail/lemon_parser.y"
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
#line 5268 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 307: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==307);
#line 2224 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy340, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5275 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* clause_if ::= IF formula */
#line 2259 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_IF); 		}
#line 5280 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* clause_if ::= */
      case 311: /* clause_after ::= */ yytestcase(yyruleno==311);
      case 313: /* clause_ifcons ::= */ yytestcase(yyruleno==313);
      case 317: /* clause_where ::= */ yytestcase(yyruleno==317);
#line 2260 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = NULL; }
#line 5288 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* clause_after ::= AFTER formula */
#line 2261 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_AFTER);	}
#line 5293 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* clause_ifcons ::= IFCONS formula */
#line 2263 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_IFCONS); 	}
#line 5298 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* clause_unless ::= UNLESS atomic_formula */
#line 2265 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy6, yymsp[-1].minor.yy0, yymsp[0].minor.yy6, Language::Feature::CLAUSE_UNLESS); 	}
#line 5303 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* clause_unless ::= */
#line 2266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = NULL; }
#line 5308 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* clause_where ::= WHERE formula_no_const */
#line 2267 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_WHERE); 	}
#line 5313 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* stmt_law ::= law_basic */
      case 319: /* stmt_law ::= law_caused */ yytestcase(yyruleno==319);
      case 320: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==320);
      case 321: /* stmt_law ::= law_impl */ yytestcase(yyruleno==321);
      case 322: /* stmt_law ::= law_causes */ yytestcase(yyruleno==322);
      case 323: /* stmt_law ::= law_increments */ yytestcase(yyruleno==323);
      case 324: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==324);
      case 325: /* stmt_law ::= law_always */ yytestcase(yyruleno==325);
      case 326: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==326);
      case 327: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==327);
      case 328: /* stmt_law ::= law_never */ yytestcase(yyruleno==328);
      case 329: /* stmt_law ::= law_default */ yytestcase(yyruleno==329);
      case 330: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==330);
      case 331: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==331);
      case 332: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==332);
      case 333: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==333);
      case 334: /* stmt_law ::= law_observed */ yytestcase(yyruleno==334);
#line 2311 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy334 = yymsp[0].minor.yy334;}
#line 5334 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 335: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2426 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, NULL, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
																																															Language::Feature::LAW_BASIC_D, BasicLaw); }
#line 5341 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 336: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2430 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 5348 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 337: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2434 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 5355 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 338: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2438 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy334, yymsp[-4].minor.yy374, yymsp[-3].minor.yy0, yymsp[-2].minor.yy374, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 5361 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 339: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2441 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy334, yymsp[-6].minor.yy6, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 5367 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 340: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2445 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy334, yymsp[-8].minor.yy6, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-4].minor.yy113, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5374 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 341: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2449 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy334, yymsp[-6].minor.yy6, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 5380 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 342: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
      case 343: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */ yytestcase(yyruleno==343);
#line 2453 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 5388 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 344: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2461 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 5395 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 345: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2465 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 5402 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2469 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy6, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 5409 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2473 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 5416 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 348: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2477 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 5423 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2481 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 5429 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2485 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy334, yymsp[-3].minor.yy0, yymsp[-2].minor.yy357, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 5435 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD */
#line 2490 "bcplus/parser/detail/lemon_parser.y"
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
#line 5454 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* stmt_code_blk ::= ASP_GR */
#line 2524 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 5459 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* stmt_code_blk ::= ASP_CP */
#line 2525 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 5464 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* stmt_code_blk ::= F2LP_GR */
#line 2526 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 5469 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* stmt_code_blk ::= F2LP_CP */
#line 2527 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 5474 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* stmt_code_blk ::= LUA_GR */
#line 2528 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 5479 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* stmt_code_blk ::= LUA_CP */
#line 2529 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 5484 "bcplus/parser/detail/lemon_parser.c"
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
#line 5550 "bcplus/parser/detail/lemon_parser.c"
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
