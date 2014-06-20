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
		/* Check to see if we have constants in any of the terms */															\
		bool good = true;																									\
		if (!parser->lang()->support(Language::Feature::FORMULA_CONSTANT_ARGS) && arity) {									\
			int cargs = 0;																									\
			/*BOOST_FOREACH(Element const* elem, *args_ptr) { */															\
			for (TermList::const_iterator it = args_ptr->begin(); it != args_ptr->end(); it++) {							\
				cargs |=(*it)->cmask();																						\
			}																												\
			if (cargs) {																									\
				parser->_feature_error(Language::Feature::FORMULA_CONSTANT_ARGS, &id->beginLoc());							\
				YYERROR;																									\
				good = false;																								\
			}																												\
		}																													\
																															\
		if (good) {																											\
			Symbol const* sym = parser->symtab()->resolve(symtype, *id_ptr->str(), arity);									\
			if (sym && sym->type() == symtype) {																			\
				elem = new class((symclass*)sym, args, id_ptr->beginLoc(), (arity ? rparen_ptr->endLoc() : id_ptr->endLoc()));	\
			} else {																										\
				/* The preprocessor indicated this was a constant and it's not... oops. */									\
				parser->_parse_error(std::string("INTERNAL ERROR: Could not locate symbol table entry for ")				\
					+ Symbol::Type::cstr(symtype) + " \"" + Symbol::genName(*id_ptr->str(), arity)		 					\
					+ "\".", &id_ptr->beginLoc());																			\
				YYERROR;																									\
			}																												\
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

#line 463 "bcplus/parser/detail/lemon_parser.y"

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

#line 687 "bcplus/parser/detail/lemon_parser.y"

	#define NUM_UOP(t_new, t, val)																				\
		ref_ptr<const Referenced> t_ptr = t;																			\
		t_new = new Number(val, t->beginLoc(), t->endLoc());

	
	#define NUM_BOP(t_new, l, r, val)																			\
		ref_ptr<const Referenced> l_ptr = l, r_ptr = r;																\
		t_new = new Number(val, l->beginLoc(), r->endLoc());

#line 729 "bcplus/parser/detail/lemon_parser.y"

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

#line 809 "bcplus/parser/detail/lemon_parser.y"


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
	


#line 885 "bcplus/parser/detail/lemon_parser.y"

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

#line 965 "bcplus/parser/detail/lemon_parser.y"

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



#line 1194 "bcplus/parser/detail/lemon_parser.y"

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
#line 1353 "bcplus/parser/detail/lemon_parser.y"

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
#line 2039 "bcplus/parser/detail/lemon_parser.y"

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

#line 2062 "bcplus/parser/detail/lemon_parser.y"

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
#line 2089 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRange const* maxstep;
		Token const* label;
	};

#line 2192 "bcplus/parser/detail/lemon_parser.y"

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

#line 2264 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2347 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2528 "bcplus/parser/detail/lemon_parser.y"

	#define CODE_BLK(stmt, code, feature, type) 												\
		ref_ptr<const Token> code_ptr = code;													\
		if (!parser->lang()->support(feature)) {												\
			stmt = NULL;																		\
			parser->_feature_error(feature, &code->beginLoc());									\
			YYERROR;																			\
		} else {																				\
			stmt = new type(code, code->beginLoc(), code->endLoc());							\
		}
#line 532 "bcplus/parser/detail/lemon_parser.c"
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
#define YYNSTATE 697
#define YYNRULE 361
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
#define YY_ACTTAB_COUNT (2777)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   696,  438,   58,  695,  694,  693,  692,  691,  690,  689,
 /*    10 */   688,  687,  686,  685,  684,  683,  682,  681,  437,  654,
 /*    20 */   261,  680,  673,  679,  678,  672,   29,   28,   27,   31,
 /*    30 */    30,  258,  262,   32,   28,   27,   31,   30,  617,  344,
 /*    40 */    32,  654,  261,  680,  673,  379,  678,  672,  101,   99,
 /*    50 */    98,  189,  615,  257,  262,  130,  128,  127,  268,  611,
 /*    60 */   608,  607,  606,  605,  654,  261,  680,  673,  679,  678,
 /*    70 */   672,   24,  381,   53,   31,   30,  257,  262,   32,  126,
 /*    80 */   508,  284,  611,  608,  607,  606,  605,  697,  481,  480,
 /*    90 */   479,  478,  477,  476,  475,  474,  473,  472,  471,  470,
 /*   100 */   469,  468,  467,  466,  465,  653,  440,  159,  160,  441,
 /*   110 */   652,  450,  449,  446,  445,  448,  447,  653,  440,  487,
 /*   120 */   486,  441,  652,  631,  625,  680,  673,  679,  678,  672,
 /*   130 */    39,  635,   10,  380,  306,  252,   37,   52,    8,    9,
 /*   140 */   244,  156,   51,  207,   38,  630,  440,   50,  201,  441,
 /*   150 */   629,  633,  646,  645,  647,    7,  483,  482,    6,  904,
 /*   160 */   157,  193,  400,  904,  646,  645,  401,  613,  438,  614,
 /*   170 */   633,   44,   43,  263,  152,   45,  109,  166,  381,  603,
 /*   180 */   250,  604,  110,  164,  675,  504,    4,  552,   23,   18,
 /*   190 */    17,  551,  250,   19,  653,  440,  165,  381,  441,  652,
 /*   200 */   654,  261,  680,  673,  679,  678,  672,  626,  674,  627,
 /*   210 */   107,  438,  258,  262,  106,   92,  133,  359,  502,  617,
 /*   220 */   616,   20,   21,  444,   60,  653,  440,   92,  504,  441,
 /*   230 */   652,   59,  249,  615,  670,  443,  671,  442,  106,  155,
 /*   240 */   669,  646,  645,  647,  375,  525,  374,  443,  671,  442,
 /*   250 */   131,   41,   40,   44,   43,  129,  603,   45,  604,  570,
 /*   260 */   164,  505,   68,    4,  106,   35,  182,  180,  179,  250,
 /*   270 */   106,  111,  646,  645,  647,  443,  671,  442,  162,  238,
 /*   280 */   654,  329,  680,  673,  679,  678,  672,  603,   32,  604,
 /*   290 */    45,  164,  286,  330,    4,  573,   23,  414,   33,   34,
 /*   300 */   250,    5,  506,  424,   92,  631,  625,  680,  673,  679,
 /*   310 */   678,  672,   22,   67,   66,   65,  307,   64,   63,   62,
 /*   320 */    61,  366,  511,  365,  443,  671,  442,  519,  282,   20,
 /*   330 */    21,  148,  364,  360,  409,   92,   36,   97,   96,   95,
 /*   340 */    94,   93,   54,  668,  676,  677,  680,  673,  679,  678,
 /*   350 */   672,  385,  332,   36,  572,  443,  671,  442,  571,  632,
 /*   360 */   369,  517,  368,  518,  236,  438,  654,  261,  680,  673,
 /*   370 */   679,  678,  672,  559,  545,   11,  653,  440,  254,  262,
 /*   380 */   441,  652,  504,  235,  496,  608,  607,  606,  605,  135,
 /*   390 */   147,  600,  599,  277,  539,  538,  537,  536,  425,   26,
 /*   400 */   568,  151,  416,  169,  140,    2,  146,  149,  150,  410,
 /*   410 */   535,  426,  533,  407,  358,  502,  438,  532,  105,  103,
 /*   420 */   101,   99,   98,  646,  645,  647,  356,  485,  484,  531,
 /*   430 */   529,  530,  534,  436,  911,  404,  405,  563,  613,  237,
 /*   440 */   614,  191,  190,  561,  185,  152,  544,  109,  524,  911,
 /*   450 */   911,  250,  159,  160,  654,  261,  680,  673,  379,  678,
 /*   460 */   672,  143,  251,  168,  418,   49,  257,  262,  144,  911,
 /*   470 */   911,  270,  611,  608,  607,  606,  605,  417,  183,  378,
 /*   480 */   562,  107,  911,  181,   25,  381,   92,  139,  911,  565,
 /*   490 */   566,  654,  261,  680,  673,  679,  678,  672,   29,   28,
 /*   500 */    27,   31,   30,  254,  262,   32,  443,  671,  442,  496,
 /*   510 */   608,  607,  606,  605,  507,  593,  440,  439,  276,  441,
 /*   520 */   592,  145,  631,  625,  680,  673,  679,  678,  672,    5,
 /*   530 */   540,  527,  651,  345,  667,  440,  560,  374,  441,  666,
 /*   540 */    22,   67,   66,   65,  602,   64,   63,   62,   61,  519,
 /*   550 */   282,  433,  489,  488,  105,  103,  101,   99,   98,  413,
 /*   560 */   142,  142,  586,  585,  587,   97,   96,   95,   94,   93,
 /*   570 */   595,   29,   28,   27,   31,   30,  526,  576,   32,  577,
 /*   580 */    36,  661,  660,  662,  158,  521,   48,  654,  642,  680,
 /*   590 */   673,  679,  678,  672,  520,  528,  663,  516,  664,  644,
 /*   600 */   330,   15,   14,   18,   17,  104,  142,   19,  515,  365,
 /*   610 */   654,  261,  680,  673,  679,  678,  672,  167,  547,   46,
 /*   620 */    47,  141,  255,  262,  142,  113,  138,  266,  611,  608,
 /*   630 */   607,  606,  605,  137,   29,   28,   27,   31,   30,  102,
 /*   640 */   510,   32,  600,  599,  100,  443,  671,  442,  136,  251,
 /*   650 */   654,  260,  680,  673,  679,  678,  672,  546,  282,  513,
 /*   660 */   161,  501,  612,  262,  443,  671,  442,  597,  611,  608,
 /*   670 */   607,  606,  605,  408,  654,  261,  680,  673,  679,  678,
 /*   680 */   672,  543,  373,  423,  422,  509,  257,  262,  421,  419,
 /*   690 */   148,  610,  611,  608,  607,  606,  605,  654,  261,  680,
 /*   700 */   673,  679,  678,  672,  439,  596,  178,  188,  247,  257,
 /*   710 */   262,  665,  250,   12,  609,  611,  608,  607,  606,  605,
 /*   720 */   654,  261,  680,  673,  679,  678,  672,   29,   28,   27,
 /*   730 */    31,   30,  257,  262,   32,   57,  503,  435,  611,  608,
 /*   740 */   607,  606,  605,  500,  105,  103,  101,   99,   98,  433,
 /*   750 */   550,   19,  654,  261,  680,  673,  679,  678,  672,   29,
 /*   760 */    28,   27,   31,   30,  257,  262,   32,  443,  225,  434,
 /*   770 */   611,  608,  607,  606,  605,  654,  261,  680,  673,  679,
 /*   780 */   678,  672,   29,   28,   27,   31,   30,  257,  262,   32,
 /*   790 */   376,  564,  302,  611,  608,  607,  606,  605,  519,  282,
 /*   800 */   654,  261,  680,  673,  679,  678,  672,   29,   28,   27,
 /*   810 */    31,   30,  257,  262,   32,  363,  409,  343,  611,  608,
 /*   820 */   607,  606,  605,  654,  261,  680,  673,  679,  678,  672,
 /*   830 */    56,  406,  523,  368,  518,  257,  262,  362,  409,  177,
 /*   840 */   342,  611,  608,  607,  606,  605,  154,  654,  261,  680,
 /*   850 */   673,  679,  678,  672,   29,   28,   27,   31,   30,  257,
 /*   860 */   262,   32,  361,  409,  271,  611,  608,  607,  606,  605,
 /*   870 */   654,  261,  680,  673,  679,  678,  672,  355,  574,  112,
 /*   880 */   251,  464,  257,  262,  335,  409,  463,  269,  611,  608,
 /*   890 */   607,  606,  605,  654,  261,  680,  673,  679,  678,  672,
 /*   900 */    42,   41,   40,   44,   43,  257,  262,   45, 1004,  490,
 /*   910 */   267,  611,  608,  607,  606,  605,  462,  433,  433,  461,
 /*   920 */   654,  261,  680,  673,  679,  678,  672,  460, 1004,    3,
 /*   930 */   459,   55,  253,  262,  634,  439,  212,  386,  496,  608,
 /*   940 */   607,  606,  605,  491,  282, 1059,    1,  264,  654,  261,
 /*   950 */   680,  673,  679,  678,  672,   29,   28,   27,   31,   30,
 /*   960 */   254,  262,   32,  458,  457,  456,  496,  608,  607,  606,
 /*   970 */   605,  455,  454,  453,  452,  278,  654,  260,  680,  673,
 /*   980 */   679,  678,  672,   42,   41,   40,   44,   43,  497,  262,
 /*   990 */    45,  671,  619,  439,  496,  608,  607,  606,  605,  618,
 /*  1000 */   443,  246,   36,  493,  654,  261,  680,  673,  679,  678,
 /*  1010 */   672,   16,   15,   14,   18,   17,  254,  262,   19,  245,
 /*  1020 */   601,  163,  496,  608,  607,  606,  605,  598,   49,  430,
 /*  1030 */   429,  495,  654,  261,  680,  673,  679,  678,  672,   86,
 /*  1040 */    85,   84,   83,   82,  254,  262,  242,  569,  428,  240,
 /*  1050 */   496,  608,  607,  606,  605,  239,  377,  550,  545,  494,
 /*  1060 */   427,  654,  261,  680,  673,  679,  678,  672,  549,  522,
 /*  1070 */   514,  233,   13,  254,  262,  232,   12,  399,  227,  496,
 /*  1080 */   608,  607,  606,  605,  229,  231,  226,  228,  403,  654,
 /*  1090 */   261,  680,  673,  679,  678,  672,  222,  224,  397,  211,
 /*  1100 */   223,  254,  262,  221,  210,  396,  219,  496,  608,  607,
 /*  1110 */   606,  605,  206,  209,  205,  395,  402,  654,  261,  680,
 /*  1120 */   673,  679,  678,  672,  217,  204,  394,  215,  200,  254,
 /*  1130 */   262,  393,  392,  213,  208,  496,  608,  607,  606,  605,
 /*  1140 */   391,  203,  202,  198,  288,  654,  261,  680,  673,  679,
 /*  1150 */   678,  672,  390,  196,  199,  389,  197,  254,  262,  194,
 /*  1160 */   192,  388,  387,  496,  608,  607,  606,  605,  567,  548,
 /*  1170 */   234,  248,  334,  654,  261,  680,  673,  679,  678,  672,
 /*  1180 */   289,  336,  243,  281,  337,  254,  262,  557,  553,  220,
 /*  1190 */   556,  496,  608,  607,  606,  605,  555,  554,  280,  279,
 /*  1200 */   333,  654,  261,  680,  673,  679,  678,  672,  187,  398,
 /*  1210 */   412,  218,  216,  257,  262,  214,  195,  512,  275,  611,
 /*  1220 */   608,  607,  606,  605,  186,  184,  182,  180,  179,  654,
 /*  1230 */   261,  680,  673,  679,  678,  672, 1060, 1060, 1060, 1060,
 /*  1240 */  1060,  257,  262, 1060, 1060, 1060,  274,  611,  608,  607,
 /*  1250 */   606,  605,   91,   90,   89,   88,   87,  654,  261,  680,
 /*  1260 */   673,  679,  678,  672, 1060, 1060, 1060, 1060, 1060,  257,
 /*  1270 */   262, 1060, 1060, 1060,  175,  611,  608,  607,  606,  605,
 /*  1280 */   134,  132,  130,  128,  127,  654,  261,  680,  673,  679,
 /*  1290 */   678,  672, 1060, 1060, 1060, 1060, 1060,  257,  262, 1060,
 /*  1300 */  1060, 1060,  174,  611,  608,  607,  606,  605,  186,  184,
 /*  1310 */   182,  180,  179,  654,  261,  680,  673,  679,  678,  672,
 /*  1320 */  1060,  559,  411,  559,  545,  257,  262, 1060, 1060, 1060,
 /*  1330 */   173,  611,  608,  607,  606,  605, 1060, 1060, 1060, 1060,
 /*  1340 */  1060,  654,  261,  680,  673,  679,  678,  672, 1060, 1060,
 /*  1350 */  1060, 1060, 1060,  257,  262, 1060, 1060, 1060,  172,  611,
 /*  1360 */   608,  607,  606,  605, 1060, 1060, 1060, 1060, 1060,  654,
 /*  1370 */   261,  680,  673,  679,  678,  672, 1060, 1060, 1060, 1060,
 /*  1380 */  1060,  257,  262, 1060, 1060, 1060,  171,  611,  608,  607,
 /*  1390 */   606,  605,  185, 1060,  185, 1060, 1060,  654,  261,  680,
 /*  1400 */   673,  679,  678,  672, 1060, 1060, 1060, 1060, 1060,  257,
 /*  1410 */   262, 1060, 1060, 1060,  170,  611,  608,  607,  606,  605,
 /*  1420 */  1060, 1060, 1060, 1060,    5, 1060,  183,  651,  183,  546,
 /*  1430 */   282,  181,  153,  181, 1060, 1060,   67,   66,   65, 1060,
 /*  1440 */    64,   63,   62,   61,   67,   66,   65, 1060,   64,   63,
 /*  1450 */    62,   61,  372,  542,  373,  423,  422, 1060, 1060, 1060,
 /*  1460 */    97,   96,   95,   94,   93,  591, 1060, 1060,   97,   96,
 /*  1470 */    95,   94,   93,    5,  120,  119,  118, 1060,  117,  116,
 /*  1480 */   115,  114, 1060, 1060, 1060,   67,   66,   65,  651,   64,
 /*  1490 */    63,   62,   61, 1060, 1060, 1060, 1060, 1060,  125,  124,
 /*  1500 */   123,  122,  121,  653,  440, 1060,  492,  441,  652,   97,
 /*  1510 */    96,   95,   94,   93,  654,  261,  680,  673,  679,  678,
 /*  1520 */   672,   97,   96,   95,   94,   93,  258,  262,   16,   15,
 /*  1530 */    14,   18,   17,  617,  616,   19,  668,  676,  677,  680,
 /*  1540 */   673,  679,  678,  672,  384,  332,  241,  615, 1060, 1060,
 /*  1550 */   646,  645,  647,  654,  261,  680,  673,  679,  678,  672,
 /*  1560 */  1060, 1060, 1060, 1060, 1060,  258,  262, 1060, 1060, 1060,
 /*  1570 */  1060, 1060,  617,  616,  109,   81,   80,   79, 1060,   78,
 /*  1580 */    77,   76,   75, 1060, 1060,  230,  615, 1060, 1060, 1060,
 /*  1590 */    68,   74,   73, 1060,   72,   71,   70,   69, 1060,   86,
 /*  1600 */    85,   84,   83,   82, 1060, 1060, 1060, 1060,  108, 1060,
 /*  1610 */  1060, 1060, 1060,   92,   91,   90,   89,   88,   87,  668,
 /*  1620 */   676,  677,  680,  673,  679,  678,  672,  383,  332, 1060,
 /*  1630 */  1060, 1060, 1060,  443,  671,  442,  762,  762,  762, 1060,
 /*  1640 */   762,  762,  762,  762, 1060, 1060, 1060, 1060,  120,  119,
 /*  1650 */   118, 1060,  117,  116,  115,  114, 1060,  546,  282, 1060,
 /*  1660 */   762,  762,  762,  762,  762,  594,  584,  680,  673,  679,
 /*  1670 */   678,  672,  125,  124,  123,  122,  121, 1060, 1060,  259,
 /*  1680 */   371,  542,  373,  423,  422, 1060,  546,  282,  283,  581,
 /*  1690 */   578,  668,  676,  677,  680,  673,  679,  678,  672,  382,
 /*  1700 */   332, 1060,  594,  584,  680,  673,  679,  678,  672,  370,
 /*  1710 */   542,  373,  423,  422, 1060, 1060,  256, 1060, 1060, 1060,
 /*  1720 */  1060,  546,  282, 1060, 1060,  265,  581,  578,  594,  584,
 /*  1730 */   680,  673,  679,  678,  672,  594,  584,  680,  673,  679,
 /*  1740 */   678,  672,  583, 1060,  367,  542,  373,  423,  422,  259,
 /*  1750 */  1060,  575,  581,  578, 1060, 1060,  546,  282,  580,  581,
 /*  1760 */   578, 1060, 1060, 1060,  594,  584,  680,  673,  679,  678,
 /*  1770 */   672,  594,  584,  680,  673,  679,  678,  672,  259,  628,
 /*  1780 */   420,  373,  423,  422, 1060,  259, 1060,  579,  581,  578,
 /*  1790 */  1060, 1060, 1060, 1060,  432,  581,  578,  594,  584,  680,
 /*  1800 */   673,  679,  678,  672,  594,  584,  680,  673,  679,  678,
 /*  1810 */   672,  259,  134,  132,  130,  128,  127, 1060,  259, 1060,
 /*  1820 */   431,  581,  578, 1060, 1060, 1060, 1060,  290,  581,  578,
 /*  1830 */   594,  584,  680,  673,  679,  678,  672, 1060, 1060, 1060,
 /*  1840 */  1060,  558, 1060, 1060,  259,  594,  584,  680,  673,  679,
 /*  1850 */   678,  672, 1060,  339,  581,  578, 1060, 1060, 1060,  259,
 /*  1860 */   654,  272,  680,  673,  679,  678,  672, 1060,  338,  581,
 /*  1870 */   578, 1060,  644,  330,  186,  184,  182,  180,  179, 1060,
 /*  1880 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  331,
 /*  1890 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  353,
 /*  1900 */  1060, 1060,  499, 1060,  668,  676,  677,  680,  673,  679,
 /*  1910 */   678,  672, 1060,  287,  668,  676,  677,  680,  673,  679,
 /*  1920 */   678,  672, 1060,  354,  668,  676,  677,  680,  673,  679,
 /*  1930 */   678,  672, 1060,  659,  186,  184,  182,  180,  179, 1060,
 /*  1940 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  655,
 /*  1950 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  658,
 /*  1960 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  657,
 /*  1970 */   546,  282, 1060,  668,  676,  677,  680,  673,  679,  678,
 /*  1980 */   672, 1060,  656,  668,  676,  677,  680,  673,  679,  678,
 /*  1990 */   672, 1060,  352, 1060,  541,  373,  423,  422,  498,  668,
 /*  2000 */   676,  677,  680,  673,  679,  678,  672, 1060,  351,  668,
 /*  2010 */   676,  677,  680,  673,  679,  678,  672, 1060,  650,  668,
 /*  2020 */   676,  677,  680,  673,  679,  678,  672, 1060,  649, 1060,
 /*  2030 */   186,  184,  182,  180,  179,  668,  676,  677,  680,  673,
 /*  2040 */   679,  678,  672, 1060,  648,  668,  676,  677,  680,  673,
 /*  2050 */   679,  678,  672, 1060,  643, 1060, 1060,  451, 1060,  668,
 /*  2060 */   676,  677,  680,  673,  679,  678,  672, 1060,  350,  668,
 /*  2070 */   676,  677,  680,  673,  679,  678,  672,  559,  349,  668,
 /*  2080 */   676,  677,  680,  673,  679,  678,  672, 1060,  641,  134,
 /*  2090 */   132,  130,  128,  127, 1060,  668,  676,  677,  680,  673,
 /*  2100 */   679,  678,  672, 1060,  640,  668,  676,  677,  680,  673,
 /*  2110 */   679,  678,  672, 1060,  639,  668,  676,  677,  680,  673,
 /*  2120 */   679,  678,  672, 1060,  348,  546,  282, 1060,  668,  676,
 /*  2130 */   677,  680,  673,  679,  678,  672, 1060,  347,  668,  676,
 /*  2140 */   677,  680,  673,  679,  678,  672, 1060,  638,  185,  415,
 /*  2150 */   373,  423,  422, 1060,  668,  676,  677,  680,  673,  679,
 /*  2160 */   678,  672, 1060,  637,  668,  676,  677,  680,  673,  679,
 /*  2170 */   678,  672, 1060,  636,  668,  676,  677,  680,  673,  679,
 /*  2180 */   678,  672,  183,  328, 1060, 1060, 1060,  181, 1060, 1060,
 /*  2190 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  327,
 /*  2200 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  326,
 /*  2210 */  1060, 1060, 1060, 1060,  668,  676,  677,  680,  673,  679,
 /*  2220 */   678,  672, 1060,  325,  668,  676,  677,  680,  673,  679,
 /*  2230 */   678,  672,  357,  324,  668,  676,  677,  680,  673,  679,
 /*  2240 */   678,  672, 1060,  323, 1060, 1060, 1060, 1060, 1060, 1060,
 /*  2250 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  322,
 /*  2260 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  321,
 /*  2270 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  320,
 /*  2280 */  1060, 1060, 1060,  668,  676,  677,  680,  673,  679,  678,
 /*  2290 */   672, 1060,  319,  668,  676,  677,  680,  673,  679,  678,
 /*  2300 */   672, 1060,  318,  185, 1060, 1060, 1060, 1060, 1060,  668,
 /*  2310 */   676,  677,  680,  673,  679,  678,  672, 1060,  317,  668,
 /*  2320 */   676,  677,  680,  673,  679,  678,  672, 1060,  316,  668,
 /*  2330 */   676,  677,  680,  673,  679,  678,  672,  183,  315, 1060,
 /*  2340 */  1060, 1060,  181, 1060, 1060,  668,  676,  677,  680,  673,
 /*  2350 */   679,  678,  672, 1060,  314,  668,  676,  677,  680,  673,
 /*  2360 */   679,  678,  672, 1060,  313, 1060, 1060, 1060, 1060,  668,
 /*  2370 */   676,  677,  680,  673,  679,  678,  672, 1060,  312,  668,
 /*  2380 */   676,  677,  680,  673,  679,  678,  672, 1060,  311,  668,
 /*  2390 */   676,  677,  680,  673,  679,  678,  672, 1060,  310, 1060,
 /*  2400 */  1060, 1060, 1060, 1060, 1060,  668,  676,  677,  680,  673,
 /*  2410 */   679,  678,  672, 1060,  309,  668,  676,  677,  680,  673,
 /*  2420 */   679,  678,  672, 1060,  308,  668,  676,  677,  680,  673,
 /*  2430 */   679,  678,  672, 1060,  304, 1060, 1060, 1060,  668,  676,
 /*  2440 */   677,  680,  673,  679,  678,  672, 1060,  303,  668,  676,
 /*  2450 */   677,  680,  673,  679,  678,  672, 1060,  301, 1060, 1060,
 /*  2460 */  1060, 1060, 1060, 1060,  668,  676,  677,  680,  673,  679,
 /*  2470 */   678,  672, 1060,  300,  668,  676,  677,  680,  673,  679,
 /*  2480 */   678,  672, 1060,  299,  668,  676,  677,  680,  673,  679,
 /*  2490 */   678,  672, 1060,  298, 1060, 1060, 1060, 1060, 1060, 1060,
 /*  2500 */   668,  676,  677,  680,  673,  679,  678,  672, 1060,  176,
 /*  2510 */   631,  625,  680,  673,  679,  678,  672, 1060, 1060, 1060,
 /*  2520 */  1060,  285, 1060, 1060, 1060,  631,  625,  680,  673,  679,
 /*  2530 */   678,  672, 1060, 1060, 1060, 1060,  346,  631,  625,  680,
 /*  2540 */   673,  679,  678,  672, 1060, 1060, 1060, 1060,  624,  631,
 /*  2550 */   625,  680,  673,  679,  678,  672, 1060, 1060, 1060, 1060,
 /*  2560 */   620,  631,  625,  680,  673,  679,  678,  672, 1060, 1060,
 /*  2570 */  1060, 1060,  623,  631,  625,  680,  673,  679,  678,  672,
 /*  2580 */  1060, 1060, 1060, 1060,  622,  631,  625,  680,  673,  679,
 /*  2590 */   678,  672, 1060, 1060, 1060, 1060,  621,  631,  625,  680,
 /*  2600 */   673,  679,  678,  672, 1060, 1060, 1060, 1060,  305,  631,
 /*  2610 */   625,  680,  673,  679,  678,  672, 1060, 1060, 1060, 1060,
 /*  2620 */   341,  631,  625,  680,  673,  679,  678,  672, 1060, 1060,
 /*  2630 */  1060, 1060,  340,  631,  625,  680,  673,  679,  678,  672,
 /*  2640 */  1060, 1060, 1060, 1060,  590,  631,  625,  680,  673,  679,
 /*  2650 */   678,  672, 1060, 1060, 1060, 1060,  589,  631,  625,  680,
 /*  2660 */   673,  679,  678,  672, 1060, 1060, 1060, 1060,  588,  631,
 /*  2670 */   625,  680,  673,  679,  678,  672, 1060, 1060, 1060, 1060,
 /*  2680 */   297,  631,  625,  680,  673,  679,  678,  672, 1060, 1060,
 /*  2690 */  1060, 1060,  296,  631,  625,  680,  673,  679,  678,  672,
 /*  2700 */  1060, 1060, 1060, 1060,  295,  631,  625,  680,  673,  679,
 /*  2710 */   678,  672, 1060, 1060, 1060, 1060,  294,  631,  625,  680,
 /*  2720 */   673,  679,  678,  672, 1060, 1060, 1060, 1060,  293,  631,
 /*  2730 */   625,  680,  673,  679,  678,  672, 1060, 1060, 1060, 1060,
 /*  2740 */   292,  631,  625,  680,  673,  679,  678,  672, 1060, 1060,
 /*  2750 */  1060, 1060,  291,  631,  625,  680,  673,  679,  678,  672,
 /*  2760 */  1060, 1060, 1060, 1060,  582,  631,  625,  680,  673,  679,
 /*  2770 */   678,  672, 1060, 1060, 1060, 1060,  273,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   136,  164,   71,  139,  140,  141,  142,  143,  144,  145,
 /*    10 */   146,  147,  148,  149,  150,  151,  152,  153,  181,  155,
 /*    20 */   156,  157,  158,  159,  160,  161,   95,   96,   97,   98,
 /*    30 */    99,  167,  168,  102,   96,   97,   98,   99,  174,  175,
 /*    40 */   102,  155,  156,  157,  158,  159,  160,  161,  108,  109,
 /*    50 */   110,  187,  188,  167,  168,  108,  109,  110,  172,  173,
 /*    60 */   174,  175,  176,  177,  155,  156,  157,  158,  159,  160,
 /*    70 */   161,  185,  186,   72,   98,   99,  167,  168,  102,   82,
 /*    80 */    73,  172,  173,  174,  175,  176,  177,    0,  224,  225,
 /*    90 */   226,  227,  228,  229,  230,  231,  232,  233,  234,  235,
 /*   100 */   236,  237,  238,  239,  240,    1,    2,  100,  101,    5,
 /*   110 */     6,    7,    8,    9,   10,   11,   12,    1,    2,    1,
 /*   120 */     2,    5,    6,  155,  156,  157,  158,  159,  160,  161,
 /*   130 */    33,   99,   28,  165,  166,  103,   32,   72,   34,   35,
 /*   140 */    43,   37,   72,   39,   47,    1,    2,   72,   44,    5,
 /*   150 */     6,  159,   48,   49,   50,   51,   52,   53,   54,   99,
 /*   160 */    56,   57,   46,  103,   48,   49,   50,   63,  164,   65,
 /*   170 */   159,   98,   99,   69,   70,  102,   72,  185,  186,   63,
 /*   180 */    76,   65,   72,   67,   73,  181,   70,  108,   72,   98,
 /*   190 */    99,  112,   76,  102,    1,    2,  185,  186,    5,    6,
 /*   200 */   155,  156,  157,  158,  159,  160,  161,   63,   73,   65,
 /*   210 */   106,  164,  167,  168,  103,  111,   72,  213,  214,  174,
 /*   220 */   175,  105,  106,  119,   71,    1,    2,  111,  181,    5,
 /*   230 */     6,   71,  187,  188,   73,  131,  132,  133,  103,  135,
 /*   240 */    73,   48,   49,   50,  198,  199,  200,  131,  132,  133,
 /*   250 */   106,   96,   97,   98,   99,  111,   63,  102,   65,   74,
 /*   260 */    67,  214,   82,   70,  103,   72,  108,  109,  110,   76,
 /*   270 */   103,   77,   48,   49,   50,  131,  132,  133,   72,   94,
 /*   280 */   155,  156,  157,  158,  159,  160,  161,   63,  102,   65,
 /*   290 */   102,   67,  167,  168,   70,   74,   72,  103,  105,  106,
 /*   300 */    76,   70,   74,   97,  111,  155,  156,  157,  158,  159,
 /*   310 */   160,  161,   81,   82,   83,   84,  166,   86,   87,   88,
 /*   320 */    89,  207,  208,  209,  131,  132,  133,  170,  171,  105,
 /*   330 */   106,  103,  210,  211,  212,  111,   41,  106,  107,  108,
 /*   340 */   109,  110,   31,  154,  155,  156,  157,  158,  159,  160,
 /*   350 */   161,  162,  163,   41,   74,  131,  132,  133,   74,   73,
 /*   360 */   203,  204,  205,  206,   77,  164,  155,  156,  157,  158,
 /*   370 */   159,  160,  161,    1,    2,   80,    1,    2,  167,  168,
 /*   380 */     5,    6,  181,   77,  173,  174,  175,  176,  177,  103,
 /*   390 */   103,   91,   92,  182,   22,   23,   24,   25,   97,   99,
 /*   400 */    75,   14,   30,   16,   17,   18,   19,   20,   21,  103,
 /*   410 */    38,   73,   40,   27,  213,  214,  164,   45,  106,  107,
 /*   420 */   108,  109,  110,   48,   49,   50,  215,  216,  217,   57,
 /*   430 */    58,   59,   60,  181,   26,   48,   49,   75,   63,  107,
 /*   440 */    65,  103,  107,   74,   72,   70,   73,   72,   74,   41,
 /*   450 */    42,   76,  100,  101,  155,  156,  157,  158,  159,  160,
 /*   460 */   161,   72,   76,   94,   55,   62,  167,  168,   94,   61,
 /*   470 */    62,  172,  173,  174,  175,  176,  177,    2,  106,  189,
 /*   480 */   190,  106,   74,  111,  185,  186,  111,   72,   80,    3,
 /*   490 */     4,  155,  156,  157,  158,  159,  160,  161,   95,   96,
 /*   500 */    97,   98,   99,  167,  168,  102,  131,  132,  133,  173,
 /*   510 */   174,  175,  176,  177,    2,    1,    2,  131,  182,    5,
 /*   520 */     6,  134,  155,  156,  157,  158,  159,  160,  161,   70,
 /*   530 */    73,   73,   73,  166,    1,    2,  199,  200,    5,    6,
 /*   540 */    81,   82,   83,   84,   68,   86,   87,   88,   89,  170,
 /*   550 */   171,  156,  216,  217,  106,  107,  108,  109,  110,    2,
 /*   560 */   103,  103,   48,   49,   50,  106,  107,  108,  109,  110,
 /*   570 */   175,   95,   96,   97,   98,   99,   73,   63,  102,   65,
 /*   580 */    41,   48,   49,   50,   72,  206,   72,  155,  156,  157,
 /*   590 */   158,  159,  160,  161,   73,   73,   63,   74,   65,  167,
 /*   600 */   168,   96,   97,   98,   99,   72,  103,  102,  208,  209,
 /*   610 */   155,  156,  157,  158,  159,  160,  161,   94,    1,  105,
 /*   620 */   106,   72,  167,  168,  103,  111,   72,  172,  173,  174,
 /*   630 */   175,  176,  177,   72,   95,   96,   97,   98,   99,  106,
 /*   640 */    74,  102,   91,   92,  111,  131,  132,  133,   72,   76,
 /*   650 */   155,  156,  157,  158,  159,  160,  161,  170,  171,    2,
 /*   660 */    94,   74,  167,  168,  131,  132,  133,  172,  173,  174,
 /*   670 */   175,  176,  177,  103,  155,  156,  157,  158,  159,  160,
 /*   680 */   161,  194,  195,  196,  197,    2,  167,  168,  201,  202,
 /*   690 */   103,  172,  173,  174,  175,  176,  177,  155,  156,  157,
 /*   700 */   158,  159,  160,  161,  131,   73,   82,  183,  184,  167,
 /*   710 */   168,   73,   76,   26,  172,  173,  174,  175,  176,  177,
 /*   720 */   155,  156,  157,  158,  159,  160,  161,   95,   96,   97,
 /*   730 */    98,   99,  167,  168,  102,   71,   74,  172,  173,  174,
 /*   740 */   175,  176,  177,   74,  106,  107,  108,  109,  110,  156,
 /*   750 */   133,  102,  155,  156,  157,  158,  159,  160,  161,   95,
 /*   760 */    96,   97,   98,   99,  167,  168,  102,  131,  175,  172,
 /*   770 */   173,  174,  175,  176,  177,  155,  156,  157,  158,  159,
 /*   780 */   160,  161,   95,   96,   97,   98,   99,  167,  168,  102,
 /*   790 */   191,  192,  172,  173,  174,  175,  176,  177,  170,  171,
 /*   800 */   155,  156,  157,  158,  159,  160,  161,   95,   96,   97,
 /*   810 */    98,   99,  167,  168,  102,  211,  212,  172,  173,  174,
 /*   820 */   175,  176,  177,  155,  156,  157,  158,  159,  160,  161,
 /*   830 */    71,   27,  204,  205,  206,  167,  168,  211,  212,   82,
 /*   840 */   172,  173,  174,  175,  176,  177,   77,  155,  156,  157,
 /*   850 */   158,  159,  160,  161,   95,   96,   97,   98,   99,  167,
 /*   860 */   168,  102,  211,  212,  172,  173,  174,  175,  176,  177,
 /*   870 */   155,  156,  157,  158,  159,  160,  161,   77,   73,   66,
 /*   880 */    76,   74,  167,  168,  211,  212,   74,  172,  173,  174,
 /*   890 */   175,  176,  177,  155,  156,  157,  158,  159,  160,  161,
 /*   900 */    95,   96,   97,   98,   99,  167,  168,  102,   74,   74,
 /*   910 */   172,  173,  174,  175,  176,  177,   74,  156,  156,   74,
 /*   920 */   155,  156,  157,  158,  159,  160,  161,   74,   94,   94,
 /*   930 */    74,   71,  167,  168,  159,  131,  175,  175,  173,  174,
 /*   940 */   175,  176,  177,  170,  171,  137,  138,  182,  155,  156,
 /*   950 */   157,  158,  159,  160,  161,   95,   96,   97,   98,   99,
 /*   960 */   167,  168,  102,   74,   74,   74,  173,  174,  175,  176,
 /*   970 */   177,   74,   74,   74,   74,  182,  155,  156,  157,  158,
 /*   980 */   159,  160,  161,   95,   96,   97,   98,   99,  167,  168,
 /*   990 */   102,  132,  164,  131,  173,  174,  175,  176,  177,  156,
 /*  1000 */   131,  219,   41,  182,  155,  156,  157,  158,  159,  160,
 /*  1010 */   161,   95,   96,   97,   98,   99,  167,  168,  102,  222,
 /*  1020 */   159,   61,  173,  174,  175,  176,  177,  159,   62,  223,
 /*  1030 */   156,  182,  155,  156,  157,  158,  159,  160,  161,  106,
 /*  1040 */   107,  108,  109,  110,  167,  168,  222,  190,  223,  219,
 /*  1050 */   173,  174,  175,  176,  177,  222,    2,  133,    2,  182,
 /*  1060 */   223,  155,  156,  157,  158,  159,  160,  161,  158,  197,
 /*  1070 */   197,  221,   42,  167,  168,  220,   26,  223,  220,  173,
 /*  1080 */   174,  175,  176,  177,  219,  222,  222,  221,  182,  155,
 /*  1090 */   156,  157,  158,  159,  160,  161,  220,  219,  223,  219,
 /*  1100 */   221,  167,  168,  222,  221,  223,  222,  173,  174,  175,
 /*  1110 */   176,  177,  156,  220,  219,  223,  182,  155,  156,  157,
 /*  1120 */   158,  159,  160,  161,  222,  221,  223,  222,  156,  167,
 /*  1130 */   168,  223,  223,  222,  222,  173,  174,  175,  176,  177,
 /*  1140 */   223,  220,  222,  221,  182,  155,  156,  157,  158,  159,
 /*  1150 */   160,  161,  223,  222,  219,  223,  220,  167,  168,  222,
 /*  1160 */   156,  223,  223,  173,  174,  175,  176,  177,  192,  158,
 /*  1170 */   219,  184,  182,  155,  156,  157,  158,  159,  160,  161,
 /*  1180 */   171,  171,  219,  171,  171,  167,  168,  171,  171,  220,
 /*  1190 */   171,  173,  174,  175,  176,  177,  171,  171,  171,  171,
 /*  1200 */   182,  155,  156,  157,  158,  159,  160,  161,   90,  223,
 /*  1210 */     2,  220,  220,  167,  168,  220,  219,    2,  172,  173,
 /*  1220 */   174,  175,  176,  177,  106,  107,  108,  109,  110,  155,
 /*  1230 */   156,  157,  158,  159,  160,  161,  241,  241,  241,  241,
 /*  1240 */   241,  167,  168,  241,  241,  241,  172,  173,  174,  175,
 /*  1250 */   176,  177,  106,  107,  108,  109,  110,  155,  156,  157,
 /*  1260 */   158,  159,  160,  161,  241,  241,  241,  241,  241,  167,
 /*  1270 */   168,  241,  241,  241,  172,  173,  174,  175,  176,  177,
 /*  1280 */   106,  107,  108,  109,  110,  155,  156,  157,  158,  159,
 /*  1290 */   160,  161,  241,  241,  241,  241,  241,  167,  168,  241,
 /*  1300 */   241,  241,  172,  173,  174,  175,  176,  177,  106,  107,
 /*  1310 */   108,  109,  110,  155,  156,  157,  158,  159,  160,  161,
 /*  1320 */   241,    1,    2,    1,    2,  167,  168,  241,  241,  241,
 /*  1330 */   172,  173,  174,  175,  176,  177,  241,  241,  241,  241,
 /*  1340 */   241,  155,  156,  157,  158,  159,  160,  161,  241,  241,
 /*  1350 */   241,  241,  241,  167,  168,  241,  241,  241,  172,  173,
 /*  1360 */   174,  175,  176,  177,  241,  241,  241,  241,  241,  155,
 /*  1370 */   156,  157,  158,  159,  160,  161,  241,  241,  241,  241,
 /*  1380 */   241,  167,  168,  241,  241,  241,  172,  173,  174,  175,
 /*  1390 */   176,  177,   72,  241,   72,  241,  241,  155,  156,  157,
 /*  1400 */   158,  159,  160,  161,  241,  241,  241,  241,  241,  167,
 /*  1410 */   168,  241,  241,  241,  172,  173,  174,  175,  176,  177,
 /*  1420 */   241,  241,  241,  241,   70,  241,  106,   73,  106,  170,
 /*  1430 */   171,  111,   70,  111,  241,  241,   82,   83,   84,  241,
 /*  1440 */    86,   87,   88,   89,   82,   83,   84,  241,   86,   87,
 /*  1450 */    88,   89,  193,  194,  195,  196,  197,  241,  241,  241,
 /*  1460 */   106,  107,  108,  109,  110,   73,  241,  241,  106,  107,
 /*  1470 */   108,  109,  110,   70,   82,   83,   84,  241,   86,   87,
 /*  1480 */    88,   89,  241,  241,  241,   82,   83,   84,   73,   86,
 /*  1490 */    87,   88,   89,  241,  241,  241,  241,  241,  106,  107,
 /*  1500 */   108,  109,  110,    1,    2,  241,   73,    5,    6,  106,
 /*  1510 */   107,  108,  109,  110,  155,  156,  157,  158,  159,  160,
 /*  1520 */   161,  106,  107,  108,  109,  110,  167,  168,   95,   96,
 /*  1530 */    97,   98,   99,  174,  175,  102,  154,  155,  156,  157,
 /*  1540 */   158,  159,  160,  161,  162,  163,  187,  188,  241,  241,
 /*  1550 */    48,   49,   50,  155,  156,  157,  158,  159,  160,  161,
 /*  1560 */   241,  241,  241,  241,  241,  167,  168,  241,  241,  241,
 /*  1570 */   241,  241,  174,  175,   72,   82,   83,   84,  241,   86,
 /*  1580 */    87,   88,   89,  241,  241,  187,  188,  241,  241,  241,
 /*  1590 */    82,   83,   84,  241,   86,   87,   88,   89,  241,  106,
 /*  1600 */   107,  108,  109,  110,  241,  241,  241,  241,  106,  241,
 /*  1610 */   241,  241,  241,  111,  106,  107,  108,  109,  110,  154,
 /*  1620 */   155,  156,  157,  158,  159,  160,  161,  162,  163,  241,
 /*  1630 */   241,  241,  241,  131,  132,  133,   82,   83,   84,  241,
 /*  1640 */    86,   87,   88,   89,  241,  241,  241,  241,   82,   83,
 /*  1650 */    84,  241,   86,   87,   88,   89,  241,  170,  171,  241,
 /*  1660 */   106,  107,  108,  109,  110,  155,  156,  157,  158,  159,
 /*  1670 */   160,  161,  106,  107,  108,  109,  110,  241,  241,  169,
 /*  1680 */   193,  194,  195,  196,  197,  241,  170,  171,  178,  179,
 /*  1690 */   180,  154,  155,  156,  157,  158,  159,  160,  161,  162,
 /*  1700 */   163,  241,  155,  156,  157,  158,  159,  160,  161,  193,
 /*  1710 */   194,  195,  196,  197,  241,  241,  169,  241,  241,  241,
 /*  1720 */   241,  170,  171,  241,  241,  178,  179,  180,  155,  156,
 /*  1730 */   157,  158,  159,  160,  161,  155,  156,  157,  158,  159,
 /*  1740 */   160,  161,  169,  241,  193,  194,  195,  196,  197,  169,
 /*  1750 */   241,  178,  179,  180,  241,  241,  170,  171,  178,  179,
 /*  1760 */   180,  241,  241,  241,  155,  156,  157,  158,  159,  160,
 /*  1770 */   161,  155,  156,  157,  158,  159,  160,  161,  169,   73,
 /*  1780 */   194,  195,  196,  197,  241,  169,  241,  178,  179,  180,
 /*  1790 */   241,  241,  241,  241,  178,  179,  180,  155,  156,  157,
 /*  1800 */   158,  159,  160,  161,  155,  156,  157,  158,  159,  160,
 /*  1810 */   161,  169,  106,  107,  108,  109,  110,  241,  169,  241,
 /*  1820 */   178,  179,  180,  241,  241,  241,  241,  178,  179,  180,
 /*  1830 */   155,  156,  157,  158,  159,  160,  161,  241,  241,  241,
 /*  1840 */   241,   73,  241,  241,  169,  155,  156,  157,  158,  159,
 /*  1850 */   160,  161,  241,  178,  179,  180,  241,  241,  241,  169,
 /*  1860 */   155,  156,  157,  158,  159,  160,  161,  241,  178,  179,
 /*  1870 */   180,  241,  167,  168,  106,  107,  108,  109,  110,  241,
 /*  1880 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  1890 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  1900 */   241,  241,   74,  241,  154,  155,  156,  157,  158,  159,
 /*  1910 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  1920 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  1930 */   160,  161,  241,  163,  106,  107,  108,  109,  110,  241,
 /*  1940 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  1950 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  1960 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  1970 */   170,  171,  241,  154,  155,  156,  157,  158,  159,  160,
 /*  1980 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  1990 */   161,  241,  163,  241,  194,  195,  196,  197,   74,  154,
 /*  2000 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  154,
 /*  2010 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  154,
 /*  2020 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  241,
 /*  2030 */   106,  107,  108,  109,  110,  154,  155,  156,  157,  158,
 /*  2040 */   159,  160,  161,  241,  163,  154,  155,  156,  157,  158,
 /*  2050 */   159,  160,  161,  241,  163,  241,  241,   74,  241,  154,
 /*  2060 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  154,
 /*  2070 */   155,  156,  157,  158,  159,  160,  161,    1,  163,  154,
 /*  2080 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  106,
 /*  2090 */   107,  108,  109,  110,  241,  154,  155,  156,  157,  158,
 /*  2100 */   159,  160,  161,  241,  163,  154,  155,  156,  157,  158,
 /*  2110 */   159,  160,  161,  241,  163,  154,  155,  156,  157,  158,
 /*  2120 */   159,  160,  161,  241,  163,  170,  171,  241,  154,  155,
 /*  2130 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2140 */   156,  157,  158,  159,  160,  161,  241,  163,   72,  194,
 /*  2150 */   195,  196,  197,  241,  154,  155,  156,  157,  158,  159,
 /*  2160 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  2170 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  2180 */   160,  161,  106,  163,  241,  241,  241,  111,  241,  241,
 /*  2190 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  2200 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  2210 */   241,  241,  241,  241,  154,  155,  156,  157,  158,  159,
 /*  2220 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  2230 */   160,  161,    1,  163,  154,  155,  156,  157,  158,  159,
 /*  2240 */   160,  161,  241,  163,  241,  241,  241,  241,  241,  241,
 /*  2250 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  2260 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  2270 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  2280 */   241,  241,  241,  154,  155,  156,  157,  158,  159,  160,
 /*  2290 */   161,  241,  163,  154,  155,  156,  157,  158,  159,  160,
 /*  2300 */   161,  241,  163,   72,  241,  241,  241,  241,  241,  154,
 /*  2310 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  154,
 /*  2320 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  154,
 /*  2330 */   155,  156,  157,  158,  159,  160,  161,  106,  163,  241,
 /*  2340 */   241,  241,  111,  241,  241,  154,  155,  156,  157,  158,
 /*  2350 */   159,  160,  161,  241,  163,  154,  155,  156,  157,  158,
 /*  2360 */   159,  160,  161,  241,  163,  241,  241,  241,  241,  154,
 /*  2370 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  154,
 /*  2380 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  154,
 /*  2390 */   155,  156,  157,  158,  159,  160,  161,  241,  163,  241,
 /*  2400 */   241,  241,  241,  241,  241,  154,  155,  156,  157,  158,
 /*  2410 */   159,  160,  161,  241,  163,  154,  155,  156,  157,  158,
 /*  2420 */   159,  160,  161,  241,  163,  154,  155,  156,  157,  158,
 /*  2430 */   159,  160,  161,  241,  163,  241,  241,  241,  154,  155,
 /*  2440 */   156,  157,  158,  159,  160,  161,  241,  163,  154,  155,
 /*  2450 */   156,  157,  158,  159,  160,  161,  241,  163,  241,  241,
 /*  2460 */   241,  241,  241,  241,  154,  155,  156,  157,  158,  159,
 /*  2470 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  2480 */   160,  161,  241,  163,  154,  155,  156,  157,  158,  159,
 /*  2490 */   160,  161,  241,  163,  241,  241,  241,  241,  241,  241,
 /*  2500 */   154,  155,  156,  157,  158,  159,  160,  161,  241,  163,
 /*  2510 */   155,  156,  157,  158,  159,  160,  161,  241,  241,  241,
 /*  2520 */   241,  166,  241,  241,  241,  155,  156,  157,  158,  159,
 /*  2530 */   160,  161,  241,  241,  241,  241,  166,  155,  156,  157,
 /*  2540 */   158,  159,  160,  161,  241,  241,  241,  241,  166,  155,
 /*  2550 */   156,  157,  158,  159,  160,  161,  241,  241,  241,  241,
 /*  2560 */   166,  155,  156,  157,  158,  159,  160,  161,  241,  241,
 /*  2570 */   241,  241,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2580 */   241,  241,  241,  241,  166,  155,  156,  157,  158,  159,
 /*  2590 */   160,  161,  241,  241,  241,  241,  166,  155,  156,  157,
 /*  2600 */   158,  159,  160,  161,  241,  241,  241,  241,  166,  155,
 /*  2610 */   156,  157,  158,  159,  160,  161,  241,  241,  241,  241,
 /*  2620 */   166,  155,  156,  157,  158,  159,  160,  161,  241,  241,
 /*  2630 */   241,  241,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2640 */   241,  241,  241,  241,  166,  155,  156,  157,  158,  159,
 /*  2650 */   160,  161,  241,  241,  241,  241,  166,  155,  156,  157,
 /*  2660 */   158,  159,  160,  161,  241,  241,  241,  241,  166,  155,
 /*  2670 */   156,  157,  158,  159,  160,  161,  241,  241,  241,  241,
 /*  2680 */   166,  155,  156,  157,  158,  159,  160,  161,  241,  241,
 /*  2690 */   241,  241,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2700 */   241,  241,  241,  241,  166,  155,  156,  157,  158,  159,
 /*  2710 */   160,  161,  241,  241,  241,  241,  166,  155,  156,  157,
 /*  2720 */   158,  159,  160,  161,  241,  241,  241,  241,  166,  155,
 /*  2730 */   156,  157,  158,  159,  160,  161,  241,  241,  241,  241,
 /*  2740 */   166,  155,  156,  157,  158,  159,  160,  161,  241,  241,
 /*  2750 */   241,  241,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2760 */   241,  241,  241,  241,  166,  155,  156,  157,  158,  159,
 /*  2770 */   160,  161,  241,  241,  241,  241,  166,
};
#define YY_SHIFT_USE_DFLT (-70)
#define YY_SHIFT_COUNT (444)
#define YY_SHIFT_MIN   (-69)
#define YY_SHIFT_MAX   (2231)
static const short yy_shift_ofst[] = {
 /*     0 */   -70,  104,  116,  116,  193,  193,  193,  193,  193,  193,
 /*    10 */   193,  193,  193,  193,  224,  224,  224,  224,  224,  224,
 /*    20 */   224,  224,  224,  224,  193,  193,  193,  193,  193,  193,
 /*    30 */   193,  193,  193,  193,  193,  193,  193,  375,  375,  375,
 /*    40 */   514,  514,  514,  514,  514,  514,  514,  514,  514,  514,
 /*    50 */   533,  533,  533,  533,  533,  533,  533,  533,  533,  533,
 /*    60 */   533,  533,  533,  533,  533,  533,  533,  533,  533,  533,
 /*    70 */   533,  533,  533,  533,  533,  533,  533,  533,  533,  533,
 /*    80 */   533,  533,  533,  533,  533,  533,  533,  533,  533,  533,
 /*    90 */   533,  533,  533,  533,  533,  533,  533,  533,  533,  533,
 /*   100 */   533,  533,  533,  533,  533,  533,  533, 1502, 1502, 1502,
 /*   110 */   144,  372,  144,  144,  144,  144,  144,  144,  144,  144,
 /*   120 */   144,  144,  144,  144,  144,  144,  144,  144,  144,  144,
 /*   130 */   144,  144,  144,  144,  144,  144, 1322, 1322, 1322, 1322,
 /*   140 */  1320, 1322, 1322, 1322, 1320,  804,  386, 1320,  573,  512,
 /*   150 */  1215, 1208,  859,  859, 2231,  636,  636,  636,  512,  512,
 /*   160 */   512,  512,  486,  636,  551,  573,  573, 1215, 1208, 1054,
 /*   170 */   539,  687,  687,  687,  687,  403,  312, 2076, 2076, 2076,
 /*   180 */  2076, 2076, 2076, 2076, 2076, 2076, 2076, 2076,  300,  295,
 /*   190 */   617,  486,  966,  869,  966,  960,  966,  960, 1050, 1030,
 /*   200 */   961,  869,  966,  960, 1050, 1030,  961,  869,  966,  960,
 /*   210 */  1050, 1030,  961,  966,  960,  966,  960,  966,  960,  966,
 /*   220 */   960,  966,  960, 1050, 1030,  961,  966,  960, 1050, 1030,
 /*   230 */   961,  966,  960, 1050, 1030, 1056, 1056,  924, 1054,  966,
 /*   240 */   960,  961,  966,  960,  869,  966,  960,  859,  859,  961,
 /*   250 */   869,  862,  859,  459,  231, 1354, 1392, 1403, 1362, 1566,
 /*   260 */  1554, 1508, 1493,  387, 1433,  805,  632,  860,  759,  664,
 /*   270 */   -69,  476,  408, 1983,  712,  712,  916,  916,  916, 1924,
 /*   280 */  1828, 1768, 1118,  888,  712, 1706, 1415,  638,  505, 1202,
 /*   290 */   155, 1174, 1174, 1174, 1174, 1174, 1174, 1174,  448,  448,
 /*   300 */   448,  448,  -62,  448,  448, 1174, 1174, 1174,  448,  448,
 /*   310 */   448,  448,  448,  448,  448,  448,  448,  448,  448,  448,
 /*   320 */   448,  448,  448,  448,  448,  448,  448,  448,  448, 1146,
 /*   330 */   933,  448,  448,   91,   91,    7,  158,  158,   73,   73,
 /*   340 */   -53,  -53,  -24,  -24,   97,  -53,  -53,  -60,  -60,  -60,
 /*   350 */   -60,  -60,  -60,  -60,  -60,  118,  835,  834,  587,  228,
 /*   360 */   352,  352,  352,  352,  566,  306,  523,  521,  287,  374,
 /*   370 */   503,  458,  457,   79,  194,  369,  338,  206,  185,   60,
 /*   380 */   286,   32,  167,  161,  135,  111,  813,  900,  899,  898,
 /*   390 */   897,  891,  890,  889,  856,  853,  845,  842,  812,  807,
 /*   400 */   800,  769,  649,  649,  757,  624,  669,  662,  683,  570,
 /*   410 */   657,  576,  561,  554,  557,  522,  549,  415,  475,  409,
 /*   420 */   373,  389,  335,  332,  362,  325,  301,  284,  280,  311,
 /*   430 */   221,  188,  188,  180,  186,  186,  160,  153,   -3,  110,
 /*   440 */    75,   70,   65,    1,   87,
};
#define YY_REDUCE_USE_DFLT (-164)
#define YY_REDUCE_COUNT (252)
#define YY_REDUCE_MIN   (-163)
#define YY_REDUCE_MAX   (2610)
static const short yy_reduce_ofst[] = {
 /*     0 */   808, -136,  211,  336,  299, -114, 1242, 1214, 1186, 1158,
 /*    10 */  1130, 1102, 1074, 1046, 1018,  990,  962,  934,  906,  877,
 /*    20 */   849,  821,  793,  765,  738,  715,  692,  668,  645,  620,
 /*    30 */   597,  565,  542,  519,  495,  455,  -91, 1398, 1359,   45,
 /*    40 */  1690, 1675, 1649, 1642, 1616, 1609, 1580, 1573, 1547, 1510,
 /*    50 */  1537, 1465, 1382,  189, 2346, 2330, 2320, 2310, 2294, 2284,
 /*    60 */  2271, 2261, 2251, 2235, 2225, 2215, 2201, 2191, 2175, 2165,
 /*    70 */  2155, 2139, 2129, 2116, 2106, 2096, 2080, 2070, 2060, 2046,
 /*    80 */  2036, 2020, 2010, 2000, 1984, 1974, 1961, 1951, 1941, 1925,
 /*    90 */  1915, 1905, 1891, 1881, 1865, 1855, 1845, 1829, 1819, 1806,
 /*   100 */  1796, 1786, 1770, 1760, 1750, 1736, 1726, 1705,  432,  125,
 /*   110 */   -32,  487, 2610, 2598, 2586, 2574, 2562, 2550, 2538, 2526,
 /*   120 */  2514, 2502, 2490, 2478, 2466, 2454, 2442, 2430, 2418, 2406,
 /*   130 */  2394, 2382, 2370, 2355,  367,  150, 1551, 1516, 1487, 1259,
 /*   140 */   157, 1955, 1800, 1586,  628,  201,    4,  379,   47,  122,
 /*   150 */   114,   46,   11,   -8,  773,  762,  761,  593,  673,  651,
 /*   160 */   626,  604,  599,  395,  524,  252, -163,  400,  337,  290,
 /*   170 */   997,  995,  992,  991,  969,  986,  963, 1028, 1027, 1026,
 /*   180 */  1025, 1019, 1017, 1016, 1013, 1012, 1010, 1009,  987,  951,
 /*   190 */  1011,  976,  939, 1004,  938,  937,  932,  931,  936,  922,
 /*   200 */   935,  972,  929,  920,  921,  904,  895,  956,  917,  912,
 /*   210 */   893,  883,  880,  909,  911,  908,  905,  903,  902,  892,
 /*   220 */   884,  882,  881,  876,  879,  878,  875,  864,  858,  866,
 /*   230 */   865,  854,  863,  855,  850,  873,  872,  910,  857,  837,
 /*   240 */   833,  830,  825,  824,  874,  806,  797,  868,  861,  782,
 /*   250 */   843,  828,  775,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   698, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*    10 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*    20 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*    30 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*    40 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*    50 */  1058, 1058, 1058, 1058, 1058,  896,  900,  895,  899,  912,
 /*    60 */   913, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*    70 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*    80 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*    90 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   100 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   110 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   120 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   130 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   140 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   150 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   160 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   170 */  1009, 1011, 1011, 1011, 1011, 1017, 1009, 1058, 1058, 1058,
 /*   180 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1009,
 /*   190 */  1058, 1058, 1017, 1058, 1017, 1015, 1017, 1015, 1011, 1013,
 /*   200 */  1009, 1058, 1017, 1015, 1011, 1013, 1009, 1058, 1017, 1015,
 /*   210 */  1011, 1013, 1009, 1017, 1015, 1017, 1015, 1017, 1015, 1017,
 /*   220 */  1015, 1017, 1015, 1011, 1013, 1009, 1017, 1015, 1011, 1013,
 /*   230 */  1009, 1017, 1015, 1011, 1013, 1058, 1058, 1058, 1058, 1017,
 /*   240 */  1015, 1009, 1017, 1015, 1058, 1017, 1015, 1058, 1058, 1009,
 /*   250 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   260 */   853,  853, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   270 */  1058, 1058,  762, 1058, 1010, 1012, 1001,  998,  889, 1058,
 /*   280 */  1058, 1058, 1058, 1016, 1008, 1058, 1058, 1058,  886,  807,
 /*   290 */   863,  875,  874,  873,  872,  871,  870,  869,  898,  902,
 /*   300 */   897,  901,  824,  914,  915,  878,  735,  736,  839,  838,
 /*   310 */   837,  836,  835,  834,  833,  855,  852,  851,  850,  849,
 /*   320 */   848,  847,  846,  845,  844,  843,  842,  841,  840, 1058,
 /*   330 */  1058,  732,  731,  888,  887, 1058,  812,  813,  865,  864,
 /*   340 */   789,  788,  826,  825,  907,  802,  803,  764,  763,  769,
 /*   350 */   768,  774,  773,  748,  749, 1058, 1058,  808, 1058, 1058,
 /*   360 */   978,  982,  981,  979, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   370 */  1058, 1058, 1058,  927, 1058, 1058, 1058, 1058, 1058,  718,
 /*   380 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   390 */  1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   400 */  1058,  757,  885,  884, 1058, 1058, 1058, 1058, 1058,  980,
 /*   410 */  1058,  968,  945,  947, 1058, 1058,  960,  943, 1058, 1058,
 /*   420 */  1058,  942,  933,  934, 1058, 1058, 1058, 1058, 1058, 1058,
 /*   430 */  1058,  862,  861,  853,  823,  822, 1058, 1058,  876,  734,
 /*   440 */   730,  728,  724,  721, 1058, 1057, 1056, 1055, 1054, 1053,
 /*   450 */  1052, 1051, 1050, 1049, 1048, 1047, 1046, 1045, 1044, 1043,
 /*   460 */  1042, 1037, 1036, 1038, 1035, 1034, 1033, 1032, 1031, 1030,
 /*   470 */  1029, 1028, 1027, 1026, 1025, 1024, 1023, 1022, 1021, 1020,
 /*   480 */  1019, 1018,  994,  993, 1000,  999, 1007, 1006, 1003, 1002,
 /*   490 */   997, 1005,  880,  882,  883,  881,  879,  760,  996,  995,
 /*   500 */   989,  988,  990,  987,  992,  991,  986,  984,  983,  985,
 /*   510 */   977,  972,  975,  976,  974,  973,  971,  963,  966,  970,
 /*   520 */   969,  967,  965,  964,  962,  938,  946,  948,  961,  959,
 /*   530 */   958,  957,  956,  955,  954,  953,  952,  951,  950,  949,
 /*   540 */   944,  926,  925,  941,  940,  936,  935,  932,  931,  930,
 /*   550 */   724,  929,  928,  814,  816,  815,  811,  810,  809,  808,
 /*   560 */   939,  937,  917,  920,  921,  924,  923,  922,  919,  918,
 /*   570 */   916, 1041, 1040, 1039,  857,  859,  868,  867,  866,  860,
 /*   580 */   858,  856,  787,  786,  785,  784,  783,  782,  792,  791,
 /*   590 */   790,  781,  780,  779,  778, 1014,  818,  820,  891,  894,
 /*   600 */   893,  892,  890,  832,  831,  830,  829,  828,  827,  821,
 /*   610 */   819,  817,  760,  910,  909,  908,  907,  906,  854,  877,
 /*   620 */   804,  806,  805,  801,  800,  799,  798,  797,  796,  795,
 /*   630 */   794,  793,  733,  904,  905,  903,  767,  766,  765,  772,
 /*   640 */   771,  770,  762,  761,  760,  759,  758,  757,  777,  776,
 /*   650 */   775,  756,  755,  754,  753,  750,  752,  751,  747,  746,
 /*   660 */   745,  744,  743,  742,  741,  740,  739,  738,  737,  729,
 /*   670 */   727,  726,  725,  723,  722,  720,  716,  715,  719,  718,
 /*   680 */   717,  714,  713,  712,  711,  710,  709,  708,  707,  706,
 /*   690 */   705,  704,  703,  702,  701,  700,  699,
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
 /* 202 */ "formula_card ::= CBRACKET_L formula CBRACKET_R",
 /* 203 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R",
 /* 204 */ "formula_card ::= CBRACKET_L formula CBRACKET_R term",
 /* 205 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term",
 /* 206 */ "card_var_lst ::= card_var_lst_inner PIPE",
 /* 207 */ "card_var_lst_inner ::= variable",
 /* 208 */ "card_var_lst_inner ::= card_var_lst_inner COMMA variable",
 /* 209 */ "head_formula ::= comparison",
 /* 210 */ "head_formula ::= atomic_formula",
 /* 211 */ "head_formula ::= formula_smpl_card",
 /* 212 */ "head_formula ::= TRUE",
 /* 213 */ "head_formula ::= FALSE",
 /* 214 */ "head_formula ::= DASH constant",
 /* 215 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 216 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 217 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 218 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 219 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 220 */ "macro_def_lst ::= macro_bnd",
 /* 221 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 222 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 223 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 224 */ "macro_args ::= macro_arg",
 /* 225 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 226 */ "macro_arg ::= POUND_INTEGER",
 /* 227 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 228 */ "sort_lst ::= sort",
 /* 229 */ "sort_lst ::= sort_lst COMMA sort",
 /* 230 */ "sort ::= sort_id_nr",
 /* 231 */ "sort ::= sort_id_nr STAR",
 /* 232 */ "sort ::= sort_id_nr CARROT",
 /* 233 */ "sort ::= sort_nr PLUS object_nullary",
 /* 234 */ "sort ::= sort_id PLUS object_nullary",
 /* 235 */ "sort ::= sort_id PLUS INTEGER",
 /* 236 */ "sort_id_nr ::= sort_id",
 /* 237 */ "sort_id_nr ::= sort_nr",
 /* 238 */ "sort_nr ::= num_range",
 /* 239 */ "sort_id ::= IDENTIFIER",
 /* 240 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 241 */ "constant_bnd_lst ::= constant_bnd",
 /* 242 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 243 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 244 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 245 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 246 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 247 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 248 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 249 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 250 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 251 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 252 */ "constant_dcl_type ::= ABACTION",
 /* 253 */ "constant_dcl_type ::= ACTION",
 /* 254 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 255 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 256 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 257 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 258 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 259 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 260 */ "constant_dcl_type ::= RIGID",
 /* 261 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 262 */ "constant_dcl_type ::= SDFLUENT",
 /* 263 */ "attrib_spec ::= ATTRIBUTE",
 /* 264 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 265 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 266 */ "object_bnd_lst ::= object_bnd",
 /* 267 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 268 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 269 */ "object_lst ::= object_spec",
 /* 270 */ "object_lst ::= object_lst COMMA object_spec",
 /* 271 */ "object_spec ::= IDENTIFIER",
 /* 272 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 273 */ "object_spec ::= num_range",
 /* 274 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 275 */ "variable_bnd_lst ::= variable_bnd",
 /* 276 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 277 */ "variable_bnd ::= variable_lst DBL_COLON sort_id",
 /* 278 */ "variable_lst ::= IDENTIFIER",
 /* 279 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 280 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 281 */ "sort_bnd_lst ::= sort_bnd",
 /* 282 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 283 */ "sort_bnd ::= sort_dcl_lst",
 /* 284 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 285 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 286 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 287 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 288 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 289 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 290 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 291 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 292 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 293 */ "show_lst ::= show_elem",
 /* 294 */ "show_lst ::= show_lst COMMA show_elem",
 /* 295 */ "show_elem ::= atomic_formula_one_const",
 /* 296 */ "stmt_noconcurrency ::= NOCONCURRENCY",
 /* 297 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY",
 /* 298 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD",
 /* 299 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD",
 /* 300 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 301 */ "query_lst ::= formula_temporal",
 /* 302 */ "query_lst ::= query_maxstep_decl",
 /* 303 */ "query_lst ::= query_label_decl",
 /* 304 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 305 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 306 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 307 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 308 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range",
 /* 309 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 310 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 311 */ "clause_if ::= IF formula",
 /* 312 */ "clause_if ::=",
 /* 313 */ "clause_after ::= AFTER formula",
 /* 314 */ "clause_after ::=",
 /* 315 */ "clause_ifcons ::= IFCONS formula",
 /* 316 */ "clause_ifcons ::=",
 /* 317 */ "clause_unless ::= UNLESS atomic_formula",
 /* 318 */ "clause_unless ::=",
 /* 319 */ "clause_where ::= WHERE formula_no_const",
 /* 320 */ "clause_where ::=",
 /* 321 */ "stmt_law ::= law_basic",
 /* 322 */ "stmt_law ::= law_caused",
 /* 323 */ "stmt_law ::= law_pcaused",
 /* 324 */ "stmt_law ::= law_impl",
 /* 325 */ "stmt_law ::= law_causes",
 /* 326 */ "stmt_law ::= law_increments",
 /* 327 */ "stmt_law ::= law_mcause",
 /* 328 */ "stmt_law ::= law_always",
 /* 329 */ "stmt_law ::= law_constraint",
 /* 330 */ "stmt_law ::= law_impossible",
 /* 331 */ "stmt_law ::= law_never",
 /* 332 */ "stmt_law ::= law_default",
 /* 333 */ "stmt_law ::= law_exogenous",
 /* 334 */ "stmt_law ::= law_inertial",
 /* 335 */ "stmt_law ::= law_nonexecutable",
 /* 336 */ "stmt_law ::= law_rigid",
 /* 337 */ "stmt_law ::= law_observed",
 /* 338 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 339 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 340 */ "law_pcaused ::= POSSIBLY_CAUSED atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 341 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 342 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 343 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 344 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 345 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 346 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 347 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 348 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 349 */ "law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 350 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 351 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 352 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 353 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 354 */ "law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD",
 /* 355 */ "stmt_code_blk ::= ASP_GR",
 /* 356 */ "stmt_code_blk ::= ASP_CP",
 /* 357 */ "stmt_code_blk ::= F2LP_GR",
 /* 358 */ "stmt_code_blk ::= F2LP_CP",
 /* 359 */ "stmt_code_blk ::= LUA_GR",
 /* 360 */ "stmt_code_blk ::= LUA_CP",
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
#line 2310 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 137: /* start */
    case 138: /* statement_lst */
    case 161: /* undeclared */
{
#line 208 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2319 "bcplus/parser/detail/lemon_parser.c"
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
#line 2332 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_macro_def */
{
#line 233 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy373));								
#line 2339 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_constant_def */
{
#line 235 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy353));								
#line 2346 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_object_def */
{
#line 237 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy172));								
#line 2353 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_variable_def */
{
#line 239 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy43));								
#line 2360 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 144: /* stmt_sort_def */
{
#line 241 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy339));								
#line 2367 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_noconcurrency */
{
#line 251 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy451));								
#line 2374 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 150: /* stmt_strong_noconcurrency */
{
#line 253 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy352));								
#line 2381 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* stmt_query */
{
#line 259 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy134));								
#line 2388 "bcplus/parser/detail/lemon_parser.c"
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
#line 2401 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* constant */
    case 164: /* constant_one_const */
{
#line 297 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy357));								
#line 2409 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 157: /* object */
    case 158: /* object_nullary */
{
#line 299 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy398));								
#line 2417 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* variable */
{
#line 303 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy313));								
#line 2424 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 160: /* lua */
{
#line 305 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy105));								
#line 2431 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 162: /* term_lst */
    case 165: /* term_no_const_lst */
{
#line 309 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy67));								
#line 2439 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 170: /* num_range */
{
#line 654 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy475));								
#line 2446 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* term_numeric */
{
#line 656 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy468));								
#line 2453 "bcplus/parser/detail/lemon_parser.c"
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
#line 717 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy374));								
#line 2468 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 175: /* atomic_formula */
    case 181: /* atomic_formula_one_const */
{
#line 723 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy6));								
#line 2476 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 176: /* formula_quant */
{
#line 725 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy155));								
#line 2483 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 183: /* quant_lst */
{
#line 921 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy358));								
#line 2490 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 184: /* quant_op */
{
#line 923 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2497 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* card_var_lst */
    case 186: /* card_var_lst_inner */
{
#line 960 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy279));								
#line 2505 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 188: /* formula_smpl_card */
{
#line 1036 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy73));								
#line 2512 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 189: /* macro_def_lst */
{
#line 1076 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy381));                              
#line 2519 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 190: /* macro_bnd */
{
#line 1078 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy83));                              
#line 2526 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 191: /* macro_args */
{
#line 1080 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy198));                              
#line 2533 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 192: /* macro_arg */
{
#line 1082 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy340));                              
#line 2540 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 193: /* sort_lst */
{
#line 1172 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy151));							
#line 2547 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 194: /* sort */
    case 195: /* sort_id_nr */
    case 196: /* sort_nr */
    case 197: /* sort_id */
{
#line 1174 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2557 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 198: /* constant_bnd_lst */
    case 199: /* constant_bnd */
{
#line 1314 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy77));									
#line 2565 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* constant_dcl_lst */
{
#line 1318 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy316));									
#line 2572 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* constant_dcl_type */
{
#line 1320 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2579 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* attrib_spec */
{
#line 1322 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2586 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* object_bnd_lst */
{
#line 1657 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy84));									
#line 2593 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* object_bnd */
{
#line 1659 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy180));									
#line 2600 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* object_lst */
    case 206: /* object_spec */
{
#line 1661 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy299));									
#line 2608 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 207: /* variable_bnd_lst */
    case 208: /* variable_bnd */
{
#line 1771 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy326));									
#line 2616 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 209: /* variable_lst */
{
#line 1775 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy58));									
#line 2623 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* sort_bnd_lst */
    case 211: /* sort_bnd */
    case 212: /* sort_dcl_lst */
{
#line 1848 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy233));									
#line 2632 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* show_lst */
{
#line 1952 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy345));									
#line 2639 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* show_elem */
    case 222: /* clause_unless */
{
#line 1954 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy6));									
#line 2647 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* query_lst */
{
#line 2099 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy165).l); DEALLOC((yypminor->yy165).maxstep); DEALLOC((yypminor->yy165).label);	
#line 2654 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* query_maxstep_decl */
{
#line 2101 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy234));												
#line 2661 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 218: /* query_label_Decl */
{
#line 2103 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2668 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* clause_if */
    case 220: /* clause_after */
    case 221: /* clause_ifcons */
    case 223: /* clause_where */
{
#line 2253 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy374));									
#line 2678 "bcplus/parser/detail/lemon_parser.c"
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
#line 2294 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy334));									
#line 2701 "bcplus/parser/detail/lemon_parser.c"
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
  { 177, 3 },
  { 177, 4 },
  { 177, 4 },
  { 177, 5 },
  { 185, 2 },
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
#line 3367 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 219 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy334;
			yymsp[0].minor.yy334  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3376 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 262 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy373; }
#line 3381 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy353; }
#line 3386 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy172; }
#line 3391 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy43; }
#line 3396 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy339; }
#line 3401 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy334; }
#line 3411 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 271 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy451; }
#line 3416 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy352; }
#line 3421 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 275 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy334 = yymsp[0].minor.yy134; }
#line 3426 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 319 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy357; }
#line 3431 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 40: /* term ::= base_elem */ yytestcase(yyruleno==40);
      case 56: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==56);
      case 81: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==81);
      case 96: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==96);
#line 320 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy113; }
#line 3440 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy398;	}
#line 3445 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 323 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy313; }
#line 3450 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 324 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy113 = yymsp[0].minor.yy105; }
#line 3455 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 36: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==36);
#line 395 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy357, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3461 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 37: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==37);
#line 396 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy357, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3467 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 397 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy398, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3472 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* object ::= object_nullary */
#line 398 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy398 = yymsp[0].minor.yy398; }
#line 3477 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object_nullary ::= OBJECT_ID */
#line 399 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy398, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3482 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= undeclared */
#line 400 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3487 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* variable ::= VARIABLE_ID */
#line 403 "bcplus/parser/detail/lemon_parser.y"
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
#line 3502 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 414 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy105, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy67, yymsp[0].minor.yy0); }
#line 3507 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* lua ::= AT_IDENTIFIER */
#line 415 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy105, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3512 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 416 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy49, yymsp[-3].minor.yy0, yymsp[-1].minor.yy67);   yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3519 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* undeclared ::= IDENTIFIER */
#line 417 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy49, yymsp[0].minor.yy0, NULL); }
#line 3524 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* term_lst ::= term */
      case 38: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==38);
#line 420 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy67 = new TermList();
			yygotominor.yy67->push_back(yymsp[0].minor.yy113);
		}
#line 3533 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* term_lst ::= term_lst COMMA term */
      case 39: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==39);
#line 426 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy67 = yymsp[-2].minor.yy67;
			yymsp[-2].minor.yy67->push_back(yymsp[0].minor.yy113);
		  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 3543 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 41: /* term ::= INTEGER */
      case 57: /* term_strong ::= INTEGER */ yytestcase(yyruleno==57);
      case 82: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==82);
      case 97: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==97);
#line 525 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy113, yymsp[0].minor.yy0);	}
#line 3551 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 42: /* term ::= STRING_LITERAL */
      case 44: /* term ::= TRUE */ yytestcase(yyruleno==44);
      case 45: /* term ::= FALSE */ yytestcase(yyruleno==45);
      case 58: /* term_strong ::= STRING_LITERAL */ yytestcase(yyruleno==58);
      case 83: /* term_no_const_strong ::= STRING_LITERAL */ yytestcase(yyruleno==83);
      case 98: /* term_no_const ::= STRING_LITERAL */ yytestcase(yyruleno==98);
      case 100: /* term_no_const ::= TRUE */ yytestcase(yyruleno==100);
      case 101: /* term_no_const ::= FALSE */ yytestcase(yyruleno==101);
#line 526 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy113, yymsp[0].minor.yy0); }
#line 3563 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 43: /* term ::= PAREN_L term PAREN_R */
      case 59: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==99);
#line 527 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy113, yymsp[-2].minor.yy0, yymsp[-1].minor.yy113, yymsp[0].minor.yy0); }
#line 3571 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 46: /* term ::= MAXSTEP */
      case 60: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==60);
      case 85: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==85);
#line 530 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3578 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 47: /* term ::= MAXADDITIVE */
      case 61: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==61);
      case 86: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==86);
#line 531 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3585 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 48: /* term ::= MAXAFVALUE */
      case 62: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==87);
#line 532 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy113, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3592 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= DASH term */
      case 63: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==63);
      case 89: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==89);
      case 103: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==103);
#line 536 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, UnaryTerm::Operator::NEGATIVE); }
#line 3600 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= ABS term */
      case 64: /* term_strong ::= ABS term */ yytestcase(yyruleno==64);
      case 90: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==90);
      case 104: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==104);
#line 537 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, UnaryTerm::Operator::ABS); }
#line 3608 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= term DASH term */
      case 66: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==66);
      case 76: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==76);
      case 91: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==91);
      case 105: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==105);
#line 541 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MINUS); }
#line 3617 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= term PLUS term */
      case 67: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==67);
      case 77: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==77);
      case 92: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==92);
      case 106: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==106);
#line 542 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::PLUS); }
#line 3626 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= term STAR term */
      case 68: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==68);
      case 78: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==78);
      case 93: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==93);
      case 107: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==107);
#line 543 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::TIMES); }
#line 3635 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= term INT_DIV term */
      case 69: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==69);
      case 79: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==79);
      case 94: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==94);
      case 108: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==108);
#line 544 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::DIVIDE); }
#line 3644 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= term MOD term */
      case 70: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==70);
      case 80: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==80);
      case 95: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==95);
      case 109: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==109);
#line 545 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MOD); }
#line 3653 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 65: /* term_strong_candidate ::= DASH constant */
#line 564 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy357, UnaryTerm::Operator::NEGATIVE); }
#line 3658 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 71: /* term_strong ::= constant DASH term */
#line 573 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MINUS); }
#line 3663 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 72: /* term_strong ::= constant PLUS term */
#line 574 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::PLUS); }
#line 3668 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 73: /* term_strong ::= constant STAR term */
#line 575 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::TIMES); }
#line 3673 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant INT_DIV term */
#line 576 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::DIVIDE); }
#line 3678 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant MOD term */
#line 577 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy113, yymsp[-2].minor.yy357, yymsp[-1].minor.yy0, yymsp[0].minor.yy113, BinaryTerm::Operator::MOD); }
#line 3683 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 88: /* term_no_const_strong ::= constant */
#line 599 "bcplus/parser/detail/lemon_parser.y"
{
		// error handling for constants so they don'yygotominor.yy113 default to undeclared identifiers
		yygotominor.yy113 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy357;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy357->beginLoc());
		YYERROR;
	}
#line 3694 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 102: /* term_no_const ::= constant */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy113 default to undeclared identifiers
		yygotominor.yy113 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy357;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy357->beginLoc());
		YYERROR;
	}
#line 3705 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 110: /* num_range ::= term_numeric DBL_PERIOD term_numeric */
#line 659 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy468, r_ptr = yymsp[0].minor.yy468, s_ptr = yymsp[-1].minor.yy0;

	yygotominor.yy475 = new NumberRange(yymsp[-2].minor.yy468->val(), yymsp[0].minor.yy468->val(), yymsp[-2].minor.yy468->beginLoc(), yymsp[0].minor.yy468->endLoc());

}
#line 3715 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 111: /* term_numeric ::= INTEGER */
#line 667 "bcplus/parser/detail/lemon_parser.y"
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
#line 3731 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 112: /* term_numeric ::= PAREN_L term_numeric PAREN_R */
#line 680 "bcplus/parser/detail/lemon_parser.y"
{ 
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy468 = yymsp[-1].minor.yy468;  
	yygotominor.yy468->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy468->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 3741 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 113: /* term_numeric ::= DASH term_numeric */
#line 700 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy468, yymsp[0].minor.yy468, -1 * yymsp[0].minor.yy468->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3747 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 114: /* term_numeric ::= ABS term_numeric */
#line 701 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy468, yymsp[0].minor.yy468, yymsp[0].minor.yy468->val() < 0 ? - yymsp[0].minor.yy468->val() : yymsp[0].minor.yy468->val());   yy_destructor(yypParser,111,&yymsp[-1].minor);
}
#line 3753 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 115: /* term_numeric ::= term_numeric DASH term_numeric */
#line 703 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() - yymsp[0].minor.yy468->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3759 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 116: /* term_numeric ::= term_numeric PLUS term_numeric */
#line 704 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() + yymsp[0].minor.yy468->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 3765 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 117: /* term_numeric ::= term_numeric STAR term_numeric */
#line 705 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() * yymsp[0].minor.yy468->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 3771 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 118: /* term_numeric ::= term_numeric INT_DIV term_numeric */
#line 706 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() / yymsp[0].minor.yy468->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 3777 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 119: /* term_numeric ::= term_numeric MOD term_numeric */
#line 707 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy468, yymsp[-2].minor.yy468, yymsp[0].minor.yy468, yymsp[-2].minor.yy468->val() % yymsp[0].minor.yy468->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 3783 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 120: /* formula ::= formula_base */
      case 159: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==159);
      case 182: /* formula_temporal ::= formula_base */ yytestcase(yyruleno==182);
#line 763 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy374;				}
#line 3790 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 121: /* formula ::= PAREN_L formula PAREN_R */
      case 160: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==160);
      case 183: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==183);
#line 764 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[-1].minor.yy374; yygotominor.yy374->parens(true); 	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3799 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 122: /* formula ::= NOT formula */
      case 161: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==161);
      case 184: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==184);
#line 765 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 3806 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 123: /* formula ::= DASH formula */
      case 162: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==162);
      case 185: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==185);
#line 766 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 3813 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 124: /* formula ::= formula AMP formula */
      case 163: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==163);
      case 186: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==186);
#line 767 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy374, yymsp[0].minor.yy374, yymsp[-2].minor.yy374->beginLoc(), yymsp[0].minor.yy374->endLoc());   yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3821 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 125: /* formula ::= formula DBL_PLUS formula */
      case 126: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==126);
      case 164: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==164);
      case 165: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==165);
      case 187: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==187);
      case 188: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==188);
#line 768 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::OR); }
#line 3831 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 127: /* formula ::= formula EQUIV formula */
      case 166: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==166);
      case 189: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==189);
#line 770 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::EQUIV); }
#line 3838 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 128: /* formula ::= formula IMPL formula */
      case 129: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==129);
      case 167: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==167);
      case 168: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==168);
      case 190: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==190);
      case 191: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==191);
#line 771 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy374, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, BinaryFormula::Operator::IMPL); }
#line 3848 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* formula_base ::= comparison */
      case 169: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==169);
      case 209: /* head_formula ::= comparison */ yytestcase(yyruleno==209);
#line 774 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy374; }
#line 3855 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 131: /* formula_base ::= atomic_formula */
      case 210: /* head_formula ::= atomic_formula */ yytestcase(yyruleno==210);
#line 775 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy6; }
#line 3861 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* formula_base ::= formula_quant */
#line 776 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = yymsp[0].minor.yy155; }
#line 3866 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* formula_base ::= formula_card */
#line 778 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy374 = yymsp[0].minor.yy374;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy374->beginLoc());
			YYERROR;
		}
	}
#line 3877 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* formula_base ::= TRUE */
      case 170: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==170);
      case 212: /* head_formula ::= TRUE */ yytestcase(yyruleno==212);
#line 785 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3884 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* formula_base ::= FALSE */
      case 171: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==171);
      case 213: /* head_formula ::= FALSE */ yytestcase(yyruleno==213);
#line 786 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3891 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* comparison ::= term_strong EQ term */
      case 143: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==143);
      case 172: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==172);
#line 788 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3899 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* comparison ::= term_strong DBL_EQ term */
      case 144: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==144);
      case 173: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==173);
#line 789 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3907 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* comparison ::= term_strong NEQ term */
      case 145: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==145);
      case 174: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==174);
#line 790 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3915 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* comparison ::= term_strong LTHAN term */
      case 146: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==146);
      case 175: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==175);
#line 791 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3923 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* comparison ::= term_strong GTHAN term */
      case 147: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==147);
      case 176: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==176);
#line 792 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3931 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* comparison ::= term_strong LTHAN_EQ term */
      case 148: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==148);
      case 177: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==177);
#line 793 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3939 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* comparison ::= term_strong GTHAN_EQ term */
      case 149: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==149);
      case 178: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==178);
#line 794 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy113, yymsp[0].minor.yy113, yymsp[-2].minor.yy113->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 3947 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 150: /* comparison ::= constant DBL_EQ term */
#line 802 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3953 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 151: /* comparison ::= constant NEQ term */
#line 803 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3959 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* comparison ::= constant LTHAN term */
#line 804 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3965 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* comparison ::= constant GTHAN term */
#line 805 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3971 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* comparison ::= constant LTHAN_EQ term */
#line 806 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3977 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* comparison ::= constant GTHAN_EQ term */
#line 807 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 3983 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* atomic_formula ::= constant */
      case 179: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==179);
#line 837 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy6, yymsp[0].minor.yy357, "true"); }
#line 3989 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* atomic_formula ::= TILDE constant */
      case 180: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==180);
#line 838 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy6, yymsp[0].minor.yy357, "false");   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 3996 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* atomic_formula ::= constant EQ term */
      case 181: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==181);
#line 839 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = new AtomicFormula(yymsp[-2].minor.yy357, yymsp[0].minor.yy113, yymsp[-2].minor.yy357->beginLoc(), yymsp[0].minor.yy113->endLoc());	  yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4003 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 192: /* formula_temporal ::= term_strong COLON formula_temporal */
#line 914 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy374, yymsp[-2].minor.yy113, yymsp[-1].minor.yy0, yymsp[0].minor.yy374); }
#line 4008 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 193: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
#line 926 "bcplus/parser/detail/lemon_parser.y"
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
#line 4025 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 194: /* quant_lst ::= quant_op variable */
#line 940 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy358 = new QuantifierFormula::QuantifierList();
		yygotominor.yy358->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy423, yymsp[0].minor.yy313));
	}
#line 4033 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 195: /* quant_lst ::= quant_lst quant_op variable */
#line 946 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy358 = yymsp[-2].minor.yy358;
		yygotominor.yy358->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy423, yymsp[0].minor.yy313));
	}
#line 4041 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 196: /* quant_op ::= BIG_CONJ */
#line 951 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4047 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 197: /* quant_op ::= BIG_DISJ */
#line 952 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,92,&yymsp[0].minor);
}
#line 4053 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 198: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
#line 998 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, NULL);  }
#line 4058 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 199: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
#line 999 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, yymsp[-4].minor.yy113, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy374,  yymsp[0].minor.yy0, NULL);  }
#line 4063 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 200: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 1000 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4068 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 201: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 1001 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, yymsp[-5].minor.yy113, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy374,  yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4073 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 202: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
#line 1002 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, NULL);  }
#line 4078 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 203: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
#line 1003 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, yymsp[-3].minor.yy113, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy374,  yymsp[0].minor.yy0, NULL);  }
#line 4083 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 204: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
#line 1004 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4088 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 205: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
#line 1005 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy374, yymsp[-4].minor.yy113, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy374,  yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4093 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 206: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1009 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy279 = yymsp[-1].minor.yy279;
	  yy_destructor(yypParser,99,&yymsp[0].minor);
}
#line 4101 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 207: /* card_var_lst_inner ::= variable */
#line 1014 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy313;
		yygotominor.yy279 = new CardinalityFormula::VariableList();
		yygotominor.yy279->push_back(yymsp[0].minor.yy313->symbol());
	}
#line 4110 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1021 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy313;
		yygotominor.yy279 = yymsp[-2].minor.yy279;
		yygotominor.yy279->push_back(yymsp[0].minor.yy313->symbol());
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4120 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* head_formula ::= formula_smpl_card */
#line 1041 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy374 = yymsp[0].minor.yy73;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy73->beginLoc());
			YYERROR;
		}
	}
#line 4131 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* head_formula ::= DASH constant */
#line 1051 "bcplus/parser/detail/lemon_parser.y"
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
#line 4147 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 215: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1064 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy6, yymsp[0].minor.yy0, NULL);  }
#line 4152 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1065 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, yymsp[-4].minor.yy113, yymsp[-3].minor.yy0, yymsp[-2].minor.yy279, yymsp[-1].minor.yy6,  yymsp[0].minor.yy0, NULL);  }
#line 4157 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1066 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy6, yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4162 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 218: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1067 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy73, yymsp[-5].minor.yy113, yymsp[-4].minor.yy0, yymsp[-3].minor.yy279, yymsp[-2].minor.yy6,  yymsp[-1].minor.yy0, yymsp[0].minor.yy113); }
#line 4167 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 219: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1086 "bcplus/parser/detail/lemon_parser.y"
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
#line 4197 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 220: /* macro_def_lst ::= macro_bnd */
#line 1114 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy381 = new MacroDeclaration::ElementList();
        yygotominor.yy381->push_back(yymsp[0].minor.yy83);
    }
#line 4205 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 221: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1120 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy381 = yymsp[-2].minor.yy381;
        yygotominor.yy381->push_back(yymsp[0].minor.yy83);
      yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4214 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1126 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy198;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy83 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy198);
      yy_destructor(yypParser,72,&yymsp[-4].minor);
  yy_destructor(yypParser,73,&yymsp[-2].minor);
  yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4228 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 223: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1135 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy83 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4239 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 224: /* macro_args ::= macro_arg */
#line 1143 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy198 = new MacroSymbol::ArgumentList();
        yygotominor.yy198->push_back(yymsp[0].minor.yy340->str());
        delete yymsp[0].minor.yy340;
    }
#line 4248 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 225: /* macro_args ::= macro_args COMMA macro_arg */
#line 1149 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy198 = yymsp[-2].minor.yy198;
        yygotominor.yy198->push_back(yymsp[0].minor.yy340->str());
        delete yymsp[0].minor.yy340;
      yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4258 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 226: /* macro_arg ::= POUND_INTEGER */
      case 227: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==227);
#line 1156 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy340 = yymsp[0].minor.yy0;
    }
#line 4266 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 228: /* sort_lst ::= sort */
#line 1183 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy151 = new ConstantSymbol::SortList();
		yygotominor.yy151->push_back(yymsp[0].minor.yy325);
	}
#line 4274 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 229: /* sort_lst ::= sort_lst COMMA sort */
#line 1188 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy151 = yymsp[-2].minor.yy151;
		yygotominor.yy151->push_back(yymsp[0].minor.yy325);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4283 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 230: /* sort ::= sort_id_nr */
      case 236: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==236);
      case 237: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==237);
#line 1243 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy325 = yymsp[0].minor.yy325; }
#line 4290 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 231: /* sort ::= sort_id_nr STAR */
#line 1244 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-1].minor.yy325, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, *yymsp[-1].minor.yy325->base() + "__plus_none_0", "none"); }
#line 4295 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* sort ::= sort_id_nr CARROT */
#line 1245 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-1].minor.yy325, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, *yymsp[-1].minor.yy325->base() + "__plus_unknown_0__", "unknown"); }
#line 4300 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 233: /* sort ::= sort_nr PLUS object_nullary */
      case 234: /* sort ::= sort_id PLUS object_nullary */ yytestcase(yyruleno==234);
#line 1247 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy325, yymsp[-2].minor.yy325, yymsp[-1].minor.yy0, yymsp[0].minor.yy398); }
#line 4306 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* sort ::= sort_id PLUS INTEGER */
#line 1250 "bcplus/parser/detail/lemon_parser.y"
{ ref_ptr<const Referenced> i_ptr = yymsp[0].minor.yy0; DYNAMIC_SORT_SYM(yygotominor.yy325, yymsp[-2].minor.yy325, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, *yymsp[-2].minor.yy325->base() + "__plus_" + *yymsp[0].minor.yy0->str(), (*((std::string const*)yymsp[0].minor.yy0->str()))); }
#line 4311 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 238: /* sort_nr ::= num_range */
#line 1257 "bcplus/parser/detail/lemon_parser.y"
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
#line 4350 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 239: /* sort_id ::= IDENTIFIER */
#line 1294 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy325 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy325) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4363 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 240: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1325 "bcplus/parser/detail/lemon_parser.y"
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
#line 4382 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 241: /* constant_bnd_lst ::= constant_bnd */
#line 1342 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy77 = yymsp[0].minor.yy77;
	}
#line 4389 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 242: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1347 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy77;
		yygotominor.yy77 = yymsp[-2].minor.yy77;
		yygotominor.yy77->splice(yygotominor.yy77->end(), *yymsp[0].minor.yy77);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4399 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 243: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1367 "bcplus/parser/detail/lemon_parser.y"
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
#line 4418 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 244: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1380 "bcplus/parser/detail/lemon_parser.y"
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
#line 4433 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 245: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1391 "bcplus/parser/detail/lemon_parser.y"
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
#line 4448 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 246: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1402 "bcplus/parser/detail/lemon_parser.y"
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
#line 4477 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 247: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1426 "bcplus/parser/detail/lemon_parser.y"
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
#line 4558 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 248: /* constant_dcl_lst ::= IDENTIFIER */
#line 1502 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = new IdentifierDeclList();
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 4566 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 249: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1507 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = new IdentifierDeclList();
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy151));
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4576 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 250: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1512 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = yymsp[-2].minor.yy316;
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4585 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 251: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1517 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy316 = yymsp[-5].minor.yy316;
		yygotominor.yy316->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy151));
	  yy_destructor(yypParser,103,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4596 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 252: /* constant_dcl_type ::= ABACTION */
#line 1524 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4608 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 253: /* constant_dcl_type ::= ACTION */
#line 1533 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4620 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 254: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1542 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4632 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 255: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1551 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4644 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 256: /* constant_dcl_type ::= EXTERNALACTION */
#line 1560 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4656 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 257: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1569 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4668 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 258: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1578 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4680 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 259: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1587 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4692 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 260: /* constant_dcl_type ::= RIGID */
#line 1596 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4704 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 261: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1605 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4716 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 262: /* constant_dcl_type ::= SDFLUENT */
#line 1615 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4728 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 263: /* attrib_spec ::= ATTRIBUTE */
#line 1625 "bcplus/parser/detail/lemon_parser.y"
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
#line 4743 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 264: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1638 "bcplus/parser/detail/lemon_parser.y"
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
#line 4759 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1666 "bcplus/parser/detail/lemon_parser.y"
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
#line 4784 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* object_bnd_lst ::= object_bnd */
#line 1689 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy84 = new ObjectDeclaration::ElementList();
		yygotominor.yy84->push_back(yymsp[0].minor.yy180);
	}
#line 4792 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 267: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 1695 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy84 = yymsp[-2].minor.yy84;
		yygotominor.yy84->push_back(yymsp[0].minor.yy180);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4801 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 1701 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy180 = new ObjectDeclaration::Element(yymsp[0].minor.yy325, yymsp[-2].minor.yy299);
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4809 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 269: /* object_lst ::= object_spec */
#line 1706 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = yymsp[0].minor.yy299;
	}
#line 4816 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 270: /* object_lst ::= object_lst COMMA object_spec */
#line 1710 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy299 = yymsp[-2].minor.yy299;
		yygotominor.yy299->splice(yygotominor.yy299->end(), *yymsp[0].minor.yy299);
		delete yymsp[0].minor.yy299;
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4826 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 271: /* object_spec ::= IDENTIFIER */
#line 1719 "bcplus/parser/detail/lemon_parser.y"
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
#line 4842 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 272: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1732 "bcplus/parser/detail/lemon_parser.y"
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
#line 4861 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 273: /* object_spec ::= num_range */
#line 1746 "bcplus/parser/detail/lemon_parser.y"
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
#line 4881 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 274: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 1778 "bcplus/parser/detail/lemon_parser.y"
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
#line 4912 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 275: /* variable_bnd_lst ::= variable_bnd */
#line 1807 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = yymsp[0].minor.yy326;
	}
#line 4919 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 276: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 1812 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = yymsp[-2].minor.yy326;
		yygotominor.yy326->splice(yygotominor.yy326->end(), *yymsp[0].minor.yy326);
		delete yymsp[0].minor.yy326;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4929 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 277: /* variable_bnd ::= variable_lst DBL_COLON sort_id */
#line 1819 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy326 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy58) {
			yygotominor.yy326->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy325));
		}
		delete yymsp[-2].minor.yy58;
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4942 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 278: /* variable_lst ::= IDENTIFIER */
#line 1829 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy58 = new TokenList();
		yygotominor.yy58->push_back(yymsp[0].minor.yy0);
	}
#line 4950 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 279: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 1834 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy58 = yymsp[-2].minor.yy58;
		yygotominor.yy58->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4959 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 280: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 1855 "bcplus/parser/detail/lemon_parser.y"
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
#line 4977 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 281: /* sort_bnd_lst ::= sort_bnd */
      case 283: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==283);
#line 1871 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[0].minor.yy233;
	}
#line 4985 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 282: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 1876 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[-2].minor.yy233;
		yygotominor.yy233->splice(yygotominor.yy233->end(), *yymsp[0].minor.yy233);
		delete yymsp[0].minor.yy233;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4995 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 284: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 1888 "bcplus/parser/detail/lemon_parser.y"
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
#line 5011 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 285: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 1900 "bcplus/parser/detail/lemon_parser.y"
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
#line 5026 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 286: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 1911 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy233 = yymsp[-1].minor.yy233;
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 5035 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* sort_dcl_lst ::= IDENTIFIER */
#line 1916 "bcplus/parser/detail/lemon_parser.y"
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
#line 5052 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 1930 "bcplus/parser/detail/lemon_parser.y"
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
#line 5071 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 1957 "bcplus/parser/detail/lemon_parser.y"
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
#line 5087 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 1971 "bcplus/parser/detail/lemon_parser.y"
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
#line 5105 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 1988 "bcplus/parser/detail/lemon_parser.y"
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
#line 5121 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2002 "bcplus/parser/detail/lemon_parser.y"
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
#line 5139 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* show_lst ::= show_elem */
#line 2020 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy345 = new ShowStatement::ElementList();
		yygotominor.yy345->push_back(yymsp[0].minor.yy6);
	}
#line 5147 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 294: /* show_lst ::= show_lst COMMA show_elem */
#line 2025 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy345 = yymsp[-2].minor.yy345;
		yygotominor.yy345->push_back(yymsp[0].minor.yy6);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5156 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* show_elem ::= atomic_formula_one_const */
#line 2030 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = yymsp[0].minor.yy6; }
#line 5161 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* stmt_noconcurrency ::= NOCONCURRENCY */
#line 2053 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy451, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5166 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY */
#line 2054 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy352, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5171 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD */
#line 2080 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy334, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy468, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5177 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD */
#line 2081 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy334, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy468, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5183 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2106 "bcplus/parser/detail/lemon_parser.y"
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
#line 5216 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* query_lst ::= formula_temporal */
#line 2138 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = NULL;
		yygotominor.yy165.label = NULL;

		yygotominor.yy165.l->push_back(yymsp[0].minor.yy374);
	}
#line 5227 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* query_lst ::= query_maxstep_decl */
#line 2147 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = yymsp[0].minor.yy234;
		yygotominor.yy165.label = NULL;
	}
#line 5236 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* query_lst ::= query_label_decl */
#line 2154 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165.l = new QueryStatement::FormulaList();
		yygotominor.yy165.maxstep = NULL;
		yygotominor.yy165.label = yymsp[0].minor.yy340;
	}
#line 5245 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2161 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy165 = yymsp[-2].minor.yy165;
		yymsp[-2].minor.yy165.l->push_back(yymsp[0].minor.yy374);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5254 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2167 "bcplus/parser/detail/lemon_parser.y"
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
#line 5270 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2180 "bcplus/parser/detail/lemon_parser.y"
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
#line 5286 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2206 "bcplus/parser/detail/lemon_parser.y"
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
#line 5311 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range */
#line 2227 "bcplus/parser/detail/lemon_parser.y"
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
#line 5328 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 310: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==310);
#line 2241 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy340, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5335 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* clause_if ::= IF formula */
#line 2276 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_IF); 		}
#line 5340 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* clause_if ::= */
      case 314: /* clause_after ::= */ yytestcase(yyruleno==314);
      case 316: /* clause_ifcons ::= */ yytestcase(yyruleno==316);
      case 320: /* clause_where ::= */ yytestcase(yyruleno==320);
#line 2277 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy374 = NULL; }
#line 5348 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* clause_after ::= AFTER formula */
#line 2278 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_AFTER);	}
#line 5353 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* clause_ifcons ::= IFCONS formula */
#line 2280 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_IFCONS); 	}
#line 5358 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* clause_unless ::= UNLESS atomic_formula */
#line 2282 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy6, yymsp[-1].minor.yy0, yymsp[0].minor.yy6, Language::Feature::CLAUSE_UNLESS); 	}
#line 5363 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* clause_unless ::= */
#line 2283 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy6 = NULL; }
#line 5368 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* clause_where ::= WHERE formula_no_const */
#line 2284 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy374, yymsp[-1].minor.yy0, yymsp[0].minor.yy374, Language::Feature::CLAUSE_WHERE); 	}
#line 5373 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* stmt_law ::= law_basic */
      case 322: /* stmt_law ::= law_caused */ yytestcase(yyruleno==322);
      case 323: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==323);
      case 324: /* stmt_law ::= law_impl */ yytestcase(yyruleno==324);
      case 325: /* stmt_law ::= law_causes */ yytestcase(yyruleno==325);
      case 326: /* stmt_law ::= law_increments */ yytestcase(yyruleno==326);
      case 327: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==327);
      case 328: /* stmt_law ::= law_always */ yytestcase(yyruleno==328);
      case 329: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==329);
      case 330: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==330);
      case 331: /* stmt_law ::= law_never */ yytestcase(yyruleno==331);
      case 332: /* stmt_law ::= law_default */ yytestcase(yyruleno==332);
      case 333: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==333);
      case 334: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==334);
      case 335: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==335);
      case 336: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==336);
      case 337: /* stmt_law ::= law_observed */ yytestcase(yyruleno==337);
#line 2328 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy334 = yymsp[0].minor.yy334;}
#line 5394 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 338: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2443 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, NULL, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
																																															Language::Feature::LAW_BASIC_D, BasicLaw); }
#line 5401 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 339: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2447 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy374, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 5408 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 340: /* law_pcaused ::= POSSIBLY_CAUSED atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2451 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy6, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 5415 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 341: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2455 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy334, yymsp[-4].minor.yy374, yymsp[-3].minor.yy0, yymsp[-2].minor.yy374, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 5421 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 342: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2458 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy334, yymsp[-6].minor.yy6, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 5427 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 343: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2462 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy334, yymsp[-8].minor.yy6, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-4].minor.yy113, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5434 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 344: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2466 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy334, yymsp[-6].minor.yy6, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 5440 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 345: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2470 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 5447 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2474 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 5454 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2478 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 5461 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 348: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2482 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 5468 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2486 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy6, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 5475 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2490 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 5482 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2494 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy334, yymsp[-7].minor.yy0, yymsp[-6].minor.yy357, yymsp[-5].minor.yy374, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, 
																																														yymsp[-2].minor.yy6, yymsp[-1].minor.yy374, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 5489 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2498 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy334, yymsp[-5].minor.yy0, yymsp[-4].minor.yy374, yymsp[-3].minor.yy374, yymsp[-2].minor.yy6, yymsp[-1].minor.yy374,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 5495 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2502 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy334, yymsp[-3].minor.yy0, yymsp[-2].minor.yy357, yymsp[-1].minor.yy374, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 5501 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD */
#line 2507 "bcplus/parser/detail/lemon_parser.y"
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
#line 5520 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* stmt_code_blk ::= ASP_GR */
#line 2541 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 5525 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* stmt_code_blk ::= ASP_CP */
#line 2542 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 5530 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* stmt_code_blk ::= F2LP_GR */
#line 2543 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 5535 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* stmt_code_blk ::= F2LP_CP */
#line 2544 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 5540 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* stmt_code_blk ::= LUA_GR */
#line 2545 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 5545 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* stmt_code_blk ::= LUA_CP */
#line 2546 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy334, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 5550 "bcplus/parser/detail/lemon_parser.c"
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
#line 5616 "bcplus/parser/detail/lemon_parser.c"
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
