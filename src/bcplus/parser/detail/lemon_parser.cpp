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
			#include "bcplus/parser/detail/NumberRangeEval.h"
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
			#include "bcplus/symbols/NumberRangeSymbol.h"

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

		
#line 329 "bcplus/parser/detail/lemon_parser.y"

	#define BASE_ELEM_DEF(elem, id, lparen, args, rparen, symtype, class, symclass)											\
		BASE_ELEM_DEF9(elem, id, lparen, args, rparen, symtype, class, symclass, false)

	#define BASE_ELEM_DEF9(elem, id, lparen, args, rparen, symtype, class, symclass, dynamic)								\
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
				good = false;																								\
			}																												\
		}																													\
																															\
		if (good) {																											\
			Symbol const* sym = parser->symtab()->resolve(symtype, *id_ptr->str(), arity);									\
																															\
			if (!sym && dynamic) {																							\
				/* Dynamic declarations are allowed. Attempt to create a new symbol */										\
				ref_ptr<ConstantSymbol::SortList> sorts = new ConstantSymbol::SortList();									\
				int argnum = 0;																								\
				if (args) {																									\
					BOOST_FOREACH(Term const* t, *args_ptr) {																\
							argnum++;																						\
						switch (t->subType()) {																				\
						case Term::Type::VARIABLE:																			\
							sorts->push_back(((Variable const*)t)->symbol()->sort());										\
							break;																							\
						case Term::Type::CONSTANT:																			\
							sorts->push_back(((Constant const*)t)->symbol()->sort());										\
							break;																							\
						case Term::Type::ANON_VAR:																			\
						case Term::Type::NULLARY:																			\
						case Term::Type::LUA:																				\
						case Term::Type::OBJECT:																			\
						case Term::Type::BINARY:																			\
						case Term::Type::UNARY:																				\
						case Term::Type::BINDING:																			\
							parser->_parse_error("Unable to dynamically declare abAction \"" + Symbol::genName(*id_ptr->str(), (args_ptr ? args_ptr->size() : 0))\
							+ "\". Could not deduce the sort of argument #" + boost::lexical_cast<std::string>(argnum)		\
							+ " as it isn't a constant or variable. This problem can be fixed by explicitly declaring the abAction" \
							" prior to this rule.", &id_ptr->beginLoc());													\
							good = false;																					\
							break;																							\
						}																									\
					}																										\
				}																											\
				if  (good) {																								\
					ref_ptr<ConstantSymbol> cs = new ConstantSymbol(ConstantSymbol::Type::ABACTION, id_ptr->str(), parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN), sorts);\
					/* add the sort to the symbol table */																	\
					if (!parser->symtab()->create(cs)) {																	\
						/* It seems there was a problem. */																	\
						parser->_parse_error("An error occurred while declaring \"" + Symbol::genName(*id_ptr->str(), (args_ptr ? args_ptr->size() : 0)) + "\".");\
						good = false;																						\
						break;																								\
					} else sym = cs;																						\
				}																											\
			}																												\
																															\
			if (!good) {																									\
				YYERROR;																									\
			} else if (!sym || sym->type() != symtype) {																	\
				/* The preprocessor indicated this was a constant and it's not... oops. */									\
				parser->_parse_error(std::string("INTERNAL ERROR: Could not locate symbol table entry for ")				\
					+ Symbol::Type::cstr(symtype) + " \"" + Symbol::genName(*id_ptr->str(), arity)		 					\
					+ "\".", &id_ptr->beginLoc());																			\
				YYERROR;																									\
			} else {																										\
				elem = new class((symclass*)sym, args, id_ptr->beginLoc(), (arity ? rparen_ptr->endLoc() : id_ptr->endLoc()));	\
			} 																												\
		}																													\
	
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

#line 518 "bcplus/parser/detail/lemon_parser.y"

	#define BASIC_TERM(term, id)																							\
		term = NULL;																										\
		ref_ptr<const Token> id_ptr = id;																					\
		ObjectSymbol const* sym = parser->symtab()->resolveOrCreate(new ObjectSymbol(id->str()));							\
		if (!sym) {																											\
			parser->_parse_error("An error occurred creating symbol \"" + *(id->str()) + "/0\".", &id->beginLoc());			\
			YYERROR;																										\
		} else term = new Object(sym, NULL, id->beginLoc(), id->endLoc());

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
			parser->_parse_error("Malformed arithmetic operation. Left operand is not numeric.", &op->beginLoc());			\
			YYERROR;																										\
		}																													\
		if (rhs->domainType() != DomainType::INTEGRAL && rhs->domainType() != DomainType::UNKNOWN) {						\
			good = false;																									\
			parser->_parse_error("Malformed arithmetic operation. Right operand is not numeric.", &rhs->beginLoc());		\
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

#line 787 "bcplus/parser/detail/lemon_parser.y"

	#define NUM_UOP(t_new, t, val) \
		ref_ptr<const Referenced> t_ptr = t; \
		t_new = new Number(val, t->beginLoc(), t->endLoc());


	#define NUM_BOP(t_new, l, r, val) \
		ref_ptr<const Referenced> l_ptr = l, r_ptr = r; \
		t_new = new Number(val, l->beginLoc(), r->endLoc());

#line 833 "bcplus/parser/detail/lemon_parser.y"

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

#line 913 "bcplus/parser/detail/lemon_parser.y"


	#define ATOMIC_FORMULA(af, constant, value) 														\
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
			ref_ptr<const ObjectSymbol> t =																\
				(value ? parser->symtab()->bobj(SymbolTable::BuiltinObject::TRUE) 						\
					: parser->symtab()->bobj(SymbolTable::BuiltinObject::FALSE));						\
			af = new AtomicFormula(constant,															\
					new Object(t, NULL, constant->beginLoc(), constant->endLoc()), 						\
					constant->beginLoc(), constant->endLoc());											\
		}
	


#line 1045 "bcplus/parser/detail/lemon_parser.y"

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



#line 1114 "bcplus/parser/detail/lemon_parser.y"

	#define BINDING(new_f, lhs, op, rhs, class)																	\
		new_f = NULL;																							\
		ref_ptr<const Element> lhs_ptr = lhs, rhs_ptr;															\
		ref_ptr<const Token> op_ptr = op;																		\
																												\
		if (!parser->lang()->support(Language::Feature::QUERY_BIND_STEP)) {										\
			parser->_feature_error(Language::Feature::QUERY_BIND_STEP, &op->beginLoc());						\
			YYERROR;																							\
		} else {																								\
			new_f = new class(lhs, rhs, lhs->beginLoc(), rhs->endLoc());										\
		}

#line 1516 "bcplus/parser/detail/lemon_parser.y"

		
	#define DYNAMIC_SORT_PLUS(new_s, s, op, feature, o)																												\
		new_s = NULL;																																				\
		ref_ptr<const Referenced> s_ptr = s, op_ptr = op;																											\
																																									\
																																									\
		if (!parser->lang()->support(feature)) {																													\
			parser->_feature_error(feature, &op->beginLoc());																										\
			YYERROR;																																				\
		} else {																																					\
			new_s = parser->symtab()->plus(s, o);																													\
		}
#line 1644 "bcplus/parser/detail/lemon_parser.y"

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
#line 2391 "bcplus/parser/detail/lemon_parser.y"

	#define NC_STATEMENT(stmt, kw, period, feature, class)											\
		stmt = NULL;																				\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = period;											\
																									\
		if (!parser->lang()->support(feature)) {													\
			parser->_feature_error(feature, &kw->beginLoc());										\
			YYERROR;																				\
		} else {																					\
			stmt = new class(kw->beginLoc(), period->endLoc());										\
		}																							

#line 2414 "bcplus/parser/detail/lemon_parser.y"

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
#line 2443 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRangeEval* maxstep;
		Token const* label;
	};

#line 2550 "bcplus/parser/detail/lemon_parser.y"

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

#line 2622 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2708 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2901 "bcplus/parser/detail/lemon_parser.y"

	#define CODE_BLK(stmt, code, feature, type) 												\
		ref_ptr<const Token> code_ptr = code;													\
		if (!parser->lang()->support(feature)) {												\
			stmt = NULL;																		\
			parser->_feature_error(feature, &code->beginLoc());									\
			YYERROR;																			\
		} else {																				\
			stmt = new type(code, code->beginLoc(), code->endLoc());							\
		}
#line 542 "bcplus/parser/detail/lemon_parser.c"
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
#define YYNOCODE 255
#define YYACTIONTYPE unsigned short int
#define lemon_parserTOKENTYPE  Token const* 								
typedef union {
  int yyinit;
  lemon_parserTOKENTYPE yy0;
  Formula* yy17;
  NumberRangeEval* yy29;
  TokenList* yy52;
  SortSymbol const* yy60;
  SortDeclaration* yy61;
  Token const* yy79;
  SortSymbol* yy93;
  ObjectDeclaration::ElementList* yy102;
  LuaTerm* yy117;
  ShowStatement::ElementList* yy123;
  CardinalityFormula* yy133;
  MacroDeclaration* yy151;
  MacroSymbol::ArgumentList* yy154;
  QuantifierFormula* yy177;
  Variable* yy189;
  ConstantSymbol::SortList* yy195;
  SortDeclaration::ElementList* yy196;
  UNUSED yy217;
  QuantifierFormula::QuantifierList* yy221;
  Constant* yy225;
  IdentifierDeclList* yy226;
  TermList* yy259;
  ConstantSymbol::Type::type yy265;
  MacroDeclaration::ElementList* yy269;
  ObjectDeclaration::Element* yy294;
  Term* yy299;
  NumberRange* yy345;
  MacroSymbol* yy359;
  CardinalityFormula::VariableList* yy375;
  Number* yy396;
  ConstantDeclaration::ElementList* yy409;
  VariableDeclaration::ElementList* yy410;
  Object* yy422;
  AtomicFormula* yy423;
  ObjectDeclaration::Element::ObjectList* yy425;
  ConstantDeclaration* yy431;
  StrongNCStatement* yy446;
  VariableDeclaration* yy459;
  QueryStatement* yy466;
  ObjectDeclaration* yy468;
  Statement* yy484;
  NCStatement* yy485;
  QueryData yy489;
  QuantifierFormula::Operator::type yy497;
  int yy509;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define lemon_parserARG_SDECL  BCParser* parser						;
#define lemon_parserARG_PDECL , BCParser* parser						
#define lemon_parserARG_FETCH  BCParser* parser						 = yypParser->parser						
#define lemon_parserARG_STORE yypParser->parser						 = parser						
#define YYNSTATE 855
#define YYNRULE 452
#define YYERRORSYMBOL 135
#define YYERRSYMDT yy509
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
#define YY_ACTTAB_COUNT (3381)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   854,   62,  855,  853,  852,  851,  850,  849,  848,  847,
 /*    10 */   846,  845,  844,  843,  842,  841,  840,  839,   61,  812,
 /*    20 */   318,  838,  831,  837,  836,  830,  148,  146,  144,  143,
 /*    30 */   142,  833,  314,  319,  812,  318,  838,  831,  474,  836,
 /*    40 */   830,  771,  416,  624,  115,  113,  112,  313,  319,   24,
 /*    50 */    23,   62,  635,   25,  332,  764,  761,  760,  759,  758,
 /*    60 */   686,  120,  357,  244,  770,  769,   60,   17,  475,  812,
 /*    70 */   318,  838,  831,  837,  836,  830,  148,  146,  144,  143,
 /*    80 */   142,   59,  313,  319,  238,  236,  234,  233,  232,  362,
 /*    90 */   764,  761,  760,  759,  758,  465,  511,  467,  687,  688,
 /*   100 */   552,  582,  581,  580,  579,  578,  577,  576,  575,  574,
 /*   110 */   573,  572,  571,  570,  569,  568,  567,  566,  565,  811,
 /*   120 */   541,  652,  684,  542,  810,  551,  550,  547,  546,  549,
 /*   130 */   548,  832,  172,  170,  168,  166,  165,  631,  531,  838,
 /*   140 */   831,  837,  836,  830,   45,   58,   12,  294,  200,  201,
 /*   150 */    43,   29,   10,   11,  297,  191,  242,  265,   44,   74,
 /*   160 */   600,  120,  259,  168,  166,  165,  804,  803,  805,    9,
 /*   170 */   494,  493,    8,  309,   42,  251,  322,  604,  601,  599,
 /*   180 */   598,  767,  768,  706,  685,  124,  320,  184,  717,  123,
 /*   190 */   214,  213,  212,  306,  826,  834,  835,  838,  831,  837,
 /*   200 */   836,  830,  480,  415,  682,  681,  680,  679,  289,  630,
 /*   210 */   541,  508,  509,  542,  629,  119,  117,  115,  113,  112,
 /*   220 */   678,  164,  676,  121,   73,   51,   50,  675,  106,   52,
 /*   230 */   701,  700,  702,  630,  541,  708,  545,  542,  629,  674,
 /*   240 */   672,  673,  677,  753,  752,  703,  704,  531,  544,  829,
 /*   250 */   543,   19,  190,  219,  495,  209,  608,  607,  496,  826,
 /*   260 */   834,  835,  838,  831,  837,  836,  830,  479,  415,  766,
 /*   270 */   650,  596,  597,  644,  198,  531,  592,    6,  339,   41,
 /*   280 */   608,  607,  609,  306,  811,  541,  530,  217,  542,  810,
 /*   290 */   192,  270,  215,  192,   72,  596,  597,  746,  198,  193,
 /*   300 */   745,    6,  193,   41,  469,  668,  468,  306,   21,   20,
 /*   310 */    24,   23,   39,   40,   25,   29,  642,   71,  130,   70,
 /*   320 */   500,  812,  318,  838,  831,  837,  836,  830,  234,  233,
 /*   330 */   232,  804,  803,  805,  314,  319,   39,   40,  544,  829,
 /*   340 */   543,   25,  130,  771,  766,   69,  756,  757,  662,  205,
 /*   350 */   357,  776,    3,   14,   28,  307,  811,  541,  306,  828,
 /*   360 */   542,  810,  544,  829,  543,  772,  770,  769,  308,   22,
 /*   370 */    21,   20,   24,   23,  448,   46,   25,  812,  318,  838,
 /*   380 */   831,  474,  836,  830,  753,  752,  239,   26,   27,  120,
 /*   390 */   313,  319,   32,  106,  666,  459,  661,  334,  764,  761,
 /*   400 */   760,  759,  758,  804,  803,  805, 1150,  144,  143,  142,
 /*   410 */    18,  475, 1075,  544,  829,  543, 1075,  531,  767,  768,
 /*   420 */   237, 1150, 1150,  540,  184,  235,  123,   37,   36,  308,
 /*   430 */   306,   38,  631,  531,  838,  831,  837,  836,  830,  766,
 /*   440 */   827, 1150, 1150, 1213,  239,  196,  306,  210,  179,    2,
 /*   450 */   187,  194,  195, 1150,  686,  600,  357,  667,  793, 1150,
 /*   460 */   121,  481,  743,  541,  499,  106,  542,  742,  310, 1213,
 /*   470 */   120,  338,  604,  601,  599,  598,  271,  183,  237,  449,
 /*   480 */   450, 1150,  539,  235,  540,  544,  829,  543,  173,  463,
 /*   490 */   511,  467,  687,  688,  631,  531,  838,  831,  474,  836,
 /*   500 */   830,  544,    7,  538,  694,  447,  586,  585,  693,  736,
 /*   510 */   735,  737,  308,   13,  137,  136,  135,  600,  134,  133,
 /*   520 */   132,  131,  200,  201,  726,  727,   57,   30,  475,  662,
 /*   530 */   310,  357,   55,  324,  604,  601,  599,  598,  150,  141,
 /*   540 */   140,  139,  138,  748,  203,  631,  531,  838,  831,  474,
 /*   550 */   836,  830,  826,  834,  835,  838,  831,  837,  836,  830,
 /*   560 */   478,  415,   82,  671,  186,   53,   54,  540,  600,  517,
 /*   570 */   686,  151,  357,  120,  460,  660,  459,  661,   31,  475,
 /*   580 */   662,  310,  357,   65,  326,  604,  601,  599,  598,  741,
 /*   590 */   519,  544,  829,  543,  457,  655,  456,  242,  158,  157,
 /*   600 */   156,   52,  155,  154,  153,  152,  466,  467,  687,  688,
 /*   610 */   631,  531,  838,  831,  837,  836,  830,  683,    7,  288,
 /*   620 */   247,  628,  163,  162,  161,  160,  159,  539,  664,   13,
 /*   630 */   137,  136,  135,  600,  134,  133,  132,  131,  723,  791,
 /*   640 */   541,  722,  670,  542,  790,  189,  310,  181,  536,  337,
 /*   650 */   604,  601,  599,  598,  150,  141,  140,  139,  138,  721,
 /*   660 */   812,  318,  838,  831,  837,  836,  830,  119,  117,  115,
 /*   670 */   113,  112,  181,  314,  319,  631,  531,  838,  831,  837,
 /*   680 */   836,  830,  771,  766,  590,  589,  785,  784,  786,  626,
 /*   690 */   541,  539,  539,  542,  625,  686,   64,  357,  600,  669,
 /*   700 */    63,  787,  788,  720,  249,  770,  769,  689,  690,  171,
 /*   710 */   651,  606,  648,  648,  594,  604,  601,  599,  598,  826,
 /*   720 */   834,  835,  838,  831,  837,  836,  830,  477,  415,  181,
 /*   730 */   462,  511,  467,  687,  688,   29,  620,  619,  621,  518,
 /*   740 */   825,  541,  231,  169,  542,  824,  532,  240,  167,  308,
 /*   750 */   719,  622,  623,  426,  645,  649,   95,   94,   93,  149,
 /*   760 */    92,   91,   90,   89,  455,  451,  502,  663,  544,  829,
 /*   770 */   543,  627,  618,  838,  831,  837,  836,  830,  539,  199,
 /*   780 */   100,   99,   98,   97,   96,  229,  718,  819,  818,  820,
 /*   790 */   230,  588,  587,  147,  634,   46,  178,  181,  145,  648,
 /*   800 */   539,  182,  821,  822,  540,  829,  420,  712,  713,  715,
 /*   810 */   118,  812,  318,  838,  831,  837,  836,  830,  544,  829,
 /*   820 */   543,  648,  503,  659,  311,  319,  238,  236,  234,  233,
 /*   830 */   232,  330,  764,  761,  760,  759,  758,  532,  692,    4,
 /*   840 */   425,  645,  809,  208,  116,  710,  654, 1251,  591,  114,
 /*   850 */   513,   81,   80,   79,  242,   78,   77,   76,   75,  512,
 /*   860 */   811,  541,  177,  647,  542,  810,  202, 1251,    5,  544,
 /*   870 */   829,  543,  472,  709,  544,  111,  110,  109,  108,  107,
 /*   880 */   826,  834,  835,  838,  831,  837,  836,  830,  473,  415,
 /*   890 */   180,  812,  317,  838,  831,  837,  836,  830,  176,   56,
 /*   900 */   507,  211,  707,  468,  765,  319,  657,  804,  803,  805,
 /*   910 */   204,  750,  764,  761,  760,  759,  758,  218,  216,  214,
 /*   920 */   213,  212,  175,  812,  318,  838,  831,  837,  836,  830,
 /*   930 */   123,   22,   21,   20,   24,   23,  313,  319,   25,  658,
 /*   940 */   456,  243,  304,  763,  764,  761,  760,  759,  758,  773,
 /*   950 */   174,  753,  752,  812,  318,  838,  831,  837,  836,  830,
 /*   960 */   747,  470,  711,  653,  122,  544,  313,  319,  501,  106,
 /*   970 */   454,  502,  646,  762,  764,  761,  760,  759,  758,  812,
 /*   980 */   318,  838,  831,  837,  836,  830,  823,  453,  502,  544,
 /*   990 */   829,  543,  313,  319,  643,  185,  241,  304,   38,  534,
 /*  1000 */   764,  761,  760,  759,  758,  197,  809,   81,   80,   79,
 /*  1010 */   584,   78,   77,   76,   75,  452,  502, 1308,    1,  119,
 /*  1020 */   117,  115,  113,  112,  812,  318,  838,  831,  837,  836,
 /*  1030 */   830,  111,  110,  109,  108,  107,  583,  313,  319,  111,
 /*  1040 */   110,  109,  108,  107,  533,  764,  761,  760,  759,  758,
 /*  1050 */   812,  318,  838,  831,  837,  836,  830,  446,  686,  755,
 /*  1060 */   357,  427,  502,  313,  319,  100,   99,   98,   97,   96,
 /*  1070 */   383,  764,  761,  760,  759,  758,  564,  563,  812,  318,
 /*  1080 */   838,  831,  837,  836,  830,  562,   22,   21,   20,   24,
 /*  1090 */    23,  313,  319,   25,  510,  467,  687,  688,  435,  764,
 /*  1100 */   761,  760,  759,  758,  561,  560,  559,  557,  812,  318,
 /*  1110 */   838,  831,  837,  836,  830,  556,  789,  125,  555,  554,
 /*  1120 */   553,  313,  319,  540,  775,  228,  829,  777,  434,  764,
 /*  1130 */   761,  760,  759,  758,  812,  318,  838,  831,  837,  836,
 /*  1140 */   830,  238,  236,  234,  233,  232,  773,  313,  319,  172,
 /*  1150 */   170,  168,  166,  165,  335,  764,  761,  760,  759,  758,
 /*  1160 */    82,   88,   87,  754,   86,   85,   84,   83,  544,  105,
 /*  1170 */   104,  103,  102,  101,  751,  302,  812,  318,  838,  831,
 /*  1180 */   837,  836,  830,   16,  105,  104,  103,  102,  101,  313,
 /*  1190 */   319,  301,   15,  188,  524,   56,  333,  764,  761,  760,
 /*  1200 */   759,  758,  812,  318,  838,  831,  837,  836,  830,  300,
 /*  1210 */   705,  527,  298,  522,  525,  313,  319,  172,  170,  168,
 /*  1220 */   166,  165,  331,  764,  761,  760,  759,  758,  812,  318,
 /*  1230 */   838,  831,  837,  836,  830,  295,  471,  523,   68,  665,
 /*  1240 */   292,  313,  319,  218,  216,  214,  213,  212,  361,  764,
 /*  1250 */   761,  760,  759,  758,  812,  318,  838,  831,  837,  836,
 /*  1260 */   830,  521,   22,   21,   20,   24,   23,  313,  319,   25,
 /*  1270 */   520,  290,  716,  685,  360,  764,  761,  760,  759,  758,
 /*  1280 */   558,  269,  286,  812,  318,  838,  831,  837,  836,  830,
 /*  1290 */   284,  285,  492,   67,  282,  281,  313,  319,   29,  280,
 /*  1300 */   268,  491,  278,  225,  764,  761,  760,  759,  758,  812,
 /*  1310 */   318,  838,  831,  837,  836,  830,  490,   22,   21,   20,
 /*  1320 */    24,   23,  315,  319,   25,  276,  264,  489,  274,  351,
 /*  1330 */   764,  761,  760,  759,  758,  812,  318,  838,  831,  837,
 /*  1340 */   836,  830,  488,  272,  266,   66,  487,  267,  313,  319,
 /*  1350 */   218,  216,  214,  213,  212,  224,  764,  761,  760,  759,
 /*  1360 */   758,  812,  318,  838,  831,  837,  836,  830,  263,   22,
 /*  1370 */    21,   20,   24,   23,  313,  319,   25,  486,  262,  261,
 /*  1380 */   260,  223,  764,  761,  760,  759,  758,  812,  318,  838,
 /*  1390 */   831,  837,  836,  830,  485,  641,  258,  256,  257,  254,
 /*  1400 */   313,  319,  238,  236,  234,  233,  232,  222,  764,  761,
 /*  1410 */   760,  759,  758,  812,  318,  838,  831,  837,  836,  830,
 /*  1420 */   255,  484,  483,  252,  250,  482,  313,  319,  238,  236,
 /*  1430 */   234,  233,  232,  221,  764,  761,  760,  759,  758,  812,
 /*  1440 */   318,  838,  831,  837,  836,  830,  299,  291,   34,   33,
 /*  1450 */    37,   36,  313,  319,   38,  595,  714,  287,  283,  220,
 /*  1460 */   764,  761,  760,  759,  758,  812,  318,  838,  831,  837,
 /*  1470 */   836,  830,  774,  303,  305,  691,  355,  539,  314,  319,
 /*  1480 */   356,  423,   35,   34,   33,   37,   36,  771,  766,   38,
 /*  1490 */   812,  318,  838,  831,  837,  836,  830,  640,  537,  424,
 /*  1500 */   639,  207,  475,  314,  319,  638,  637,  636,  354,  248,
 /*  1510 */   770,  769,  771,  766,  353,  812,  318,  838,  831,  837,
 /*  1520 */   836,  830,  352,   35,   34,   33,   37,   36,  314,  319,
 /*  1530 */    38,  366,  296,  293,  246,  770,  769,  771,  766,  631,
 /*  1540 */   531,  838,  831,  837,  836,  830,  253,  631,  531,  838,
 /*  1550 */   831,  837,  836,  830,  526,  358,  428,  279,  699,  245,
 /*  1560 */   770,  769,  600,  812,  412,  838,  831,  837,  836,  830,
 /*  1570 */   600,  277,  275,  429,  273,  310,  364,  413,  603,  604,
 /*  1580 */   601,  599,  598,  310,  698,  697,  602,  604,  601,  599,
 /*  1590 */   598,  631,  531,  838,  831,  837,  836,  830,  696,  631,
 /*  1600 */   531,  838,  831,  837,  836,  830,  695,  812,  800,  838,
 /*  1610 */   831,  837,  836,  830,  600,  369,  506,  656, 1309, 1309,
 /*  1620 */   802,  413,  600,   48,   47,   51,   50,  310, 1309,   52,
 /*  1630 */   498,  604,  601,  599,  598,  310, 1309, 1309,  497,  604,
 /*  1640 */   601,  599,  598,  631,  531,  838,  831,  837,  836,  830,
 /*  1650 */  1309,  631,  531,  838,  831,  837,  836,  830,  812,  321,
 /*  1660 */   838,  831,  837,  836,  830, 1309,  600, 1309, 1309, 1309,
 /*  1670 */  1309,  802,  413, 1309,  600,  686, 1309,  357, 1309,  310,
 /*  1680 */  1309, 1309,  367,  604,  601,  599,  598,  310, 1309, 1309,
 /*  1690 */   418,  604,  601,  599,  598,  631,  531,  838,  831,  837,
 /*  1700 */   836,  830, 1309,  631,  531,  838,  831,  837,  836,  830,
 /*  1710 */   458,  511,  467,  687,  688, 1309, 1309, 1309,  600,  792,
 /*  1720 */   783,  838,  831,  837,  836,  830,  600, 1309, 1309,  476,
 /*  1730 */   389,  310,  633, 1309,  417,  604,  601,  599,  598,  310,
 /*  1740 */  1309, 1309,  327,  604,  601,  599,  598,  631,  531,  838,
 /*  1750 */   831,  837,  836,  830, 1309,  631,  531,  838,  831,  837,
 /*  1760 */   836,  830, 1309,    4,  238,  236,  234,  233,  232, 1309,
 /*  1770 */   600, 1309, 1309, 1309, 1309,   81,   80,   79,  600,   78,
 /*  1780 */    77,   76,   75,  310, 1309, 1309,  325,  604,  601,  599,
 /*  1790 */   598,  310, 1309, 1309,  323,  604,  601,  599,  598,  111,
 /*  1800 */   110,  109,  108,  107, 1309, 1309, 1309, 1309,  922,  922,
 /*  1810 */   922, 1309,  922,  922,  922,  922,  158,  157,  156, 1309,
 /*  1820 */   155,  154,  153,  152,   81,   80,   79, 1309,   78,   77,
 /*  1830 */    76,   75,  922,  922,  922,  922,  922, 1309, 1309, 1309,
 /*  1840 */   163,  162,  161,  160,  159, 1309,  129, 1309,  111,  110,
 /*  1850 */   109,  108,  107,  744,  734,  838,  831,  837,  836,  830,
 /*  1860 */   744,  734,  838,  831,  837,  836,  830, 1309,  316, 1309,
 /*  1870 */    35,   34,   33,   37,   36,  312, 1309,   38, 1309, 1309,
 /*  1880 */   359,  731,  728, 1309, 1309, 1309, 1309,  329,  731,  728,
 /*  1890 */   744,  734,  838,  831,  837,  836,  830, 1309,  744,  734,
 /*  1900 */   838,  831,  837,  836,  830,  733,  744,  734,  838,  831,
 /*  1910 */   837,  836,  830,  316, 1309, 1309, 1309,  725,  731,  728,
 /*  1920 */  1309,  316, 1309, 1309, 1309,  730,  731,  728, 1309, 1309,
 /*  1930 */   749, 1309, 1309,  729,  731,  728,  744,  734,  838,  831,
 /*  1940 */   837,  836,  830,  744,  734,  838,  831,  837,  836,  830,
 /*  1950 */  1309,  316,   22,   21,   20,   24,   23, 1309,  316,   25,
 /*  1960 */  1309, 1309, 1309,  529,  731,  728, 1309,  724, 1309, 1309,
 /*  1970 */   528,  731,  728,  744,  734,  838,  831,  837,  836,  830,
 /*  1980 */   744,  734,  838,  831,  837,  836,  830, 1309,  316,   49,
 /*  1990 */    48,   47,   51,   50, 1309,  316,   52,  461,  505, 1309,
 /*  2000 */   370,  731,  728,  706,  685, 1309,  706,  431,  731,  728,
 /*  2010 */   744,  734,  838,  831,  837,  836,  830,  826,  834,  835,
 /*  2020 */   838,  831,  837,  836,  830,  316,  414, 1309, 1309, 1309,
 /*  2030 */   792,  783,  838,  831,  837,  836,  830,  430,  731,  728,
 /*  2040 */  1309,  390, 1309, 1309,  701,  700,  702, 1309, 1309, 1309,
 /*  2050 */   701,  700,  702,  701,  700,  702, 1309, 1309, 1309,  703,
 /*  2060 */   704, 1309, 1309, 1309, 1309,  703,  704,  219,  703,  704,
 /*  2070 */  1309, 1309, 1309,  219, 1309, 1309,  219, 1309, 1309,  826,
 /*  2080 */   834,  835,  838,  831,  837,  836,  830, 1309,  444,  826,
 /*  2090 */   834,  835,  838,  831,  837,  836,  830, 1309,  365, 1309,
 /*  2100 */  1309,  217, 1309, 1309, 1309, 1309,  215,  217, 1309, 1309,
 /*  2110 */   217, 1309,  215, 1309, 1309,  215, 1309,  826,  834,  835,
 /*  2120 */   838,  831,  837,  836,  830, 1309,  445,  826,  834,  835,
 /*  2130 */   838,  831,  837,  836,  830, 1309,  817,  826,  834,  835,
 /*  2140 */   838,  831,  837,  836,  830,  686,  813,  357, 1309, 1309,
 /*  2150 */  1309,  826,  834,  835,  838,  831,  837,  836,  830, 1309,
 /*  2160 */   816,  826,  834,  835,  838,  831,  837,  836,  830, 1309,
 /*  2170 */   815,  826,  834,  835,  838,  831,  837,  836,  830, 1309,
 /*  2180 */   814,  516,  467,  687,  688, 1309, 1309, 1309,  515,  514,
 /*  2190 */  1309,  826,  834,  835,  838,  831,  837,  836,  830, 1309,
 /*  2200 */   443, 1309, 1309, 1309,  826,  834,  835,  838,  831,  837,
 /*  2210 */   836,  830, 1309,  442,  826,  834,  835,  838,  831,  837,
 /*  2220 */   836,  830, 1309,  808, 1309, 1309, 1309,  826,  834,  835,
 /*  2230 */   838,  831,  837,  836,  830, 1309,  807,  826,  834,  835,
 /*  2240 */   838,  831,  837,  836,  830, 1309,  806,  826,  834,  835,
 /*  2250 */   838,  831,  837,  836,  830, 1309,  801, 1309, 1309, 1309,
 /*  2260 */   826,  834,  835,  838,  831,  837,  836,  830, 1309,  441,
 /*  2270 */  1309,  826,  834,  835,  838,  831,  837,  836,  830, 1309,
 /*  2280 */   440,  826,  834,  835,  838,  831,  837,  836,  830, 1309,
 /*  2290 */   799,  826,  834,  835,  838,  831,  837,  836,  830, 1309,
 /*  2300 */   798, 1309, 1309, 1309, 1309,  826,  834,  835,  838,  831,
 /*  2310 */   837,  836,  830,  128,  797,  826,  834,  835,  838,  831,
 /*  2320 */   837,  836,  830, 1309,  439,  826,  834,  835,  838,  831,
 /*  2330 */   837,  836,  830, 1309,  438, 1309, 1309,   35,   34,   33,
 /*  2340 */    37,   36, 1309, 1309,   38,  826,  834,  835,  838,  831,
 /*  2350 */   837,  836,  830, 1309,  796, 1309, 1309, 1309,  826,  834,
 /*  2360 */   835,  838,  831,  837,  836,  830, 1309,  795,  826,  834,
 /*  2370 */   835,  838,  831,  837,  836,  830, 1309,  794, 1309, 1309,
 /*  2380 */  1309,  826,  834,  835,  838,  831,  837,  836,  830, 1309,
 /*  2390 */   411,  826,  834,  835,  838,  831,  837,  836,  830, 1309,
 /*  2400 */   410,  826,  834,  835,  838,  831,  837,  836,  830, 1309,
 /*  2410 */   409, 1309, 1309, 1309,  826,  834,  835,  838,  831,  837,
 /*  2420 */   836,  830, 1309,  408, 1309,  826,  834,  835,  838,  831,
 /*  2430 */   837,  836,  830, 1309,  407,  826,  834,  835,  838,  831,
 /*  2440 */   837,  836,  830, 1309,  406,  826,  834,  835,  838,  831,
 /*  2450 */   837,  836,  830, 1309,  405, 1309, 1309, 1309, 1309,  826,
 /*  2460 */   834,  835,  838,  831,  837,  836,  830,  127,  404,  826,
 /*  2470 */   834,  835,  838,  831,  837,  836,  830, 1309,  403,  826,
 /*  2480 */   834,  835,  838,  831,  837,  836,  830, 1309,  402, 1309,
 /*  2490 */  1309,   35,   34,   33,   37,   36, 1309, 1309,   38,  826,
 /*  2500 */   834,  835,  838,  831,  837,  836,  830, 1309,  401, 1309,
 /*  2510 */  1309, 1309,  826,  834,  835,  838,  831,  837,  836,  830,
 /*  2520 */  1309,  400,  826,  834,  835,  838,  831,  837,  836,  830,
 /*  2530 */  1309,  399, 1309, 1309, 1309,  826,  834,  835,  838,  831,
 /*  2540 */   837,  836,  830, 1309,  398,  826,  834,  835,  838,  831,
 /*  2550 */   837,  836,  830, 1309,  397,  826,  834,  835,  838,  831,
 /*  2560 */   837,  836,  830, 1309,  396, 1309, 1309, 1309,  826,  834,
 /*  2570 */   835,  838,  831,  837,  836,  830, 1309,  395, 1309,  826,
 /*  2580 */   834,  835,  838,  831,  837,  836,  830, 1309,  394,  826,
 /*  2590 */   834,  835,  838,  831,  837,  836,  830, 1309,  393,  826,
 /*  2600 */   834,  835,  838,  831,  837,  836,  830, 1309,  392, 1309,
 /*  2610 */  1309, 1309, 1309,  826,  834,  835,  838,  831,  837,  836,
 /*  2620 */   830,  126,  391,  826,  834,  835,  838,  831,  837,  836,
 /*  2630 */   830, 1309,  387,  826,  834,  835,  838,  831,  837,  836,
 /*  2640 */   830, 1309,  386, 1309, 1309,   35,   34,   33,   37,   36,
 /*  2650 */  1309, 1309,   38,  826,  834,  835,  838,  831,  837,  836,
 /*  2660 */   830, 1309,  385, 1309, 1309, 1309,  826,  834,  835,  838,
 /*  2670 */   831,  837,  836,  830, 1309,  384,  826,  834,  835,  838,
 /*  2680 */   831,  837,  836,  830, 1309,  382, 1309, 1309, 1309,  826,
 /*  2690 */   834,  835,  838,  831,  837,  836,  830, 1309,  381,  826,
 /*  2700 */   834,  835,  838,  831,  837,  836,  830, 1309,  380,  826,
 /*  2710 */   834,  835,  838,  831,  837,  836,  830, 1309,  379, 1309,
 /*  2720 */  1309, 1309,  826,  834,  835,  838,  831,  837,  836,  830,
 /*  2730 */  1309,  378,   15,  826,  834,  835,  838,  831,  837,  836,
 /*  2740 */   830, 1309,  227,  826,  834,  835,  838,  831,  837,  836,
 /*  2750 */   830, 1309,  226,  826,  834,  835,  838,  831,  837,  836,
 /*  2760 */   830, 1309,  368,  792,  783,  838,  831,  837,  836,  830,
 /*  2770 */  1309, 1309, 1309, 1309,  436,  792,  783,  838,  831,  837,
 /*  2780 */   836,  830, 1309, 1309, 1309, 1309,  363,  792,  783,  838,
 /*  2790 */   831,  837,  836,  830, 1309, 1309, 1309, 1309,  437, 1309,
 /*  2800 */    22,   21,   20,   24,   23, 1309, 1309,   25,  792,  783,
 /*  2810 */   838,  831,  837,  836,  830, 1309,  686, 1309,  357,  782,
 /*  2820 */  1309,  792,  783,  838,  831,  837,  836,  830,  686, 1309,
 /*  2830 */   357, 1309,  778,  792,  783,  838,  831,  837,  836,  830,
 /*  2840 */  1309, 1309, 1309, 1309,  781,  792,  783,  838,  831,  837,
 /*  2850 */   836,  830,  464,  467,  687,  688,  780,  792,  783,  838,
 /*  2860 */   831,  837,  836,  830,  504,  467,  687,  688,  779,  792,
 /*  2870 */   783,  838,  831,  837,  836,  830, 1309, 1309, 1309, 1309,
 /*  2880 */   388,  792,  783,  838,  831,  837,  836,  830, 1309, 1309,
 /*  2890 */  1309, 1309,  433,  792,  783,  838,  831,  837,  836,  830,
 /*  2900 */  1309, 1309, 1309, 1309,  432,  792,  783,  838,  831,  837,
 /*  2910 */   836,  830, 1309, 1309, 1309, 1309,  740,  792,  783,  838,
 /*  2920 */   831,  837,  836,  830, 1309, 1309, 1309, 1309,  739,  792,
 /*  2930 */   783,  838,  831,  837,  836,  830, 1309, 1309, 1309, 1309,
 /*  2940 */   738,  792,  783,  838,  831,  837,  836,  830, 1309, 1309,
 /*  2950 */  1309, 1309,  377,  792,  783,  838,  831,  837,  836,  830,
 /*  2960 */  1309, 1309, 1309, 1309,  376,  792,  783,  838,  831,  837,
 /*  2970 */   836,  830, 1309, 1309, 1309, 1309,  375,  792,  783,  838,
 /*  2980 */   831,  837,  836,  830, 1309, 1309, 1309, 1309,  374,  792,
 /*  2990 */   783,  838,  831,  837,  836,  830, 1309, 1309, 1309, 1309,
 /*  3000 */   373,  792,  783,  838,  831,  837,  836,  830, 1309, 1309,
 /*  3010 */  1309, 1309,  372,  792,  783,  838,  831,  837,  836,  830,
 /*  3020 */  1309, 1309, 1309, 1309,  371,  792,  783,  838,  831,  837,
 /*  3030 */   836,  830, 1309, 1309, 1309, 1309,  732,  627,  618,  838,
 /*  3040 */   831,  837,  836,  830, 1309, 1309, 1309,  627,  618,  838,
 /*  3050 */   831,  837,  836,  830,  627,  618,  838,  831,  837,  836,
 /*  3060 */   830,  627,  618,  838,  831,  837,  836,  830, 1309, 1309,
 /*  3070 */  1309, 1309,  328,  627,  618,  838,  831,  837,  836,  830,
 /*  3080 */  1309, 1309,  421,   22,   21,   20,   24,   23, 1309,  617,
 /*  3090 */    25, 1309, 1309, 1309, 1309, 1309,  422,  627,  618,  838,
 /*  3100 */   831,  837,  836,  830, 1309, 1309, 1309, 1309,  616,  627,
 /*  3110 */   618,  838,  831,  837,  836,  830, 1309, 1309, 1309,  627,
 /*  3120 */   618,  838,  831,  837,  836,  830, 1309, 1309, 1309, 1309,
 /*  3130 */   632, 1309,  615,  627,  618,  838,  831,  837,  836,  830,
 /*  3140 */  1309, 1309, 1309, 1309,  614,  627,  618,  838,  831,  837,
 /*  3150 */   836,  830, 1309, 1309,  613,  627,  618,  838,  831,  837,
 /*  3160 */   836,  830,  238,  236,  234,  233,  232, 1309,  419,  627,
 /*  3170 */   618,  838,  831,  837,  836,  830, 1309, 1309, 1309, 1309,
 /*  3180 */   612,  627,  618,  838,  831,  837,  836,  830, 1309, 1309,
 /*  3190 */   611,  627,  618,  838,  831,  837,  836,  830, 1309, 1309,
 /*  3200 */  1309, 1309, 1309, 1309,  610,  627,  618,  838,  831,  837,
 /*  3210 */   836,  830,  593, 1309, 1309, 1309,  350,  627,  618,  838,
 /*  3220 */   831,  837,  836,  830, 1309, 1309,  349,  627,  618,  838,
 /*  3230 */   831,  837,  836,  830,   35,   34,   33,   37,   36, 1309,
 /*  3240 */   348,   38, 1309, 1309,  627,  618,  838,  831,  837,  836,
 /*  3250 */   830, 1309,  347,  627,  618,  838,  831,  837,  836,  830,
 /*  3260 */  1309, 1309,  346,  627,  618,  838,  831,  837,  836,  830,
 /*  3270 */   627,  618,  838,  831,  837,  836,  830,   29, 1309,  345,
 /*  3280 */   627,  618,  838,  831,  837,  836,  830, 1309,  344,  627,
 /*  3290 */   618,  838,  831,  837,  836,  830, 1309, 1309,  605, 1309,
 /*  3300 */  1309, 1309, 1309, 1309, 1309,  343,  627,  618,  838,  831,
 /*  3310 */   837,  836,  830, 1309, 1309,  342,  792,  783,  838,  831,
 /*  3320 */   837,  836,  830, 1309,  341, 1309, 1309,  336, 1309, 1309,
 /*  3330 */    22,   21,   20,   24,   23, 1309, 1309,   25, 1309, 1309,
 /*  3340 */  1309,  340,   49,   48,   47,   51,   50, 1309, 1309,   52,
 /*  3350 */   774, 1309, 1309, 1309, 1309,  539, 1309, 1309, 1309, 1309,
 /*  3360 */  1309, 1309, 1309, 1309, 1309, 1309, 1309, 1309, 1309, 1309,
 /*  3370 */  1309, 1309, 1309, 1309, 1309, 1309,  535, 1309, 1309,  206,
 /*  3380 */   475,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   135,   80,    0,  138,  139,  140,  141,  142,  143,  144,
 /*    10 */   145,  146,  147,  148,  149,  150,  151,  152,   71,  154,
 /*    20 */   155,  156,  157,  158,  159,  160,  105,  106,  107,  108,
 /*    30 */   109,   72,  167,  168,  154,  155,  156,  157,  158,  159,
 /*    40 */   160,  176,  177,   72,  107,  108,  109,  167,  168,   97,
 /*    50 */    98,   80,   73,  101,  174,  175,  176,  177,  178,  179,
 /*    60 */   170,  102,  172,  198,  199,  200,   71,  187,  188,  154,
 /*    70 */   155,  156,  157,  158,  159,  160,  105,  106,  107,  108,
 /*    80 */   109,   71,  167,  168,  105,  106,  107,  108,  109,  174,
 /*    90 */   175,  176,  177,  178,  179,  205,  206,  207,  208,  209,
 /*   100 */    73,  236,  237,  238,  239,  240,  241,  242,  243,  244,
 /*   110 */   245,  246,  247,  248,  249,  250,  251,  252,  253,    1,
 /*   120 */     2,   72,   72,    5,    6,    7,    8,    9,   10,   11,
 /*   130 */    12,   72,  105,  106,  107,  108,  109,  154,  155,  156,
 /*   140 */   157,  158,  159,  160,   33,   71,   28,   36,   99,  100,
 /*   150 */    32,   41,   34,   35,   43,   37,  106,   39,   47,   71,
 /*   160 */   177,  102,   44,  107,  108,  109,   48,   49,   50,   51,
 /*   170 */    52,   53,   54,  190,   56,   57,  193,  194,  195,  196,
 /*   180 */   197,   63,   64,    1,    2,   76,   68,   69,   73,   71,
 /*   190 */   107,  108,  109,   75,  153,  154,  155,  156,  157,  158,
 /*   200 */   159,  160,  161,  162,   22,   23,   24,   25,   93,    1,
 /*   210 */     2,  102,   30,    5,    6,  105,  106,  107,  108,  109,
 /*   220 */    38,   81,   40,  105,   70,   97,   98,   45,  110,  101,
 /*   230 */    48,   49,   50,    1,    2,   73,  118,    5,    6,   57,
 /*   240 */    58,   59,   60,   90,   91,   63,   64,  155,  130,  131,
 /*   250 */   132,   98,  134,   71,   46,   93,   48,   49,   50,  153,
 /*   260 */   154,  155,  156,  157,  158,  159,  160,  161,  162,  177,
 /*   270 */    73,   63,   64,   73,   66,  155,  171,   69,  173,   71,
 /*   280 */    48,   49,   50,   75,    1,    2,  166,  105,    5,    6,
 /*   290 */    93,  199,  110,   93,   70,   63,   64,  177,   66,  102,
 /*   300 */   180,   69,  102,   71,  210,  211,  212,   75,   95,   96,
 /*   310 */    97,   98,  104,  105,  101,   41,    1,   70,  110,   70,
 /*   320 */    27,  154,  155,  156,  157,  158,  159,  160,  107,  108,
 /*   330 */   109,   48,   49,   50,  167,  168,  104,  105,  130,  131,
 /*   340 */   132,  101,  110,  176,  177,   70,   63,   64,  170,   66,
 /*   350 */   172,   98,   69,   79,   71,  102,    1,    2,   75,   72,
 /*   360 */     5,    6,  130,  131,  132,  198,  199,  200,   75,   94,
 /*   370 */    95,   96,   97,   98,    1,  101,  101,  154,  155,  156,
 /*   380 */   157,  158,  159,  160,   90,   91,   71,  104,  105,  102,
 /*   390 */   167,  168,   98,  110,  216,  217,  218,  174,  175,  176,
 /*   400 */   177,  178,  179,   48,   49,   50,   26,  107,  108,  109,
 /*   410 */   187,  188,   98,  130,  131,  132,  102,  155,   63,   64,
 /*   420 */   105,   41,   42,  130,   69,  110,   71,   97,   98,   75,
 /*   430 */    75,  101,  154,  155,  156,  157,  158,  159,  160,  177,
 /*   440 */    72,   61,   62,   76,   71,   14,   75,   16,   17,   18,
 /*   450 */    19,   20,   21,   73,  170,  177,  172,   73,   72,   79,
 /*   460 */   105,  199,    1,    2,   27,  110,    5,    6,  190,  102,
 /*   470 */   102,  193,  194,  195,  196,  197,  105,   93,  105,   48,
 /*   480 */    49,  101,  163,  110,  130,  130,  131,  132,  102,  205,
 /*   490 */   206,  207,  208,  209,  154,  155,  156,  157,  158,  159,
 /*   500 */   160,  130,   69,  184,  107,  227,  228,  229,  111,   48,
 /*   510 */    49,   50,   75,   80,   81,   82,   83,  177,   85,   86,
 /*   520 */    87,   88,   99,  100,   63,   64,   71,  187,  188,  170,
 /*   530 */   190,  172,   71,  193,  194,  195,  196,  197,  105,  106,
 /*   540 */   107,  108,  109,   72,   71,  154,  155,  156,  157,  158,
 /*   550 */   159,  160,  153,  154,  155,  156,  157,  158,  159,  160,
 /*   560 */   161,  162,   81,   72,  133,  104,  105,  130,  177,   96,
 /*   570 */   170,  110,  172,  102,  215,  216,  217,  218,  187,  188,
 /*   580 */   170,  190,  172,   81,  193,  194,  195,  196,  197,   72,
 /*   590 */    72,  130,  131,  132,  219,  220,  221,  106,   81,   82,
 /*   600 */    83,  101,   85,   86,   87,   88,  206,  207,  208,  209,
 /*   610 */   154,  155,  156,  157,  158,  159,  160,   72,   69,   76,
 /*   620 */   102,   72,  105,  106,  107,  108,  109,  163,  218,   80,
 /*   630 */    81,   82,   83,  177,   85,   86,   87,   88,   73,    1,
 /*   640 */     2,   73,   72,    5,    6,  102,  190,  102,  184,  193,
 /*   650 */   194,  195,  196,  197,  105,  106,  107,  108,  109,   73,
 /*   660 */   154,  155,  156,  157,  158,  159,  160,  105,  106,  107,
 /*   670 */   108,  109,  102,  167,  168,  154,  155,  156,  157,  158,
 /*   680 */   159,  160,  176,  177,  228,  229,   48,   49,   50,    1,
 /*   690 */     2,  163,  163,    5,    6,  170,   31,  172,  177,   72,
 /*   700 */    31,   63,   64,   73,  198,  199,  200,    1,    2,   71,
 /*   710 */     2,  190,  184,  184,  193,  194,  195,  196,  197,  153,
 /*   720 */   154,  155,  156,  157,  158,  159,  160,  161,  162,  102,
 /*   730 */   205,  206,  207,  208,  209,   41,   48,   49,   50,   96,
 /*   740 */     1,    2,   76,  105,    5,    6,    2,   81,  110,   75,
 /*   750 */    73,   63,   64,  225,  226,  226,   81,   82,   83,   71,
 /*   760 */    85,   86,   87,   88,  222,  223,  224,   72,  130,  131,
 /*   770 */   132,  154,  155,  156,  157,  158,  159,  160,  163,   71,
 /*   780 */   105,  106,  107,  108,  109,   76,   73,   48,   49,   50,
 /*   790 */    81,    1,    2,  105,   73,  101,   76,  102,  110,  184,
 /*   800 */   163,   71,   63,   64,  130,  131,  189,    3,    4,   74,
 /*   810 */    71,  154,  155,  156,  157,  158,  159,  160,  130,  131,
 /*   820 */   132,  184,  102,   73,  167,  168,  105,  106,  107,  108,
 /*   830 */   109,  174,  175,  176,  177,  178,  179,    2,  132,   69,
 /*   840 */   225,  226,   72,   93,  105,   74,   73,   73,   73,  110,
 /*   850 */    55,   81,   82,   83,  106,   85,   86,   87,   88,    2,
 /*   860 */     1,    2,   71,  226,    5,    6,   93,   93,   93,  130,
 /*   870 */   131,  132,  201,  202,  130,  105,  106,  107,  108,  109,
 /*   880 */   153,  154,  155,  156,  157,  158,  159,  160,  161,  162,
 /*   890 */    71,  154,  155,  156,  157,  158,  159,  160,   71,   62,
 /*   900 */     2,   89,  211,  212,  167,  168,    2,   48,   49,   50,
 /*   910 */    75,  174,  175,  176,  177,  178,  179,  105,  106,  107,
 /*   920 */   108,  109,   71,  154,  155,  156,  157,  158,  159,  160,
 /*   930 */    71,   94,   95,   96,   97,   98,  167,  168,  101,  220,
 /*   940 */   221,  185,  186,  174,  175,  176,  177,  178,  179,  155,
 /*   950 */    71,   90,   91,  154,  155,  156,  157,  158,  159,  160,
 /*   960 */   166,  203,  204,    2,  105,  130,  167,  168,  102,  110,
 /*   970 */   223,  224,   73,  174,  175,  176,  177,  178,  179,  154,
 /*   980 */   155,  156,  157,  158,  159,  160,   72,  223,  224,  130,
 /*   990 */   131,  132,  167,  168,   73,   69,  185,  186,  101,  174,
 /*  1000 */   175,  176,  177,  178,  179,   76,   72,   81,   82,   83,
 /*  1010 */    73,   85,   86,   87,   88,  223,  224,  136,  137,  105,
 /*  1020 */   106,  107,  108,  109,  154,  155,  156,  157,  158,  159,
 /*  1030 */   160,  105,  106,  107,  108,  109,   73,  167,  168,  105,
 /*  1040 */   106,  107,  108,  109,  174,  175,  176,  177,  178,  179,
 /*  1050 */   154,  155,  156,  157,  158,  159,  160,   76,  170,   67,
 /*  1060 */   172,  223,  224,  167,  168,  105,  106,  107,  108,  109,
 /*  1070 */   174,  175,  176,  177,  178,  179,   73,   73,  154,  155,
 /*  1080 */   156,  157,  158,  159,  160,   73,   94,   95,   96,   97,
 /*  1090 */    98,  167,  168,  101,  206,  207,  208,  209,  174,  175,
 /*  1100 */   176,  177,  178,  179,   73,   73,   73,   73,  154,  155,
 /*  1110 */   156,  157,  158,  159,  160,   73,   72,   65,   73,   73,
 /*  1120 */    73,  167,  168,  130,  158,   89,  131,  163,  174,  175,
 /*  1130 */   176,  177,  178,  179,  154,  155,  156,  157,  158,  159,
 /*  1140 */   160,  105,  106,  107,  108,  109,  155,  167,  168,  105,
 /*  1150 */   106,  107,  108,  109,  174,  175,  176,  177,  178,  179,
 /*  1160 */    81,   82,   83,  158,   85,   86,   87,   88,  130,  105,
 /*  1170 */   106,  107,  108,  109,  158,  233,  154,  155,  156,  157,
 /*  1180 */   158,  159,  160,   42,  105,  106,  107,  108,  109,  167,
 /*  1190 */   168,  232,   26,   61,  155,   62,  174,  175,  176,  177,
 /*  1200 */   178,  179,  154,  155,  156,  157,  158,  159,  160,  234,
 /*  1210 */    72,  235,  234,  155,  235,  167,  168,  105,  106,  107,
 /*  1220 */   108,  109,  174,  175,  176,  177,  178,  179,  154,  155,
 /*  1230 */   156,  157,  158,  159,  160,  234,    2,  235,   70,  209,
 /*  1240 */   234,  167,  168,  105,  106,  107,  108,  109,  174,  175,
 /*  1250 */   176,  177,  178,  179,  154,  155,  156,  157,  158,  159,
 /*  1260 */   160,  235,   94,   95,   96,   97,   98,  167,  168,  101,
 /*  1270 */   235,  234,  202,    2,  174,  175,  176,  177,  178,  179,
 /*  1280 */   155,  231,  233,  154,  155,  156,  157,  158,  159,  160,
 /*  1290 */   234,  232,  235,   70,  233,  232,  167,  168,   41,  234,
 /*  1300 */   233,  235,  234,  174,  175,  176,  177,  178,  179,  154,
 /*  1310 */   155,  156,  157,  158,  159,  160,  235,   94,   95,   96,
 /*  1320 */    97,   98,  167,  168,  101,  234,  155,  235,  234,  174,
 /*  1330 */   175,  176,  177,  178,  179,  154,  155,  156,  157,  158,
 /*  1340 */   159,  160,  235,  234,  234,   70,  235,  232,  167,  168,
 /*  1350 */   105,  106,  107,  108,  109,  174,  175,  176,  177,  178,
 /*  1360 */   179,  154,  155,  156,  157,  158,  159,  160,  231,   94,
 /*  1370 */    95,   96,   97,   98,  167,  168,  101,  235,  233,  232,
 /*  1380 */   234,  174,  175,  176,  177,  178,  179,  154,  155,  156,
 /*  1390 */   157,  158,  159,  160,  235,   72,  155,  233,  231,  234,
 /*  1400 */   167,  168,  105,  106,  107,  108,  109,  174,  175,  176,
 /*  1410 */   177,  178,  179,  154,  155,  156,  157,  158,  159,  160,
 /*  1420 */   232,  235,  235,  234,  155,  235,  167,  168,  105,  106,
 /*  1430 */   107,  108,  109,  174,  175,  176,  177,  178,  179,  154,
 /*  1440 */   155,  156,  157,  158,  159,  160,  231,  231,   95,   96,
 /*  1450 */    97,   98,  167,  168,  101,   67,  204,  231,  231,  174,
 /*  1460 */   175,  176,  177,  178,  179,  154,  155,  156,  157,  158,
 /*  1470 */   159,  160,  158,  231,  186,  157,  173,  163,  167,  168,
 /*  1480 */   173,  173,   94,   95,   96,   97,   98,  176,  177,  101,
 /*  1490 */   154,  155,  156,  157,  158,  159,  160,  173,  184,  173,
 /*  1500 */   173,  187,  188,  167,  168,  173,  173,  173,  173,  198,
 /*  1510 */   199,  200,  176,  177,  173,  154,  155,  156,  157,  158,
 /*  1520 */   159,  160,  173,   94,   95,   96,   97,   98,  167,  168,
 /*  1530 */   101,  173,  231,  231,  198,  199,  200,  176,  177,  154,
 /*  1540 */   155,  156,  157,  158,  159,  160,  231,  154,  155,  156,
 /*  1550 */   157,  158,  159,  160,  235,  172,  172,  232,  172,  198,
 /*  1560 */   199,  200,  177,  154,  155,  156,  157,  158,  159,  160,
 /*  1570 */   177,  232,  232,  172,  232,  190,  167,  168,  193,  194,
 /*  1580 */   195,  196,  197,  190,  172,  172,  193,  194,  195,  196,
 /*  1590 */   197,  154,  155,  156,  157,  158,  159,  160,  172,  154,
 /*  1600 */   155,  156,  157,  158,  159,  160,  172,  154,  155,  156,
 /*  1610 */   157,  158,  159,  160,  177,  172,    2,    2,  254,  254,
 /*  1620 */   167,  168,  177,   95,   96,   97,   98,  190,  254,  101,
 /*  1630 */   193,  194,  195,  196,  197,  190,  254,  254,  193,  194,
 /*  1640 */   195,  196,  197,  154,  155,  156,  157,  158,  159,  160,
 /*  1650 */   254,  154,  155,  156,  157,  158,  159,  160,  154,  155,
 /*  1660 */   156,  157,  158,  159,  160,  254,  177,  254,  254,  254,
 /*  1670 */   254,  167,  168,  254,  177,  170,  254,  172,  254,  190,
 /*  1680 */   254,  254,  193,  194,  195,  196,  197,  190,  254,  254,
 /*  1690 */   193,  194,  195,  196,  197,  154,  155,  156,  157,  158,
 /*  1700 */   159,  160,  254,  154,  155,  156,  157,  158,  159,  160,
 /*  1710 */   205,  206,  207,  208,  209,  254,  254,  254,  177,  154,
 /*  1720 */   155,  156,  157,  158,  159,  160,  177,  254,  254,  164,
 /*  1730 */   165,  190,   73,  254,  193,  194,  195,  196,  197,  190,
 /*  1740 */   254,  254,  193,  194,  195,  196,  197,  154,  155,  156,
 /*  1750 */   157,  158,  159,  160,  254,  154,  155,  156,  157,  158,
 /*  1760 */   159,  160,  254,   69,  105,  106,  107,  108,  109,  254,
 /*  1770 */   177,  254,  254,  254,  254,   81,   82,   83,  177,   85,
 /*  1780 */    86,   87,   88,  190,  254,  254,  193,  194,  195,  196,
 /*  1790 */   197,  190,  254,  254,  193,  194,  195,  196,  197,  105,
 /*  1800 */   106,  107,  108,  109,  254,  254,  254,  254,   81,   82,
 /*  1810 */    83,  254,   85,   86,   87,   88,   81,   82,   83,  254,
 /*  1820 */    85,   86,   87,   88,   81,   82,   83,  254,   85,   86,
 /*  1830 */    87,   88,  105,  106,  107,  108,  109,  254,  254,  254,
 /*  1840 */   105,  106,  107,  108,  109,  254,   70,  254,  105,  106,
 /*  1850 */   107,  108,  109,  154,  155,  156,  157,  158,  159,  160,
 /*  1860 */   154,  155,  156,  157,  158,  159,  160,  254,  169,  254,
 /*  1870 */    94,   95,   96,   97,   98,  169,  254,  101,  254,  254,
 /*  1880 */   181,  182,  183,  254,  254,  254,  254,  181,  182,  183,
 /*  1890 */   154,  155,  156,  157,  158,  159,  160,  254,  154,  155,
 /*  1900 */   156,  157,  158,  159,  160,  169,  154,  155,  156,  157,
 /*  1910 */   158,  159,  160,  169,  254,  254,  254,  181,  182,  183,
 /*  1920 */   254,  169,  254,  254,  254,  181,  182,  183,  254,  254,
 /*  1930 */    72,  254,  254,  181,  182,  183,  154,  155,  156,  157,
 /*  1940 */   158,  159,  160,  154,  155,  156,  157,  158,  159,  160,
 /*  1950 */   254,  169,   94,   95,   96,   97,   98,  254,  169,  101,
 /*  1960 */   254,  254,  254,  181,  182,  183,  254,   72,  254,  254,
 /*  1970 */   181,  182,  183,  154,  155,  156,  157,  158,  159,  160,
 /*  1980 */   154,  155,  156,  157,  158,  159,  160,  254,  169,   94,
 /*  1990 */    95,   96,   97,   98,  254,  169,  101,    1,    2,  254,
 /*  2000 */   181,  182,  183,    1,    2,  254,    1,  181,  182,  183,
 /*  2010 */   154,  155,  156,  157,  158,  159,  160,  153,  154,  155,
 /*  2020 */   156,  157,  158,  159,  160,  169,  162,  254,  254,  254,
 /*  2030 */   154,  155,  156,  157,  158,  159,  160,  181,  182,  183,
 /*  2040 */   254,  165,  254,  254,   48,   49,   50,  254,  254,  254,
 /*  2050 */    48,   49,   50,   48,   49,   50,  254,  254,  254,   63,
 /*  2060 */    64,  254,  254,  254,  254,   63,   64,   71,   63,   64,
 /*  2070 */   254,  254,  254,   71,  254,  254,   71,  254,  254,  153,
 /*  2080 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2090 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  254,
 /*  2100 */   254,  105,  254,  254,  254,  254,  110,  105,  254,  254,
 /*  2110 */   105,  254,  110,  254,  254,  110,  254,  153,  154,  155,
 /*  2120 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2130 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2140 */   156,  157,  158,  159,  160,  170,  162,  172,  254,  254,
 /*  2150 */   254,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2160 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2170 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2180 */   162,  206,  207,  208,  209,  254,  254,  254,  213,  214,
 /*  2190 */   254,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2200 */   162,  254,  254,  254,  153,  154,  155,  156,  157,  158,
 /*  2210 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2220 */   159,  160,  254,  162,  254,  254,  254,  153,  154,  155,
 /*  2230 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2240 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2250 */   156,  157,  158,  159,  160,  254,  162,  254,  254,  254,
 /*  2260 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2270 */   254,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2280 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2290 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2300 */   162,  254,  254,  254,  254,  153,  154,  155,  156,  157,
 /*  2310 */   158,  159,  160,   70,  162,  153,  154,  155,  156,  157,
 /*  2320 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2330 */   158,  159,  160,  254,  162,  254,  254,   94,   95,   96,
 /*  2340 */    97,   98,  254,  254,  101,  153,  154,  155,  156,  157,
 /*  2350 */   158,  159,  160,  254,  162,  254,  254,  254,  153,  154,
 /*  2360 */   155,  156,  157,  158,  159,  160,  254,  162,  153,  154,
 /*  2370 */   155,  156,  157,  158,  159,  160,  254,  162,  254,  254,
 /*  2380 */   254,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2390 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2400 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2410 */   162,  254,  254,  254,  153,  154,  155,  156,  157,  158,
 /*  2420 */   159,  160,  254,  162,  254,  153,  154,  155,  156,  157,
 /*  2430 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2440 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2450 */   158,  159,  160,  254,  162,  254,  254,  254,  254,  153,
 /*  2460 */   154,  155,  156,  157,  158,  159,  160,   70,  162,  153,
 /*  2470 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2480 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  254,
 /*  2490 */   254,   94,   95,   96,   97,   98,  254,  254,  101,  153,
 /*  2500 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  254,
 /*  2510 */   254,  254,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2520 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2530 */   254,  162,  254,  254,  254,  153,  154,  155,  156,  157,
 /*  2540 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2550 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2560 */   158,  159,  160,  254,  162,  254,  254,  254,  153,  154,
 /*  2570 */   155,  156,  157,  158,  159,  160,  254,  162,  254,  153,
 /*  2580 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2590 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2600 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  254,
 /*  2610 */   254,  254,  254,  153,  154,  155,  156,  157,  158,  159,
 /*  2620 */   160,   70,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2630 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2640 */   160,  254,  162,  254,  254,   94,   95,   96,   97,   98,
 /*  2650 */   254,  254,  101,  153,  154,  155,  156,  157,  158,  159,
 /*  2660 */   160,  254,  162,  254,  254,  254,  153,  154,  155,  156,
 /*  2670 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2680 */   157,  158,  159,  160,  254,  162,  254,  254,  254,  153,
 /*  2690 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2700 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2710 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  254,
 /*  2720 */   254,  254,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2730 */   254,  162,   26,  153,  154,  155,  156,  157,  158,  159,
 /*  2740 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2750 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2760 */   160,  254,  162,  154,  155,  156,  157,  158,  159,  160,
 /*  2770 */   254,  254,  254,  254,  165,  154,  155,  156,  157,  158,
 /*  2780 */   159,  160,  254,  254,  254,  254,  165,  154,  155,  156,
 /*  2790 */   157,  158,  159,  160,  254,  254,  254,  254,  165,  254,
 /*  2800 */    94,   95,   96,   97,   98,  254,  254,  101,  154,  155,
 /*  2810 */   156,  157,  158,  159,  160,  254,  170,  254,  172,  165,
 /*  2820 */   254,  154,  155,  156,  157,  158,  159,  160,  170,  254,
 /*  2830 */   172,  254,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2840 */   254,  254,  254,  254,  165,  154,  155,  156,  157,  158,
 /*  2850 */   159,  160,  206,  207,  208,  209,  165,  154,  155,  156,
 /*  2860 */   157,  158,  159,  160,  206,  207,  208,  209,  165,  154,
 /*  2870 */   155,  156,  157,  158,  159,  160,  254,  254,  254,  254,
 /*  2880 */   165,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  2890 */   254,  254,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2900 */   254,  254,  254,  254,  165,  154,  155,  156,  157,  158,
 /*  2910 */   159,  160,  254,  254,  254,  254,  165,  154,  155,  156,
 /*  2920 */   157,  158,  159,  160,  254,  254,  254,  254,  165,  154,
 /*  2930 */   155,  156,  157,  158,  159,  160,  254,  254,  254,  254,
 /*  2940 */   165,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  2950 */   254,  254,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2960 */   254,  254,  254,  254,  165,  154,  155,  156,  157,  158,
 /*  2970 */   159,  160,  254,  254,  254,  254,  165,  154,  155,  156,
 /*  2980 */   157,  158,  159,  160,  254,  254,  254,  254,  165,  154,
 /*  2990 */   155,  156,  157,  158,  159,  160,  254,  254,  254,  254,
 /*  3000 */   165,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  3010 */   254,  254,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  3020 */   254,  254,  254,  254,  165,  154,  155,  156,  157,  158,
 /*  3030 */   159,  160,  254,  254,  254,  254,  165,  154,  155,  156,
 /*  3040 */   157,  158,  159,  160,  254,  254,  254,  154,  155,  156,
 /*  3050 */   157,  158,  159,  160,  154,  155,  156,  157,  158,  159,
 /*  3060 */   160,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  3070 */   254,  254,  189,  154,  155,  156,  157,  158,  159,  160,
 /*  3080 */   254,  254,  189,   94,   95,   96,   97,   98,  254,  189,
 /*  3090 */   101,  254,  254,  254,  254,  254,  189,  154,  155,  156,
 /*  3100 */   157,  158,  159,  160,  254,  254,  254,  254,  189,  154,
 /*  3110 */   155,  156,  157,  158,  159,  160,  254,  254,  254,  154,
 /*  3120 */   155,  156,  157,  158,  159,  160,  254,  254,  254,  254,
 /*  3130 */    73,  254,  189,  154,  155,  156,  157,  158,  159,  160,
 /*  3140 */   254,  254,  254,  254,  189,  154,  155,  156,  157,  158,
 /*  3150 */   159,  160,  254,  254,  189,  154,  155,  156,  157,  158,
 /*  3160 */   159,  160,  105,  106,  107,  108,  109,  254,  189,  154,
 /*  3170 */   155,  156,  157,  158,  159,  160,  254,  254,  254,  254,
 /*  3180 */   189,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  3190 */   189,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  3200 */   254,  254,  254,  254,  189,  154,  155,  156,  157,  158,
 /*  3210 */   159,  160,   72,  254,  254,  254,  189,  154,  155,  156,
 /*  3220 */   157,  158,  159,  160,  254,  254,  189,  154,  155,  156,
 /*  3230 */   157,  158,  159,  160,   94,   95,   96,   97,   98,  254,
 /*  3240 */   189,  101,  254,  254,  154,  155,  156,  157,  158,  159,
 /*  3250 */   160,  254,  189,  154,  155,  156,  157,  158,  159,  160,
 /*  3260 */   254,  254,  189,  154,  155,  156,  157,  158,  159,  160,
 /*  3270 */   154,  155,  156,  157,  158,  159,  160,   41,  254,  189,
 /*  3280 */   154,  155,  156,  157,  158,  159,  160,  254,  189,  154,
 /*  3290 */   155,  156,  157,  158,  159,  160,  254,  254,  189,  254,
 /*  3300 */   254,  254,  254,  254,  254,  189,  154,  155,  156,  157,
 /*  3310 */   158,  159,  160,  254,  254,  189,  154,  155,  156,  157,
 /*  3320 */   158,  159,  160,  254,  189,  254,  254,  165,  254,  254,
 /*  3330 */    94,   95,   96,   97,   98,  254,  254,  101,  254,  254,
 /*  3340 */   254,  189,   94,   95,   96,   97,   98,  254,  254,  101,
 /*  3350 */   158,  254,  254,  254,  254,  163,  254,  254,  254,  254,
 /*  3360 */   254,  254,  254,  254,  254,  254,  254,  254,  254,  254,
 /*  3370 */   254,  254,  254,  254,  254,  254,  184,  254,  254,  187,
 /*  3380 */   188,
};
#define YY_SHIFT_USE_DFLT (-80)
#define YY_SHIFT_COUNT (545)
#define YY_SHIFT_MIN   (-79)
#define YY_SHIFT_MAX   (3248)
static const short yy_shift_ofst[] = {
 /*     0 */   -80,  118,  208,  283,  283,  208,  232,  232,  283,  283,
 /*    10 */   283,  283,  283,  283,  283,  283,  283,  283,  283,  283,
 /*    20 */   283,  283,  283,  283,  283,  283,  283,  283,  283,  283,
 /*    30 */   232,  232,  232,  232,  232,  232,  232,  232,  232,  232,
 /*    40 */   232,  232,  355,  355,  355,  355,  355,  461,  461,  461,
 /*    50 */   461,  461,  461,  461,  461,  461,  461,  739,  739,  739,
 /*    60 */   739,  739,  739,  739,  739,  739,  739,  739,  739,  739,
 /*    70 */   739,  739,  739,  739,  638,  739,  739,  739,  739,  739,
 /*    80 */   739,  739,  739,  739,  739,  739,  739,  739,  739,  739,
 /*    90 */   739,  739,  739,  739,  739,  739,  739,  739,  739,  739,
 /*   100 */   739,  739,  739,  739,  739,  739,  739,  739,  739,  739,
 /*   110 */   739,  739,  739,  739,  739,  739,  739,  739,  739,  739,
 /*   120 */   739,  859,  859,  859,  182,  638,  688,  688,  688,  688,
 /*   130 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*   140 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*   150 */   688,  638,  638,  638,  638,  638,  638,  638,  638,  638,
 /*   160 */   638,  638,  638,  638,  638,  638,  638,  638,  638,  638,
 /*   170 */   638,  638,  638,  638, 2002, 2002, 2002, 2002, 2002, 1996,
 /*   180 */  2002, 2002, 2002, 1996,  674,  674,  437,  293,  835, 1996,
 /*   190 */   371,  371,  354,  354,  708, 1615, 1614,  373,  861,  708,
 /*   200 */   708,  708,  708,  804,  744,  861,  354,  354, 1615, 1614,
 /*   210 */  1234, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005,
 /*   220 */  3236, 2706, 2706, 2706, 2706,  837,  110,  110,  315,  315,
 /*   230 */   315,  315,  315,  315,  315,  315,  315,  315,  315,  315,
 /*   240 */   315,  294,  706,  153,  274,  694,  694,  804,  694,  694,
 /*   250 */  1133, 1038, 1133, 1132, 1133, 1132, 1166, 1141, 1257, 1038,
 /*   260 */  1133, 1132, 1166, 1141, 1257, 1038, 1133, 1132, 1166, 1141,
 /*   270 */  1257, 1038, 1133, 1132, 1133, 1132, 1133, 1132, 1133, 1132,
 /*   280 */  1133, 1132, 1166, 1141, 1133, 1132, 1166, 1141, 1271, 1234,
 /*   290 */  1133, 1132, 1133, 1132, 1038, 1133, 1132, 1038, 1133, 1132,
 /*   300 */  1133, 1132, 1166, 1141,  995,  995, 1038,  995,  993,  549,
 /*   310 */   433,  770,  517, 1694,  926, 1743, 1735, 1727, 1079,  675,
 /*   320 */   431,  380, 3140, 2551, 2397, 2243, 1776, 1388,  -29, 1895,
 /*   330 */  1858, 1275, 1223, 1168,  275,  992,   27, 1429, 1429, 1036,
 /*   340 */   -79,  -79,  -79,  -79,  -79,  -79,  -79,  -79,  -79,  -79,
 /*   350 */   -79, 2989, 3057, 1659,  721,  -21, 1323,  812, 1138, 3248,
 /*   360 */  2989, 2989, 2989, 1044,  934,  914, 1297, 1353,  562, 1245,
 /*   370 */  1528, 1112, 1112, 1112, 1112, 1112, 1112, 1112,  562,  562,
 /*   380 */   562,  562,  562,  213,  562,  562,  562,  562, 1112, 1112,
 /*   390 */  1112,  562,  562,  562,  562,  562,  562,  562,  562,  562,
 /*   400 */   562,  562,  562,  562,  562,  562,  562,  562,  562,  562,
 /*   410 */   562,  562, 1064,  960,  562,  562,  111,  330,  330,  300,
 /*   420 */   300,  300,  300,  221,  221,  200,  197,   49,   83,   83,
 /*   430 */   128,  128,   56,   56,  -48,  -48,   56,   56,  -63,  -63,
 /*   440 */   -63,  -63,  -63,  -63,  -63,  -63,  790,  775,  774,  709,
 /*   450 */   666,  423,  423,  423,  423,  773,  720,  750,  695,  543,
 /*   460 */   384,  367,  627,  570,  491,  545,   50,  397,  109,  162,
 /*   470 */   518,  473,  115,  471,  314,  253,  386,  368,  287,   59,
 /*   480 */   -41, 1052, 1047, 1046, 1045, 1042, 1034, 1033, 1032, 1031,
 /*   490 */  1012, 1004, 1003,  963,  937,  981,  929,  897,  897,  921,
 /*   500 */   899,  961,  866,  904,  748,  879,  851,  827,  898,  819,
 /*   510 */   748,  748,  791,  857,  795,  730,  748,  771,  735,  643,
 /*   520 */   713,  677,  669,  630,  665,  586,  568,  565,  500,  500,
 /*   530 */   502,  481,  455,  240,  240,  249,  247,  224,  154,  140,
 /*   540 */    88,   74,   10,   -5,  -53,    2,
};
#define YY_REDUCE_USE_DFLT (-136)
#define YY_REDUCE_COUNT (308)
#define YY_REDUCE_MIN   (-135)
#define YY_REDUCE_MAX   (3192)
static const short yy_reduce_ofst[] = {
 /*     0 */   881, -135,  278,  223, -120,  456,  391,  340, 1285, 1259,
 /*    10 */  1233, 1207, 1181, 1155, 1129, 1100, 1074, 1048, 1022,  980,
 /*    20 */   954,  924,  896,  870,  825,  799,  769,  737,  657,  -85,
 /*    30 */  1601, 1593, 1549, 1541, 1497, 1489, 1445, 1437, 1393, 1385,
 /*    40 */   521,  -17, 1361, 1336, 1311,  506,  167, 1856, 1826, 1819,
 /*    50 */  1789, 1782, 1752, 1744, 1736, 1706, 1699,  727,  566,  399,
 /*    60 */   106,   41, 2600, 2590, 2580, 2569, 2556, 2546, 2536, 2523,
 /*    70 */  2513, 2500, 2480, 2470, 1565, 2460, 2446, 2436, 2426, 2415,
 /*    80 */  2402, 2392, 2382, 2369, 2359, 2346, 2326, 2316, 2306, 2292,
 /*    90 */  2282, 2272, 2261, 2248, 2238, 2228, 2215, 2205, 2192, 2172,
 /*   100 */  2162, 2152, 2138, 2128, 2118, 2107, 2094, 2084, 2074, 2061,
 /*   110 */  2051, 2038, 2018, 2008, 1998, 1984, 1974, 1964, 1936, 1926,
 /*   120 */  1864, 1504, 1453, 1409, 1975, 3162, 3152, 3135, 3126, 3116,
 /*   130 */  3109, 3099, 3090, 3073, 3063, 3051, 3037, 3027, 3015, 3001,
 /*   140 */  2991, 2979, 2965, 2955, 2943, 2919, 2907, 2900, 2893, 2883,
 /*   150 */   617, 2871, 2859, 2847, 2835, 2823, 2811, 2799, 2787, 2775,
 /*   160 */  2763, 2751, 2739, 2727, 2715, 2703, 2691, 2679, 2667, 2654,
 /*   170 */  2633, 2621, 2609, 1876, 1505,  525,  284, -110, 2658,  359,
 /*   180 */  2646,  888,  400,  178, 3192, 1314,  615,  528,  120,  410,
 /*   190 */   262,   92,  637,  529,  542,  375,   94,  105,  811,  838,
 /*   200 */   792,  764,  747,  758,  794,  756,  464,  319,  719,  691,
 /*   210 */   671, 1443, 1434, 1426, 1413, 1412, 1401, 1386, 1384, 1383,
 /*   220 */  1315, 1342, 1340, 1339, 1325, 1319, 1302, 1301, 1358, 1349,
 /*   230 */  1341, 1335, 1334, 1333, 1332, 1327, 1326, 1324, 1308, 1307,
 /*   240 */  1303, 1288, 1318, 1288, 1242, 1227, 1226, 1252, 1216, 1215,
 /*   250 */  1190, 1269, 1187, 1189, 1186, 1165, 1188, 1164, 1167, 1241,
 /*   260 */  1159, 1146, 1147, 1145, 1137, 1171, 1142, 1110, 1115, 1067,
 /*   270 */  1050, 1125, 1111, 1109, 1107, 1094, 1092, 1091, 1081, 1068,
 /*   280 */  1066, 1065, 1063, 1061, 1057, 1056, 1059, 1049, 1030, 1070,
 /*   290 */  1035, 1037, 1026, 1006, 1058, 1002, 1001, 1039,  979,  978,
 /*   300 */   976,  975,  959,  942, 1016, 1005,  991,  966,  964,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   856, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*    10 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*    20 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*    30 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*    40 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*    50 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*    60 */  1307, 1307, 1307, 1307, 1307, 1307, 1067, 1071, 1066, 1070,
 /*    70 */  1155, 1151, 1156, 1152, 1307, 1307, 1307, 1307, 1307, 1307,
 /*    80 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*    90 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   100 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   110 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   120 */  1307, 1307, 1307, 1307, 1307, 1307, 1136, 1140, 1135, 1139,
 /*   130 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   140 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   150 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   160 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   170 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   180 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   190 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   200 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   210 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   220 */  1256, 1258, 1258, 1258, 1258, 1264, 1256, 1256, 1307, 1307,
 /*   230 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   240 */  1307, 1307, 1307, 1307, 1256, 1256, 1256, 1307, 1256, 1256,
 /*   250 */  1264, 1307, 1264, 1262, 1264, 1262, 1258, 1260, 1256, 1307,
 /*   260 */  1264, 1262, 1258, 1260, 1256, 1307, 1264, 1262, 1258, 1260,
 /*   270 */  1256, 1307, 1264, 1262, 1264, 1262, 1264, 1262, 1264, 1262,
 /*   280 */  1264, 1262, 1258, 1260, 1264, 1262, 1258, 1260, 1307, 1307,
 /*   290 */  1264, 1262, 1264, 1262, 1307, 1264, 1262, 1307, 1264, 1262,
 /*   300 */  1264, 1262, 1258, 1260, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   310 */  1307, 1307, 1307, 1307, 1307, 1102, 1307, 1031, 1031, 1307,
 /*   320 */  1307,  922, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   330 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1248, 1245, 1307,
 /*   340 */  1138, 1142, 1137, 1141, 1133, 1132, 1131, 1130, 1129, 1128,
 /*   350 */  1127, 1120, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1263,
 /*   360 */  1257, 1259, 1255, 1307, 1307, 1307,  985, 1117, 1089,  984,
 /*   370 */  1045, 1057, 1056, 1055, 1054, 1053, 1052, 1051, 1037, 1069,
 /*   380 */  1073, 1068, 1072, 1002, 1157, 1153, 1158, 1154, 1060,  895,
 /*   390 */   896, 1017, 1016, 1015, 1014, 1013, 1012, 1011, 1033, 1030,
 /*   400 */  1029, 1028, 1027, 1026, 1025, 1024, 1023, 1022, 1021, 1020,
 /*   410 */  1019, 1018, 1307, 1307,  892,  891, 1149, 1119, 1118, 1106,
 /*   420 */  1105, 1090, 1091,  990,  991, 1307, 1307, 1307,  979,  980,
 /*   430 */  1047, 1046,  949,  948, 1004, 1003,  965,  966,  924,  923,
 /*   440 */   929,  928,  934,  933,  908,  909, 1307, 1307,  986, 1307,
 /*   450 */  1307, 1222, 1226, 1225, 1223, 1307, 1307, 1307, 1307, 1307,
 /*   460 */  1307,  970, 1307, 1307, 1307, 1307, 1307, 1170, 1307, 1307,
 /*   470 */  1307, 1307, 1307, 1307,  876, 1307, 1307, 1307, 1307, 1307,
 /*   480 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307,
 /*   490 */  1307, 1307, 1307, 1307, 1307, 1307, 1099, 1116, 1115, 1307,
 /*   500 */  1307, 1307, 1224, 1307, 1218, 1211, 1188, 1190, 1307, 1203,
 /*   510 */  1169, 1168, 1186, 1307, 1307, 1185, 1184, 1307, 1307, 1307,
 /*   520 */  1307, 1307, 1307, 1307, 1307, 1307, 1307, 1307, 1044, 1043,
 /*   530 */  1035, 1031,  880, 1001, 1000, 1307, 1307, 1307, 1307, 1058,
 /*   540 */   894,  890,  888,  884,  879, 1307, 1306, 1305, 1304, 1303,
 /*   550 */  1302, 1301, 1300, 1299, 1298, 1297, 1296, 1295, 1150, 1294,
 /*   560 */  1293, 1292, 1291, 1285, 1284, 1282, 1281, 1280, 1279, 1278,
 /*   570 */  1277, 1276, 1275, 1274, 1273, 1272, 1271, 1270, 1269, 1268,
 /*   580 */  1267, 1266, 1265, 1239, 1238, 1247, 1246, 1254, 1253, 1250,
 /*   590 */  1249, 1244, 1252, 1111, 1113, 1134, 1126, 1125, 1124, 1123,
 /*   600 */  1122, 1121, 1114, 1112, 1110, 1104, 1103, 1101, 1100, 1099,
 /*   610 */  1109, 1108, 1107, 1094, 1093, 1092, 1088, 1087, 1086, 1085,
 /*   620 */  1084, 1083, 1082, 1081, 1080, 1079, 1078, 1077, 1098, 1097,
 /*   630 */  1096, 1095, 1243, 1242, 1241, 1240,  994,  993,  992,  989,
 /*   640 */   988,  987,  986, 1233, 1232, 1234, 1231, 1236, 1237, 1235,
 /*   650 */  1230, 1228, 1227, 1229, 1221, 1216, 1219, 1220, 1217, 1215,
 /*   660 */  1206, 1209, 1214, 1212, 1210, 1208, 1207, 1205, 1181, 1189,
 /*   670 */  1191, 1204, 1202, 1201, 1200, 1199, 1198, 1197, 1196, 1195,
 /*   680 */  1194, 1193, 1192, 1187, 1183, 1179, 1178, 1177, 1176, 1175,
 /*   690 */  1174, 1173,  884, 1172, 1171,  983,  982,  981,  978,  977,
 /*   700 */   976,  975,  974,  973,  972,  971,  970, 1182, 1180, 1160,
 /*   710 */  1163, 1164, 1167, 1166, 1165, 1162, 1161, 1159, 1290, 1289,
 /*   720 */  1288, 1287, 1286, 1283, 1039, 1041, 1050, 1049, 1048, 1042,
 /*   730 */  1040, 1038,  947,  946,  945,  944,  943,  942,  952,  951,
 /*   740 */   950,  941,  940,  939,  938, 1261, 1034, 1036,  881,  996,
 /*   750 */   998, 1062, 1065, 1064, 1063, 1061, 1010, 1009, 1008, 1007,
 /*   760 */  1006, 1005,  999,  997,  995,  920, 1149, 1148, 1147, 1146,
 /*   770 */  1145, 1144, 1143, 1032, 1075, 1076, 1074, 1059,  967,  969,
 /*   780 */   968,  964,  963,  962,  961,  960,  959,  958,  957,  956,
 /*   790 */   955,  954,  953,  893,  927,  926,  925,  932,  931,  930,
 /*   800 */   922,  921,  920,  919,  918,  917,  937,  936,  935,  916,
 /*   810 */   915,  914,  913,  910,  912,  911,  907,  906,  905,  904,
 /*   820 */   903,  902,  901,  900,  899,  898,  897,  889,  887,  886,
 /*   830 */   885,  883,  882,  878,  874,  873,  877,  876,  875,  872,
 /*   840 */   871,  870,  869,  868,  867,  866,  865,  864,  863,  862,
 /*   850 */   861,  860,  859,  858,  857,
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
  "TRUE",          "AT",            "BRACKET_L",     "BRACKET_R",   
  "COLON_DASH",    "CBRACKET_L",    "CBRACKET_R",    "PAREN_L",     
  "PAREN_R",       "PERIOD",        "MACRO_STRING",  "TILDE",       
  "DBL_COLON",     "ARROW_LEQ",     "ARROW_REQ",     "ARROW_LDASH", 
  "COLON",         "EQ",            "DBL_EQ",        "NEQ",         
  "NOT_EQ",        "LTHAN",         "GTHAN",         "LTHAN_EQ",    
  "GTHAN_EQ",      "DBL_PERIOD",    "BIG_CONJ",      "BIG_DISJ",    
  "POUND",         "SEMICOLON",     "EQUIV",         "IMPL",        
  "ARROW_RDASH",   "DBL_PLUS",      "PIPE",          "DBL_GTHAN",   
  "DBL_LTHAN",     "AMP",           "COMMA",         "DBL_AMP",     
  "NOT",           "DASH",          "PLUS",          "STAR",        
  "INT_DIV",       "MOD",           "ABS",           "CARROT",      
  "UMINUS",        "PREC_4",        "PREC_3",        "PREC_2",      
  "PREC_1",        "PREC_0",        "EOF",           "ERR_IO",      
  "ERR_UNKNOWN_SYMBOL",  "ERR_UNTERMINATED_STRING",  "ERR_UNTERMINATED_ASP",  "ERR_UNTERMINATED_LUA",
  "ERR_UNTERMINATED_F2LP",  "ERR_UNTERMINATED_BLK_COMMENT",  "ERR_SYNTAX",    "ERR_PAREN_MISMATCH",
  "ARG",           "NOOP",          "CONSTANT_ID",   "VARIABLE_ID", 
  "OBJECT_ID",     "HIDE",          "OBSERVED",      "error",       
  "start",         "statement_lst",  "statement",     "stmt_macro_def",
  "stmt_constant_def",  "stmt_object_def",  "stmt_variable_def",  "stmt_sort_def",
  "stmt_code_blk",  "stmt_law",      "stmt_show",     "stmt_hide",   
  "stmt_noconcurrency",  "stmt_strong_noconcurrency",  "stmt_maxafvalue",  "stmt_maxadditive",
  "stmt_query",    "base_elem",     "base_elem_no_const",  "constant",    
  "object",        "object_nullary",  "variable",      "lua",         
  "undeclared",    "term_lst",      "term",          "constant_one_const",
  "term_no_const_lst",  "term_no_const",  "const_anon",    "term_strong", 
  "term_strong_candidate",  "term_no_const_strong",  "num_range",     "num_range_eval",
  "term_integral",  "term_int_eval",  "formula",       "formula_base",
  "comparison",    "atomic_formula",  "formula_quant",  "formula_card",
  "atomic_formula_anon",  "formula_no_const",  "formula_no_const_base",  "comparison_no_const",
  "atomic_formula_one_const",  "quant_lst",     "quant_op",      "card_var_lst",
  "card_var_lst_inner",  "term_temporal",  "term_temporal_strong",  "term_temporal_strong_candidate",
  "constant_temporal",  "formula_temporal",  "formula_temporal_base",  "comparison_temporal",
  "formula_temporal_quant",  "formula_temporal_card",  "head_formula",  "atomic_head_formula",
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
  "law_causes",    "law_increments",  "law_decrements",  "law_mcause",  
  "law_always",    "law_constraint",  "law_impossible",  "law_never",   
  "law_default",   "law_exogenous",  "law_inertial",  "law_nonexecutable",
  "law_rigid",     "law_observed",
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
 /*  25 */ "const_anon ::= IDENTIFIER",
 /*  26 */ "const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  27 */ "object ::= OBJECT_ID PAREN_L term_lst PAREN_R",
 /*  28 */ "object ::= object_nullary",
 /*  29 */ "object_nullary ::= OBJECT_ID",
 /*  30 */ "object ::= undeclared",
 /*  31 */ "variable ::= VARIABLE_ID",
 /*  32 */ "lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  33 */ "lua ::= AT_IDENTIFIER",
 /*  34 */ "undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  35 */ "undeclared ::= IDENTIFIER",
 /*  36 */ "term_lst ::= term",
 /*  37 */ "term_lst ::= term_lst COMMA term",
 /*  38 */ "constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R",
 /*  39 */ "constant_one_const ::= CONSTANT_ID",
 /*  40 */ "term_no_const_lst ::= term_no_const",
 /*  41 */ "term_no_const_lst ::= term_no_const_lst COMMA term_no_const",
 /*  42 */ "term ::= base_elem",
 /*  43 */ "term ::= INTEGER",
 /*  44 */ "term ::= STRING_LITERAL",
 /*  45 */ "term ::= PAREN_L term PAREN_R",
 /*  46 */ "term ::= TRUE",
 /*  47 */ "term ::= FALSE",
 /*  48 */ "term ::= MAXSTEP",
 /*  49 */ "term ::= MAXADDITIVE",
 /*  50 */ "term ::= MAXAFVALUE",
 /*  51 */ "term ::= DASH term",
 /*  52 */ "term ::= ABS term",
 /*  53 */ "term ::= term DASH term",
 /*  54 */ "term ::= term PLUS term",
 /*  55 */ "term ::= term STAR term",
 /*  56 */ "term ::= term INT_DIV term",
 /*  57 */ "term ::= term MOD term",
 /*  58 */ "term_strong ::= base_elem_no_const",
 /*  59 */ "term_strong ::= INTEGER",
 /*  60 */ "term_strong ::= STRING_LITERAL",
 /*  61 */ "term_strong ::= PAREN_L term_strong PAREN_R",
 /*  62 */ "term_strong ::= MAXSTEP",
 /*  63 */ "term_strong ::= MAXADDITIVE",
 /*  64 */ "term_strong ::= MAXAFVALUE",
 /*  65 */ "term_strong ::= DASH term_strong",
 /*  66 */ "term_strong ::= ABS term",
 /*  67 */ "term_strong_candidate ::= DASH constant",
 /*  68 */ "term_strong ::= term_strong_candidate DASH term",
 /*  69 */ "term_strong ::= term_strong_candidate PLUS term",
 /*  70 */ "term_strong ::= term_strong_candidate STAR term",
 /*  71 */ "term_strong ::= term_strong_candidate INT_DIV term",
 /*  72 */ "term_strong ::= term_strong_candidate MOD term",
 /*  73 */ "term_strong ::= constant DASH term",
 /*  74 */ "term_strong ::= constant PLUS term",
 /*  75 */ "term_strong ::= constant STAR term",
 /*  76 */ "term_strong ::= constant INT_DIV term",
 /*  77 */ "term_strong ::= constant MOD term",
 /*  78 */ "term_strong ::= term_strong DASH term",
 /*  79 */ "term_strong ::= term_strong PLUS term",
 /*  80 */ "term_strong ::= term_strong STAR term",
 /*  81 */ "term_strong ::= term_strong INT_DIV term",
 /*  82 */ "term_strong ::= term_strong MOD term",
 /*  83 */ "term_no_const_strong ::= base_elem_no_const",
 /*  84 */ "term_no_const_strong ::= INTEGER",
 /*  85 */ "term_no_const_strong ::= STRING_LITERAL",
 /*  86 */ "term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R",
 /*  87 */ "term_no_const_strong ::= MAXSTEP",
 /*  88 */ "term_no_const_strong ::= MAXADDITIVE",
 /*  89 */ "term_no_const_strong ::= MAXAFVALUE",
 /*  90 */ "term_no_const_strong ::= constant",
 /*  91 */ "term_no_const_strong ::= DASH term_no_const_strong",
 /*  92 */ "term_no_const_strong ::= ABS term_no_const",
 /*  93 */ "term_no_const_strong ::= term_no_const_strong DASH term_no_const",
 /*  94 */ "term_no_const_strong ::= term_no_const_strong PLUS term_no_const",
 /*  95 */ "term_no_const_strong ::= term_no_const_strong STAR term_no_const",
 /*  96 */ "term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const",
 /*  97 */ "term_no_const_strong ::= term_no_const_strong MOD term_no_const",
 /*  98 */ "term_no_const ::= base_elem_no_const",
 /*  99 */ "term_no_const ::= INTEGER",
 /* 100 */ "term_no_const ::= STRING_LITERAL",
 /* 101 */ "term_no_const ::= PAREN_L term_no_const PAREN_R",
 /* 102 */ "term_no_const ::= TRUE",
 /* 103 */ "term_no_const ::= FALSE",
 /* 104 */ "term_no_const ::= MAXSTEP",
 /* 105 */ "term_no_const ::= MAXADDITIVE",
 /* 106 */ "term_no_const ::= MAXAFVALUE",
 /* 107 */ "term_no_const ::= constant",
 /* 108 */ "term_no_const ::= DASH term_no_const",
 /* 109 */ "term_no_const ::= ABS term_no_const",
 /* 110 */ "term_no_const ::= term_no_const DASH term_no_const",
 /* 111 */ "term_no_const ::= term_no_const PLUS term_no_const",
 /* 112 */ "term_no_const ::= term_no_const STAR term_no_const",
 /* 113 */ "term_no_const ::= term_no_const INT_DIV term_no_const",
 /* 114 */ "term_no_const ::= term_no_const MOD term_no_const",
 /* 115 */ "term_integral ::= INTEGER",
 /* 116 */ "term_integral ::= PAREN_L term_integral PAREN_R",
 /* 117 */ "term_integral ::= TRUE",
 /* 118 */ "term_integral ::= FALSE",
 /* 119 */ "term_integral ::= MAXSTEP",
 /* 120 */ "term_integral ::= MAXADDITIVE",
 /* 121 */ "term_integral ::= MAXAFVALUE",
 /* 122 */ "term_integral ::= DASH term_integral",
 /* 123 */ "term_integral ::= ABS term_integral",
 /* 124 */ "term_integral ::= term_integral DASH term_integral",
 /* 125 */ "term_integral ::= term_integral PLUS term_integral",
 /* 126 */ "term_integral ::= term_integral STAR term_integral",
 /* 127 */ "term_integral ::= term_integral INT_DIV term_integral",
 /* 128 */ "term_integral ::= term_integral MOD term_integral",
 /* 129 */ "num_range ::= term_integral DBL_PERIOD term_integral",
 /* 130 */ "num_range_eval ::= term_int_eval DBL_PERIOD term_int_eval",
 /* 131 */ "term_int_eval ::= INTEGER",
 /* 132 */ "term_int_eval ::= PAREN_L term_int_eval PAREN_R",
 /* 133 */ "term_int_eval ::= DASH term_int_eval",
 /* 134 */ "term_int_eval ::= ABS term_int_eval",
 /* 135 */ "term_int_eval ::= term_int_eval DASH term_int_eval",
 /* 136 */ "term_int_eval ::= term_int_eval PLUS term_int_eval",
 /* 137 */ "term_int_eval ::= term_int_eval STAR term_int_eval",
 /* 138 */ "term_int_eval ::= term_int_eval INT_DIV term_int_eval",
 /* 139 */ "term_int_eval ::= term_int_eval MOD term_int_eval",
 /* 140 */ "formula ::= formula_base",
 /* 141 */ "formula ::= PAREN_L formula PAREN_R",
 /* 142 */ "formula ::= NOT formula",
 /* 143 */ "formula ::= DASH formula",
 /* 144 */ "formula ::= formula AMP formula",
 /* 145 */ "formula ::= formula DBL_PLUS formula",
 /* 146 */ "formula ::= formula PIPE formula",
 /* 147 */ "formula ::= formula EQUIV formula",
 /* 148 */ "formula ::= formula IMPL formula",
 /* 149 */ "formula ::= formula ARROW_RDASH formula",
 /* 150 */ "formula_base ::= comparison",
 /* 151 */ "formula_base ::= atomic_formula",
 /* 152 */ "formula_base ::= formula_quant",
 /* 153 */ "formula_base ::= formula_card",
 /* 154 */ "formula_base ::= TRUE",
 /* 155 */ "formula_base ::= FALSE",
 /* 156 */ "comparison ::= term_strong EQ term",
 /* 157 */ "comparison ::= term_strong DBL_EQ term",
 /* 158 */ "comparison ::= term_strong NEQ term",
 /* 159 */ "comparison ::= term_strong LTHAN term",
 /* 160 */ "comparison ::= term_strong GTHAN term",
 /* 161 */ "comparison ::= term_strong LTHAN_EQ term",
 /* 162 */ "comparison ::= term_strong GTHAN_EQ term",
 /* 163 */ "comparison ::= term_strong_candidate EQ term",
 /* 164 */ "comparison ::= term_strong_candidate DBL_EQ term",
 /* 165 */ "comparison ::= term_strong_candidate NEQ term",
 /* 166 */ "comparison ::= term_strong_candidate LTHAN term",
 /* 167 */ "comparison ::= term_strong_candidate GTHAN term",
 /* 168 */ "comparison ::= term_strong_candidate LTHAN_EQ term",
 /* 169 */ "comparison ::= term_strong_candidate GTHAN_EQ term",
 /* 170 */ "comparison ::= constant DBL_EQ term",
 /* 171 */ "comparison ::= constant NEQ term",
 /* 172 */ "comparison ::= constant LTHAN term",
 /* 173 */ "comparison ::= constant GTHAN term",
 /* 174 */ "comparison ::= constant LTHAN_EQ term",
 /* 175 */ "comparison ::= constant GTHAN_EQ term",
 /* 176 */ "atomic_formula ::= constant",
 /* 177 */ "atomic_formula ::= TILDE constant",
 /* 178 */ "atomic_formula ::= constant EQ term",
 /* 179 */ "atomic_formula_anon ::= atomic_formula",
 /* 180 */ "atomic_formula_anon ::= const_anon",
 /* 181 */ "atomic_formula_anon ::= TILDE const_anon",
 /* 182 */ "atomic_formula_anon ::= const_anon EQ term",
 /* 183 */ "formula_no_const ::= formula_no_const_base",
 /* 184 */ "formula_no_const ::= PAREN_L formula_no_const PAREN_R",
 /* 185 */ "formula_no_const ::= NOT formula_no_const",
 /* 186 */ "formula_no_const ::= DASH formula_no_const",
 /* 187 */ "formula_no_const ::= formula_no_const AMP formula_no_const",
 /* 188 */ "formula_no_const ::= formula_no_const DBL_PLUS formula_no_const",
 /* 189 */ "formula_no_const ::= formula_no_const PIPE formula_no_const",
 /* 190 */ "formula_no_const ::= formula_no_const EQUIV formula_no_const",
 /* 191 */ "formula_no_const ::= formula_no_const IMPL formula_no_const",
 /* 192 */ "formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const",
 /* 193 */ "formula_no_const_base ::= comparison_no_const",
 /* 194 */ "formula_no_const_base ::= TRUE",
 /* 195 */ "formula_no_const_base ::= FALSE",
 /* 196 */ "comparison_no_const ::= term_no_const_strong EQ term_no_const",
 /* 197 */ "comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const",
 /* 198 */ "comparison_no_const ::= term_no_const_strong NEQ term_no_const",
 /* 199 */ "comparison_no_const ::= term_no_const_strong LTHAN term_no_const",
 /* 200 */ "comparison_no_const ::= term_no_const_strong GTHAN term_no_const",
 /* 201 */ "comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const",
 /* 202 */ "comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const",
 /* 203 */ "atomic_formula_one_const ::= constant_one_const",
 /* 204 */ "atomic_formula_one_const ::= TILDE constant_one_const",
 /* 205 */ "atomic_formula_one_const ::= constant_one_const EQ term_no_const",
 /* 206 */ "formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R",
 /* 207 */ "quant_lst ::= quant_op variable",
 /* 208 */ "quant_lst ::= quant_lst quant_op variable",
 /* 209 */ "quant_op ::= BIG_CONJ",
 /* 210 */ "quant_op ::= BIG_DISJ",
 /* 211 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 212 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 213 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 214 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 215 */ "formula_card ::= CBRACKET_L formula CBRACKET_R",
 /* 216 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R",
 /* 217 */ "formula_card ::= CBRACKET_L formula CBRACKET_R term",
 /* 218 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term",
 /* 219 */ "card_var_lst ::= card_var_lst_inner PIPE",
 /* 220 */ "card_var_lst_inner ::= variable",
 /* 221 */ "card_var_lst_inner ::= card_var_lst_inner COMMA variable",
 /* 222 */ "term_temporal ::= base_elem_no_const",
 /* 223 */ "term_temporal ::= INTEGER",
 /* 224 */ "term_temporal ::= STRING_LITERAL",
 /* 225 */ "term_temporal ::= PAREN_L term_temporal PAREN_R",
 /* 226 */ "term_temporal ::= TRUE",
 /* 227 */ "term_temporal ::= FALSE",
 /* 228 */ "term_temporal ::= MAXSTEP",
 /* 229 */ "term_temporal ::= MAXADDITIVE",
 /* 230 */ "term_temporal ::= MAXAFVALUE",
 /* 231 */ "term_temporal ::= constant",
 /* 232 */ "term_temporal ::= DASH term_temporal",
 /* 233 */ "term_temporal ::= ABS term_temporal",
 /* 234 */ "term_temporal ::= term_temporal COLON term",
 /* 235 */ "term_temporal ::= term_temporal DASH term_temporal",
 /* 236 */ "term_temporal ::= term_temporal PLUS term_temporal",
 /* 237 */ "term_temporal ::= term_temporal STAR term_temporal",
 /* 238 */ "term_temporal ::= term_temporal INT_DIV term_temporal",
 /* 239 */ "term_temporal ::= term_temporal MOD term_temporal",
 /* 240 */ "term_temporal_strong ::= base_elem_no_const",
 /* 241 */ "term_temporal_strong ::= INTEGER",
 /* 242 */ "term_temporal_strong ::= STRING_LITERAL",
 /* 243 */ "term_temporal_strong ::= PAREN_L term_temporal_strong PAREN_R",
 /* 244 */ "term_temporal_strong ::= MAXSTEP",
 /* 245 */ "term_temporal_strong ::= MAXADDITIVE",
 /* 246 */ "term_temporal_strong ::= MAXAFVALUE",
 /* 247 */ "term_temporal_strong ::= term_temporal_strong COLON term_strong",
 /* 248 */ "term_temporal_strong ::= DASH term_temporal_strong",
 /* 249 */ "term_temporal_strong ::= ABS term_temporal",
 /* 250 */ "term_temporal_strong ::= term_temporal_strong DASH term_temporal",
 /* 251 */ "term_temporal_strong ::= term_temporal_strong PLUS term_temporal",
 /* 252 */ "term_temporal_strong ::= term_temporal_strong STAR term_temporal",
 /* 253 */ "term_temporal_strong ::= term_temporal_strong INT_DIV term_temporal",
 /* 254 */ "term_temporal_strong ::= term_temporal_strong MOD term_temporal",
 /* 255 */ "formula_temporal ::= formula_temporal_base",
 /* 256 */ "formula_temporal ::= PAREN_L formula_temporal PAREN_R",
 /* 257 */ "formula_temporal ::= NOT formula_temporal",
 /* 258 */ "formula_temporal ::= DASH formula_temporal",
 /* 259 */ "formula_temporal ::= formula_temporal AMP formula_temporal",
 /* 260 */ "formula_temporal ::= formula_temporal DBL_PLUS formula_temporal",
 /* 261 */ "formula_temporal ::= formula_temporal PIPE formula_temporal",
 /* 262 */ "formula_temporal ::= formula_temporal EQUIV formula_temporal",
 /* 263 */ "formula_temporal ::= formula_temporal IMPL formula_temporal",
 /* 264 */ "formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal",
 /* 265 */ "formula_temporal ::= term_temporal_strong COLON formula",
 /* 266 */ "formula_temporal_base ::= comparison_temporal",
 /* 267 */ "formula_temporal_base ::= atomic_formula",
 /* 268 */ "formula_temporal_base ::= formula_temporal_quant",
 /* 269 */ "formula_temporal_base ::= formula_temporal_card",
 /* 270 */ "formula_temporal_base ::= TRUE",
 /* 271 */ "formula_temporal_base ::= FALSE",
 /* 272 */ "comparison_temporal ::= term_temporal_strong EQ term_temporal",
 /* 273 */ "comparison_temporal ::= term_temporal_strong DBL_EQ term_temporal",
 /* 274 */ "comparison_temporal ::= term_temporal_strong NEQ term_temporal",
 /* 275 */ "comparison_temporal ::= term_temporal_strong LTHAN term_temporal",
 /* 276 */ "comparison_temporal ::= term_temporal_strong GTHAN term_temporal",
 /* 277 */ "comparison_temporal ::= term_temporal_strong LTHAN_EQ term_temporal",
 /* 278 */ "comparison_temporal ::= term_temporal_strong GTHAN_EQ term_temporal",
 /* 279 */ "formula_temporal_quant ::= BRACKET_L quant_lst PIPE formula_temporal BRACKET_R",
 /* 280 */ "formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R",
 /* 281 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R",
 /* 282 */ "formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal",
 /* 283 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal",
 /* 284 */ "formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R",
 /* 285 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R",
 /* 286 */ "formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R term_temporal",
 /* 287 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R term_temporal",
 /* 288 */ "head_formula ::= head_formula AMP head_formula",
 /* 289 */ "head_formula ::= comparison",
 /* 290 */ "head_formula ::= atomic_head_formula",
 /* 291 */ "head_formula ::= formula_smpl_card",
 /* 292 */ "head_formula ::= TRUE",
 /* 293 */ "head_formula ::= FALSE",
 /* 294 */ "atomic_head_formula ::= atomic_formula",
 /* 295 */ "atomic_head_formula ::= DASH constant",
 /* 296 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 297 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 298 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 299 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 300 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 301 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 302 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 303 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 304 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 305 */ "macro_def_lst ::= macro_bnd",
 /* 306 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 307 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 308 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 309 */ "macro_args ::= macro_arg",
 /* 310 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 311 */ "macro_arg ::= POUND_INTEGER",
 /* 312 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 313 */ "sort_lst ::= sort",
 /* 314 */ "sort_lst ::= sort_lst COMMA sort",
 /* 315 */ "sort ::= sort_id_nr",
 /* 316 */ "sort ::= sort_id_nr STAR",
 /* 317 */ "sort ::= sort_id_nr CARROT",
 /* 318 */ "sort ::= sort PLUS object_nullary",
 /* 319 */ "sort ::= sort PLUS IDENTIFIER",
 /* 320 */ "sort ::= sort PLUS INTEGER",
 /* 321 */ "sort_id_nr ::= sort_id",
 /* 322 */ "sort_id_nr ::= sort_nr",
 /* 323 */ "sort_nr ::= num_range",
 /* 324 */ "sort_id ::= IDENTIFIER",
 /* 325 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 326 */ "constant_bnd_lst ::= constant_bnd",
 /* 327 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 328 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 329 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 330 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 331 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 332 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 333 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 334 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 335 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 336 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 337 */ "constant_dcl_type ::= ABACTION",
 /* 338 */ "constant_dcl_type ::= ACTION",
 /* 339 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 340 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 341 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 342 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 343 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 344 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 345 */ "constant_dcl_type ::= RIGID",
 /* 346 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 347 */ "constant_dcl_type ::= SDFLUENT",
 /* 348 */ "attrib_spec ::= ATTRIBUTE",
 /* 349 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 350 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 351 */ "object_bnd_lst ::= object_bnd",
 /* 352 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 353 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 354 */ "object_lst ::= object_spec",
 /* 355 */ "object_lst ::= object_lst COMMA object_spec",
 /* 356 */ "object_spec ::= IDENTIFIER",
 /* 357 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 358 */ "object_spec ::= INTEGER",
 /* 359 */ "object_spec ::= num_range",
 /* 360 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 361 */ "variable_bnd_lst ::= variable_bnd",
 /* 362 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 363 */ "variable_bnd ::= variable_lst DBL_COLON sort",
 /* 364 */ "variable_lst ::= IDENTIFIER",
 /* 365 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 366 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 367 */ "sort_bnd_lst ::= sort_bnd",
 /* 368 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 369 */ "sort_bnd ::= sort_dcl_lst",
 /* 370 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 371 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 372 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 373 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 374 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 375 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 376 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 377 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 378 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 379 */ "show_lst ::= show_elem",
 /* 380 */ "show_lst ::= show_lst COMMA show_elem",
 /* 381 */ "show_lst ::= show_lst SEMICOLON show_elem",
 /* 382 */ "show_elem ::= atomic_formula_one_const",
 /* 383 */ "stmt_noconcurrency ::= NOCONCURRENCY PERIOD",
 /* 384 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD",
 /* 385 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_int_eval PERIOD",
 /* 386 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE DBL_COLON term_int_eval PERIOD",
 /* 387 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_int_eval PERIOD",
 /* 388 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE DBL_COLON term_int_eval PERIOD",
 /* 389 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 390 */ "query_lst ::= formula_temporal",
 /* 391 */ "query_lst ::= query_maxstep_decl",
 /* 392 */ "query_lst ::= query_label_decl",
 /* 393 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 394 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 395 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 396 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 397 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range_eval",
 /* 398 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 399 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 400 */ "clause_if ::= IF formula",
 /* 401 */ "clause_if ::=",
 /* 402 */ "clause_after ::= AFTER formula",
 /* 403 */ "clause_after ::=",
 /* 404 */ "clause_ifcons ::= IFCONS formula",
 /* 405 */ "clause_ifcons ::=",
 /* 406 */ "clause_unless ::= UNLESS atomic_formula_anon",
 /* 407 */ "clause_unless ::=",
 /* 408 */ "clause_where ::= WHERE formula_no_const",
 /* 409 */ "clause_where ::=",
 /* 410 */ "stmt_law ::= law_basic",
 /* 411 */ "stmt_law ::= law_caused",
 /* 412 */ "stmt_law ::= law_pcaused",
 /* 413 */ "stmt_law ::= law_impl",
 /* 414 */ "stmt_law ::= law_causes",
 /* 415 */ "stmt_law ::= law_increments",
 /* 416 */ "stmt_law ::= law_decrements",
 /* 417 */ "stmt_law ::= law_mcause",
 /* 418 */ "stmt_law ::= law_always",
 /* 419 */ "stmt_law ::= law_constraint",
 /* 420 */ "stmt_law ::= law_impossible",
 /* 421 */ "stmt_law ::= law_never",
 /* 422 */ "stmt_law ::= law_default",
 /* 423 */ "stmt_law ::= law_exogenous",
 /* 424 */ "stmt_law ::= law_inertial",
 /* 425 */ "stmt_law ::= law_nonexecutable",
 /* 426 */ "stmt_law ::= law_rigid",
 /* 427 */ "stmt_law ::= law_observed",
 /* 428 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 429 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 430 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 431 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 432 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 433 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 434 */ "law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 435 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 436 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 437 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 438 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 439 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 440 */ "law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 441 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 442 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 443 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 444 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 445 */ "law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD",
 /* 446 */ "stmt_code_blk ::= ASP_GR",
 /* 447 */ "stmt_code_blk ::= ASP_CP",
 /* 448 */ "stmt_code_blk ::= F2LP_GR",
 /* 449 */ "stmt_code_blk ::= F2LP_CP",
 /* 450 */ "stmt_code_blk ::= LUA_GR",
 /* 451 */ "stmt_code_blk ::= LUA_CP",
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
    case 64: /* TRUE */
    case 65: /* AT */
    case 66: /* BRACKET_L */
    case 67: /* BRACKET_R */
    case 68: /* COLON_DASH */
    case 69: /* CBRACKET_L */
    case 70: /* CBRACKET_R */
    case 71: /* PAREN_L */
    case 72: /* PAREN_R */
    case 73: /* PERIOD */
    case 74: /* MACRO_STRING */
    case 75: /* TILDE */
    case 76: /* DBL_COLON */
    case 77: /* ARROW_LEQ */
    case 78: /* ARROW_REQ */
    case 79: /* ARROW_LDASH */
    case 80: /* COLON */
    case 81: /* EQ */
    case 82: /* DBL_EQ */
    case 83: /* NEQ */
    case 84: /* NOT_EQ */
    case 85: /* LTHAN */
    case 86: /* GTHAN */
    case 87: /* LTHAN_EQ */
    case 88: /* GTHAN_EQ */
    case 89: /* DBL_PERIOD */
    case 90: /* BIG_CONJ */
    case 91: /* BIG_DISJ */
    case 92: /* POUND */
    case 93: /* SEMICOLON */
    case 94: /* EQUIV */
    case 95: /* IMPL */
    case 96: /* ARROW_RDASH */
    case 97: /* DBL_PLUS */
    case 98: /* PIPE */
    case 99: /* DBL_GTHAN */
    case 100: /* DBL_LTHAN */
    case 101: /* AMP */
    case 102: /* COMMA */
    case 103: /* DBL_AMP */
    case 104: /* NOT */
    case 105: /* DASH */
    case 106: /* PLUS */
    case 107: /* STAR */
    case 108: /* INT_DIV */
    case 109: /* MOD */
    case 110: /* ABS */
    case 111: /* CARROT */
    case 112: /* UMINUS */
    case 113: /* PREC_4 */
    case 114: /* PREC_3 */
    case 115: /* PREC_2 */
    case 116: /* PREC_1 */
    case 117: /* PREC_0 */
    case 118: /* EOF */
    case 119: /* ERR_IO */
    case 120: /* ERR_UNKNOWN_SYMBOL */
    case 121: /* ERR_UNTERMINATED_STRING */
    case 122: /* ERR_UNTERMINATED_ASP */
    case 123: /* ERR_UNTERMINATED_LUA */
    case 124: /* ERR_UNTERMINATED_F2LP */
    case 125: /* ERR_UNTERMINATED_BLK_COMMENT */
    case 126: /* ERR_SYNTAX */
    case 127: /* ERR_PAREN_MISMATCH */
    case 128: /* ARG */
    case 129: /* NOOP */
    case 130: /* CONSTANT_ID */
    case 131: /* VARIABLE_ID */
    case 132: /* OBJECT_ID */
    case 133: /* HIDE */
    case 134: /* OBSERVED */
{
#line 199 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));								
#line 2565 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 136: /* start */
    case 137: /* statement_lst */
    case 160: /* undeclared */
{
#line 209 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2574 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 138: /* statement */
    case 144: /* stmt_code_blk */
    case 145: /* stmt_law */
    case 146: /* stmt_show */
    case 147: /* stmt_hide */
    case 150: /* stmt_maxafvalue */
    case 151: /* stmt_maxadditive */
{
#line 213 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy484));								
#line 2587 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 139: /* stmt_macro_def */
{
#line 234 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy151));								
#line 2594 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_constant_def */
{
#line 236 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy431));								
#line 2601 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_object_def */
{
#line 238 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy468));								
#line 2608 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_variable_def */
{
#line 240 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy459));								
#line 2615 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_sort_def */
{
#line 242 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy61));								
#line 2622 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 148: /* stmt_noconcurrency */
{
#line 252 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy485));								
#line 2629 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_strong_noconcurrency */
{
#line 254 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy446));								
#line 2636 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 152: /* stmt_query */
{
#line 260 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy466));								
#line 2643 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* base_elem */
    case 154: /* base_elem_no_const */
    case 162: /* term */
    case 165: /* term_no_const */
    case 167: /* term_strong */
    case 168: /* term_strong_candidate */
    case 169: /* term_no_const_strong */
    case 172: /* term_integral */
    case 189: /* term_temporal */
    case 190: /* term_temporal_strong */
    case 191: /* term_temporal_strong_candidate */
    case 192: /* constant_temporal */
{
#line 294 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy299));								
#line 2661 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 155: /* constant */
    case 163: /* constant_one_const */
    case 166: /* const_anon */
{
#line 298 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy225));								
#line 2670 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* object */
    case 157: /* object_nullary */
{
#line 300 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy422));								
#line 2678 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 158: /* variable */
{
#line 304 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy189));								
#line 2685 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* lua */
{
#line 306 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy117));								
#line 2692 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 161: /* term_lst */
    case 164: /* term_no_const_lst */
{
#line 310 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy259));								
#line 2700 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 170: /* num_range */
{
#line 712 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy345));								
#line 2707 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* num_range_eval */
{
#line 714 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy29));								
#line 2714 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 173: /* term_int_eval */
{
#line 718 "bcplus/parser/detail/lemon_parser.y"
 /* Initially left Blank */				
#line 2721 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 174: /* formula */
    case 175: /* formula_base */
    case 176: /* comparison */
    case 179: /* formula_card */
    case 181: /* formula_no_const */
    case 182: /* formula_no_const_base */
    case 183: /* comparison_no_const */
    case 193: /* formula_temporal */
    case 194: /* formula_temporal_base */
    case 195: /* comparison_temporal */
    case 197: /* formula_temporal_card */
    case 198: /* head_formula */
{
#line 819 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy17));								
#line 2739 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 177: /* atomic_formula */
    case 180: /* atomic_formula_anon */
    case 184: /* atomic_formula_one_const */
    case 199: /* atomic_head_formula */
{
#line 825 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy423));								
#line 2749 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 178: /* formula_quant */
    case 196: /* formula_temporal_quant */
{
#line 827 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy177));								
#line 2757 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_lst */
{
#line 1001 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy221));								
#line 2764 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* quant_op */
{
#line 1003 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2771 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 187: /* card_var_lst */
    case 188: /* card_var_lst_inner */
{
#line 1040 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy375));								
#line 2779 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* formula_smpl_card */
{
#line 1346 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy133));								
#line 2786 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* macro_def_lst */
{
#line 1398 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy269));                              
#line 2793 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* macro_bnd */
{
#line 1400 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy359));                              
#line 2800 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* macro_args */
{
#line 1402 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy154));                              
#line 2807 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* macro_arg */
{
#line 1404 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy79));                              
#line 2814 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* sort_lst */
{
#line 1494 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy195));							
#line 2821 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 206: /* sort */
    case 207: /* sort_id_nr */
    case 208: /* sort_nr */
    case 209: /* sort_id */
{
#line 1496 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2831 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* constant_bnd_lst */
    case 211: /* constant_bnd */
{
#line 1605 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy409));									
#line 2839 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* constant_dcl_lst */
{
#line 1609 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy226));									
#line 2846 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* constant_dcl_type */
{
#line 1611 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2853 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* attrib_spec */
{
#line 1613 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2860 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* object_bnd_lst */
{
#line 1972 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy102));									
#line 2867 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* object_bnd */
{
#line 1974 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy294));									
#line 2874 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* object_lst */
    case 218: /* object_spec */
{
#line 1976 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy425));									
#line 2882 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* variable_bnd_lst */
    case 220: /* variable_bnd */
{
#line 2108 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy410));									
#line 2890 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 221: /* variable_lst */
{
#line 2112 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy52));									
#line 2897 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 222: /* sort_bnd_lst */
    case 223: /* sort_bnd */
    case 224: /* sort_dcl_lst */
{
#line 2195 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy196));									
#line 2906 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 225: /* show_lst */
{
#line 2299 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy123));									
#line 2913 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 226: /* show_elem */
    case 234: /* clause_unless */
{
#line 2301 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy423));									
#line 2921 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 227: /* query_lst */
{
#line 2453 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy489).l); DEALLOC((yypminor->yy489).maxstep); DEALLOC((yypminor->yy489).label);	
#line 2928 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 228: /* query_maxstep_decl */
{
#line 2455 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy29));												
#line 2935 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 230: /* query_label_Decl */
{
#line 2457 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2942 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 231: /* clause_if */
    case 232: /* clause_after */
    case 233: /* clause_ifcons */
    case 235: /* clause_where */
{
#line 2611 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy17));									
#line 2952 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 236: /* law_basic */
    case 237: /* law_caused */
    case 238: /* law_pcaused */
    case 239: /* law_impl */
    case 240: /* law_causes */
    case 241: /* law_increments */
    case 242: /* law_decrements */
    case 243: /* law_mcause */
    case 244: /* law_always */
    case 245: /* law_constraint */
    case 246: /* law_impossible */
    case 247: /* law_never */
    case 248: /* law_default */
    case 249: /* law_exogenous */
    case 250: /* law_inertial */
    case 251: /* law_nonexecutable */
    case 252: /* law_rigid */
    case 253: /* law_observed */
{
#line 2652 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy484));									
#line 2976 "bcplus/parser/detail/lemon_parser.c"
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
  { 136, 2 },
  { 137, 0 },
  { 137, 2 },
  { 137, 2 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 138, 1 },
  { 153, 1 },
  { 153, 1 },
  { 154, 1 },
  { 154, 1 },
  { 154, 1 },
  { 155, 4 },
  { 155, 1 },
  { 166, 1 },
  { 166, 4 },
  { 156, 4 },
  { 156, 1 },
  { 157, 1 },
  { 156, 1 },
  { 158, 1 },
  { 159, 4 },
  { 159, 1 },
  { 160, 4 },
  { 160, 1 },
  { 161, 1 },
  { 161, 3 },
  { 163, 4 },
  { 163, 1 },
  { 164, 1 },
  { 164, 3 },
  { 162, 1 },
  { 162, 1 },
  { 162, 1 },
  { 162, 3 },
  { 162, 1 },
  { 162, 1 },
  { 162, 1 },
  { 162, 1 },
  { 162, 1 },
  { 162, 2 },
  { 162, 2 },
  { 162, 3 },
  { 162, 3 },
  { 162, 3 },
  { 162, 3 },
  { 162, 3 },
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
  { 165, 1 },
  { 165, 1 },
  { 165, 1 },
  { 165, 3 },
  { 165, 1 },
  { 165, 1 },
  { 165, 1 },
  { 165, 1 },
  { 165, 1 },
  { 165, 1 },
  { 165, 2 },
  { 165, 2 },
  { 165, 3 },
  { 165, 3 },
  { 165, 3 },
  { 165, 3 },
  { 165, 3 },
  { 172, 1 },
  { 172, 3 },
  { 172, 1 },
  { 172, 1 },
  { 172, 1 },
  { 172, 1 },
  { 172, 1 },
  { 172, 2 },
  { 172, 2 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 172, 3 },
  { 170, 3 },
  { 171, 3 },
  { 173, 1 },
  { 173, 3 },
  { 173, 2 },
  { 173, 2 },
  { 173, 3 },
  { 173, 3 },
  { 173, 3 },
  { 173, 3 },
  { 173, 3 },
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
  { 180, 1 },
  { 180, 2 },
  { 180, 3 },
  { 181, 1 },
  { 181, 3 },
  { 181, 2 },
  { 181, 2 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 182, 1 },
  { 182, 1 },
  { 182, 1 },
  { 183, 3 },
  { 183, 3 },
  { 183, 3 },
  { 183, 3 },
  { 183, 3 },
  { 183, 3 },
  { 183, 3 },
  { 184, 1 },
  { 184, 2 },
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
  { 179, 3 },
  { 179, 4 },
  { 179, 4 },
  { 179, 5 },
  { 187, 2 },
  { 188, 1 },
  { 188, 3 },
  { 189, 1 },
  { 189, 1 },
  { 189, 1 },
  { 189, 3 },
  { 189, 1 },
  { 189, 1 },
  { 189, 1 },
  { 189, 1 },
  { 189, 1 },
  { 189, 1 },
  { 189, 2 },
  { 189, 2 },
  { 189, 3 },
  { 189, 3 },
  { 189, 3 },
  { 189, 3 },
  { 189, 3 },
  { 189, 3 },
  { 190, 1 },
  { 190, 1 },
  { 190, 1 },
  { 190, 3 },
  { 190, 1 },
  { 190, 1 },
  { 190, 1 },
  { 190, 3 },
  { 190, 2 },
  { 190, 2 },
  { 190, 3 },
  { 190, 3 },
  { 190, 3 },
  { 190, 3 },
  { 190, 3 },
  { 193, 1 },
  { 193, 3 },
  { 193, 2 },
  { 193, 2 },
  { 193, 3 },
  { 193, 3 },
  { 193, 3 },
  { 193, 3 },
  { 193, 3 },
  { 193, 3 },
  { 193, 3 },
  { 194, 1 },
  { 194, 1 },
  { 194, 1 },
  { 194, 1 },
  { 194, 1 },
  { 194, 1 },
  { 195, 3 },
  { 195, 3 },
  { 195, 3 },
  { 195, 3 },
  { 195, 3 },
  { 195, 3 },
  { 195, 3 },
  { 196, 5 },
  { 197, 4 },
  { 197, 5 },
  { 197, 5 },
  { 197, 6 },
  { 197, 3 },
  { 197, 4 },
  { 197, 4 },
  { 197, 5 },
  { 198, 3 },
  { 198, 1 },
  { 198, 1 },
  { 198, 1 },
  { 198, 1 },
  { 198, 1 },
  { 199, 1 },
  { 199, 2 },
  { 200, 4 },
  { 200, 5 },
  { 200, 5 },
  { 200, 6 },
  { 200, 3 },
  { 200, 4 },
  { 200, 4 },
  { 200, 5 },
  { 139, 4 },
  { 201, 1 },
  { 201, 3 },
  { 202, 6 },
  { 202, 3 },
  { 203, 1 },
  { 203, 3 },
  { 204, 1 },
  { 204, 1 },
  { 205, 1 },
  { 205, 3 },
  { 206, 1 },
  { 206, 2 },
  { 206, 2 },
  { 206, 3 },
  { 206, 3 },
  { 206, 3 },
  { 207, 1 },
  { 207, 1 },
  { 208, 1 },
  { 209, 1 },
  { 140, 4 },
  { 210, 1 },
  { 210, 3 },
  { 211, 6 },
  { 211, 3 },
  { 211, 3 },
  { 211, 5 },
  { 211, 8 },
  { 212, 1 },
  { 212, 4 },
  { 212, 3 },
  { 212, 6 },
  { 213, 1 },
  { 213, 1 },
  { 213, 1 },
  { 213, 1 },
  { 213, 1 },
  { 213, 1 },
  { 213, 1 },
  { 213, 1 },
  { 213, 1 },
  { 213, 1 },
  { 213, 1 },
  { 214, 1 },
  { 214, 4 },
  { 141, 4 },
  { 215, 1 },
  { 215, 3 },
  { 216, 3 },
  { 217, 1 },
  { 217, 3 },
  { 218, 1 },
  { 218, 4 },
  { 218, 1 },
  { 218, 1 },
  { 142, 4 },
  { 219, 1 },
  { 219, 3 },
  { 220, 3 },
  { 221, 1 },
  { 221, 3 },
  { 143, 4 },
  { 222, 1 },
  { 222, 3 },
  { 223, 1 },
  { 223, 3 },
  { 223, 3 },
  { 223, 3 },
  { 224, 1 },
  { 224, 3 },
  { 146, 4 },
  { 146, 4 },
  { 147, 4 },
  { 147, 4 },
  { 225, 1 },
  { 225, 3 },
  { 225, 3 },
  { 226, 1 },
  { 148, 2 },
  { 149, 2 },
  { 150, 5 },
  { 150, 5 },
  { 151, 5 },
  { 151, 5 },
  { 152, 4 },
  { 227, 1 },
  { 227, 1 },
  { 227, 1 },
  { 227, 3 },
  { 227, 3 },
  { 227, 3 },
  { 228, 3 },
  { 228, 3 },
  { 229, 3 },
  { 229, 3 },
  { 231, 2 },
  { 231, 0 },
  { 232, 2 },
  { 232, 0 },
  { 233, 2 },
  { 233, 0 },
  { 234, 2 },
  { 234, 0 },
  { 235, 2 },
  { 235, 0 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 145, 1 },
  { 236, 7 },
  { 237, 8 },
  { 238, 8 },
  { 239, 5 },
  { 240, 7 },
  { 241, 9 },
  { 242, 9 },
  { 243, 7 },
  { 244, 6 },
  { 245, 6 },
  { 246, 6 },
  { 247, 6 },
  { 248, 8 },
  { 249, 8 },
  { 250, 8 },
  { 251, 6 },
  { 252, 4 },
  { 253, 5 },
  { 144, 1 },
  { 144, 1 },
  { 144, 1 },
  { 144, 1 },
  { 144, 1 },
  { 144, 1 },
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
#line 215 "bcplus/parser/detail/lemon_parser.y"
{
  yy_destructor(yypParser,118,&yymsp[0].minor);
}
#line 3733 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 220 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy484;
			yymsp[0].minor.yy484  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3742 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy151; }
#line 3747 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy431; }
#line 3752 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy468; }
#line 3757 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy459; }
#line 3762 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy61; }
#line 3767 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 268 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy484; }
#line 3777 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy485; }
#line 3782 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 273 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy446; }
#line 3787 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 276 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy466; }
#line 3792 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy225; }
#line 3797 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 42: /* term ::= base_elem */ yytestcase(yyruleno==42);
      case 58: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==58);
      case 83: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==83);
      case 98: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==98);
      case 222: /* term_temporal ::= base_elem_no_const */ yytestcase(yyruleno==222);
      case 240: /* term_temporal_strong ::= base_elem_no_const */ yytestcase(yyruleno==240);
#line 323 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy299; }
#line 3808 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 325 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy422;	}
#line 3813 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 326 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy189; }
#line 3818 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 327 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy117; }
#line 3823 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 38: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==38);
#line 445 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy225, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3829 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 39: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==39);
#line 446 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy225, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3835 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* const_anon ::= IDENTIFIER */
#line 448 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy225, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true); }
#line 3840 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 449 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy225, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true);	}
#line 3845 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 452 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy422, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3850 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= object_nullary */
#line 453 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy422 = yymsp[0].minor.yy422; }
#line 3855 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* object_nullary ::= OBJECT_ID */
#line 454 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy422, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3860 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* object ::= undeclared */
#line 455 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3865 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* variable ::= VARIABLE_ID */
#line 458 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy189 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::VARIABLE)) {
			parser->_feature_error(Language::Feature::VARIABLE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			BASE_ELEM_BARE_DEF(yygotominor.yy189, yymsp[0].minor.yy0, Symbol::Type::VARIABLE, Variable, VariableSymbol); 
		}
	}
#line 3880 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 469 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy117, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0); }
#line 3885 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* lua ::= AT_IDENTIFIER */
#line 470 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy117, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3890 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 471 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy217, yymsp[-3].minor.yy0, yymsp[-1].minor.yy259);   yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 3897 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* undeclared ::= IDENTIFIER */
#line 472 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy217, yymsp[0].minor.yy0, NULL); }
#line 3902 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 36: /* term_lst ::= term */
      case 40: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==40);
#line 475 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy259 = new TermList();
			yygotominor.yy259->push_back(yymsp[0].minor.yy299);
		}
#line 3911 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 37: /* term_lst ::= term_lst COMMA term */
      case 41: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==41);
#line 481 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy259 = yymsp[-2].minor.yy259;
			yymsp[-2].minor.yy259->push_back(yymsp[0].minor.yy299);
		  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3921 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 43: /* term ::= INTEGER */
      case 59: /* term_strong ::= INTEGER */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==99);
      case 115: /* term_integral ::= INTEGER */ yytestcase(yyruleno==115);
      case 223: /* term_temporal ::= INTEGER */ yytestcase(yyruleno==223);
      case 241: /* term_temporal_strong ::= INTEGER */ yytestcase(yyruleno==241);
#line 580 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy299, yymsp[0].minor.yy0);	}
#line 3932 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 44: /* term ::= STRING_LITERAL */
      case 46: /* term ::= TRUE */ yytestcase(yyruleno==46);
      case 47: /* term ::= FALSE */ yytestcase(yyruleno==47);
      case 60: /* term_strong ::= STRING_LITERAL */ yytestcase(yyruleno==60);
      case 85: /* term_no_const_strong ::= STRING_LITERAL */ yytestcase(yyruleno==85);
      case 100: /* term_no_const ::= STRING_LITERAL */ yytestcase(yyruleno==100);
      case 102: /* term_no_const ::= TRUE */ yytestcase(yyruleno==102);
      case 103: /* term_no_const ::= FALSE */ yytestcase(yyruleno==103);
      case 117: /* term_integral ::= TRUE */ yytestcase(yyruleno==117);
      case 118: /* term_integral ::= FALSE */ yytestcase(yyruleno==118);
      case 224: /* term_temporal ::= STRING_LITERAL */ yytestcase(yyruleno==224);
      case 226: /* term_temporal ::= TRUE */ yytestcase(yyruleno==226);
      case 227: /* term_temporal ::= FALSE */ yytestcase(yyruleno==227);
      case 242: /* term_temporal_strong ::= STRING_LITERAL */ yytestcase(yyruleno==242);
#line 581 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy299, yymsp[0].minor.yy0); }
#line 3950 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 45: /* term ::= PAREN_L term PAREN_R */
      case 61: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==61);
      case 86: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==86);
      case 101: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==101);
      case 116: /* term_integral ::= PAREN_L term_integral PAREN_R */ yytestcase(yyruleno==116);
      case 225: /* term_temporal ::= PAREN_L term_temporal PAREN_R */ yytestcase(yyruleno==225);
      case 243: /* term_temporal_strong ::= PAREN_L term_temporal_strong PAREN_R */ yytestcase(yyruleno==243);
#line 582 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy299, yymsp[-2].minor.yy0, yymsp[-1].minor.yy299, yymsp[0].minor.yy0); }
#line 3961 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 48: /* term ::= MAXSTEP */
      case 62: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==87);
      case 104: /* term_no_const ::= MAXSTEP */ yytestcase(yyruleno==104);
      case 119: /* term_integral ::= MAXSTEP */ yytestcase(yyruleno==119);
      case 228: /* term_temporal ::= MAXSTEP */ yytestcase(yyruleno==228);
      case 244: /* term_temporal_strong ::= MAXSTEP */ yytestcase(yyruleno==244);
#line 585 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy299, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3972 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= MAXADDITIVE */
      case 63: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==63);
      case 88: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==88);
      case 105: /* term_no_const ::= MAXADDITIVE */ yytestcase(yyruleno==105);
      case 120: /* term_integral ::= MAXADDITIVE */ yytestcase(yyruleno==120);
      case 229: /* term_temporal ::= MAXADDITIVE */ yytestcase(yyruleno==229);
      case 245: /* term_temporal_strong ::= MAXADDITIVE */ yytestcase(yyruleno==245);
#line 586 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy299, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3983 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= MAXAFVALUE */
      case 64: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==64);
      case 89: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==89);
      case 106: /* term_no_const ::= MAXAFVALUE */ yytestcase(yyruleno==106);
      case 121: /* term_integral ::= MAXAFVALUE */ yytestcase(yyruleno==121);
      case 230: /* term_temporal ::= MAXAFVALUE */ yytestcase(yyruleno==230);
      case 246: /* term_temporal_strong ::= MAXAFVALUE */ yytestcase(yyruleno==246);
#line 587 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy299, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3994 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= DASH term */
      case 65: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==65);
      case 91: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==91);
      case 108: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==108);
      case 122: /* term_integral ::= DASH term_integral */ yytestcase(yyruleno==122);
      case 232: /* term_temporal ::= DASH term_temporal */ yytestcase(yyruleno==232);
      case 248: /* term_temporal_strong ::= DASH term_temporal_strong */ yytestcase(yyruleno==248);
#line 591 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, UnaryTerm::Operator::NEGATIVE); }
#line 4005 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= ABS term */
      case 66: /* term_strong ::= ABS term */ yytestcase(yyruleno==66);
      case 92: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==92);
      case 109: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==109);
      case 123: /* term_integral ::= ABS term_integral */ yytestcase(yyruleno==123);
      case 233: /* term_temporal ::= ABS term_temporal */ yytestcase(yyruleno==233);
      case 249: /* term_temporal_strong ::= ABS term_temporal */ yytestcase(yyruleno==249);
#line 592 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, UnaryTerm::Operator::ABS); }
#line 4016 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= term DASH term */
      case 68: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==68);
      case 78: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==78);
      case 93: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==93);
      case 110: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==110);
      case 124: /* term_integral ::= term_integral DASH term_integral */ yytestcase(yyruleno==124);
      case 235: /* term_temporal ::= term_temporal DASH term_temporal */ yytestcase(yyruleno==235);
      case 250: /* term_temporal_strong ::= term_temporal_strong DASH term_temporal */ yytestcase(yyruleno==250);
#line 596 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MINUS); }
#line 4028 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= term PLUS term */
      case 69: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==69);
      case 79: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==79);
      case 94: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==94);
      case 111: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==111);
      case 125: /* term_integral ::= term_integral PLUS term_integral */ yytestcase(yyruleno==125);
      case 236: /* term_temporal ::= term_temporal PLUS term_temporal */ yytestcase(yyruleno==236);
      case 251: /* term_temporal_strong ::= term_temporal_strong PLUS term_temporal */ yytestcase(yyruleno==251);
#line 597 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::PLUS); }
#line 4040 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= term STAR term */
      case 70: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==70);
      case 80: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==80);
      case 95: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==95);
      case 112: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==112);
      case 126: /* term_integral ::= term_integral STAR term_integral */ yytestcase(yyruleno==126);
      case 237: /* term_temporal ::= term_temporal STAR term_temporal */ yytestcase(yyruleno==237);
      case 252: /* term_temporal_strong ::= term_temporal_strong STAR term_temporal */ yytestcase(yyruleno==252);
#line 598 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::TIMES); }
#line 4052 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 56: /* term ::= term INT_DIV term */
      case 71: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==71);
      case 81: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==81);
      case 96: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==96);
      case 113: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==113);
      case 127: /* term_integral ::= term_integral INT_DIV term_integral */ yytestcase(yyruleno==127);
      case 238: /* term_temporal ::= term_temporal INT_DIV term_temporal */ yytestcase(yyruleno==238);
      case 253: /* term_temporal_strong ::= term_temporal_strong INT_DIV term_temporal */ yytestcase(yyruleno==253);
#line 599 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::DIVIDE); }
#line 4064 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 57: /* term ::= term MOD term */
      case 72: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==72);
      case 82: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==82);
      case 97: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==97);
      case 114: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==114);
      case 128: /* term_integral ::= term_integral MOD term_integral */ yytestcase(yyruleno==128);
      case 239: /* term_temporal ::= term_temporal MOD term_temporal */ yytestcase(yyruleno==239);
      case 254: /* term_temporal_strong ::= term_temporal_strong MOD term_temporal */ yytestcase(yyruleno==254);
#line 600 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MOD); }
#line 4076 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 67: /* term_strong_candidate ::= DASH constant */
#line 619 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy225, UnaryTerm::Operator::NEGATIVE); }
#line 4081 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 73: /* term_strong ::= constant DASH term */
#line 628 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MINUS); }
#line 4086 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant PLUS term */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::PLUS); }
#line 4091 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant STAR term */
#line 630 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::TIMES); }
#line 4096 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 76: /* term_strong ::= constant INT_DIV term */
#line 631 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::DIVIDE); }
#line 4101 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong ::= constant MOD term */
#line 632 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MOD); }
#line 4106 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 90: /* term_no_const_strong ::= constant */
#line 654 "bcplus/parser/detail/lemon_parser.y"
{
		// error handling for constants so they don'yygotominor.yy299 default to undeclared identifiers
		yygotominor.yy299 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy225;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy225->beginLoc());
		YYERROR;
	}
#line 4117 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 107: /* term_no_const ::= constant */
#line 687 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy299 default to undeclared identifiers
		yygotominor.yy299 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy225;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy225->beginLoc());
		YYERROR;
	}
#line 4128 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 129: /* num_range ::= term_integral DBL_PERIOD term_integral */
#line 744 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy299, r_ptr = yymsp[0].minor.yy299, s_ptr = yymsp[-1].minor.yy0;
	yygotominor.yy345 = NULL;

	if (yymsp[-2].minor.yy299->domainType() != DomainType::INTEGRAL) {
		parser->_parse_error("Number ranges cannot have non-numeric operands.", &yymsp[-1].minor.yy0->beginLoc());
		YYERROR;
	}
	
	if (yymsp[0].minor.yy299->domainType() != DomainType::INTEGRAL) {
		parser->_parse_error("Number ranges cannot have non-numeric operands.", &yymsp[0].minor.yy299->beginLoc());
		YYERROR;
	}

	yygotominor.yy345 = new NumberRange(yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());
}
#line 4148 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* num_range_eval ::= term_int_eval DBL_PERIOD term_int_eval */
#line 762 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy396, r_ptr = yymsp[0].minor.yy396, s_ptr = yymsp[-1].minor.yy0;
	yygotominor.yy29 = new NumberRangeEval(yymsp[-2].minor.yy396->val(), yymsp[0].minor.yy396->val(), yymsp[-2].minor.yy396->beginLoc(), yymsp[0].minor.yy396->endLoc());
}
#line 4156 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 131: /* term_int_eval ::= INTEGER */
#line 768 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> i_ptr = yymsp[0].minor.yy0;

	yygotominor.yy396 = 0;
	try {
		yygotominor.yy396 = new Number(boost::lexical_cast<int>(*yymsp[0].minor.yy0->str()), yymsp[0].minor.yy0->beginLoc());
	} catch (boost::bad_lexical_cast const& e) {
	parser->_parse_error("INTERNAL ERROR: Failed to parse integer \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
		YYERROR;
	}
}
#line 4171 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* term_int_eval ::= PAREN_L term_int_eval PAREN_R */
#line 780 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy396 = yymsp[-1].minor.yy396;
	yygotominor.yy396->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy396->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 4181 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* term_int_eval ::= DASH term_int_eval */
#line 800 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy396, yymsp[0].minor.yy396, -1 * yymsp[0].minor.yy396->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4187 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* term_int_eval ::= ABS term_int_eval */
#line 801 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy396, yymsp[0].minor.yy396, yymsp[0].minor.yy396->val() < 0 ? - yymsp[0].minor.yy396->val() : yymsp[0].minor.yy396->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 4193 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* term_int_eval ::= term_int_eval DASH term_int_eval */
#line 803 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() - yymsp[0].minor.yy396->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4199 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* term_int_eval ::= term_int_eval PLUS term_int_eval */
#line 804 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() + yymsp[0].minor.yy396->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 4205 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* term_int_eval ::= term_int_eval STAR term_int_eval */
#line 805 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() * yymsp[0].minor.yy396->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 4211 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* term_int_eval ::= term_int_eval INT_DIV term_int_eval */
#line 806 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() / yymsp[0].minor.yy396->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 4217 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* term_int_eval ::= term_int_eval MOD term_int_eval */
#line 807 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() % yymsp[0].minor.yy396->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 4223 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* formula ::= formula_base */
      case 183: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==183);
      case 255: /* formula_temporal ::= formula_temporal_base */ yytestcase(yyruleno==255);
#line 867 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy17;				}
#line 4230 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* formula ::= PAREN_L formula PAREN_R */
      case 184: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==184);
      case 256: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==256);
#line 868 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[-1].minor.yy17; yygotominor.yy17->parens(true); 	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4239 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* formula ::= NOT formula */
      case 185: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==185);
      case 257: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==257);
#line 869 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 4246 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* formula ::= DASH formula */
      case 186: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==186);
      case 258: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==258);
#line 870 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 4253 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 144: /* formula ::= formula AMP formula */
      case 187: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==187);
      case 259: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==259);
#line 871 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy17, yymsp[0].minor.yy17, yymsp[-2].minor.yy17->beginLoc(), yymsp[0].minor.yy17->endLoc());   yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 4261 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 145: /* formula ::= formula DBL_PLUS formula */
      case 146: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==146);
      case 188: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==188);
      case 189: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==189);
      case 260: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==260);
      case 261: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==261);
#line 872 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::OR); }
#line 4271 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 147: /* formula ::= formula EQUIV formula */
      case 190: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==190);
      case 262: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==262);
#line 874 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::EQUIV); }
#line 4278 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 148: /* formula ::= formula IMPL formula */
      case 149: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==149);
      case 191: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==191);
      case 192: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==192);
      case 263: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==263);
      case 264: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==264);
#line 875 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::IMPL); }
#line 4288 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 150: /* formula_base ::= comparison */
      case 193: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==193);
      case 266: /* formula_temporal_base ::= comparison_temporal */ yytestcase(yyruleno==266);
      case 289: /* head_formula ::= comparison */ yytestcase(yyruleno==289);
#line 878 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy17; }
#line 4296 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 151: /* formula_base ::= atomic_formula */
      case 290: /* head_formula ::= atomic_head_formula */ yytestcase(yyruleno==290);
#line 879 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy423; }
#line 4302 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* formula_base ::= formula_quant */
      case 268: /* formula_temporal_base ::= formula_temporal_quant */ yytestcase(yyruleno==268);
#line 880 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy177; }
#line 4308 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* formula_base ::= formula_card */
      case 269: /* formula_temporal_base ::= formula_temporal_card */ yytestcase(yyruleno==269);
#line 882 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy17 = yymsp[0].minor.yy17;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy17->beginLoc());
			YYERROR;
		}
	}
#line 4320 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* formula_base ::= TRUE */
      case 194: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==194);
      case 270: /* formula_temporal_base ::= TRUE */ yytestcase(yyruleno==270);
      case 292: /* head_formula ::= TRUE */ yytestcase(yyruleno==292);
#line 889 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4328 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* formula_base ::= FALSE */
      case 195: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==195);
      case 271: /* formula_temporal_base ::= FALSE */ yytestcase(yyruleno==271);
      case 293: /* head_formula ::= FALSE */ yytestcase(yyruleno==293);
#line 890 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4336 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* comparison ::= term_strong EQ term */
      case 163: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==163);
      case 196: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==196);
      case 272: /* comparison_temporal ::= term_temporal_strong EQ term_temporal */ yytestcase(yyruleno==272);
#line 892 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4345 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= term_strong DBL_EQ term */
      case 164: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==164);
      case 197: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==197);
      case 273: /* comparison_temporal ::= term_temporal_strong DBL_EQ term_temporal */ yytestcase(yyruleno==273);
#line 893 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4354 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* comparison ::= term_strong NEQ term */
      case 165: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==165);
      case 198: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==198);
      case 274: /* comparison_temporal ::= term_temporal_strong NEQ term_temporal */ yytestcase(yyruleno==274);
#line 894 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4363 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* comparison ::= term_strong LTHAN term */
      case 166: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==166);
      case 199: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==199);
      case 275: /* comparison_temporal ::= term_temporal_strong LTHAN term_temporal */ yytestcase(yyruleno==275);
#line 895 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4372 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* comparison ::= term_strong GTHAN term */
      case 167: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==167);
      case 200: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==200);
      case 276: /* comparison_temporal ::= term_temporal_strong GTHAN term_temporal */ yytestcase(yyruleno==276);
#line 896 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4381 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 161: /* comparison ::= term_strong LTHAN_EQ term */
      case 168: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==168);
      case 201: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==201);
      case 277: /* comparison_temporal ::= term_temporal_strong LTHAN_EQ term_temporal */ yytestcase(yyruleno==277);
#line 897 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4390 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 162: /* comparison ::= term_strong GTHAN_EQ term */
      case 169: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==169);
      case 202: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==202);
      case 278: /* comparison_temporal ::= term_temporal_strong GTHAN_EQ term_temporal */ yytestcase(yyruleno==278);
#line 898 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4399 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 170: /* comparison ::= constant DBL_EQ term */
#line 906 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4405 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 171: /* comparison ::= constant NEQ term */
#line 907 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4411 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 172: /* comparison ::= constant LTHAN term */
#line 908 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4417 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 173: /* comparison ::= constant GTHAN term */
#line 909 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4423 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 174: /* comparison ::= constant LTHAN_EQ term */
#line 910 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4429 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 175: /* comparison ::= constant GTHAN_EQ term */
#line 911 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4435 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 176: /* atomic_formula ::= constant */
      case 180: /* atomic_formula_anon ::= const_anon */ yytestcase(yyruleno==180);
      case 203: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==203);
#line 938 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy423, yymsp[0].minor.yy225, true); }
#line 4442 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 177: /* atomic_formula ::= TILDE constant */
      case 181: /* atomic_formula_anon ::= TILDE const_anon */ yytestcase(yyruleno==181);
      case 204: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==204);
#line 939 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy423, yymsp[0].minor.yy225, false);   yy_destructor(yypParser,75,&yymsp[-1].minor);
}
#line 4450 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 178: /* atomic_formula ::= constant EQ term */
      case 182: /* atomic_formula_anon ::= const_anon EQ term */ yytestcase(yyruleno==182);
      case 205: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==205);
#line 940 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = new AtomicFormula(yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());	  yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4458 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 179: /* atomic_formula_anon ::= atomic_formula */
      case 294: /* atomic_head_formula ::= atomic_formula */ yytestcase(yyruleno==294);
      case 382: /* show_elem ::= atomic_formula_one_const */ yytestcase(yyruleno==382);
#line 942 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = yymsp[0].minor.yy423; }
#line 4465 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 206: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
      case 279: /* formula_temporal_quant ::= BRACKET_L quant_lst PIPE formula_temporal BRACKET_R */ yytestcase(yyruleno==279);
#line 1006 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy177=NULL;
		ref_ptr<const Token> bl_ptr = yymsp[-4].minor.yy0;
		ref_ptr<QuantifierFormula::QuantifierList> lst_ptr = yymsp[-3].minor.yy221;
		ref_ptr<Formula> sub_ptr = yymsp[-1].minor.yy17;
		ref_ptr<const Token> br_ptr = yymsp[0].minor.yy0;

		if (!parser->lang()->support(Language::Feature::FORMULA_QUANTIFIER)) {
			parser->_feature_error(Language::Feature::FORMULA_QUANTIFIER, &yymsp[-4].minor.yy0->beginLoc());
			YYERROR;
		} else yygotominor.yy177 = new QuantifierFormula(yymsp[-3].minor.yy221, yymsp[-1].minor.yy17, yymsp[-4].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
	  yy_destructor(yypParser,98,&yymsp[-2].minor);
}
#line 4483 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 207: /* quant_lst ::= quant_op variable */
#line 1020 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy221 = new QuantifierFormula::QuantifierList();
		yygotominor.yy221->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy497, yymsp[0].minor.yy189));
	}
#line 4491 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* quant_lst ::= quant_lst quant_op variable */
#line 1026 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy221 = yymsp[-2].minor.yy221;
		yygotominor.yy221->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy497, yymsp[0].minor.yy189));
	}
#line 4499 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 209: /* quant_op ::= BIG_CONJ */
#line 1031 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy497 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,90,&yymsp[0].minor);
}
#line 4505 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* quant_op ::= BIG_DISJ */
#line 1032 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy497 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4511 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
      case 280: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==280);
#line 1078 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, NULL);  }
#line 4517 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
      case 281: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==281);
#line 1079 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy17,  yymsp[0].minor.yy0, NULL);  }
#line 4523 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 282: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==282);
#line 1080 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4529 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 283: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==283);
#line 1081 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-5].minor.yy299, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy17,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4535 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 215: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
      case 284: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==284);
#line 1082 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, NULL);  }
#line 4541 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
      case 285: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==285);
#line 1083 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-3].minor.yy299, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy17,  yymsp[0].minor.yy0, NULL);  }
#line 4547 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
      case 286: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==286);
#line 1084 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4553 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 218: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
      case 287: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==287);
#line 1085 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy17,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4559 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 219: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1089 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy375 = yymsp[-1].minor.yy375;
	  yy_destructor(yypParser,98,&yymsp[0].minor);
}
#line 4567 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 220: /* card_var_lst_inner ::= variable */
#line 1094 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy189;
		yygotominor.yy375 = new CardinalityFormula::VariableList();
		yygotominor.yy375->push_back(yymsp[0].minor.yy189->symbol());
	}
#line 4576 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 221: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1101 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy189;
		yygotominor.yy375 = yymsp[-2].minor.yy375;
		yygotominor.yy375->push_back(yymsp[0].minor.yy189->symbol());
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4586 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 231: /* term_temporal ::= constant */
#line 1155 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy299 default to undeclared identifiers
		yygotominor.yy299 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy225;
		parser->_parse_error("All constant symbols must be bound to a step using the i:F notation.", &yymsp[0].minor.yy225->beginLoc());
		YYERROR;
	}
#line 4597 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 234: /* term_temporal ::= term_temporal COLON term */
      case 247: /* term_temporal_strong ::= term_temporal_strong COLON term_strong */ yytestcase(yyruleno==247);
#line 1167 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BindingTerm); }
#line 4603 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* formula_temporal ::= term_temporal_strong COLON formula */
#line 1248 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy17, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BindingFormula); }
#line 4608 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 267: /* formula_temporal_base ::= atomic_formula */
#line 1254 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for more useful error messages
		yygotominor.yy17 = NULL;
		ref_ptr<const Referenced> l_ptr = yymsp[0].minor.yy423;
		parser->_parse_error("All constant symbols must be bound to a step using the i:F notation.", &yymsp[0].minor.yy423->beginLoc());
		YYERROR;
	}
#line 4619 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* head_formula ::= head_formula AMP head_formula */
#line 1349 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy17 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy17, yymsp[0].minor.yy17, yymsp[-2].minor.yy17->beginLoc(), yymsp[0].minor.yy17->endLoc());
	  yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 4627 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* head_formula ::= formula_smpl_card */
#line 1356 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy17 = yymsp[0].minor.yy133;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy133->beginLoc());
			YYERROR;
		}
	}
#line 4638 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* atomic_head_formula ::= DASH constant */
#line 1369 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy423 = NULL;
		ref_ptr<const Token> d_ptr = yymsp[-1].minor.yy0;
		ref_ptr<Constant> c_ptr = yymsp[0].minor.yy225;

		if (!parser->lang()->support(Language::Feature::FORMULA_NOT_DASH_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_NOT_DASH_HEAD);
			YYERROR;
		} else {
			ATOMIC_FORMULA(yygotominor.yy423, yymsp[0].minor.yy225, false); 
		}
	}
#line 4654 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1382 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy423, yymsp[0].minor.yy0, NULL);  }
#line 4659 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1383 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy423,  yymsp[0].minor.yy0, NULL);  }
#line 4664 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1384 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4669 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1385 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-5].minor.yy299, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy423,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4674 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1386 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy423, yymsp[0].minor.yy0, NULL);  }
#line 4679 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1387 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-3].minor.yy299, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy423,  yymsp[0].minor.yy0, NULL);  }
#line 4684 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1388 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4689 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1389 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy423,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4694 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1408 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy151 = NULL;
        ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
        ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
        ref_ptr<MacroDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy269;
        ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;

        if (!parser->lang()->support(Language::Feature::DECL_MACRO)) {
            parser->_feature_error(Language::Feature::DECL_MACRO, &yymsp[-2].minor.yy0->beginLoc());
            YYERROR;
        } else {
		    BOOST_FOREACH(symbols::MacroSymbol* m, *yymsp[-1].minor.yy269) {
			    if (!parser->symtab()->create(m)) {
	    	        // Check if it's a duplicate
	    	        symbols::MacroSymbol* m2 = (symbols::MacroSymbol*)parser->symtab()->resolve(symbols::Symbol::Type::MACRO, *m->base(), m->arity());
		            if (!m2 || m2 != m) {
		                parser->_parse_error("Detected conflicting definition of symbol \"" + *m->name() + "\".", &yygotominor.yy151->beginLoc());
		            } else {
		                parser->_parse_error("Detected a duplicate definition of symbol \"" + *m->name() + "\".", &yygotominor.yy151->beginLoc());
		            }
		        }
		    }

			yygotominor.yy151 = new MacroDeclaration(yymsp[-1].minor.yy269, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
        }
    }
#line 4724 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* macro_def_lst ::= macro_bnd */
#line 1436 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy269 = new MacroDeclaration::ElementList();
        yygotominor.yy269->push_back(yymsp[0].minor.yy359);
    }
#line 4732 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1442 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy269 = yymsp[-2].minor.yy269;
        yygotominor.yy269->push_back(yymsp[0].minor.yy359);
      yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4741 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1448 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy154;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy359 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy154);
      yy_destructor(yypParser,71,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4755 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1457 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy359 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4766 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* macro_args ::= macro_arg */
#line 1465 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy154 = new MacroSymbol::ArgumentList();
        yygotominor.yy154->push_back(yymsp[0].minor.yy79->str());
        delete yymsp[0].minor.yy79;
    }
#line 4775 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* macro_args ::= macro_args COMMA macro_arg */
#line 1471 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy154 = yymsp[-2].minor.yy154;
        yygotominor.yy154->push_back(yymsp[0].minor.yy79->str());
        delete yymsp[0].minor.yy79;
      yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4785 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* macro_arg ::= POUND_INTEGER */
      case 312: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==312);
#line 1478 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy79 = yymsp[0].minor.yy0;
    }
#line 4793 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* sort_lst ::= sort */
#line 1505 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy195 = new ConstantSymbol::SortList();
		yygotominor.yy195->push_back(yymsp[0].minor.yy93);
	}
#line 4801 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* sort_lst ::= sort_lst COMMA sort */
#line 1510 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy195 = yymsp[-2].minor.yy195;
		yygotominor.yy195->push_back(yymsp[0].minor.yy93);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4810 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* sort ::= sort_id_nr */
      case 321: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==321);
      case 322: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==322);
#line 1535 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = yymsp[0].minor.yy93; }
#line 4817 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* sort ::= sort_id_nr STAR */
#line 1536 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::NONE)); }
#line 4822 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* sort ::= sort_id_nr CARROT */
#line 1537 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::UNKNOWN)); }
#line 4827 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* sort ::= sort PLUS object_nullary */
#line 1539 "bcplus/parser/detail/lemon_parser.y"
{ u::ref_ptr<const Object> o_ptr = yymsp[0].minor.yy422; DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, yymsp[0].minor.yy422->symbol()); }
#line 4832 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* sort ::= sort PLUS IDENTIFIER */
#line 1542 "bcplus/parser/detail/lemon_parser.y"
{
												  u::ref_ptr<const Referenced> s_ptr = yymsp[-2].minor.yy93, op_ptr = yymsp[-1].minor.yy0, id_ptr = yymsp[0].minor.yy0;
												  u::ref_ptr<const ObjectSymbol> obj = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
												  if(!obj) {
													if (parser->lang()->support(Language::Feature::SORT_PLUS)) 
														parser->_parse_error("\"" + *yymsp[0].minor.yy0->str() + "\" could not be declared as an object as this conflicts with a previous declarations of this identifier.", &yymsp[0].minor.yy0->beginLoc());
													else 
														parser->_feature_error(Language::Feature::SORT_PLUS, &yymsp[-1].minor.yy0->beginLoc());
													YYERROR;
												  } else {
													DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, obj);
												  }
												}
#line 4849 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 320: /* sort ::= sort PLUS INTEGER */
#line 1556 "bcplus/parser/detail/lemon_parser.y"
{ 
												  ref_ptr<const Object> t_ptr;
												  BASIC_TERM(t_ptr, yymsp[0].minor.yy0);
												  DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, t_ptr->symbol()); 
												}
#line 4858 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 323: /* sort_nr ::= num_range */
#line 1567 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy345;

		yygotominor.yy93 = NULL;

		if (!parser->lang()->support(Language::Feature::NUMRANGE_SORT)) {
			parser->_feature_error(Language::Feature::NUMRANGE_SORT, &yymsp[0].minor.yy345->beginLoc());
			YYERROR;
		}

		// X..Y becomes __rsort_N_
		if(!(yygotominor.yy93 = parser->_newRange(yymsp[0].minor.yy345->min(), yymsp[0].minor.yy345->max()))) {
			parser->_parse_error("INTERNAL ERROR: An error occurred while instantiating the dynamic sort declaration.", &yymsp[0].minor.yy345->beginLoc());
			YYERROR;
		}
	}
#line 4878 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 324: /* sort_id ::= IDENTIFIER */
#line 1585 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy93 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy93) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4891 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 325: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1616 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ConstantDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy409;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_CONSTANT)) {
			yygotominor.yy431 = NULL;
			parser->_feature_error(Language::Feature::DECL_CONSTANT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy431 = new ConstantDeclaration(yymsp[-1].minor.yy409, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

		}
	}
#line 4910 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 326: /* constant_bnd_lst ::= constant_bnd */
#line 1633 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy409 = yymsp[0].minor.yy409;
	}
#line 4917 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 327: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1638 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy409;
		yygotominor.yy409 = yymsp[-2].minor.yy409;
		yygotominor.yy409->splice(yygotominor.yy409->end(), *yymsp[0].minor.yy409);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4927 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 328: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1658 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const SortSymbol> s_ptr = yymsp[-1].minor.yy93;
		ref_ptr<const Referenced> names_ptr = yymsp[-5].minor.yy226;
		yygotominor.yy409 = new ConstantDeclaration::ElementList();

		// NOTE: additive constants default to the additive sort, not the boolean sort
		if (yymsp[-3].minor.yy265 & ConstantSymbol::Type::M_ADDITIVE) s_ptr = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

		// external constants should have "unknown" in their sort
		else if (yymsp[-3].minor.yy265 & ConstantSymbol::Type::M_EXTERNAL) s_ptr = parser->symtab()->carrot(yymsp[-1].minor.yy93);

		// non-boolean abActions should contain "none"
		else if (yymsp[-3].minor.yy265 == ConstantSymbol::Type::ABACTION && s_ptr->domainType() != DomainType::BOOLEAN) s_ptr = parser->symtab()->star(yymsp[-1].minor.yy93);

		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-5].minor.yy226) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[-3].minor.yy265, decl.first->str(), s_ptr, decl.second);
			yygotominor.yy409->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4955 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1680 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy226, s_ptr = yymsp[0].minor.yy93;
		yygotominor.yy409 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy226) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(ConstantSymbol::Type::RIGID, decl.first->str(), yymsp[0].minor.yy93, decl.second);
			yygotominor.yy409->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 4970 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 330: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1691 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy226;
		yygotominor.yy409 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy226) {
			// attempt to declare each symbol
			ref_ptr<SortSymbol> s = parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN);

			// NOTE: additive constants default to the additive sort, not the boolean sort
			if (yymsp[0].minor.yy265 & ConstantSymbol::Type::M_ADDITIVE && s->domainType() == DomainType::BOOLEAN ) 
				s = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

			// external constants should have "unknown" in their sort
			else if (yymsp[0].minor.yy265 & ConstantSymbol::Type::M_EXTERNAL) 
				s = parser->symtab()->carrot(s);

			// non-boolean abActions should contain "none"
			else if (yymsp[0].minor.yy265 == ConstantSymbol::Type::ABACTION && s->domainType() != DomainType::BOOLEAN) 
				s = parser->symtab()->star(s);


			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[0].minor.yy265, decl.first->str(), parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN), decl.second);
			yygotominor.yy409->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5000 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 331: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1717 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy409 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-4].minor.yy226, s_ptr = yymsp[-2].minor.yy60, id_ptr = yymsp[0].minor.yy0;


		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[0].minor.yy0->str());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\" is not a valid constant symbol.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy409 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-4].minor.yy226) {
				ref_ptr<ConstantSymbol> c= new AttributeSymbol(decl.first->str(), yymsp[-2].minor.yy60, c, decl.second);
				yygotominor.yy409->push_back(c);
				CONSTANT_DECL(c, decl.first->beginLoc());
			}
		}
	  yy_destructor(yypParser,76,&yymsp[-3].minor);
  yy_destructor(yypParser,55,&yymsp[-1].minor);
}
#line 5029 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 332: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1741 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy409 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-7].minor.yy226, s_ptr = yymsp[-5].minor.yy60, id_ptr = yymsp[-3].minor.yy0, lst_ptr = yymsp[-1].minor.yy195;

		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy195->size());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy195->size()) + "\" is not a valid constant symbol.", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy195->size()) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// ensure that the sorts match the declaration of the symbol
			SortList::const_iterator it = yymsp[-1].minor.yy195->begin();
			BOOST_FOREACH(SortSymbol const* sort, *c) {
				if (*it != sort) {
					// check to see if it'yymsp[-5].minor.yy60 a subsort, which is also permissable
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

			yygotominor.yy409 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-7].minor.yy226) {
				// ensure that the sorts match the start of the sort list for each of the symbols
				if (decl.second->size() < yymsp[-1].minor.yy195->size()) {
					parser->_parse_error("Detected a malformed attribute declaration. An attribute must duplicate its parent'yymsp[-5].minor.yy60 parameters.", &decl.first->beginLoc());
					YYERROR;
				} else {
					bool good_sort = true;		
					it = decl.second->begin();
					BOOST_FOREACH(SortSymbol const* sort, *yymsp[-1].minor.yy195) {
						if (*it != sort) {
							// check to see if it'yymsp[-5].minor.yy60 a subsort, which is also permissable
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
						ref_ptr<ConstantSymbol> sym = new AttributeSymbol(decl.first->str(), yymsp[-5].minor.yy60, c, decl.second);
						yygotominor.yy409->push_back(sym);
						CONSTANT_DECL(sym, decl.first->beginLoc());

					}
				}
			}
		}
	  yy_destructor(yypParser,76,&yymsp[-6].minor);
  yy_destructor(yypParser,55,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5110 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 333: /* constant_dcl_lst ::= IDENTIFIER */
#line 1817 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = new IdentifierDeclList();
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 5118 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 334: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1822 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = new IdentifierDeclList();
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy195));
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5128 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 335: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1827 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = yymsp[-2].minor.yy226;
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5137 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 336: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1832 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = yymsp[-5].minor.yy226;
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy195));
	  yy_destructor(yypParser,102,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5148 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 337: /* constant_dcl_type ::= ABACTION */
#line 1839 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5160 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 338: /* constant_dcl_type ::= ACTION */
#line 1848 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5172 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 339: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1857 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5184 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 340: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1866 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5196 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 341: /* constant_dcl_type ::= EXTERNALACTION */
#line 1875 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5208 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 342: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1884 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5220 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 343: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1893 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5232 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 344: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1902 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5244 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 345: /* constant_dcl_type ::= RIGID */
#line 1911 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5256 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1920 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5268 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* constant_dcl_type ::= SDFLUENT */
#line 1930 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5280 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 348: /* attrib_spec ::= ATTRIBUTE */
#line 1940 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy60 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[0].minor.yy0;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// grab the boolean sort and provide it
			yygotominor.yy60 = parser->symtab()->star(parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN));
		}
	}
#line 5295 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1953 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy60 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[-3].minor.yy0, s_ptr = yymsp[-1].minor.yy93;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy60 = parser->symtab()->star(yymsp[-1].minor.yy93);
		}
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5311 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1981 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ObjectDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy102;
		
		if (!parser->lang()->support(Language::Feature::DECL_OBJECT)) {
			yygotominor.yy468 = NULL;
			parser->_feature_error(Language::Feature::DECL_OBJECT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy468 = new ObjectDeclaration(yymsp[-1].minor.yy102, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ObjectDeclaration::Element* bnd, *yymsp[-1].minor.yy102) {
				BOOST_FOREACH(Symbol const* o, *bnd) {
					switch (o->type()) {
					case Symbol::Type::OBJECT:
						bnd->sort()->add((ObjectSymbol const*)o);
						break;
					case Symbol::Type::RANGE:
						bnd->sort()->add((NumberRangeSymbol const*)o);
						break;
					default:
						// will not happen
						break;
					}
				}
			}
		}
	}
#line 5346 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* object_bnd_lst ::= object_bnd */
#line 2014 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy102 = new ObjectDeclaration::ElementList();
		yygotominor.yy102->push_back(yymsp[0].minor.yy294);
	}
#line 5354 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 2020 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy102 = yymsp[-2].minor.yy102;
		yygotominor.yy102->push_back(yymsp[0].minor.yy294);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5363 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 2026 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy294 = new ObjectDeclaration::Element(yymsp[0].minor.yy93, yymsp[-2].minor.yy425);
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5371 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* object_lst ::= object_spec */
#line 2031 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = yymsp[0].minor.yy425;
	}
#line 5378 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* object_lst ::= object_lst COMMA object_spec */
#line 2035 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = yymsp[-2].minor.yy425;
		yygotominor.yy425->splice(yygotominor.yy425->end(), *yymsp[0].minor.yy425);
		delete yymsp[0].minor.yy425;
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5388 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* object_spec ::= IDENTIFIER */
#line 2044 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		yygotominor.yy425 = NULL;
		ref_ptr<const Symbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy425 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy425->push_back(o);
		}
	}
#line 5404 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 2057 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = NULL;
		ref_ptr<ObjectSymbol::SortList> lst_ptr = yymsp[-1].minor.yy195;
		ref_ptr<const Token> id_ptr = yymsp[-3].minor.yy0;
		ref_ptr<ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy195));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(),yymsp[-1].minor.yy195->size()) + "\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy425 = new  ObjectDeclaration::Element::ObjectList();
			yygotominor.yy425->push_back(o.get());
		}
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5423 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* object_spec ::= INTEGER */
#line 2072 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy425 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy425->push_back(o.get());
		}
	}
#line 5439 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* object_spec ::= num_range */
#line 2086 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = new ObjectDeclaration::Element::ObjectList();
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy345;

		// iterate over the range and add it to the list
		ref_ptr<const Symbol> o = parser->symtab()->resolveOrCreate(parser->_newRangeSymbol( yymsp[0].minor.yy345->min(), yymsp[0].minor.yy345->max()));
		if (!o) {
			parser->_parse_error("INTERNAL ERROR: Could not create object symbol.", &yymsp[0].minor.yy345->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy425->push_back(o.get());
		}
	}
#line 5456 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 2115 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<VariableDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy410;
		
		if (!parser->lang()->support(Language::Feature::DECL_VARIABLE)) {
			yygotominor.yy459 = NULL;
			parser->_feature_error(Language::Feature::DECL_VARIABLE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {

			VariableSymbol* v2;

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ref_ptr<VariableSymbol>& v, *yymsp[-1].minor.yy410) {
				if (!(v2 = parser->symtab()->resolveOrCreate(v))) {
					// Check if it's a duplicate
					v2 = (VariableSymbol*)parser->symtab()->resolve(Symbol::Type::VARIABLE, *v->base());
					if (!v2 || v2 != v) {
						parser->_parse_error("Detected conflicting definition of symbol \"" + *v->name() + "\".", &yymsp[-3].minor.yy0->beginLoc());
					} else {
						parser->_parse_error("Detected a duplicate definition of symbol \"" + *v->name() + "\".", &yymsp[-3].minor.yy0->beginLoc());
					}
				} else {
					v = v2;
				}
			}

			yygotominor.yy459 = new VariableDeclaration(yymsp[-1].minor.yy410, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());


		}
	}
#line 5494 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* variable_bnd_lst ::= variable_bnd */
#line 2151 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = yymsp[0].minor.yy410;
	}
#line 5501 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 2156 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = yymsp[-2].minor.yy410;
		yygotominor.yy410->splice(yygotominor.yy410->end(), *yymsp[0].minor.yy410);
		delete yymsp[0].minor.yy410;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5511 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* variable_bnd ::= variable_lst DBL_COLON sort */
#line 2163 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy52) {
			yygotominor.yy410->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy93));
		}



		delete yymsp[-2].minor.yy52;
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5527 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* variable_lst ::= IDENTIFIER */
#line 2176 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy52 = new TokenList();
		yygotominor.yy52->push_back(yymsp[0].minor.yy0);
	}
#line 5535 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 365: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 2181 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy52 = yymsp[-2].minor.yy52;
		yygotominor.yy52->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5544 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 2202 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<SortDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy196;
		
		if (!parser->lang()->support(Language::Feature::DECL_SORT)) {
			yygotominor.yy61 = NULL;
			parser->_feature_error(Language::Feature::DECL_SORT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy61 = new SortDeclaration(yymsp[-1].minor.yy196, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5562 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* sort_bnd_lst ::= sort_bnd */
      case 369: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==369);
#line 2218 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[0].minor.yy196;
	}
#line 5570 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 2223 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[-2].minor.yy196;
		yygotominor.yy196->splice(yygotominor.yy196->end(), *yymsp[0].minor.yy196);
		delete yymsp[0].minor.yy196;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5580 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 370: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 2235 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy196) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy196) {
				sym2->addSubSort(sym);
			}
		}
		yygotominor.yy196 = yymsp[-2].minor.yy196;
		yygotominor.yy196->splice(yymsp[-2].minor.yy196->end(), *yymsp[0].minor.yy196);
		delete yymsp[0].minor.yy196;

	  yy_destructor(yypParser,100,&yymsp[-1].minor);
}
#line 5596 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 371: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 2247 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy196) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy196) {
				sym->addSubSort(sym2);
			}
		}
		yygotominor.yy196 = yymsp[-2].minor.yy196;
		yygotominor.yy196->splice(yymsp[-2].minor.yy196->end(), *yymsp[0].minor.yy196);
		delete yymsp[0].minor.yy196;
	  yy_destructor(yypParser,99,&yymsp[-1].minor);
}
#line 5611 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 372: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 2258 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[-1].minor.yy196;
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5620 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 373: /* sort_dcl_lst ::= IDENTIFIER */
#line 2263 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yygotominor.yy196 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy196 = new SortDeclaration::ElementList();
			yygotominor.yy196->push_back(s);
		}

		delete yymsp[0].minor.yy0;
	}
#line 5637 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 374: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 2277 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[-2].minor.yy196;
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yymsp[-2].minor.yy196 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yymsp[-2].minor.yy196->push_back(s);
		}

		delete yymsp[0].minor.yy0;

	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5656 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 375: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 2304 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<ShowStatement::ElementList> lst_ptr = yymsp[-1].minor.yy123;

		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy484 = new ShowStatement(yymsp[-1].minor.yy123, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5672 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 376: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 2318 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_SHOW_ALL)) {
			parser->_feature_error(Language::Feature::DECL_SHOW_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy484 = new ShowAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5690 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 377: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 2335 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<HideStatement::ElementList> lst_ptr = yymsp[-1].minor.yy123;

		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy484 = new HideStatement(yymsp[-1].minor.yy123, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5706 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 378: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2349 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_HIDE_ALL)) {
			parser->_feature_error(Language::Feature::DECL_HIDE_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy484 = new HideAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5724 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 379: /* show_lst ::= show_elem */
#line 2367 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = new ShowStatement::ElementList();
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	}
#line 5732 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 380: /* show_lst ::= show_lst COMMA show_elem */
#line 2372 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = yymsp[-2].minor.yy123;
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5741 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 381: /* show_lst ::= show_lst SEMICOLON show_elem */
#line 2377 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = yymsp[-2].minor.yy123;
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5750 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 383: /* stmt_noconcurrency ::= NOCONCURRENCY PERIOD */
#line 2405 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy485, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5755 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 384: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD */
#line 2406 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy446, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5760 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 385: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_int_eval PERIOD */
#line 2432 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5766 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 386: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE DBL_COLON term_int_eval PERIOD */
#line 2433 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,76,&yymsp[-2].minor);
}
#line 5772 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 387: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_int_eval PERIOD */
#line 2434 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5778 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 388: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE DBL_COLON term_int_eval PERIOD */
#line 2435 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,76,&yymsp[-2].minor);
}
#line 5784 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 389: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2460 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy466 = NULL;
		ref_ptr<const Referenced> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, data_l_ptr = yymsp[-1].minor.yy489.l, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Referenced> data_maxstep_ptr = yymsp[-1].minor.yy489.maxstep, data_label_ptr = yymsp[-1].minor.yy489.label;

		ref_ptr<const ReferencedString> label;
		if (yymsp[-1].minor.yy489.label) label = yymsp[-1].minor.yy489.label->str();
		else label = new ReferencedString("0");

		int min = -1, max = -1;
		if (yymsp[-1].minor.yy489.maxstep) {
			min = yymsp[-1].minor.yy489.maxstep->min();
			max = yymsp[-1].minor.yy489.maxstep->max();
		}

		if (!parser->lang()->support(Language::Feature::DECL_QUERY)) {
			parser->_feature_error(Language::Feature::DECL_QUERY, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			bool good = true;

			// resolve the query label
			ref_ptr<QuerySymbol> sym = new QuerySymbol(label, min, max);
			if (!parser->symtab()->create(sym)) {
				parser->_parse_error("Could not create query, the label \"" + *label + "\" already exists.", (yymsp[-1].minor.yy489.label ? &yymsp[-1].minor.yy489.label->beginLoc() : &yymsp[-2].minor.yy0->beginLoc()));
				good = false;
				YYERROR;
			}


			if (good) yygotominor.yy466 = new QueryStatement(sym, yymsp[-1].minor.yy489.l, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5821 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 390: /* query_lst ::= formula_temporal */
#line 2496 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = NULL;
		yygotominor.yy489.label = NULL;

		yygotominor.yy489.l->push_back(yymsp[0].minor.yy17);
	}
#line 5832 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 391: /* query_lst ::= query_maxstep_decl */
#line 2505 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = yymsp[0].minor.yy29;
		yygotominor.yy489.label = NULL;
	}
#line 5841 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 392: /* query_lst ::= query_label_decl */
#line 2512 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = NULL;
		yygotominor.yy489.label = yymsp[0].minor.yy79;
	}
#line 5850 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 393: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2519 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy489 = yymsp[-2].minor.yy489;
		yymsp[-2].minor.yy489.l->push_back(yymsp[0].minor.yy17);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 394: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2525 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489 = yymsp[-2].minor.yy489;

		if (yygotominor.yy489.maxstep) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy29->beginLoc());
			delete yymsp[0].minor.yy29;
			YYERROR;
		} else {
			yygotominor.yy489.maxstep = yymsp[0].minor.yy29;
		}
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5875 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 395: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2538 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489 = yymsp[-2].minor.yy489;
		if (yygotominor.yy489.label) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy79->beginLoc());
			delete yymsp[0].minor.yy79;
			YYERROR;

		} else {
			yygotominor.yy489.label = yymsp[0].minor.yy79;
		}
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5891 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 396: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2564 "bcplus/parser/detail/lemon_parser.y"
{ 
	yygotominor.yy29 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, i_ptr = yymsp[0].minor.yy0;


	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {

		int max = -1;
		try {
			max = boost::lexical_cast<int>(*yymsp[0].minor.yy0->str());
			yygotominor.yy29 = new NumberRangeEval(-1, max, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		} catch (boost::bad_lexical_cast const& e) {
			parser->_parse_error("INTERNAL ERROR: An error occurred extracting an integer from \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5916 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 397: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range_eval */
#line 2585 "bcplus/parser/detail/lemon_parser.y"
{
	yygotominor.yy29 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, nr_ptr = yymsp[0].minor.yy29;

	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {
		yygotominor.yy29 = yymsp[0].minor.yy29;
		nr_ptr.release();
	}
  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5933 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 398: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 399: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==399);
#line 2599 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy79, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5940 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 400: /* clause_if ::= IF formula */
#line 2634 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_IF); 		}
#line 5945 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 401: /* clause_if ::= */
      case 403: /* clause_after ::= */ yytestcase(yyruleno==403);
      case 405: /* clause_ifcons ::= */ yytestcase(yyruleno==405);
      case 409: /* clause_where ::= */ yytestcase(yyruleno==409);
#line 2635 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = NULL; }
#line 5953 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 402: /* clause_after ::= AFTER formula */
#line 2636 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_AFTER);	}
#line 5958 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 404: /* clause_ifcons ::= IFCONS formula */
#line 2638 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_IFCONS); 	}
#line 5963 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 406: /* clause_unless ::= UNLESS atomic_formula_anon */
#line 2640 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy423, Language::Feature::CLAUSE_UNLESS); 	}
#line 5968 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 407: /* clause_unless ::= */
#line 2641 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = NULL; }
#line 5973 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 408: /* clause_where ::= WHERE formula_no_const */
#line 2642 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_WHERE); 	}
#line 5978 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 410: /* stmt_law ::= law_basic */
      case 411: /* stmt_law ::= law_caused */ yytestcase(yyruleno==411);
      case 412: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==412);
      case 413: /* stmt_law ::= law_impl */ yytestcase(yyruleno==413);
      case 414: /* stmt_law ::= law_causes */ yytestcase(yyruleno==414);
      case 415: /* stmt_law ::= law_increments */ yytestcase(yyruleno==415);
      case 416: /* stmt_law ::= law_decrements */ yytestcase(yyruleno==416);
      case 417: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==417);
      case 418: /* stmt_law ::= law_always */ yytestcase(yyruleno==418);
      case 419: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==419);
      case 420: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==420);
      case 421: /* stmt_law ::= law_never */ yytestcase(yyruleno==421);
      case 422: /* stmt_law ::= law_default */ yytestcase(yyruleno==422);
      case 423: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==423);
      case 424: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==424);
      case 425: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==425);
      case 426: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==426);
      case 427: /* stmt_law ::= law_observed */ yytestcase(yyruleno==427);
#line 2688 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy484 = yymsp[0].minor.yy484;}
#line 6000 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 428: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2805 "bcplus/parser/detail/lemon_parser.y"
{ 
		if (yymsp[-5].minor.yy17 || yymsp[-4].minor.yy17 || yymsp[-3].minor.yy17 || yymsp[-2].minor.yy423 || yymsp[-1].minor.yy17) {
			LAW_BASIC_FORM(yygotominor.yy484, NULL, yymsp[-6].minor.yy17, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
				yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
				Language::Feature::LAW_BASIC_D, BasicLaw); 
		} else {
			LAW_BASIC_FORM(yygotominor.yy484, NULL, yymsp[-6].minor.yy17, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
				yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_FACT, 
				Language::Feature::LAW_BASIC_FACT, BasicLaw); 
		}
	}
#line 6015 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 429: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2817 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy17, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 6022 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 430: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2821 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy17, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 6029 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 431: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2825 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy484, yymsp[-4].minor.yy17, yymsp[-3].minor.yy0, yymsp[-2].minor.yy17, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 6035 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 432: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2828 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy484, yymsp[-6].minor.yy423, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 6041 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 433: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2832 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy484, yymsp[-8].minor.yy423, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-4].minor.yy299, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 6048 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 434: /* law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2835 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy484, yymsp[-8].minor.yy423, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-4].minor.yy299, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_DECREMENTS, DecrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 6055 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 435: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2839 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy484, yymsp[-6].minor.yy423, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 6061 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 436: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2843 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 6068 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 437: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2847 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 6075 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 438: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2851 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 6082 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 439: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2855 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 6089 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 440: /* law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2859 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy423, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 6096 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 441: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2863 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 6103 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 442: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2867 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 6110 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 443: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2871 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 6116 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 444: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2875 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy484, yymsp[-3].minor.yy0, yymsp[-2].minor.yy225, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 6122 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 445: /* law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD */
#line 2880 "bcplus/parser/detail/lemon_parser.y"
{ 
			yygotominor.yy484 = NULL;
			ref_ptr<const Token> kw_ptr = yymsp[-4].minor.yy0, p_ptr = yymsp[0].minor.yy0;
			ref_ptr<AtomicFormula> head_ptr = yymsp[-3].minor.yy423;
			ref_ptr<Term> t_ptr = yymsp[-1].minor.yy299;

			// make sure that the At clause is integral
			if (yymsp[-1].minor.yy299->domainType() != DomainType::INTEGRAL) {
				parser->_parse_error("Expected an integral expression.", &yymsp[-1].minor.yy299->beginLoc());
				YYERROR;
			} else {
				LAW_SIMPLE_FORM(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy423, yymsp[-1].minor.yy299, yymsp[0].minor.yy0, Language::Feature::LAW_OBSERVED, ObservedLaw); 
			}
		  yy_destructor(yypParser,65,&yymsp[-2].minor);
}
#line 6141 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 446: /* stmt_code_blk ::= ASP_GR */
#line 2914 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 6146 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 447: /* stmt_code_blk ::= ASP_CP */
#line 2915 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 6151 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 448: /* stmt_code_blk ::= F2LP_GR */
#line 2916 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 6156 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 449: /* stmt_code_blk ::= F2LP_CP */
#line 2917 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 6161 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 450: /* stmt_code_blk ::= LUA_GR */
#line 2918 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 6166 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 451: /* stmt_code_blk ::= LUA_CP */
#line 2919 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 6171 "bcplus/parser/detail/lemon_parser.c"
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
#line 200 "bcplus/parser/detail/lemon_parser.y"
 parser->_parse_error("Syntax error.");	
#line 6237 "bcplus/parser/detail/lemon_parser.c"
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
