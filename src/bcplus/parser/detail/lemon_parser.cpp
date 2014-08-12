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

#line 1524 "bcplus/parser/detail/lemon_parser.y"

		
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
#line 1652 "bcplus/parser/detail/lemon_parser.y"

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
#line 2399 "bcplus/parser/detail/lemon_parser.y"

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

#line 2422 "bcplus/parser/detail/lemon_parser.y"

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
#line 2451 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRangeEval* maxstep;
		Token const* label;
	};

#line 2558 "bcplus/parser/detail/lemon_parser.y"

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

#line 2630 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2716 "bcplus/parser/detail/lemon_parser.y"

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
		ref_ptr<Element> body_ptr = body, where_ptr = where;																						\
		ref_ptr<Formula> head_ptr = head;																											\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;																								\
																																					\
		if (!head) head_ptr = new NullaryFormula(NullaryFormula::Operator::FALSE, kw->beginLoc(), kw->beginLoc());									\
		if (!parser->lang()->support(feature)) {																									\
			parser->_feature_error(feature, &kw->beginLoc());																						\
			YYERROR;																																\
		} else {																																	\
			law = new class(head_ptr, body, where, head_ptr->beginLoc(), p->endLoc());																\
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
		

#line 2914 "bcplus/parser/detail/lemon_parser.y"

	#define CODE_BLK(stmt, code, feature, type) 												\
		ref_ptr<const Token> code_ptr = code;													\
		if (!parser->lang()->support(feature)) {												\
			stmt = NULL;																		\
			parser->_feature_error(feature, &code->beginLoc());									\
			YYERROR;																			\
		} else {																				\
			stmt = new type(code, code->beginLoc(), code->endLoc());							\
		}
#line 544 "bcplus/parser/detail/lemon_parser.c"
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
#define YYNSTATE 863
#define YYNRULE 454
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
#define YY_ACTTAB_COUNT (3388)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   862,   64,  863,  861,  860,  859,  858,  857,  856,  855,
 /*    10 */   854,  853,  852,  851,  850,  849,  848,  847,   63,  820,
 /*    20 */   322,  846,  839,  845,  844,  838,  150,  148,  146,  145,
 /*    30 */   144,  841,  318,  323,  820,  322,  846,  839,  478,  844,
 /*    40 */   838,  779,  420,  631,  117,  115,  114,  317,  323,   25,
 /*    50 */    24,   64,  642,   26,  336,  771,  768,  767,  766,  765,
 /*    60 */   693,  122,  361,  247,  778,  777,   62,   18,  480,  820,
 /*    70 */   322,  846,  839,  845,  844,  838,  150,  148,  146,  145,
 /*    80 */   144,   61,  317,  323,  241,  239,  237,  236,  235,  366,
 /*    90 */   771,  768,  767,  766,  765,  469,  517,  471,  694,  695,
 /*   100 */   558,  589,  588,  587,  586,  585,  584,  583,  582,  581,
 /*   110 */   580,  579,  578,  577,  576,  575,  574,  573,  572,  819,
 /*   120 */   547,  659,   60,  548,  818,  557,  556,  553,  552,  555,
 /*   130 */   554,  840,  174,  172,  170,  168,  167,  638,  537,  846,
 /*   140 */   839,  845,  844,  838,   46,  101,   12,  297,  202,  203,
 /*   150 */    44,   30,   10,   11,  300,  193,  773,  268,   45,  166,
 /*   160 */   607,  122,  262,  693,   26,  361,  792,  791,  793,    9,
 /*   170 */   500,  499,    8,  312,   43,  254,  326,  611,  608,  606,
 /*   180 */   605,  775,  776,  713,  692,   47,  324,  186,  724,   48,
 /*   190 */   170,  168,  167,  309,  216,  215,  214,   13,  467,  517,
 /*   200 */   471,  694,  695,  545,  689,  688,  687,  686,  292,  637,
 /*   210 */   547,  100,  515,  548,  636,  121,  119,  117,  115,  114,
 /*   220 */   685,   99,  683,  125,  544,   53,   52,  682,   98,   54,
 /*   230 */   708,  707,  709,  637,  547,  715,  551,  548,  636,  681,
 /*   240 */   679,  680,  684,  760,  759,  710,  711,  537,  550,  837,
 /*   250 */   549,   20,  192,  221,  501,  211,  615,  614,  502,  834,
 /*   260 */   842,  843,  846,  839,  845,  844,  838,  485,  419,  774,
 /*   270 */   657,  603,  604,  651,  200,  537,  599,    6,  343,   42,
 /*   280 */   615,  614,  616,  309,  819,  547,  536,  219,  548,  818,
 /*   290 */   194,  273,  217,  194,   73,  603,  604,  753,  200,  195,
 /*   300 */   752,    6,  195,   42,  473,  675,  472,  309,   22,   21,
 /*   310 */    25,   24,   40,   41,   26,   38,   37,   72,  132,   39,
 /*   320 */    59,  820,  322,  846,  839,  845,  844,  838,  237,  236,
 /*   330 */   235,  792,  791,  793,  315,  323,   40,   41,  550,  837,
 /*   340 */   549,  691,  132,  779,  774,   71,  763,  764,  669,  207,
 /*   350 */   361,  796,    3,   74,   29,  310,  819,  547,  309,   54,
 /*   360 */   548,  818,  550,  837,  549,  479,  778,  777,  545,   23,
 /*   370 */    22,   21,   25,   24,  649,  245,   26,  820,  322,  846,
 /*   380 */   839,  478,  844,  838,  693,   67,  361,   27,   28,  655,
 /*   390 */   317,  323,  730,   98,  673,  463,  668,  338,  771,  768,
 /*   400 */   767,  766,  765,  792,  791,  793,  146,  145,  144,  674,
 /*   410 */    19,  480,  762,  550,  837,  549,  202,  203,  775,  776,
 /*   420 */   522,  471,  694,  695,  186,  729,   48,  521,  520,  185,
 /*   430 */   309,  656,  638,  537,  846,  839,  845,  844,  838,   23,
 /*   440 */    22,   21,   25,   24,  242,  198,   26,  212,  181,    2,
 /*   450 */   189,  196,  197,  728,  693,  607,  361,  461,  662,  460,
 /*   460 */   125,  666,  750,  547,  506,   98,  548,  749,  313,  760,
 /*   470 */   759,  342,  611,  608,  606,  605,  205,   33,  240,  453,
 /*   480 */   454,  210,  537,  238,   30,  550,  837,  549,  661,  466,
 /*   490 */   517,  471,  694,  695,  638,  537,  846,  839,  478,  844,
 /*   500 */   838,  523,    7, 1260,  774,  451,  593,  592,  204,  743,
 /*   510 */   742,  744,  311,   14,  139,  138,  137,  607,  136,  135,
 /*   520 */   134,  133,   15, 1260,  733,  734,  486,   31,  480,  669,
 /*   530 */   313,  361,   57,  328,  611,  608,  606,  605,  152,  143,
 /*   540 */   142,  141,  140,  836,   47,  638,  537,  846,  839,  478,
 /*   550 */   844,  838,  834,  842,  843,  846,  839,  845,  844,  838,
 /*   560 */   484,  419,  598,   66,  188,   55,   56,  546,  607,  505,
 /*   570 */   693,  153,  361,  122,  464,  667,  463,  668,   32,  480,
 /*   580 */   727,  313,    5,   65,  330,  611,  608,  606,  605,  748,
 /*   590 */   309,  550,  837,  549,  459,  455,  508, 1083,  160,  159,
 /*   600 */   158, 1083,  157,  156,  155,  154,  470,  471,  694,  695,
 /*   610 */   638,  537,  846,  839,  845,  844,  838,  311,    7,  726,
 /*   620 */   274,  635,  165,  164,  163,  162,  161,  595,  594,   14,
 /*   630 */   139,  138,  137,  607,  136,  135,  134,  133,  452,  811,
 /*   640 */   547,  476,  716,  548,  810,  550,  313,   30,  311,  341,
 /*   650 */   611,  608,  606,  605,  152,  143,  142,  141,  140,  725,
 /*   660 */   820,  322,  846,  839,  845,  844,  838,  121,  119,  117,
 /*   670 */   115,  114,  546,  318,  323,  638,  537,  846,  839,  845,
 /*   680 */   844,  838,  779,  774,  597,  596,  805,  804,  806,  633,
 /*   690 */   547,  545,  545,  548,  632,  693,  701,  361,  607,  835,
 /*   700 */   700,  807,  808,  546,  780,  778,  777,   47,  242,  173,
 /*   710 */   311,  613,  655,  655,  601,  611,  608,  606,  605,  834,
 /*   720 */   842,  843,  846,  839,  845,  844,  838,  483,  419,  122,
 /*   730 */   462,  517,  471,  694,  695,  538,  627,  626,  628,  524,
 /*   740 */   833,  547,  240,  171,  548,  832,  722,  238,  169,  813,
 /*   750 */   717,  629,  630,  430,  652,  654,   87,   86,   85,  151,
 /*   760 */    84,   83,   82,   81,  245,  546,  837,  678,  550,  837,
 /*   770 */   549,  634,  625,  846,  839,  845,  844,  838,  545,  175,
 /*   780 */    92,   91,   90,   89,   88,  234,  184,  827,  826,  828,
 /*   790 */   243,  696,  697,  149,  641,  669,  126,  361,  147,  655,
 /*   800 */   232,  245,  829,  830,  519,  233,  424,  755,  206,  518,
 /*   810 */   120,  820,  322,  846,  839,  845,  844,  838,  550,  837,
 /*   820 */   549, 1222,  514,  179,  314,  323,  241,  239,  237,  236,
 /*   830 */   235,  334,  771,  768,  767,  766,  765,  122,  525,  187,
 /*   840 */   429,  652,  817,  671,  118,  719,  720, 1222,  291,  116,
 /*   850 */   182,  108,  107,  106,  513,  105,  104,  103,  102,  178,
 /*   860 */   819,  547,  781,  550,  548,  818,  714,  472,  250,  550,
 /*   870 */   837,  549,  690,  754,  191,  113,  112,  111,  110,  109,
 /*   880 */   834,  842,  843,  846,  839,  845,  844,  838,  482,  419,
 /*   890 */   177,  820,  321,  846,  839,  845,  844,  838,  176,   58,
 /*   900 */   664,  213,  183,  507,  772,  323,  660,  792,  791,  793,
 /*   910 */   677,  757,  771,  768,  767,  766,  765,  220,  218,  216,
 /*   920 */   215,  214,  699,  820,  322,  846,  839,  845,  844,  838,
 /*   930 */   124,   23,   22,   21,   25,   24,  317,  323,   26,  545,
 /*   940 */   183,  676,  180,  770,  771,  768,  767,  766,  765,  665,
 /*   950 */   460,  760,  759,  820,  322,  846,  839,  845,  844,  838,
 /*   960 */   542,   39,  246,  307,  123,  653,  317,  323,  509,   98,
 /*   970 */   650,  183,  591,  769,  771,  768,  767,  766,  765,  820,
 /*   980 */   322,  846,  839,  845,  844,  838,  831,  670,  658,  550,
 /*   990 */   837,  549,  317,  323,  199,    4,  538,  450,  817,  540,
 /*  1000 */   771,  768,  767,  766,  765,  590,  809,  108,  107,  106,
 /*  1010 */   571,  105,  104,  103,  102,  458,  508,  183,  570,  121,
 /*  1020 */   119,  117,  115,  114,  820,  322,  846,  839,  845,  844,
 /*  1030 */   838,  113,  112,  111,  110,  109,  569,  317,  323,  174,
 /*  1040 */   172,  170,  168,  167,  539,  771,  768,  767,  766,  765,
 /*  1050 */   820,  322,  846,  839,  845,  844,  838,  201,  693,  546,
 /*  1060 */   361,  568,   70,  317,  323,  174,  172,  170,  168,  167,
 /*  1070 */   387,  771,  768,  767,  766,  765,  474,  718,  820,  322,
 /*  1080 */   846,  839,  845,  844,  838,  567,   23,   22,   21,   25,
 /*  1090 */    24,  317,  323,   26,  516,  471,  694,  695,  439,  771,
 /*  1100 */   768,  767,  766,  765,  457,  508,  456,  508,  820,  322,
 /*  1110 */   846,  839,  845,  844,  838,  566,  817,  431,  508, 1318,
 /*  1120 */     1,  317,  323,  565,  550,  231,  244,  307,  438,  771,
 /*  1130 */   768,  767,  766,  765,  820,  322,  846,  839,  845,  844,
 /*  1140 */   838,  241,  239,  237,  236,  235,  563,  317,  323,  113,
 /*  1150 */   112,  111,  110,  109,  339,  771,  768,  767,  766,  765,
 /*  1160 */    74,   80,   79,  562,   78,   77,   76,   75,   92,   91,
 /*  1170 */    90,   89,   88,  127,  561,  560,  820,  322,  846,  839,
 /*  1180 */   845,  844,  838,  559,   97,   96,   95,   94,   93,  317,
 /*  1190 */   323,  797,  795,  837,  781,  550,  337,  771,  768,  767,
 /*  1200 */   766,  765,  820,  322,  846,  839,  845,  844,  838,  761,
 /*  1210 */   712,  758,  305,   17,  304,  317,  323,   97,   96,   95,
 /*  1220 */    94,   93,  335,  771,  768,  767,  766,  765,  820,  322,
 /*  1230 */   846,  839,  845,  844,  838,   16,  303,  190,   69,  533,
 /*  1240 */    58,  317,  323,  220,  218,  216,  215,  214,  365,  771,
 /*  1250 */   768,  767,  766,  765,  820,  322,  846,  839,  845,  844,
 /*  1260 */   838,  301,   23,   22,   21,   25,   24,  317,  323,   26,
 /*  1270 */   531,  530,  298,  528,  364,  771,  768,  767,  766,  765,
 /*  1280 */   529,  295,  723,  820,  322,  846,  839,  845,  844,  838,
 /*  1290 */   527,  293,  475,   68,  526,  672,  317,  323,  692,  289,
 /*  1300 */   288,  287,  285,  228,  771,  768,  767,  766,  765,  820,
 /*  1310 */   322,  846,  839,  845,  844,  838,  498,   23,   22,   21,
 /*  1320 */    25,   24,  319,  323,   26,  284,  283,  564,  281,  355,
 /*  1330 */   771,  768,  767,  766,  765,  820,  322,  846,  839,  845,
 /*  1340 */   844,  838,  497,   30,  495,  279,  272,  756,  317,  323,
 /*  1350 */   220,  218,  216,  215,  214,  227,  771,  768,  767,  766,
 /*  1360 */   765,  820,  322,  846,  839,  845,  844,  838,  494,   23,
 /*  1370 */    22,   21,   25,   24,  317,  323,   26,  277,  271,  493,
 /*  1380 */   275,  226,  771,  768,  767,  766,  765,  820,  322,  846,
 /*  1390 */   839,  845,  844,  838,  492,  648,  270,  269,  267,  265,
 /*  1400 */   317,  323,  241,  239,  237,  236,  235,  225,  771,  768,
 /*  1410 */   767,  766,  765,  820,  322,  846,  839,  845,  844,  838,
 /*  1420 */   491,  266,  640,  264,  490,  261,  317,  323,  241,  239,
 /*  1430 */   237,  236,  235,  224,  771,  768,  767,  766,  765,  820,
 /*  1440 */   322,  846,  839,  845,  844,  838,  794,  263,  260,  259,
 /*  1450 */   258,  545,  317,  323,  241,  239,  237,  236,  235,  223,
 /*  1460 */   771,  768,  767,  766,  765,  820,  322,  846,  839,  845,
 /*  1470 */   844,  838,  543,  257,  255,  209,  480,  794,  317,  323,
 /*  1480 */   253,  602,  545,  489,  488,  222,  771,  768,  767,  766,
 /*  1490 */   765,  820,  322,  846,  839,  845,  844,  838,  487,  302,
 /*  1500 */   294,  308,  721,  541,  318,  323,  208,  480,   36,   35,
 /*  1510 */    34,   38,   37,  779,  774,   39,  820,  322,  846,  839,
 /*  1520 */   845,  844,  838,  290,   36,   35,   34,   38,   37,  318,
 /*  1530 */   323,   39,  286,  698,  306,  252,  778,  777,  779,  774,
 /*  1540 */   359,  820,  322,  846,  839,  845,  844,  838,  360,  299,
 /*  1550 */   427,  647,  731,  428,  318,  323,  646,  645,  644,  643,
 /*  1560 */   251,  778,  777,  779,  774,  358,  820,  322,  846,  839,
 /*  1570 */   845,  844,  838,  357,   51,   50,   49,   53,   52,  318,
 /*  1580 */   323,   54,  356,  370,  296,  249,  778,  777,  779,  774,
 /*  1590 */   638,  537,  846,  839,  845,  844,  838,  282,  638,  537,
 /*  1600 */   846,  839,  845,  844,  838,  532,  496,  362,  512,  432,
 /*  1610 */   248,  778,  777,  607,  820,  325,  846,  839,  845,  844,
 /*  1620 */   838,  607,  280,  278,  706,  276,  313,  790,  405,  610,
 /*  1630 */   611,  608,  606,  605,  313,  131,  256,  609,  611,  608,
 /*  1640 */   606,  605,  638,  537,  846,  839,  845,  844,  838,  433,
 /*  1650 */   638,  537,  846,  839,  845,  844,  838,  705,  704,   36,
 /*  1660 */    35,   34,   38,   37,  703,  607,   39,   23,   22,   21,
 /*  1670 */    25,   24,  702,  607,   26,  373,  663, 1319,  313, 1319,
 /*  1680 */  1319,  504,  611,  608,  606,  605,  313, 1319, 1319,  503,
 /*  1690 */   611,  608,  606,  605,  638,  537,  846,  839,  845,  844,
 /*  1700 */   838, 1319,  638,  537,  846,  839,  845,  844,  838,  820,
 /*  1710 */   404,  846,  839,  845,  844,  838, 1319,  607, 1319, 1319,
 /*  1720 */  1319, 1319,  367,  405, 1319,  607,   50,   49,   53,   52,
 /*  1730 */   313, 1319,   54,  371,  611,  608,  606,  605,  313,  600,
 /*  1740 */  1319,  422,  611,  608,  606,  605,  638,  537,  846,  839,
 /*  1750 */   845,  844,  838, 1319,  638,  537,  846,  839,  845,  844,
 /*  1760 */   838,   36,   35,   34,   38,   37, 1319, 1319,   39,  607,
 /*  1770 */   820,  788,  846,  839,  845,  844,  838,  607, 1319, 1319,
 /*  1780 */  1319, 1319,  313,  790,  405,  421,  611,  608,  606,  605,
 /*  1790 */   313, 1319, 1319,  331,  611,  608,  606,  605,  638,  537,
 /*  1800 */   846,  839,  845,  844,  838, 1319,  638,  537,  846,  839,
 /*  1810 */   845,  844,  838, 1319,  187, 1319, 1319, 1319, 1319, 1319,
 /*  1820 */  1319,  607, 1319, 1319, 1319, 1319,  108,  107,  106,  607,
 /*  1830 */   105,  104,  103,  102,  313, 1319, 1319,  329,  611,  608,
 /*  1840 */   606,  605,  313, 1319, 1319,  327,  611,  608,  606,  605,
 /*  1850 */   113,  112,  111,  110,  109,    4, 1319, 1319, 1319,  930,
 /*  1860 */   930,  930, 1319,  930,  930,  930,  930,  108,  107,  106,
 /*  1870 */  1319,  105,  104,  103,  102,  160,  159,  158, 1319,  157,
 /*  1880 */   156,  155,  154,  930,  930,  930,  930,  930, 1319, 1319,
 /*  1890 */  1319,  113,  112,  111,  110,  109, 1319, 1319, 1319,  165,
 /*  1900 */   164,  163,  162,  161, 1319, 1319, 1319, 1319,  108,  107,
 /*  1910 */   106, 1319,  105,  104,  103,  102, 1319, 1319, 1319, 1319,
 /*  1920 */  1319, 1319, 1319, 1319,  751,  741,  846,  839,  845,  844,
 /*  1930 */   838, 1319,  113,  112,  111,  110,  109, 1319, 1319,  320,
 /*  1940 */  1319,  751,  741,  846,  839,  845,  844,  838, 1319, 1319,
 /*  1950 */  1319,  363,  738,  735, 1319, 1319,  316,  751,  741,  846,
 /*  1960 */   839,  845,  844,  838, 1319, 1319, 1319, 1319,  333,  738,
 /*  1970 */   735, 1319,  740, 1319, 1319, 1319,  751,  741,  846,  839,
 /*  1980 */   845,  844,  838, 1319,  732,  738,  735, 1319, 1319, 1319,
 /*  1990 */  1319,  320,  751,  741,  846,  839,  845,  844,  838, 1319,
 /*  2000 */  1319, 1319, 1319,  737,  738,  735, 1319,  320,  751,  741,
 /*  2010 */   846,  839,  845,  844,  838, 1319,  465,  511, 1319,  736,
 /*  2020 */   738,  735, 1319,  320,  751,  741,  846,  839,  845,  844,
 /*  2030 */   838, 1319, 1319, 1319, 1319,  535,  738,  735, 1319,  320,
 /*  2040 */   751,  741,  846,  839,  845,  844,  838,  713,  692,  130,
 /*  2050 */  1319,  534,  738,  735, 1319,  320,  751,  741,  846,  839,
 /*  2060 */   845,  844,  838,  708,  707,  709, 1319,  374,  738,  735,
 /*  2070 */  1319,  320, 1319,   36,   35,   34,   38,   37,  710,  711,
 /*  2080 */    39, 1319, 1319,  435,  738,  735,  221,  751,  741,  846,
 /*  2090 */   839,  845,  844,  838,  708,  707,  709, 1319,  713, 1319,
 /*  2100 */  1319, 1319,  320, 1319, 1319, 1319, 1159, 1319, 1319,  710,
 /*  2110 */   711, 1319,  129, 1319,  434,  738,  735,  221, 1319, 1319,
 /*  2120 */   219, 1159, 1159, 1319, 1319,  217,  834,  842,  843,  846,
 /*  2130 */   839,  845,  844,  838,  477,  419,   36,   35,   34,   38,
 /*  2140 */    37, 1159, 1159,   39, 1319,  708,  707,  709, 1319, 1319,
 /*  2150 */  1319,  219, 1159, 1159, 1319, 1319,  217, 1319, 1319, 1159,
 /*  2160 */   710,  711,   51,   50,   49,   53,   52, 1319,  221,   54,
 /*  2170 */   834,  842,  843,  846,  839,  845,  844,  838, 1319,  418,
 /*  2180 */  1319, 1159,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2190 */  1319,  448, 1319, 1319,  834,  842,  843,  846,  839,  845,
 /*  2200 */   844,  838,  219,  369, 1319, 1319, 1319,  217, 1319,  834,
 /*  2210 */   842,  843,  846,  839,  845,  844,  838, 1319,  449,  834,
 /*  2220 */   842,  843,  846,  839,  845,  844,  838, 1319,  825, 1319,
 /*  2230 */  1319, 1319,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2240 */  1319,  821,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2250 */  1319,  824,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2260 */  1319,  823, 1319,  834,  842,  843,  846,  839,  845,  844,
 /*  2270 */   838, 1319,  822, 1319,  834,  842,  843,  846,  839,  845,
 /*  2280 */   844,  838, 1319,  447,  834,  842,  843,  846,  839,  845,
 /*  2290 */   844,  838, 1319,  446, 1319,  834,  842,  843,  846,  839,
 /*  2300 */   845,  844,  838, 1319,  816,  834,  842,  843,  846,  839,
 /*  2310 */   845,  844,  838, 1319,  815,  834,  842,  843,  846,  839,
 /*  2320 */   845,  844,  838, 1319,  814,  834,  842,  843,  846,  839,
 /*  2330 */   845,  844,  838, 1319,  417,  834,  842,  843,  846,  839,
 /*  2340 */   845,  844,  838, 1319,  416,  834,  842,  843,  846,  839,
 /*  2350 */   845,  844,  838, 1319,  415,  834,  842,  843,  846,  839,
 /*  2360 */   845,  844,  838, 1319,  414,  834,  842,  843,  846,  839,
 /*  2370 */   845,  844,  838, 1319,  413,  834,  842,  843,  846,  839,
 /*  2380 */   845,  844,  838, 1319,  412, 1319,  834,  842,  843,  846,
 /*  2390 */   839,  845,  844,  838, 1319,  411,  812,  803,  846,  839,
 /*  2400 */   845,  844,  838, 1319, 1319, 1319,  481,  409,  834,  842,
 /*  2410 */   843,  846,  839,  845,  844,  838, 1319,  407,  834,  842,
 /*  2420 */   843,  846,  839,  845,  844,  838, 1319,  406,  834,  842,
 /*  2430 */   843,  846,  839,  845,  844,  838, 1319,  789,  834,  842,
 /*  2440 */   843,  846,  839,  845,  844,  838, 1319,  443, 1319,  834,
 /*  2450 */   842,  843,  846,  839,  845,  844,  838, 1319,  442,  834,
 /*  2460 */   842,  843,  846,  839,  845,  844,  838, 1319,  787,  834,
 /*  2470 */   842,  843,  846,  839,  845,  844,  838, 1319,  786,  834,
 /*  2480 */   842,  843,  846,  839,  845,  844,  838, 1319,  785,  834,
 /*  2490 */   842,  843,  846,  839,  845,  844,  838, 1319,  441,  834,
 /*  2500 */   842,  843,  846,  839,  845,  844,  838, 1319,  440,  834,
 /*  2510 */   842,  843,  846,  839,  845,  844,  838, 1319,  784,  834,
 /*  2520 */   842,  843,  846,  839,  845,  844,  838, 1319,  783,  834,
 /*  2530 */   842,  843,  846,  839,  845,  844,  838, 1319,  782, 1319,
 /*  2540 */   834,  842,  843,  846,  839,  845,  844,  838, 1319,  403,
 /*  2550 */   834,  842,  843,  846,  839,  845,  844,  838, 1319,  402,
 /*  2560 */   834,  842,  843,  846,  839,  845,  844,  838, 1319,  401,
 /*  2570 */  1319,  834,  842,  843,  846,  839,  845,  844,  838, 1319,
 /*  2580 */   400,  834,  842,  843,  846,  839,  845,  844,  838, 1319,
 /*  2590 */   399,  834,  842,  843,  846,  839,  845,  844,  838, 1319,
 /*  2600 */   398,  834,  842,  843,  846,  839,  845,  844,  838, 1319,
 /*  2610 */   397, 1319,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2620 */  1319,  396,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2630 */  1319,  395,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2640 */  1319,  394,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2650 */  1319,  393,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2660 */  1319,  392,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2670 */  1319,  391,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2680 */  1319,  390,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2690 */  1319,  389,  834,  842,  843,  846,  839,  845,  844,  838,
 /*  2700 */  1319,  388, 1319,  834,  842,  843,  846,  839,  845,  844,
 /*  2710 */   838, 1319,  386,  834,  842,  843,  846,  839,  845,  844,
 /*  2720 */   838, 1319,  385,  834,  842,  843,  846,  839,  845,  844,
 /*  2730 */   838, 1319,  384, 1319,  834,  842,  843,  846,  839,  845,
 /*  2740 */   844,  838, 1319,  383,  834,  842,  843,  846,  839,  845,
 /*  2750 */   844,  838, 1319,  382,  834,  842,  843,  846,  839,  845,
 /*  2760 */   844,  838, 1319,  230,  834,  842,  843,  846,  839,  845,
 /*  2770 */   844,  838, 1319,  229, 1319,  834,  842,  843,  846,  839,
 /*  2780 */   845,  844,  838, 1319,  372, 1319,  812,  803,  846,  839,
 /*  2790 */   845,  844,  838,  693, 1319,  361, 1319,  410,  812,  803,
 /*  2800 */   846,  839,  845,  844,  838,  693, 1319,  361, 1319,  444,
 /*  2810 */   812,  803,  846,  839,  845,  844,  838, 1319, 1319, 1319,
 /*  2820 */  1319,  368,  812,  803,  846,  839,  845,  844,  838,  468,
 /*  2830 */   471,  694,  695,  445,  812,  803,  846,  839,  845,  844,
 /*  2840 */   838,  510,  471,  694,  695,  802,  812,  803,  846,  839,
 /*  2850 */   845,  844,  838, 1319, 1319, 1319, 1319,  798,  812,  803,
 /*  2860 */   846,  839,  845,  844,  838, 1319, 1319, 1319, 1319,  801,
 /*  2870 */   812,  803,  846,  839,  845,  844,  838, 1319, 1319, 1319,
 /*  2880 */  1319,  800,  812,  803,  846,  839,  845,  844,  838, 1319,
 /*  2890 */  1319, 1319, 1319,  799,  812,  803,  846,  839,  845,  844,
 /*  2900 */   838, 1319, 1319, 1319, 1319,  408,  812,  803,  846,  839,
 /*  2910 */   845,  844,  838, 1319, 1319, 1319, 1319,  437,  812,  803,
 /*  2920 */   846,  839,  845,  844,  838, 1319, 1319, 1319, 1319,  436,
 /*  2930 */   812,  803,  846,  839,  845,  844,  838, 1319, 1319, 1319,
 /*  2940 */  1319,  747,  812,  803,  846,  839,  845,  844,  838, 1319,
 /*  2950 */  1319, 1319, 1319,  746,  812,  803,  846,  839,  845,  844,
 /*  2960 */   838, 1319, 1319, 1319, 1319,  745,  812,  803,  846,  839,
 /*  2970 */   845,  844,  838, 1319, 1319, 1319, 1319,  381,  812,  803,
 /*  2980 */   846,  839,  845,  844,  838, 1319, 1319, 1319, 1319,  380,
 /*  2990 */   812,  803,  846,  839,  845,  844,  838, 1319, 1319, 1319,
 /*  3000 */  1319,  379,  812,  803,  846,  839,  845,  844,  838, 1319,
 /*  3010 */  1319, 1319, 1319,  378,  812,  803,  846,  839,  845,  844,
 /*  3020 */   838, 1319, 1319, 1319, 1319,  377,  812,  803,  846,  839,
 /*  3030 */   845,  844,  838, 1319, 1319, 1319, 1319,  376,  812,  803,
 /*  3040 */   846,  839,  845,  844,  838, 1319, 1319, 1319, 1319,  375,
 /*  3050 */   812,  803,  846,  839,  845,  844,  838, 1319, 1319, 1319,
 /*  3060 */  1319,  739,  634,  625,  846,  839,  845,  844,  838, 1319,
 /*  3070 */  1319, 1319,  634,  625,  846,  839,  845,  844,  838,  634,
 /*  3080 */   625,  846,  839,  845,  844,  838,  634,  625,  846,  839,
 /*  3090 */   845,  844,  838, 1319, 1319, 1319, 1319,  332,  634,  625,
 /*  3100 */   846,  839,  845,  844,  838, 1319, 1319,  425, 1319,   35,
 /*  3110 */    34,   38,   37, 1319,  624,   39, 1319, 1319, 1319, 1319,
 /*  3120 */  1319,  426,  634,  625,  846,  839,  845,  844,  838, 1319,
 /*  3130 */  1319, 1319, 1319,  623,  634,  625,  846,  839,  845,  844,
 /*  3140 */   838, 1319, 1319, 1319,  634,  625,  846,  839,  845,  844,
 /*  3150 */   838, 1319, 1319, 1319, 1319, 1319, 1319,  622,  634,  625,
 /*  3160 */   846,  839,  845,  844,  838, 1319, 1319, 1319, 1319,  621,
 /*  3170 */   634,  625,  846,  839,  845,  844,  838, 1319, 1319,  620,
 /*  3180 */   634,  625,  846,  839,  845,  844,  838, 1319, 1319, 1319,
 /*  3190 */  1319,   16, 1319,  423,  634,  625,  846,  839,  845,  844,
 /*  3200 */   838, 1319, 1319, 1319, 1319,  619,  634,  625,  846,  839,
 /*  3210 */   845,  844,  838, 1319, 1319,  618,  634,  625,  846,  839,
 /*  3220 */   845,  844,  838, 1319, 1319, 1319, 1319, 1319, 1319,  617,
 /*  3230 */   634,  625,  846,  839,  845,  844,  838, 1319, 1319, 1319,
 /*  3240 */  1319,  354,  634,  625,  846,  839,  845,  844,  838, 1319,
 /*  3250 */  1319,  353,  634,  625,  846,  839,  845,  844,  838,   23,
 /*  3260 */    22,   21,   25,   24, 1319,  352,   26, 1319, 1319,  634,
 /*  3270 */   625,  846,  839,  845,  844,  838, 1319,  351,  634,  625,
 /*  3280 */   846,  839,  845,  844,  838, 1319, 1319,  350,  634,  625,
 /*  3290 */   846,  839,  845,  844,  838,  634,  625,  846,  839,  845,
 /*  3300 */   844,  838, 1319, 1319,  349,  634,  625,  846,  839,  845,
 /*  3310 */   844,  838, 1319,  348,  634,  625,  846,  839,  845,  844,
 /*  3320 */   838,   30, 1319,  612, 1319, 1319, 1319, 1319, 1319,  128,
 /*  3330 */   347,  634,  625,  846,  839,  845,  844,  838, 1319, 1319,
 /*  3340 */   346,  812,  803,  846,  839,  845,  844,  838, 1319,  345,
 /*  3350 */  1319,  639,  340,   36,   35,   34,   38,   37, 1319, 1319,
 /*  3360 */    39, 1319, 1319, 1319, 1319, 1319,  344, 1319, 1319, 1319,
 /*  3370 */  1319, 1319, 1319, 1319,   23,   22,   21,   25,   24, 1319,
 /*  3380 */  1319,   26, 1319,  241,  239,  237,  236,  235,
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
 /*   120 */     2,   72,   71,    5,    6,    7,    8,    9,   10,   11,
 /*   130 */    12,   72,  105,  106,  107,  108,  109,  154,  155,  156,
 /*   140 */   157,  158,  159,  160,   33,   71,   28,   36,   99,  100,
 /*   150 */    32,   41,   34,   35,   43,   37,   72,   39,   47,   81,
 /*   160 */   177,  102,   44,  170,  101,  172,   48,   49,   50,   51,
 /*   170 */    52,   53,   54,  190,   56,   57,  193,  194,  195,  196,
 /*   180 */   197,   63,   64,    1,    2,  101,   68,   69,   73,   71,
 /*   190 */   107,  108,  109,   75,  107,  108,  109,   79,  205,  206,
 /*   200 */   207,  208,  209,  163,   22,   23,   24,   25,   93,    1,
 /*   210 */     2,   70,   30,    5,    6,  105,  106,  107,  108,  109,
 /*   220 */    38,   70,   40,  105,  184,   97,   98,   45,  110,  101,
 /*   230 */    48,   49,   50,    1,    2,   73,  118,    5,    6,   57,
 /*   240 */    58,   59,   60,   90,   91,   63,   64,  155,  130,  131,
 /*   250 */   132,   98,  134,   71,   46,   93,   48,   49,   50,  153,
 /*   260 */   154,  155,  156,  157,  158,  159,  160,  161,  162,  177,
 /*   270 */    73,   63,   64,   73,   66,  155,  171,   69,  173,   71,
 /*   280 */    48,   49,   50,   75,    1,    2,  166,  105,    5,    6,
 /*   290 */    93,  199,  110,   93,   70,   63,   64,  177,   66,  102,
 /*   300 */   180,   69,  102,   71,  210,  211,  212,   75,   95,   96,
 /*   310 */    97,   98,  104,  105,  101,   97,   98,   70,  110,  101,
 /*   320 */    71,  154,  155,  156,  157,  158,  159,  160,  107,  108,
 /*   330 */   109,   48,   49,   50,  167,  168,  104,  105,  130,  131,
 /*   340 */   132,   72,  110,  176,  177,   70,   63,   64,  170,   66,
 /*   350 */   172,   98,   69,   81,   71,  102,    1,    2,   75,  101,
 /*   360 */     5,    6,  130,  131,  132,  198,  199,  200,  163,   94,
 /*   370 */    95,   96,   97,   98,    1,  106,  101,  154,  155,  156,
 /*   380 */   157,  158,  159,  160,  170,   81,  172,  104,  105,  184,
 /*   390 */   167,  168,   73,  110,  216,  217,  218,  174,  175,  176,
 /*   400 */   177,  178,  179,   48,   49,   50,  107,  108,  109,   73,
 /*   410 */   187,  188,   67,  130,  131,  132,   99,  100,   63,   64,
 /*   420 */   206,  207,  208,  209,   69,   73,   71,  213,  214,   93,
 /*   430 */    75,  226,  154,  155,  156,  157,  158,  159,  160,   94,
 /*   440 */    95,   96,   97,   98,   71,   14,  101,   16,   17,   18,
 /*   450 */    19,   20,   21,   73,  170,  177,  172,  219,  220,  221,
 /*   460 */   105,   73,    1,    2,   27,  110,    5,    6,  190,   90,
 /*   470 */    91,  193,  194,  195,  196,  197,   71,   98,  105,   48,
 /*   480 */    49,   93,  155,  110,   41,  130,  131,  132,   73,  205,
 /*   490 */   206,  207,  208,  209,  154,  155,  156,  157,  158,  159,
 /*   500 */   160,   96,   69,   73,  177,  227,  228,  229,   93,   48,
 /*   510 */    49,   50,   75,   80,   81,   82,   83,  177,   85,   86,
 /*   520 */    87,   88,   79,   93,   63,   64,  199,  187,  188,  170,
 /*   530 */   190,  172,   71,  193,  194,  195,  196,  197,  105,  106,
 /*   540 */   107,  108,  109,   72,  101,  154,  155,  156,  157,  158,
 /*   550 */   159,  160,  153,  154,  155,  156,  157,  158,  159,  160,
 /*   560 */   161,  162,   73,   31,  133,  104,  105,  130,  177,   27,
 /*   570 */   170,  110,  172,  102,  215,  216,  217,  218,  187,  188,
 /*   580 */    73,  190,   93,   31,  193,  194,  195,  196,  197,   72,
 /*   590 */    75,  130,  131,  132,  222,  223,  224,   98,   81,   82,
 /*   600 */    83,  102,   85,   86,   87,   88,  206,  207,  208,  209,
 /*   610 */   154,  155,  156,  157,  158,  159,  160,   75,   69,   73,
 /*   620 */   105,   72,  105,  106,  107,  108,  109,    1,    2,   80,
 /*   630 */    81,   82,   83,  177,   85,   86,   87,   88,    1,    1,
 /*   640 */     2,  201,  202,    5,    6,  130,  190,   41,   75,  193,
 /*   650 */   194,  195,  196,  197,  105,  106,  107,  108,  109,   73,
 /*   660 */   154,  155,  156,  157,  158,  159,  160,  105,  106,  107,
 /*   670 */   108,  109,  130,  167,  168,  154,  155,  156,  157,  158,
 /*   680 */   159,  160,  176,  177,  228,  229,   48,   49,   50,    1,
 /*   690 */     2,  163,  163,    5,    6,  170,  107,  172,  177,   72,
 /*   700 */   111,   63,   64,  130,  198,  199,  200,  101,   71,   71,
 /*   710 */    75,  190,  184,  184,  193,  194,  195,  196,  197,  153,
 /*   720 */   154,  155,  156,  157,  158,  159,  160,  161,  162,  102,
 /*   730 */   205,  206,  207,  208,  209,    2,   48,   49,   50,   96,
 /*   740 */     1,    2,  105,  105,    5,    6,   74,  110,  110,   72,
 /*   750 */    74,   63,   64,  225,  226,  226,   81,   82,   83,   71,
 /*   760 */    85,   86,   87,   88,  106,  130,  131,   72,  130,  131,
 /*   770 */   132,  154,  155,  156,  157,  158,  159,  160,  163,  102,
 /*   780 */   105,  106,  107,  108,  109,   76,   71,   48,   49,   50,
 /*   790 */    81,    1,    2,  105,   73,  170,   76,  172,  110,  184,
 /*   800 */    76,  106,   63,   64,   55,   81,  189,   72,   75,    2,
 /*   810 */    71,  154,  155,  156,  157,  158,  159,  160,  130,  131,
 /*   820 */   132,   76,  102,   71,  167,  168,  105,  106,  107,  108,
 /*   830 */   109,  174,  175,  176,  177,  178,  179,  102,   72,   69,
 /*   840 */   225,  226,   72,  218,  105,    3,    4,  102,   76,  110,
 /*   850 */    71,   81,   82,   83,    2,   85,   86,   87,   88,   71,
 /*   860 */     1,    2,  155,  130,    5,    6,  211,  212,  102,  130,
 /*   870 */   131,  132,   72,  166,  102,  105,  106,  107,  108,  109,
 /*   880 */   153,  154,  155,  156,  157,  158,  159,  160,  161,  162,
 /*   890 */    71,  154,  155,  156,  157,  158,  159,  160,   71,   62,
 /*   900 */     2,   89,  102,  102,  167,  168,    2,   48,   49,   50,
 /*   910 */    72,  174,  175,  176,  177,  178,  179,  105,  106,  107,
 /*   920 */   108,  109,  132,  154,  155,  156,  157,  158,  159,  160,
 /*   930 */    71,   94,   95,   96,   97,   98,  167,  168,  101,  163,
 /*   940 */   102,   72,   76,  174,  175,  176,  177,  178,  179,  220,
 /*   950 */   221,   90,   91,  154,  155,  156,  157,  158,  159,  160,
 /*   960 */   184,  101,  185,  186,  105,   73,  167,  168,  102,  110,
 /*   970 */    73,  102,   73,  174,  175,  176,  177,  178,  179,  154,
 /*   980 */   155,  156,  157,  158,  159,  160,   72,   72,    2,  130,
 /*   990 */   131,  132,  167,  168,   76,   69,    2,   76,   72,  174,
 /*  1000 */   175,  176,  177,  178,  179,   73,   72,   81,   82,   83,
 /*  1010 */    73,   85,   86,   87,   88,  223,  224,  102,   73,  105,
 /*  1020 */   106,  107,  108,  109,  154,  155,  156,  157,  158,  159,
 /*  1030 */   160,  105,  106,  107,  108,  109,   73,  167,  168,  105,
 /*  1040 */   106,  107,  108,  109,  174,  175,  176,  177,  178,  179,
 /*  1050 */   154,  155,  156,  157,  158,  159,  160,   71,  170,  130,
 /*  1060 */   172,   73,   70,  167,  168,  105,  106,  107,  108,  109,
 /*  1070 */   174,  175,  176,  177,  178,  179,  203,  204,  154,  155,
 /*  1080 */   156,  157,  158,  159,  160,   73,   94,   95,   96,   97,
 /*  1090 */    98,  167,  168,  101,  206,  207,  208,  209,  174,  175,
 /*  1100 */   176,  177,  178,  179,  223,  224,  223,  224,  154,  155,
 /*  1110 */   156,  157,  158,  159,  160,   73,   72,  223,  224,  136,
 /*  1120 */   137,  167,  168,   73,  130,   89,  185,  186,  174,  175,
 /*  1130 */   176,  177,  178,  179,  154,  155,  156,  157,  158,  159,
 /*  1140 */   160,  105,  106,  107,  108,  109,   73,  167,  168,  105,
 /*  1150 */   106,  107,  108,  109,  174,  175,  176,  177,  178,  179,
 /*  1160 */    81,   82,   83,   73,   85,   86,   87,   88,  105,  106,
 /*  1170 */   107,  108,  109,   65,   73,   73,  154,  155,  156,  157,
 /*  1180 */   158,  159,  160,   73,  105,  106,  107,  108,  109,  167,
 /*  1190 */   168,  163,  158,  131,  155,  130,  174,  175,  176,  177,
 /*  1200 */   178,  179,  154,  155,  156,  157,  158,  159,  160,  158,
 /*  1210 */    72,  158,  233,   42,  232,  167,  168,  105,  106,  107,
 /*  1220 */   108,  109,  174,  175,  176,  177,  178,  179,  154,  155,
 /*  1230 */   156,  157,  158,  159,  160,   26,  234,   61,   70,  235,
 /*  1240 */    62,  167,  168,  105,  106,  107,  108,  109,  174,  175,
 /*  1250 */   176,  177,  178,  179,  154,  155,  156,  157,  158,  159,
 /*  1260 */   160,  234,   94,   95,   96,   97,   98,  167,  168,  101,
 /*  1270 */   235,  155,  234,  155,  174,  175,  176,  177,  178,  179,
 /*  1280 */   235,  234,  202,  154,  155,  156,  157,  158,  159,  160,
 /*  1290 */   235,  234,    2,   70,  235,  209,  167,  168,    2,  233,
 /*  1300 */   232,  234,  233,  174,  175,  176,  177,  178,  179,  154,
 /*  1310 */   155,  156,  157,  158,  159,  160,  235,   94,   95,   96,
 /*  1320 */    97,   98,  167,  168,  101,  232,  234,  155,  234,  174,
 /*  1330 */   175,  176,  177,  178,  179,  154,  155,  156,  157,  158,
 /*  1340 */   159,  160,  235,   41,  235,  234,  231,   72,  167,  168,
 /*  1350 */   105,  106,  107,  108,  109,  174,  175,  176,  177,  178,
 /*  1360 */   179,  154,  155,  156,  157,  158,  159,  160,  235,   94,
 /*  1370 */    95,   96,   97,   98,  167,  168,  101,  234,  233,  235,
 /*  1380 */   234,  174,  175,  176,  177,  178,  179,  154,  155,  156,
 /*  1390 */   157,  158,  159,  160,  235,   72,  232,  234,  155,  233,
 /*  1400 */   167,  168,  105,  106,  107,  108,  109,  174,  175,  176,
 /*  1410 */   177,  178,  179,  154,  155,  156,  157,  158,  159,  160,
 /*  1420 */   235,  231,   73,  232,  235,  155,  167,  168,  105,  106,
 /*  1430 */   107,  108,  109,  174,  175,  176,  177,  178,  179,  154,
 /*  1440 */   155,  156,  157,  158,  159,  160,  158,  234,  231,  233,
 /*  1450 */   232,  163,  167,  168,  105,  106,  107,  108,  109,  174,
 /*  1460 */   175,  176,  177,  178,  179,  154,  155,  156,  157,  158,
 /*  1470 */   159,  160,  184,  234,  234,  187,  188,  158,  167,  168,
 /*  1480 */   155,   67,  163,  235,  235,  174,  175,  176,  177,  178,
 /*  1490 */   179,  154,  155,  156,  157,  158,  159,  160,  235,  231,
 /*  1500 */   231,  186,  204,  184,  167,  168,  187,  188,   94,   95,
 /*  1510 */    96,   97,   98,  176,  177,  101,  154,  155,  156,  157,
 /*  1520 */   158,  159,  160,  231,   94,   95,   96,   97,   98,  167,
 /*  1530 */   168,  101,  231,  157,  231,  198,  199,  200,  176,  177,
 /*  1540 */   173,  154,  155,  156,  157,  158,  159,  160,  173,  231,
 /*  1550 */   173,  173,   72,  173,  167,  168,  173,  173,  173,  173,
 /*  1560 */   198,  199,  200,  176,  177,  173,  154,  155,  156,  157,
 /*  1570 */   158,  159,  160,  173,   94,   95,   96,   97,   98,  167,
 /*  1580 */   168,  101,  173,  173,  231,  198,  199,  200,  176,  177,
 /*  1590 */   154,  155,  156,  157,  158,  159,  160,  232,  154,  155,
 /*  1600 */   156,  157,  158,  159,  160,  235,  235,  172,    2,  172,
 /*  1610 */   198,  199,  200,  177,  154,  155,  156,  157,  158,  159,
 /*  1620 */   160,  177,  232,  232,  172,  232,  190,  167,  168,  193,
 /*  1630 */   194,  195,  196,  197,  190,   70,  231,  193,  194,  195,
 /*  1640 */   196,  197,  154,  155,  156,  157,  158,  159,  160,  172,
 /*  1650 */   154,  155,  156,  157,  158,  159,  160,  172,  172,   94,
 /*  1660 */    95,   96,   97,   98,  172,  177,  101,   94,   95,   96,
 /*  1670 */    97,   98,  172,  177,  101,  172,    2,  254,  190,  254,
 /*  1680 */   254,  193,  194,  195,  196,  197,  190,  254,  254,  193,
 /*  1690 */   194,  195,  196,  197,  154,  155,  156,  157,  158,  159,
 /*  1700 */   160,  254,  154,  155,  156,  157,  158,  159,  160,  154,
 /*  1710 */   155,  156,  157,  158,  159,  160,  254,  177,  254,  254,
 /*  1720 */   254,  254,  167,  168,  254,  177,   95,   96,   97,   98,
 /*  1730 */   190,  254,  101,  193,  194,  195,  196,  197,  190,   72,
 /*  1740 */   254,  193,  194,  195,  196,  197,  154,  155,  156,  157,
 /*  1750 */   158,  159,  160,  254,  154,  155,  156,  157,  158,  159,
 /*  1760 */   160,   94,   95,   96,   97,   98,  254,  254,  101,  177,
 /*  1770 */   154,  155,  156,  157,  158,  159,  160,  177,  254,  254,
 /*  1780 */   254,  254,  190,  167,  168,  193,  194,  195,  196,  197,
 /*  1790 */   190,  254,  254,  193,  194,  195,  196,  197,  154,  155,
 /*  1800 */   156,  157,  158,  159,  160,  254,  154,  155,  156,  157,
 /*  1810 */   158,  159,  160,  254,   69,  254,  254,  254,  254,  254,
 /*  1820 */   254,  177,  254,  254,  254,  254,   81,   82,   83,  177,
 /*  1830 */    85,   86,   87,   88,  190,  254,  254,  193,  194,  195,
 /*  1840 */   196,  197,  190,  254,  254,  193,  194,  195,  196,  197,
 /*  1850 */   105,  106,  107,  108,  109,   69,  254,  254,  254,   81,
 /*  1860 */    82,   83,  254,   85,   86,   87,   88,   81,   82,   83,
 /*  1870 */   254,   85,   86,   87,   88,   81,   82,   83,  254,   85,
 /*  1880 */    86,   87,   88,  105,  106,  107,  108,  109,  254,  254,
 /*  1890 */   254,  105,  106,  107,  108,  109,  254,  254,  254,  105,
 /*  1900 */   106,  107,  108,  109,  254,  254,  254,  254,   81,   82,
 /*  1910 */    83,  254,   85,   86,   87,   88,  254,  254,  254,  254,
 /*  1920 */   254,  254,  254,  254,  154,  155,  156,  157,  158,  159,
 /*  1930 */   160,  254,  105,  106,  107,  108,  109,  254,  254,  169,
 /*  1940 */   254,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  1950 */   254,  181,  182,  183,  254,  254,  169,  154,  155,  156,
 /*  1960 */   157,  158,  159,  160,  254,  254,  254,  254,  181,  182,
 /*  1970 */   183,  254,  169,  254,  254,  254,  154,  155,  156,  157,
 /*  1980 */   158,  159,  160,  254,  181,  182,  183,  254,  254,  254,
 /*  1990 */   254,  169,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2000 */   254,  254,  254,  181,  182,  183,  254,  169,  154,  155,
 /*  2010 */   156,  157,  158,  159,  160,  254,    1,    2,  254,  181,
 /*  2020 */   182,  183,  254,  169,  154,  155,  156,  157,  158,  159,
 /*  2030 */   160,  254,  254,  254,  254,  181,  182,  183,  254,  169,
 /*  2040 */   154,  155,  156,  157,  158,  159,  160,    1,    2,   70,
 /*  2050 */   254,  181,  182,  183,  254,  169,  154,  155,  156,  157,
 /*  2060 */   158,  159,  160,   48,   49,   50,  254,  181,  182,  183,
 /*  2070 */   254,  169,  254,   94,   95,   96,   97,   98,   63,   64,
 /*  2080 */   101,  254,  254,  181,  182,  183,   71,  154,  155,  156,
 /*  2090 */   157,  158,  159,  160,   48,   49,   50,  254,    1,  254,
 /*  2100 */   254,  254,  169,  254,  254,  254,   26,  254,  254,   63,
 /*  2110 */    64,  254,   70,  254,  181,  182,  183,   71,  254,  254,
 /*  2120 */   105,   41,   42,  254,  254,  110,  153,  154,  155,  156,
 /*  2130 */   157,  158,  159,  160,  161,  162,   94,   95,   96,   97,
 /*  2140 */    98,   61,   62,  101,  254,   48,   49,   50,  254,  254,
 /*  2150 */   254,  105,   72,   73,  254,  254,  110,  254,  254,   79,
 /*  2160 */    63,   64,   94,   95,   96,   97,   98,  254,   71,  101,
 /*  2170 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2180 */   254,  101,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2190 */   254,  162,  254,  254,  153,  154,  155,  156,  157,  158,
 /*  2200 */   159,  160,  105,  162,  254,  254,  254,  110,  254,  153,
 /*  2210 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2220 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  254,
 /*  2230 */   254,  254,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2240 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2250 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2260 */   254,  162,  254,  153,  154,  155,  156,  157,  158,  159,
 /*  2270 */   160,  254,  162,  254,  153,  154,  155,  156,  157,  158,
 /*  2280 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2290 */   159,  160,  254,  162,  254,  153,  154,  155,  156,  157,
 /*  2300 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2310 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2320 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2330 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2340 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2350 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2360 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2370 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2380 */   158,  159,  160,  254,  162,  254,  153,  154,  155,  156,
 /*  2390 */   157,  158,  159,  160,  254,  162,  154,  155,  156,  157,
 /*  2400 */   158,  159,  160,  254,  254,  254,  164,  165,  153,  154,
 /*  2410 */   155,  156,  157,  158,  159,  160,  254,  162,  153,  154,
 /*  2420 */   155,  156,  157,  158,  159,  160,  254,  162,  153,  154,
 /*  2430 */   155,  156,  157,  158,  159,  160,  254,  162,  153,  154,
 /*  2440 */   155,  156,  157,  158,  159,  160,  254,  162,  254,  153,
 /*  2450 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2460 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2470 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2480 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2490 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2500 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2510 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2520 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2530 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  254,
 /*  2540 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2550 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2560 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2570 */   254,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2580 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2590 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2600 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2610 */   162,  254,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2620 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2630 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2640 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2650 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2660 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2670 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2680 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2690 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2700 */   254,  162,  254,  153,  154,  155,  156,  157,  158,  159,
 /*  2710 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2720 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2730 */   160,  254,  162,  254,  153,  154,  155,  156,  157,  158,
 /*  2740 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2750 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2760 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2770 */   159,  160,  254,  162,  254,  153,  154,  155,  156,  157,
 /*  2780 */   158,  159,  160,  254,  162,  254,  154,  155,  156,  157,
 /*  2790 */   158,  159,  160,  170,  254,  172,  254,  165,  154,  155,
 /*  2800 */   156,  157,  158,  159,  160,  170,  254,  172,  254,  165,
 /*  2810 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  2820 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  206,
 /*  2830 */   207,  208,  209,  165,  154,  155,  156,  157,  158,  159,
 /*  2840 */   160,  206,  207,  208,  209,  165,  154,  155,  156,  157,
 /*  2850 */   158,  159,  160,  254,  254,  254,  254,  165,  154,  155,
 /*  2860 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  2870 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  2880 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2890 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  2900 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  2910 */   158,  159,  160,  254,  254,  254,  254,  165,  154,  155,
 /*  2920 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  2930 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  2940 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2950 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  2960 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  2970 */   158,  159,  160,  254,  254,  254,  254,  165,  154,  155,
 /*  2980 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  2990 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3000 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3010 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  3020 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  3030 */   158,  159,  160,  254,  254,  254,  254,  165,  154,  155,
 /*  3040 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  3050 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3060 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3070 */   254,  254,  154,  155,  156,  157,  158,  159,  160,  154,
 /*  3080 */   155,  156,  157,  158,  159,  160,  154,  155,  156,  157,
 /*  3090 */   158,  159,  160,  254,  254,  254,  254,  189,  154,  155,
 /*  3100 */   156,  157,  158,  159,  160,  254,  254,  189,  254,   95,
 /*  3110 */    96,   97,   98,  254,  189,  101,  254,  254,  254,  254,
 /*  3120 */   254,  189,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3130 */   254,  254,  254,  189,  154,  155,  156,  157,  158,  159,
 /*  3140 */   160,  254,  254,  254,  154,  155,  156,  157,  158,  159,
 /*  3150 */   160,  254,  254,  254,  254,  254,  254,  189,  154,  155,
 /*  3160 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  189,
 /*  3170 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  189,
 /*  3180 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3190 */   254,   26,  254,  189,  154,  155,  156,  157,  158,  159,
 /*  3200 */   160,  254,  254,  254,  254,  189,  154,  155,  156,  157,
 /*  3210 */   158,  159,  160,  254,  254,  189,  154,  155,  156,  157,
 /*  3220 */   158,  159,  160,  254,  254,  254,  254,  254,  254,  189,
 /*  3230 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3240 */   254,  189,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3250 */   254,  189,  154,  155,  156,  157,  158,  159,  160,   94,
 /*  3260 */    95,   96,   97,   98,  254,  189,  101,  254,  254,  154,
 /*  3270 */   155,  156,  157,  158,  159,  160,  254,  189,  154,  155,
 /*  3280 */   156,  157,  158,  159,  160,  254,  254,  189,  154,  155,
 /*  3290 */   156,  157,  158,  159,  160,  154,  155,  156,  157,  158,
 /*  3300 */   159,  160,  254,  254,  189,  154,  155,  156,  157,  158,
 /*  3310 */   159,  160,  254,  189,  154,  155,  156,  157,  158,  159,
 /*  3320 */   160,   41,  254,  189,  254,  254,  254,  254,  254,   70,
 /*  3330 */   189,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  3340 */   189,  154,  155,  156,  157,  158,  159,  160,  254,  189,
 /*  3350 */   254,   73,  165,   94,   95,   96,   97,   98,  254,  254,
 /*  3360 */   101,  254,  254,  254,  254,  254,  189,  254,  254,  254,
 /*  3370 */   254,  254,  254,  254,   94,   95,   96,   97,   98,  254,
 /*  3380 */   254,  101,  254,  105,  106,  107,  108,  109,
};
#define YY_SHIFT_USE_DFLT (-80)
#define YY_SHIFT_COUNT (551)
#define YY_SHIFT_MIN   (-79)
#define YY_SHIFT_MAX   (3280)
static const short yy_shift_ofst[] = {
 /*     0 */   -80,  118,  208,  283,  283,  208,  232,  232,  283,  283,
 /*    10 */   283,  283,  283,  283,  283,  283,  283,  283,  283,  283,
 /*    20 */   283,  283,  283,  283,  283,  283,  283,  283,  283,  283,
 /*    30 */   283,  232,  232,  232,  232,  232,  232,  232,  232,  232,
 /*    40 */   232,  232,  232,  355,  355,  355,  355,  355,  355,  461,
 /*    50 */   461,  461,  461,  461,  461,  461,  461,  461,  461,  739,
 /*    60 */   739,  739,  739,  739,  739,  739,  739,  739,  739,  739,
 /*    70 */   739,  739,  739,  739,  739,  739,  739,  739,  739,  739,
 /*    80 */   739,  739,  739,  739,  739,  739,  739,  739,  739,  739,
 /*    90 */   739,  739,  739,  739,  739,  739,  739,  739,  739,  739,
 /*   100 */   739,  638,  739,  739,  739,  739,  739,  739,  739,  739,
 /*   110 */   739,  739,  739,  739,  739,  739,  739,  739,  739,  739,
 /*   120 */   739,  739,  739,  859,  859,  859,  182,  638,  688,  688,
 /*   130 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*   140 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*   150 */   688,  688,  688,  638,  638,  638,  638,  638,  638,  638,
 /*   160 */   638,  638,  638,  638,  638,  638,  638,  638,  638,  638,
 /*   170 */   638,  638,  638,  638,  638,  638, 2046, 2046, 2046, 2046,
 /*   180 */  2046, 2015, 2046, 2046, 2046, 2015,  635,  635,  542,  437,
 /*   190 */   733, 2015,  515,  515,  573,  573,  986, 1674, 1606,  637,
 /*   200 */   861,  986,  986,  986,  986,  842,  994,  861,  573,  573,
 /*   210 */  1674, 1606, 1290, 2097, 2097, 2097, 2097, 2097, 2097, 2097,
 /*   220 */  2097, 2097, 3280, 3165, 3165, 3165, 3165,  837,  837,  110,
 /*   230 */   110,  373,  373,  373,  373,  373,  373,  373,  373,  373,
 /*   240 */   373,  373,  373,  373,  379,  790,  153,  443,  606,  606,
 /*   250 */   842,  606,  606, 1178, 1065, 1178, 1176, 1178, 1176, 1209,
 /*   260 */  1171, 1302, 1065, 1178, 1176, 1209, 1171, 1302, 1065, 1178,
 /*   270 */  1176, 1209, 1171, 1302, 1065, 1178, 1176, 1178, 1176, 1178,
 /*   280 */  1176, 1178, 1176, 1178, 1176, 1209, 1171, 1178, 1176, 1209,
 /*   290 */  1171, 1296, 1290, 1178, 1176, 1178, 1176, 1065, 1178, 1176,
 /*   300 */  1065, 1178, 1176, 1178, 1176, 1209, 1171, 1062, 1062, 1065,
 /*   310 */  1062,  929,  549,  433,  926,  770,  517, 1786, 1745, 1827,
 /*   320 */  1794, 1778, 1079,  675,  431, 2080, 1667, 3259, 2042, 1979,
 /*   330 */  1565, 1414,  -29, 1480, 1275, 1223, 1168,  992,  275,  345,
 /*   340 */    27, 1430, 1430, 1036,  -79,  -79,  -79,  -79,  -79,  -79,
 /*   350 */   -79,  -79,  -79,  -79,  -79, 1573, 3278, 1349,  721,  -21,
 /*   360 */  1323,  812, 1138, 2068, 1573, 1573, 1573, 1044,  934,  914,
 /*   370 */  1297, 3014,  562, 1245, 1631,  960,  960,  960,  960,  960,
 /*   380 */   960,  960,  562,  562,  562,  562,  562,  213,  562,  562,
 /*   390 */   562,  562,  562,  562,  562,  562,  562,  562,  562,  562,
 /*   400 */   562,  562,  562,  562, 1112, 1063,  562,  562,  960,  960,
 /*   410 */   960,  562,  562,  562,  562,  562,  562,  562,  562,  562,
 /*   420 */   111,  218,  218,  299,  299,  299,  299,  221,  221,  200,
 /*   430 */   197,   49,   87,   87,  128,  128,   83,   83,  -48,  -48,
 /*   440 */   -63,  -63,  -63,  -63,   83,   83,  -63,  -63,  -63,  -63,
 /*   450 */   626,  489,  430,  724,  709,  317,  317,  317,  317,  415,
 /*   460 */   866,  388,  915,  772,  336,  745,  869,  838,  695,  800,
 /*   470 */   269,  589,  720,  162,  766,  405,  115,  735,  499,   84,
 /*   480 */   253,  677,  627,  471,   59,  -41, 1108, 1110, 1102, 1101,
 /*   490 */  1090, 1073, 1050, 1042, 1012,  988,  963,  945,  937,  932,
 /*   500 */   899,  921,  918,  860,  860,  897,  892,  904,  801,  898,
 /*   510 */   658,  827,  819,  788,  852,  779,  658,  658,  752,  807,
 /*   520 */   749,  715,  658,  676,  672,  643,  586,  546,  552,  507,
 /*   530 */   532,  380,  352,  319,  258,  258,  304,  272,  249,   63,
 /*   540 */    63,  247,  224,  151,  141,   78,   74,   51,   10,   -5,
 /*   550 */   -53,    2,
};
#define YY_REDUCE_USE_DFLT (-136)
#define YY_REDUCE_COUNT (311)
#define YY_REDUCE_MIN   (-135)
#define YY_REDUCE_MAX   (3187)
static const short yy_reduce_ofst[] = {
 /*     0 */   983, -135,  278,  223, -120,  456,  391,  340, 1311, 1285,
 /*    10 */  1259, 1233, 1207, 1181, 1155, 1129, 1100, 1074, 1048, 1022,
 /*    20 */   980,  954,  924,  896,  870,  825,  799,  769,  737,  657,
 /*    30 */   -85, 1652, 1644, 1600, 1592, 1548, 1540, 1496, 1488, 1444,
 /*    40 */  1436,  521,  -17, 1412, 1387, 1362, 1337,  506,  167, 1933,
 /*    50 */  1902, 1886, 1870, 1854, 1838, 1822, 1803, 1787, 1770, 1973,
 /*    60 */   727,  566,  399,  106, 2622, 2611, 2601, 2591, 2581, 2570,
 /*    70 */  2560, 2550, 2539, 2529, 2519, 2509, 2499, 2489, 2479, 2469,
 /*    80 */  2459, 2448, 2438, 2428, 2418, 2407, 2397, 2387, 2376, 2366,
 /*    90 */  2356, 2346, 2336, 2326, 2316, 2306, 2296, 2285, 2275, 2265,
 /*   100 */  2255, 2242, 2233, 2222, 2212, 2202, 2192, 2182, 2172, 2162,
 /*   110 */  2152, 2142, 2131, 2121, 2110, 2099, 2089, 2079, 2066, 2056,
 /*   120 */  2041, 2029, 2017, 1616, 1555, 1460,  214, 3187, 3177, 3160,
 /*   130 */  3151, 3141, 3134, 3124, 3115, 3098, 3088, 3076, 3062, 3052,
 /*   140 */  3040, 3026, 3016, 3004, 2990, 2980, 2968, 2944, 2932, 2925,
 /*   150 */  2918, 2908,  617, 2896, 2884, 2872, 2860, 2848, 2836, 2824,
 /*   160 */  2812, 2800, 2788, 2776, 2764, 2752, 2740, 2728, 2716, 2704,
 /*   170 */  2692, 2680, 2668, 2656, 2644, 2632,  525,  284,   -7, -110,
 /*   180 */  2635,  359, 2623,  888,  400,  178, 1319, 1288,  615,  528,
 /*   190 */   120,  625,  327,   92,  529,  205,  372,  238,   94,  105,
 /*   200 */   941,  894,  883,  881,  792,  873,  707,  777,  776,   40,
 /*   210 */   729,  655,  440, 1503, 1500, 1492, 1486, 1485, 1477, 1452,
 /*   220 */  1437, 1435, 1405, 1393, 1391, 1390, 1365, 1371, 1370, 1353,
 /*   230 */  1318, 1410, 1409, 1400, 1392, 1386, 1385, 1384, 1383, 1380,
 /*   240 */  1378, 1377, 1375, 1367, 1315, 1376, 1315, 1303, 1301, 1292,
 /*   250 */  1298, 1269, 1268, 1263, 1325, 1249, 1240, 1248, 1239, 1218,
 /*   260 */  1216, 1217, 1270, 1189, 1213, 1191, 1166, 1190, 1243, 1185,
 /*   270 */  1163, 1164, 1145, 1115, 1172, 1159, 1146, 1144, 1143, 1133,
 /*   280 */  1111, 1109, 1094, 1107, 1092, 1093, 1069, 1081, 1067, 1068,
 /*   290 */  1066, 1086, 1080, 1059, 1057, 1055, 1047, 1118, 1045, 1038,
 /*   300 */  1116, 1035, 1027, 1004, 1002,  982,  979, 1053, 1051, 1039,
 /*   310 */  1034, 1028,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   864, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*    10 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*    20 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*    30 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*    40 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*    50 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*    60 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1075, 1079,
 /*    70 */  1074, 1078, 1164, 1160, 1317, 1317, 1317, 1317, 1317, 1317,
 /*    80 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*    90 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1165,
 /*   100 */  1161, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   110 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   120 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1144, 1148,
 /*   130 */  1143, 1147, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   140 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   150 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   160 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   170 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   180 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   190 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   200 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   210 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   220 */  1317, 1317, 1265, 1267, 1267, 1267, 1267, 1273, 1273, 1265,
 /*   230 */  1265, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   240 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1265, 1265, 1265,
 /*   250 */  1317, 1265, 1265, 1273, 1317, 1273, 1271, 1273, 1271, 1267,
 /*   260 */  1269, 1265, 1317, 1273, 1271, 1267, 1269, 1265, 1317, 1273,
 /*   270 */  1271, 1267, 1269, 1265, 1317, 1273, 1271, 1273, 1271, 1273,
 /*   280 */  1271, 1273, 1271, 1273, 1271, 1267, 1269, 1273, 1271, 1267,
 /*   290 */  1269, 1317, 1317, 1273, 1271, 1273, 1271, 1317, 1273, 1271,
 /*   300 */  1317, 1273, 1271, 1273, 1271, 1267, 1269, 1317, 1317, 1317,
 /*   310 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1110,
 /*   320 */  1317, 1039, 1039, 1317, 1317,  930, 1317, 1317, 1317, 1317,
 /*   330 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   340 */  1317, 1257, 1254, 1317, 1146, 1150, 1145, 1149, 1141, 1140,
 /*   350 */  1139, 1138, 1137, 1136, 1135, 1128, 1317, 1317, 1317, 1317,
 /*   360 */  1317, 1317, 1317, 1272, 1266, 1268, 1264, 1317, 1317, 1317,
 /*   370 */   993, 1125, 1097,  992, 1053, 1065, 1064, 1063, 1062, 1061,
 /*   380 */  1060, 1059, 1045, 1077, 1081, 1076, 1080, 1010, 1166, 1162,
 /*   390 */  1041, 1038, 1037, 1036, 1035, 1034, 1033, 1032, 1031, 1030,
 /*   400 */  1029, 1028, 1027, 1026, 1317, 1317, 1167, 1163, 1068,  903,
 /*   410 */   904, 1025, 1024, 1023, 1022, 1021, 1020, 1019,  900,  899,
 /*   420 */  1158, 1127, 1126, 1114, 1113, 1098, 1099,  998,  999, 1317,
 /*   430 */  1317, 1317,  987,  988, 1055, 1054,  957,  956, 1012, 1011,
 /*   440 */   932,  931,  937,  936,  973,  974,  942,  941,  916,  917,
 /*   450 */  1317, 1317,  994, 1317, 1317, 1231, 1235, 1234, 1232, 1317,
 /*   460 */  1317, 1317, 1317, 1317, 1317,  978, 1317, 1317, 1317, 1317,
 /*   470 */  1317, 1179, 1317, 1317, 1317, 1317, 1317, 1317,  884, 1317,
 /*   480 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   490 */  1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   500 */  1317, 1317, 1107, 1124, 1123, 1317, 1317, 1317, 1233, 1317,
 /*   510 */  1227, 1220, 1197, 1199, 1317, 1212, 1178, 1177, 1195, 1317,
 /*   520 */  1317, 1194, 1193, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
 /*   530 */  1317, 1317, 1317, 1317, 1052, 1051, 1043, 1039,  888, 1009,
 /*   540 */  1008, 1317, 1317, 1317, 1317, 1066,  902,  898,  896,  892,
 /*   550 */   887, 1317, 1316, 1315, 1314, 1313, 1312, 1311, 1310, 1309,
 /*   560 */  1308, 1307, 1306, 1305, 1159, 1304, 1303, 1302, 1301, 1296,
 /*   570 */  1294, 1293, 1291, 1290, 1289, 1288, 1287, 1286, 1285, 1284,
 /*   580 */  1283, 1282, 1281, 1280, 1279, 1278, 1277, 1276, 1275, 1274,
 /*   590 */  1248, 1247, 1256, 1255, 1263, 1262, 1259, 1258, 1253, 1261,
 /*   600 */  1119, 1121, 1142, 1134, 1133, 1132, 1131, 1130, 1129, 1122,
 /*   610 */  1120, 1118, 1112, 1111, 1109, 1108, 1107, 1117, 1116, 1115,
 /*   620 */  1102, 1101, 1100, 1096, 1095, 1094, 1093, 1092, 1091, 1090,
 /*   630 */  1089, 1088, 1087, 1086, 1085, 1106, 1105, 1104, 1103, 1252,
 /*   640 */  1251, 1250, 1249, 1002, 1001, 1000,  997,  996,  995,  994,
 /*   650 */  1242, 1241, 1243, 1240, 1245, 1246, 1244, 1239, 1237, 1236,
 /*   660 */  1238, 1230, 1225, 1228, 1229, 1226, 1224, 1215, 1218, 1223,
 /*   670 */  1221, 1219, 1217, 1216, 1214, 1190, 1198, 1200, 1213, 1211,
 /*   680 */  1210, 1209, 1208, 1207, 1206, 1205, 1204, 1203, 1202, 1201,
 /*   690 */  1196, 1192, 1188, 1187, 1186, 1185, 1184, 1183, 1182,  892,
 /*   700 */  1181, 1180,  991,  990,  989,  986,  985,  984,  983,  982,
 /*   710 */   981,  980,  979,  978, 1191, 1189, 1169, 1172, 1173, 1176,
 /*   720 */  1175, 1174, 1171, 1170, 1168, 1300, 1299, 1298, 1297, 1295,
 /*   730 */  1292, 1047, 1049, 1058, 1057, 1056, 1050, 1048, 1046,  955,
 /*   740 */   954,  953,  952,  951,  950,  960,  959,  958,  949,  948,
 /*   750 */   947,  946, 1270, 1042, 1044,  889, 1004, 1006, 1070, 1073,
 /*   760 */  1072, 1071, 1069, 1018, 1017, 1016, 1015, 1014, 1013, 1007,
 /*   770 */  1005, 1003,  928, 1152, 1158, 1157, 1156, 1155, 1154, 1153,
 /*   780 */  1151, 1040,  935,  934,  933,  940,  939,  938,  930,  929,
 /*   790 */   928,  927,  926,  925, 1083, 1084, 1082, 1067,  975,  977,
 /*   800 */   976,  972,  971,  970,  969,  968,  967,  966,  965,  964,
 /*   810 */   963,  962,  961,  901,  945,  944,  943,  924,  923,  922,
 /*   820 */   921,  918,  920,  919,  915,  914,  913,  912,  911,  910,
 /*   830 */   909,  908,  907,  906,  905,  897,  895,  894,  893,  891,
 /*   840 */   890,  886,  882,  881,  885,  884,  883,  880,  879,  878,
 /*   850 */   877,  876,  875,  874,  873,  872,  871,  870,  869,  868,
 /*   860 */   867,  866,  865,
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
 /* 289 */ "head_formula ::= PAREN_L head_formula PAREN_R",
 /* 290 */ "head_formula ::= comparison",
 /* 291 */ "head_formula ::= atomic_head_formula",
 /* 292 */ "head_formula ::= formula_smpl_card",
 /* 293 */ "head_formula ::= TRUE",
 /* 294 */ "head_formula ::= FALSE",
 /* 295 */ "atomic_head_formula ::= atomic_formula",
 /* 296 */ "atomic_head_formula ::= DASH constant",
 /* 297 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 298 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 299 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 300 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 301 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 302 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 303 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 304 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 305 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 306 */ "macro_def_lst ::= macro_bnd",
 /* 307 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 308 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 309 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 310 */ "macro_args ::= macro_arg",
 /* 311 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 312 */ "macro_arg ::= POUND_INTEGER",
 /* 313 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 314 */ "sort_lst ::= sort",
 /* 315 */ "sort_lst ::= sort_lst COMMA sort",
 /* 316 */ "sort ::= sort_id_nr",
 /* 317 */ "sort ::= sort_id_nr STAR",
 /* 318 */ "sort ::= sort_id_nr CARROT",
 /* 319 */ "sort ::= sort PLUS object_nullary",
 /* 320 */ "sort ::= sort PLUS IDENTIFIER",
 /* 321 */ "sort ::= sort PLUS INTEGER",
 /* 322 */ "sort_id_nr ::= sort_id",
 /* 323 */ "sort_id_nr ::= sort_nr",
 /* 324 */ "sort_nr ::= num_range",
 /* 325 */ "sort_id ::= IDENTIFIER",
 /* 326 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 327 */ "constant_bnd_lst ::= constant_bnd",
 /* 328 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 329 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 330 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 331 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 332 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 333 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 334 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 335 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 336 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 337 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 338 */ "constant_dcl_type ::= ABACTION",
 /* 339 */ "constant_dcl_type ::= ACTION",
 /* 340 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 341 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 342 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 343 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 344 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 345 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 346 */ "constant_dcl_type ::= RIGID",
 /* 347 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 348 */ "constant_dcl_type ::= SDFLUENT",
 /* 349 */ "attrib_spec ::= ATTRIBUTE",
 /* 350 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 351 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 352 */ "object_bnd_lst ::= object_bnd",
 /* 353 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 354 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 355 */ "object_lst ::= object_spec",
 /* 356 */ "object_lst ::= object_lst COMMA object_spec",
 /* 357 */ "object_spec ::= IDENTIFIER",
 /* 358 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 359 */ "object_spec ::= INTEGER",
 /* 360 */ "object_spec ::= num_range",
 /* 361 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 362 */ "variable_bnd_lst ::= variable_bnd",
 /* 363 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 364 */ "variable_bnd ::= variable_lst DBL_COLON sort",
 /* 365 */ "variable_lst ::= IDENTIFIER",
 /* 366 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 367 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 368 */ "sort_bnd_lst ::= sort_bnd",
 /* 369 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 370 */ "sort_bnd ::= sort_dcl_lst",
 /* 371 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 372 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 373 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 374 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 375 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 376 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 377 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 378 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 379 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 380 */ "show_lst ::= show_elem",
 /* 381 */ "show_lst ::= show_lst COMMA show_elem",
 /* 382 */ "show_lst ::= show_lst SEMICOLON show_elem",
 /* 383 */ "show_elem ::= atomic_formula_one_const",
 /* 384 */ "stmt_noconcurrency ::= NOCONCURRENCY PERIOD",
 /* 385 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD",
 /* 386 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_int_eval PERIOD",
 /* 387 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE DBL_COLON term_int_eval PERIOD",
 /* 388 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_int_eval PERIOD",
 /* 389 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE DBL_COLON term_int_eval PERIOD",
 /* 390 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 391 */ "query_lst ::= formula_temporal",
 /* 392 */ "query_lst ::= query_maxstep_decl",
 /* 393 */ "query_lst ::= query_label_decl",
 /* 394 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 395 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 396 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 397 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 398 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range_eval",
 /* 399 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 400 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 401 */ "clause_if ::= IF formula",
 /* 402 */ "clause_if ::=",
 /* 403 */ "clause_after ::= AFTER formula",
 /* 404 */ "clause_after ::=",
 /* 405 */ "clause_ifcons ::= IFCONS formula",
 /* 406 */ "clause_ifcons ::=",
 /* 407 */ "clause_unless ::= UNLESS atomic_formula_anon",
 /* 408 */ "clause_unless ::=",
 /* 409 */ "clause_where ::= WHERE formula_no_const",
 /* 410 */ "clause_where ::=",
 /* 411 */ "stmt_law ::= law_basic",
 /* 412 */ "stmt_law ::= law_caused",
 /* 413 */ "stmt_law ::= law_pcaused",
 /* 414 */ "stmt_law ::= law_impl",
 /* 415 */ "stmt_law ::= law_causes",
 /* 416 */ "stmt_law ::= law_increments",
 /* 417 */ "stmt_law ::= law_decrements",
 /* 418 */ "stmt_law ::= law_mcause",
 /* 419 */ "stmt_law ::= law_always",
 /* 420 */ "stmt_law ::= law_constraint",
 /* 421 */ "stmt_law ::= law_impossible",
 /* 422 */ "stmt_law ::= law_never",
 /* 423 */ "stmt_law ::= law_default",
 /* 424 */ "stmt_law ::= law_exogenous",
 /* 425 */ "stmt_law ::= law_inertial",
 /* 426 */ "stmt_law ::= law_nonexecutable",
 /* 427 */ "stmt_law ::= law_rigid",
 /* 428 */ "stmt_law ::= law_observed",
 /* 429 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 430 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 431 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 432 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 433 */ "law_impl ::= ARROW_LDASH formula clause_where PERIOD",
 /* 434 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 435 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 436 */ "law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 437 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 438 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 439 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 440 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 441 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 442 */ "law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 443 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 444 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 445 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 446 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 447 */ "law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD",
 /* 448 */ "stmt_code_blk ::= ASP_GR",
 /* 449 */ "stmt_code_blk ::= ASP_CP",
 /* 450 */ "stmt_code_blk ::= F2LP_GR",
 /* 451 */ "stmt_code_blk ::= F2LP_CP",
 /* 452 */ "stmt_code_blk ::= LUA_GR",
 /* 453 */ "stmt_code_blk ::= LUA_CP",
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
#line 2572 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 136: /* start */
    case 137: /* statement_lst */
    case 160: /* undeclared */
{
#line 209 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2581 "bcplus/parser/detail/lemon_parser.c"
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
#line 2594 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 139: /* stmt_macro_def */
{
#line 234 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy151));								
#line 2601 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_constant_def */
{
#line 236 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy431));								
#line 2608 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_object_def */
{
#line 238 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy468));								
#line 2615 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_variable_def */
{
#line 240 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy459));								
#line 2622 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_sort_def */
{
#line 242 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy61));								
#line 2629 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 148: /* stmt_noconcurrency */
{
#line 252 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy485));								
#line 2636 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_strong_noconcurrency */
{
#line 254 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy446));								
#line 2643 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 152: /* stmt_query */
{
#line 260 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy466));								
#line 2650 "bcplus/parser/detail/lemon_parser.c"
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
#line 2668 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 155: /* constant */
    case 163: /* constant_one_const */
    case 166: /* const_anon */
{
#line 298 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy225));								
#line 2677 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* object */
    case 157: /* object_nullary */
{
#line 300 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy422));								
#line 2685 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 158: /* variable */
{
#line 304 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy189));								
#line 2692 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* lua */
{
#line 306 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy117));								
#line 2699 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 161: /* term_lst */
    case 164: /* term_no_const_lst */
{
#line 310 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy259));								
#line 2707 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 170: /* num_range */
{
#line 712 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy345));								
#line 2714 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* num_range_eval */
{
#line 714 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy29));								
#line 2721 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 173: /* term_int_eval */
{
#line 718 "bcplus/parser/detail/lemon_parser.y"
 /* Initially left Blank */				
#line 2728 "bcplus/parser/detail/lemon_parser.c"
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
#line 2746 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 177: /* atomic_formula */
    case 180: /* atomic_formula_anon */
    case 184: /* atomic_formula_one_const */
    case 199: /* atomic_head_formula */
{
#line 825 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy423));								
#line 2756 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 178: /* formula_quant */
    case 196: /* formula_temporal_quant */
{
#line 827 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy177));								
#line 2764 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_lst */
{
#line 1001 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy221));								
#line 2771 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* quant_op */
{
#line 1003 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2778 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 187: /* card_var_lst */
    case 188: /* card_var_lst_inner */
{
#line 1040 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy375));								
#line 2786 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* formula_smpl_card */
{
#line 1346 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy133));								
#line 2793 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* macro_def_lst */
{
#line 1406 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy269));                              
#line 2800 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* macro_bnd */
{
#line 1408 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy359));                              
#line 2807 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* macro_args */
{
#line 1410 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy154));                              
#line 2814 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* macro_arg */
{
#line 1412 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy79));                              
#line 2821 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* sort_lst */
{
#line 1502 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy195));							
#line 2828 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 206: /* sort */
    case 207: /* sort_id_nr */
    case 208: /* sort_nr */
    case 209: /* sort_id */
{
#line 1504 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2838 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* constant_bnd_lst */
    case 211: /* constant_bnd */
{
#line 1613 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy409));									
#line 2846 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* constant_dcl_lst */
{
#line 1617 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy226));									
#line 2853 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* constant_dcl_type */
{
#line 1619 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2860 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* attrib_spec */
{
#line 1621 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2867 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* object_bnd_lst */
{
#line 1980 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy102));									
#line 2874 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* object_bnd */
{
#line 1982 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy294));									
#line 2881 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* object_lst */
    case 218: /* object_spec */
{
#line 1984 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy425));									
#line 2889 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* variable_bnd_lst */
    case 220: /* variable_bnd */
{
#line 2116 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy410));									
#line 2897 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 221: /* variable_lst */
{
#line 2120 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy52));									
#line 2904 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 222: /* sort_bnd_lst */
    case 223: /* sort_bnd */
    case 224: /* sort_dcl_lst */
{
#line 2203 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy196));									
#line 2913 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 225: /* show_lst */
{
#line 2307 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy123));									
#line 2920 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 226: /* show_elem */
    case 234: /* clause_unless */
{
#line 2309 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy423));									
#line 2928 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 227: /* query_lst */
{
#line 2461 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy489).l); DEALLOC((yypminor->yy489).maxstep); DEALLOC((yypminor->yy489).label);	
#line 2935 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 228: /* query_maxstep_decl */
{
#line 2463 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy29));												
#line 2942 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 230: /* query_label_Decl */
{
#line 2465 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2949 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 231: /* clause_if */
    case 232: /* clause_after */
    case 233: /* clause_ifcons */
    case 235: /* clause_where */
{
#line 2619 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy17));									
#line 2959 "bcplus/parser/detail/lemon_parser.c"
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
#line 2660 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy484));									
#line 2983 "bcplus/parser/detail/lemon_parser.c"
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
  { 239, 4 },
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
#line 3742 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 220 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy484;
			yymsp[0].minor.yy484  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3751 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy151; }
#line 3756 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy431; }
#line 3761 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy468; }
#line 3766 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy459; }
#line 3771 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy61; }
#line 3776 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 268 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy484; }
#line 3786 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy485; }
#line 3791 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 273 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy446; }
#line 3796 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 276 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy466; }
#line 3801 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy225; }
#line 3806 "bcplus/parser/detail/lemon_parser.c"
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
#line 3817 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 325 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy422;	}
#line 3822 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 326 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy189; }
#line 3827 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 327 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy117; }
#line 3832 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 38: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==38);
#line 445 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy225, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3838 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 39: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==39);
#line 446 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy225, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3844 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* const_anon ::= IDENTIFIER */
#line 448 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy225, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true); }
#line 3849 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 449 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy225, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true);	}
#line 3854 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 452 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy422, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= object_nullary */
#line 453 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy422 = yymsp[0].minor.yy422; }
#line 3864 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* object_nullary ::= OBJECT_ID */
#line 454 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy422, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3869 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* object ::= undeclared */
#line 455 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3874 "bcplus/parser/detail/lemon_parser.c"
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
#line 3889 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 469 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy117, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0); }
#line 3894 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* lua ::= AT_IDENTIFIER */
#line 470 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy117, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3899 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 471 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy217, yymsp[-3].minor.yy0, yymsp[-1].minor.yy259);   yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 3906 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* undeclared ::= IDENTIFIER */
#line 472 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy217, yymsp[0].minor.yy0, NULL); }
#line 3911 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 36: /* term_lst ::= term */
      case 40: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==40);
#line 475 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy259 = new TermList();
			yygotominor.yy259->push_back(yymsp[0].minor.yy299);
		}
#line 3920 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 37: /* term_lst ::= term_lst COMMA term */
      case 41: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==41);
#line 481 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy259 = yymsp[-2].minor.yy259;
			yymsp[-2].minor.yy259->push_back(yymsp[0].minor.yy299);
		  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3930 "bcplus/parser/detail/lemon_parser.c"
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
#line 3941 "bcplus/parser/detail/lemon_parser.c"
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
#line 3959 "bcplus/parser/detail/lemon_parser.c"
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
#line 3970 "bcplus/parser/detail/lemon_parser.c"
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
#line 3981 "bcplus/parser/detail/lemon_parser.c"
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
#line 3992 "bcplus/parser/detail/lemon_parser.c"
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
#line 4003 "bcplus/parser/detail/lemon_parser.c"
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
#line 4014 "bcplus/parser/detail/lemon_parser.c"
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
#line 4025 "bcplus/parser/detail/lemon_parser.c"
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
#line 4037 "bcplus/parser/detail/lemon_parser.c"
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
#line 4049 "bcplus/parser/detail/lemon_parser.c"
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
#line 4061 "bcplus/parser/detail/lemon_parser.c"
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
#line 4073 "bcplus/parser/detail/lemon_parser.c"
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
#line 4085 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 67: /* term_strong_candidate ::= DASH constant */
#line 619 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy225, UnaryTerm::Operator::NEGATIVE); }
#line 4090 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 73: /* term_strong ::= constant DASH term */
#line 628 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MINUS); }
#line 4095 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant PLUS term */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::PLUS); }
#line 4100 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant STAR term */
#line 630 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::TIMES); }
#line 4105 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 76: /* term_strong ::= constant INT_DIV term */
#line 631 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::DIVIDE); }
#line 4110 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong ::= constant MOD term */
#line 632 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MOD); }
#line 4115 "bcplus/parser/detail/lemon_parser.c"
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
#line 4126 "bcplus/parser/detail/lemon_parser.c"
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
#line 4137 "bcplus/parser/detail/lemon_parser.c"
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
#line 4157 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* num_range_eval ::= term_int_eval DBL_PERIOD term_int_eval */
#line 762 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy396, r_ptr = yymsp[0].minor.yy396, s_ptr = yymsp[-1].minor.yy0;
	yygotominor.yy29 = new NumberRangeEval(yymsp[-2].minor.yy396->val(), yymsp[0].minor.yy396->val(), yymsp[-2].minor.yy396->beginLoc(), yymsp[0].minor.yy396->endLoc());
}
#line 4165 "bcplus/parser/detail/lemon_parser.c"
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
#line 4180 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* term_int_eval ::= PAREN_L term_int_eval PAREN_R */
#line 780 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy396 = yymsp[-1].minor.yy396;
	yygotominor.yy396->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy396->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 4190 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* term_int_eval ::= DASH term_int_eval */
#line 800 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy396, yymsp[0].minor.yy396, -1 * yymsp[0].minor.yy396->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4196 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* term_int_eval ::= ABS term_int_eval */
#line 801 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy396, yymsp[0].minor.yy396, yymsp[0].minor.yy396->val() < 0 ? - yymsp[0].minor.yy396->val() : yymsp[0].minor.yy396->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 4202 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* term_int_eval ::= term_int_eval DASH term_int_eval */
#line 803 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() - yymsp[0].minor.yy396->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4208 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* term_int_eval ::= term_int_eval PLUS term_int_eval */
#line 804 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() + yymsp[0].minor.yy396->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 4214 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* term_int_eval ::= term_int_eval STAR term_int_eval */
#line 805 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() * yymsp[0].minor.yy396->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 4220 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* term_int_eval ::= term_int_eval INT_DIV term_int_eval */
#line 806 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() / yymsp[0].minor.yy396->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 4226 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* term_int_eval ::= term_int_eval MOD term_int_eval */
#line 807 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() % yymsp[0].minor.yy396->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 4232 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* formula ::= formula_base */
      case 183: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==183);
      case 255: /* formula_temporal ::= formula_temporal_base */ yytestcase(yyruleno==255);
#line 867 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy17;				}
#line 4239 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* formula ::= PAREN_L formula PAREN_R */
      case 184: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==184);
      case 256: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==256);
#line 868 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[-1].minor.yy17; yygotominor.yy17->parens(true); 	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4248 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* formula ::= NOT formula */
      case 185: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==185);
      case 257: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==257);
#line 869 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 4255 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* formula ::= DASH formula */
      case 186: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==186);
      case 258: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==258);
#line 870 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 4262 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 144: /* formula ::= formula AMP formula */
      case 187: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==187);
      case 259: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==259);
#line 871 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy17, yymsp[0].minor.yy17, yymsp[-2].minor.yy17->beginLoc(), yymsp[0].minor.yy17->endLoc());   yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 4270 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 145: /* formula ::= formula DBL_PLUS formula */
      case 146: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==146);
      case 188: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==188);
      case 189: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==189);
      case 260: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==260);
      case 261: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==261);
#line 872 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::OR); }
#line 4280 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 147: /* formula ::= formula EQUIV formula */
      case 190: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==190);
      case 262: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==262);
#line 874 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::EQUIV); }
#line 4287 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 148: /* formula ::= formula IMPL formula */
      case 149: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==149);
      case 191: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==191);
      case 192: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==192);
      case 263: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==263);
      case 264: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==264);
#line 875 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::IMPL); }
#line 4297 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 150: /* formula_base ::= comparison */
      case 193: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==193);
      case 266: /* formula_temporal_base ::= comparison_temporal */ yytestcase(yyruleno==266);
      case 290: /* head_formula ::= comparison */ yytestcase(yyruleno==290);
#line 878 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy17; }
#line 4305 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 151: /* formula_base ::= atomic_formula */
      case 291: /* head_formula ::= atomic_head_formula */ yytestcase(yyruleno==291);
#line 879 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy423; }
#line 4311 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* formula_base ::= formula_quant */
      case 268: /* formula_temporal_base ::= formula_temporal_quant */ yytestcase(yyruleno==268);
#line 880 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy177; }
#line 4317 "bcplus/parser/detail/lemon_parser.c"
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
#line 4329 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* formula_base ::= TRUE */
      case 194: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==194);
      case 270: /* formula_temporal_base ::= TRUE */ yytestcase(yyruleno==270);
      case 293: /* head_formula ::= TRUE */ yytestcase(yyruleno==293);
#line 889 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4337 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* formula_base ::= FALSE */
      case 195: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==195);
      case 271: /* formula_temporal_base ::= FALSE */ yytestcase(yyruleno==271);
      case 294: /* head_formula ::= FALSE */ yytestcase(yyruleno==294);
#line 890 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4345 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* comparison ::= term_strong EQ term */
      case 163: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==163);
      case 196: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==196);
      case 272: /* comparison_temporal ::= term_temporal_strong EQ term_temporal */ yytestcase(yyruleno==272);
#line 892 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4354 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= term_strong DBL_EQ term */
      case 164: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==164);
      case 197: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==197);
      case 273: /* comparison_temporal ::= term_temporal_strong DBL_EQ term_temporal */ yytestcase(yyruleno==273);
#line 893 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4363 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* comparison ::= term_strong NEQ term */
      case 165: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==165);
      case 198: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==198);
      case 274: /* comparison_temporal ::= term_temporal_strong NEQ term_temporal */ yytestcase(yyruleno==274);
#line 894 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4372 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* comparison ::= term_strong LTHAN term */
      case 166: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==166);
      case 199: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==199);
      case 275: /* comparison_temporal ::= term_temporal_strong LTHAN term_temporal */ yytestcase(yyruleno==275);
#line 895 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4381 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* comparison ::= term_strong GTHAN term */
      case 167: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==167);
      case 200: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==200);
      case 276: /* comparison_temporal ::= term_temporal_strong GTHAN term_temporal */ yytestcase(yyruleno==276);
#line 896 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4390 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 161: /* comparison ::= term_strong LTHAN_EQ term */
      case 168: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==168);
      case 201: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==201);
      case 277: /* comparison_temporal ::= term_temporal_strong LTHAN_EQ term_temporal */ yytestcase(yyruleno==277);
#line 897 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4399 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 162: /* comparison ::= term_strong GTHAN_EQ term */
      case 169: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==169);
      case 202: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==202);
      case 278: /* comparison_temporal ::= term_temporal_strong GTHAN_EQ term_temporal */ yytestcase(yyruleno==278);
#line 898 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4408 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 170: /* comparison ::= constant DBL_EQ term */
#line 906 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4414 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 171: /* comparison ::= constant NEQ term */
#line 907 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4420 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 172: /* comparison ::= constant LTHAN term */
#line 908 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4426 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 173: /* comparison ::= constant GTHAN term */
#line 909 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4432 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 174: /* comparison ::= constant LTHAN_EQ term */
#line 910 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4438 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 175: /* comparison ::= constant GTHAN_EQ term */
#line 911 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4444 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 176: /* atomic_formula ::= constant */
      case 180: /* atomic_formula_anon ::= const_anon */ yytestcase(yyruleno==180);
      case 203: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==203);
#line 938 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy423, yymsp[0].minor.yy225, true); }
#line 4451 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 177: /* atomic_formula ::= TILDE constant */
      case 181: /* atomic_formula_anon ::= TILDE const_anon */ yytestcase(yyruleno==181);
      case 204: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==204);
#line 939 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy423, yymsp[0].minor.yy225, false);   yy_destructor(yypParser,75,&yymsp[-1].minor);
}
#line 4459 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 178: /* atomic_formula ::= constant EQ term */
      case 182: /* atomic_formula_anon ::= const_anon EQ term */ yytestcase(yyruleno==182);
      case 205: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==205);
#line 940 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = new AtomicFormula(yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());	  yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4467 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 179: /* atomic_formula_anon ::= atomic_formula */
      case 295: /* atomic_head_formula ::= atomic_formula */ yytestcase(yyruleno==295);
      case 383: /* show_elem ::= atomic_formula_one_const */ yytestcase(yyruleno==383);
#line 942 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = yymsp[0].minor.yy423; }
#line 4474 "bcplus/parser/detail/lemon_parser.c"
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
#line 4492 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 207: /* quant_lst ::= quant_op variable */
#line 1020 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy221 = new QuantifierFormula::QuantifierList();
		yygotominor.yy221->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy497, yymsp[0].minor.yy189));
	}
#line 4500 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* quant_lst ::= quant_lst quant_op variable */
#line 1026 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy221 = yymsp[-2].minor.yy221;
		yygotominor.yy221->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy497, yymsp[0].minor.yy189));
	}
#line 4508 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 209: /* quant_op ::= BIG_CONJ */
#line 1031 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy497 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,90,&yymsp[0].minor);
}
#line 4514 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* quant_op ::= BIG_DISJ */
#line 1032 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy497 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4520 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
      case 280: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==280);
#line 1078 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, NULL);  }
#line 4526 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
      case 281: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==281);
#line 1079 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy17,  yymsp[0].minor.yy0, NULL);  }
#line 4532 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 282: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==282);
#line 1080 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4538 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 283: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==283);
#line 1081 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-5].minor.yy299, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy17,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4544 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 215: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
      case 284: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==284);
#line 1082 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, NULL);  }
#line 4550 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
      case 285: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==285);
#line 1083 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-3].minor.yy299, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy17,  yymsp[0].minor.yy0, NULL);  }
#line 4556 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
      case 286: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==286);
#line 1084 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4562 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 218: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
      case 287: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==287);
#line 1085 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy17,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4568 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 219: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1089 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy375 = yymsp[-1].minor.yy375;
	  yy_destructor(yypParser,98,&yymsp[0].minor);
}
#line 4576 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 220: /* card_var_lst_inner ::= variable */
#line 1094 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy189;
		yygotominor.yy375 = new CardinalityFormula::VariableList();
		yygotominor.yy375->push_back(yymsp[0].minor.yy189->symbol());
	}
#line 4585 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 221: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1101 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy189;
		yygotominor.yy375 = yymsp[-2].minor.yy375;
		yygotominor.yy375->push_back(yymsp[0].minor.yy189->symbol());
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4595 "bcplus/parser/detail/lemon_parser.c"
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
#line 4606 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 234: /* term_temporal ::= term_temporal COLON term */
      case 247: /* term_temporal_strong ::= term_temporal_strong COLON term_strong */ yytestcase(yyruleno==247);
#line 1167 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BindingTerm); }
#line 4612 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* formula_temporal ::= term_temporal_strong COLON formula */
#line 1248 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy17, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BindingFormula); }
#line 4617 "bcplus/parser/detail/lemon_parser.c"
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
#line 4628 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* head_formula ::= head_formula AMP head_formula */
#line 1349 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy17 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy17, yymsp[0].minor.yy17, yymsp[-2].minor.yy17->beginLoc(), yymsp[0].minor.yy17->endLoc());
	  yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 4636 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* head_formula ::= PAREN_L head_formula PAREN_R */
#line 1353 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> lp_ptr = yymsp[-2].minor.yy0, rp_ptr = yymsp[0].minor.yy0;
		yygotominor.yy17 = yymsp[-1].minor.yy17;
		yygotominor.yy17->parens(true);																									\
		yygotominor.yy17->beginLoc(yymsp[-2].minor.yy0->beginLoc());																					\
		yygotominor.yy17->endLoc(yymsp[0].minor.yy0->endLoc());
		
	}
#line 4648 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* head_formula ::= formula_smpl_card */
#line 1364 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy17 = yymsp[0].minor.yy133;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy133->beginLoc());
			YYERROR;
		}
	}
#line 4659 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* atomic_head_formula ::= DASH constant */
#line 1377 "bcplus/parser/detail/lemon_parser.y"
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
#line 4675 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1390 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy423, yymsp[0].minor.yy0, NULL);  }
#line 4680 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1391 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy423,  yymsp[0].minor.yy0, NULL);  }
#line 4685 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1392 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4690 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1393 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-5].minor.yy299, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy423,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4695 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1394 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy423, yymsp[0].minor.yy0, NULL);  }
#line 4700 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1395 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-3].minor.yy299, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy423,  yymsp[0].minor.yy0, NULL);  }
#line 4705 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1396 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4710 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1397 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy423,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4715 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1416 "bcplus/parser/detail/lemon_parser.y"
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
#line 4745 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* macro_def_lst ::= macro_bnd */
#line 1444 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy269 = new MacroDeclaration::ElementList();
        yygotominor.yy269->push_back(yymsp[0].minor.yy359);
    }
#line 4753 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1450 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy269 = yymsp[-2].minor.yy269;
        yygotominor.yy269->push_back(yymsp[0].minor.yy359);
      yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4762 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1456 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy154;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy359 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy154);
      yy_destructor(yypParser,71,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4776 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1465 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy359 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4787 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* macro_args ::= macro_arg */
#line 1473 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy154 = new MacroSymbol::ArgumentList();
        yygotominor.yy154->push_back(yymsp[0].minor.yy79->str());
        delete yymsp[0].minor.yy79;
    }
#line 4796 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* macro_args ::= macro_args COMMA macro_arg */
#line 1479 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy154 = yymsp[-2].minor.yy154;
        yygotominor.yy154->push_back(yymsp[0].minor.yy79->str());
        delete yymsp[0].minor.yy79;
      yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4806 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* macro_arg ::= POUND_INTEGER */
      case 313: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==313);
#line 1486 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy79 = yymsp[0].minor.yy0;
    }
#line 4814 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* sort_lst ::= sort */
#line 1513 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy195 = new ConstantSymbol::SortList();
		yygotominor.yy195->push_back(yymsp[0].minor.yy93);
	}
#line 4822 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* sort_lst ::= sort_lst COMMA sort */
#line 1518 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy195 = yymsp[-2].minor.yy195;
		yygotominor.yy195->push_back(yymsp[0].minor.yy93);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4831 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* sort ::= sort_id_nr */
      case 322: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==322);
      case 323: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==323);
#line 1543 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = yymsp[0].minor.yy93; }
#line 4838 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* sort ::= sort_id_nr STAR */
#line 1544 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::NONE)); }
#line 4843 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* sort ::= sort_id_nr CARROT */
#line 1545 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::UNKNOWN)); }
#line 4848 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* sort ::= sort PLUS object_nullary */
#line 1547 "bcplus/parser/detail/lemon_parser.y"
{ u::ref_ptr<const Object> o_ptr = yymsp[0].minor.yy422; DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, yymsp[0].minor.yy422->symbol()); }
#line 4853 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 320: /* sort ::= sort PLUS IDENTIFIER */
#line 1550 "bcplus/parser/detail/lemon_parser.y"
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
#line 4870 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* sort ::= sort PLUS INTEGER */
#line 1564 "bcplus/parser/detail/lemon_parser.y"
{ 
												  ref_ptr<const Object> t_ptr;
												  BASIC_TERM(t_ptr, yymsp[0].minor.yy0);
												  DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, t_ptr->symbol()); 
												}
#line 4879 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 324: /* sort_nr ::= num_range */
#line 1575 "bcplus/parser/detail/lemon_parser.y"
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
#line 4899 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 325: /* sort_id ::= IDENTIFIER */
#line 1593 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy93 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy93) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4912 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 326: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1624 "bcplus/parser/detail/lemon_parser.y"
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
#line 4931 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 327: /* constant_bnd_lst ::= constant_bnd */
#line 1641 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy409 = yymsp[0].minor.yy409;
	}
#line 4938 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 328: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1646 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy409;
		yygotominor.yy409 = yymsp[-2].minor.yy409;
		yygotominor.yy409->splice(yygotominor.yy409->end(), *yymsp[0].minor.yy409);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4948 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1666 "bcplus/parser/detail/lemon_parser.y"
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
#line 4976 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 330: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1688 "bcplus/parser/detail/lemon_parser.y"
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
#line 4991 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 331: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1699 "bcplus/parser/detail/lemon_parser.y"
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
#line 5021 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 332: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1725 "bcplus/parser/detail/lemon_parser.y"
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
#line 5050 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 333: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1749 "bcplus/parser/detail/lemon_parser.y"
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
#line 5131 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 334: /* constant_dcl_lst ::= IDENTIFIER */
#line 1825 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = new IdentifierDeclList();
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 5139 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 335: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1830 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = new IdentifierDeclList();
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy195));
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5149 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 336: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1835 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = yymsp[-2].minor.yy226;
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5158 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 337: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1840 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = yymsp[-5].minor.yy226;
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy195));
	  yy_destructor(yypParser,102,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5169 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 338: /* constant_dcl_type ::= ABACTION */
#line 1847 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5181 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 339: /* constant_dcl_type ::= ACTION */
#line 1856 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5193 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 340: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1865 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5205 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 341: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1874 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5217 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 342: /* constant_dcl_type ::= EXTERNALACTION */
#line 1883 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5229 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 343: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1892 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5241 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 344: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1901 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5253 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 345: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1910 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5265 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* constant_dcl_type ::= RIGID */
#line 1919 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5277 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1928 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5289 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 348: /* constant_dcl_type ::= SDFLUENT */
#line 1938 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5301 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* attrib_spec ::= ATTRIBUTE */
#line 1948 "bcplus/parser/detail/lemon_parser.y"
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
#line 5316 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1961 "bcplus/parser/detail/lemon_parser.y"
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
#line 5332 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1989 "bcplus/parser/detail/lemon_parser.y"
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
#line 5367 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* object_bnd_lst ::= object_bnd */
#line 2022 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy102 = new ObjectDeclaration::ElementList();
		yygotominor.yy102->push_back(yymsp[0].minor.yy294);
	}
#line 5375 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 2028 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy102 = yymsp[-2].minor.yy102;
		yygotominor.yy102->push_back(yymsp[0].minor.yy294);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5384 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 2034 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy294 = new ObjectDeclaration::Element(yymsp[0].minor.yy93, yymsp[-2].minor.yy425);
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5392 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* object_lst ::= object_spec */
#line 2039 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = yymsp[0].minor.yy425;
	}
#line 5399 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* object_lst ::= object_lst COMMA object_spec */
#line 2043 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = yymsp[-2].minor.yy425;
		yygotominor.yy425->splice(yygotominor.yy425->end(), *yymsp[0].minor.yy425);
		delete yymsp[0].minor.yy425;
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5409 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* object_spec ::= IDENTIFIER */
#line 2052 "bcplus/parser/detail/lemon_parser.y"
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
#line 5425 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 2065 "bcplus/parser/detail/lemon_parser.y"
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
#line 5444 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* object_spec ::= INTEGER */
#line 2080 "bcplus/parser/detail/lemon_parser.y"
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
#line 5460 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* object_spec ::= num_range */
#line 2094 "bcplus/parser/detail/lemon_parser.y"
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
#line 5477 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 2123 "bcplus/parser/detail/lemon_parser.y"
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
#line 5515 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* variable_bnd_lst ::= variable_bnd */
#line 2159 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = yymsp[0].minor.yy410;
	}
#line 5522 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 2164 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = yymsp[-2].minor.yy410;
		yygotominor.yy410->splice(yygotominor.yy410->end(), *yymsp[0].minor.yy410);
		delete yymsp[0].minor.yy410;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5532 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* variable_bnd ::= variable_lst DBL_COLON sort */
#line 2171 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy52) {
			yygotominor.yy410->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy93));
		}



		delete yymsp[-2].minor.yy52;
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5548 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 365: /* variable_lst ::= IDENTIFIER */
#line 2184 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy52 = new TokenList();
		yygotominor.yy52->push_back(yymsp[0].minor.yy0);
	}
#line 5556 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 2189 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy52 = yymsp[-2].minor.yy52;
		yygotominor.yy52->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5565 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 2210 "bcplus/parser/detail/lemon_parser.y"
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
#line 5583 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* sort_bnd_lst ::= sort_bnd */
      case 370: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==370);
#line 2226 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[0].minor.yy196;
	}
#line 5591 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 369: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 2231 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[-2].minor.yy196;
		yygotominor.yy196->splice(yygotominor.yy196->end(), *yymsp[0].minor.yy196);
		delete yymsp[0].minor.yy196;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5601 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 371: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 2243 "bcplus/parser/detail/lemon_parser.y"
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
#line 5617 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 372: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 2255 "bcplus/parser/detail/lemon_parser.y"
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
#line 5632 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 373: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 2266 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[-1].minor.yy196;
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5641 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 374: /* sort_dcl_lst ::= IDENTIFIER */
#line 2271 "bcplus/parser/detail/lemon_parser.y"
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
#line 5658 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 375: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 2285 "bcplus/parser/detail/lemon_parser.y"
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
#line 5677 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 376: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 2312 "bcplus/parser/detail/lemon_parser.y"
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
#line 5693 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 377: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 2326 "bcplus/parser/detail/lemon_parser.y"
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
#line 5711 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 378: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 2343 "bcplus/parser/detail/lemon_parser.y"
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
#line 5727 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 379: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2357 "bcplus/parser/detail/lemon_parser.y"
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
#line 5745 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 380: /* show_lst ::= show_elem */
#line 2375 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = new ShowStatement::ElementList();
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	}
#line 5753 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 381: /* show_lst ::= show_lst COMMA show_elem */
#line 2380 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = yymsp[-2].minor.yy123;
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5762 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 382: /* show_lst ::= show_lst SEMICOLON show_elem */
#line 2385 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = yymsp[-2].minor.yy123;
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5771 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 384: /* stmt_noconcurrency ::= NOCONCURRENCY PERIOD */
#line 2413 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy485, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5776 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 385: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD */
#line 2414 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy446, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5781 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 386: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_int_eval PERIOD */
#line 2440 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5787 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 387: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE DBL_COLON term_int_eval PERIOD */
#line 2441 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,76,&yymsp[-2].minor);
}
#line 5793 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 388: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_int_eval PERIOD */
#line 2442 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5799 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 389: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE DBL_COLON term_int_eval PERIOD */
#line 2443 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,76,&yymsp[-2].minor);
}
#line 5805 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 390: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2468 "bcplus/parser/detail/lemon_parser.y"
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
#line 5842 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 391: /* query_lst ::= formula_temporal */
#line 2504 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = NULL;
		yygotominor.yy489.label = NULL;

		yygotominor.yy489.l->push_back(yymsp[0].minor.yy17);
	}
#line 5853 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 392: /* query_lst ::= query_maxstep_decl */
#line 2513 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = yymsp[0].minor.yy29;
		yygotominor.yy489.label = NULL;
	}
#line 5862 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 393: /* query_lst ::= query_label_decl */
#line 2520 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = NULL;
		yygotominor.yy489.label = yymsp[0].minor.yy79;
	}
#line 5871 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 394: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2527 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy489 = yymsp[-2].minor.yy489;
		yymsp[-2].minor.yy489.l->push_back(yymsp[0].minor.yy17);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5880 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 395: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2533 "bcplus/parser/detail/lemon_parser.y"
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
#line 5896 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 396: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2546 "bcplus/parser/detail/lemon_parser.y"
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
#line 5912 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 397: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2572 "bcplus/parser/detail/lemon_parser.y"
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
#line 5937 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 398: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range_eval */
#line 2593 "bcplus/parser/detail/lemon_parser.y"
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
#line 5954 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 399: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 400: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==400);
#line 2607 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy79, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5961 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 401: /* clause_if ::= IF formula */
#line 2642 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_IF); 		}
#line 5966 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 402: /* clause_if ::= */
      case 404: /* clause_after ::= */ yytestcase(yyruleno==404);
      case 406: /* clause_ifcons ::= */ yytestcase(yyruleno==406);
      case 410: /* clause_where ::= */ yytestcase(yyruleno==410);
#line 2643 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = NULL; }
#line 5974 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 403: /* clause_after ::= AFTER formula */
#line 2644 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_AFTER);	}
#line 5979 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 405: /* clause_ifcons ::= IFCONS formula */
#line 2646 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_IFCONS); 	}
#line 5984 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 407: /* clause_unless ::= UNLESS atomic_formula_anon */
#line 2648 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy423, Language::Feature::CLAUSE_UNLESS); 	}
#line 5989 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 408: /* clause_unless ::= */
#line 2649 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = NULL; }
#line 5994 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 409: /* clause_where ::= WHERE formula_no_const */
#line 2650 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_WHERE); 	}
#line 5999 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 411: /* stmt_law ::= law_basic */
      case 412: /* stmt_law ::= law_caused */ yytestcase(yyruleno==412);
      case 413: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==413);
      case 414: /* stmt_law ::= law_impl */ yytestcase(yyruleno==414);
      case 415: /* stmt_law ::= law_causes */ yytestcase(yyruleno==415);
      case 416: /* stmt_law ::= law_increments */ yytestcase(yyruleno==416);
      case 417: /* stmt_law ::= law_decrements */ yytestcase(yyruleno==417);
      case 418: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==418);
      case 419: /* stmt_law ::= law_always */ yytestcase(yyruleno==419);
      case 420: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==420);
      case 421: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==421);
      case 422: /* stmt_law ::= law_never */ yytestcase(yyruleno==422);
      case 423: /* stmt_law ::= law_default */ yytestcase(yyruleno==423);
      case 424: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==424);
      case 425: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==425);
      case 426: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==426);
      case 427: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==427);
      case 428: /* stmt_law ::= law_observed */ yytestcase(yyruleno==428);
#line 2696 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy484 = yymsp[0].minor.yy484;}
#line 6021 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 429: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2815 "bcplus/parser/detail/lemon_parser.y"
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
#line 6036 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 430: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2827 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy17, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 6043 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 431: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2831 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy17, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 6050 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 432: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2835 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy484, yymsp[-4].minor.yy17, yymsp[-3].minor.yy0, yymsp[-2].minor.yy17, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 6056 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 433: /* law_impl ::= ARROW_LDASH formula clause_where PERIOD */
#line 2838 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy484, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy17, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 6062 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 434: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2841 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy484, yymsp[-6].minor.yy423, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 6068 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 435: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2845 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy484, yymsp[-8].minor.yy423, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-4].minor.yy299, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 6075 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 436: /* law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2848 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy484, yymsp[-8].minor.yy423, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-4].minor.yy299, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_DECREMENTS, DecrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 6082 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 437: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2852 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy484, yymsp[-6].minor.yy423, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 6088 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 438: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2856 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 6095 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 439: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2860 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 6102 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 440: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2864 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 6109 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 441: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2868 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 6116 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 442: /* law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2872 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy423, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 6123 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 443: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2876 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 6130 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 444: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2880 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 6137 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 445: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2884 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 6143 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 446: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2888 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy484, yymsp[-3].minor.yy0, yymsp[-2].minor.yy225, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 6149 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 447: /* law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD */
#line 2893 "bcplus/parser/detail/lemon_parser.y"
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
#line 6168 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 448: /* stmt_code_blk ::= ASP_GR */
#line 2927 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 6173 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 449: /* stmt_code_blk ::= ASP_CP */
#line 2928 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 6178 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 450: /* stmt_code_blk ::= F2LP_GR */
#line 2929 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 6183 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 451: /* stmt_code_blk ::= F2LP_CP */
#line 2930 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 6188 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 452: /* stmt_code_blk ::= LUA_GR */
#line 2931 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 6193 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 453: /* stmt_code_blk ::= LUA_CP */
#line 2932 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 6198 "bcplus/parser/detail/lemon_parser.c"
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
#line 6264 "bcplus/parser/detail/lemon_parser.c"
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
