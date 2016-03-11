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
			#include "bcplus/parser/Number.h"
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

#line 519 "bcplus/parser/detail/lemon_parser.y"

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

#line 788 "bcplus/parser/detail/lemon_parser.y"

	#define NUM_UOP(t_new, t, val) \
		ref_ptr<const Referenced> t_ptr = t; \
		t_new = new Number(val, t->beginLoc(), t->endLoc());


	#define NUM_BOP(t_new, l, r, val) \
		ref_ptr<const Referenced> l_ptr = l, r_ptr = r; \
		t_new = new Number(val, l->beginLoc(), r->endLoc());

#line 834 "bcplus/parser/detail/lemon_parser.y"

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

#line 915 "bcplus/parser/detail/lemon_parser.y"


	#define ATOMIC_FORMULA(af, constant, value) 														\
		af = NULL;																						\
		ref_ptr<Constant> c_ptr = constant;																\
																										\
																										\
		ref_ptr<const ObjectSymbol> t =																	\
			(value ? parser->symtab()->bobj(SymbolTable::BuiltinObject::TRUE) 							\
				: parser->symtab()->bobj(SymbolTable::BuiltinObject::FALSE));							\
																										\
																										\
		/* If this is a boolean constant we can interpret it as a shortcut for c=true */				\
		if (!constant->symbol()->sort()->contains(t)) {													\
			parser->_parse_error("\"" + *constant->symbol()->name() 									\
				+ "\" is not a boolean valued constant and therefore "									\
				"cannot be used in a bare atomic formula.", &constant->beginLoc());						\
			YYERROR;																					\
		} else {																						\
			af = new AtomicFormula(constant,															\
					new Object(t, NULL, constant->beginLoc(), constant->endLoc()), 						\
					constant->beginLoc(), constant->endLoc());											\
		}
	


#line 1051 "bcplus/parser/detail/lemon_parser.y"

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



#line 1120 "bcplus/parser/detail/lemon_parser.y"

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

#line 1535 "bcplus/parser/detail/lemon_parser.y"

		
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
#line 1663 "bcplus/parser/detail/lemon_parser.y"

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
#line 2410 "bcplus/parser/detail/lemon_parser.y"

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

#line 2433 "bcplus/parser/detail/lemon_parser.y"

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
#line 2462 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRangeEval* maxstep;
		Token const* label;
	};

#line 2569 "bcplus/parser/detail/lemon_parser.y"

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

#line 2641 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2730 "bcplus/parser/detail/lemon_parser.y"

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
		ref_ptr<Element> head_ptr = head;																		\
		ref_ptr<const Referenced> where_ptr = where;															\
		ref_ptr<const Token> kw_ptr = kw, p_ptr = p;															\
																												\
		if (!parser->lang()->support(feature)) {																\
			parser->_feature_error(feature, (kw ? &kw_ptr->beginLoc() : &head_ptr->beginLoc()));				\
			YYERROR;																							\
		} else {																								\
			law = new class(head, where, (kw ? kw_ptr->beginLoc() : head_ptr->beginLoc()), p_ptr->endLoc());	\
		}
		

#line 2923 "bcplus/parser/detail/lemon_parser.y"

	#define CODE_BLK(stmt, code, feature, type) 												\
		ref_ptr<const Token> code_ptr = code;													\
		if (!parser->lang()->support(feature)) {												\
			stmt = NULL;																		\
			parser->_feature_error(feature, &code->beginLoc());									\
			YYERROR;																			\
		} else {																				\
			stmt = new type(code, code->beginLoc(), code->endLoc());							\
		}
#line 548 "bcplus/parser/detail/lemon_parser.c"
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
#define YYCODETYPE unsigned short int
#define YYNOCODE 256
#define YYACTIONTYPE unsigned short int
#define lemon_parserTOKENTYPE  Token const* 								
typedef union {
  int yyinit;
  lemon_parserTOKENTYPE yy0;
  Statement* yy10;
  MacroDeclaration::ElementList* yy19;
  AtomicFormula* yy52;
  ShowStatement::ElementList* yy65;
  Formula* yy93;
  QuantifierFormula* yy101;
  ConstantSymbol::Type::type yy109;
  Object* yy138;
  Token const* yy143;
  CardinalityFormula::VariableList* yy145;
  NumberRange* yy171;
  ObjectDeclaration::ElementList* yy214;
  Term* yy217;
  SortDeclaration* yy225;
  NumberRangeEval* yy259;
  VariableDeclaration* yy267;
  NCStatement* yy268;
  QueryData yy271;
  UNUSED yy283;
  Variable* yy284;
  ObjectDeclaration* yy288;
  MacroDeclaration* yy291;
  ConstantDeclaration::ElementList* yy295;
  TokenList* yy298;
  MacroSymbol* yy313;
  TermList* yy317;
  QueryStatement* yy320;
  QuantifierFormula::Operator::type yy321;
  SortSymbol const* yy322;
  QuantifierFormula::QuantifierList* yy347;
  ConstantDeclaration* yy355;
  VariableDeclaration::ElementList* yy377;
  ObjectDeclaration::Element* yy380;
  SortSymbol* yy385;
  ObjectDeclaration::Element::ObjectList* yy387;
  ConstantSymbol::SortList* yy405;
  StrongNCStatement* yy418;
  Constant* yy427;
  MacroSymbol::ArgumentList* yy440;
  IdentifierDeclList* yy444;
  SortDeclaration::ElementList* yy445;
  Number* yy458;
  LuaTerm* yy463;
  CardinalityFormula* yy471;
  int yy511;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define lemon_parserARG_SDECL  BCParser* parser						;
#define lemon_parserARG_PDECL , BCParser* parser						
#define lemon_parserARG_FETCH  BCParser* parser						 = yypParser->parser						
#define lemon_parserARG_STORE yypParser->parser						 = parser						
#define YYNSTATE 868
#define YYNRULE 457
#define YYERRORSYMBOL 135
#define YYERRSYMDT yy511
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
#define YY_ACTTAB_COUNT (3464)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   867,   64,  868,  866,  865,  864,  863,  862,  861,  860,
 /*    10 */   859,  858,  857,  856,  855,  854,  853,  852,  540,  824,
 /*    20 */   323,  851,  844,  850,  849,  843,  149,  147,  145,  144,
 /*    30 */   143,  846,  319,  324,  824,  323,  851,  844,  480,  849,
 /*    40 */   843,  783,  422,   22,   21,   25,   24,  318,  324,  766,
 /*    50 */    26,  117,  115,  114,  337,  775,  772,  771,  770,  769,
 /*    60 */   697,  122,  363,  248,  782,  781,   63,   18,  482,  824,
 /*    70 */   323,  851,  844,  850,  849,  843,   23,   22,   21,   25,
 /*    80 */    24,   62,  318,  324,   26,  169,  167,  166,   59,  368,
 /*    90 */   775,  772,  771,  770,  769,  471,  519,  473,  698,  699,
 /*   100 */   569,  593,  592,  591,  590,  589,  588,  587,  586,  585,
 /*   110 */   584,  583,  582,  581,  580,  579,  578,  577,  576,  575,
 /*   120 */   823,  549,  663,   61,  550,  822,  559,  558,  555,  554,
 /*   130 */   557,  556,  242,  240,  238,  237,  236,  728,  642,  539,
 /*   140 */   851,  844,  850,  849,  843,   46,  552,   12,  298,  201,
 /*   150 */   202,   44,   30,   10,   11,  301,  192,  293,  269,   45,
 /*   160 */   126,  611,  165,  263,  697,   26,  363,  796,  795,  797,
 /*   170 */     9,  502,  501,    8,  313,   43,  255,  327,  615,  612,
 /*   180 */   610,  609,  779,  780,  717,  696,  516,  325,  185,  800,
 /*   190 */    48,   25,   24,  311,  310,  101,   26,  845,   13,  469,
 /*   200 */   519,  473,  698,  699,  100,  693,  692,  691,  690,   99,
 /*   210 */   641,  549,   73,  517,  550,  640,  121,  119,  117,  115,
 /*   220 */   114,  689,  695,  687,  125,   53,   52,  122,  686,   98,
 /*   230 */    54,  712,  711,  713,  641,  549,   60,  553,  550,  640,
 /*   240 */   685,  683,  684,  688,  764,  763,  714,  715,  539,  552,
 /*   250 */   842,  551,   20,  191,  220,  503,  246,  619,  618,  504,
 /*   260 */   838,  847,  848,  851,  844,  850,  849,  843,  487,  421,
 /*   270 */   778,   74,  607,  608,  661,  199,  539,   72,    6,  547,
 /*   280 */    42,  619,  618,  620,  310,  823,  549,  538,  218,  550,
 /*   290 */   822,  310,  274,  216,  193,  719,  607,  608,  757,  199,
 /*   300 */   659,  756,    6,  194,   42,  475,  679,  474,  310,   50,
 /*   310 */    49,   53,   52,   40,   41,  210,   54,  201,  202,  131,
 /*   320 */    67,  275,  824,  323,  851,  844,  850,  849,  843,  673,
 /*   330 */   547,  363,  796,  795,  797,  316,  324,   40,   41,  552,
 /*   340 */   842,  551,  660,  131,  783,  778,  552,  767,  768,  678,
 /*   350 */   206,  659,   30,    3,  212,   29,  653,  823,  549,  310,
 /*   360 */    54,  550,  822,  552,  842,  551,  481,  782,  781,  184,
 /*   370 */   219,  217,  215,  214,  213,  677,  465,  672,  824,  323,
 /*   380 */   851,  844,  480,  849,  843,  697,  547,  363,   27,   28,
 /*   390 */    15,  318,  324,  658,   98,  215,  214,  213,  339,  775,
 /*   400 */   772,  771,  770,  769,  796,  795,  797,  546,  835,   66,
 /*   410 */   670,   19,  482,   47,  552,  842,  551,  841,  539,  779,
 /*   420 */   780,  524,  473,  698,  699,  185,  243,   48,  523,  522,
 /*   430 */   209,  310,  734,  642,  539,  851,  844,  850,  849,  843,
 /*   440 */   778,  121,  119,  117,  115,  114,  197,  122,  211,  180,
 /*   450 */     2,  188,  195,  196,   38,   37,  611,  665,  733,   39,
 /*   460 */   241,  125,  488,  837,  549,  239,   98,  550,  836,  314,
 /*   470 */   764,  763,  344,  615,  612,  610,  609,  203,   33,   30,
 /*   480 */   455,  456,  238,  237,  236,  732,  552,  842,  551,  638,
 /*   490 */   629,  851,  844,  850,  849,  843,  642,  539,  851,  844,
 /*   500 */   480,  849,  843,  145,  144,  143,  453,  597,  596,  646,
 /*   510 */   831,  830,  832,  754,  549,  312,  731,  550,  753,  611,
 /*   520 */    35,   34,   38,   37,  426,  833,  834,   39,   65,   31,
 /*   530 */   482,  730,  314,  120,  840,  329,  615,  612,  610,  609,
 /*   540 */    47,  242,  240,  238,  237,  236,  642,  539,  851,  844,
 /*   550 */   480,  849,  843,  816,  807,  851,  844,  850,  849,  843,
 /*   560 */   747,  746,  748,  483,  411,  187,  635,  118,  729,  611,
 /*   570 */   548,  842,  116, 1089,   64,  737,  738, 1089, 1228,   32,
 /*   580 */   482,  312,  314,   57,  526,  331,  615,  612,  610,  609,
 /*   590 */   467,  513,  552,  842,  551,    7,  599,  598,  639,  149,
 /*   600 */   147,  145,  144,  143, 1228,  204,   14,  138,  137,  136,
 /*   610 */   508,  135,  134,  133,  132,  645,   55,   56,  726,  463,
 /*   620 */   666,  462,  152,  642,  539,  851,  844,  850,  849,  843,
 /*   630 */   525,  151,  142,  141,  140,  139,  548,  712,  711,  713,
 /*   640 */   235,  655,  552,  842,  551,  244,  611,  242,  240,  238,
 /*   650 */   237,  236,  714,  715,  454,  717,  696,  246,  312,  314,
 /*   660 */   220,  193,  343,  615,  612,  610,  609,  815,  549,  292,
 /*   670 */   194,  550,  814,  637,  549,  700,  701,  550,  636,  837,
 /*   680 */   549,  705,  721,  550,  836,  704,  521,  824,  323,  851,
 /*   690 */   844,  850,  849,  843,  218,  190,  183,  601,  600,  216,
 /*   700 */   315,  324,  712,  711,  713,  540,  682,  335,  775,  772,
 /*   710 */   771,  770,  769,  548,  809,  808,  810,  714,  715, 1266,
 /*   720 */   631,  630,  632,  520,  243,  220,  831,  830,  832,  811,
 /*   730 */   812,  697,  673,  363,  363,  633,  634,  172,  662, 1266,
 /*   740 */   246,  833,  834,  150,  121,  119,  117,  115,  114,  120,
 /*   750 */   642,  539,  851,  844,  850,  849,  843,  507,  241,  218,
 /*   760 */   461,  457,  510,  239,  216,  839,  468,  519,  473,  698,
 /*   770 */   699,  170,  178,  611,  233,  181,  168,  148,  205,  234,
 /*   780 */   675,  817,  146,  118,  177,  697,  617,  363,  116,  605,
 /*   790 */   615,  612,  610,  609,  176,  122,  552,  842,  551,  777,
 /*   800 */   759,  527,  552,  842,  551,  312,  703,  200,  552,  842,
 /*   810 */   551,  174,  478,  720,  824,  322,  851,  844,  850,  849,
 /*   820 */   843,  472,  473,  698,  699,   71,  515,  776,  324,   47,
 /*   830 */   122,  251,  179,  552,  761,  775,  772,  771,  770,  769,
 /*   840 */   175,  602,  824,  323,  851,  844,  850,  849,  843,   23,
 /*   850 */    22,   21,   25,   24,  694,  318,  324,   26,  511,  668,
 /*   860 */   548,    5,  774,  775,  772,  771,  770,  769,  509,  824,
 /*   870 */   323,  851,  844,  850,  849,  843,  723,  724,  681,   70,
 /*   880 */   718,  474,  318,  324,  182,  603,  697,  345,  363,  773,
 /*   890 */   775,  772,  771,  770,  769,  664,  824,  323,  851,  844,
 /*   900 */   850,  849,  843,   23,   22,   21,   25,   24,  182,  318,
 /*   910 */   324,   26,  760,  764,  763,  680,  542,  775,  772,  771,
 /*   920 */   770,  769,  518,  473,  698,  699,  824,  323,  851,  844,
 /*   930 */   850,  849,  843,  674,   23,   22,   21,   25,   24,  318,
 /*   940 */   324,  657,   26,  654, 1165,  182,  541,  775,  772,  771,
 /*   950 */   770,  769,  824,  323,  851,  844,  850,  849,  843, 1165,
 /*   960 */  1165,   39,  697,  182,  363,  318,  324,  669,  462,  247,
 /*   970 */   308,  198,  389,  775,  772,  771,  770,  769,  452, 1165,
 /*   980 */  1165,  595,  824,  323,  851,  844,  850,  849,  843,  594,
 /*   990 */  1165, 1165,  813,  476,  722,  318,  324, 1165,  470,  473,
 /*  1000 */   698,  699,  441,  775,  772,  771,  770,  769,  785,  574,
 /*  1010 */   824,  323,  851,  844,  850,  849,  843,  460,  510,  758,
 /*  1020 */  1165,   69,  573,  318,  324,  173,  171,  169,  167,  166,
 /*  1030 */   440,  775,  772,  771,  770,  769,  572,  824,  323,  851,
 /*  1040 */   844,  850,  849,  843,  606,   23,   22,   21,   25,   24,
 /*  1050 */   318,  324,  571,   26,  459,  510,  570,  340,  775,  772,
 /*  1060 */   771,  770,  769,  568,  824,  323,  851,  844,  850,  849,
 /*  1070 */   843,   36,   35,   34,   38,   37,  821,  318,  324,   39,
 /*  1080 */   547,  458,  510,  567,  338,  775,  772,  771,  770,  769,
 /*  1090 */   565,  673,  564,  363,  824,  323,  851,  844,  850,  849,
 /*  1100 */   843,  544,  563,   68,   30,  433,  510,  318,  324,  113,
 /*  1110 */   112,  111,  110,  109,  336,  775,  772,  771,  770,  769,
 /*  1120 */   824,  323,  851,  844,  850,  849,  843,   23,   22,   21,
 /*  1130 */    25,   24,  562,  318,  324,   26,  466,  671,  465,  672,
 /*  1140 */   367,  775,  772,  771,  770,  769,  245,  308, 1326,    1,
 /*  1150 */   824,  323,  851,  844,  850,  849,  843,   23,   22,   21,
 /*  1160 */    25,   24,  561,  318,  324,   26,  230,  548,  801,  232,
 /*  1170 */   366,  775,  772,  771,  770,  769,  799,  842,  824,  323,
 /*  1180 */   851,  844,  850,  849,  843,  242,  240,  238,  237,  236,
 /*  1190 */   735,  318,  324,  173,  171,  169,  167,  166,  227,  775,
 /*  1200 */   772,  771,  770,  769,  785,  824,  323,  851,  844,  850,
 /*  1210 */   849,  843,   51,   50,   49,   53,   52,  604,  320,  324,
 /*  1220 */    54,  552,  697,  765,  363,  357,  775,  772,  771,  770,
 /*  1230 */   769,  762,  824,  323,  851,  844,  850,  849,  843,   36,
 /*  1240 */    35,   34,   38,   37,  716,  318,  324,   39,  306,   17,
 /*  1250 */    16,  189,  226,  775,  772,  771,  770,  769,  512,  473,
 /*  1260 */   698,  699,  824,  323,  851,  844,  850,  849,  843,  304,
 /*  1270 */   305,  130,   58,  477,  535,  318,  324,  219,  217,  215,
 /*  1280 */   214,  213,  225,  775,  772,  771,  770,  769,  824,  323,
 /*  1290 */   851,  844,  850,  849,  843,   36,   35,   34,   38,   37,
 /*  1300 */   652,  318,  324,   39,  302,  532,  533,  531,  221,  775,
 /*  1310 */   772,  771,  770,  769,  530,  299,  296,  294,  824,  323,
 /*  1320 */   851,  844,  850,  849,  843,  529,  697,  528,  363,  727,
 /*  1330 */   676,  318,  324,  242,  240,  238,  237,  236,  224,  775,
 /*  1340 */   772,  771,  770,  769,  696,  290,  824,  323,  851,  844,
 /*  1350 */   850,  849,  843,  288,  289,  500,  286,  129,  285,  318,
 /*  1360 */   324,  464,  519,  473,  698,  699,  223,  775,  772,  771,
 /*  1370 */   770,  769,  284,  824,  323,  851,  844,  850,  849,  843,
 /*  1380 */   282,   36,   35,   34,   38,   37,  318,  324,  186,   39,
 /*  1390 */   499,  821,  497,  222,  775,  772,  771,  770,  769,  280,
 /*  1400 */   108,  107,  106,  496,  105,  104,  103,  102,  278,  642,
 /*  1410 */   539,  851,  844,  850,  849,  843,  495,    4,  276,  566,
 /*  1420 */   821,  494,  273,   30,  113,  112,  111,  110,  109,  108,
 /*  1430 */   107,  106,  611,  105,  104,  103,  102,  838,  847,  848,
 /*  1440 */   851,  844,  850,  849,  843,  314,  420,    7,  614,  615,
 /*  1450 */   612,  610,  609,  113,  112,  111,  110,  109,   14,  138,
 /*  1460 */   137,  136,  272,  135,  134,  133,  132,  824,  323,  851,
 /*  1470 */   844,  850,  849,  843,  271,  270,  128,  268,  493,  267,
 /*  1480 */   319,  324,  266,  151,  142,  141,  140,  139,  265,  783,
 /*  1490 */   778,  264,  262,  824,  323,  851,  844,  850,  849,  843,
 /*  1500 */    36,   35,   34,   38,   37,  492,  319,  324,   39,  261,
 /*  1510 */   260,  784,  782,  781,  259,  783,  778,  258,  254,  303,
 /*  1520 */   491,  824,  323,  851,  844,  850,  849,  843,  256,  127,
 /*  1530 */   490,  489,  295,  725,  319,  324,  291,  253,  782,  781,
 /*  1540 */   287,  307,  309,  783,  778,  702,  824,  323,  851,  844,
 /*  1550 */   850,  849,  843,   36,   35,   34,   38,   37,  361,  319,
 /*  1560 */   324,   39,  362,  429,  651,  252,  782,  781,  783,  778,
 /*  1570 */   430,  650,  824,  323,  851,  844,  850,  849,  843,   23,
 /*  1580 */    22,   21,   25,   24,  649,  319,  324,   26,  648,  647,
 /*  1590 */   250,  782,  781,  360,  783,  778,  359,  358,  372,  342,
 /*  1600 */   642,  539,  851,  844,  850,  849,  843,  341,  642,  539,
 /*  1610 */   851,  844,  850,  849,  843,  300,  249,  782,  781,  364,
 /*  1620 */   434,  534,  498,  611,  824,  326,  851,  844,  850,  849,
 /*  1630 */   843,  611,  297,  710,  283,  279,  314,  794,  407,  613,
 /*  1640 */   615,  612,  610,  609,  314,  277,  281,  506,  615,  612,
 /*  1650 */   610,  609,  642,  539,  851,  844,  850,  849,  843,  257,
 /*  1660 */   435,  642,  539,  851,  844,  850,  849,  843,  709,  708,
 /*  1670 */   514,  707,  706,  375,  667,  611,  642,  539,  851,  844,
 /*  1680 */   850,  849,  843, 1327,  611, 1327, 1327, 1327,  314, 1327,
 /*  1690 */  1327,  505,  615,  612,  610,  609, 1327,  314, 1327,  611,
 /*  1700 */   373,  615,  612,  610,  609,  642,  539,  851,  844,  850,
 /*  1710 */   849,  843,  314, 1327, 1327,  424,  615,  612,  610,  609,
 /*  1720 */   642,  539,  851,  844,  850,  849,  843, 1327,  611,  642,
 /*  1730 */   539,  851,  844,  850,  849,  843,   92,   91,   90,   89,
 /*  1740 */    88,  314, 1327,  611,  423,  615,  612,  610,  609, 1327,
 /*  1750 */  1327, 1327,  611, 1327, 1327, 1327,  314, 1327, 1327,  332,
 /*  1760 */   615,  612,  610,  609, 1327,  314, 1327, 1327,  330,  615,
 /*  1770 */   612,  610,  609,  642,  539,  851,  844,  850,  849,  843,
 /*  1780 */  1327,  186, 1327,   16,   97,   96,   95,   94,   93,    4,
 /*  1790 */  1327, 1327, 1327,  108,  107,  106,  611,  105,  104,  103,
 /*  1800 */   102,  108,  107,  106,   58,  105,  104,  103,  102,  314,
 /*  1810 */  1327, 1327,  328,  615,  612,  610,  609,  113,  112,  111,
 /*  1820 */   110,  109,  231, 1327, 1327,  113,  112,  111,  110,  109,
 /*  1830 */   823,  549,  752, 1327,  550,  822,   23,   22,   21,   25,
 /*  1840 */    24,  159,  158,  157,   26,  156,  155,  154,  153, 1327,
 /*  1850 */  1327,   23,   22,   21,   25,   24,   87,   86,   85,   26,
 /*  1860 */    84,   83,   82,   81, 1327,  164,  163,  162,  161,  160,
 /*  1870 */   638,  629,  851,  844,  850,  849,  843,  796,  795,  797,
 /*  1880 */    92,   91,   90,   89,   88,   74,   80,   79, 1327,   78,
 /*  1890 */    77,   76,   75, 1327, 1327, 1327, 1327,  936,  936,  936,
 /*  1900 */   124,  936,  936,  936,  936,  333, 1327, 1327, 1327,   97,
 /*  1910 */    96,   95,   94,   93,  219,  217,  215,  214,  213, 1327,
 /*  1920 */  1327,  936,  936,  936,  936,  936,  838,  847,  848,  851,
 /*  1930 */   844,  850,  849,  843,  123,  450, 1327, 1327, 1327,   98,
 /*  1940 */   159,  158,  157, 1327,  156,  155,  154,  153,  108,  107,
 /*  1950 */   106, 1327,  105,  104,  103,  102, 1327, 1327, 1327,  552,
 /*  1960 */   842,  551, 1327, 1327,  164,  163,  162,  161,  160, 1327,
 /*  1970 */  1327, 1327,  113,  112,  111,  110,  109, 1327, 1327, 1327,
 /*  1980 */   755,  745,  851,  844,  850,  849,  843,  755,  745,  851,
 /*  1990 */   844,  850,  849,  843,   16,  321,   51,   50,   49,   53,
 /*  2000 */    52,  644,  317, 1327,   54, 1327, 1327,  365,  742,  739,
 /*  2010 */  1327, 1327, 1327, 1327,  334,  742,  739,  755,  745,  851,
 /*  2020 */   844,  850,  849,  843,  755,  745,  851,  844,  850,  849,
 /*  2030 */   843, 1327,  744,  242,  240,  238,  237,  236, 1327,  321,
 /*  2040 */   643, 1327, 1327, 1327,  736,  742,  739, 1327, 1327, 1327,
 /*  2050 */   547,  741,  742,  739,  755,  745,  851,  844,  850,  849,
 /*  2060 */   843, 1327,   23,   22,   21,   25,   24, 1327, 1327,  321,
 /*  2070 */    26,  659,  242,  240,  238,  237,  236, 1327, 1327, 1327,
 /*  2080 */  1327,  740,  742,  739, 1327, 1327, 1327, 1327, 1327,  755,
 /*  2090 */   745,  851,  844,  850,  849,  843,  755,  745,  851,  844,
 /*  2100 */   850,  849,  843, 1327,  321,  242,  240,  238,  237,  236,
 /*  2110 */  1327,  321,  432,  656, 1327, 1327,  537,  742,  739, 1327,
 /*  2120 */  1327, 1327, 1327,  536,  742,  739, 1327,  755,  745,  851,
 /*  2130 */   844,  850,  849,  843, 1327,  755,  745,  851,  844,  850,
 /*  2140 */   849,  843,  321,  755,  745,  851,  844,  850,  849,  843,
 /*  2150 */   321, 1327, 1327, 1327,  376,  742,  739, 1327,  321, 1327,
 /*  2160 */  1327, 1327,  437,  742,  739, 1327, 1327, 1327, 1327, 1327,
 /*  2170 */   436,  742,  739,  838,  847,  848,  851,  844,  850,  849,
 /*  2180 */   843,  486,  421,  838,  847,  848,  851,  844,  850,  849,
 /*  2190 */   843,  484,  421,  838,  847,  848,  851,  844,  850,  849,
 /*  2200 */   843,  479,  421,  717, 1327,  838,  847,  848,  851,  844,
 /*  2210 */   850,  849,  843,  485,  421,  824,  406,  851,  844,  850,
 /*  2220 */   849,  843,   36,   35,   34,   38,   37, 1327,  369,  407,
 /*  2230 */    39, 1327, 1327, 1327, 1327,  838,  847,  848,  851,  844,
 /*  2240 */   850,  849,  843, 1327,  371, 1327, 1327, 1327, 1327, 1327,
 /*  2250 */   712,  711,  713, 1327,  824,  792,  851,  844,  850,  849,
 /*  2260 */   843, 1327, 1327, 1327, 1327,  714,  715,  794,  407, 1327,
 /*  2270 */  1327, 1327, 1327,  220, 1327, 1327, 1327,  838,  847,  848,
 /*  2280 */   851,  844,  850,  849,  843, 1327,  451,  838,  847,  848,
 /*  2290 */   851,  844,  850,  849,  843, 1327,  829,  838,  847,  848,
 /*  2300 */   851,  844,  850,  849,  843, 1327,  825,  218, 1327, 1327,
 /*  2310 */  1327, 1327,  216, 1327, 1327, 1327,  838,  847,  848,  851,
 /*  2320 */   844,  850,  849,  843, 1327,  828,  838,  847,  848,  851,
 /*  2330 */   844,  850,  849,  843, 1327,  827,  838,  847,  848,  851,
 /*  2340 */   844,  850,  849,  843, 1327,  826,  838,  847,  848,  851,
 /*  2350 */   844,  850,  849,  843, 1327,  449,  838,  847,  848,  851,
 /*  2360 */   844,  850,  849,  843, 1327,  448,  838,  847,  848,  851,
 /*  2370 */   844,  850,  849,  843, 1327,  820, 1327, 1327, 1327, 1327,
 /*  2380 */  1327, 1327,  838,  847,  848,  851,  844,  850,  849,  843,
 /*  2390 */  1327,  819,  838,  847,  848,  851,  844,  850,  849,  843,
 /*  2400 */  1327,  818, 1327, 1327,  838,  847,  848,  851,  844,  850,
 /*  2410 */   849,  843, 1327,  419,  838,  847,  848,  851,  844,  850,
 /*  2420 */   849,  843, 1327,  418, 1327, 1327, 1327,  838,  847,  848,
 /*  2430 */   851,  844,  850,  849,  843, 1327,  417,  838,  847,  848,
 /*  2440 */   851,  844,  850,  849,  843, 1327,  416,  838,  847,  848,
 /*  2450 */   851,  844,  850,  849,  843, 1327,  415,  838,  847,  848,
 /*  2460 */   851,  844,  850,  849,  843, 1327,  414, 1327, 1327, 1327,
 /*  2470 */   838,  847,  848,  851,  844,  850,  849,  843, 1327,  413,
 /*  2480 */   838,  847,  848,  851,  844,  850,  849,  843, 1327,  409,
 /*  2490 */   838,  847,  848,  851,  844,  850,  849,  843, 1327,  408,
 /*  2500 */   838,  847,  848,  851,  844,  850,  849,  843, 1327,  793,
 /*  2510 */   838,  847,  848,  851,  844,  850,  849,  843, 1327,  445,
 /*  2520 */   838,  847,  848,  851,  844,  850,  849,  843, 1327,  444,
 /*  2530 */  1327, 1327, 1327, 1327, 1327, 1327,  838,  847,  848,  851,
 /*  2540 */   844,  850,  849,  843, 1327,  791,  838,  847,  848,  851,
 /*  2550 */   844,  850,  849,  843, 1327,  790, 1327, 1327,  838,  847,
 /*  2560 */   848,  851,  844,  850,  849,  843, 1327,  789,  838,  847,
 /*  2570 */   848,  851,  844,  850,  849,  843, 1327,  443, 1327, 1327,
 /*  2580 */  1327,  838,  847,  848,  851,  844,  850,  849,  843, 1327,
 /*  2590 */   442,  838,  847,  848,  851,  844,  850,  849,  843, 1327,
 /*  2600 */   788,  838,  847,  848,  851,  844,  850,  849,  843, 1327,
 /*  2610 */   787,  838,  847,  848,  851,  844,  850,  849,  843, 1327,
 /*  2620 */   786, 1327, 1327, 1327,  838,  847,  848,  851,  844,  850,
 /*  2630 */   849,  843, 1327,  405,  838,  847,  848,  851,  844,  850,
 /*  2640 */   849,  843, 1327,  404,  838,  847,  848,  851,  844,  850,
 /*  2650 */   849,  843, 1327,  403,  838,  847,  848,  851,  844,  850,
 /*  2660 */   849,  843, 1327,  402,  838,  847,  848,  851,  844,  850,
 /*  2670 */   849,  843, 1327,  401,  838,  847,  848,  851,  844,  850,
 /*  2680 */   849,  843, 1327,  400, 1327, 1327, 1327, 1327, 1327, 1327,
 /*  2690 */   838,  847,  848,  851,  844,  850,  849,  843, 1327,  399,
 /*  2700 */   838,  847,  848,  851,  844,  850,  849,  843, 1327,  398,
 /*  2710 */  1327, 1327,  838,  847,  848,  851,  844,  850,  849,  843,
 /*  2720 */  1327,  397,  838,  847,  848,  851,  844,  850,  849,  843,
 /*  2730 */  1327,  396, 1327, 1327, 1327,  838,  847,  848,  851,  844,
 /*  2740 */   850,  849,  843, 1327,  395,  838,  847,  848,  851,  844,
 /*  2750 */   850,  849,  843, 1327,  394,  838,  847,  848,  851,  844,
 /*  2760 */   850,  849,  843, 1327,  393,  838,  847,  848,  851,  844,
 /*  2770 */   850,  849,  843, 1327,  392, 1327, 1327, 1327,  838,  847,
 /*  2780 */   848,  851,  844,  850,  849,  843, 1327,  391,  838,  847,
 /*  2790 */   848,  851,  844,  850,  849,  843, 1327,  390,  838,  847,
 /*  2800 */   848,  851,  844,  850,  849,  843, 1327,  388,  838,  847,
 /*  2810 */   848,  851,  844,  850,  849,  843, 1327,  387,  838,  847,
 /*  2820 */   848,  851,  844,  850,  849,  843, 1327,  386,  838,  847,
 /*  2830 */   848,  851,  844,  850,  849,  843, 1327,  385, 1327, 1327,
 /*  2840 */  1327, 1327, 1327, 1327,  838,  847,  848,  851,  844,  850,
 /*  2850 */   849,  843, 1327,  384,  838,  847,  848,  851,  844,  850,
 /*  2860 */   849,  843, 1327,  229, 1327, 1327,  838,  847,  848,  851,
 /*  2870 */   844,  850,  849,  843, 1327,  228,  838,  847,  848,  851,
 /*  2880 */   844,  850,  849,  843, 1327,  374,  816,  807,  851,  844,
 /*  2890 */   850,  849,  843, 1327, 1327, 1327, 1327,  412, 1327,  816,
 /*  2900 */   807,  851,  844,  850,  849,  843, 1327, 1327, 1327, 1327,
 /*  2910 */   446,  816,  807,  851,  844,  850,  849,  843, 1327, 1327,
 /*  2920 */  1327, 1327,  370,  816,  807,  851,  844,  850,  849,  843,
 /*  2930 */  1327, 1327, 1327, 1327,  447,  816,  807,  851,  844,  850,
 /*  2940 */   849,  843, 1327, 1327, 1327,  547,  806,  816,  807,  851,
 /*  2950 */   844,  850,  849,  843, 1327, 1327, 1327, 1327,  802,  816,
 /*  2960 */   807,  851,  844,  850,  849,  843,  659, 1327, 1327, 1327,
 /*  2970 */   805, 1327,  816,  807,  851,  844,  850,  849,  843, 1327,
 /*  2980 */  1327, 1327, 1327,  804,  816,  807,  851,  844,  850,  849,
 /*  2990 */   843, 1327, 1327, 1327, 1327,  803, 1327, 1327, 1327,  816,
 /*  3000 */   807,  851,  844,  850,  849,  843, 1327,  431,  656, 1327,
 /*  3010 */   410,  816,  807,  851,  844,  850,  849,  843, 1327, 1327,
 /*  3020 */  1327, 1327,  439,  816,  807,  851,  844,  850,  849,  843,
 /*  3030 */  1327, 1327, 1327, 1327,  438,  816,  807,  851,  844,  850,
 /*  3040 */   849,  843, 1327, 1327, 1327, 1327,  751,  816,  807,  851,
 /*  3050 */   844,  850,  849,  843, 1327, 1327, 1327, 1327,  750,  816,
 /*  3060 */   807,  851,  844,  850,  849,  843, 1327, 1327, 1327, 1327,
 /*  3070 */   749,  816,  807,  851,  844,  850,  849,  843, 1327, 1327,
 /*  3080 */  1327, 1327,  383,  816,  807,  851,  844,  850,  849,  843,
 /*  3090 */  1327, 1327, 1327, 1327,  382,  816,  807,  851,  844,  850,
 /*  3100 */   849,  843, 1327, 1327, 1327, 1327,  381,  816,  807,  851,
 /*  3110 */   844,  850,  849,  843, 1327, 1327, 1327, 1327,  380,  816,
 /*  3120 */   807,  851,  844,  850,  849,  843, 1327, 1327, 1327, 1327,
 /*  3130 */   379,  816,  807,  851,  844,  850,  849,  843, 1327, 1327,
 /*  3140 */  1327, 1327,  378,  816,  807,  851,  844,  850,  849,  843,
 /*  3150 */  1327, 1327, 1327, 1327,  377,  816,  807,  851,  844,  850,
 /*  3160 */   849,  843, 1327, 1327, 1327, 1327,  743,  638,  629,  851,
 /*  3170 */   844,  850,  849,  843, 1327, 1327, 1327,  638,  629,  851,
 /*  3180 */   844,  850,  849,  843,  638,  629,  851,  844,  850,  849,
 /*  3190 */   843,  638,  629,  851,  844,  850,  849,  843, 1327, 1327,
 /*  3200 */  1327, 1327,  427,  638,  629,  851,  844,  850,  849,  843,
 /*  3210 */  1327, 1327,  628, 1327, 1327, 1327, 1327, 1327, 1327,  428,
 /*  3220 */  1327, 1327, 1327, 1327, 1327, 1327,  627,  638,  629,  851,
 /*  3230 */   844,  850,  849,  843, 1327, 1327, 1327, 1327,  626,  638,
 /*  3240 */   629,  851,  844,  850,  849,  843, 1327, 1327, 1327,  638,
 /*  3250 */   629,  851,  844,  850,  849,  843, 1327, 1327, 1327, 1327,
 /*  3260 */   560, 1327,  625,  638,  629,  851,  844,  850,  849,  843,
 /*  3270 */  1327, 1327, 1327, 1327,  624,  638,  629,  851,  844,  850,
 /*  3280 */   849,  843, 1327, 1327,  425,  638,  629,  851,  844,  850,
 /*  3290 */   849,  843,  242,  240,  238,  237,  236, 1327,  623,  638,
 /*  3300 */   629,  851,  844,  850,  849,  843, 1327, 1327, 1327, 1327,
 /*  3310 */   622,  638,  629,  851,  844,  850,  849,  843, 1327, 1327,
 /*  3320 */   621,  638,  629,  851,  844,  850,  849,  843, 1327, 1327,
 /*  3330 */  1327, 1327, 1327, 1327,  356,  638,  629,  851,  844,  850,
 /*  3340 */   849,  843, 1327, 1327, 1327, 1327,  355,  638,  629,  851,
 /*  3350 */   844,  850,  849,  843, 1327, 1327,  354,  638,  629,  851,
 /*  3360 */   844,  850,  849,  843, 1327, 1327, 1327, 1327, 1327, 1327,
 /*  3370 */   353, 1327, 1327, 1327,  638,  629,  851,  844,  850,  849,
 /*  3380 */   843, 1327,  352,  638,  629,  851,  844,  850,  849,  843,
 /*  3390 */  1327, 1327,  351,  638,  629,  851,  844,  850,  849,  843,
 /*  3400 */   638,  629,  851,  844,  850,  849,  843, 1327, 1327,  350,
 /*  3410 */   638,  629,  851,  844,  850,  849,  843, 1327,  616,  638,
 /*  3420 */   629,  851,  844,  850,  849,  843,  798, 1327,  349, 1327,
 /*  3430 */  1327,  547, 1327,  798, 1327,  348, 1327, 1327,  547, 1327,
 /*  3440 */  1327, 1327, 1327, 1327, 1327,  347, 1327, 1327, 1327, 1327,
 /*  3450 */  1327, 1327,  545, 1327,  346,  208,  482, 1327, 1327,  543,
 /*  3460 */  1327, 1327,  207,  482,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   135,   80,    0,  138,  139,  140,  141,  142,  143,  144,
 /*    10 */   145,  146,  147,  148,  149,  150,  151,  152,    2,  154,
 /*    20 */   155,  156,  157,  158,  159,  160,  105,  106,  107,  108,
 /*    30 */   109,   72,  167,  168,  154,  155,  156,  157,  158,  159,
 /*    40 */   160,  176,  177,   95,   96,   97,   98,  167,  168,   67,
 /*    50 */   102,  107,  108,  109,  174,  175,  176,  177,  178,  179,
 /*    60 */   170,  102,  172,  198,  199,  200,   71,  187,  188,  154,
 /*    70 */   155,  156,  157,  158,  159,  160,   94,   95,   96,   97,
 /*    80 */    98,   71,  167,  168,  102,  107,  108,  109,   71,  174,
 /*    90 */   175,  176,  177,  178,  179,  205,  206,  207,  208,  209,
 /*   100 */    73,  236,  237,  238,  239,  240,  241,  242,  243,  244,
 /*   110 */   245,  246,  247,  248,  249,  250,  251,  252,  253,  254,
 /*   120 */     1,    2,   72,   71,    5,    6,    7,    8,    9,   10,
 /*   130 */    11,   12,  105,  106,  107,  108,  109,   73,  154,  155,
 /*   140 */   156,  157,  158,  159,  160,   33,  130,   28,   36,   99,
 /*   150 */   100,   32,   41,   34,   35,   43,   37,   93,   39,   47,
 /*   160 */    76,  177,   81,   44,  170,  102,  172,   48,   49,   50,
 /*   170 */    51,   52,   53,   54,  190,   56,   57,  193,  194,  195,
 /*   180 */   196,  197,   63,   64,    1,    2,  102,   68,   69,   98,
 /*   190 */    71,   97,   98,  102,   75,   71,  102,   72,   79,  205,
 /*   200 */   206,  207,  208,  209,   70,   22,   23,   24,   25,   70,
 /*   210 */     1,    2,   70,   30,    5,    6,  105,  106,  107,  108,
 /*   220 */   109,   38,   72,   40,  105,   97,   98,  102,   45,  110,
 /*   230 */   102,   48,   49,   50,    1,    2,   71,  118,    5,    6,
 /*   240 */    57,   58,   59,   60,   90,   91,   63,   64,  155,  130,
 /*   250 */   131,  132,   98,  134,   71,   46,  106,   48,   49,   50,
 /*   260 */   153,  154,  155,  156,  157,  158,  159,  160,  161,  162,
 /*   270 */   177,   81,   63,   64,   73,   66,  155,   70,   69,  163,
 /*   280 */    71,   48,   49,   50,   75,    1,    2,  166,  105,    5,
 /*   290 */     6,   75,  199,  110,   93,   73,   63,   64,  177,   66,
 /*   300 */   184,  180,   69,  102,   71,  210,  211,  212,   75,   95,
 /*   310 */    96,   97,   98,  104,  105,   93,  102,   99,  100,  110,
 /*   320 */    81,  105,  154,  155,  156,  157,  158,  159,  160,  170,
 /*   330 */   163,  172,   48,   49,   50,  167,  168,  104,  105,  130,
 /*   340 */   131,  132,  226,  110,  176,  177,  130,   63,   64,   73,
 /*   350 */    66,  184,   41,   69,   89,   71,    1,    1,    2,   75,
 /*   360 */   102,    5,    6,  130,  131,  132,  198,  199,  200,   93,
 /*   370 */   105,  106,  107,  108,  109,  216,  217,  218,  154,  155,
 /*   380 */   156,  157,  158,  159,  160,  170,  163,  172,  104,  105,
 /*   390 */    79,  167,  168,  226,  110,  107,  108,  109,  174,  175,
 /*   400 */   176,  177,  178,  179,   48,   49,   50,  184,   72,   31,
 /*   410 */    73,  187,  188,  102,  130,  131,  132,   72,  155,   63,
 /*   420 */    64,  206,  207,  208,  209,   69,   71,   71,  213,  214,
 /*   430 */    93,   75,   73,  154,  155,  156,  157,  158,  159,  160,
 /*   440 */   177,  105,  106,  107,  108,  109,   14,  102,   16,   17,
 /*   450 */    18,   19,   20,   21,   97,   98,  177,   73,   73,  102,
 /*   460 */   105,  105,  199,    1,    2,  110,  110,    5,    6,  190,
 /*   470 */    90,   91,  193,  194,  195,  196,  197,   93,   98,   41,
 /*   480 */    48,   49,  107,  108,  109,   73,  130,  131,  132,  154,
 /*   490 */   155,  156,  157,  158,  159,  160,  154,  155,  156,  157,
 /*   500 */   158,  159,  160,  107,  108,  109,  227,  228,  229,   73,
 /*   510 */    48,   49,   50,    1,    2,   75,   73,    5,    6,  177,
 /*   520 */    95,   96,   97,   98,  189,   63,   64,  102,   31,  187,
 /*   530 */   188,   73,  190,   71,   72,  193,  194,  195,  196,  197,
 /*   540 */   102,  105,  106,  107,  108,  109,  154,  155,  156,  157,
 /*   550 */   158,  159,  160,  154,  155,  156,  157,  158,  159,  160,
 /*   560 */    48,   49,   50,  164,  165,  133,   72,  105,   73,  177,
 /*   570 */   130,  131,  110,   98,   80,   63,   64,  102,   76,  187,
 /*   580 */   188,   75,  190,   71,   96,  193,  194,  195,  196,  197,
 /*   590 */     1,    2,  130,  131,  132,   69,    1,    2,   72,  105,
 /*   600 */   106,  107,  108,  109,  102,   71,   80,   81,   82,   83,
 /*   610 */    27,   85,   86,   87,   88,   73,  104,  105,   74,  219,
 /*   620 */   220,  221,  110,  154,  155,  156,  157,  158,  159,  160,
 /*   630 */    96,  105,  106,  107,  108,  109,  130,   48,   49,   50,
 /*   640 */    76,   73,  130,  131,  132,   81,  177,  105,  106,  107,
 /*   650 */   108,  109,   63,   64,    1,    1,    2,  106,   75,  190,
 /*   660 */    71,   93,  193,  194,  195,  196,  197,    1,    2,   76,
 /*   670 */   102,    5,    6,    1,    2,    1,    2,    5,    6,    1,
 /*   680 */     2,  107,   74,    5,    6,  111,   55,  154,  155,  156,
 /*   690 */   157,  158,  159,  160,  105,  102,   71,  228,  229,  110,
 /*   700 */   167,  168,   48,   49,   50,    2,   72,  174,  175,  176,
 /*   710 */   177,  178,  179,  130,   48,   49,   50,   63,   64,   73,
 /*   720 */    48,   49,   50,    2,   71,   71,   48,   49,   50,   63,
 /*   730 */    64,  170,  170,  172,  172,   63,   64,   71,    2,   93,
 /*   740 */   106,   63,   64,   71,  105,  106,  107,  108,  109,   71,
 /*   750 */   154,  155,  156,  157,  158,  159,  160,   27,  105,  105,
 /*   760 */   222,  223,  224,  110,  110,   72,  205,  206,  207,  208,
 /*   770 */   209,  105,   71,  177,   76,   71,  110,  105,   75,   81,
 /*   780 */   218,   72,  110,  105,   71,  170,  190,  172,  110,  193,
 /*   790 */   194,  195,  196,  197,   71,  102,  130,  131,  132,   72,
 /*   800 */    72,   72,  130,  131,  132,   75,  132,   71,  130,  131,
 /*   810 */   132,  102,  201,  202,  154,  155,  156,  157,  158,  159,
 /*   820 */   160,  206,  207,  208,  209,   70,    2,  167,  168,  102,
 /*   830 */   102,  102,   76,  130,  174,  175,  176,  177,  178,  179,
 /*   840 */    71,   73,  154,  155,  156,  157,  158,  159,  160,   94,
 /*   850 */    95,   96,   97,   98,   72,  167,  168,  102,  102,    2,
 /*   860 */   130,   93,  174,  175,  176,  177,  178,  179,  102,  154,
 /*   870 */   155,  156,  157,  158,  159,  160,    3,    4,   72,   70,
 /*   880 */   211,  212,  167,  168,  102,  171,  170,  173,  172,  174,
 /*   890 */   175,  176,  177,  178,  179,    2,  154,  155,  156,  157,
 /*   900 */   158,  159,  160,   94,   95,   96,   97,   98,  102,  167,
 /*   910 */   168,  102,   72,   90,   91,   72,  174,  175,  176,  177,
 /*   920 */   178,  179,  206,  207,  208,  209,  154,  155,  156,  157,
 /*   930 */   158,  159,  160,   72,   94,   95,   96,   97,   98,  167,
 /*   940 */   168,   73,  102,   73,   26,  102,  174,  175,  176,  177,
 /*   950 */   178,  179,  154,  155,  156,  157,  158,  159,  160,   41,
 /*   960 */    42,  102,  170,  102,  172,  167,  168,  220,  221,  185,
 /*   970 */   186,   76,  174,  175,  176,  177,  178,  179,   76,   61,
 /*   980 */    62,   73,  154,  155,  156,  157,  158,  159,  160,   73,
 /*   990 */    72,   73,   72,  203,  204,  167,  168,   79,  206,  207,
 /*  1000 */   208,  209,  174,  175,  176,  177,  178,  179,  155,   73,
 /*  1010 */   154,  155,  156,  157,  158,  159,  160,  223,  224,  166,
 /*  1020 */   102,   70,   73,  167,  168,  105,  106,  107,  108,  109,
 /*  1030 */   174,  175,  176,  177,  178,  179,   73,  154,  155,  156,
 /*  1040 */   157,  158,  159,  160,   67,   94,   95,   96,   97,   98,
 /*  1050 */   167,  168,   73,  102,  223,  224,   73,  174,  175,  176,
 /*  1060 */   177,  178,  179,   73,  154,  155,  156,  157,  158,  159,
 /*  1070 */   160,   94,   95,   96,   97,   98,   72,  167,  168,  102,
 /*  1080 */   163,  223,  224,   73,  174,  175,  176,  177,  178,  179,
 /*  1090 */    73,  170,   73,  172,  154,  155,  156,  157,  158,  159,
 /*  1100 */   160,  184,   73,   70,   41,  223,  224,  167,  168,  105,
 /*  1110 */   106,  107,  108,  109,  174,  175,  176,  177,  178,  179,
 /*  1120 */   154,  155,  156,  157,  158,  159,  160,   94,   95,   96,
 /*  1130 */    97,   98,   73,  167,  168,  102,  215,  216,  217,  218,
 /*  1140 */   174,  175,  176,  177,  178,  179,  185,  186,  136,  137,
 /*  1150 */   154,  155,  156,  157,  158,  159,  160,   94,   95,   96,
 /*  1160 */    97,   98,   73,  167,  168,  102,   65,  130,  163,   89,
 /*  1170 */   174,  175,  176,  177,  178,  179,  158,  131,  154,  155,
 /*  1180 */   156,  157,  158,  159,  160,  105,  106,  107,  108,  109,
 /*  1190 */    72,  167,  168,  105,  106,  107,  108,  109,  174,  175,
 /*  1200 */   176,  177,  178,  179,  155,  154,  155,  156,  157,  158,
 /*  1210 */   159,  160,   94,   95,   96,   97,   98,   72,  167,  168,
 /*  1220 */   102,  130,  170,  158,  172,  174,  175,  176,  177,  178,
 /*  1230 */   179,  158,  154,  155,  156,  157,  158,  159,  160,   94,
 /*  1240 */    95,   96,   97,   98,   72,  167,  168,  102,  233,   42,
 /*  1250 */    26,   61,  174,  175,  176,  177,  178,  179,  206,  207,
 /*  1260 */   208,  209,  154,  155,  156,  157,  158,  159,  160,  234,
 /*  1270 */   232,   70,   62,    2,  235,  167,  168,  105,  106,  107,
 /*  1280 */   108,  109,  174,  175,  176,  177,  178,  179,  154,  155,
 /*  1290 */   156,  157,  158,  159,  160,   94,   95,   96,   97,   98,
 /*  1300 */    72,  167,  168,  102,  234,  155,  235,  235,  174,  175,
 /*  1310 */   176,  177,  178,  179,  155,  234,  234,  234,  154,  155,
 /*  1320 */   156,  157,  158,  159,  160,  235,  170,  235,  172,  202,
 /*  1330 */   209,  167,  168,  105,  106,  107,  108,  109,  174,  175,
 /*  1340 */   176,  177,  178,  179,    2,  233,  154,  155,  156,  157,
 /*  1350 */   158,  159,  160,  234,  232,  235,  233,   70,  232,  167,
 /*  1360 */   168,  205,  206,  207,  208,  209,  174,  175,  176,  177,
 /*  1370 */   178,  179,  234,  154,  155,  156,  157,  158,  159,  160,
 /*  1380 */   234,   94,   95,   96,   97,   98,  167,  168,   69,  102,
 /*  1390 */   235,   72,  235,  174,  175,  176,  177,  178,  179,  234,
 /*  1400 */    81,   82,   83,  235,   85,   86,   87,   88,  234,  154,
 /*  1410 */   155,  156,  157,  158,  159,  160,  235,   69,  234,  155,
 /*  1420 */    72,  235,  231,   41,  105,  106,  107,  108,  109,   81,
 /*  1430 */    82,   83,  177,   85,   86,   87,   88,  153,  154,  155,
 /*  1440 */   156,  157,  158,  159,  160,  190,  162,   69,  193,  194,
 /*  1450 */   195,  196,  197,  105,  106,  107,  108,  109,   80,   81,
 /*  1460 */    82,   83,  233,   85,   86,   87,   88,  154,  155,  156,
 /*  1470 */   157,  158,  159,  160,  232,  234,   70,  155,  235,  231,
 /*  1480 */   167,  168,  233,  105,  106,  107,  108,  109,  232,  176,
 /*  1490 */   177,  234,  155,  154,  155,  156,  157,  158,  159,  160,
 /*  1500 */    94,   95,   96,   97,   98,  235,  167,  168,  102,  231,
 /*  1510 */   233,  198,  199,  200,  232,  176,  177,  234,  155,  231,
 /*  1520 */   235,  154,  155,  156,  157,  158,  159,  160,  234,   70,
 /*  1530 */   235,  235,  231,  204,  167,  168,  231,  198,  199,  200,
 /*  1540 */   231,  231,  186,  176,  177,  157,  154,  155,  156,  157,
 /*  1550 */   158,  159,  160,   94,   95,   96,   97,   98,  173,  167,
 /*  1560 */   168,  102,  173,  173,  173,  198,  199,  200,  176,  177,
 /*  1570 */   173,  173,  154,  155,  156,  157,  158,  159,  160,   94,
 /*  1580 */    95,   96,   97,   98,  173,  167,  168,  102,  173,  173,
 /*  1590 */   198,  199,  200,  173,  176,  177,  173,  173,  173,  173,
 /*  1600 */   154,  155,  156,  157,  158,  159,  160,  173,  154,  155,
 /*  1610 */   156,  157,  158,  159,  160,  231,  198,  199,  200,  172,
 /*  1620 */   172,  235,  235,  177,  154,  155,  156,  157,  158,  159,
 /*  1630 */   160,  177,  231,  172,  232,  232,  190,  167,  168,  193,
 /*  1640 */   194,  195,  196,  197,  190,  232,  232,  193,  194,  195,
 /*  1650 */   196,  197,  154,  155,  156,  157,  158,  159,  160,  231,
 /*  1660 */   172,  154,  155,  156,  157,  158,  159,  160,  172,  172,
 /*  1670 */     2,  172,  172,  172,    2,  177,  154,  155,  156,  157,
 /*  1680 */   158,  159,  160,  255,  177,  255,  255,  255,  190,  255,
 /*  1690 */   255,  193,  194,  195,  196,  197,  255,  190,  255,  177,
 /*  1700 */   193,  194,  195,  196,  197,  154,  155,  156,  157,  158,
 /*  1710 */   159,  160,  190,  255,  255,  193,  194,  195,  196,  197,
 /*  1720 */   154,  155,  156,  157,  158,  159,  160,  255,  177,  154,
 /*  1730 */   155,  156,  157,  158,  159,  160,  105,  106,  107,  108,
 /*  1740 */   109,  190,  255,  177,  193,  194,  195,  196,  197,  255,
 /*  1750 */   255,  255,  177,  255,  255,  255,  190,  255,  255,  193,
 /*  1760 */   194,  195,  196,  197,  255,  190,  255,  255,  193,  194,
 /*  1770 */   195,  196,  197,  154,  155,  156,  157,  158,  159,  160,
 /*  1780 */   255,   69,  255,   26,  105,  106,  107,  108,  109,   69,
 /*  1790 */   255,  255,  255,   81,   82,   83,  177,   85,   86,   87,
 /*  1800 */    88,   81,   82,   83,   62,   85,   86,   87,   88,  190,
 /*  1810 */   255,  255,  193,  194,  195,  196,  197,  105,  106,  107,
 /*  1820 */   108,  109,   65,  255,  255,  105,  106,  107,  108,  109,
 /*  1830 */     1,    2,   72,  255,    5,    6,   94,   95,   96,   97,
 /*  1840 */    98,   81,   82,   83,  102,   85,   86,   87,   88,  255,
 /*  1850 */   255,   94,   95,   96,   97,   98,   81,   82,   83,  102,
 /*  1860 */    85,   86,   87,   88,  255,  105,  106,  107,  108,  109,
 /*  1870 */   154,  155,  156,  157,  158,  159,  160,   48,   49,   50,
 /*  1880 */   105,  106,  107,  108,  109,   81,   82,   83,  255,   85,
 /*  1890 */    86,   87,   88,  255,  255,  255,  255,   81,   82,   83,
 /*  1900 */    71,   85,   86,   87,   88,  189,  255,  255,  255,  105,
 /*  1910 */   106,  107,  108,  109,  105,  106,  107,  108,  109,  255,
 /*  1920 */   255,  105,  106,  107,  108,  109,  153,  154,  155,  156,
 /*  1930 */   157,  158,  159,  160,  105,  162,  255,  255,  255,  110,
 /*  1940 */    81,   82,   83,  255,   85,   86,   87,   88,   81,   82,
 /*  1950 */    83,  255,   85,   86,   87,   88,  255,  255,  255,  130,
 /*  1960 */   131,  132,  255,  255,  105,  106,  107,  108,  109,  255,
 /*  1970 */   255,  255,  105,  106,  107,  108,  109,  255,  255,  255,
 /*  1980 */   154,  155,  156,  157,  158,  159,  160,  154,  155,  156,
 /*  1990 */   157,  158,  159,  160,   26,  169,   94,   95,   96,   97,
 /*  2000 */    98,   73,  169,  255,  102,  255,  255,  181,  182,  183,
 /*  2010 */   255,  255,  255,  255,  181,  182,  183,  154,  155,  156,
 /*  2020 */   157,  158,  159,  160,  154,  155,  156,  157,  158,  159,
 /*  2030 */   160,  255,  169,  105,  106,  107,  108,  109,  255,  169,
 /*  2040 */    73,  255,  255,  255,  181,  182,  183,  255,  255,  255,
 /*  2050 */   163,  181,  182,  183,  154,  155,  156,  157,  158,  159,
 /*  2060 */   160,  255,   94,   95,   96,   97,   98,  255,  255,  169,
 /*  2070 */   102,  184,  105,  106,  107,  108,  109,  255,  255,  255,
 /*  2080 */   255,  181,  182,  183,  255,  255,  255,  255,  255,  154,
 /*  2090 */   155,  156,  157,  158,  159,  160,  154,  155,  156,  157,
 /*  2100 */   158,  159,  160,  255,  169,  105,  106,  107,  108,  109,
 /*  2110 */   255,  169,  225,  226,  255,  255,  181,  182,  183,  255,
 /*  2120 */   255,  255,  255,  181,  182,  183,  255,  154,  155,  156,
 /*  2130 */   157,  158,  159,  160,  255,  154,  155,  156,  157,  158,
 /*  2140 */   159,  160,  169,  154,  155,  156,  157,  158,  159,  160,
 /*  2150 */   169,  255,  255,  255,  181,  182,  183,  255,  169,  255,
 /*  2160 */   255,  255,  181,  182,  183,  255,  255,  255,  255,  255,
 /*  2170 */   181,  182,  183,  153,  154,  155,  156,  157,  158,  159,
 /*  2180 */   160,  161,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2190 */   160,  161,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2200 */   160,  161,  162,    1,  255,  153,  154,  155,  156,  157,
 /*  2210 */   158,  159,  160,  161,  162,  154,  155,  156,  157,  158,
 /*  2220 */   159,  160,   94,   95,   96,   97,   98,  255,  167,  168,
 /*  2230 */   102,  255,  255,  255,  255,  153,  154,  155,  156,  157,
 /*  2240 */   158,  159,  160,  255,  162,  255,  255,  255,  255,  255,
 /*  2250 */    48,   49,   50,  255,  154,  155,  156,  157,  158,  159,
 /*  2260 */   160,  255,  255,  255,  255,   63,   64,  167,  168,  255,
 /*  2270 */   255,  255,  255,   71,  255,  255,  255,  153,  154,  155,
 /*  2280 */   156,  157,  158,  159,  160,  255,  162,  153,  154,  155,
 /*  2290 */   156,  157,  158,  159,  160,  255,  162,  153,  154,  155,
 /*  2300 */   156,  157,  158,  159,  160,  255,  162,  105,  255,  255,
 /*  2310 */   255,  255,  110,  255,  255,  255,  153,  154,  155,  156,
 /*  2320 */   157,  158,  159,  160,  255,  162,  153,  154,  155,  156,
 /*  2330 */   157,  158,  159,  160,  255,  162,  153,  154,  155,  156,
 /*  2340 */   157,  158,  159,  160,  255,  162,  153,  154,  155,  156,
 /*  2350 */   157,  158,  159,  160,  255,  162,  153,  154,  155,  156,
 /*  2360 */   157,  158,  159,  160,  255,  162,  153,  154,  155,  156,
 /*  2370 */   157,  158,  159,  160,  255,  162,  255,  255,  255,  255,
 /*  2380 */   255,  255,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2390 */   255,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2400 */   255,  162,  255,  255,  153,  154,  155,  156,  157,  158,
 /*  2410 */   159,  160,  255,  162,  153,  154,  155,  156,  157,  158,
 /*  2420 */   159,  160,  255,  162,  255,  255,  255,  153,  154,  155,
 /*  2430 */   156,  157,  158,  159,  160,  255,  162,  153,  154,  155,
 /*  2440 */   156,  157,  158,  159,  160,  255,  162,  153,  154,  155,
 /*  2450 */   156,  157,  158,  159,  160,  255,  162,  153,  154,  155,
 /*  2460 */   156,  157,  158,  159,  160,  255,  162,  255,  255,  255,
 /*  2470 */   153,  154,  155,  156,  157,  158,  159,  160,  255,  162,
 /*  2480 */   153,  154,  155,  156,  157,  158,  159,  160,  255,  162,
 /*  2490 */   153,  154,  155,  156,  157,  158,  159,  160,  255,  162,
 /*  2500 */   153,  154,  155,  156,  157,  158,  159,  160,  255,  162,
 /*  2510 */   153,  154,  155,  156,  157,  158,  159,  160,  255,  162,
 /*  2520 */   153,  154,  155,  156,  157,  158,  159,  160,  255,  162,
 /*  2530 */   255,  255,  255,  255,  255,  255,  153,  154,  155,  156,
 /*  2540 */   157,  158,  159,  160,  255,  162,  153,  154,  155,  156,
 /*  2550 */   157,  158,  159,  160,  255,  162,  255,  255,  153,  154,
 /*  2560 */   155,  156,  157,  158,  159,  160,  255,  162,  153,  154,
 /*  2570 */   155,  156,  157,  158,  159,  160,  255,  162,  255,  255,
 /*  2580 */   255,  153,  154,  155,  156,  157,  158,  159,  160,  255,
 /*  2590 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  255,
 /*  2600 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  255,
 /*  2610 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  255,
 /*  2620 */   162,  255,  255,  255,  153,  154,  155,  156,  157,  158,
 /*  2630 */   159,  160,  255,  162,  153,  154,  155,  156,  157,  158,
 /*  2640 */   159,  160,  255,  162,  153,  154,  155,  156,  157,  158,
 /*  2650 */   159,  160,  255,  162,  153,  154,  155,  156,  157,  158,
 /*  2660 */   159,  160,  255,  162,  153,  154,  155,  156,  157,  158,
 /*  2670 */   159,  160,  255,  162,  153,  154,  155,  156,  157,  158,
 /*  2680 */   159,  160,  255,  162,  255,  255,  255,  255,  255,  255,
 /*  2690 */   153,  154,  155,  156,  157,  158,  159,  160,  255,  162,
 /*  2700 */   153,  154,  155,  156,  157,  158,  159,  160,  255,  162,
 /*  2710 */   255,  255,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2720 */   255,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2730 */   255,  162,  255,  255,  255,  153,  154,  155,  156,  157,
 /*  2740 */   158,  159,  160,  255,  162,  153,  154,  155,  156,  157,
 /*  2750 */   158,  159,  160,  255,  162,  153,  154,  155,  156,  157,
 /*  2760 */   158,  159,  160,  255,  162,  153,  154,  155,  156,  157,
 /*  2770 */   158,  159,  160,  255,  162,  255,  255,  255,  153,  154,
 /*  2780 */   155,  156,  157,  158,  159,  160,  255,  162,  153,  154,
 /*  2790 */   155,  156,  157,  158,  159,  160,  255,  162,  153,  154,
 /*  2800 */   155,  156,  157,  158,  159,  160,  255,  162,  153,  154,
 /*  2810 */   155,  156,  157,  158,  159,  160,  255,  162,  153,  154,
 /*  2820 */   155,  156,  157,  158,  159,  160,  255,  162,  153,  154,
 /*  2830 */   155,  156,  157,  158,  159,  160,  255,  162,  255,  255,
 /*  2840 */   255,  255,  255,  255,  153,  154,  155,  156,  157,  158,
 /*  2850 */   159,  160,  255,  162,  153,  154,  155,  156,  157,  158,
 /*  2860 */   159,  160,  255,  162,  255,  255,  153,  154,  155,  156,
 /*  2870 */   157,  158,  159,  160,  255,  162,  153,  154,  155,  156,
 /*  2880 */   157,  158,  159,  160,  255,  162,  154,  155,  156,  157,
 /*  2890 */   158,  159,  160,  255,  255,  255,  255,  165,  255,  154,
 /*  2900 */   155,  156,  157,  158,  159,  160,  255,  255,  255,  255,
 /*  2910 */   165,  154,  155,  156,  157,  158,  159,  160,  255,  255,
 /*  2920 */   255,  255,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2930 */   255,  255,  255,  255,  165,  154,  155,  156,  157,  158,
 /*  2940 */   159,  160,  255,  255,  255,  163,  165,  154,  155,  156,
 /*  2950 */   157,  158,  159,  160,  255,  255,  255,  255,  165,  154,
 /*  2960 */   155,  156,  157,  158,  159,  160,  184,  255,  255,  255,
 /*  2970 */   165,  255,  154,  155,  156,  157,  158,  159,  160,  255,
 /*  2980 */   255,  255,  255,  165,  154,  155,  156,  157,  158,  159,
 /*  2990 */   160,  255,  255,  255,  255,  165,  255,  255,  255,  154,
 /*  3000 */   155,  156,  157,  158,  159,  160,  255,  225,  226,  255,
 /*  3010 */   165,  154,  155,  156,  157,  158,  159,  160,  255,  255,
 /*  3020 */   255,  255,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  3030 */   255,  255,  255,  255,  165,  154,  155,  156,  157,  158,
 /*  3040 */   159,  160,  255,  255,  255,  255,  165,  154,  155,  156,
 /*  3050 */   157,  158,  159,  160,  255,  255,  255,  255,  165,  154,
 /*  3060 */   155,  156,  157,  158,  159,  160,  255,  255,  255,  255,
 /*  3070 */   165,  154,  155,  156,  157,  158,  159,  160,  255,  255,
 /*  3080 */   255,  255,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  3090 */   255,  255,  255,  255,  165,  154,  155,  156,  157,  158,
 /*  3100 */   159,  160,  255,  255,  255,  255,  165,  154,  155,  156,
 /*  3110 */   157,  158,  159,  160,  255,  255,  255,  255,  165,  154,
 /*  3120 */   155,  156,  157,  158,  159,  160,  255,  255,  255,  255,
 /*  3130 */   165,  154,  155,  156,  157,  158,  159,  160,  255,  255,
 /*  3140 */   255,  255,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  3150 */   255,  255,  255,  255,  165,  154,  155,  156,  157,  158,
 /*  3160 */   159,  160,  255,  255,  255,  255,  165,  154,  155,  156,
 /*  3170 */   157,  158,  159,  160,  255,  255,  255,  154,  155,  156,
 /*  3180 */   157,  158,  159,  160,  154,  155,  156,  157,  158,  159,
 /*  3190 */   160,  154,  155,  156,  157,  158,  159,  160,  255,  255,
 /*  3200 */   255,  255,  189,  154,  155,  156,  157,  158,  159,  160,
 /*  3210 */   255,  255,  189,  255,  255,  255,  255,  255,  255,  189,
 /*  3220 */   255,  255,  255,  255,  255,  255,  189,  154,  155,  156,
 /*  3230 */   157,  158,  159,  160,  255,  255,  255,  255,  189,  154,
 /*  3240 */   155,  156,  157,  158,  159,  160,  255,  255,  255,  154,
 /*  3250 */   155,  156,  157,  158,  159,  160,  255,  255,  255,  255,
 /*  3260 */    73,  255,  189,  154,  155,  156,  157,  158,  159,  160,
 /*  3270 */   255,  255,  255,  255,  189,  154,  155,  156,  157,  158,
 /*  3280 */   159,  160,  255,  255,  189,  154,  155,  156,  157,  158,
 /*  3290 */   159,  160,  105,  106,  107,  108,  109,  255,  189,  154,
 /*  3300 */   155,  156,  157,  158,  159,  160,  255,  255,  255,  255,
 /*  3310 */   189,  154,  155,  156,  157,  158,  159,  160,  255,  255,
 /*  3320 */   189,  154,  155,  156,  157,  158,  159,  160,  255,  255,
 /*  3330 */   255,  255,  255,  255,  189,  154,  155,  156,  157,  158,
 /*  3340 */   159,  160,  255,  255,  255,  255,  189,  154,  155,  156,
 /*  3350 */   157,  158,  159,  160,  255,  255,  189,  154,  155,  156,
 /*  3360 */   157,  158,  159,  160,  255,  255,  255,  255,  255,  255,
 /*  3370 */   189,  255,  255,  255,  154,  155,  156,  157,  158,  159,
 /*  3380 */   160,  255,  189,  154,  155,  156,  157,  158,  159,  160,
 /*  3390 */   255,  255,  189,  154,  155,  156,  157,  158,  159,  160,
 /*  3400 */   154,  155,  156,  157,  158,  159,  160,  255,  255,  189,
 /*  3410 */   154,  155,  156,  157,  158,  159,  160,  255,  189,  154,
 /*  3420 */   155,  156,  157,  158,  159,  160,  158,  255,  189,  255,
 /*  3430 */   255,  163,  255,  158,  255,  189,  255,  255,  163,  255,
 /*  3440 */   255,  255,  255,  255,  255,  189,  255,  255,  255,  255,
 /*  3450 */   255,  255,  184,  255,  189,  187,  188,  255,  255,  184,
 /*  3460 */   255,  255,  187,  188,
};
#define YY_SHIFT_USE_DFLT (-80)
#define YY_SHIFT_COUNT (553)
#define YY_SHIFT_MIN   (-79)
#define YY_SHIFT_MAX   (3187)
static const short yy_shift_ofst[] = {
 /*     0 */   -80,  119,  209,  284,  284,  209,  233,  233,  284,  284,
 /*    10 */   284,  284,  284,  284,  284,  284,  284,  284,  284,  284,
 /*    20 */   284,  284,  284,  284,  284,  284,  284,  284,  284,  284,
 /*    30 */   284,  233,  233,  233,  233,  233,  233,  233,  233,  233,
 /*    40 */   233,  233,  233,  356,  356,  356,  356,  356,  356,  512,
 /*    50 */   512,  512,  512,  512,  512,  512,  512,  512,  512,  462,
 /*    60 */   678,  678,  678,  678,  678,  678,  678,  678,  678,  678,
 /*    70 */   678,  678,  678,  678,  678,  678,  678,  678,  678,  678,
 /*    80 */   678,  678,  678,  678,  678,  678,  678,  678,  678,  678,
 /*    90 */   678,  678,  678,  678,  678,  678,  678,  678,  678,  678,
 /*   100 */   678,  666,  678,  678,  678,  678,  678,  678,  678,  678,
 /*   110 */   678,  678,  678,  678,  678,  678,  678,  678,  678,  678,
 /*   120 */   678,  678,  678, 1829, 1829, 1829,  183,  672,  672,  672,
 /*   130 */   672,  672,  672,  672,  672,  672,  672,  672,  672,  672,
 /*   140 */   672,  672,  672,  672,  672,  672,  672,  672,  672,  672,
 /*   150 */   672,  672,  666,  666,  666,  666,  666,  666,  666,  666,
 /*   160 */   666,  666,  666,  666,  666,  666,  666,  666,  666,  666,
 /*   170 */   666,  666,  666,  666,  666,  654,  654,  654,  654,  654,
 /*   180 */   589,  654,  654,  654,  589,  440,  440,  730,  583,  703,
 /*   190 */   589,  216,  216,  506,  506,  736, 1672, 1668,  653,  823,
 /*   200 */   736,  736,  736,  736,  873,   16,  823,  506,  506, 1672,
 /*   210 */  1668, 1271, 2202, 2202, 2202, 2202, 2202, 2202, 2202, 2202,
 /*   220 */  2202, 1757, 1063, 1968, 1968, 1968, 1742, 1742,  111,  111,
 /*   230 */   355,  355,  355,  355,  355,  355,  355,  355,  355,  355,
 /*   240 */   355,  355,  355,  355,  355,  380,  674,  154,  311,  438,
 /*   250 */   438,  873,  438,  438, 1210, 1091, 1210, 1190, 1210, 1190,
 /*   260 */  1224, 1207, 1382, 1091, 1210, 1190, 1224, 1207, 1382, 1091,
 /*   270 */  1210, 1190, 1224, 1207, 1382, 1091, 1210, 1190, 1210, 1190,
 /*   280 */  1210, 1190, 1210, 1190, 1210, 1190, 1224, 1207, 1210, 1190,
 /*   290 */  1224, 1207, 1342, 1271, 1210, 1190, 1210, 1190, 1091, 1210,
 /*   300 */  1190, 1091, 1210, 1190, 1210, 1190, 1224, 1207, 1046, 1046,
 /*   310 */  1091, 1046, 1037,  526, 1378, 1348, 1319, 1760, 1720, 1712,
 /*   320 */  1867, 1859, 1816, 1804, 1775,  432,  918, 1145, 1459, 1406,
 /*   330 */  1287, 1201,  977,  494, 1118,  840, 1033,  951,  809,  755,
 /*   340 */   -18, 3187,   27, 2128, 2128, 1080,  -79,  -79,  -79,  -79,
 /*   350 */   -79,  -79,  -79,  -79,  -79,  -79,  -79, 1485, 1967, 1928,
 /*   360 */   542,  436, 1228,  265, 1172, 1902, 1485, 1485, 1485, 1004,
 /*   370 */   920,  336, 2000,  425,  639, 1809,  214, 1088, 1088, 1088,
 /*   380 */  1088, 1088, 1088, 1088,  639,  639,  639,  639,  639,  -52,
 /*   390 */   639,  639,  639,  639,  639,  639,  639,  639,  639,  639,
 /*   400 */   639,  639,  639,  639,  639,  639, 1679, 1631,  639,  639,
 /*   410 */  1088, 1088, 1088,  639,  639,  639,  639,  639,  639,  639,
 /*   420 */   639,  639,  112,  357,  357,  396,  396,  396,  396,  375,
 /*   430 */   375,  568,  201,   50,  288,  288,  128,  128,  -22,  -22,
 /*   440 */    94,   94,  -56,  -56,  -56,  -56,  -22,  -22,  -56,  -56,
 /*   450 */   -56,  -56,  595,  768,  646,  698,  564,  218,  218,  218,
 /*   460 */   218,  384,  756,  337,  861,  593,  276,  502,  843,  806,
 /*   470 */   634,  782,  150,  574,   84,  222,  729,  534,   64,  728,
 /*   480 */   475,  727,   91,  709,  693,  345,  125,  -41, 1101, 1089,
 /*   490 */  1059, 1029, 1019, 1017, 1010,  990,  983,  979,  963,  949,
 /*   500 */   936,  916,  908,  902,  895,  859,  859,  870,  868,  893,
 /*   510 */   766,  857,  551,  769,  723,  713,  824,  704,  551,  551,
 /*   520 */   701,  721,  631,  625,  551,  608,  544,  488,  495,  458,
 /*   530 */   497,  443,  378,  412,  385,  359,  258,  258,  239,  190,
 /*   540 */   165,   63,   63,  207,  142,  139,  134,   81,  124,   52,
 /*   550 */    17,   10,   -5,    2,
};
#define YY_REDUCE_USE_DFLT (-136)
#define YY_REDUCE_COUNT (312)
#define YY_REDUCE_MIN   (-135)
#define YY_REDUCE_MAX   (3275)
static const short yy_reduce_ofst[] = {
 /*     0 */  1012, -135,  279,  224, -120,  469,  392,  342, 1219, 1192,
 /*    10 */  1164, 1134, 1108, 1078, 1051, 1024,  996,  966,  940,  910,
 /*    20 */   883,  856,  828,  798,  772,  742,  715,  688,  660,  533,
 /*    30 */   -85, 1619, 1575, 1566, 1551, 1522, 1507, 1498, 1454, 1446,
 /*    40 */  1255,  596,  -16, 1418, 1392, 1367, 1339, 1313,  168, 1989,
 /*    50 */  1981, 1973, 1942, 1935, 1900, 1870, 1863, 1833, 1826, 2052,
 /*    60 */  2040, 2030, 2020,  107, 2723, 2713, 2701, 2691, 2675, 2665,
 /*    70 */  2655, 2645, 2635, 2625, 2612, 2602, 2592, 2582, 2569, 2559,
 /*    80 */  2547, 2537, 2521, 2511, 2501, 2491, 2481, 2471, 2458, 2448,
 /*    90 */  2438, 2428, 2415, 2405, 2393, 2383, 2367, 2357, 2347, 2337,
 /*   100 */  2327,  399, 2317, 2304, 2294, 2284, 2274, 2261, 2251, 2239,
 /*   110 */  2229, 2213, 2203, 2193, 2183, 2173, 2163, 2144, 2134, 2124,
 /*   120 */  2082, 1773, 1284, 2100, 2061, 1470,  215, 3265, 3256, 3246,
 /*   130 */  3239, 3229, 3220, 3203, 3193, 3181, 3167, 3157, 3145, 3131,
 /*   140 */  3121, 3109, 3095, 3085, 3073, 3049, 3037, 3030, 3023, 3013,
 /*   150 */  1716,  335, 3001, 2989, 2977, 2965, 2953, 2941, 2929, 2917,
 /*   160 */  2905, 2893, 2881, 2869, 2857, 2845, 2830, 2818, 2805, 2793,
 /*   170 */  2781, 2769, 2757, 2745, 2732, 1156,  561,   -6, -110, 1052,
 /*   180 */   921,  792,  716,  615,  159, 3275, 3268, 2782, 1887,  121,
 /*   190 */   562,  263,   93,  167,  116,  538,  400,   95,  714,  961,
 /*   200 */   882,  858,  831,  794,  790,  853,  784,  917,  223,  747,
 /*   210 */   669,  611, 1501, 1500, 1499, 1497, 1496, 1488, 1461, 1448,
 /*   220 */  1447, 1414, 1428, 1413, 1403, 1402, 1387, 1386, 1401, 1384,
 /*   230 */  1434, 1426, 1425, 1424, 1423, 1420, 1416, 1415, 1411, 1398,
 /*   240 */  1397, 1391, 1390, 1389, 1385, 1356, 1388, 1356, 1310, 1309,
 /*   250 */  1305, 1329, 1301, 1288, 1296, 1363, 1295, 1294, 1285, 1283,
 /*   260 */  1282, 1277, 1278, 1337, 1270, 1257, 1256, 1249, 1248, 1322,
 /*   270 */  1243, 1241, 1242, 1229, 1191, 1264, 1186, 1184, 1181, 1174,
 /*   280 */  1168, 1165, 1157, 1146, 1155, 1138, 1126, 1123, 1120, 1119,
 /*   290 */  1122, 1112, 1121, 1127, 1092, 1083, 1090, 1082, 1159, 1072,
 /*   300 */  1081, 1150, 1071, 1070, 1039, 1035, 1038, 1015, 1073, 1065,
 /*   310 */  1049, 1018, 1005,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   869, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*    10 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*    20 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*    30 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*    40 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*    50 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*    60 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1081, 1085,
 /*    70 */  1080, 1084, 1170, 1166, 1325, 1325, 1325, 1325, 1325, 1325,
 /*    80 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*    90 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1171,
 /*   100 */  1167, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   110 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   120 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1150, 1154, 1149,
 /*   130 */  1153, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   140 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   150 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   160 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   170 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   180 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   190 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   200 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   210 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   220 */  1325, 1273, 1271, 1273, 1273, 1273, 1279, 1279, 1271, 1271,
 /*   230 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   240 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1271, 1271,
 /*   250 */  1271, 1325, 1271, 1271, 1279, 1325, 1279, 1277, 1279, 1277,
 /*   260 */  1273, 1275, 1271, 1325, 1279, 1277, 1273, 1275, 1271, 1325,
 /*   270 */  1279, 1277, 1273, 1275, 1271, 1325, 1279, 1277, 1279, 1277,
 /*   280 */  1279, 1277, 1279, 1277, 1279, 1277, 1273, 1275, 1279, 1277,
 /*   290 */  1273, 1275, 1325, 1325, 1279, 1277, 1279, 1277, 1325, 1279,
 /*   300 */  1277, 1325, 1279, 1277, 1279, 1277, 1273, 1275, 1325, 1325,
 /*   310 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   320 */  1116, 1325, 1045, 1045, 1325, 1325,  936, 1325, 1325, 1325,
 /*   330 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   340 */  1325, 1325, 1325, 1263, 1260, 1325, 1152, 1156, 1151, 1155,
 /*   350 */  1147, 1146, 1145, 1144, 1143, 1142, 1141, 1134, 1325, 1325,
 /*   360 */  1325, 1325, 1325, 1325, 1325, 1278, 1272, 1274, 1270, 1325,
 /*   370 */  1325, 1325,  999, 1131, 1103,  998, 1059, 1071, 1070, 1069,
 /*   380 */  1068, 1067, 1066, 1065, 1051, 1083, 1087, 1082, 1086, 1016,
 /*   390 */  1172, 1168, 1047, 1044, 1043, 1042, 1041, 1040, 1039, 1038,
 /*   400 */  1037, 1036, 1035, 1034, 1033, 1032, 1325, 1325, 1173, 1169,
 /*   410 */  1074,  909,  910, 1031, 1030, 1029, 1028, 1027, 1026, 1025,
 /*   420 */   906,  905, 1164, 1133, 1132, 1120, 1119, 1104, 1105, 1004,
 /*   430 */  1005, 1325, 1325, 1325,  993,  994, 1061, 1060,  963,  962,
 /*   440 */  1018, 1017,  938,  937,  943,  942,  979,  980,  948,  947,
 /*   450 */   922,  923, 1325, 1325, 1000, 1325, 1325, 1237, 1241, 1240,
 /*   460 */  1238, 1325, 1325, 1325, 1325, 1325, 1325,  984, 1325, 1325,
 /*   470 */  1325, 1325, 1325, 1185, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   480 */   889, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   490 */  1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
 /*   500 */  1325, 1325, 1325, 1325, 1113, 1130, 1129, 1325, 1325, 1325,
 /*   510 */  1239, 1325, 1233, 1226, 1203, 1205, 1325, 1218, 1184, 1183,
 /*   520 */  1201, 1325, 1325, 1200, 1199, 1325, 1325, 1325, 1325, 1325,
 /*   530 */  1325, 1325, 1325, 1325, 1325, 1325, 1058, 1057, 1049, 1045,
 /*   540 */   893, 1015, 1014, 1325, 1325, 1325, 1325, 1072,  908,  904,
 /*   550 */   901,  897,  892, 1325, 1324, 1323, 1322, 1321, 1320, 1319,
 /*   560 */  1317, 1316, 1315, 1314, 1313, 1312, 1165, 1311, 1310, 1318,
 /*   570 */  1309, 1308, 1303, 1301, 1300, 1298, 1297, 1296, 1295, 1294,
 /*   580 */  1293, 1292, 1291, 1290, 1289, 1288, 1287, 1286, 1285, 1284,
 /*   590 */  1283, 1282, 1281, 1280, 1254, 1253, 1262, 1261, 1269, 1268,
 /*   600 */  1265, 1264, 1259, 1267, 1125, 1127, 1148, 1140, 1139, 1138,
 /*   610 */  1137, 1136, 1135, 1128, 1126, 1124, 1118, 1117, 1115, 1114,
 /*   620 */  1113, 1123, 1122, 1121, 1108, 1107, 1106, 1102, 1101, 1100,
 /*   630 */  1099, 1098, 1097, 1096, 1095, 1094, 1093, 1092, 1091, 1112,
 /*   640 */  1111, 1110, 1109, 1258, 1257, 1256, 1255, 1008, 1007, 1006,
 /*   650 */  1003, 1002, 1001, 1000, 1248, 1247, 1249, 1246, 1251, 1252,
 /*   660 */  1250, 1245, 1243, 1242, 1244, 1236, 1231, 1234, 1235, 1232,
 /*   670 */  1230, 1221, 1224, 1229, 1227, 1225, 1223, 1222, 1220, 1196,
 /*   680 */  1204, 1206, 1219, 1217, 1216, 1215, 1214, 1213, 1212, 1211,
 /*   690 */  1210, 1209, 1208, 1207, 1202, 1198, 1194, 1193, 1192, 1191,
 /*   700 */  1190, 1189, 1188,  897, 1187, 1186,  997,  996,  995,  992,
 /*   710 */   991,  990,  989,  988,  987,  986,  985,  984, 1197, 1195,
 /*   720 */  1175, 1178, 1179, 1182, 1181, 1180, 1177, 1176, 1174, 1307,
 /*   730 */  1306, 1305, 1304, 1302, 1299, 1053, 1055, 1064, 1063, 1062,
 /*   740 */  1056, 1054, 1052,  961,  960,  959,  958,  957,  956,  966,
 /*   750 */   965,  964,  955,  954,  953,  952, 1276, 1048, 1050,  894,
 /*   760 */  1010, 1012, 1076, 1079, 1078, 1077, 1075, 1024, 1023, 1022,
 /*   770 */  1021, 1020, 1019, 1013, 1011, 1009,  934, 1158, 1164, 1163,
 /*   780 */  1162, 1161, 1160, 1159, 1157, 1046,  941,  940,  939,  946,
 /*   790 */   945,  944,  936,  935,  934,  933,  932,  931, 1089, 1090,
 /*   800 */  1088, 1073,  981,  983,  982,  978,  977,  976,  975,  974,
 /*   810 */   973,  972,  971,  970,  969,  968,  967,  907,  951,  950,
 /*   820 */   949,  930,  929,  928,  927,  924,  926,  925,  921,  920,
 /*   830 */   919,  918,  917,  916,  915,  914,  913,  912,  911,  903,
 /*   840 */   902,  900,  899,  898,  896,  895,  891,  887,  886,  890,
 /*   850 */   889,  888,  885,  884,  883,  882,  881,  880,  879,  878,
 /*   860 */   877,  876,  875,  874,  873,  872,  871,  870,
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
  "law_rigid",     "law_observed",  "law_temporal_constraint",
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
 /*  34 */ "lua ::= AT_IDENTIFIER PAREN_L PAREN_R",
 /*  35 */ "undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R",
 /*  36 */ "undeclared ::= IDENTIFIER",
 /*  37 */ "term_lst ::= term",
 /*  38 */ "term_lst ::= term_lst COMMA term",
 /*  39 */ "constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R",
 /*  40 */ "constant_one_const ::= CONSTANT_ID",
 /*  41 */ "term_no_const_lst ::= term_no_const",
 /*  42 */ "term_no_const_lst ::= term_no_const_lst COMMA term_no_const",
 /*  43 */ "term ::= base_elem",
 /*  44 */ "term ::= INTEGER",
 /*  45 */ "term ::= STRING_LITERAL",
 /*  46 */ "term ::= PAREN_L term PAREN_R",
 /*  47 */ "term ::= TRUE",
 /*  48 */ "term ::= FALSE",
 /*  49 */ "term ::= MAXSTEP",
 /*  50 */ "term ::= MAXADDITIVE",
 /*  51 */ "term ::= MAXAFVALUE",
 /*  52 */ "term ::= DASH term",
 /*  53 */ "term ::= ABS term",
 /*  54 */ "term ::= term DASH term",
 /*  55 */ "term ::= term PLUS term",
 /*  56 */ "term ::= term STAR term",
 /*  57 */ "term ::= term INT_DIV term",
 /*  58 */ "term ::= term MOD term",
 /*  59 */ "term_strong ::= base_elem_no_const",
 /*  60 */ "term_strong ::= INTEGER",
 /*  61 */ "term_strong ::= STRING_LITERAL",
 /*  62 */ "term_strong ::= PAREN_L term_strong PAREN_R",
 /*  63 */ "term_strong ::= MAXSTEP",
 /*  64 */ "term_strong ::= MAXADDITIVE",
 /*  65 */ "term_strong ::= MAXAFVALUE",
 /*  66 */ "term_strong ::= DASH term_strong",
 /*  67 */ "term_strong ::= ABS term",
 /*  68 */ "term_strong_candidate ::= DASH constant",
 /*  69 */ "term_strong ::= term_strong_candidate DASH term",
 /*  70 */ "term_strong ::= term_strong_candidate PLUS term",
 /*  71 */ "term_strong ::= term_strong_candidate STAR term",
 /*  72 */ "term_strong ::= term_strong_candidate INT_DIV term",
 /*  73 */ "term_strong ::= term_strong_candidate MOD term",
 /*  74 */ "term_strong ::= constant DASH term",
 /*  75 */ "term_strong ::= constant PLUS term",
 /*  76 */ "term_strong ::= constant STAR term",
 /*  77 */ "term_strong ::= constant INT_DIV term",
 /*  78 */ "term_strong ::= constant MOD term",
 /*  79 */ "term_strong ::= term_strong DASH term",
 /*  80 */ "term_strong ::= term_strong PLUS term",
 /*  81 */ "term_strong ::= term_strong STAR term",
 /*  82 */ "term_strong ::= term_strong INT_DIV term",
 /*  83 */ "term_strong ::= term_strong MOD term",
 /*  84 */ "term_no_const_strong ::= base_elem_no_const",
 /*  85 */ "term_no_const_strong ::= INTEGER",
 /*  86 */ "term_no_const_strong ::= STRING_LITERAL",
 /*  87 */ "term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R",
 /*  88 */ "term_no_const_strong ::= MAXSTEP",
 /*  89 */ "term_no_const_strong ::= MAXADDITIVE",
 /*  90 */ "term_no_const_strong ::= MAXAFVALUE",
 /*  91 */ "term_no_const_strong ::= constant",
 /*  92 */ "term_no_const_strong ::= DASH term_no_const_strong",
 /*  93 */ "term_no_const_strong ::= ABS term_no_const",
 /*  94 */ "term_no_const_strong ::= term_no_const_strong DASH term_no_const",
 /*  95 */ "term_no_const_strong ::= term_no_const_strong PLUS term_no_const",
 /*  96 */ "term_no_const_strong ::= term_no_const_strong STAR term_no_const",
 /*  97 */ "term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const",
 /*  98 */ "term_no_const_strong ::= term_no_const_strong MOD term_no_const",
 /*  99 */ "term_no_const ::= base_elem_no_const",
 /* 100 */ "term_no_const ::= INTEGER",
 /* 101 */ "term_no_const ::= STRING_LITERAL",
 /* 102 */ "term_no_const ::= PAREN_L term_no_const PAREN_R",
 /* 103 */ "term_no_const ::= TRUE",
 /* 104 */ "term_no_const ::= FALSE",
 /* 105 */ "term_no_const ::= MAXSTEP",
 /* 106 */ "term_no_const ::= MAXADDITIVE",
 /* 107 */ "term_no_const ::= MAXAFVALUE",
 /* 108 */ "term_no_const ::= constant",
 /* 109 */ "term_no_const ::= DASH term_no_const",
 /* 110 */ "term_no_const ::= ABS term_no_const",
 /* 111 */ "term_no_const ::= term_no_const DASH term_no_const",
 /* 112 */ "term_no_const ::= term_no_const PLUS term_no_const",
 /* 113 */ "term_no_const ::= term_no_const STAR term_no_const",
 /* 114 */ "term_no_const ::= term_no_const INT_DIV term_no_const",
 /* 115 */ "term_no_const ::= term_no_const MOD term_no_const",
 /* 116 */ "term_integral ::= INTEGER",
 /* 117 */ "term_integral ::= PAREN_L term_integral PAREN_R",
 /* 118 */ "term_integral ::= TRUE",
 /* 119 */ "term_integral ::= FALSE",
 /* 120 */ "term_integral ::= MAXSTEP",
 /* 121 */ "term_integral ::= MAXADDITIVE",
 /* 122 */ "term_integral ::= MAXAFVALUE",
 /* 123 */ "term_integral ::= DASH term_integral",
 /* 124 */ "term_integral ::= ABS term_integral",
 /* 125 */ "term_integral ::= term_integral DASH term_integral",
 /* 126 */ "term_integral ::= term_integral PLUS term_integral",
 /* 127 */ "term_integral ::= term_integral STAR term_integral",
 /* 128 */ "term_integral ::= term_integral INT_DIV term_integral",
 /* 129 */ "term_integral ::= term_integral MOD term_integral",
 /* 130 */ "num_range ::= term_integral DBL_PERIOD term_integral",
 /* 131 */ "num_range_eval ::= term_int_eval DBL_PERIOD term_int_eval",
 /* 132 */ "term_int_eval ::= INTEGER",
 /* 133 */ "term_int_eval ::= PAREN_L term_int_eval PAREN_R",
 /* 134 */ "term_int_eval ::= DASH term_int_eval",
 /* 135 */ "term_int_eval ::= ABS term_int_eval",
 /* 136 */ "term_int_eval ::= term_int_eval DASH term_int_eval",
 /* 137 */ "term_int_eval ::= term_int_eval PLUS term_int_eval",
 /* 138 */ "term_int_eval ::= term_int_eval STAR term_int_eval",
 /* 139 */ "term_int_eval ::= term_int_eval INT_DIV term_int_eval",
 /* 140 */ "term_int_eval ::= term_int_eval MOD term_int_eval",
 /* 141 */ "formula ::= formula_base",
 /* 142 */ "formula ::= PAREN_L formula PAREN_R",
 /* 143 */ "formula ::= NOT formula",
 /* 144 */ "formula ::= DASH formula",
 /* 145 */ "formula ::= formula COMMA formula",
 /* 146 */ "formula ::= formula DBL_PLUS formula",
 /* 147 */ "formula ::= formula PIPE formula",
 /* 148 */ "formula ::= formula EQUIV formula",
 /* 149 */ "formula ::= formula IMPL formula",
 /* 150 */ "formula ::= formula ARROW_RDASH formula",
 /* 151 */ "formula_base ::= comparison",
 /* 152 */ "formula_base ::= atomic_formula",
 /* 153 */ "formula_base ::= formula_quant",
 /* 154 */ "formula_base ::= formula_card",
 /* 155 */ "formula_base ::= TRUE",
 /* 156 */ "formula_base ::= FALSE",
 /* 157 */ "comparison ::= term_strong EQ term",
 /* 158 */ "comparison ::= term_strong DBL_EQ term",
 /* 159 */ "comparison ::= term_strong NEQ term",
 /* 160 */ "comparison ::= term_strong LTHAN term",
 /* 161 */ "comparison ::= term_strong GTHAN term",
 /* 162 */ "comparison ::= term_strong LTHAN_EQ term",
 /* 163 */ "comparison ::= term_strong GTHAN_EQ term",
 /* 164 */ "comparison ::= term_strong_candidate EQ term",
 /* 165 */ "comparison ::= term_strong_candidate DBL_EQ term",
 /* 166 */ "comparison ::= term_strong_candidate NEQ term",
 /* 167 */ "comparison ::= term_strong_candidate LTHAN term",
 /* 168 */ "comparison ::= term_strong_candidate GTHAN term",
 /* 169 */ "comparison ::= term_strong_candidate LTHAN_EQ term",
 /* 170 */ "comparison ::= term_strong_candidate GTHAN_EQ term",
 /* 171 */ "comparison ::= constant DBL_EQ term",
 /* 172 */ "comparison ::= constant NEQ term",
 /* 173 */ "comparison ::= constant LTHAN term",
 /* 174 */ "comparison ::= constant GTHAN term",
 /* 175 */ "comparison ::= constant LTHAN_EQ term",
 /* 176 */ "comparison ::= constant GTHAN_EQ term",
 /* 177 */ "atomic_formula ::= constant",
 /* 178 */ "atomic_formula ::= TILDE constant",
 /* 179 */ "atomic_formula ::= constant EQ term",
 /* 180 */ "atomic_formula_anon ::= atomic_formula",
 /* 181 */ "atomic_formula_anon ::= const_anon",
 /* 182 */ "atomic_formula_anon ::= TILDE const_anon",
 /* 183 */ "atomic_formula_anon ::= const_anon EQ term",
 /* 184 */ "formula_no_const ::= formula_no_const_base",
 /* 185 */ "formula_no_const ::= PAREN_L formula_no_const PAREN_R",
 /* 186 */ "formula_no_const ::= NOT formula_no_const",
 /* 187 */ "formula_no_const ::= DASH formula_no_const",
 /* 188 */ "formula_no_const ::= formula_no_const COMMA formula_no_const",
 /* 189 */ "formula_no_const ::= formula_no_const DBL_PLUS formula_no_const",
 /* 190 */ "formula_no_const ::= formula_no_const PIPE formula_no_const",
 /* 191 */ "formula_no_const ::= formula_no_const EQUIV formula_no_const",
 /* 192 */ "formula_no_const ::= formula_no_const IMPL formula_no_const",
 /* 193 */ "formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const",
 /* 194 */ "formula_no_const_base ::= comparison_no_const",
 /* 195 */ "formula_no_const_base ::= TRUE",
 /* 196 */ "formula_no_const_base ::= FALSE",
 /* 197 */ "comparison_no_const ::= term_no_const_strong EQ term_no_const",
 /* 198 */ "comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const",
 /* 199 */ "comparison_no_const ::= term_no_const_strong NEQ term_no_const",
 /* 200 */ "comparison_no_const ::= term_no_const_strong LTHAN term_no_const",
 /* 201 */ "comparison_no_const ::= term_no_const_strong GTHAN term_no_const",
 /* 202 */ "comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const",
 /* 203 */ "comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const",
 /* 204 */ "atomic_formula_one_const ::= constant_one_const",
 /* 205 */ "atomic_formula_one_const ::= TILDE constant_one_const",
 /* 206 */ "atomic_formula_one_const ::= constant_one_const EQ term_no_const",
 /* 207 */ "formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R",
 /* 208 */ "quant_lst ::= quant_op variable",
 /* 209 */ "quant_lst ::= quant_lst quant_op variable",
 /* 210 */ "quant_op ::= BIG_CONJ",
 /* 211 */ "quant_op ::= BIG_DISJ",
 /* 212 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 213 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 214 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 215 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 216 */ "formula_card ::= CBRACKET_L formula CBRACKET_R",
 /* 217 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R",
 /* 218 */ "formula_card ::= CBRACKET_L formula CBRACKET_R term",
 /* 219 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term",
 /* 220 */ "card_var_lst ::= card_var_lst_inner PIPE",
 /* 221 */ "card_var_lst_inner ::= variable",
 /* 222 */ "card_var_lst_inner ::= card_var_lst_inner COMMA variable",
 /* 223 */ "term_temporal ::= base_elem_no_const",
 /* 224 */ "term_temporal ::= INTEGER",
 /* 225 */ "term_temporal ::= STRING_LITERAL",
 /* 226 */ "term_temporal ::= PAREN_L term_temporal PAREN_R",
 /* 227 */ "term_temporal ::= TRUE",
 /* 228 */ "term_temporal ::= FALSE",
 /* 229 */ "term_temporal ::= MAXSTEP",
 /* 230 */ "term_temporal ::= MAXADDITIVE",
 /* 231 */ "term_temporal ::= MAXAFVALUE",
 /* 232 */ "term_temporal ::= constant",
 /* 233 */ "term_temporal ::= DASH term_temporal",
 /* 234 */ "term_temporal ::= ABS term_temporal",
 /* 235 */ "term_temporal ::= term_temporal COLON term",
 /* 236 */ "term_temporal ::= term_temporal DASH term_temporal",
 /* 237 */ "term_temporal ::= term_temporal PLUS term_temporal",
 /* 238 */ "term_temporal ::= term_temporal STAR term_temporal",
 /* 239 */ "term_temporal ::= term_temporal INT_DIV term_temporal",
 /* 240 */ "term_temporal ::= term_temporal MOD term_temporal",
 /* 241 */ "term_temporal_strong ::= base_elem_no_const",
 /* 242 */ "term_temporal_strong ::= INTEGER",
 /* 243 */ "term_temporal_strong ::= STRING_LITERAL",
 /* 244 */ "term_temporal_strong ::= PAREN_L term_temporal_strong PAREN_R",
 /* 245 */ "term_temporal_strong ::= MAXSTEP",
 /* 246 */ "term_temporal_strong ::= MAXADDITIVE",
 /* 247 */ "term_temporal_strong ::= MAXAFVALUE",
 /* 248 */ "term_temporal_strong ::= term_temporal_strong COLON term_strong",
 /* 249 */ "term_temporal_strong ::= DASH term_temporal_strong",
 /* 250 */ "term_temporal_strong ::= ABS term_temporal",
 /* 251 */ "term_temporal_strong ::= term_temporal_strong DASH term_temporal",
 /* 252 */ "term_temporal_strong ::= term_temporal_strong PLUS term_temporal",
 /* 253 */ "term_temporal_strong ::= term_temporal_strong STAR term_temporal",
 /* 254 */ "term_temporal_strong ::= term_temporal_strong INT_DIV term_temporal",
 /* 255 */ "term_temporal_strong ::= term_temporal_strong MOD term_temporal",
 /* 256 */ "formula_temporal ::= formula_temporal_base",
 /* 257 */ "formula_temporal ::= PAREN_L formula_temporal PAREN_R",
 /* 258 */ "formula_temporal ::= NOT formula_temporal",
 /* 259 */ "formula_temporal ::= DASH formula_temporal",
 /* 260 */ "formula_temporal ::= formula_temporal COMMA formula_temporal",
 /* 261 */ "formula_temporal ::= formula_temporal DBL_PLUS formula_temporal",
 /* 262 */ "formula_temporal ::= formula_temporal PIPE formula_temporal",
 /* 263 */ "formula_temporal ::= formula_temporal EQUIV formula_temporal",
 /* 264 */ "formula_temporal ::= formula_temporal IMPL formula_temporal",
 /* 265 */ "formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal",
 /* 266 */ "formula_temporal ::= term_temporal_strong COLON formula",
 /* 267 */ "formula_temporal_base ::= comparison_temporal",
 /* 268 */ "formula_temporal_base ::= atomic_formula",
 /* 269 */ "formula_temporal_base ::= formula_temporal_quant",
 /* 270 */ "formula_temporal_base ::= formula_temporal_card",
 /* 271 */ "formula_temporal_base ::= TRUE",
 /* 272 */ "formula_temporal_base ::= FALSE",
 /* 273 */ "comparison_temporal ::= term_temporal_strong EQ term_temporal",
 /* 274 */ "comparison_temporal ::= term_temporal_strong DBL_EQ term_temporal",
 /* 275 */ "comparison_temporal ::= term_temporal_strong NEQ term_temporal",
 /* 276 */ "comparison_temporal ::= term_temporal_strong LTHAN term_temporal",
 /* 277 */ "comparison_temporal ::= term_temporal_strong GTHAN term_temporal",
 /* 278 */ "comparison_temporal ::= term_temporal_strong LTHAN_EQ term_temporal",
 /* 279 */ "comparison_temporal ::= term_temporal_strong GTHAN_EQ term_temporal",
 /* 280 */ "formula_temporal_quant ::= BRACKET_L quant_lst PIPE formula_temporal BRACKET_R",
 /* 281 */ "formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R",
 /* 282 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R",
 /* 283 */ "formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal",
 /* 284 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal",
 /* 285 */ "formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R",
 /* 286 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R",
 /* 287 */ "formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R term_temporal",
 /* 288 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R term_temporal",
 /* 289 */ "head_formula ::= head_formula COMMA head_formula",
 /* 290 */ "head_formula ::= PAREN_L head_formula PAREN_R",
 /* 291 */ "head_formula ::= comparison",
 /* 292 */ "head_formula ::= atomic_head_formula",
 /* 293 */ "head_formula ::= formula_smpl_card",
 /* 294 */ "head_formula ::= TRUE",
 /* 295 */ "head_formula ::= FALSE",
 /* 296 */ "atomic_head_formula ::= atomic_formula",
 /* 297 */ "atomic_head_formula ::= DASH constant",
 /* 298 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 299 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 300 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 301 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 302 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 303 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 304 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 305 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 306 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 307 */ "macro_def_lst ::= macro_bnd",
 /* 308 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 309 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 310 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 311 */ "macro_args ::= macro_arg",
 /* 312 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 313 */ "macro_arg ::= POUND_INTEGER",
 /* 314 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 315 */ "sort_lst ::= sort",
 /* 316 */ "sort_lst ::= sort_lst COMMA sort",
 /* 317 */ "sort ::= sort_id_nr",
 /* 318 */ "sort ::= sort_id_nr STAR",
 /* 319 */ "sort ::= sort_id_nr CARROT",
 /* 320 */ "sort ::= sort PLUS object_nullary",
 /* 321 */ "sort ::= sort PLUS IDENTIFIER",
 /* 322 */ "sort ::= sort PLUS INTEGER",
 /* 323 */ "sort_id_nr ::= sort_id",
 /* 324 */ "sort_id_nr ::= sort_nr",
 /* 325 */ "sort_nr ::= num_range",
 /* 326 */ "sort_id ::= IDENTIFIER",
 /* 327 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 328 */ "constant_bnd_lst ::= constant_bnd",
 /* 329 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 330 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 331 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 332 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 333 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 334 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 335 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 336 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 337 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 338 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 339 */ "constant_dcl_type ::= ABACTION",
 /* 340 */ "constant_dcl_type ::= ACTION",
 /* 341 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 342 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 343 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 344 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 345 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 346 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 347 */ "constant_dcl_type ::= RIGID",
 /* 348 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 349 */ "constant_dcl_type ::= SDFLUENT",
 /* 350 */ "attrib_spec ::= ATTRIBUTE",
 /* 351 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 352 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 353 */ "object_bnd_lst ::= object_bnd",
 /* 354 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 355 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 356 */ "object_lst ::= object_spec",
 /* 357 */ "object_lst ::= object_lst COMMA object_spec",
 /* 358 */ "object_spec ::= IDENTIFIER",
 /* 359 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 360 */ "object_spec ::= INTEGER",
 /* 361 */ "object_spec ::= num_range",
 /* 362 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 363 */ "variable_bnd_lst ::= variable_bnd",
 /* 364 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 365 */ "variable_bnd ::= variable_lst DBL_COLON sort",
 /* 366 */ "variable_lst ::= IDENTIFIER",
 /* 367 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 368 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 369 */ "sort_bnd_lst ::= sort_bnd",
 /* 370 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 371 */ "sort_bnd ::= sort_dcl_lst",
 /* 372 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 373 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 374 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 375 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 376 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 377 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 378 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 379 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 380 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 381 */ "show_lst ::= show_elem",
 /* 382 */ "show_lst ::= show_lst COMMA show_elem",
 /* 383 */ "show_lst ::= show_lst SEMICOLON show_elem",
 /* 384 */ "show_elem ::= atomic_formula_one_const",
 /* 385 */ "stmt_noconcurrency ::= NOCONCURRENCY PERIOD",
 /* 386 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD",
 /* 387 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_int_eval PERIOD",
 /* 388 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE DBL_COLON term_int_eval PERIOD",
 /* 389 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_int_eval PERIOD",
 /* 390 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE DBL_COLON term_int_eval PERIOD",
 /* 391 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 392 */ "query_lst ::= formula_temporal",
 /* 393 */ "query_lst ::= query_maxstep_decl",
 /* 394 */ "query_lst ::= query_label_decl",
 /* 395 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 396 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 397 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 398 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 399 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range_eval",
 /* 400 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 401 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 402 */ "clause_if ::= IF formula",
 /* 403 */ "clause_if ::=",
 /* 404 */ "clause_after ::= AFTER formula",
 /* 405 */ "clause_after ::=",
 /* 406 */ "clause_ifcons ::= IFCONS formula",
 /* 407 */ "clause_ifcons ::=",
 /* 408 */ "clause_unless ::= UNLESS atomic_formula_anon",
 /* 409 */ "clause_unless ::=",
 /* 410 */ "clause_where ::= WHERE formula_no_const",
 /* 411 */ "clause_where ::=",
 /* 412 */ "stmt_law ::= law_basic",
 /* 413 */ "stmt_law ::= law_caused",
 /* 414 */ "stmt_law ::= law_pcaused",
 /* 415 */ "stmt_law ::= law_impl",
 /* 416 */ "stmt_law ::= law_causes",
 /* 417 */ "stmt_law ::= law_increments",
 /* 418 */ "stmt_law ::= law_decrements",
 /* 419 */ "stmt_law ::= law_mcause",
 /* 420 */ "stmt_law ::= law_always",
 /* 421 */ "stmt_law ::= law_constraint",
 /* 422 */ "stmt_law ::= law_impossible",
 /* 423 */ "stmt_law ::= law_never",
 /* 424 */ "stmt_law ::= law_default",
 /* 425 */ "stmt_law ::= law_exogenous",
 /* 426 */ "stmt_law ::= law_inertial",
 /* 427 */ "stmt_law ::= law_nonexecutable",
 /* 428 */ "stmt_law ::= law_rigid",
 /* 429 */ "stmt_law ::= law_observed",
 /* 430 */ "stmt_law ::= law_temporal_constraint",
 /* 431 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 432 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 433 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 434 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 435 */ "law_impl ::= ARROW_LDASH formula clause_where PERIOD",
 /* 436 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 437 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 438 */ "law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 439 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 440 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 441 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 442 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 443 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 444 */ "law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 445 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 446 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 447 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 448 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 449 */ "law_observed ::= OBSERVED atomic_head_formula AT term_int_eval PERIOD",
 /* 450 */ "law_temporal_constraint ::= CONSTRAINT formula AT term_int_eval PERIOD",
 /* 451 */ "stmt_code_blk ::= ASP_GR",
 /* 452 */ "stmt_code_blk ::= ASP_CP",
 /* 453 */ "stmt_code_blk ::= F2LP_GR",
 /* 454 */ "stmt_code_blk ::= F2LP_CP",
 /* 455 */ "stmt_code_blk ::= LUA_GR",
 /* 456 */ "stmt_code_blk ::= LUA_CP",
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
#line 2595 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 136: /* start */
    case 137: /* statement_lst */
    case 160: /* undeclared */
{
#line 209 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2604 "bcplus/parser/detail/lemon_parser.c"
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
 DEALLOC((yypminor->yy10));								
#line 2617 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 139: /* stmt_macro_def */
{
#line 234 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy291));								
#line 2624 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_constant_def */
{
#line 236 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy355));								
#line 2631 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_object_def */
{
#line 238 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy288));								
#line 2638 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_variable_def */
{
#line 240 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy267));								
#line 2645 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_sort_def */
{
#line 242 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy225));								
#line 2652 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 148: /* stmt_noconcurrency */
{
#line 252 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy268));								
#line 2659 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_strong_noconcurrency */
{
#line 254 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy418));								
#line 2666 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 152: /* stmt_query */
{
#line 260 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy320));								
#line 2673 "bcplus/parser/detail/lemon_parser.c"
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
 DEALLOC((yypminor->yy217));								
#line 2691 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 155: /* constant */
    case 163: /* constant_one_const */
    case 166: /* const_anon */
{
#line 298 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy427));								
#line 2700 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* object */
    case 157: /* object_nullary */
{
#line 300 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy138));								
#line 2708 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 158: /* variable */
{
#line 304 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy284));								
#line 2715 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* lua */
{
#line 306 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy463));								
#line 2722 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 161: /* term_lst */
    case 164: /* term_no_const_lst */
{
#line 310 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy317));								
#line 2730 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 170: /* num_range */
{
#line 713 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy171));								
#line 2737 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* num_range_eval */
{
#line 715 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy259));								
#line 2744 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 173: /* term_int_eval */
{
#line 719 "bcplus/parser/detail/lemon_parser.y"
 /* Initially left Blank */				
#line 2751 "bcplus/parser/detail/lemon_parser.c"
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
#line 820 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy93));								
#line 2769 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 177: /* atomic_formula */
    case 180: /* atomic_formula_anon */
    case 184: /* atomic_formula_one_const */
    case 199: /* atomic_head_formula */
{
#line 826 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy52));								
#line 2779 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 178: /* formula_quant */
    case 196: /* formula_temporal_quant */
{
#line 828 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy101));								
#line 2787 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_lst */
{
#line 1007 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy347));								
#line 2794 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* quant_op */
{
#line 1009 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2801 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 187: /* card_var_lst */
    case 188: /* card_var_lst_inner */
{
#line 1046 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy145));								
#line 2809 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* formula_smpl_card */
{
#line 1353 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy471));								
#line 2816 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* macro_def_lst */
{
#line 1417 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy19));                              
#line 2823 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* macro_bnd */
{
#line 1419 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy313));                              
#line 2830 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* macro_args */
{
#line 1421 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy440));                              
#line 2837 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* macro_arg */
{
#line 1423 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy143));                              
#line 2844 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* sort_lst */
{
#line 1513 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy405));							
#line 2851 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 206: /* sort */
    case 207: /* sort_id_nr */
    case 208: /* sort_nr */
    case 209: /* sort_id */
{
#line 1515 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2861 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* constant_bnd_lst */
    case 211: /* constant_bnd */
{
#line 1624 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy295));									
#line 2869 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* constant_dcl_lst */
{
#line 1628 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy444));									
#line 2876 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* constant_dcl_type */
{
#line 1630 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2883 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* attrib_spec */
{
#line 1632 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2890 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* object_bnd_lst */
{
#line 1991 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy214));									
#line 2897 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* object_bnd */
{
#line 1993 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy380));									
#line 2904 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* object_lst */
    case 218: /* object_spec */
{
#line 1995 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy387));									
#line 2912 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* variable_bnd_lst */
    case 220: /* variable_bnd */
{
#line 2127 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy377));									
#line 2920 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 221: /* variable_lst */
{
#line 2131 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy298));									
#line 2927 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 222: /* sort_bnd_lst */
    case 223: /* sort_bnd */
    case 224: /* sort_dcl_lst */
{
#line 2214 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy445));									
#line 2936 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 225: /* show_lst */
{
#line 2318 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy65));									
#line 2943 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 226: /* show_elem */
    case 234: /* clause_unless */
{
#line 2320 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy52));									
#line 2951 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 227: /* query_lst */
{
#line 2472 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy271).l); DEALLOC((yypminor->yy271).maxstep); DEALLOC((yypminor->yy271).label);	
#line 2958 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 228: /* query_maxstep_decl */
{
#line 2474 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy259));												
#line 2965 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 230: /* query_label_Decl */
{
#line 2476 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2972 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 231: /* clause_if */
    case 232: /* clause_after */
    case 233: /* clause_ifcons */
    case 235: /* clause_where */
{
#line 2630 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy93));									
#line 2982 "bcplus/parser/detail/lemon_parser.c"
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
    case 254: /* law_temporal_constraint */
{
#line 2671 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy10));									
#line 3007 "bcplus/parser/detail/lemon_parser.c"
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
  { 159, 3 },
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
  { 254, 5 },
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
#line 3769 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 220 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy10;
			yymsp[0].minor.yy10  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3778 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy10 = yymsp[0].minor.yy291; }
#line 3783 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy10 = yymsp[0].minor.yy355; }
#line 3788 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy10 = yymsp[0].minor.yy288; }
#line 3793 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy10 = yymsp[0].minor.yy267; }
#line 3798 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy10 = yymsp[0].minor.yy225; }
#line 3803 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 268 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy10 = yymsp[0].minor.yy10; }
#line 3813 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy10 = yymsp[0].minor.yy268; }
#line 3818 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 273 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy10 = yymsp[0].minor.yy418; }
#line 3823 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 276 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy10 = yymsp[0].minor.yy320; }
#line 3828 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy217 = yymsp[0].minor.yy427; }
#line 3833 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 43: /* term ::= base_elem */ yytestcase(yyruleno==43);
      case 59: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==99);
      case 223: /* term_temporal ::= base_elem_no_const */ yytestcase(yyruleno==223);
      case 241: /* term_temporal_strong ::= base_elem_no_const */ yytestcase(yyruleno==241);
#line 323 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy217 = yymsp[0].minor.yy217; }
#line 3844 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 325 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy217 = yymsp[0].minor.yy138;	}
#line 3849 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 326 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy217 = yymsp[0].minor.yy284; }
#line 3854 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 327 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy217 = yymsp[0].minor.yy463; }
#line 3859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 39: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==39);
#line 445 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy427, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy317, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3865 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 40: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==40);
#line 446 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy427, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3871 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* const_anon ::= IDENTIFIER */
#line 448 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy427, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true); }
#line 3876 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 449 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy427, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy317, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true);	}
#line 3881 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 452 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy138, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy317, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3886 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= object_nullary */
#line 453 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy138 = yymsp[0].minor.yy138; }
#line 3891 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* object_nullary ::= OBJECT_ID */
#line 454 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy138, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3896 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* object ::= undeclared */
#line 455 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3901 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* variable ::= VARIABLE_ID */
#line 458 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy284 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::VARIABLE)) {
			parser->_feature_error(Language::Feature::VARIABLE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			BASE_ELEM_BARE_DEF(yygotominor.yy284, yymsp[0].minor.yy0, Symbol::Type::VARIABLE, Variable, VariableSymbol); 
		}
	}
#line 3916 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 469 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy463, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy317, yymsp[0].minor.yy0); }
#line 3921 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* lua ::= AT_IDENTIFIER */
#line 470 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy463, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3926 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* lua ::= AT_IDENTIFIER PAREN_L PAREN_R */
#line 471 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy463, yymsp[-2].minor.yy0, yymsp[-1].minor.yy0, NULL, yymsp[0].minor.yy0); }
#line 3931 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 472 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy283, yymsp[-3].minor.yy0, yymsp[-1].minor.yy317);   yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 3938 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 36: /* undeclared ::= IDENTIFIER */
#line 473 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy283, yymsp[0].minor.yy0, NULL); }
#line 3943 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 37: /* term_lst ::= term */
      case 41: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==41);
#line 476 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy317 = new TermList();
			yygotominor.yy317->push_back(yymsp[0].minor.yy217);
		}
#line 3952 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 38: /* term_lst ::= term_lst COMMA term */
      case 42: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==42);
#line 482 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy317 = yymsp[-2].minor.yy317;
			yymsp[-2].minor.yy317->push_back(yymsp[0].minor.yy217);
		  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3962 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 44: /* term ::= INTEGER */
      case 60: /* term_strong ::= INTEGER */ yytestcase(yyruleno==60);
      case 85: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==85);
      case 100: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==100);
      case 116: /* term_integral ::= INTEGER */ yytestcase(yyruleno==116);
      case 224: /* term_temporal ::= INTEGER */ yytestcase(yyruleno==224);
      case 242: /* term_temporal_strong ::= INTEGER */ yytestcase(yyruleno==242);
#line 581 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy217, yymsp[0].minor.yy0);	}
#line 3973 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 45: /* term ::= STRING_LITERAL */
      case 47: /* term ::= TRUE */ yytestcase(yyruleno==47);
      case 48: /* term ::= FALSE */ yytestcase(yyruleno==48);
      case 61: /* term_strong ::= STRING_LITERAL */ yytestcase(yyruleno==61);
      case 86: /* term_no_const_strong ::= STRING_LITERAL */ yytestcase(yyruleno==86);
      case 101: /* term_no_const ::= STRING_LITERAL */ yytestcase(yyruleno==101);
      case 103: /* term_no_const ::= TRUE */ yytestcase(yyruleno==103);
      case 104: /* term_no_const ::= FALSE */ yytestcase(yyruleno==104);
      case 118: /* term_integral ::= TRUE */ yytestcase(yyruleno==118);
      case 119: /* term_integral ::= FALSE */ yytestcase(yyruleno==119);
      case 225: /* term_temporal ::= STRING_LITERAL */ yytestcase(yyruleno==225);
      case 227: /* term_temporal ::= TRUE */ yytestcase(yyruleno==227);
      case 228: /* term_temporal ::= FALSE */ yytestcase(yyruleno==228);
      case 243: /* term_temporal_strong ::= STRING_LITERAL */ yytestcase(yyruleno==243);
#line 582 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy217, yymsp[0].minor.yy0); }
#line 3991 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 46: /* term ::= PAREN_L term PAREN_R */
      case 62: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==87);
      case 102: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==102);
      case 117: /* term_integral ::= PAREN_L term_integral PAREN_R */ yytestcase(yyruleno==117);
      case 226: /* term_temporal ::= PAREN_L term_temporal PAREN_R */ yytestcase(yyruleno==226);
      case 244: /* term_temporal_strong ::= PAREN_L term_temporal_strong PAREN_R */ yytestcase(yyruleno==244);
#line 583 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy217, yymsp[-2].minor.yy0, yymsp[-1].minor.yy217, yymsp[0].minor.yy0); }
#line 4002 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= MAXSTEP */
      case 63: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==63);
      case 88: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==88);
      case 105: /* term_no_const ::= MAXSTEP */ yytestcase(yyruleno==105);
      case 120: /* term_integral ::= MAXSTEP */ yytestcase(yyruleno==120);
      case 229: /* term_temporal ::= MAXSTEP */ yytestcase(yyruleno==229);
      case 245: /* term_temporal_strong ::= MAXSTEP */ yytestcase(yyruleno==245);
#line 586 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy217, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 4013 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= MAXADDITIVE */
      case 64: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==64);
      case 89: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==89);
      case 106: /* term_no_const ::= MAXADDITIVE */ yytestcase(yyruleno==106);
      case 121: /* term_integral ::= MAXADDITIVE */ yytestcase(yyruleno==121);
      case 230: /* term_temporal ::= MAXADDITIVE */ yytestcase(yyruleno==230);
      case 246: /* term_temporal_strong ::= MAXADDITIVE */ yytestcase(yyruleno==246);
#line 587 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy217, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 4024 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= MAXAFVALUE */
      case 65: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==65);
      case 90: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==90);
      case 107: /* term_no_const ::= MAXAFVALUE */ yytestcase(yyruleno==107);
      case 122: /* term_integral ::= MAXAFVALUE */ yytestcase(yyruleno==122);
      case 231: /* term_temporal ::= MAXAFVALUE */ yytestcase(yyruleno==231);
      case 247: /* term_temporal_strong ::= MAXAFVALUE */ yytestcase(yyruleno==247);
#line 588 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy217, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 4035 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= DASH term */
      case 66: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==66);
      case 92: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==92);
      case 109: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==109);
      case 123: /* term_integral ::= DASH term_integral */ yytestcase(yyruleno==123);
      case 233: /* term_temporal ::= DASH term_temporal */ yytestcase(yyruleno==233);
      case 249: /* term_temporal_strong ::= DASH term_temporal_strong */ yytestcase(yyruleno==249);
#line 592 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy217, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, UnaryTerm::Operator::NEGATIVE); }
#line 4046 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= ABS term */
      case 67: /* term_strong ::= ABS term */ yytestcase(yyruleno==67);
      case 93: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==93);
      case 110: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==110);
      case 124: /* term_integral ::= ABS term_integral */ yytestcase(yyruleno==124);
      case 234: /* term_temporal ::= ABS term_temporal */ yytestcase(yyruleno==234);
      case 250: /* term_temporal_strong ::= ABS term_temporal */ yytestcase(yyruleno==250);
#line 593 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy217, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, UnaryTerm::Operator::ABS); }
#line 4057 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= term DASH term */
      case 69: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==69);
      case 79: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==79);
      case 94: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==94);
      case 111: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==111);
      case 125: /* term_integral ::= term_integral DASH term_integral */ yytestcase(yyruleno==125);
      case 236: /* term_temporal ::= term_temporal DASH term_temporal */ yytestcase(yyruleno==236);
      case 251: /* term_temporal_strong ::= term_temporal_strong DASH term_temporal */ yytestcase(yyruleno==251);
#line 597 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy217, yymsp[-2].minor.yy217, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BinaryTerm::Operator::MINUS); }
#line 4069 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= term PLUS term */
      case 70: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==70);
      case 80: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==80);
      case 95: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==95);
      case 112: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==112);
      case 126: /* term_integral ::= term_integral PLUS term_integral */ yytestcase(yyruleno==126);
      case 237: /* term_temporal ::= term_temporal PLUS term_temporal */ yytestcase(yyruleno==237);
      case 252: /* term_temporal_strong ::= term_temporal_strong PLUS term_temporal */ yytestcase(yyruleno==252);
#line 598 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy217, yymsp[-2].minor.yy217, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BinaryTerm::Operator::PLUS); }
#line 4081 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 56: /* term ::= term STAR term */
      case 71: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==71);
      case 81: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==81);
      case 96: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==96);
      case 113: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==113);
      case 127: /* term_integral ::= term_integral STAR term_integral */ yytestcase(yyruleno==127);
      case 238: /* term_temporal ::= term_temporal STAR term_temporal */ yytestcase(yyruleno==238);
      case 253: /* term_temporal_strong ::= term_temporal_strong STAR term_temporal */ yytestcase(yyruleno==253);
#line 599 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy217, yymsp[-2].minor.yy217, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BinaryTerm::Operator::TIMES); }
#line 4093 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 57: /* term ::= term INT_DIV term */
      case 72: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==72);
      case 82: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==82);
      case 97: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==97);
      case 114: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==114);
      case 128: /* term_integral ::= term_integral INT_DIV term_integral */ yytestcase(yyruleno==128);
      case 239: /* term_temporal ::= term_temporal INT_DIV term_temporal */ yytestcase(yyruleno==239);
      case 254: /* term_temporal_strong ::= term_temporal_strong INT_DIV term_temporal */ yytestcase(yyruleno==254);
#line 600 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy217, yymsp[-2].minor.yy217, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BinaryTerm::Operator::DIVIDE); }
#line 4105 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 58: /* term ::= term MOD term */
      case 73: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==73);
      case 83: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==83);
      case 98: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==98);
      case 115: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==115);
      case 129: /* term_integral ::= term_integral MOD term_integral */ yytestcase(yyruleno==129);
      case 240: /* term_temporal ::= term_temporal MOD term_temporal */ yytestcase(yyruleno==240);
      case 255: /* term_temporal_strong ::= term_temporal_strong MOD term_temporal */ yytestcase(yyruleno==255);
#line 601 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy217, yymsp[-2].minor.yy217, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BinaryTerm::Operator::MOD); }
#line 4117 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 68: /* term_strong_candidate ::= DASH constant */
#line 620 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy217, yymsp[-1].minor.yy0, yymsp[0].minor.yy427, UnaryTerm::Operator::NEGATIVE); }
#line 4122 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant DASH term */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy217, yymsp[-2].minor.yy427, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BinaryTerm::Operator::MINUS); }
#line 4127 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant PLUS term */
#line 630 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy217, yymsp[-2].minor.yy427, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BinaryTerm::Operator::PLUS); }
#line 4132 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 76: /* term_strong ::= constant STAR term */
#line 631 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy217, yymsp[-2].minor.yy427, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BinaryTerm::Operator::TIMES); }
#line 4137 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong ::= constant INT_DIV term */
#line 632 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy217, yymsp[-2].minor.yy427, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BinaryTerm::Operator::DIVIDE); }
#line 4142 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 78: /* term_strong ::= constant MOD term */
#line 633 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy217, yymsp[-2].minor.yy427, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BinaryTerm::Operator::MOD); }
#line 4147 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 91: /* term_no_const_strong ::= constant */
#line 655 "bcplus/parser/detail/lemon_parser.y"
{
		// error handling for constants so they don'yygotominor.yy217 default to undeclared identifiers
		yygotominor.yy217 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy427;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy427->beginLoc());
		YYERROR;
	}
#line 4158 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 108: /* term_no_const ::= constant */
#line 688 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy217 default to undeclared identifiers
		yygotominor.yy217 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy427;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy427->beginLoc());
		YYERROR;
	}
#line 4169 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* num_range ::= term_integral DBL_PERIOD term_integral */
#line 745 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy217, r_ptr = yymsp[0].minor.yy217, s_ptr = yymsp[-1].minor.yy0;
	yygotominor.yy171 = NULL;

	if (yymsp[-2].minor.yy217->domainType() != DomainType::INTEGRAL) {
		parser->_parse_error("Number ranges cannot have non-numeric operands.", &yymsp[-1].minor.yy0->beginLoc());
		YYERROR;
	}
	
	if (yymsp[0].minor.yy217->domainType() != DomainType::INTEGRAL) {
		parser->_parse_error("Number ranges cannot have non-numeric operands.", &yymsp[0].minor.yy217->beginLoc());
		YYERROR;
	}

	yygotominor.yy171 = new NumberRange(yymsp[-2].minor.yy217, yymsp[0].minor.yy217, yymsp[-2].minor.yy217->beginLoc(), yymsp[0].minor.yy217->endLoc());
}
#line 4189 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 131: /* num_range_eval ::= term_int_eval DBL_PERIOD term_int_eval */
#line 763 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy458, r_ptr = yymsp[0].minor.yy458, s_ptr = yymsp[-1].minor.yy0;
	yygotominor.yy259 = new NumberRangeEval(yymsp[-2].minor.yy458->val(), yymsp[0].minor.yy458->val(), yymsp[-2].minor.yy458->beginLoc(), yymsp[0].minor.yy458->endLoc());
}
#line 4197 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* term_int_eval ::= INTEGER */
#line 769 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> i_ptr = yymsp[0].minor.yy0;

	yygotominor.yy458 = 0;
	try {
		yygotominor.yy458 = new Number(boost::lexical_cast<int>(*yymsp[0].minor.yy0->str()), yymsp[0].minor.yy0->beginLoc());
	} catch (boost::bad_lexical_cast const& e) {
	parser->_parse_error("INTERNAL ERROR: Failed to parse integer \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
		YYERROR;
	}
}
#line 4212 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* term_int_eval ::= PAREN_L term_int_eval PAREN_R */
#line 781 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy458 = yymsp[-1].minor.yy458;
	yygotominor.yy458->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy458->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 4222 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* term_int_eval ::= DASH term_int_eval */
#line 801 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy458, yymsp[0].minor.yy458, -1 * yymsp[0].minor.yy458->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4228 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* term_int_eval ::= ABS term_int_eval */
#line 802 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy458, yymsp[0].minor.yy458, yymsp[0].minor.yy458->val() < 0 ? - yymsp[0].minor.yy458->val() : yymsp[0].minor.yy458->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 4234 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* term_int_eval ::= term_int_eval DASH term_int_eval */
#line 804 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() - yymsp[0].minor.yy458->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4240 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* term_int_eval ::= term_int_eval PLUS term_int_eval */
#line 805 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() + yymsp[0].minor.yy458->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 4246 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* term_int_eval ::= term_int_eval STAR term_int_eval */
#line 806 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() * yymsp[0].minor.yy458->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 4252 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* term_int_eval ::= term_int_eval INT_DIV term_int_eval */
#line 807 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() / yymsp[0].minor.yy458->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 4258 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* term_int_eval ::= term_int_eval MOD term_int_eval */
#line 808 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() % yymsp[0].minor.yy458->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 4264 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* formula ::= formula_base */
      case 184: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==184);
      case 256: /* formula_temporal ::= formula_temporal_base */ yytestcase(yyruleno==256);
#line 868 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = yymsp[0].minor.yy93;				}
#line 4271 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* formula ::= PAREN_L formula PAREN_R */
      case 185: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==185);
      case 257: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==257);
#line 869 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = yymsp[-1].minor.yy93; yygotominor.yy93->parens(true); 	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4280 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* formula ::= NOT formula */
      case 186: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==186);
      case 258: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==258);
#line 870 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy93, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 4287 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 144: /* formula ::= DASH formula */
      case 187: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==187);
      case 259: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==259);
#line 871 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy93, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 4294 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 145: /* formula ::= formula COMMA formula */
      case 188: /* formula_no_const ::= formula_no_const COMMA formula_no_const */ yytestcase(yyruleno==188);
      case 260: /* formula_temporal ::= formula_temporal COMMA formula_temporal */ yytestcase(yyruleno==260);
#line 873 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy93, yymsp[0].minor.yy93, yymsp[-2].minor.yy93->beginLoc(), yymsp[0].minor.yy93->endLoc());   yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4302 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 146: /* formula ::= formula DBL_PLUS formula */
      case 147: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==147);
      case 189: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==189);
      case 190: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==190);
      case 261: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==261);
      case 262: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==262);
#line 874 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy93, BinaryFormula::Operator::OR); }
#line 4312 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 148: /* formula ::= formula EQUIV formula */
      case 191: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==191);
      case 263: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==263);
#line 876 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy93, BinaryFormula::Operator::EQUIV); }
#line 4319 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 149: /* formula ::= formula IMPL formula */
      case 150: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==150);
      case 192: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==192);
      case 193: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==193);
      case 264: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==264);
      case 265: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==265);
#line 877 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy93, BinaryFormula::Operator::IMPL); }
#line 4329 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 151: /* formula_base ::= comparison */
      case 194: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==194);
      case 267: /* formula_temporal_base ::= comparison_temporal */ yytestcase(yyruleno==267);
      case 291: /* head_formula ::= comparison */ yytestcase(yyruleno==291);
#line 880 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = yymsp[0].minor.yy93; }
#line 4337 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* formula_base ::= atomic_formula */
      case 292: /* head_formula ::= atomic_head_formula */ yytestcase(yyruleno==292);
#line 881 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = yymsp[0].minor.yy52; }
#line 4343 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* formula_base ::= formula_quant */
      case 269: /* formula_temporal_base ::= formula_temporal_quant */ yytestcase(yyruleno==269);
#line 882 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = yymsp[0].minor.yy101; }
#line 4349 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* formula_base ::= formula_card */
      case 270: /* formula_temporal_base ::= formula_temporal_card */ yytestcase(yyruleno==270);
#line 884 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy93 = yymsp[0].minor.yy93;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy93->beginLoc());
			YYERROR;
		}
	}
#line 4361 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* formula_base ::= TRUE */
      case 195: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==195);
      case 271: /* formula_temporal_base ::= TRUE */ yytestcase(yyruleno==271);
      case 294: /* head_formula ::= TRUE */ yytestcase(yyruleno==294);
#line 891 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4369 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* formula_base ::= FALSE */
      case 196: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==196);
      case 272: /* formula_temporal_base ::= FALSE */ yytestcase(yyruleno==272);
      case 295: /* head_formula ::= FALSE */ yytestcase(yyruleno==295);
#line 892 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4377 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= term_strong EQ term */
      case 164: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==164);
      case 197: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==197);
      case 273: /* comparison_temporal ::= term_temporal_strong EQ term_temporal */ yytestcase(yyruleno==273);
#line 894 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy217, yymsp[0].minor.yy217, yymsp[-2].minor.yy217->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4386 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* comparison ::= term_strong DBL_EQ term */
      case 165: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==165);
      case 198: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==198);
      case 274: /* comparison_temporal ::= term_temporal_strong DBL_EQ term_temporal */ yytestcase(yyruleno==274);
#line 895 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy217, yymsp[0].minor.yy217, yymsp[-2].minor.yy217->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4395 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* comparison ::= term_strong NEQ term */
      case 166: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==166);
      case 199: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==199);
      case 275: /* comparison_temporal ::= term_temporal_strong NEQ term_temporal */ yytestcase(yyruleno==275);
#line 896 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy217, yymsp[0].minor.yy217, yymsp[-2].minor.yy217->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4404 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* comparison ::= term_strong LTHAN term */
      case 167: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==167);
      case 200: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==200);
      case 276: /* comparison_temporal ::= term_temporal_strong LTHAN term_temporal */ yytestcase(yyruleno==276);
#line 897 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy217, yymsp[0].minor.yy217, yymsp[-2].minor.yy217->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4413 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 161: /* comparison ::= term_strong GTHAN term */
      case 168: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==168);
      case 201: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==201);
      case 277: /* comparison_temporal ::= term_temporal_strong GTHAN term_temporal */ yytestcase(yyruleno==277);
#line 898 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy217, yymsp[0].minor.yy217, yymsp[-2].minor.yy217->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4422 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 162: /* comparison ::= term_strong LTHAN_EQ term */
      case 169: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==169);
      case 202: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==202);
      case 278: /* comparison_temporal ::= term_temporal_strong LTHAN_EQ term_temporal */ yytestcase(yyruleno==278);
#line 899 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy217, yymsp[0].minor.yy217, yymsp[-2].minor.yy217->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4431 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 163: /* comparison ::= term_strong GTHAN_EQ term */
      case 170: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==170);
      case 203: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==203);
      case 279: /* comparison_temporal ::= term_temporal_strong GTHAN_EQ term_temporal */ yytestcase(yyruleno==279);
#line 900 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy217, yymsp[0].minor.yy217, yymsp[-2].minor.yy217->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4440 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 171: /* comparison ::= constant DBL_EQ term */
#line 908 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy427, yymsp[0].minor.yy217, yymsp[-2].minor.yy427->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4446 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 172: /* comparison ::= constant NEQ term */
#line 909 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy427, yymsp[0].minor.yy217, yymsp[-2].minor.yy427->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4452 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 173: /* comparison ::= constant LTHAN term */
#line 910 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy427, yymsp[0].minor.yy217, yymsp[-2].minor.yy427->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4458 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 174: /* comparison ::= constant GTHAN term */
#line 911 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy427, yymsp[0].minor.yy217, yymsp[-2].minor.yy427->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4464 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 175: /* comparison ::= constant LTHAN_EQ term */
#line 912 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy427, yymsp[0].minor.yy217, yymsp[-2].minor.yy427->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4470 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 176: /* comparison ::= constant GTHAN_EQ term */
#line 913 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy427, yymsp[0].minor.yy217, yymsp[-2].minor.yy427->beginLoc(), yymsp[0].minor.yy217->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4476 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 177: /* atomic_formula ::= constant */
      case 181: /* atomic_formula_anon ::= const_anon */ yytestcase(yyruleno==181);
      case 204: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==204);
#line 943 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy52, yymsp[0].minor.yy427, true); }
#line 4483 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 178: /* atomic_formula ::= TILDE constant */
      case 182: /* atomic_formula_anon ::= TILDE const_anon */ yytestcase(yyruleno==182);
      case 205: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==205);
#line 944 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy52, yymsp[0].minor.yy427, false);   yy_destructor(yypParser,75,&yymsp[-1].minor);
}
#line 4491 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 179: /* atomic_formula ::= constant EQ term */
      case 183: /* atomic_formula_anon ::= const_anon EQ term */ yytestcase(yyruleno==183);
      case 206: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==206);
#line 945 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy52 = new AtomicFormula(yymsp[-2].minor.yy427, yymsp[0].minor.yy217, yymsp[-2].minor.yy427->beginLoc(), yymsp[0].minor.yy217->endLoc());	  yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4499 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 180: /* atomic_formula_anon ::= atomic_formula */
      case 296: /* atomic_head_formula ::= atomic_formula */ yytestcase(yyruleno==296);
      case 384: /* show_elem ::= atomic_formula_one_const */ yytestcase(yyruleno==384);
#line 947 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy52 = yymsp[0].minor.yy52; }
#line 4506 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 207: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
      case 280: /* formula_temporal_quant ::= BRACKET_L quant_lst PIPE formula_temporal BRACKET_R */ yytestcase(yyruleno==280);
#line 1012 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy101=NULL;
		ref_ptr<const Token> bl_ptr = yymsp[-4].minor.yy0;
		ref_ptr<QuantifierFormula::QuantifierList> lst_ptr = yymsp[-3].minor.yy347;
		ref_ptr<Formula> sub_ptr = yymsp[-1].minor.yy93;
		ref_ptr<const Token> br_ptr = yymsp[0].minor.yy0;

		if (!parser->lang()->support(Language::Feature::FORMULA_QUANTIFIER)) {
			parser->_feature_error(Language::Feature::FORMULA_QUANTIFIER, &yymsp[-4].minor.yy0->beginLoc());
			YYERROR;
		} else yygotominor.yy101 = new QuantifierFormula(yymsp[-3].minor.yy347, yymsp[-1].minor.yy93, yymsp[-4].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
	  yy_destructor(yypParser,98,&yymsp[-2].minor);
}
#line 4524 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* quant_lst ::= quant_op variable */
#line 1026 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy347 = new QuantifierFormula::QuantifierList();
		yygotominor.yy347->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy321, yymsp[0].minor.yy284));
	}
#line 4532 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 209: /* quant_lst ::= quant_lst quant_op variable */
#line 1032 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy347 = yymsp[-2].minor.yy347;
		yygotominor.yy347->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy321, yymsp[0].minor.yy284));
	}
#line 4540 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* quant_op ::= BIG_CONJ */
#line 1037 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy321 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,90,&yymsp[0].minor);
}
#line 4546 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* quant_op ::= BIG_DISJ */
#line 1038 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy321 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4552 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
      case 281: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==281);
#line 1084 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy93, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy145, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, NULL);  }
#line 4558 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
      case 282: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==282);
#line 1085 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy93, yymsp[-4].minor.yy217, yymsp[-3].minor.yy0, yymsp[-2].minor.yy145, yymsp[-1].minor.yy93,  yymsp[0].minor.yy0, NULL);  }
#line 4564 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 283: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==283);
#line 1086 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy93, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy145, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy217); 	}
#line 4570 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 215: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 284: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==284);
#line 1087 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy93, yymsp[-5].minor.yy217, yymsp[-4].minor.yy0, yymsp[-3].minor.yy145, yymsp[-2].minor.yy93,  yymsp[-1].minor.yy0, yymsp[0].minor.yy217); 	}
#line 4576 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
      case 285: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==285);
#line 1088 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy93, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, NULL);  }
#line 4582 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
      case 286: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==286);
#line 1089 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy93, yymsp[-3].minor.yy217, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy93,  yymsp[0].minor.yy0, NULL);  }
#line 4588 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 218: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
      case 287: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==287);
#line 1090 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy93, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy217); 	}
#line 4594 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 219: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
      case 288: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==288);
#line 1091 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy93, yymsp[-4].minor.yy217, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy93,  yymsp[-1].minor.yy0, yymsp[0].minor.yy217); 	}
#line 4600 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 220: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1095 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy145 = yymsp[-1].minor.yy145;
	  yy_destructor(yypParser,98,&yymsp[0].minor);
}
#line 4608 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 221: /* card_var_lst_inner ::= variable */
#line 1100 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy284;
		yygotominor.yy145 = new CardinalityFormula::VariableList();
		yygotominor.yy145->push_back(yymsp[0].minor.yy284->symbol());
	}
#line 4617 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1107 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy284;
		yygotominor.yy145 = yymsp[-2].minor.yy145;
		yygotominor.yy145->push_back(yymsp[0].minor.yy284->symbol());
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4627 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* term_temporal ::= constant */
#line 1161 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy217 default to undeclared identifiers
		yygotominor.yy217 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy427;
		parser->_parse_error("All constant symbols must be bound to a step using the i:F notation.", &yymsp[0].minor.yy427->beginLoc());
		YYERROR;
	}
#line 4638 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* term_temporal ::= term_temporal COLON term */
      case 248: /* term_temporal_strong ::= term_temporal_strong COLON term_strong */ yytestcase(yyruleno==248);
#line 1173 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy217, yymsp[-2].minor.yy217, yymsp[-1].minor.yy0, yymsp[0].minor.yy217, BindingTerm); }
#line 4644 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* formula_temporal ::= term_temporal_strong COLON formula */
#line 1255 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy93, yymsp[-2].minor.yy217, yymsp[-1].minor.yy0, yymsp[0].minor.yy93, BindingFormula); }
#line 4649 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* formula_temporal_base ::= atomic_formula */
#line 1261 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for more useful error messages
		yygotominor.yy93 = NULL;
		ref_ptr<const Referenced> l_ptr = yymsp[0].minor.yy52;
		parser->_parse_error("All constant symbols must be bound to a step using the i:F notation.", &yymsp[0].minor.yy52->beginLoc());
		YYERROR;
	}
#line 4660 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* head_formula ::= head_formula COMMA head_formula */
#line 1360 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy93 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy93, yymsp[0].minor.yy93, yymsp[-2].minor.yy93->beginLoc(), yymsp[0].minor.yy93->endLoc());
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4668 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* head_formula ::= PAREN_L head_formula PAREN_R */
#line 1364 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> lp_ptr = yymsp[-2].minor.yy0, rp_ptr = yymsp[0].minor.yy0;
		yygotominor.yy93 = yymsp[-1].minor.yy93;
		yygotominor.yy93->parens(true);																									\
		yygotominor.yy93->beginLoc(yymsp[-2].minor.yy0->beginLoc());																					\
		yygotominor.yy93->endLoc(yymsp[0].minor.yy0->endLoc());
		
	}
#line 4680 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* head_formula ::= formula_smpl_card */
#line 1375 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy93 = yymsp[0].minor.yy471;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy471->beginLoc());
			YYERROR;
		}
	}
#line 4691 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* atomic_head_formula ::= DASH constant */
#line 1388 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy52 = NULL;
		ref_ptr<const Token> d_ptr = yymsp[-1].minor.yy0;
		ref_ptr<Constant> c_ptr = yymsp[0].minor.yy427;

		if (!parser->lang()->support(Language::Feature::FORMULA_NOT_DASH_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_NOT_DASH_HEAD);
			YYERROR;
		} else {
			ATOMIC_FORMULA(yygotominor.yy52, yymsp[0].minor.yy427, false); 
		}
	}
#line 4707 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1401 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy471, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy145, yymsp[-1].minor.yy52, yymsp[0].minor.yy0, NULL);  }
#line 4712 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1402 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy471, yymsp[-4].minor.yy217, yymsp[-3].minor.yy0, yymsp[-2].minor.yy145, yymsp[-1].minor.yy52,  yymsp[0].minor.yy0, NULL);  }
#line 4717 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1403 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy471, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy145, yymsp[-2].minor.yy52, yymsp[-1].minor.yy0, yymsp[0].minor.yy217); 	}
#line 4722 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1404 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy471, yymsp[-5].minor.yy217, yymsp[-4].minor.yy0, yymsp[-3].minor.yy145, yymsp[-2].minor.yy52,  yymsp[-1].minor.yy0, yymsp[0].minor.yy217); 	}
#line 4727 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1405 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy471, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy52, yymsp[0].minor.yy0, NULL);  }
#line 4732 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1406 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy471, yymsp[-3].minor.yy217, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy52,  yymsp[0].minor.yy0, NULL);  }
#line 4737 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1407 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy471, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy52, yymsp[-1].minor.yy0, yymsp[0].minor.yy217); 	}
#line 4742 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1408 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy471, yymsp[-4].minor.yy217, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy52,  yymsp[-1].minor.yy0, yymsp[0].minor.yy217); 	}
#line 4747 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1427 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy291 = NULL;
        ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
        ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
        ref_ptr<MacroDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy19;
        ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;

        if (!parser->lang()->support(Language::Feature::DECL_MACRO)) {
            parser->_feature_error(Language::Feature::DECL_MACRO, &yymsp[-2].minor.yy0->beginLoc());
            YYERROR;
        } else {
		    BOOST_FOREACH(symbols::MacroSymbol* m, *yymsp[-1].minor.yy19) {
			    if (!parser->symtab()->create(m)) {
	    	        // Check if it's a duplicate
	    	        symbols::MacroSymbol* m2 = (symbols::MacroSymbol*)parser->symtab()->resolve(symbols::Symbol::Type::MACRO, *m->base(), m->arity());
		            if (!m2 || m2 != m) {
		                parser->_parse_error("Detected conflicting definition of symbol \"" + *m->name() + "\".", &yygotominor.yy291->beginLoc());
		            } else {
		                parser->_parse_error("Detected a duplicate definition of symbol \"" + *m->name() + "\".", &yygotominor.yy291->beginLoc());
		            }
		        }
		    }

			yygotominor.yy291 = new MacroDeclaration(yymsp[-1].minor.yy19, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
        }
    }
#line 4777 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* macro_def_lst ::= macro_bnd */
#line 1455 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy19 = new MacroDeclaration::ElementList();
        yygotominor.yy19->push_back(yymsp[0].minor.yy313);
    }
#line 4785 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1461 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy19 = yymsp[-2].minor.yy19;
        yygotominor.yy19->push_back(yymsp[0].minor.yy313);
      yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4794 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1467 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy440;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy313 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy440);
      yy_destructor(yypParser,71,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4808 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1476 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy313 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4819 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* macro_args ::= macro_arg */
#line 1484 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy440 = new MacroSymbol::ArgumentList();
        yygotominor.yy440->push_back(yymsp[0].minor.yy143->str());
        delete yymsp[0].minor.yy143;
    }
#line 4828 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* macro_args ::= macro_args COMMA macro_arg */
#line 1490 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy440 = yymsp[-2].minor.yy440;
        yygotominor.yy440->push_back(yymsp[0].minor.yy143->str());
        delete yymsp[0].minor.yy143;
      yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4838 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* macro_arg ::= POUND_INTEGER */
      case 314: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==314);
#line 1497 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy143 = yymsp[0].minor.yy0;
    }
#line 4846 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* sort_lst ::= sort */
#line 1524 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy405 = new ConstantSymbol::SortList();
		yygotominor.yy405->push_back(yymsp[0].minor.yy385);
	}
#line 4854 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* sort_lst ::= sort_lst COMMA sort */
#line 1529 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy405 = yymsp[-2].minor.yy405;
		yygotominor.yy405->push_back(yymsp[0].minor.yy385);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4863 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* sort ::= sort_id_nr */
      case 323: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==323);
      case 324: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==324);
#line 1554 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy385 = yymsp[0].minor.yy385; }
#line 4870 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* sort ::= sort_id_nr STAR */
#line 1555 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy385, yymsp[-1].minor.yy385, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::NONE)); }
#line 4875 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* sort ::= sort_id_nr CARROT */
#line 1556 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy385, yymsp[-1].minor.yy385, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::UNKNOWN)); }
#line 4880 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 320: /* sort ::= sort PLUS object_nullary */
#line 1558 "bcplus/parser/detail/lemon_parser.y"
{ u::ref_ptr<const Object> o_ptr = yymsp[0].minor.yy138; DYNAMIC_SORT_PLUS(yygotominor.yy385, yymsp[-2].minor.yy385, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, yymsp[0].minor.yy138->symbol()); }
#line 4885 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* sort ::= sort PLUS IDENTIFIER */
#line 1561 "bcplus/parser/detail/lemon_parser.y"
{
												  u::ref_ptr<const Referenced> s_ptr = yymsp[-2].minor.yy385, op_ptr = yymsp[-1].minor.yy0, id_ptr = yymsp[0].minor.yy0;
												  u::ref_ptr<const ObjectSymbol> obj = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
												  if(!obj) {
													if (parser->lang()->support(Language::Feature::SORT_PLUS)) 
														parser->_parse_error("\"" + *yymsp[0].minor.yy0->str() + "\" could not be declared as an object as this conflicts with a previous declarations of this identifier.", &yymsp[0].minor.yy0->beginLoc());
													else 
														parser->_feature_error(Language::Feature::SORT_PLUS, &yymsp[-1].minor.yy0->beginLoc());
													YYERROR;
												  } else {
													DYNAMIC_SORT_PLUS(yygotominor.yy385, yymsp[-2].minor.yy385, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, obj);
												  }
												}
#line 4902 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 322: /* sort ::= sort PLUS INTEGER */
#line 1575 "bcplus/parser/detail/lemon_parser.y"
{ 
												  ref_ptr<const Object> t_ptr;
												  BASIC_TERM(t_ptr, yymsp[0].minor.yy0);
												  DYNAMIC_SORT_PLUS(yygotominor.yy385, yymsp[-2].minor.yy385, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, t_ptr->symbol()); 
												}
#line 4911 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 325: /* sort_nr ::= num_range */
#line 1586 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy171;

		yygotominor.yy385 = NULL;

		if (!parser->lang()->support(Language::Feature::NUMRANGE_SORT)) {
			parser->_feature_error(Language::Feature::NUMRANGE_SORT, &yymsp[0].minor.yy171->beginLoc());
			YYERROR;
		}

		// X..Y becomes __rsort_N_
		if(!(yygotominor.yy385 = parser->_newRange(yymsp[0].minor.yy171->min(), yymsp[0].minor.yy171->max()))) {
			parser->_parse_error("INTERNAL ERROR: An error occurred while instantiating the dynamic sort declaration.", &yymsp[0].minor.yy171->beginLoc());
			YYERROR;
		}
	}
#line 4931 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 326: /* sort_id ::= IDENTIFIER */
#line 1604 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy385 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy385) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4944 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 327: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1635 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ConstantDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy295;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_CONSTANT)) {
			yygotominor.yy355 = NULL;
			parser->_feature_error(Language::Feature::DECL_CONSTANT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy355 = new ConstantDeclaration(yymsp[-1].minor.yy295, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

		}
	}
#line 4963 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 328: /* constant_bnd_lst ::= constant_bnd */
#line 1652 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy295 = yymsp[0].minor.yy295;
	}
#line 4970 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1657 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy295;
		yygotominor.yy295 = yymsp[-2].minor.yy295;
		yygotominor.yy295->splice(yygotominor.yy295->end(), *yymsp[0].minor.yy295);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4980 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 330: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1677 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const SortSymbol> s_ptr = yymsp[-1].minor.yy385;
		ref_ptr<const Referenced> names_ptr = yymsp[-5].minor.yy444;
		yygotominor.yy295 = new ConstantDeclaration::ElementList();

		// NOTE: additive constants default to the additive sort, not the boolean sort
		if (yymsp[-3].minor.yy109 & ConstantSymbol::Type::M_ADDITIVE) s_ptr = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

		// external constants should have "unknown" in their sort
		else if (yymsp[-3].minor.yy109 & ConstantSymbol::Type::M_EXTERNAL) s_ptr = parser->symtab()->carrot(yymsp[-1].minor.yy385);

		// non-boolean abActions should contain "none"
		else if (yymsp[-3].minor.yy109 == ConstantSymbol::Type::ABACTION && s_ptr->domainType() != DomainType::BOOLEAN) s_ptr = parser->symtab()->star(yymsp[-1].minor.yy385);

		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-5].minor.yy444) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[-3].minor.yy109, decl.first->str(), s_ptr, decl.second);
			yygotominor.yy295->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5008 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 331: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1699 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy444, s_ptr = yymsp[0].minor.yy385;
		yygotominor.yy295 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy444) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(ConstantSymbol::Type::RIGID, decl.first->str(), yymsp[0].minor.yy385, decl.second);
			yygotominor.yy295->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5023 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 332: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1710 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy444;
		yygotominor.yy295 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy444) {
			// attempt to declare each symbol
			ref_ptr<SortSymbol> s = parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN);

			// NOTE: additive constants default to the additive sort, not the boolean sort
			if (yymsp[0].minor.yy109 & ConstantSymbol::Type::M_ADDITIVE && s->domainType() == DomainType::BOOLEAN ) 
				s = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

			// external constants should have "unknown" in their sort
			else if (yymsp[0].minor.yy109 & ConstantSymbol::Type::M_EXTERNAL) 
				s = parser->symtab()->carrot(s);

			// non-boolean abActions should contain "none"
			else if (yymsp[0].minor.yy109 == ConstantSymbol::Type::ABACTION && s->domainType() != DomainType::BOOLEAN) 
				s = parser->symtab()->star(s);


			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[0].minor.yy109, decl.first->str(), s, decl.second);
			yygotominor.yy295->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5053 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 333: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1736 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy295 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-4].minor.yy444, s_ptr = yymsp[-2].minor.yy322, id_ptr = yymsp[0].minor.yy0;


		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[0].minor.yy0->str());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\" is not a valid constant symbol.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy295 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-4].minor.yy444) {
				ref_ptr<ConstantSymbol> c= new AttributeSymbol(decl.first->str(), yymsp[-2].minor.yy322, c, decl.second);
				yygotominor.yy295->push_back(c);
				CONSTANT_DECL(c, decl.first->beginLoc());
			}
		}
	  yy_destructor(yypParser,76,&yymsp[-3].minor);
  yy_destructor(yypParser,55,&yymsp[-1].minor);
}
#line 5082 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 334: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1760 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy295 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-7].minor.yy444, s_ptr = yymsp[-5].minor.yy322, id_ptr = yymsp[-3].minor.yy0, lst_ptr = yymsp[-1].minor.yy405;

		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy405->size());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy405->size()) + "\" is not a valid constant symbol.", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy405->size()) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// ensure that the sorts match the declaration of the symbol
			SortList::const_iterator it = yymsp[-1].minor.yy405->begin();
			BOOST_FOREACH(SortSymbol const* sort, *c) {
				if (*it != sort) {
					// check to see if it'yymsp[-5].minor.yy322 a subsort, which is also permissable
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

			yygotominor.yy295 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-7].minor.yy444) {
				// ensure that the sorts match the start of the sort list for each of the symbols
				if (decl.second->size() < yymsp[-1].minor.yy405->size()) {
					parser->_parse_error("Detected a malformed attribute declaration. An attribute must duplicate its parent'yymsp[-5].minor.yy322 parameters.", &decl.first->beginLoc());
					YYERROR;
				} else {
					bool good_sort = true;		
					it = decl.second->begin();
					BOOST_FOREACH(SortSymbol const* sort, *yymsp[-1].minor.yy405) {
						if (*it != sort) {
							// check to see if it'yymsp[-5].minor.yy322 a subsort, which is also permissable
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
						ref_ptr<ConstantSymbol> sym = new AttributeSymbol(decl.first->str(), yymsp[-5].minor.yy322, c, decl.second);
						yygotominor.yy295->push_back(sym);
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
#line 5163 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 335: /* constant_dcl_lst ::= IDENTIFIER */
#line 1836 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy444 = new IdentifierDeclList();
		yygotominor.yy444->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 5171 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 336: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1841 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy444 = new IdentifierDeclList();
		yygotominor.yy444->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy405));
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5181 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 337: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1846 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy444 = yymsp[-2].minor.yy444;
		yygotominor.yy444->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5190 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 338: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1851 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy444 = yymsp[-5].minor.yy444;
		yygotominor.yy444->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy405));
	  yy_destructor(yypParser,102,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5201 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 339: /* constant_dcl_type ::= ABACTION */
#line 1858 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5213 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 340: /* constant_dcl_type ::= ACTION */
#line 1867 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5225 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 341: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1876 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5237 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 342: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1885 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5249 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 343: /* constant_dcl_type ::= EXTERNALACTION */
#line 1894 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5261 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 344: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1903 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5273 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 345: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1912 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5285 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1921 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5297 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* constant_dcl_type ::= RIGID */
#line 1930 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5309 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 348: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1939 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5321 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* constant_dcl_type ::= SDFLUENT */
#line 1949 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy109 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5333 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* attrib_spec ::= ATTRIBUTE */
#line 1959 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy322 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[0].minor.yy0;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// grab the boolean sort and provide it
			yygotominor.yy322 = parser->symtab()->star(parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN));
		}
	}
#line 5348 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1972 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy322 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[-3].minor.yy0, s_ptr = yymsp[-1].minor.yy385;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy322 = parser->symtab()->star(yymsp[-1].minor.yy385);
		}
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5364 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 2000 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ObjectDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy214;
		
		if (!parser->lang()->support(Language::Feature::DECL_OBJECT)) {
			yygotominor.yy288 = NULL;
			parser->_feature_error(Language::Feature::DECL_OBJECT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy288 = new ObjectDeclaration(yymsp[-1].minor.yy214, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ObjectDeclaration::Element* bnd, *yymsp[-1].minor.yy214) {
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
#line 5399 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* object_bnd_lst ::= object_bnd */
#line 2033 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy214 = new ObjectDeclaration::ElementList();
		yygotominor.yy214->push_back(yymsp[0].minor.yy380);
	}
#line 5407 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 2039 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy214 = yymsp[-2].minor.yy214;
		yygotominor.yy214->push_back(yymsp[0].minor.yy380);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5416 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 2045 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy380 = new ObjectDeclaration::Element(yymsp[0].minor.yy385, yymsp[-2].minor.yy387);
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5424 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* object_lst ::= object_spec */
#line 2050 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy387 = yymsp[0].minor.yy387;
	}
#line 5431 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* object_lst ::= object_lst COMMA object_spec */
#line 2054 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy387 = yymsp[-2].minor.yy387;
		yygotominor.yy387->splice(yygotominor.yy387->end(), *yymsp[0].minor.yy387);
		delete yymsp[0].minor.yy387;
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5441 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* object_spec ::= IDENTIFIER */
#line 2063 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		yygotominor.yy387 = NULL;
		ref_ptr<const Symbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy387 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy387->push_back(o);
		}
	}
#line 5457 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 2076 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy387 = NULL;
		ref_ptr<ObjectSymbol::SortList> lst_ptr = yymsp[-1].minor.yy405;
		ref_ptr<const Token> id_ptr = yymsp[-3].minor.yy0;
		ref_ptr<ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy405));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(),yymsp[-1].minor.yy405->size()) + "\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy387 = new  ObjectDeclaration::Element::ObjectList();
			yygotominor.yy387->push_back(o.get());
		}
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5476 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* object_spec ::= INTEGER */
#line 2091 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy387 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy387 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy387->push_back(o.get());
		}
	}
#line 5492 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* object_spec ::= num_range */
#line 2105 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy387 = new ObjectDeclaration::Element::ObjectList();
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy171;

		// iterate over the range and add it to the list
		ref_ptr<const Symbol> o = parser->symtab()->resolveOrCreate(parser->_newRangeSymbol( yymsp[0].minor.yy171->min(), yymsp[0].minor.yy171->max()));
		if (!o) {
			parser->_parse_error("INTERNAL ERROR: Could not create object symbol.", &yymsp[0].minor.yy171->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy387->push_back(o.get());
		}
	}
#line 5509 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 2134 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<VariableDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy377;
		
		if (!parser->lang()->support(Language::Feature::DECL_VARIABLE)) {
			yygotominor.yy267 = NULL;
			parser->_feature_error(Language::Feature::DECL_VARIABLE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {

			VariableSymbol* v2;

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ref_ptr<VariableSymbol>& v, *yymsp[-1].minor.yy377) {
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

			yygotominor.yy267 = new VariableDeclaration(yymsp[-1].minor.yy377, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());


		}
	}
#line 5547 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* variable_bnd_lst ::= variable_bnd */
#line 2170 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy377 = yymsp[0].minor.yy377;
	}
#line 5554 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 2175 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy377 = yymsp[-2].minor.yy377;
		yygotominor.yy377->splice(yygotominor.yy377->end(), *yymsp[0].minor.yy377);
		delete yymsp[0].minor.yy377;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5564 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 365: /* variable_bnd ::= variable_lst DBL_COLON sort */
#line 2182 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy377 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy298) {
			yygotominor.yy377->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy385));
		}



		delete yymsp[-2].minor.yy298;
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5580 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* variable_lst ::= IDENTIFIER */
#line 2195 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy298 = new TokenList();
		yygotominor.yy298->push_back(yymsp[0].minor.yy0);
	}
#line 5588 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 2200 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy298 = yymsp[-2].minor.yy298;
		yygotominor.yy298->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5597 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 2221 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<SortDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy445;
		
		if (!parser->lang()->support(Language::Feature::DECL_SORT)) {
			yygotominor.yy225 = NULL;
			parser->_feature_error(Language::Feature::DECL_SORT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy225 = new SortDeclaration(yymsp[-1].minor.yy445, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5615 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 369: /* sort_bnd_lst ::= sort_bnd */
      case 371: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==371);
#line 2237 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy445 = yymsp[0].minor.yy445;
	}
#line 5623 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 370: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 2242 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy445 = yymsp[-2].minor.yy445;
		yygotominor.yy445->splice(yygotominor.yy445->end(), *yymsp[0].minor.yy445);
		delete yymsp[0].minor.yy445;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5633 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 372: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 2254 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy445) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy445) {
				sym2->addSubSort(sym);
			}
		}
		yygotominor.yy445 = yymsp[-2].minor.yy445;
		yygotominor.yy445->splice(yymsp[-2].minor.yy445->end(), *yymsp[0].minor.yy445);
		delete yymsp[0].minor.yy445;

	  yy_destructor(yypParser,100,&yymsp[-1].minor);
}
#line 5649 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 373: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 2266 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy445) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy445) {
				sym->addSubSort(sym2);
			}
		}
		yygotominor.yy445 = yymsp[-2].minor.yy445;
		yygotominor.yy445->splice(yymsp[-2].minor.yy445->end(), *yymsp[0].minor.yy445);
		delete yymsp[0].minor.yy445;
	  yy_destructor(yypParser,99,&yymsp[-1].minor);
}
#line 5664 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 374: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 2277 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy445 = yymsp[-1].minor.yy445;
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5673 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 375: /* sort_dcl_lst ::= IDENTIFIER */
#line 2282 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yygotominor.yy445 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy445 = new SortDeclaration::ElementList();
			yygotominor.yy445->push_back(s);
		}

		delete yymsp[0].minor.yy0;
	}
#line 5690 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 376: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 2296 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy445 = yymsp[-2].minor.yy445;
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yymsp[-2].minor.yy445 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yymsp[-2].minor.yy445->push_back(s);
		}

		delete yymsp[0].minor.yy0;

	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5709 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 377: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 2323 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy10 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<ShowStatement::ElementList> lst_ptr = yymsp[-1].minor.yy65;

		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy10 = new ShowStatement(yymsp[-1].minor.yy65, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5725 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 378: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 2337 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy10 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_SHOW_ALL)) {
			parser->_feature_error(Language::Feature::DECL_SHOW_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy10 = new ShowAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5743 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 379: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 2354 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy10 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<HideStatement::ElementList> lst_ptr = yymsp[-1].minor.yy65;

		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy10 = new HideStatement(yymsp[-1].minor.yy65, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5759 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 380: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2368 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy10 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_HIDE_ALL)) {
			parser->_feature_error(Language::Feature::DECL_HIDE_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy10 = new HideAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5777 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 381: /* show_lst ::= show_elem */
#line 2386 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy65 = new ShowStatement::ElementList();
		yygotominor.yy65->push_back(yymsp[0].minor.yy52);
	}
#line 5785 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 382: /* show_lst ::= show_lst COMMA show_elem */
#line 2391 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy65 = yymsp[-2].minor.yy65;
		yygotominor.yy65->push_back(yymsp[0].minor.yy52);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5794 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 383: /* show_lst ::= show_lst SEMICOLON show_elem */
#line 2396 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy65 = yymsp[-2].minor.yy65;
		yygotominor.yy65->push_back(yymsp[0].minor.yy52);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5803 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 385: /* stmt_noconcurrency ::= NOCONCURRENCY PERIOD */
#line 2424 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy268, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5808 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 386: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD */
#line 2425 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy418, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5813 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 387: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_int_eval PERIOD */
#line 2451 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy10, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy458, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5819 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 388: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE DBL_COLON term_int_eval PERIOD */
#line 2452 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy10, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy458, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,76,&yymsp[-2].minor);
}
#line 5825 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 389: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_int_eval PERIOD */
#line 2453 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy10, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy458, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5831 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 390: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE DBL_COLON term_int_eval PERIOD */
#line 2454 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy10, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy458, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,76,&yymsp[-2].minor);
}
#line 5837 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 391: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2479 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy320 = NULL;
		ref_ptr<const Referenced> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, data_l_ptr = yymsp[-1].minor.yy271.l, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Referenced> data_maxstep_ptr = yymsp[-1].minor.yy271.maxstep, data_label_ptr = yymsp[-1].minor.yy271.label;

		ref_ptr<const ReferencedString> label;
		if (yymsp[-1].minor.yy271.label) label = yymsp[-1].minor.yy271.label->str();
		else label = new ReferencedString("0");

		int min = -1, max = -1;
		if (yymsp[-1].minor.yy271.maxstep) {
			min = yymsp[-1].minor.yy271.maxstep->min();
			max = yymsp[-1].minor.yy271.maxstep->max();
		}

		if (!parser->lang()->support(Language::Feature::DECL_QUERY)) {
			parser->_feature_error(Language::Feature::DECL_QUERY, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			bool good = true;

			// resolve the query label
			ref_ptr<QuerySymbol> sym = new QuerySymbol(label, min, max);
			if (!parser->symtab()->create(sym)) {
				parser->_parse_error("Could not create query, the label \"" + *label + "\" already exists.", (yymsp[-1].minor.yy271.label ? &yymsp[-1].minor.yy271.label->beginLoc() : &yymsp[-2].minor.yy0->beginLoc()));
				good = false;
				YYERROR;
			}


			if (good) yygotominor.yy320 = new QueryStatement(sym, yymsp[-1].minor.yy271.l, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5874 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 392: /* query_lst ::= formula_temporal */
#line 2515 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy271.l = new QueryStatement::FormulaList();
		yygotominor.yy271.maxstep = NULL;
		yygotominor.yy271.label = NULL;

		yygotominor.yy271.l->push_back(yymsp[0].minor.yy93);
	}
#line 5885 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 393: /* query_lst ::= query_maxstep_decl */
#line 2524 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy271.l = new QueryStatement::FormulaList();
		yygotominor.yy271.maxstep = yymsp[0].minor.yy259;
		yygotominor.yy271.label = NULL;
	}
#line 5894 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 394: /* query_lst ::= query_label_decl */
#line 2531 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy271.l = new QueryStatement::FormulaList();
		yygotominor.yy271.maxstep = NULL;
		yygotominor.yy271.label = yymsp[0].minor.yy143;
	}
#line 5903 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 395: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2538 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy271 = yymsp[-2].minor.yy271;
		yymsp[-2].minor.yy271.l->push_back(yymsp[0].minor.yy93);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5912 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 396: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2544 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy271 = yymsp[-2].minor.yy271;

		if (yygotominor.yy271.maxstep) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy259->beginLoc());
			delete yymsp[0].minor.yy259;
			YYERROR;
		} else {
			yygotominor.yy271.maxstep = yymsp[0].minor.yy259;
		}
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5928 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 397: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2557 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy271 = yymsp[-2].minor.yy271;
		if (yygotominor.yy271.label) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy143->beginLoc());
			delete yymsp[0].minor.yy143;
			YYERROR;

		} else {
			yygotominor.yy271.label = yymsp[0].minor.yy143;
		}
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5944 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 398: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2583 "bcplus/parser/detail/lemon_parser.y"
{ 
	yygotominor.yy259 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, i_ptr = yymsp[0].minor.yy0;


	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {

		int max = -1;
		try {
			max = boost::lexical_cast<int>(*yymsp[0].minor.yy0->str());
			yygotominor.yy259 = new NumberRangeEval(-1, max, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		} catch (boost::bad_lexical_cast const& e) {
			parser->_parse_error("INTERNAL ERROR: An error occurred extracting an integer from \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5969 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 399: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range_eval */
#line 2604 "bcplus/parser/detail/lemon_parser.y"
{
	yygotominor.yy259 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, nr_ptr = yymsp[0].minor.yy259;

	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {
		yygotominor.yy259 = yymsp[0].minor.yy259;
		nr_ptr.release();
	}
  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5986 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 400: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 401: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==401);
#line 2618 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy143, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5993 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 402: /* clause_if ::= IF formula */
#line 2653 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy93, Language::Feature::CLAUSE_IF); 		}
#line 5998 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 403: /* clause_if ::= */
      case 405: /* clause_after ::= */ yytestcase(yyruleno==405);
      case 407: /* clause_ifcons ::= */ yytestcase(yyruleno==407);
      case 411: /* clause_where ::= */ yytestcase(yyruleno==411);
#line 2654 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = NULL; }
#line 6006 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 404: /* clause_after ::= AFTER formula */
#line 2655 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy93, Language::Feature::CLAUSE_AFTER);	}
#line 6011 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 406: /* clause_ifcons ::= IFCONS formula */
#line 2657 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy93, Language::Feature::CLAUSE_IFCONS); 	}
#line 6016 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 408: /* clause_unless ::= UNLESS atomic_formula_anon */
#line 2659 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy52, yymsp[-1].minor.yy0, yymsp[0].minor.yy52, Language::Feature::CLAUSE_UNLESS); 	}
#line 6021 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 409: /* clause_unless ::= */
#line 2660 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy52 = NULL; }
#line 6026 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 410: /* clause_where ::= WHERE formula_no_const */
#line 2661 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy93, yymsp[-1].minor.yy0, yymsp[0].minor.yy93, Language::Feature::CLAUSE_WHERE); 	}
#line 6031 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 412: /* stmt_law ::= law_basic */
      case 413: /* stmt_law ::= law_caused */ yytestcase(yyruleno==413);
      case 414: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==414);
      case 415: /* stmt_law ::= law_impl */ yytestcase(yyruleno==415);
      case 416: /* stmt_law ::= law_causes */ yytestcase(yyruleno==416);
      case 417: /* stmt_law ::= law_increments */ yytestcase(yyruleno==417);
      case 418: /* stmt_law ::= law_decrements */ yytestcase(yyruleno==418);
      case 419: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==419);
      case 420: /* stmt_law ::= law_always */ yytestcase(yyruleno==420);
      case 421: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==421);
      case 422: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==422);
      case 423: /* stmt_law ::= law_never */ yytestcase(yyruleno==423);
      case 424: /* stmt_law ::= law_default */ yytestcase(yyruleno==424);
      case 425: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==425);
      case 426: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==426);
      case 427: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==427);
      case 428: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==428);
      case 429: /* stmt_law ::= law_observed */ yytestcase(yyruleno==429);
      case 430: /* stmt_law ::= law_temporal_constraint */ yytestcase(yyruleno==430);
#line 2709 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy10 = yymsp[0].minor.yy10;}
#line 6054 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 431: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2830 "bcplus/parser/detail/lemon_parser.y"
{ 
		if (yymsp[-5].minor.yy93 || yymsp[-4].minor.yy93 || yymsp[-3].minor.yy93 || yymsp[-2].minor.yy52 || yymsp[-1].minor.yy93) {
			LAW_BASIC_FORM(yygotominor.yy10, NULL, yymsp[-6].minor.yy93, yymsp[-5].minor.yy93, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, 
				yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
				Language::Feature::LAW_BASIC_D, BasicLaw); 
		} else {
			LAW_BASIC_FORM(yygotominor.yy10, NULL, yymsp[-6].minor.yy93, yymsp[-5].minor.yy93, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, 
				yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_FACT, 
				Language::Feature::LAW_BASIC_FACT, BasicLaw); 
		}
	}
#line 6069 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 432: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2842 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy10, yymsp[-7].minor.yy0, yymsp[-6].minor.yy93, yymsp[-5].minor.yy93, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, 
																																														yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 6076 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 433: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2846 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy10, yymsp[-7].minor.yy0, yymsp[-6].minor.yy93, yymsp[-5].minor.yy93, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, 
																																														yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 6083 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 434: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2850 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy10, yymsp[-4].minor.yy93, yymsp[-3].minor.yy0, yymsp[-2].minor.yy93, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 6089 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 435: /* law_impl ::= ARROW_LDASH formula clause_where PERIOD */
#line 2853 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy10, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy93, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 6095 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 436: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2856 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy10, yymsp[-6].minor.yy52, yymsp[-5].minor.yy0, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 6101 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 437: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2860 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy10, yymsp[-8].minor.yy52, yymsp[-7].minor.yy0, yymsp[-6].minor.yy427, yymsp[-4].minor.yy217, yymsp[-3].minor.yy93, yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 6108 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 438: /* law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2863 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy10, yymsp[-8].minor.yy52, yymsp[-7].minor.yy0, yymsp[-6].minor.yy427, yymsp[-4].minor.yy217, yymsp[-3].minor.yy93, yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_DECREMENTS, DecrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 6115 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 439: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2867 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy10, yymsp[-6].minor.yy52, yymsp[-5].minor.yy0, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 6121 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 440: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2871 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy10, yymsp[-5].minor.yy0, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 6128 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 441: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2875 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy10, yymsp[-5].minor.yy0, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 6135 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 442: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2879 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy10, yymsp[-5].minor.yy0, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 6142 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 443: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2883 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy10, yymsp[-5].minor.yy0, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 6149 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 444: /* law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2887 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy10, yymsp[-7].minor.yy0, yymsp[-6].minor.yy52, yymsp[-5].minor.yy93, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, 
																																														yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 6156 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 445: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2891 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy10, yymsp[-7].minor.yy0, yymsp[-6].minor.yy427, yymsp[-5].minor.yy93, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, 
																																														yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 6163 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 446: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2895 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy10, yymsp[-7].minor.yy0, yymsp[-6].minor.yy427, yymsp[-5].minor.yy93, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, 
																																														yymsp[-2].minor.yy52, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 6170 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 447: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2899 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy10, yymsp[-5].minor.yy0, yymsp[-4].minor.yy93, yymsp[-3].minor.yy93, yymsp[-2].minor.yy52, yymsp[-1].minor.yy93,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 6176 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 448: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2903 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy10, yymsp[-3].minor.yy0, yymsp[-2].minor.yy427, yymsp[-1].minor.yy93, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 6182 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 449: /* law_observed ::= OBSERVED atomic_head_formula AT term_int_eval PERIOD */
#line 2908 "bcplus/parser/detail/lemon_parser.y"
{ 
			LAW_SIMPLE_FORM(yygotominor.yy10, yymsp[-4].minor.yy0, yymsp[-3].minor.yy52, yymsp[-1].minor.yy458, yymsp[0].minor.yy0, Language::Feature::LAW_OBSERVED, ObservedLaw); 
		  yy_destructor(yypParser,65,&yymsp[-2].minor);
}
#line 6190 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 450: /* law_temporal_constraint ::= CONSTRAINT formula AT term_int_eval PERIOD */
#line 2913 "bcplus/parser/detail/lemon_parser.y"
{ 
			LAW_SIMPLE_FORM(yygotominor.yy10, yymsp[-4].minor.yy0, yymsp[-3].minor.yy93, yymsp[-1].minor.yy458, yymsp[0].minor.yy0, Language::Feature::LAW_TEMPORAL_CONSTRAINT, TemporalConstraintLaw); 
		  yy_destructor(yypParser,65,&yymsp[-2].minor);
}
#line 6198 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 451: /* stmt_code_blk ::= ASP_GR */
#line 2936 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy10, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 6203 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 452: /* stmt_code_blk ::= ASP_CP */
#line 2937 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy10, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 6208 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 453: /* stmt_code_blk ::= F2LP_GR */
#line 2938 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy10, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 6213 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 454: /* stmt_code_blk ::= F2LP_CP */
#line 2939 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy10, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 6218 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 455: /* stmt_code_blk ::= LUA_GR */
#line 2940 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy10, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 6223 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 456: /* stmt_code_blk ::= LUA_CP */
#line 2941 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy10, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 6228 "bcplus/parser/detail/lemon_parser.c"
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
#line 6294 "bcplus/parser/detail/lemon_parser.c"
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
