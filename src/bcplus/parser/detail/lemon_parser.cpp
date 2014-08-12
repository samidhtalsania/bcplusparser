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

#line 914 "bcplus/parser/detail/lemon_parser.y"


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
	


#line 1046 "bcplus/parser/detail/lemon_parser.y"

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



#line 1115 "bcplus/parser/detail/lemon_parser.y"

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

#line 1525 "bcplus/parser/detail/lemon_parser.y"

		
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
#line 1653 "bcplus/parser/detail/lemon_parser.y"

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
#line 2400 "bcplus/parser/detail/lemon_parser.y"

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

#line 2423 "bcplus/parser/detail/lemon_parser.y"

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
#line 2452 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRangeEval* maxstep;
		Token const* label;
	};

#line 2559 "bcplus/parser/detail/lemon_parser.y"

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

#line 2631 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2717 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2915 "bcplus/parser/detail/lemon_parser.y"

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
#define YYNSTATE 864
#define YYNRULE 455
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
#define YY_ACTTAB_COUNT (3454)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   863,   64,  864,  862,  861,  860,  859,  858,  857,  856,
 /*    10 */   855,  854,  853,  852,  851,  850,  849,  848,   63,  820,
 /*    20 */   322,  847,  840,  846,  845,  839,  150,  148,  146,  145,
 /*    30 */   144,  842,  318,  323,  820,  322,  847,  840,  478,  845,
 /*    40 */   839,  779,  420,  631,  117,  115,  114,  317,  323,   25,
 /*    50 */    24,   64,  642,   26,  336,  771,  768,  767,  766,  765,
 /*    60 */   693,  122,  361,  247,  778,  777,   62,   18,  480,  820,
 /*    70 */   322,  847,  840,  846,  845,  839,  150,  148,  146,  145,
 /*    80 */   144,   59,  317,  323,  241,  239,  237,  236,  235,  366,
 /*    90 */   771,  768,  767,  766,  765,  469,  517,  471,  694,  695,
 /*   100 */   558,  589,  588,  587,  586,  585,  584,  583,  582,  581,
 /*   110 */   580,  579,  578,  577,  576,  575,  574,  573,  572,  819,
 /*   120 */   547,  659,   61,  548,  818,  557,  556,  553,  552,  555,
 /*   130 */   554,  841,  174,  172,  170,  168,  167,  638,  537,  847,
 /*   140 */   840,  846,  845,  839,   46,  101,   12,  297,  202,  203,
 /*   150 */    44,   30,   10,   11,  300,  193,  773,  268,   45,  166,
 /*   160 */   607,  122,  262,  693,   26,  361,  792,  791,  793,    9,
 /*   170 */   500,  499,    8,  312,   43,  254,  326,  611,  608,  606,
 /*   180 */   605,  775,  776,  713,  692,   47,  324,  186,  724,   48,
 /*   190 */   170,  168,  167,  309,  216,  215,  214,   13,  467,  517,
 /*   200 */   471,  694,  695,  545,  689,  688,  687,  686,  292,  637,
 /*   210 */   547,  100,  515,  548,  636,  121,  119,  117,  115,  114,
 /*   220 */   685,   99,  683,  125,  544,   53,   52,  682,   98,   54,
 /*   230 */   708,  707,  709,  637,  547,  715,  551,  548,  636,  681,
 /*   240 */   679,  680,  684,  760,  759,  710,  711,  537,  550,  838,
 /*   250 */   549,   20,  192,  221,  501,  211,  615,  614,  502,  834,
 /*   260 */   843,  844,  847,  840,  846,  845,  839,  485,  419,  774,
 /*   270 */   657,  603,  604,  651,  200,  537,  599,    6,  343,   42,
 /*   280 */   615,  614,  616,  309,  819,  547,  536,  219,  548,  818,
 /*   290 */   194,  273,  217,  194,   73,  603,  604,  753,  200,  195,
 /*   300 */   752,    6,  195,   42,  473,  675,  472,  309,   22,   21,
 /*   310 */    25,   24,   40,   41,   26,   38,   37,   72,  132,   39,
 /*   320 */    60,  820,  322,  847,  840,  846,  845,  839,  237,  236,
 /*   330 */   235,  792,  791,  793,  315,  323,   40,   41,  550,  838,
 /*   340 */   549,  691,  132,  779,  774,   71,  763,  764,  669,  207,
 /*   350 */   361,  796,    3,   74,   29,  310,  819,  547,  309,   54,
 /*   360 */   548,  818,  550,  838,  549,  479,  778,  777,  545,   23,
 /*   370 */    22,   21,   25,   24,  649,  245,   26,  820,  322,  847,
 /*   380 */   840,  478,  845,  839,  693,   67,  361,   27,   28,  655,
 /*   390 */   317,  323,  730,   98,  673,  463,  668,  338,  771,  768,
 /*   400 */   767,  766,  765,  792,  791,  793,  146,  145,  144,  729,
 /*   410 */    19,  480,  762,  550,  838,  549,   30,  728,  775,  776,
 /*   420 */   522,  471,  694,  695,  186,   66,   48,  521,  520,  837,
 /*   430 */   309,  656,  638,  537,  847,  840,  846,  845,  839,   23,
 /*   440 */    22,   21,   25,   24,  242,  198,   26,  212,  181,    2,
 /*   450 */   189,  196,  197,  234,   15,  607,  760,  759,  243,  122,
 /*   460 */   125,  545,  833,  547,   33,   98,  548,  832,  313,  202,
 /*   470 */   203,  342,  611,  608,  606,  605,   47, 1085,  240,  453,
 /*   480 */   454, 1085,  542,  238,  309,  550,  838,  549,  634,  625,
 /*   490 */   847,  840,  846,  845,  839,  638,  537,  847,  840,  478,
 /*   500 */   845,  839,  459,  455,  508,  451,  593,  592,  641,  827,
 /*   510 */   826,  828,  750,  547,  274,  727,  548,  749,  607,  461,
 /*   520 */   662,  460,  701,  424,  829,  830,  700,   65,   31,  480,
 /*   530 */   726,  313,  120,  836,  328,  611,  608,  606,  605,  550,
 /*   540 */   241,  239,  237,  236,  235,  638,  537,  847,  840,  478,
 /*   550 */   845,  839,  812,  803,  847,  840,  846,  845,  839,  743,
 /*   560 */   742,  744,  481,  409,  188,  831,  118,  538,  607,  524,
 /*   570 */   205,  116,  506,  835,  733,  734,  674,  725,   32,  480,
 /*   580 */   311,  313,   57,  722,  330,  611,  608,  606,  605,  465,
 /*   590 */   511,  550,  838,  549,    7,  523,  185,  635,  121,  119,
 /*   600 */   117,  115,  114,  122,  717,   14,  139,  138,  137,  505,
 /*   610 */   136,  135,  134,  133,  452,   55,   56,  184,  545,  602,
 /*   620 */   311,  153,  638,  537,  847,  840,  846,  845,  839,  245,
 /*   630 */   152,  143,  142,  141,  140,  546,  708,  707,  709,  655,
 /*   640 */   206,  550,  838,  549,  813,  607,   36,   35,   34,   38,
 /*   650 */    37,  710,  711,   39,  713,  692,  518,  311,  313,  221,
 /*   660 */   666,  341,  611,  608,  606,  605,  811,  547,  696,  697,
 /*   670 */   548,  810,  633,  547,  175,  546,  548,  632,  833,  547,
 /*   680 */   210,  654,  548,  832,  242,  519,  820,  322,  847,  840,
 /*   690 */   846,  845,  839,  219,  179,  550,  597,  596,  217,  314,
 /*   700 */   323,  708,  707,  709,  311,  678,  334,  771,  768,  767,
 /*   710 */   766,  765,  546,  805,  804,  806,  710,  711,  240,  627,
 /*   720 */   626,  628,  182,  238,  221,  827,  826,  828,  807,  808,
 /*   730 */   693,  669,  361,  361,  629,  630,  173,  595,  594,  245,
 /*   740 */   829,  830,  151,  121,  119,  117,  115,  114,  120,  638,
 /*   750 */   537,  847,  840,  846,  845,  839,  537,  180,  219,  546,
 /*   760 */   838,  719,  720,  217,  755,  466,  517,  471,  694,  695,
 /*   770 */   171,  513,  607,  661,  178,  169,  149,  177,  774,  671,
 /*   780 */   525,  147,  118,  509,  669,  613,  361,  116,  601,  611,
 /*   790 */   608,  606,  605,  204,  122,  550,  838,  549,  690,  699,
 /*   800 */   486,  550,  838,  549,  213,  176,  232,  550,  838,  549,
 /*   810 */   250,  233,  664,  820,  321,  847,  840,  846,  845,  839,
 /*   820 */   220,  218,  216,  215,  214,   70,  772,  323,  183,  464,
 /*   830 */   667,  463,  668,  757,  771,  768,  767,  766,  765,  507,
 /*   840 */  1262,  820,  322,  847,  840,  846,  845,  839,  677,   23,
 /*   850 */    22,   21,   25,   24,  317,  323,   26,  126, 1224,  291,
 /*   860 */  1262,  770,  771,  768,  767,  766,  765,  598,  820,  322,
 /*   870 */   847,  840,  846,  845,  839,  476,  716,  660,  183,   69,
 /*   880 */   653,  317,  323,  514, 1224,  191,  650,    5,  769,  771,
 /*   890 */   768,  767,  766,  765,  676,  820,  322,  847,  840,  846,
 /*   900 */   845,  839,  670,   23,   22,   21,   25,   24,  317,  323,
 /*   910 */    26,  756,  714,  472,  199,  540,  771,  768,  767,  766,
 /*   920 */   765,   39,  665,  460,  183,  820,  322,  847,  840,  846,
 /*   930 */   845,  839,  183,   23,   22,   21,   25,   24,  317,  323,
 /*   940 */    26,  246,  307, 1161,  538,  539,  771,  768,  767,  766,
 /*   950 */   765,  820,  322,  847,  840,  846,  845,  839, 1161, 1161,
 /*   960 */   450,  693,  591,  361,  317,  323,  760,  759,  590,  781,
 /*   970 */   571,  387,  771,  768,  767,  766,  765,  570, 1161, 1161,
 /*   980 */   754,  820,  322,  847,  840,  846,  845,  839,  569, 1161,
 /*   990 */  1161,  809,   30,  658,  317,  323, 1161,  470,  471,  694,
 /*  1000 */   695,  439,  771,  768,  767,  766,  765,  474,  718,  820,
 /*  1010 */   322,  847,  840,  846,  845,  839,  458,  508, 1161,  568,
 /*  1020 */    68,  567,  317,  323,  174,  172,  170,  168,  167,  438,
 /*  1030 */   771,  768,  767,  766,  765,  566,  820,  322,  847,  840,
 /*  1040 */   846,  845,  839,  565,   23,   22,   21,   25,   24,  317,
 /*  1050 */   323,   26,   47,  693,  563,  361,  339,  771,  768,  767,
 /*  1060 */   766,  765,  201,  820,  322,  847,  840,  846,  845,  839,
 /*  1070 */   457,  508,  550,  562,   30,  561,  317,  323,  560,  731,
 /*  1080 */   456,  508,  559,  337,  771,  768,  767,  766,  765,  516,
 /*  1090 */   471,  694,  695,  820,  322,  847,  840,  846,  845,  839,
 /*  1100 */   127,   51,   50,   49,   53,   52,  317,  323,   54,  431,
 /*  1110 */   508,  244,  307,  335,  771,  768,  767,  766,  765,  820,
 /*  1120 */   322,  847,  840,  846,  845,  839,  797,   23,   22,   21,
 /*  1130 */    25,   24,  317,  323,   26, 1320,    1,  795,  546,  365,
 /*  1140 */   771,  768,  767,  766,  765,  781,  838,  550,  231,  820,
 /*  1150 */   322,  847,  840,  846,  845,  839,  761,  693,  305,  361,
 /*  1160 */   304,  758,  317,  323,  241,  239,  237,  236,  235,  364,
 /*  1170 */   771,  768,  767,  766,  765,   16,   17,  820,  322,  847,
 /*  1180 */   840,  846,  845,  839,  693,  190,  361,  303,  131,  533,
 /*  1190 */   317,  323,  462,  517,  471,  694,  695,  228,  771,  768,
 /*  1200 */   767,  766,  765,  301,  820,  322,  847,  840,  846,  845,
 /*  1210 */   839,  531,   36,   35,   34,   38,   37,  319,  323,   39,
 /*  1220 */   468,  471,  694,  695,  355,  771,  768,  767,  766,  765,
 /*  1230 */    58,  820,  322,  847,  840,  846,  845,  839,  530,  529,
 /*  1240 */   298,  528,  723,  817,  317,  323,  174,  172,  170,  168,
 /*  1250 */   167,  227,  771,  768,  767,  766,  765,  527,  295,  293,
 /*  1260 */   475,  820,  322,  847,  840,  846,  845,  839,  692,  526,
 /*  1270 */   672,  130,  289,  288,  317,  323,  113,  112,  111,  110,
 /*  1280 */   109,  226,  771,  768,  767,  766,  765,  820,  322,  847,
 /*  1290 */   840,  846,  845,  839,  287,   36,   35,   34,   38,   37,
 /*  1300 */   317,  323,   39,  498,  285,  284,  283,  225,  771,  768,
 /*  1310 */   767,  766,  765,  497,  281,  564,  495,  820,  322,  847,
 /*  1320 */   840,  846,  845,  839,  279,   30,  494,  712,  277,  275,
 /*  1330 */   317,  323,   92,   91,   90,   89,   88,  224,  771,  768,
 /*  1340 */   767,  766,  765,  493,  492,  820,  322,  847,  840,  846,
 /*  1350 */   845,  839,   97,   96,   95,   94,   93,  272,  317,  323,
 /*  1360 */   220,  218,  216,  215,  214,  223,  771,  768,  767,  766,
 /*  1370 */   765,  271,  820,  322,  847,  840,  846,  845,  839,  270,
 /*  1380 */   220,  218,  216,  215,  214,  317,  323,  187,  269,  267,
 /*  1390 */   817,  491,  222,  771,  768,  767,  766,  765,  266,  108,
 /*  1400 */   107,  106,  265,  105,  104,  103,  102,  264,  638,  537,
 /*  1410 */   847,  840,  846,  845,  839,  263,    4,  490,  261,  817,
 /*  1420 */   260,  259,  258,  113,  112,  111,  110,  109,  108,  107,
 /*  1430 */   106,  607,  105,  104,  103,  102,  834,  843,  844,  847,
 /*  1440 */   840,  846,  845,  839,  313,  418,    7,  610,  611,  608,
 /*  1450 */   606,  605,  113,  112,  111,  110,  109,   14,  139,  138,
 /*  1460 */   137,  489,  136,  135,  134,  133,  820,  322,  847,  840,
 /*  1470 */   846,  845,  839,  257,  255,  253,  129,  488,  487,  318,
 /*  1480 */   323,  302,  152,  143,  142,  141,  140,  294,  779,  774,
 /*  1490 */   721,  290,  820,  322,  847,  840,  846,  845,  839,  286,
 /*  1500 */    36,   35,   34,   38,   37,  318,  323,   39,  306,  308,
 /*  1510 */   780,  778,  777,  698,  779,  774,  359,  360,  427,  647,
 /*  1520 */   820,  322,  847,  840,  846,  845,  839,  428,  646,  128,
 /*  1530 */   645,  644,  643,  318,  323,  358,  252,  778,  777,  299,
 /*  1540 */   357,  356,  779,  774,  370,  820,  322,  847,  840,  846,
 /*  1550 */   845,  839,  296,   36,   35,   34,   38,   37,  318,  323,
 /*  1560 */    39,  532,  496,  282,  251,  778,  777,  779,  774,  280,
 /*  1570 */   256,  820,  322,  847,  840,  846,  845,  839,  512,   23,
 /*  1580 */    22,   21,   25,   24,  318,  323,   26,  362,  432,  249,
 /*  1590 */   778,  777,  278,  779,  774,  693,  276,  361,  706,  638,
 /*  1600 */   537,  847,  840,  846,  845,  839,  433,  638,  537,  847,
 /*  1610 */   840,  846,  845,  839,  705,  248,  778,  777,  704,  703,
 /*  1620 */   702,  373,  607,  820,  325,  847,  840,  846,  845,  839,
 /*  1630 */   607,  510,  471,  694,  695,  313,  790,  405,  609,  611,
 /*  1640 */   608,  606,  605,  313,  663, 1321,  504,  611,  608,  606,
 /*  1650 */   605,  638,  537,  847,  840,  846,  845,  839, 1321, 1321,
 /*  1660 */   638,  537,  847,  840,  846,  845,  839,   50,   49,   53,
 /*  1670 */    52, 1321, 1321,   54,  607,  638,  537,  847,  840,  846,
 /*  1680 */   845,  839, 1321,  607, 1321, 1321, 1321,  313, 1321, 1321,
 /*  1690 */   503,  611,  608,  606,  605, 1321,  313, 1321,  607,  371,
 /*  1700 */   611,  608,  606,  605,  638,  537,  847,  840,  846,  845,
 /*  1710 */   839,  313, 1321, 1321,  422,  611,  608,  606,  605,  638,
 /*  1720 */   537,  847,  840,  846,  845,  839, 1321,  607,  638,  537,
 /*  1730 */   847,  840,  846,  845,  839,   35,   34,   38,   37, 1321,
 /*  1740 */   313,   39,  607,  421,  611,  608,  606,  605, 1321, 1321,
 /*  1750 */    58,  607, 1321, 1321, 1321,  313, 1321, 1321,  331,  611,
 /*  1760 */   608,  606,  605, 1321,  313, 1321, 1321,  329,  611,  608,
 /*  1770 */   606,  605,  638,  537,  847,  840,  846,  845,  839, 1321,
 /*  1780 */   187, 1321,   23,   22,   21,   25,   24, 1321,    4,   26,
 /*  1790 */  1321, 1321,  108,  107,  106,  607,  105,  104,  103,  102,
 /*  1800 */   108,  107,  106,  640,  105,  104,  103,  102,  313, 1321,
 /*  1810 */  1321,  327,  611,  608,  606,  605,  113,  112,  111,  110,
 /*  1820 */   109, 1321, 1321, 1321,  113,  112,  111,  110,  109,  819,
 /*  1830 */   547,  748,  545,  548,  818,  241,  239,  237,  236,  235,
 /*  1840 */   160,  159,  158, 1321,  157,  156,  155,  154,  241,  239,
 /*  1850 */   237,  236,  235,  655, 1321,   87,   86,   85, 1321,   84,
 /*  1860 */    83,   82,   81, 1321,  165,  164,  163,  162,  161,  634,
 /*  1870 */   625,  847,  840,  846,  845,  839,  792,  791,  793,   92,
 /*  1880 */    91,   90,   89,   88,   74,   80,   79,  794,   78,   77,
 /*  1890 */    76,   75,  545, 1321,  430,  652,  932,  932,  932,  124,
 /*  1900 */   932,  932,  932,  932,  332, 1321, 1321, 1321,   97,   96,
 /*  1910 */    95,   94,   93,  543,  545, 1321,  209,  480, 1321, 1321,
 /*  1920 */   932,  932,  932,  932,  932,  834,  843,  844,  847,  840,
 /*  1930 */   846,  845,  839,  123,  448,  655, 1321, 1321,   98,  160,
 /*  1940 */   159,  158, 1321,  157,  156,  155,  154,  108,  107,  106,
 /*  1950 */  1321,  105,  104,  103,  102, 1321, 1321, 1321,  550,  838,
 /*  1960 */   549, 1321, 1321,  165,  164,  163,  162,  161, 1321, 1321,
 /*  1970 */  1321,  113,  112,  111,  110,  109,  429,  652, 1321,  751,
 /*  1980 */   741,  847,  840,  846,  845,  839,  751,  741,  847,  840,
 /*  1990 */   846,  845,  839, 1321,  320, 1321,   51,   50,   49,   53,
 /*  2000 */    52,  316,   16,   54, 1321, 1321,  363,  738,  735, 1321,
 /*  2010 */  1321,  600, 1321,  333,  738,  735,  751,  741,  847,  840,
 /*  2020 */   846,  845,  839,  751,  741,  847,  840,  846,  845,  839,
 /*  2030 */  1321,  740, 1321,   36,   35,   34,   38,   37,  320, 1321,
 /*  2040 */    39, 1321, 1321,  732,  738,  735, 1321, 1321, 1321, 1321,
 /*  2050 */   737,  738,  735,  751,  741,  847,  840,  846,  845,  839,
 /*  2060 */    36,   35,   34,   38,   37, 1321, 1321,   39,  320, 1321,
 /*  2070 */    23,   22,   21,   25,   24, 1321, 1321,   26, 1321,  794,
 /*  2080 */   736,  738,  735, 1321,  545, 1321, 1321, 1321,  751,  741,
 /*  2090 */   847,  840,  846,  845,  839,  751,  741,  847,  840,  846,
 /*  2100 */   845,  839, 1321,  320, 1321,  541, 1321, 1321,  208,  480,
 /*  2110 */   320, 1321, 1321, 1321, 1321,  535,  738,  735, 1321, 1321,
 /*  2120 */  1321, 1321,  534,  738,  735, 1321,  751,  741,  847,  840,
 /*  2130 */   846,  845,  839, 1321,  751,  741,  847,  840,  846,  845,
 /*  2140 */   839,  320,  751,  741,  847,  840,  846,  845,  839,  320,
 /*  2150 */  1321, 1321, 1321,  374,  738,  735, 1321,  320, 1321, 1321,
 /*  2160 */  1321,  435,  738,  735, 1321, 1321, 1321, 1321, 1321,  434,
 /*  2170 */   738,  735,  834,  843,  844,  847,  840,  846,  845,  839,
 /*  2180 */   484,  419,  834,  843,  844,  847,  840,  846,  845,  839,
 /*  2190 */   482,  419,  834,  843,  844,  847,  840,  846,  845,  839,
 /*  2200 */   477,  419,  713, 1321,  834,  843,  844,  847,  840,  846,
 /*  2210 */   845,  839,  483,  419,  820,  404,  847,  840,  846,  845,
 /*  2220 */   839, 1321, 1321, 1321, 1321, 1321, 1321,  367,  405, 1321,
 /*  2230 */  1321, 1321, 1321, 1321,  834,  843,  844,  847,  840,  846,
 /*  2240 */   845,  839, 1321,  369, 1321, 1321, 1321, 1321, 1321,  708,
 /*  2250 */   707,  709, 1321,  820,  788,  847,  840,  846,  845,  839,
 /*  2260 */  1321, 1321, 1321, 1321,  710,  711,  790,  405, 1321, 1321,
 /*  2270 */  1321, 1321,  221, 1321, 1321, 1321,  834,  843,  844,  847,
 /*  2280 */   840,  846,  845,  839, 1321,  449,  834,  843,  844,  847,
 /*  2290 */   840,  846,  845,  839, 1321,  825,  834,  843,  844,  847,
 /*  2300 */   840,  846,  845,  839, 1321,  821,  219, 1321, 1321, 1321,
 /*  2310 */  1321,  217, 1321, 1321, 1321,  834,  843,  844,  847,  840,
 /*  2320 */   846,  845,  839, 1321,  824,  834,  843,  844,  847,  840,
 /*  2330 */   846,  845,  839, 1321,  823,  834,  843,  844,  847,  840,
 /*  2340 */   846,  845,  839, 1321,  822,  834,  843,  844,  847,  840,
 /*  2350 */   846,  845,  839, 1321,  447,  834,  843,  844,  847,  840,
 /*  2360 */   846,  845,  839, 1321,  446,  834,  843,  844,  847,  840,
 /*  2370 */   846,  845,  839, 1321,  816, 1321, 1321, 1321, 1321, 1321,
 /*  2380 */  1321,  834,  843,  844,  847,  840,  846,  845,  839, 1321,
 /*  2390 */   815,  834,  843,  844,  847,  840,  846,  845,  839, 1321,
 /*  2400 */   814, 1321, 1321,  834,  843,  844,  847,  840,  846,  845,
 /*  2410 */   839, 1321,  417,  834,  843,  844,  847,  840,  846,  845,
 /*  2420 */   839, 1321,  416, 1321, 1321, 1321,  834,  843,  844,  847,
 /*  2430 */   840,  846,  845,  839, 1321,  415,  834,  843,  844,  847,
 /*  2440 */   840,  846,  845,  839, 1321,  414,  834,  843,  844,  847,
 /*  2450 */   840,  846,  845,  839, 1321,  413,  834,  843,  844,  847,
 /*  2460 */   840,  846,  845,  839, 1321,  412, 1321, 1321, 1321,  834,
 /*  2470 */   843,  844,  847,  840,  846,  845,  839, 1321,  411,  834,
 /*  2480 */   843,  844,  847,  840,  846,  845,  839, 1321,  407,  834,
 /*  2490 */   843,  844,  847,  840,  846,  845,  839, 1321,  406,  834,
 /*  2500 */   843,  844,  847,  840,  846,  845,  839, 1321,  789,  834,
 /*  2510 */   843,  844,  847,  840,  846,  845,  839, 1321,  443,  834,
 /*  2520 */   843,  844,  847,  840,  846,  845,  839, 1321,  442, 1321,
 /*  2530 */  1321, 1321, 1321, 1321, 1321,  834,  843,  844,  847,  840,
 /*  2540 */   846,  845,  839, 1321,  787,  834,  843,  844,  847,  840,
 /*  2550 */   846,  845,  839, 1321,  786, 1321, 1321,  834,  843,  844,
 /*  2560 */   847,  840,  846,  845,  839, 1321,  785,  834,  843,  844,
 /*  2570 */   847,  840,  846,  845,  839, 1321,  441, 1321, 1321, 1321,
 /*  2580 */   834,  843,  844,  847,  840,  846,  845,  839, 1321,  440,
 /*  2590 */   834,  843,  844,  847,  840,  846,  845,  839, 1321,  784,
 /*  2600 */   834,  843,  844,  847,  840,  846,  845,  839, 1321,  783,
 /*  2610 */   834,  843,  844,  847,  840,  846,  845,  839, 1321,  782,
 /*  2620 */  1321, 1321, 1321,  834,  843,  844,  847,  840,  846,  845,
 /*  2630 */   839, 1321,  403,  834,  843,  844,  847,  840,  846,  845,
 /*  2640 */   839, 1321,  402,  834,  843,  844,  847,  840,  846,  845,
 /*  2650 */   839, 1321,  401,  834,  843,  844,  847,  840,  846,  845,
 /*  2660 */   839, 1321,  400,  834,  843,  844,  847,  840,  846,  845,
 /*  2670 */   839, 1321,  399,  834,  843,  844,  847,  840,  846,  845,
 /*  2680 */   839, 1321,  398, 1321, 1321, 1321, 1321, 1321, 1321,  834,
 /*  2690 */   843,  844,  847,  840,  846,  845,  839, 1321,  397,  834,
 /*  2700 */   843,  844,  847,  840,  846,  845,  839, 1321,  396, 1321,
 /*  2710 */  1321,  834,  843,  844,  847,  840,  846,  845,  839, 1321,
 /*  2720 */   395,  834,  843,  844,  847,  840,  846,  845,  839, 1321,
 /*  2730 */   394, 1321, 1321, 1321,  834,  843,  844,  847,  840,  846,
 /*  2740 */   845,  839, 1321,  393,  834,  843,  844,  847,  840,  846,
 /*  2750 */   845,  839, 1321,  392,  834,  843,  844,  847,  840,  846,
 /*  2760 */   845,  839, 1321,  391,  834,  843,  844,  847,  840,  846,
 /*  2770 */   845,  839, 1321,  390, 1321, 1321, 1321,  834,  843,  844,
 /*  2780 */   847,  840,  846,  845,  839, 1321,  389,  834,  843,  844,
 /*  2790 */   847,  840,  846,  845,  839, 1321,  388,  834,  843,  844,
 /*  2800 */   847,  840,  846,  845,  839, 1321,  386,  834,  843,  844,
 /*  2810 */   847,  840,  846,  845,  839, 1321,  385,  834,  843,  844,
 /*  2820 */   847,  840,  846,  845,  839, 1321,  384,  834,  843,  844,
 /*  2830 */   847,  840,  846,  845,  839, 1321,  383, 1321, 1321, 1321,
 /*  2840 */  1321, 1321, 1321,  834,  843,  844,  847,  840,  846,  845,
 /*  2850 */   839, 1321,  382,  834,  843,  844,  847,  840,  846,  845,
 /*  2860 */   839, 1321,  230, 1321, 1321,  834,  843,  844,  847,  840,
 /*  2870 */   846,  845,  839, 1321,  229,  834,  843,  844,  847,  840,
 /*  2880 */   846,  845,  839, 1321,  372,  812,  803,  847,  840,  846,
 /*  2890 */   845,  839, 1321, 1321, 1321, 1321,  410, 1321,  812,  803,
 /*  2900 */   847,  840,  846,  845,  839, 1321, 1321, 1321, 1321,  444,
 /*  2910 */   812,  803,  847,  840,  846,  845,  839, 1321, 1321, 1321,
 /*  2920 */  1321,  368,  812,  803,  847,  840,  846,  845,  839, 1321,
 /*  2930 */  1321, 1321, 1321,  445,  812,  803,  847,  840,  846,  845,
 /*  2940 */   839, 1321, 1321, 1321, 1321,  802,  812,  803,  847,  840,
 /*  2950 */   846,  845,  839, 1321, 1321, 1321, 1321,  798,  812,  803,
 /*  2960 */   847,  840,  846,  845,  839, 1321, 1321, 1321, 1321,  801,
 /*  2970 */  1321,  812,  803,  847,  840,  846,  845,  839, 1321, 1321,
 /*  2980 */  1321, 1321,  800,  812,  803,  847,  840,  846,  845,  839,
 /*  2990 */  1321, 1321, 1321, 1321,  799, 1321, 1321, 1321,  812,  803,
 /*  3000 */   847,  840,  846,  845,  839, 1321, 1321, 1321, 1321,  408,
 /*  3010 */   812,  803,  847,  840,  846,  845,  839, 1321, 1321, 1321,
 /*  3020 */  1321,  437,  812,  803,  847,  840,  846,  845,  839, 1321,
 /*  3030 */  1321, 1321, 1321,  436,  812,  803,  847,  840,  846,  845,
 /*  3040 */   839, 1321, 1321, 1321, 1321,  747,  812,  803,  847,  840,
 /*  3050 */   846,  845,  839, 1321, 1321, 1321, 1321,  746,  812,  803,
 /*  3060 */   847,  840,  846,  845,  839, 1321, 1321, 1321, 1321,  745,
 /*  3070 */   812,  803,  847,  840,  846,  845,  839, 1321, 1321, 1321,
 /*  3080 */  1321,  381,  812,  803,  847,  840,  846,  845,  839, 1321,
 /*  3090 */  1321, 1321, 1321,  380,  812,  803,  847,  840,  846,  845,
 /*  3100 */   839, 1321, 1321, 1321, 1321,  379,  812,  803,  847,  840,
 /*  3110 */   846,  845,  839, 1321, 1321, 1321, 1321,  378,  812,  803,
 /*  3120 */   847,  840,  846,  845,  839, 1321, 1321, 1321, 1321,  377,
 /*  3130 */   812,  803,  847,  840,  846,  845,  839, 1321, 1321, 1321,
 /*  3140 */  1321,  376,  812,  803,  847,  840,  846,  845,  839, 1321,
 /*  3150 */  1321, 1321, 1321,  375,  812,  803,  847,  840,  846,  845,
 /*  3160 */   839, 1321, 1321, 1321, 1321,  739,  634,  625,  847,  840,
 /*  3170 */   846,  845,  839, 1321, 1321, 1321,  634,  625,  847,  840,
 /*  3180 */   846,  845,  839,  634,  625,  847,  840,  846,  845,  839,
 /*  3190 */   634,  625,  847,  840,  846,  845,  839, 1321, 1321, 1321,
 /*  3200 */  1321,  425,  634,  625,  847,  840,  846,  845,  839, 1321,
 /*  3210 */  1321,  624, 1321, 1321, 1321, 1321, 1321, 1321,  426, 1321,
 /*  3220 */  1321, 1321, 1321, 1321, 1321,  623,  634,  625,  847,  840,
 /*  3230 */   846,  845,  839, 1321, 1321, 1321, 1321,  622,  634,  625,
 /*  3240 */   847,  840,  846,  845,  839, 1321, 1321, 1321,  634,  625,
 /*  3250 */   847,  840,  846,  845,  839, 1321, 1321, 1321,  648, 1321,
 /*  3260 */  1321,  621,  634,  625,  847,  840,  846,  845,  839, 1321,
 /*  3270 */  1321, 1321, 1321,  620,  634,  625,  847,  840,  846,  845,
 /*  3280 */   839, 1321, 1321,  423,  634,  625,  847,  840,  846,  845,
 /*  3290 */   839,  241,  239,  237,  236,  235, 1321,  619,  634,  625,
 /*  3300 */   847,  840,  846,  845,  839, 1321, 1321, 1321, 1321,  618,
 /*  3310 */   634,  625,  847,  840,  846,  845,  839, 1321, 1321,  617,
 /*  3320 */   634,  625,  847,  840,  846,  845,  839, 1321, 1321, 1321,
 /*  3330 */  1321, 1321,  639,  354,  634,  625,  847,  840,  846,  845,
 /*  3340 */   839, 1321, 1321, 1321, 1321,  353,  634,  625,  847,  840,
 /*  3350 */   846,  845,  839, 1321, 1321,  352,  634,  625,  847,  840,
 /*  3360 */   846,  845,  839, 1321,  241,  239,  237,  236,  235,  351,
 /*  3370 */  1321, 1321, 1321,  634,  625,  847,  840,  846,  845,  839,
 /*  3380 */  1321,  350,  634,  625,  847,  840,  846,  845,  839, 1321,
 /*  3390 */  1321,  349,  634,  625,  847,  840,  846,  845,  839,  634,
 /*  3400 */   625,  847,  840,  846,  845,  839, 1321, 1321,  348,  634,
 /*  3410 */   625,  847,  840,  846,  845,  839, 1321,  612,  634,  625,
 /*  3420 */   847,  840,  846,  845,  839, 1321, 1321,  347, 1321, 1321,
 /*  3430 */  1321, 1321, 1321, 1321,  346,  812,  803,  847,  840,  846,
 /*  3440 */   845,  839, 1321, 1321,  345, 1321,  340, 1321, 1321, 1321,
 /*  3450 */  1321, 1321, 1321,  344,
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
 /*   410 */   187,  188,   67,  130,  131,  132,   41,   73,   63,   64,
 /*   420 */   206,  207,  208,  209,   69,   31,   71,  213,  214,   72,
 /*   430 */    75,  226,  154,  155,  156,  157,  158,  159,  160,   94,
 /*   440 */    95,   96,   97,   98,   71,   14,  101,   16,   17,   18,
 /*   450 */    19,   20,   21,   76,   79,  177,   90,   91,   81,  102,
 /*   460 */   105,  163,    1,    2,   98,  110,    5,    6,  190,   99,
 /*   470 */   100,  193,  194,  195,  196,  197,  101,   98,  105,   48,
 /*   480 */    49,  102,  184,  110,   75,  130,  131,  132,  154,  155,
 /*   490 */   156,  157,  158,  159,  160,  154,  155,  156,  157,  158,
 /*   500 */   159,  160,  222,  223,  224,  227,  228,  229,   73,   48,
 /*   510 */    49,   50,    1,    2,  105,   73,    5,    6,  177,  219,
 /*   520 */   220,  221,  107,  189,   63,   64,  111,   31,  187,  188,
 /*   530 */    73,  190,   71,   72,  193,  194,  195,  196,  197,  130,
 /*   540 */   105,  106,  107,  108,  109,  154,  155,  156,  157,  158,
 /*   550 */   159,  160,  154,  155,  156,  157,  158,  159,  160,   48,
 /*   560 */    49,   50,  164,  165,  133,   72,  105,    2,  177,   96,
 /*   570 */    71,  110,   27,   72,   63,   64,   73,   73,  187,  188,
 /*   580 */    75,  190,   71,   74,  193,  194,  195,  196,  197,    1,
 /*   590 */     2,  130,  131,  132,   69,   96,   93,   72,  105,  106,
 /*   600 */   107,  108,  109,  102,   74,   80,   81,   82,   83,   27,
 /*   610 */    85,   86,   87,   88,    1,  104,  105,   71,  163,   67,
 /*   620 */    75,  110,  154,  155,  156,  157,  158,  159,  160,  106,
 /*   630 */   105,  106,  107,  108,  109,  130,   48,   49,   50,  184,
 /*   640 */    75,  130,  131,  132,   72,  177,   94,   95,   96,   97,
 /*   650 */    98,   63,   64,  101,    1,    2,    2,   75,  190,   71,
 /*   660 */    73,  193,  194,  195,  196,  197,    1,    2,    1,    2,
 /*   670 */     5,    6,    1,    2,  102,  130,    5,    6,    1,    2,
 /*   680 */    93,  226,    5,    6,   71,   55,  154,  155,  156,  157,
 /*   690 */   158,  159,  160,  105,   71,  130,  228,  229,  110,  167,
 /*   700 */   168,   48,   49,   50,   75,   72,  174,  175,  176,  177,
 /*   710 */   178,  179,  130,   48,   49,   50,   63,   64,  105,   48,
 /*   720 */    49,   50,   71,  110,   71,   48,   49,   50,   63,   64,
 /*   730 */   170,  170,  172,  172,   63,   64,   71,    1,    2,  106,
 /*   740 */    63,   64,   71,  105,  106,  107,  108,  109,   71,  154,
 /*   750 */   155,  156,  157,  158,  159,  160,  155,   76,  105,  130,
 /*   760 */   131,    3,    4,  110,   72,  205,  206,  207,  208,  209,
 /*   770 */   105,    2,  177,   73,   71,  110,  105,   71,  177,  218,
 /*   780 */    72,  110,  105,  102,  170,  190,  172,  110,  193,  194,
 /*   790 */   195,  196,  197,   93,  102,  130,  131,  132,   72,  132,
 /*   800 */   199,  130,  131,  132,   89,   71,   76,  130,  131,  132,
 /*   810 */   102,   81,    2,  154,  155,  156,  157,  158,  159,  160,
 /*   820 */   105,  106,  107,  108,  109,   70,  167,  168,  102,  215,
 /*   830 */   216,  217,  218,  174,  175,  176,  177,  178,  179,  102,
 /*   840 */    73,  154,  155,  156,  157,  158,  159,  160,   72,   94,
 /*   850 */    95,   96,   97,   98,  167,  168,  101,   76,   76,   76,
 /*   860 */    93,  174,  175,  176,  177,  178,  179,   73,  154,  155,
 /*   870 */   156,  157,  158,  159,  160,  201,  202,    2,  102,   70,
 /*   880 */    73,  167,  168,  102,  102,  102,   73,   93,  174,  175,
 /*   890 */   176,  177,  178,  179,   72,  154,  155,  156,  157,  158,
 /*   900 */   159,  160,   72,   94,   95,   96,   97,   98,  167,  168,
 /*   910 */   101,   72,  211,  212,   76,  174,  175,  176,  177,  178,
 /*   920 */   179,  101,  220,  221,  102,  154,  155,  156,  157,  158,
 /*   930 */   159,  160,  102,   94,   95,   96,   97,   98,  167,  168,
 /*   940 */   101,  185,  186,   26,    2,  174,  175,  176,  177,  178,
 /*   950 */   179,  154,  155,  156,  157,  158,  159,  160,   41,   42,
 /*   960 */    76,  170,   73,  172,  167,  168,   90,   91,   73,  155,
 /*   970 */    73,  174,  175,  176,  177,  178,  179,   73,   61,   62,
 /*   980 */   166,  154,  155,  156,  157,  158,  159,  160,   73,   72,
 /*   990 */    73,   72,   41,    2,  167,  168,   79,  206,  207,  208,
 /*  1000 */   209,  174,  175,  176,  177,  178,  179,  203,  204,  154,
 /*  1010 */   155,  156,  157,  158,  159,  160,  223,  224,  101,   73,
 /*  1020 */    70,   73,  167,  168,  105,  106,  107,  108,  109,  174,
 /*  1030 */   175,  176,  177,  178,  179,   73,  154,  155,  156,  157,
 /*  1040 */   158,  159,  160,   73,   94,   95,   96,   97,   98,  167,
 /*  1050 */   168,  101,  101,  170,   73,  172,  174,  175,  176,  177,
 /*  1060 */   178,  179,   71,  154,  155,  156,  157,  158,  159,  160,
 /*  1070 */   223,  224,  130,   73,   41,   73,  167,  168,   73,   72,
 /*  1080 */   223,  224,   73,  174,  175,  176,  177,  178,  179,  206,
 /*  1090 */   207,  208,  209,  154,  155,  156,  157,  158,  159,  160,
 /*  1100 */    65,   94,   95,   96,   97,   98,  167,  168,  101,  223,
 /*  1110 */   224,  185,  186,  174,  175,  176,  177,  178,  179,  154,
 /*  1120 */   155,  156,  157,  158,  159,  160,  163,   94,   95,   96,
 /*  1130 */    97,   98,  167,  168,  101,  136,  137,  158,  130,  174,
 /*  1140 */   175,  176,  177,  178,  179,  155,  131,  130,   89,  154,
 /*  1150 */   155,  156,  157,  158,  159,  160,  158,  170,  233,  172,
 /*  1160 */   232,  158,  167,  168,  105,  106,  107,  108,  109,  174,
 /*  1170 */   175,  176,  177,  178,  179,   26,   42,  154,  155,  156,
 /*  1180 */   157,  158,  159,  160,  170,   61,  172,  234,   70,  235,
 /*  1190 */   167,  168,  205,  206,  207,  208,  209,  174,  175,  176,
 /*  1200 */   177,  178,  179,  234,  154,  155,  156,  157,  158,  159,
 /*  1210 */   160,  235,   94,   95,   96,   97,   98,  167,  168,  101,
 /*  1220 */   206,  207,  208,  209,  174,  175,  176,  177,  178,  179,
 /*  1230 */    62,  154,  155,  156,  157,  158,  159,  160,  155,  235,
 /*  1240 */   234,  155,  202,   72,  167,  168,  105,  106,  107,  108,
 /*  1250 */   109,  174,  175,  176,  177,  178,  179,  235,  234,  234,
 /*  1260 */     2,  154,  155,  156,  157,  158,  159,  160,    2,  235,
 /*  1270 */   209,   70,  233,  232,  167,  168,  105,  106,  107,  108,
 /*  1280 */   109,  174,  175,  176,  177,  178,  179,  154,  155,  156,
 /*  1290 */   157,  158,  159,  160,  234,   94,   95,   96,   97,   98,
 /*  1300 */   167,  168,  101,  235,  233,  232,  234,  174,  175,  176,
 /*  1310 */   177,  178,  179,  235,  234,  155,  235,  154,  155,  156,
 /*  1320 */   157,  158,  159,  160,  234,   41,  235,   72,  234,  234,
 /*  1330 */   167,  168,  105,  106,  107,  108,  109,  174,  175,  176,
 /*  1340 */   177,  178,  179,  235,  235,  154,  155,  156,  157,  158,
 /*  1350 */   159,  160,  105,  106,  107,  108,  109,  231,  167,  168,
 /*  1360 */   105,  106,  107,  108,  109,  174,  175,  176,  177,  178,
 /*  1370 */   179,  233,  154,  155,  156,  157,  158,  159,  160,  232,
 /*  1380 */   105,  106,  107,  108,  109,  167,  168,   69,  234,  155,
 /*  1390 */    72,  235,  174,  175,  176,  177,  178,  179,  231,   81,
 /*  1400 */    82,   83,  233,   85,   86,   87,   88,  232,  154,  155,
 /*  1410 */   156,  157,  158,  159,  160,  234,   69,  235,  155,   72,
 /*  1420 */   231,  233,  232,  105,  106,  107,  108,  109,   81,   82,
 /*  1430 */    83,  177,   85,   86,   87,   88,  153,  154,  155,  156,
 /*  1440 */   157,  158,  159,  160,  190,  162,   69,  193,  194,  195,
 /*  1450 */   196,  197,  105,  106,  107,  108,  109,   80,   81,   82,
 /*  1460 */    83,  235,   85,   86,   87,   88,  154,  155,  156,  157,
 /*  1470 */   158,  159,  160,  234,  234,  155,   70,  235,  235,  167,
 /*  1480 */   168,  231,  105,  106,  107,  108,  109,  231,  176,  177,
 /*  1490 */   204,  231,  154,  155,  156,  157,  158,  159,  160,  231,
 /*  1500 */    94,   95,   96,   97,   98,  167,  168,  101,  231,  186,
 /*  1510 */   198,  199,  200,  157,  176,  177,  173,  173,  173,  173,
 /*  1520 */   154,  155,  156,  157,  158,  159,  160,  173,  173,   70,
 /*  1530 */   173,  173,  173,  167,  168,  173,  198,  199,  200,  231,
 /*  1540 */   173,  173,  176,  177,  173,  154,  155,  156,  157,  158,
 /*  1550 */   159,  160,  231,   94,   95,   96,   97,   98,  167,  168,
 /*  1560 */   101,  235,  235,  232,  198,  199,  200,  176,  177,  232,
 /*  1570 */   231,  154,  155,  156,  157,  158,  159,  160,    2,   94,
 /*  1580 */    95,   96,   97,   98,  167,  168,  101,  172,  172,  198,
 /*  1590 */   199,  200,  232,  176,  177,  170,  232,  172,  172,  154,
 /*  1600 */   155,  156,  157,  158,  159,  160,  172,  154,  155,  156,
 /*  1610 */   157,  158,  159,  160,  172,  198,  199,  200,  172,  172,
 /*  1620 */   172,  172,  177,  154,  155,  156,  157,  158,  159,  160,
 /*  1630 */   177,  206,  207,  208,  209,  190,  167,  168,  193,  194,
 /*  1640 */   195,  196,  197,  190,    2,  254,  193,  194,  195,  196,
 /*  1650 */   197,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  1660 */   154,  155,  156,  157,  158,  159,  160,   95,   96,   97,
 /*  1670 */    98,  254,  254,  101,  177,  154,  155,  156,  157,  158,
 /*  1680 */   159,  160,  254,  177,  254,  254,  254,  190,  254,  254,
 /*  1690 */   193,  194,  195,  196,  197,  254,  190,  254,  177,  193,
 /*  1700 */   194,  195,  196,  197,  154,  155,  156,  157,  158,  159,
 /*  1710 */   160,  190,  254,  254,  193,  194,  195,  196,  197,  154,
 /*  1720 */   155,  156,  157,  158,  159,  160,  254,  177,  154,  155,
 /*  1730 */   156,  157,  158,  159,  160,   95,   96,   97,   98,  254,
 /*  1740 */   190,  101,  177,  193,  194,  195,  196,  197,  254,  254,
 /*  1750 */    62,  177,  254,  254,  254,  190,  254,  254,  193,  194,
 /*  1760 */   195,  196,  197,  254,  190,  254,  254,  193,  194,  195,
 /*  1770 */   196,  197,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  1780 */    69,  254,   94,   95,   96,   97,   98,  254,   69,  101,
 /*  1790 */   254,  254,   81,   82,   83,  177,   85,   86,   87,   88,
 /*  1800 */    81,   82,   83,   73,   85,   86,   87,   88,  190,  254,
 /*  1810 */   254,  193,  194,  195,  196,  197,  105,  106,  107,  108,
 /*  1820 */   109,  254,  254,  254,  105,  106,  107,  108,  109,    1,
 /*  1830 */     2,   72,  163,    5,    6,  105,  106,  107,  108,  109,
 /*  1840 */    81,   82,   83,  254,   85,   86,   87,   88,  105,  106,
 /*  1850 */   107,  108,  109,  184,  254,   81,   82,   83,  254,   85,
 /*  1860 */    86,   87,   88,  254,  105,  106,  107,  108,  109,  154,
 /*  1870 */   155,  156,  157,  158,  159,  160,   48,   49,   50,  105,
 /*  1880 */   106,  107,  108,  109,   81,   82,   83,  158,   85,   86,
 /*  1890 */    87,   88,  163,  254,  225,  226,   81,   82,   83,   71,
 /*  1900 */    85,   86,   87,   88,  189,  254,  254,  254,  105,  106,
 /*  1910 */   107,  108,  109,  184,  163,  254,  187,  188,  254,  254,
 /*  1920 */   105,  106,  107,  108,  109,  153,  154,  155,  156,  157,
 /*  1930 */   158,  159,  160,  105,  162,  184,  254,  254,  110,   81,
 /*  1940 */    82,   83,  254,   85,   86,   87,   88,   81,   82,   83,
 /*  1950 */   254,   85,   86,   87,   88,  254,  254,  254,  130,  131,
 /*  1960 */   132,  254,  254,  105,  106,  107,  108,  109,  254,  254,
 /*  1970 */   254,  105,  106,  107,  108,  109,  225,  226,  254,  154,
 /*  1980 */   155,  156,  157,  158,  159,  160,  154,  155,  156,  157,
 /*  1990 */   158,  159,  160,  254,  169,  254,   94,   95,   96,   97,
 /*  2000 */    98,  169,   26,  101,  254,  254,  181,  182,  183,  254,
 /*  2010 */   254,   72,  254,  181,  182,  183,  154,  155,  156,  157,
 /*  2020 */   158,  159,  160,  154,  155,  156,  157,  158,  159,  160,
 /*  2030 */   254,  169,  254,   94,   95,   96,   97,   98,  169,  254,
 /*  2040 */   101,  254,  254,  181,  182,  183,  254,  254,  254,  254,
 /*  2050 */   181,  182,  183,  154,  155,  156,  157,  158,  159,  160,
 /*  2060 */    94,   95,   96,   97,   98,  254,  254,  101,  169,  254,
 /*  2070 */    94,   95,   96,   97,   98,  254,  254,  101,  254,  158,
 /*  2080 */   181,  182,  183,  254,  163,  254,  254,  254,  154,  155,
 /*  2090 */   156,  157,  158,  159,  160,  154,  155,  156,  157,  158,
 /*  2100 */   159,  160,  254,  169,  254,  184,  254,  254,  187,  188,
 /*  2110 */   169,  254,  254,  254,  254,  181,  182,  183,  254,  254,
 /*  2120 */   254,  254,  181,  182,  183,  254,  154,  155,  156,  157,
 /*  2130 */   158,  159,  160,  254,  154,  155,  156,  157,  158,  159,
 /*  2140 */   160,  169,  154,  155,  156,  157,  158,  159,  160,  169,
 /*  2150 */   254,  254,  254,  181,  182,  183,  254,  169,  254,  254,
 /*  2160 */   254,  181,  182,  183,  254,  254,  254,  254,  254,  181,
 /*  2170 */   182,  183,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2180 */   161,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2190 */   161,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2200 */   161,  162,    1,  254,  153,  154,  155,  156,  157,  158,
 /*  2210 */   159,  160,  161,  162,  154,  155,  156,  157,  158,  159,
 /*  2220 */   160,  254,  254,  254,  254,  254,  254,  167,  168,  254,
 /*  2230 */   254,  254,  254,  254,  153,  154,  155,  156,  157,  158,
 /*  2240 */   159,  160,  254,  162,  254,  254,  254,  254,  254,   48,
 /*  2250 */    49,   50,  254,  154,  155,  156,  157,  158,  159,  160,
 /*  2260 */   254,  254,  254,  254,   63,   64,  167,  168,  254,  254,
 /*  2270 */   254,  254,   71,  254,  254,  254,  153,  154,  155,  156,
 /*  2280 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2290 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2300 */   157,  158,  159,  160,  254,  162,  105,  254,  254,  254,
 /*  2310 */   254,  110,  254,  254,  254,  153,  154,  155,  156,  157,
 /*  2320 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2330 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2340 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2350 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2360 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2370 */   158,  159,  160,  254,  162,  254,  254,  254,  254,  254,
 /*  2380 */   254,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2390 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2400 */   162,  254,  254,  153,  154,  155,  156,  157,  158,  159,
 /*  2410 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2420 */   160,  254,  162,  254,  254,  254,  153,  154,  155,  156,
 /*  2430 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2440 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2450 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2460 */   157,  158,  159,  160,  254,  162,  254,  254,  254,  153,
 /*  2470 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2480 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2490 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2500 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2510 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2520 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  254,
 /*  2530 */   254,  254,  254,  254,  254,  153,  154,  155,  156,  157,
 /*  2540 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2550 */   158,  159,  160,  254,  162,  254,  254,  153,  154,  155,
 /*  2560 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2570 */   156,  157,  158,  159,  160,  254,  162,  254,  254,  254,
 /*  2580 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2590 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2600 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2610 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2620 */   254,  254,  254,  153,  154,  155,  156,  157,  158,  159,
 /*  2630 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2640 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2650 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2660 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2670 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2680 */   160,  254,  162,  254,  254,  254,  254,  254,  254,  153,
 /*  2690 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2700 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  254,
 /*  2710 */   254,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2720 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2730 */   162,  254,  254,  254,  153,  154,  155,  156,  157,  158,
 /*  2740 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2750 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2760 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2770 */   159,  160,  254,  162,  254,  254,  254,  153,  154,  155,
 /*  2780 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2790 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2800 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2810 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2820 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2830 */   156,  157,  158,  159,  160,  254,  162,  254,  254,  254,
 /*  2840 */   254,  254,  254,  153,  154,  155,  156,  157,  158,  159,
 /*  2850 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2860 */   160,  254,  162,  254,  254,  153,  154,  155,  156,  157,
 /*  2870 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2880 */   158,  159,  160,  254,  162,  154,  155,  156,  157,  158,
 /*  2890 */   159,  160,  254,  254,  254,  254,  165,  254,  154,  155,
 /*  2900 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  2910 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  2920 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2930 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  2940 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  2950 */   158,  159,  160,  254,  254,  254,  254,  165,  154,  155,
 /*  2960 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  2970 */   254,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  2980 */   254,  254,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2990 */   254,  254,  254,  254,  165,  254,  254,  254,  154,  155,
 /*  3000 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  3010 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3020 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3030 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  3040 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  3050 */   158,  159,  160,  254,  254,  254,  254,  165,  154,  155,
 /*  3060 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  3070 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3080 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3090 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  3100 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  3110 */   158,  159,  160,  254,  254,  254,  254,  165,  154,  155,
 /*  3120 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  3130 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3140 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3150 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  3160 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  3170 */   158,  159,  160,  254,  254,  254,  154,  155,  156,  157,
 /*  3180 */   158,  159,  160,  154,  155,  156,  157,  158,  159,  160,
 /*  3190 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3200 */   254,  189,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3210 */   254,  189,  254,  254,  254,  254,  254,  254,  189,  254,
 /*  3220 */   254,  254,  254,  254,  254,  189,  154,  155,  156,  157,
 /*  3230 */   158,  159,  160,  254,  254,  254,  254,  189,  154,  155,
 /*  3240 */   156,  157,  158,  159,  160,  254,  254,  254,  154,  155,
 /*  3250 */   156,  157,  158,  159,  160,  254,  254,  254,   72,  254,
 /*  3260 */   254,  189,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3270 */   254,  254,  254,  189,  154,  155,  156,  157,  158,  159,
 /*  3280 */   160,  254,  254,  189,  154,  155,  156,  157,  158,  159,
 /*  3290 */   160,  105,  106,  107,  108,  109,  254,  189,  154,  155,
 /*  3300 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  189,
 /*  3310 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  189,
 /*  3320 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3330 */   254,  254,   73,  189,  154,  155,  156,  157,  158,  159,
 /*  3340 */   160,  254,  254,  254,  254,  189,  154,  155,  156,  157,
 /*  3350 */   158,  159,  160,  254,  254,  189,  154,  155,  156,  157,
 /*  3360 */   158,  159,  160,  254,  105,  106,  107,  108,  109,  189,
 /*  3370 */   254,  254,  254,  154,  155,  156,  157,  158,  159,  160,
 /*  3380 */   254,  189,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3390 */   254,  189,  154,  155,  156,  157,  158,  159,  160,  154,
 /*  3400 */   155,  156,  157,  158,  159,  160,  254,  254,  189,  154,
 /*  3410 */   155,  156,  157,  158,  159,  160,  254,  189,  154,  155,
 /*  3420 */   156,  157,  158,  159,  160,  254,  254,  189,  254,  254,
 /*  3430 */   254,  254,  254,  254,  189,  154,  155,  156,  157,  158,
 /*  3440 */   159,  160,  254,  254,  189,  254,  165,  254,  254,  254,
 /*  3450 */   254,  254,  254,  189,
};
#define YY_SHIFT_USE_DFLT (-80)
#define YY_SHIFT_COUNT (551)
#define YY_SHIFT_MIN   (-79)
#define YY_SHIFT_MAX   (3259)
static const short yy_shift_ofst[] = {
 /*     0 */   -80,  118,  208,  283,  283,  208,  232,  232,  283,  283,
 /*    10 */   283,  283,  283,  283,  283,  283,  283,  283,  283,  283,
 /*    20 */   283,  283,  283,  283,  283,  283,  283,  283,  283,  283,
 /*    30 */   283,  232,  232,  232,  232,  232,  232,  232,  232,  232,
 /*    40 */   232,  232,  232,  355,  355,  355,  355,  355,  355,  511,
 /*    50 */   511,  511,  511,  511,  511,  511,  511,  511,  511,  461,
 /*    60 */   677,  677,  677,  677,  677,  677,  677,  677,  677,  677,
 /*    70 */   677,  677,  677,  677,  677,  677,  677,  677,  677,  677,
 /*    80 */   677,  677,  677,  677,  677,  677,  677,  677,  677,  677,
 /*    90 */   677,  677,  677,  677,  677,  677,  677,  677,  677,  677,
 /*   100 */   677,  665,  677,  677,  677,  677,  677,  677,  677,  677,
 /*   110 */   677,  677,  677,  677,  677,  677,  677,  677,  677,  677,
 /*   120 */   677,  677,  677, 1828, 1828, 1828,  182,  665,  671,  671,
 /*   130 */   671,  671,  671,  671,  671,  671,  671,  671,  671,  671,
 /*   140 */   671,  671,  671,  671,  671,  671,  671,  671,  671,  671,
 /*   150 */   671,  671,  671,  665,  665,  665,  665,  665,  665,  665,
 /*   160 */   665,  665,  665,  665,  665,  665,  665,  665,  665,  665,
 /*   170 */   665,  665,  665,  665,  665,  665,  653,  653,  653,  653,
 /*   180 */   653,  588,  653,  653,  653,  588,  629,  629,  582,  545,
 /*   190 */   565,  588,  409,  409,  505,  505,  991, 1642, 1576,  613,
 /*   200 */   876,  991,  991,  991,  991,  758,  942,  876,  505,  505,
 /*   210 */  1642, 1576, 1258, 2201, 2201, 2201, 2201, 2201, 2201, 2201,
 /*   220 */  2201, 2201, 1033, 1976, 1976, 1976, 1976, 1688, 1688,  110,
 /*   230 */   110,  373,  373,  373,  373,  373,  373,  373,  373,  373,
 /*   240 */   373,  373,  373,  373,  366,  667,  153,  375,  951,  951,
 /*   250 */   758,  951,  951, 1168, 1017, 1168, 1124, 1168, 1124, 1149,
 /*   260 */  1134, 1284, 1017, 1168, 1124, 1149, 1134, 1284, 1017, 1168,
 /*   270 */  1124, 1149, 1134, 1284, 1017, 1168, 1124, 1168, 1124, 1168,
 /*   280 */  1124, 1168, 1124, 1168, 1124, 1149, 1134, 1168, 1124, 1149,
 /*   290 */  1134, 1266, 1258, 1168, 1124, 1168, 1124, 1017, 1168, 1124,
 /*   300 */  1017, 1168, 1124, 1168, 1124, 1149, 1134, 1015, 1015, 1017,
 /*   310 */  1015, 1008,  525, 1377, 1347, 1318, 1759, 1719, 1711, 1866,
 /*   320 */  1858, 1815, 1803, 1774,  431,  917, 1939, 1459, 1406, 1201,
 /*   330 */  1118,  552,  -29, 1007,  839,  950,  809,  755,  275,  345,
 /*   340 */    27, 1966, 1966, 1059,  -79,  -79,  -79,  -79,  -79,  -79,
 /*   350 */   -79,  -79,  -79,  -79,  -79, 1485, 3259, 1730,  435,  -21,
 /*   360 */  3186,  715, 1255, 1902, 1485, 1485, 1485, 1171,  919,  493,
 /*   370 */  1743, 1640,  638, 1275, 1572, 1141, 1141, 1141, 1141, 1141,
 /*   380 */  1141, 1141,  638,  638,  638,  638,  638,  213,  638,  638,
 /*   390 */   638,  638,  638,  638,  638,  638,  638,  638,  638,  638,
 /*   400 */   638,  638,  638,  638, 1247, 1227,  638,  638, 1141, 1141,
 /*   410 */  1141,  638,  638,  638,  638,  638,  638,  638,  638,  638,
 /*   420 */   111,  218,  218,  299,  299,  299,  299,  221,  221,  200,
 /*   430 */   197,   49,   87,   87,  128,  128,   83,   83,  -48,  -48,
 /*   440 */   -63,  -63,  -63,  -63,   83,   83,  -63,  -63,  -63,  -63,
 /*   450 */   736,  794,  767,  730,  377,  370,  370,  370,  370,  700,
 /*   460 */   681,  587,  830,  783,  503,  782,  822,  776,  633,  726,
 /*   470 */   269,  415,  781,  162,  708,  499,  115,  692,  379,   84,
 /*   480 */   253,  572,  501,  357,   59,  -41, 1035, 1009, 1005, 1002,
 /*   490 */  1000,  981,  970,  962,  948,  946,  915,  904,  897,  895,
 /*   500 */   889,  884,  838,  820,  820,  813,  807,  875,  737,  810,
 /*   510 */   523,  734,  706,  703,  769,  651,  523,  523,  623,  654,
 /*   520 */   630,  546,  523,  530,  509,  473,  504,  457,  496,  442,
 /*   530 */   394,  344,  336,  319,  258,  258,  304,  272,  249,   63,
 /*   540 */    63,  247,  224,  151,  141,   78,   74,   51,   10,   -5,
 /*   550 */   -53,    2,
};
#define YY_REDUCE_USE_DFLT (-136)
#define YY_REDUCE_COUNT (311)
#define YY_REDUCE_MIN   (-135)
#define YY_REDUCE_MAX   (3281)
static const short yy_reduce_ofst[] = {
 /*     0 */   999, -135,  278,  223, -120,  468,  391,  341, 1218, 1191,
 /*    10 */  1163, 1133, 1107, 1077, 1050, 1023,  995,  965,  939,  909,
 /*    20 */   882,  855,  827,  797,  771,  741,  714,  687,  659,  532,
 /*    30 */   -85, 1618, 1574, 1565, 1550, 1521, 1506, 1497, 1453, 1445,
 /*    40 */  1254,  595,  -17, 1417, 1391, 1366, 1338, 1312,  167, 1988,
 /*    50 */  1980, 1972, 1941, 1934, 1899, 1869, 1862, 1832, 1825, 2051,
 /*    60 */  2039, 2029, 2019,  106, 2722, 2712, 2700, 2690, 2674, 2664,
 /*    70 */  2654, 2644, 2634, 2624, 2611, 2601, 2591, 2581, 2568, 2558,
 /*    80 */  2546, 2536, 2520, 2510, 2500, 2490, 2480, 2470, 2457, 2447,
 /*    90 */  2437, 2427, 2414, 2404, 2392, 2382, 2366, 2356, 2346, 2336,
 /*   100 */  2326,  398, 2316, 2303, 2293, 2283, 2273, 2260, 2250, 2238,
 /*   110 */  2228, 2212, 2202, 2192, 2182, 2172, 2162, 2143, 2133, 2123,
 /*   120 */  2081, 1772, 1283, 2099, 2060, 1469,  214, 3281, 3264, 3255,
 /*   130 */  3245, 3238, 3228, 3219, 3202, 3192, 3180, 3166, 3156, 3144,
 /*   140 */  3130, 3120, 3108, 3094, 3084, 3072, 3048, 3036, 3029, 3022,
 /*   150 */  3012, 1715,  334, 3000, 2988, 2976, 2964, 2952, 2940, 2928,
 /*   160 */  2916, 2904, 2892, 2880, 2868, 2856, 2844, 2829, 2817, 2804,
 /*   170 */  2792, 2780, 2768, 2756, 2744, 2731,  987,  560,   -7, -110,
 /*   180 */  1425,  614, 1014,  883,  791,  178, 1921, 1729, 1751, 1669,
 /*   190 */   120,  561,  601,   92,  455,  205,  280,  300,   94,  105,
 /*   200 */   926,  886,  857,  847,  793,  804,  814,  756,  298,   40,
 /*   210 */   702,  701,  674, 1449, 1448, 1447, 1446, 1442, 1434, 1426,
 /*   220 */  1416, 1415, 1339, 1364, 1360, 1337, 1331, 1327, 1326, 1321,
 /*   230 */  1308, 1371, 1368, 1367, 1362, 1359, 1358, 1357, 1355, 1354,
 /*   240 */  1346, 1345, 1344, 1343, 1323, 1356, 1323, 1277, 1268, 1260,
 /*   250 */  1286, 1256, 1250, 1243, 1320, 1242, 1240, 1226, 1239, 1190,
 /*   260 */  1188, 1189, 1263, 1182, 1181, 1175, 1169, 1167, 1234, 1156,
 /*   270 */  1154, 1147, 1138, 1126, 1160, 1109, 1095, 1108, 1094, 1091,
 /*   280 */  1090, 1081, 1080, 1078, 1072, 1073, 1071, 1068, 1060, 1041,
 /*   290 */  1039, 1061, 1040, 1034, 1025, 1022, 1024, 1086, 1004, 1006,
 /*   300 */  1083,  976,  969,  954,  953,  928,  925, 1003,  998,  990,
 /*   310 */   979,  963,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   865, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*    10 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*    20 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*    30 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*    40 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*    50 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*    60 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1077, 1081,
 /*    70 */  1076, 1080, 1166, 1162, 1319, 1319, 1319, 1319, 1319, 1319,
 /*    80 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*    90 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1167,
 /*   100 */  1163, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   110 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   120 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1146, 1150,
 /*   130 */  1145, 1149, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   140 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   150 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   160 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   170 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   180 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   190 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   200 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   210 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   220 */  1319, 1319, 1267, 1269, 1269, 1269, 1269, 1275, 1275, 1267,
 /*   230 */  1267, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   240 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1267, 1267, 1267,
 /*   250 */  1319, 1267, 1267, 1275, 1319, 1275, 1273, 1275, 1273, 1269,
 /*   260 */  1271, 1267, 1319, 1275, 1273, 1269, 1271, 1267, 1319, 1275,
 /*   270 */  1273, 1269, 1271, 1267, 1319, 1275, 1273, 1275, 1273, 1275,
 /*   280 */  1273, 1275, 1273, 1275, 1273, 1269, 1271, 1275, 1273, 1269,
 /*   290 */  1271, 1319, 1319, 1275, 1273, 1275, 1273, 1319, 1275, 1273,
 /*   300 */  1319, 1275, 1273, 1275, 1273, 1269, 1271, 1319, 1319, 1319,
 /*   310 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1112,
 /*   320 */  1319, 1041, 1041, 1319, 1319,  932, 1319, 1319, 1319, 1319,
 /*   330 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   340 */  1319, 1259, 1256, 1319, 1148, 1152, 1147, 1151, 1143, 1142,
 /*   350 */  1141, 1140, 1139, 1138, 1137, 1130, 1319, 1319, 1319, 1319,
 /*   360 */  1319, 1319, 1319, 1274, 1268, 1270, 1266, 1319, 1319, 1319,
 /*   370 */   995, 1127, 1099,  994, 1055, 1067, 1066, 1065, 1064, 1063,
 /*   380 */  1062, 1061, 1047, 1079, 1083, 1078, 1082, 1012, 1168, 1164,
 /*   390 */  1043, 1040, 1039, 1038, 1037, 1036, 1035, 1034, 1033, 1032,
 /*   400 */  1031, 1030, 1029, 1028, 1319, 1319, 1169, 1165, 1070,  905,
 /*   410 */   906, 1027, 1026, 1025, 1024, 1023, 1022, 1021,  902,  901,
 /*   420 */  1160, 1129, 1128, 1116, 1115, 1100, 1101, 1000, 1001, 1319,
 /*   430 */  1319, 1319,  989,  990, 1057, 1056,  959,  958, 1014, 1013,
 /*   440 */   934,  933,  939,  938,  975,  976,  944,  943,  918,  919,
 /*   450 */  1319, 1319,  996, 1319, 1319, 1233, 1237, 1236, 1234, 1319,
 /*   460 */  1319, 1319, 1319, 1319, 1319,  980, 1319, 1319, 1319, 1319,
 /*   470 */  1319, 1181, 1319, 1319, 1319, 1319, 1319, 1319,  885, 1319,
 /*   480 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   490 */  1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   500 */  1319, 1319, 1109, 1126, 1125, 1319, 1319, 1319, 1235, 1319,
 /*   510 */  1229, 1222, 1199, 1201, 1319, 1214, 1180, 1179, 1197, 1319,
 /*   520 */  1319, 1196, 1195, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
 /*   530 */  1319, 1319, 1319, 1319, 1054, 1053, 1045, 1041,  889, 1011,
 /*   540 */  1010, 1319, 1319, 1319, 1319, 1068,  904,  900,  897,  893,
 /*   550 */   888, 1319, 1318, 1317, 1316, 1315, 1314, 1313, 1312, 1311,
 /*   560 */  1310, 1309, 1308, 1307, 1161, 1306, 1305, 1304, 1303, 1298,
 /*   570 */  1296, 1295, 1293, 1292, 1291, 1290, 1289, 1288, 1287, 1286,
 /*   580 */  1285, 1284, 1283, 1282, 1281, 1280, 1279, 1278, 1277, 1276,
 /*   590 */  1250, 1249, 1258, 1257, 1265, 1264, 1261, 1260, 1255, 1263,
 /*   600 */  1121, 1123, 1144, 1136, 1135, 1134, 1133, 1132, 1131, 1124,
 /*   610 */  1122, 1120, 1114, 1113, 1111, 1110, 1109, 1119, 1118, 1117,
 /*   620 */  1104, 1103, 1102, 1098, 1097, 1096, 1095, 1094, 1093, 1092,
 /*   630 */  1091, 1090, 1089, 1088, 1087, 1108, 1107, 1106, 1105, 1254,
 /*   640 */  1253, 1252, 1251, 1004, 1003, 1002,  999,  998,  997,  996,
 /*   650 */  1244, 1243, 1245, 1242, 1247, 1248, 1246, 1241, 1239, 1238,
 /*   660 */  1240, 1232, 1227, 1230, 1231, 1228, 1226, 1217, 1220, 1225,
 /*   670 */  1223, 1221, 1219, 1218, 1216, 1192, 1200, 1202, 1215, 1213,
 /*   680 */  1212, 1211, 1210, 1209, 1208, 1207, 1206, 1205, 1204, 1203,
 /*   690 */  1198, 1194, 1190, 1189, 1188, 1187, 1186, 1185, 1184,  893,
 /*   700 */  1183, 1182,  993,  992,  991,  988,  987,  986,  985,  984,
 /*   710 */   983,  982,  981,  980, 1193, 1191, 1171, 1174, 1175, 1178,
 /*   720 */  1177, 1176, 1173, 1172, 1170, 1302, 1301, 1300, 1299, 1297,
 /*   730 */  1294, 1049, 1051, 1060, 1059, 1058, 1052, 1050, 1048,  957,
 /*   740 */   956,  955,  954,  953,  952,  962,  961,  960,  951,  950,
 /*   750 */   949,  948, 1272, 1044, 1046,  890, 1006, 1008, 1072, 1075,
 /*   760 */  1074, 1073, 1071, 1020, 1019, 1018, 1017, 1016, 1015, 1009,
 /*   770 */  1007, 1005,  930, 1154, 1160, 1159, 1158, 1157, 1156, 1155,
 /*   780 */  1153, 1042,  937,  936,  935,  942,  941,  940,  932,  931,
 /*   790 */   930,  929,  928,  927, 1085, 1086, 1084, 1069,  977,  979,
 /*   800 */   978,  974,  973,  972,  971,  970,  969,  968,  967,  966,
 /*   810 */   965,  964,  963,  903,  947,  946,  945,  926,  925,  924,
 /*   820 */   923,  920,  922,  921,  917,  916,  915,  914,  913,  912,
 /*   830 */   911,  910,  909,  908,  907,  899,  898,  896,  895,  894,
 /*   840 */   892,  891,  887,  883,  882,  886,  885,  884,  881,  880,
 /*   850 */   879,  878,  877,  876,  875,  874,  873,  872,  871,  870,
 /*   860 */   869,  868,  867,  866,
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
 /* 145 */ "formula ::= formula AMP formula",
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
 /* 188 */ "formula_no_const ::= formula_no_const AMP formula_no_const",
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
 /* 260 */ "formula_temporal ::= formula_temporal AMP formula_temporal",
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
 /* 289 */ "head_formula ::= head_formula AMP head_formula",
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
 /* 430 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 431 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 432 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 433 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 434 */ "law_impl ::= ARROW_LDASH formula clause_where PERIOD",
 /* 435 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 436 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 437 */ "law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 438 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 439 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 440 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 441 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 442 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 443 */ "law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 444 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 445 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 446 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 447 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 448 */ "law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD",
 /* 449 */ "stmt_code_blk ::= ASP_GR",
 /* 450 */ "stmt_code_blk ::= ASP_CP",
 /* 451 */ "stmt_code_blk ::= F2LP_GR",
 /* 452 */ "stmt_code_blk ::= F2LP_CP",
 /* 453 */ "stmt_code_blk ::= LUA_GR",
 /* 454 */ "stmt_code_blk ::= LUA_CP",
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
#line 2587 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 136: /* start */
    case 137: /* statement_lst */
    case 160: /* undeclared */
{
#line 209 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2596 "bcplus/parser/detail/lemon_parser.c"
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
#line 2609 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 139: /* stmt_macro_def */
{
#line 234 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy151));								
#line 2616 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_constant_def */
{
#line 236 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy431));								
#line 2623 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_object_def */
{
#line 238 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy468));								
#line 2630 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_variable_def */
{
#line 240 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy459));								
#line 2637 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_sort_def */
{
#line 242 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy61));								
#line 2644 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 148: /* stmt_noconcurrency */
{
#line 252 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy485));								
#line 2651 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_strong_noconcurrency */
{
#line 254 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy446));								
#line 2658 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 152: /* stmt_query */
{
#line 260 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy466));								
#line 2665 "bcplus/parser/detail/lemon_parser.c"
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
#line 2683 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 155: /* constant */
    case 163: /* constant_one_const */
    case 166: /* const_anon */
{
#line 298 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy225));								
#line 2692 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* object */
    case 157: /* object_nullary */
{
#line 300 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy422));								
#line 2700 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 158: /* variable */
{
#line 304 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy189));								
#line 2707 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* lua */
{
#line 306 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy117));								
#line 2714 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 161: /* term_lst */
    case 164: /* term_no_const_lst */
{
#line 310 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy259));								
#line 2722 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 170: /* num_range */
{
#line 713 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy345));								
#line 2729 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* num_range_eval */
{
#line 715 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy29));								
#line 2736 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 173: /* term_int_eval */
{
#line 719 "bcplus/parser/detail/lemon_parser.y"
 /* Initially left Blank */				
#line 2743 "bcplus/parser/detail/lemon_parser.c"
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
 DEALLOC((yypminor->yy17));								
#line 2761 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 177: /* atomic_formula */
    case 180: /* atomic_formula_anon */
    case 184: /* atomic_formula_one_const */
    case 199: /* atomic_head_formula */
{
#line 826 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy423));								
#line 2771 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 178: /* formula_quant */
    case 196: /* formula_temporal_quant */
{
#line 828 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy177));								
#line 2779 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_lst */
{
#line 1002 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy221));								
#line 2786 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* quant_op */
{
#line 1004 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2793 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 187: /* card_var_lst */
    case 188: /* card_var_lst_inner */
{
#line 1041 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy375));								
#line 2801 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* formula_smpl_card */
{
#line 1347 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy133));								
#line 2808 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* macro_def_lst */
{
#line 1407 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy269));                              
#line 2815 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* macro_bnd */
{
#line 1409 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy359));                              
#line 2822 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* macro_args */
{
#line 1411 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy154));                              
#line 2829 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* macro_arg */
{
#line 1413 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy79));                              
#line 2836 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* sort_lst */
{
#line 1503 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy195));							
#line 2843 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 206: /* sort */
    case 207: /* sort_id_nr */
    case 208: /* sort_nr */
    case 209: /* sort_id */
{
#line 1505 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2853 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* constant_bnd_lst */
    case 211: /* constant_bnd */
{
#line 1614 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy409));									
#line 2861 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* constant_dcl_lst */
{
#line 1618 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy226));									
#line 2868 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* constant_dcl_type */
{
#line 1620 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2875 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* attrib_spec */
{
#line 1622 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2882 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* object_bnd_lst */
{
#line 1981 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy102));									
#line 2889 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* object_bnd */
{
#line 1983 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy294));									
#line 2896 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* object_lst */
    case 218: /* object_spec */
{
#line 1985 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy425));									
#line 2904 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* variable_bnd_lst */
    case 220: /* variable_bnd */
{
#line 2117 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy410));									
#line 2912 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 221: /* variable_lst */
{
#line 2121 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy52));									
#line 2919 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 222: /* sort_bnd_lst */
    case 223: /* sort_bnd */
    case 224: /* sort_dcl_lst */
{
#line 2204 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy196));									
#line 2928 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 225: /* show_lst */
{
#line 2308 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy123));									
#line 2935 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 226: /* show_elem */
    case 234: /* clause_unless */
{
#line 2310 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy423));									
#line 2943 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 227: /* query_lst */
{
#line 2462 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy489).l); DEALLOC((yypminor->yy489).maxstep); DEALLOC((yypminor->yy489).label);	
#line 2950 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 228: /* query_maxstep_decl */
{
#line 2464 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy29));												
#line 2957 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 230: /* query_label_Decl */
{
#line 2466 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2964 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 231: /* clause_if */
    case 232: /* clause_after */
    case 233: /* clause_ifcons */
    case 235: /* clause_where */
{
#line 2620 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy17));									
#line 2974 "bcplus/parser/detail/lemon_parser.c"
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
#line 2661 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy484));									
#line 2998 "bcplus/parser/detail/lemon_parser.c"
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
#line 3758 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 220 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy484;
			yymsp[0].minor.yy484  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3767 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy151; }
#line 3772 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy431; }
#line 3777 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy468; }
#line 3782 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy459; }
#line 3787 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy61; }
#line 3792 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 268 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy484; }
#line 3802 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy485; }
#line 3807 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 273 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy446; }
#line 3812 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 276 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy466; }
#line 3817 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy225; }
#line 3822 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 43: /* term ::= base_elem */ yytestcase(yyruleno==43);
      case 59: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==99);
      case 223: /* term_temporal ::= base_elem_no_const */ yytestcase(yyruleno==223);
      case 241: /* term_temporal_strong ::= base_elem_no_const */ yytestcase(yyruleno==241);
#line 323 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy299; }
#line 3833 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 325 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy422;	}
#line 3838 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 326 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy189; }
#line 3843 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 327 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy117; }
#line 3848 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 39: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==39);
#line 445 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy225, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3854 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 40: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==40);
#line 446 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy225, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3860 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* const_anon ::= IDENTIFIER */
#line 448 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy225, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true); }
#line 3865 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 449 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy225, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true);	}
#line 3870 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 452 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy422, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3875 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= object_nullary */
#line 453 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy422 = yymsp[0].minor.yy422; }
#line 3880 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* object_nullary ::= OBJECT_ID */
#line 454 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy422, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3885 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* object ::= undeclared */
#line 455 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3890 "bcplus/parser/detail/lemon_parser.c"
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
#line 3905 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 469 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy117, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0); }
#line 3910 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* lua ::= AT_IDENTIFIER */
#line 470 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy117, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3915 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* lua ::= AT_IDENTIFIER PAREN_L PAREN_R */
#line 471 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy117, yymsp[-2].minor.yy0, yymsp[-1].minor.yy0, NULL, yymsp[0].minor.yy0); }
#line 3920 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 472 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy217, yymsp[-3].minor.yy0, yymsp[-1].minor.yy259);   yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 3927 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 36: /* undeclared ::= IDENTIFIER */
#line 473 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy217, yymsp[0].minor.yy0, NULL); }
#line 3932 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 37: /* term_lst ::= term */
      case 41: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==41);
#line 476 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy259 = new TermList();
			yygotominor.yy259->push_back(yymsp[0].minor.yy299);
		}
#line 3941 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 38: /* term_lst ::= term_lst COMMA term */
      case 42: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==42);
#line 482 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy259 = yymsp[-2].minor.yy259;
			yymsp[-2].minor.yy259->push_back(yymsp[0].minor.yy299);
		  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3951 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 44: /* term ::= INTEGER */
      case 60: /* term_strong ::= INTEGER */ yytestcase(yyruleno==60);
      case 85: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==85);
      case 100: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==100);
      case 116: /* term_integral ::= INTEGER */ yytestcase(yyruleno==116);
      case 224: /* term_temporal ::= INTEGER */ yytestcase(yyruleno==224);
      case 242: /* term_temporal_strong ::= INTEGER */ yytestcase(yyruleno==242);
#line 581 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy299, yymsp[0].minor.yy0);	}
#line 3962 "bcplus/parser/detail/lemon_parser.c"
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
{ BASIC_TERM(yygotominor.yy299, yymsp[0].minor.yy0); }
#line 3980 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 46: /* term ::= PAREN_L term PAREN_R */
      case 62: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==87);
      case 102: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==102);
      case 117: /* term_integral ::= PAREN_L term_integral PAREN_R */ yytestcase(yyruleno==117);
      case 226: /* term_temporal ::= PAREN_L term_temporal PAREN_R */ yytestcase(yyruleno==226);
      case 244: /* term_temporal_strong ::= PAREN_L term_temporal_strong PAREN_R */ yytestcase(yyruleno==244);
#line 583 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy299, yymsp[-2].minor.yy0, yymsp[-1].minor.yy299, yymsp[0].minor.yy0); }
#line 3991 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= MAXSTEP */
      case 63: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==63);
      case 88: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==88);
      case 105: /* term_no_const ::= MAXSTEP */ yytestcase(yyruleno==105);
      case 120: /* term_integral ::= MAXSTEP */ yytestcase(yyruleno==120);
      case 229: /* term_temporal ::= MAXSTEP */ yytestcase(yyruleno==229);
      case 245: /* term_temporal_strong ::= MAXSTEP */ yytestcase(yyruleno==245);
#line 586 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy299, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 4002 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= MAXADDITIVE */
      case 64: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==64);
      case 89: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==89);
      case 106: /* term_no_const ::= MAXADDITIVE */ yytestcase(yyruleno==106);
      case 121: /* term_integral ::= MAXADDITIVE */ yytestcase(yyruleno==121);
      case 230: /* term_temporal ::= MAXADDITIVE */ yytestcase(yyruleno==230);
      case 246: /* term_temporal_strong ::= MAXADDITIVE */ yytestcase(yyruleno==246);
#line 587 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy299, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 4013 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= MAXAFVALUE */
      case 65: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==65);
      case 90: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==90);
      case 107: /* term_no_const ::= MAXAFVALUE */ yytestcase(yyruleno==107);
      case 122: /* term_integral ::= MAXAFVALUE */ yytestcase(yyruleno==122);
      case 231: /* term_temporal ::= MAXAFVALUE */ yytestcase(yyruleno==231);
      case 247: /* term_temporal_strong ::= MAXAFVALUE */ yytestcase(yyruleno==247);
#line 588 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy299, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 4024 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= DASH term */
      case 66: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==66);
      case 92: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==92);
      case 109: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==109);
      case 123: /* term_integral ::= DASH term_integral */ yytestcase(yyruleno==123);
      case 233: /* term_temporal ::= DASH term_temporal */ yytestcase(yyruleno==233);
      case 249: /* term_temporal_strong ::= DASH term_temporal_strong */ yytestcase(yyruleno==249);
#line 592 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, UnaryTerm::Operator::NEGATIVE); }
#line 4035 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= ABS term */
      case 67: /* term_strong ::= ABS term */ yytestcase(yyruleno==67);
      case 93: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==93);
      case 110: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==110);
      case 124: /* term_integral ::= ABS term_integral */ yytestcase(yyruleno==124);
      case 234: /* term_temporal ::= ABS term_temporal */ yytestcase(yyruleno==234);
      case 250: /* term_temporal_strong ::= ABS term_temporal */ yytestcase(yyruleno==250);
#line 593 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, UnaryTerm::Operator::ABS); }
#line 4046 "bcplus/parser/detail/lemon_parser.c"
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
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MINUS); }
#line 4058 "bcplus/parser/detail/lemon_parser.c"
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
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::PLUS); }
#line 4070 "bcplus/parser/detail/lemon_parser.c"
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
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::TIMES); }
#line 4082 "bcplus/parser/detail/lemon_parser.c"
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
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::DIVIDE); }
#line 4094 "bcplus/parser/detail/lemon_parser.c"
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
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MOD); }
#line 4106 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 68: /* term_strong_candidate ::= DASH constant */
#line 620 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy225, UnaryTerm::Operator::NEGATIVE); }
#line 4111 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant DASH term */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MINUS); }
#line 4116 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant PLUS term */
#line 630 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::PLUS); }
#line 4121 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 76: /* term_strong ::= constant STAR term */
#line 631 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::TIMES); }
#line 4126 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong ::= constant INT_DIV term */
#line 632 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::DIVIDE); }
#line 4131 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 78: /* term_strong ::= constant MOD term */
#line 633 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MOD); }
#line 4136 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 91: /* term_no_const_strong ::= constant */
#line 655 "bcplus/parser/detail/lemon_parser.y"
{
		// error handling for constants so they don'yygotominor.yy299 default to undeclared identifiers
		yygotominor.yy299 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy225;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy225->beginLoc());
		YYERROR;
	}
#line 4147 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 108: /* term_no_const ::= constant */
#line 688 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy299 default to undeclared identifiers
		yygotominor.yy299 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy225;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy225->beginLoc());
		YYERROR;
	}
#line 4158 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* num_range ::= term_integral DBL_PERIOD term_integral */
#line 745 "bcplus/parser/detail/lemon_parser.y"
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
#line 4178 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 131: /* num_range_eval ::= term_int_eval DBL_PERIOD term_int_eval */
#line 763 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy396, r_ptr = yymsp[0].minor.yy396, s_ptr = yymsp[-1].minor.yy0;
	yygotominor.yy29 = new NumberRangeEval(yymsp[-2].minor.yy396->val(), yymsp[0].minor.yy396->val(), yymsp[-2].minor.yy396->beginLoc(), yymsp[0].minor.yy396->endLoc());
}
#line 4186 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* term_int_eval ::= INTEGER */
#line 769 "bcplus/parser/detail/lemon_parser.y"
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
#line 4201 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* term_int_eval ::= PAREN_L term_int_eval PAREN_R */
#line 781 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy396 = yymsp[-1].minor.yy396;
	yygotominor.yy396->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy396->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 4211 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* term_int_eval ::= DASH term_int_eval */
#line 801 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy396, yymsp[0].minor.yy396, -1 * yymsp[0].minor.yy396->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4217 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* term_int_eval ::= ABS term_int_eval */
#line 802 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy396, yymsp[0].minor.yy396, yymsp[0].minor.yy396->val() < 0 ? - yymsp[0].minor.yy396->val() : yymsp[0].minor.yy396->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 4223 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* term_int_eval ::= term_int_eval DASH term_int_eval */
#line 804 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() - yymsp[0].minor.yy396->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4229 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* term_int_eval ::= term_int_eval PLUS term_int_eval */
#line 805 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() + yymsp[0].minor.yy396->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 4235 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* term_int_eval ::= term_int_eval STAR term_int_eval */
#line 806 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() * yymsp[0].minor.yy396->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 4241 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* term_int_eval ::= term_int_eval INT_DIV term_int_eval */
#line 807 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() / yymsp[0].minor.yy396->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 4247 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* term_int_eval ::= term_int_eval MOD term_int_eval */
#line 808 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() % yymsp[0].minor.yy396->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 4253 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* formula ::= formula_base */
      case 184: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==184);
      case 256: /* formula_temporal ::= formula_temporal_base */ yytestcase(yyruleno==256);
#line 868 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy17;				}
#line 4260 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* formula ::= PAREN_L formula PAREN_R */
      case 185: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==185);
      case 257: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==257);
#line 869 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[-1].minor.yy17; yygotominor.yy17->parens(true); 	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4269 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* formula ::= NOT formula */
      case 186: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==186);
      case 258: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==258);
#line 870 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 4276 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 144: /* formula ::= DASH formula */
      case 187: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==187);
      case 259: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==259);
#line 871 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 4283 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 145: /* formula ::= formula AMP formula */
      case 188: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==188);
      case 260: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==260);
#line 872 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy17, yymsp[0].minor.yy17, yymsp[-2].minor.yy17->beginLoc(), yymsp[0].minor.yy17->endLoc());   yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 4291 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 146: /* formula ::= formula DBL_PLUS formula */
      case 147: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==147);
      case 189: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==189);
      case 190: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==190);
      case 261: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==261);
      case 262: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==262);
#line 873 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::OR); }
#line 4301 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 148: /* formula ::= formula EQUIV formula */
      case 191: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==191);
      case 263: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==263);
#line 875 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::EQUIV); }
#line 4308 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 149: /* formula ::= formula IMPL formula */
      case 150: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==150);
      case 192: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==192);
      case 193: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==193);
      case 264: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==264);
      case 265: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==265);
#line 876 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::IMPL); }
#line 4318 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 151: /* formula_base ::= comparison */
      case 194: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==194);
      case 267: /* formula_temporal_base ::= comparison_temporal */ yytestcase(yyruleno==267);
      case 291: /* head_formula ::= comparison */ yytestcase(yyruleno==291);
#line 879 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy17; }
#line 4326 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* formula_base ::= atomic_formula */
      case 292: /* head_formula ::= atomic_head_formula */ yytestcase(yyruleno==292);
#line 880 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy423; }
#line 4332 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* formula_base ::= formula_quant */
      case 269: /* formula_temporal_base ::= formula_temporal_quant */ yytestcase(yyruleno==269);
#line 881 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy177; }
#line 4338 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* formula_base ::= formula_card */
      case 270: /* formula_temporal_base ::= formula_temporal_card */ yytestcase(yyruleno==270);
#line 883 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy17 = yymsp[0].minor.yy17;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy17->beginLoc());
			YYERROR;
		}
	}
#line 4350 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* formula_base ::= TRUE */
      case 195: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==195);
      case 271: /* formula_temporal_base ::= TRUE */ yytestcase(yyruleno==271);
      case 294: /* head_formula ::= TRUE */ yytestcase(yyruleno==294);
#line 890 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4358 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* formula_base ::= FALSE */
      case 196: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==196);
      case 272: /* formula_temporal_base ::= FALSE */ yytestcase(yyruleno==272);
      case 295: /* head_formula ::= FALSE */ yytestcase(yyruleno==295);
#line 891 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4366 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= term_strong EQ term */
      case 164: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==164);
      case 197: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==197);
      case 273: /* comparison_temporal ::= term_temporal_strong EQ term_temporal */ yytestcase(yyruleno==273);
#line 893 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4375 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* comparison ::= term_strong DBL_EQ term */
      case 165: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==165);
      case 198: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==198);
      case 274: /* comparison_temporal ::= term_temporal_strong DBL_EQ term_temporal */ yytestcase(yyruleno==274);
#line 894 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4384 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* comparison ::= term_strong NEQ term */
      case 166: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==166);
      case 199: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==199);
      case 275: /* comparison_temporal ::= term_temporal_strong NEQ term_temporal */ yytestcase(yyruleno==275);
#line 895 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4393 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* comparison ::= term_strong LTHAN term */
      case 167: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==167);
      case 200: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==200);
      case 276: /* comparison_temporal ::= term_temporal_strong LTHAN term_temporal */ yytestcase(yyruleno==276);
#line 896 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4402 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 161: /* comparison ::= term_strong GTHAN term */
      case 168: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==168);
      case 201: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==201);
      case 277: /* comparison_temporal ::= term_temporal_strong GTHAN term_temporal */ yytestcase(yyruleno==277);
#line 897 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4411 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 162: /* comparison ::= term_strong LTHAN_EQ term */
      case 169: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==169);
      case 202: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==202);
      case 278: /* comparison_temporal ::= term_temporal_strong LTHAN_EQ term_temporal */ yytestcase(yyruleno==278);
#line 898 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4420 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 163: /* comparison ::= term_strong GTHAN_EQ term */
      case 170: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==170);
      case 203: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==203);
      case 279: /* comparison_temporal ::= term_temporal_strong GTHAN_EQ term_temporal */ yytestcase(yyruleno==279);
#line 899 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4429 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 171: /* comparison ::= constant DBL_EQ term */
#line 907 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4435 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 172: /* comparison ::= constant NEQ term */
#line 908 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4441 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 173: /* comparison ::= constant LTHAN term */
#line 909 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4447 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 174: /* comparison ::= constant GTHAN term */
#line 910 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4453 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 175: /* comparison ::= constant LTHAN_EQ term */
#line 911 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4459 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 176: /* comparison ::= constant GTHAN_EQ term */
#line 912 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4465 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 177: /* atomic_formula ::= constant */
      case 181: /* atomic_formula_anon ::= const_anon */ yytestcase(yyruleno==181);
      case 204: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==204);
#line 939 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy423, yymsp[0].minor.yy225, true); }
#line 4472 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 178: /* atomic_formula ::= TILDE constant */
      case 182: /* atomic_formula_anon ::= TILDE const_anon */ yytestcase(yyruleno==182);
      case 205: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==205);
#line 940 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy423, yymsp[0].minor.yy225, false);   yy_destructor(yypParser,75,&yymsp[-1].minor);
}
#line 4480 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 179: /* atomic_formula ::= constant EQ term */
      case 183: /* atomic_formula_anon ::= const_anon EQ term */ yytestcase(yyruleno==183);
      case 206: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==206);
#line 941 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = new AtomicFormula(yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());	  yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4488 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 180: /* atomic_formula_anon ::= atomic_formula */
      case 296: /* atomic_head_formula ::= atomic_formula */ yytestcase(yyruleno==296);
      case 384: /* show_elem ::= atomic_formula_one_const */ yytestcase(yyruleno==384);
#line 943 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = yymsp[0].minor.yy423; }
#line 4495 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 207: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
      case 280: /* formula_temporal_quant ::= BRACKET_L quant_lst PIPE formula_temporal BRACKET_R */ yytestcase(yyruleno==280);
#line 1007 "bcplus/parser/detail/lemon_parser.y"
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
#line 4513 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* quant_lst ::= quant_op variable */
#line 1021 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy221 = new QuantifierFormula::QuantifierList();
		yygotominor.yy221->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy497, yymsp[0].minor.yy189));
	}
#line 4521 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 209: /* quant_lst ::= quant_lst quant_op variable */
#line 1027 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy221 = yymsp[-2].minor.yy221;
		yygotominor.yy221->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy497, yymsp[0].minor.yy189));
	}
#line 4529 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* quant_op ::= BIG_CONJ */
#line 1032 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy497 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,90,&yymsp[0].minor);
}
#line 4535 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* quant_op ::= BIG_DISJ */
#line 1033 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy497 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4541 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
      case 281: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==281);
#line 1079 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, NULL);  }
#line 4547 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
      case 282: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==282);
#line 1080 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy17,  yymsp[0].minor.yy0, NULL);  }
#line 4553 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 283: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==283);
#line 1081 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4559 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 215: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 284: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==284);
#line 1082 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-5].minor.yy299, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy17,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4565 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
      case 285: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==285);
#line 1083 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, NULL);  }
#line 4571 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
      case 286: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==286);
#line 1084 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-3].minor.yy299, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy17,  yymsp[0].minor.yy0, NULL);  }
#line 4577 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 218: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
      case 287: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==287);
#line 1085 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4583 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 219: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
      case 288: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==288);
#line 1086 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy17,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4589 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 220: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1090 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy375 = yymsp[-1].minor.yy375;
	  yy_destructor(yypParser,98,&yymsp[0].minor);
}
#line 4597 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 221: /* card_var_lst_inner ::= variable */
#line 1095 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy189;
		yygotominor.yy375 = new CardinalityFormula::VariableList();
		yygotominor.yy375->push_back(yymsp[0].minor.yy189->symbol());
	}
#line 4606 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1102 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy189;
		yygotominor.yy375 = yymsp[-2].minor.yy375;
		yygotominor.yy375->push_back(yymsp[0].minor.yy189->symbol());
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4616 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* term_temporal ::= constant */
#line 1156 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy299 default to undeclared identifiers
		yygotominor.yy299 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy225;
		parser->_parse_error("All constant symbols must be bound to a step using the i:F notation.", &yymsp[0].minor.yy225->beginLoc());
		YYERROR;
	}
#line 4627 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* term_temporal ::= term_temporal COLON term */
      case 248: /* term_temporal_strong ::= term_temporal_strong COLON term_strong */ yytestcase(yyruleno==248);
#line 1168 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BindingTerm); }
#line 4633 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* formula_temporal ::= term_temporal_strong COLON formula */
#line 1249 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy17, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BindingFormula); }
#line 4638 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* formula_temporal_base ::= atomic_formula */
#line 1255 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for more useful error messages
		yygotominor.yy17 = NULL;
		ref_ptr<const Referenced> l_ptr = yymsp[0].minor.yy423;
		parser->_parse_error("All constant symbols must be bound to a step using the i:F notation.", &yymsp[0].minor.yy423->beginLoc());
		YYERROR;
	}
#line 4649 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* head_formula ::= head_formula AMP head_formula */
#line 1350 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy17 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy17, yymsp[0].minor.yy17, yymsp[-2].minor.yy17->beginLoc(), yymsp[0].minor.yy17->endLoc());
	  yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 4657 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* head_formula ::= PAREN_L head_formula PAREN_R */
#line 1354 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> lp_ptr = yymsp[-2].minor.yy0, rp_ptr = yymsp[0].minor.yy0;
		yygotominor.yy17 = yymsp[-1].minor.yy17;
		yygotominor.yy17->parens(true);																									\
		yygotominor.yy17->beginLoc(yymsp[-2].minor.yy0->beginLoc());																					\
		yygotominor.yy17->endLoc(yymsp[0].minor.yy0->endLoc());
		
	}
#line 4669 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* head_formula ::= formula_smpl_card */
#line 1365 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy17 = yymsp[0].minor.yy133;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy133->beginLoc());
			YYERROR;
		}
	}
#line 4680 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* atomic_head_formula ::= DASH constant */
#line 1378 "bcplus/parser/detail/lemon_parser.y"
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
#line 4696 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1391 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy423, yymsp[0].minor.yy0, NULL);  }
#line 4701 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1392 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy423,  yymsp[0].minor.yy0, NULL);  }
#line 4706 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1393 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4711 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1394 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-5].minor.yy299, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy423,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4716 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1395 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy423, yymsp[0].minor.yy0, NULL);  }
#line 4721 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1396 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-3].minor.yy299, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy423,  yymsp[0].minor.yy0, NULL);  }
#line 4726 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1397 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4731 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1398 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy423,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4736 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1417 "bcplus/parser/detail/lemon_parser.y"
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
#line 4766 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* macro_def_lst ::= macro_bnd */
#line 1445 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy269 = new MacroDeclaration::ElementList();
        yygotominor.yy269->push_back(yymsp[0].minor.yy359);
    }
#line 4774 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1451 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy269 = yymsp[-2].minor.yy269;
        yygotominor.yy269->push_back(yymsp[0].minor.yy359);
      yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4783 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1457 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy154;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy359 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy154);
      yy_destructor(yypParser,71,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4797 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1466 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy359 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4808 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* macro_args ::= macro_arg */
#line 1474 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy154 = new MacroSymbol::ArgumentList();
        yygotominor.yy154->push_back(yymsp[0].minor.yy79->str());
        delete yymsp[0].minor.yy79;
    }
#line 4817 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* macro_args ::= macro_args COMMA macro_arg */
#line 1480 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy154 = yymsp[-2].minor.yy154;
        yygotominor.yy154->push_back(yymsp[0].minor.yy79->str());
        delete yymsp[0].minor.yy79;
      yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4827 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* macro_arg ::= POUND_INTEGER */
      case 314: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==314);
#line 1487 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy79 = yymsp[0].minor.yy0;
    }
#line 4835 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* sort_lst ::= sort */
#line 1514 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy195 = new ConstantSymbol::SortList();
		yygotominor.yy195->push_back(yymsp[0].minor.yy93);
	}
#line 4843 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* sort_lst ::= sort_lst COMMA sort */
#line 1519 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy195 = yymsp[-2].minor.yy195;
		yygotominor.yy195->push_back(yymsp[0].minor.yy93);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4852 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* sort ::= sort_id_nr */
      case 323: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==323);
      case 324: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==324);
#line 1544 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = yymsp[0].minor.yy93; }
#line 4859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* sort ::= sort_id_nr STAR */
#line 1545 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::NONE)); }
#line 4864 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* sort ::= sort_id_nr CARROT */
#line 1546 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::UNKNOWN)); }
#line 4869 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 320: /* sort ::= sort PLUS object_nullary */
#line 1548 "bcplus/parser/detail/lemon_parser.y"
{ u::ref_ptr<const Object> o_ptr = yymsp[0].minor.yy422; DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, yymsp[0].minor.yy422->symbol()); }
#line 4874 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* sort ::= sort PLUS IDENTIFIER */
#line 1551 "bcplus/parser/detail/lemon_parser.y"
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
#line 4891 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 322: /* sort ::= sort PLUS INTEGER */
#line 1565 "bcplus/parser/detail/lemon_parser.y"
{ 
												  ref_ptr<const Object> t_ptr;
												  BASIC_TERM(t_ptr, yymsp[0].minor.yy0);
												  DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, t_ptr->symbol()); 
												}
#line 4900 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 325: /* sort_nr ::= num_range */
#line 1576 "bcplus/parser/detail/lemon_parser.y"
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
#line 4920 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 326: /* sort_id ::= IDENTIFIER */
#line 1594 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy93 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy93) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4933 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 327: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1625 "bcplus/parser/detail/lemon_parser.y"
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
#line 4952 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 328: /* constant_bnd_lst ::= constant_bnd */
#line 1642 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy409 = yymsp[0].minor.yy409;
	}
#line 4959 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1647 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy409;
		yygotominor.yy409 = yymsp[-2].minor.yy409;
		yygotominor.yy409->splice(yygotominor.yy409->end(), *yymsp[0].minor.yy409);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4969 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 330: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1667 "bcplus/parser/detail/lemon_parser.y"
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
#line 4997 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 331: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1689 "bcplus/parser/detail/lemon_parser.y"
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
#line 5012 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 332: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1700 "bcplus/parser/detail/lemon_parser.y"
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
#line 5042 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 333: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1726 "bcplus/parser/detail/lemon_parser.y"
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
#line 5071 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 334: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1750 "bcplus/parser/detail/lemon_parser.y"
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
#line 5152 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 335: /* constant_dcl_lst ::= IDENTIFIER */
#line 1826 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = new IdentifierDeclList();
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 5160 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 336: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1831 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = new IdentifierDeclList();
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy195));
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5170 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 337: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1836 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = yymsp[-2].minor.yy226;
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5179 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 338: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1841 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = yymsp[-5].minor.yy226;
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy195));
	  yy_destructor(yypParser,102,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5190 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 339: /* constant_dcl_type ::= ABACTION */
#line 1848 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5202 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 340: /* constant_dcl_type ::= ACTION */
#line 1857 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5214 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 341: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1866 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5226 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 342: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1875 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5238 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 343: /* constant_dcl_type ::= EXTERNALACTION */
#line 1884 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5250 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 344: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1893 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5262 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 345: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1902 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5274 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1911 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5286 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* constant_dcl_type ::= RIGID */
#line 1920 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5298 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 348: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1929 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5310 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* constant_dcl_type ::= SDFLUENT */
#line 1939 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5322 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* attrib_spec ::= ATTRIBUTE */
#line 1949 "bcplus/parser/detail/lemon_parser.y"
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
#line 5337 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1962 "bcplus/parser/detail/lemon_parser.y"
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
#line 5353 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1990 "bcplus/parser/detail/lemon_parser.y"
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
#line 5388 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* object_bnd_lst ::= object_bnd */
#line 2023 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy102 = new ObjectDeclaration::ElementList();
		yygotominor.yy102->push_back(yymsp[0].minor.yy294);
	}
#line 5396 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 2029 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy102 = yymsp[-2].minor.yy102;
		yygotominor.yy102->push_back(yymsp[0].minor.yy294);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5405 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 2035 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy294 = new ObjectDeclaration::Element(yymsp[0].minor.yy93, yymsp[-2].minor.yy425);
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5413 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* object_lst ::= object_spec */
#line 2040 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = yymsp[0].minor.yy425;
	}
#line 5420 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* object_lst ::= object_lst COMMA object_spec */
#line 2044 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = yymsp[-2].minor.yy425;
		yygotominor.yy425->splice(yygotominor.yy425->end(), *yymsp[0].minor.yy425);
		delete yymsp[0].minor.yy425;
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5430 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* object_spec ::= IDENTIFIER */
#line 2053 "bcplus/parser/detail/lemon_parser.y"
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
#line 5446 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 2066 "bcplus/parser/detail/lemon_parser.y"
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
#line 5465 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* object_spec ::= INTEGER */
#line 2081 "bcplus/parser/detail/lemon_parser.y"
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
#line 5481 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* object_spec ::= num_range */
#line 2095 "bcplus/parser/detail/lemon_parser.y"
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
#line 5498 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 2124 "bcplus/parser/detail/lemon_parser.y"
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
#line 5536 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* variable_bnd_lst ::= variable_bnd */
#line 2160 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = yymsp[0].minor.yy410;
	}
#line 5543 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 2165 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = yymsp[-2].minor.yy410;
		yygotominor.yy410->splice(yygotominor.yy410->end(), *yymsp[0].minor.yy410);
		delete yymsp[0].minor.yy410;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5553 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 365: /* variable_bnd ::= variable_lst DBL_COLON sort */
#line 2172 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy52) {
			yygotominor.yy410->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy93));
		}



		delete yymsp[-2].minor.yy52;
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5569 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* variable_lst ::= IDENTIFIER */
#line 2185 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy52 = new TokenList();
		yygotominor.yy52->push_back(yymsp[0].minor.yy0);
	}
#line 5577 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 2190 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy52 = yymsp[-2].minor.yy52;
		yygotominor.yy52->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5586 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 2211 "bcplus/parser/detail/lemon_parser.y"
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
#line 5604 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 369: /* sort_bnd_lst ::= sort_bnd */
      case 371: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==371);
#line 2227 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[0].minor.yy196;
	}
#line 5612 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 370: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 2232 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[-2].minor.yy196;
		yygotominor.yy196->splice(yygotominor.yy196->end(), *yymsp[0].minor.yy196);
		delete yymsp[0].minor.yy196;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5622 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 372: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 2244 "bcplus/parser/detail/lemon_parser.y"
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
#line 5638 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 373: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 2256 "bcplus/parser/detail/lemon_parser.y"
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
#line 5653 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 374: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 2267 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[-1].minor.yy196;
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5662 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 375: /* sort_dcl_lst ::= IDENTIFIER */
#line 2272 "bcplus/parser/detail/lemon_parser.y"
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
#line 5679 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 376: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 2286 "bcplus/parser/detail/lemon_parser.y"
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
#line 5698 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 377: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 2313 "bcplus/parser/detail/lemon_parser.y"
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
#line 5714 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 378: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 2327 "bcplus/parser/detail/lemon_parser.y"
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
#line 5732 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 379: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 2344 "bcplus/parser/detail/lemon_parser.y"
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
#line 5748 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 380: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2358 "bcplus/parser/detail/lemon_parser.y"
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
#line 5766 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 381: /* show_lst ::= show_elem */
#line 2376 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = new ShowStatement::ElementList();
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	}
#line 5774 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 382: /* show_lst ::= show_lst COMMA show_elem */
#line 2381 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = yymsp[-2].minor.yy123;
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5783 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 383: /* show_lst ::= show_lst SEMICOLON show_elem */
#line 2386 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = yymsp[-2].minor.yy123;
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5792 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 385: /* stmt_noconcurrency ::= NOCONCURRENCY PERIOD */
#line 2414 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy485, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5797 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 386: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD */
#line 2415 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy446, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5802 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 387: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_int_eval PERIOD */
#line 2441 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5808 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 388: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE DBL_COLON term_int_eval PERIOD */
#line 2442 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,76,&yymsp[-2].minor);
}
#line 5814 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 389: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_int_eval PERIOD */
#line 2443 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5820 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 390: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE DBL_COLON term_int_eval PERIOD */
#line 2444 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,76,&yymsp[-2].minor);
}
#line 5826 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 391: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2469 "bcplus/parser/detail/lemon_parser.y"
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
#line 5863 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 392: /* query_lst ::= formula_temporal */
#line 2505 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = NULL;
		yygotominor.yy489.label = NULL;

		yygotominor.yy489.l->push_back(yymsp[0].minor.yy17);
	}
#line 5874 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 393: /* query_lst ::= query_maxstep_decl */
#line 2514 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = yymsp[0].minor.yy29;
		yygotominor.yy489.label = NULL;
	}
#line 5883 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 394: /* query_lst ::= query_label_decl */
#line 2521 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = NULL;
		yygotominor.yy489.label = yymsp[0].minor.yy79;
	}
#line 5892 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 395: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2528 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy489 = yymsp[-2].minor.yy489;
		yymsp[-2].minor.yy489.l->push_back(yymsp[0].minor.yy17);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5901 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 396: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2534 "bcplus/parser/detail/lemon_parser.y"
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
#line 5917 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 397: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2547 "bcplus/parser/detail/lemon_parser.y"
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
#line 5933 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 398: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2573 "bcplus/parser/detail/lemon_parser.y"
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
#line 5958 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 399: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range_eval */
#line 2594 "bcplus/parser/detail/lemon_parser.y"
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
#line 5975 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 400: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 401: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==401);
#line 2608 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy79, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5982 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 402: /* clause_if ::= IF formula */
#line 2643 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_IF); 		}
#line 5987 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 403: /* clause_if ::= */
      case 405: /* clause_after ::= */ yytestcase(yyruleno==405);
      case 407: /* clause_ifcons ::= */ yytestcase(yyruleno==407);
      case 411: /* clause_where ::= */ yytestcase(yyruleno==411);
#line 2644 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = NULL; }
#line 5995 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 404: /* clause_after ::= AFTER formula */
#line 2645 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_AFTER);	}
#line 6000 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 406: /* clause_ifcons ::= IFCONS formula */
#line 2647 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_IFCONS); 	}
#line 6005 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 408: /* clause_unless ::= UNLESS atomic_formula_anon */
#line 2649 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy423, Language::Feature::CLAUSE_UNLESS); 	}
#line 6010 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 409: /* clause_unless ::= */
#line 2650 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = NULL; }
#line 6015 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 410: /* clause_where ::= WHERE formula_no_const */
#line 2651 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_WHERE); 	}
#line 6020 "bcplus/parser/detail/lemon_parser.c"
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
#line 2697 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy484 = yymsp[0].minor.yy484;}
#line 6042 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 430: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2816 "bcplus/parser/detail/lemon_parser.y"
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
#line 6057 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 431: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2828 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy17, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 6064 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 432: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2832 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy17, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 6071 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 433: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2836 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy484, yymsp[-4].minor.yy17, yymsp[-3].minor.yy0, yymsp[-2].minor.yy17, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 6077 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 434: /* law_impl ::= ARROW_LDASH formula clause_where PERIOD */
#line 2839 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy484, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy17, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 6083 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 435: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2842 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy484, yymsp[-6].minor.yy423, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 6089 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 436: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2846 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy484, yymsp[-8].minor.yy423, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-4].minor.yy299, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 6096 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 437: /* law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2849 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy484, yymsp[-8].minor.yy423, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-4].minor.yy299, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_DECREMENTS, DecrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 6103 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 438: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2853 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy484, yymsp[-6].minor.yy423, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 6109 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 439: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2857 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 6116 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 440: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2861 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 6123 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 441: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2865 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 6130 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 442: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2869 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 6137 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 443: /* law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2873 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy423, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 6144 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 444: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2877 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 6151 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 445: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2881 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 6158 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 446: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2885 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 6164 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 447: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2889 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy484, yymsp[-3].minor.yy0, yymsp[-2].minor.yy225, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 6170 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 448: /* law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD */
#line 2894 "bcplus/parser/detail/lemon_parser.y"
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
#line 6189 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 449: /* stmt_code_blk ::= ASP_GR */
#line 2928 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 6194 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 450: /* stmt_code_blk ::= ASP_CP */
#line 2929 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 6199 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 451: /* stmt_code_blk ::= F2LP_GR */
#line 2930 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 6204 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 452: /* stmt_code_blk ::= F2LP_CP */
#line 2931 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 6209 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 453: /* stmt_code_blk ::= LUA_GR */
#line 2932 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 6214 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 454: /* stmt_code_blk ::= LUA_CP */
#line 2933 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 6219 "bcplus/parser/detail/lemon_parser.c"
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
#line 6285 "bcplus/parser/detail/lemon_parser.c"
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
