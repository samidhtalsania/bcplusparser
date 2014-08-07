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

#line 784 "bcplus/parser/detail/lemon_parser.y"

	#define NUM_UOP(t_new, t, val) \
		ref_ptr<const Referenced> t_ptr = t; \
		t_new = new Number(val, t->beginLoc(), t->endLoc());


	#define NUM_BOP(t_new, l, r, val) \
		ref_ptr<const Referenced> l_ptr = l, r_ptr = r; \
		t_new = new Number(val, l->beginLoc(), r->endLoc());

#line 830 "bcplus/parser/detail/lemon_parser.y"

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

#line 910 "bcplus/parser/detail/lemon_parser.y"


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
	


#line 1042 "bcplus/parser/detail/lemon_parser.y"

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



#line 1111 "bcplus/parser/detail/lemon_parser.y"

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

#line 1508 "bcplus/parser/detail/lemon_parser.y"

		
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
#line 1636 "bcplus/parser/detail/lemon_parser.y"

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
#line 2380 "bcplus/parser/detail/lemon_parser.y"

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

#line 2403 "bcplus/parser/detail/lemon_parser.y"

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
#line 2432 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRangeEval* maxstep;
		Token const* label;
	};

#line 2539 "bcplus/parser/detail/lemon_parser.y"

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

#line 2611 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2697 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2890 "bcplus/parser/detail/lemon_parser.y"

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
#define YYNSTATE 850
#define YYNRULE 448
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
#define YY_ACTTAB_COUNT (3380)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   849,   61,  850,  848,  847,  846,  845,  844,  843,  842,
 /*    10 */   841,  840,  839,  838,  837,  836,  835,  834,   60,  807,
 /*    20 */   317,  833,  826,  832,  831,  825,  146,  144,  142,  141,
 /*    30 */   140,  828,  313,  318,  807,  317,  833,  826,  473,  831,
 /*    40 */   825,  770,  415,  625,  113,  111,  110,  312,  318,   24,
 /*    50 */    23,   61,  636,   25,  330,  763,  760,  759,  758,  757,
 /*    60 */   687,  118,  358,  243,  769,  768,   59,   17,  474,  807,
 /*    70 */   317,  833,  826,  832,  831,  825,  146,  144,  142,  141,
 /*    80 */   140,   58,  312,  318,  237,  235,  233,  232,  231,  361,
 /*    90 */   763,  760,  759,  758,  757,  464,  512,  466,  688,  689,
 /*   100 */   551,  583,  582,  581,  580,  579,  578,  577,  576,  575,
 /*   110 */   574,  573,  572,  571,  570,  569,  568,  567,  566,  806,
 /*   120 */   540,  653,  685,  541,  805,  550,  549,  546,  545,  548,
 /*   130 */   547,  827,  171,  169,  167,  165,  164,  632,  530,  833,
 /*   140 */   826,  832,  831,  825,   45,   57,   12,  296,  199,  200,
 /*   150 */    43,   29,   10,   11,  299,  190,  241,  260,   44,  122,
 /*   160 */   601,  118,  254,  167,  165,  164,  799,  798,  800,    9,
 /*   170 */   495,  494,    8,  308,   42,  246,  320,  605,  602,  600,
 /*   180 */   599,  766,  767,  707,  686,  123,  319,  183,  718,  121,
 /*   190 */   213,  212,  211,  305,  821,  829,  830,  833,  826,  832,
 /*   200 */   831,  825,  479,  414,  683,  682,  681,  680,  290,  631,
 /*   210 */   540,  509,  510,  541,  630,  117,  115,  113,  111,  110,
 /*   220 */   679,  163,  677,  119,   72,   50,   49,  676,  104,   51,
 /*   230 */   702,  701,  703,  631,  540,  709,  544,  541,  630,  675,
 /*   240 */   673,  674,  678,  752,  751,  704,  705,  530,  543,  824,
 /*   250 */   542,   19,  189,  218,  496,  208,  609,  608,  497,  821,
 /*   260 */   829,  830,  833,  826,  832,  831,  825,  478,  414,  765,
 /*   270 */   651,  597,  598,  645,  197,  530,  593,    6,  340,   41,
 /*   280 */   609,  608,  610,  305,  806,  540,  529,  216,  541,  805,
 /*   290 */   191,  265,  214,  191,   71,  597,  598,  745,  197,  192,
 /*   300 */   744,    6,  192,   41,  468,  669,  467,  305,   21,   20,
 /*   310 */    24,   23,   39,   40,   25,   37,   36,   70,  128,   38,
 /*   320 */   538,  807,  317,  833,  826,  832,  831,  825,  233,  232,
 /*   330 */   231,  799,  798,  800,  313,  318,   39,   40,  543,  824,
 /*   340 */   542,  537,  128,  770,  765,   68,  755,  756,  663,  204,
 /*   350 */   358,  774,    3,   25,   28,  306,  806,  540,  305, 1141,
 /*   360 */   541,  805,  543,  824,  542,  304,  769,  768,  538,   22,
 /*   370 */    21,   20,   24,   23, 1141, 1141,   25,  807,  317,  833,
 /*   380 */   826,  473,  831,  825,  687,   69,  358,   26,   27,  649,
 /*   390 */   312,  318,  652,  104, 1141, 1141,  665,  332,  763,  760,
 /*   400 */   759,  758,  757,  799,  798,  800, 1141,  142,  141,  140,
 /*   410 */    18,  474, 1141,  543,  824,  542,   80,  748,  766,  767,
 /*   420 */   517,  466,  688,  689,  183, 1067,  121,  516,  515, 1067,
 /*   430 */   305,  650,  632,  530,  833,  826,  832,  831,  825,   22,
 /*   440 */    21,   20,   24,   23,   56,  195,   25,  209,  178,    2,
 /*   450 */   186,  193,  194,   64,  687,  601,  358,  456,  656,  455,
 /*   460 */   119,  198,  742,  540,  501,  104,  541,  741,  309,  752,
 /*   470 */   751,  339,  605,  602,  600,  599,  202,   32,  823,  448,
 /*   480 */   449,  454,  450,  503,  668,  543,  824,  542,  307,  462,
 /*   490 */   512,  466,  688,  689,  632,  530,  833,  826,  473,  831,
 /*   500 */   825,  518,    7,  660,  182,  446,  587,  586,  118,  735,
 /*   510 */   734,  736,  307,   16,  135,  134,  133,  601,  132,  131,
 /*   520 */   130,  129,  695,  207,  725,  726,  694,   30,  474,  663,
 /*   530 */   309,  358,   54,  322,  605,  602,  600,  599,  148,  139,
 /*   540 */   138,  137,  136,  539,  824,  632,  530,  833,  826,  473,
 /*   550 */   831,  825,  821,  829,  830,  833,  826,  832,  831,  825,
 /*   560 */   477,  414,  672,   29,  185,   52,   53,  539,  601,  771,
 /*   570 */   687,  150,  358,  530,  459,  661,  458,  662,   31,  474,
 /*   580 */   746,  309,  199,  200,  324,  605,  602,  600,  599,  740,
 /*   590 */   305,  543,  824,  542,   51,  765,  241,  722,  157,  156,
 /*   600 */   155,   13,  154,  153,  152,  151,  465,  466,  688,  689,
 /*   610 */   632,  530,  833,  826,  832,  831,  825,  480,    7,  531,
 /*   620 */   266,  629,  162,  161,  160,  159,  158,  690,  691,   16,
 /*   630 */   135,  134,  133,  601,  132,  131,  130,  129,  538,  627,
 /*   640 */   540,  589,  588,  541,  626,  543,  309,  500,  721,  338,
 /*   650 */   605,  602,  600,  599,  148,  139,  138,  137,  136,  535,
 /*   660 */   807,  317,  833,  826,  832,  831,  825,  117,  115,  113,
 /*   670 */   111,  110,   63,  313,  318,  632,  530,  833,  826,  832,
 /*   680 */   831,  825,  770,  765,  591,  590,  621,  620,  622,  820,
 /*   690 */   540,  307,  203,  541,  819,  307,   55,   62,  601,  519,
 /*   700 */   655,  623,  624,  822,  293,  769,  768,  716,  663,  147,
 /*   710 */   358,  607,  471,  710,  595,  605,  602,  600,  599,  788,
 /*   720 */   201,  632,  530,  833,  826,  832,  831,  825,   22,   21,
 /*   730 */    20,   24,   23,  118,  720,   25,  814,  813,  815,   47,
 /*   740 */    46,   50,   49,  145,  601,   51,  539,  543,  143,  172,
 /*   750 */   539,  816,  817,  747,  667,  458,  662,  309,  693,  116,
 /*   760 */   604,  605,  602,  600,  599,  520,  711,  684,  543,  824,
 /*   770 */   542,  671,  821,  829,  830,  833,  826,  832,  831,  825,
 /*   780 */   476,  414, 1242,  118,  592,  719,  807,  317,  833,  826,
 /*   790 */   832,  831,  825,  114,  754,  244,  210,  180,  112,  310,
 /*   800 */   318,  180, 1242,  670,    5,  181,  328,  763,  760,  759,
 /*   810 */   758,  757,  217,  215,  213,  212,  211,  514,  543,  824,
 /*   820 */   542,   22,   21,   20,   24,   23,  713,  714,   25,  241,
 /*   830 */    93,   92,   91,  180,   90,   89,   88,   87,  596,  807,
 /*   840 */   316,  833,  826,  832,  831,  825,  217,  215,  213,  212,
 /*   850 */   211, 1204,  764,  318,   98,   97,   96,   95,   94,  749,
 /*   860 */   763,  760,  759,  758,  757,   35,   34,   33,   37,   36,
 /*   870 */     4,  513,   38,  804,   80,   86,   85, 1204,   84,   83,
 /*   880 */    82,   81,   79,   78,   77,  642,   76,   75,   74,   73,
 /*   890 */   176,  807,  317,  833,  826,  832,  831,  825,  103,  102,
 /*   900 */   101,  100,   99,  179,  312,  318,  109,  108,  107,  106,
 /*   910 */   105,  762,  763,  760,  759,  758,  757,  531,  237,  235,
 /*   920 */   233,  232,  231,  807,  317,  833,  826,  832,  831,  825,
 /*   930 */   664,   22,   21,   20,   24,   23,  312,  318,   25,  594,
 /*   940 */   708,  467,  508,  761,  763,  760,  759,  758,  757,  175,
 /*   950 */   538,  174,   14,  807,  317,  833,  826,  832,  831,  825,
 /*   960 */   180,   35,   34,   33,   37,   36,  312,  318,   38,  242,
 /*   970 */   302,  649,  173,  533,  763,  760,  759,  758,  757,  658,
 /*   980 */   723,  659,  455,  502,  821,  829,  830,  833,  826,  832,
 /*   990 */   831,  825,  472,  414,  807,  317,  833,  826,  832,  831,
 /*  1000 */   825,  654,   48,   47,   46,   50,   49,  312,  318,   51,
 /*  1010 */   752,  751,  425,  646,  532,  763,  760,  759,  758,  757,
 /*  1020 */    22,   21,   20,   24,   23,  184,  647,   25,  644,  917,
 /*  1030 */   917,  917,   38,  917,  917,  917,  917,   79,   78,   77,
 /*  1040 */   818,   76,   75,   74,   73,  543,  807,  317,  833,  826,
 /*  1050 */   832,  831,  825,  917,  917,  917,  917,  917,  149,  312,
 /*  1060 */   318,  109,  108,  107,  106,  105,  382,  763,  760,  759,
 /*  1070 */   758,  757,  196,  117,  115,  113,  111,  110,  807,  317,
 /*  1080 */   833,  826,  832,  831,  825,  538,  469,  712,  453,  503,
 /*  1090 */   804,  312,  318,   98,   97,   96,   95,   94,  434,  763,
 /*  1100 */   760,  759,  758,  757,   29,  445,  649,  585,  807,  317,
 /*  1110 */   833,  826,  832,  831,  825,  103,  102,  101,  100,   99,
 /*  1120 */   289,  312,  318,  109,  108,  107,  106,  105,  433,  763,
 /*  1130 */   760,  759,  758,  757,  452,  503,  451,  503,  821,  829,
 /*  1140 */   830,  833,  826,  832,  831,  825,  188,  413,  648,  807,
 /*  1150 */   317,  833,  826,  832,  831,  825,  584,   22,   21,   20,
 /*  1160 */    24,   23,  312,  318,   25,  426,  503,  240,  302,  333,
 /*  1170 */   763,  760,  759,  758,  757,   48,   47,   46,   50,   49,
 /*  1180 */     4,  565,   51,  564,  157,  156,  155,  563,  154,  153,
 /*  1190 */   152,  151,   79,   78,   77,  784,   76,   75,   74,   73,
 /*  1200 */   562,  807,  317,  833,  826,  832,  831,  825,  162,  161,
 /*  1210 */   160,  159,  158,  561,  312,  318,  109,  108,  107,  106,
 /*  1220 */   105,  331,  763,  760,  759,  758,  757,  560,  171,  169,
 /*  1230 */   167,  165,  164,  807,  317,  833,  826,  832,  831,  825,
 /*  1240 */   538, 1299,    1,  559,  558,  706,  312,  318,  171,  169,
 /*  1250 */   167,  165,  164,  329,  763,  760,  759,  758,  757,  556,
 /*  1260 */   555,  649,  554,  807,  317,  833,  826,  832,  831,  825,
 /*  1270 */   237,  235,  233,  232,  231,  775,  314,  318,  217,  215,
 /*  1280 */   213,  212,  211,  352,  763,  760,  759,  758,  757,  553,
 /*  1290 */   552,  539,  821,  829,  830,  833,  826,  832,  831,  825,
 /*  1300 */   635,  443,  424,  646,  807,  317,  833,  826,  832,  831,
 /*  1310 */   825,  177,   35,   34,   33,   37,   36,  312,  318,   38,
 /*  1320 */   773,  824,  771,  301,  337,  763,  760,  759,  758,  757,
 /*  1330 */   543,   29,  237,  235,  233,  232,  231,  504,  753,   79,
 /*  1340 */    78,   77,  750,   76,   75,   74,   73,  628,  619,  833,
 /*  1350 */   826,  832,  831,  825,  300,  187,  807,  317,  833,  826,
 /*  1360 */   832,  831,  825,  109,  108,  107,  106,  105,  526,  312,
 /*  1370 */   318,   55,   67,  524,  525,  297,  336,  763,  760,  759,
 /*  1380 */   758,  757,  419,  523,  522,  294,  292,  291,  807,  317,
 /*  1390 */   833,  826,  832,  831,  825,  521,   22,   21,   20,   24,
 /*  1400 */    23,  312,  318,   25,  470,  717,  666,  686,  224,  763,
 /*  1410 */   760,  759,  758,  757,  287,   15,   14,  286,  807,  317,
 /*  1420 */   833,  826,  832,  831,  825,  230,  285,  283,  228,  493,
 /*  1430 */   239,  312,  318,  229,  634,  280,  278,  282,  223,  763,
 /*  1440 */   760,  759,  758,  757,  281,  491,  821,  829,  830,  833,
 /*  1450 */   826,  832,  831,  825,  687,  364,  358,  277,  276,  807,
 /*  1460 */   317,  833,  826,  832,  831,  825,  237,  235,  233,  232,
 /*  1470 */   231,  275,  312,  318,  490,  273,  557,  489,  271,  222,
 /*  1480 */   763,  760,  759,  758,  757,  488,  269,  264,  487,  461,
 /*  1490 */   512,  466,  688,  689,  743,  733,  833,  826,  832,  831,
 /*  1500 */   825,  267,  628,  619,  833,  826,  832,  831,  825,  315,
 /*  1510 */   486,  807,  317,  833,  826,  832,  831,  825,  263,  262,
 /*  1520 */   261,  360,  730,  727,  312,  318,  633,  485,  258,  259,
 /*  1530 */   256,  221,  763,  760,  759,  758,  757,  326,  255,  257,
 /*  1540 */   253,  252,  484,  807,  317,  833,  826,  832,  831,  825,
 /*  1550 */   251,  250,  249,  687,  247,  358,  312,  318,  237,  235,
 /*  1560 */   233,  232,  231,  220,  763,  760,  759,  758,  757,  483,
 /*  1570 */   482,  245,  481,  807,  317,  833,  826,  832,  831,  825,
 /*  1580 */   715,  288,  303,  692,  356,  357,  312,  318,  457,  512,
 /*  1590 */   466,  688,  689,  219,  763,  760,  759,  758,  757,  298,
 /*  1600 */   821,  829,  830,  833,  826,  832,  831,  825,  422,  444,
 /*  1610 */   687,  641,  358,  227,  807,  317,  833,  826,  832,  831,
 /*  1620 */   825,  423,  640,  639,  638,  637,  295,  313,  318,  237,
 /*  1630 */   235,  233,  232,  231,  355,  354,  770,  765,  353,  807,
 /*  1640 */   317,  833,  826,  832,  831,  825,  511,  466,  688,  689,
 /*  1650 */   365,  359,  313,  318,  274,  492,  272,  270,  284,  769,
 /*  1660 */   768,  770,  765,  268,  248,  427,  632,  530,  833,  826,
 /*  1670 */   832,  831,  825,   66,  700,  428,  787,  781,  833,  826,
 /*  1680 */   832,  831,  825,  279,  769,  768,  475,  388,  699,  601,
 /*  1690 */   632,  530,  833,  826,  832,  831,  825,   22,   21,   20,
 /*  1700 */    24,   23,  309,  698,   25,  603,  605,  602,  600,  599,
 /*  1710 */   697,  696,  368,  601,  687,  507,  358,  657,  632,  530,
 /*  1720 */   833,  826,  832,  831,  825, 1300,  309, 1300, 1300,  499,
 /*  1730 */   605,  602,  600,  599, 1300, 1300, 1300, 1300, 1300, 1300,
 /*  1740 */  1300,  601,  632,  530,  833,  826,  832,  831,  825, 1300,
 /*  1750 */   463,  466,  688,  689,  309, 1300, 1300,  498,  605,  602,
 /*  1760 */   600,  599,   65, 1300, 1300,  601, 1300, 1300, 1300,  632,
 /*  1770 */   530,  833,  826,  832,  831,  825, 1300,  687,  309,  358,
 /*  1780 */  1300,  366,  605,  602,  600,  599,   22,   21,   20,   24,
 /*  1790 */    23, 1300,  601,   25,  632,  530,  833,  826,  832,  831,
 /*  1800 */   825,   34,   33,   37,   36,  309, 1300,   38,  417,  605,
 /*  1810 */   602,  600,  599,  505,  466,  688,  689,  601, 1300, 1300,
 /*  1820 */  1300,  632,  530,  833,  826,  832,  831,  825,  127, 1300,
 /*  1830 */   309, 1300, 1300,  416,  605,  602,  600,  599,  786,  540,
 /*  1840 */  1300,  643,  541,  785,  601,  632,  530,  833,  826,  832,
 /*  1850 */   831,  825,   35,   34,   33,   37,   36,  309, 1300,   38,
 /*  1860 */   325,  605,  602,  600,  599, 1300, 1300, 1300,  601, 1300,
 /*  1870 */  1300, 1300, 1300,  632,  530,  833,  826,  832,  831,  825,
 /*  1880 */  1300,  309, 1300, 1300,  323,  605,  602,  600,  599,  806,
 /*  1890 */   540, 1300,  772,  541,  805, 1300,  601,  538, 1300, 1300,
 /*  1900 */   782,  783, 1300, 1300, 1300,  772, 1300, 1300,  170,  309,
 /*  1910 */   538,  238,  321,  605,  602,  600,  599, 1300,  536, 1300,
 /*  1920 */  1300,  206,  474, 1300,  743,  733,  833,  826,  832,  831,
 /*  1930 */   825,  534, 1300, 1300,  205,  474,  799,  798,  800,  311,
 /*  1940 */  1300, 1300,  168, 1300,  447,  236, 1300,  166, 1300, 1300,
 /*  1950 */   234,  327,  730,  727, 1300, 1300, 1300, 1300, 1300,  121,
 /*  1960 */   743,  733,  833,  826,  832,  831,  825,  543,  824,  542,
 /*  1970 */  1300, 1300, 1300, 1300, 1300,  732,  743,  733,  833,  826,
 /*  1980 */   832,  831,  825, 1300, 1300, 1300, 1300,  724,  730,  727,
 /*  1990 */  1300,  315, 1300,  120, 1300, 1300, 1300, 1300,  104, 1300,
 /*  2000 */  1300, 1300, 1300,  729,  730,  727,  743,  733,  833,  826,
 /*  2010 */   832,  831,  825, 1300,  238, 1300, 1300, 1300,  543,  824,
 /*  2020 */   542,  315, 1300,  743,  733,  833,  826,  832,  831,  825,
 /*  2030 */  1300, 1300, 1300,  728,  730,  727, 1300, 1300,  315,  743,
 /*  2040 */   733,  833,  826,  832,  831,  825, 1300, 1300,  236, 1300,
 /*  2050 */   528,  730,  727,  234,  315,  743,  733,  833,  826,  832,
 /*  2060 */   831,  825,  460,  506, 1300, 1300,  527,  730,  727, 1300,
 /*  2070 */   315,  743,  733,  833,  826,  832,  831,  825, 1300,  707,
 /*  2080 */   686, 1300,  369,  730,  727, 1300,  315, 1300,  707, 1300,
 /*  2090 */  1300,  743,  733,  833,  826,  832,  831,  825,  430,  730,
 /*  2100 */   727, 1300, 1300, 1300, 1300, 1300,  315, 1300, 1300,  702,
 /*  2110 */   701,  703, 1300, 1300, 1300, 1300, 1300, 1300,  429,  730,
 /*  2120 */   727, 1300, 1300, 1300,  704,  705,  702,  701,  703, 1300,
 /*  2130 */  1300, 1300,  218, 1300, 1300,  702,  701,  703, 1300, 1300,
 /*  2140 */  1300,  704,  705, 1300, 1300, 1300, 1300, 1300, 1300,  218,
 /*  2150 */   704,  705, 1300, 1300, 1300, 1300, 1300, 1300,  218,  807,
 /*  2160 */   411,  833,  826,  832,  831,  825,  216, 1300, 1300, 1300,
 /*  2170 */  1300,  214,  363,  412, 1300, 1300,  807,  795,  833,  826,
 /*  2180 */   832,  831,  825,  216, 1300, 1300, 1300, 1300,  214,  797,
 /*  2190 */   412, 1300,  216, 1300, 1300, 1300, 1300,  214,  807,  334,
 /*  2200 */   833,  826,  832,  831,  825, 1300, 1300, 1300, 1300, 1300,
 /*  2210 */  1300,  797,  412,  821,  829,  830,  833,  826,  832,  831,
 /*  2220 */   825, 1300,  812, 1300,  821,  829,  830,  833,  826,  832,
 /*  2230 */   831,  825, 1300,  808,  821,  829,  830,  833,  826,  832,
 /*  2240 */   831,  825, 1300,  811, 1300,  821,  829,  830,  833,  826,
 /*  2250 */   832,  831,  825, 1300,  810, 1300, 1300, 1300, 1300, 1300,
 /*  2260 */   821,  829,  830,  833,  826,  832,  831,  825, 1300,  809,
 /*  2270 */  1300, 1300,  821,  829,  830,  833,  826,  832,  831,  825,
 /*  2280 */  1300,  442,  821,  829,  830,  833,  826,  832,  831,  825,
 /*  2290 */  1300,  441, 1300, 1300, 1300,  821,  829,  830,  833,  826,
 /*  2300 */   832,  831,  825, 1300,  803,  821,  829,  830,  833,  826,
 /*  2310 */   832,  831,  825, 1300,  802,  821,  829,  830,  833,  826,
 /*  2320 */   832,  831,  825, 1300,  801,  821,  829,  830,  833,  826,
 /*  2330 */   832,  831,  825, 1300,  796,  821,  829,  830,  833,  826,
 /*  2340 */   832,  831,  825, 1300,  440, 1300,  821,  829,  830,  833,
 /*  2350 */   826,  832,  831,  825, 1300,  439,  821,  829,  830,  833,
 /*  2360 */   826,  832,  831,  825, 1300,  794,  821,  829,  830,  833,
 /*  2370 */   826,  832,  831,  825, 1300,  793,  821,  829,  830,  833,
 /*  2380 */   826,  832,  831,  825, 1300,  792, 1300,  821,  829,  830,
 /*  2390 */   833,  826,  832,  831,  825, 1300,  438,  821,  829,  830,
 /*  2400 */   833,  826,  832,  831,  825, 1300,  437, 1300,  821,  829,
 /*  2410 */   830,  833,  826,  832,  831,  825, 1300,  791,  821,  829,
 /*  2420 */   830,  833,  826,  832,  831,  825, 1300,  790,  821,  829,
 /*  2430 */   830,  833,  826,  832,  831,  825, 1300,  789,  821,  829,
 /*  2440 */   830,  833,  826,  832,  831,  825, 1300,  410, 1300,  821,
 /*  2450 */   829,  830,  833,  826,  832,  831,  825, 1300,  409,  821,
 /*  2460 */   829,  830,  833,  826,  832,  831,  825, 1300,  408,  821,
 /*  2470 */   829,  830,  833,  826,  832,  831,  825, 1300,  407,  821,
 /*  2480 */   829,  830,  833,  826,  832,  831,  825, 1300,  406,  821,
 /*  2490 */   829,  830,  833,  826,  832,  831,  825, 1300,  405, 1300,
 /*  2500 */   821,  829,  830,  833,  826,  832,  831,  825, 1300,  404,
 /*  2510 */   821,  829,  830,  833,  826,  832,  831,  825, 1300,  403,
 /*  2520 */   821,  829,  830,  833,  826,  832,  831,  825, 1300,  402,
 /*  2530 */   821,  829,  830,  833,  826,  832,  831,  825, 1300,  401,
 /*  2540 */  1300,  821,  829,  830,  833,  826,  832,  831,  825, 1300,
 /*  2550 */   400,  821,  829,  830,  833,  826,  832,  831,  825, 1300,
 /*  2560 */   399, 1300,  821,  829,  830,  833,  826,  832,  831,  825,
 /*  2570 */  1300,  398,  821,  829,  830,  833,  826,  832,  831,  825,
 /*  2580 */  1300,  397,  821,  829,  830,  833,  826,  832,  831,  825,
 /*  2590 */  1300,  396,  821,  829,  830,  833,  826,  832,  831,  825,
 /*  2600 */  1300,  395, 1300,  821,  829,  830,  833,  826,  832,  831,
 /*  2610 */   825, 1300,  394,  821,  829,  830,  833,  826,  832,  831,
 /*  2620 */   825, 1300,  393,  821,  829,  830,  833,  826,  832,  831,
 /*  2630 */   825, 1300,  392,  821,  829,  830,  833,  826,  832,  831,
 /*  2640 */   825, 1300,  391,  821,  829,  830,  833,  826,  832,  831,
 /*  2650 */   825, 1300,  390, 1300,  821,  829,  830,  833,  826,  832,
 /*  2660 */   831,  825, 1300,  386,  821,  829,  830,  833,  826,  832,
 /*  2670 */   831,  825, 1300,  385,  821,  829,  830,  833,  826,  832,
 /*  2680 */   831,  825, 1300,  384,  821,  829,  830,  833,  826,  832,
 /*  2690 */   831,  825, 1300,  383, 1300,  821,  829,  830,  833,  826,
 /*  2700 */   832,  831,  825, 1300,  381,  821,  829,  830,  833,  826,
 /*  2710 */   832,  831,  825, 1300,  380, 1300,  821,  829,  830,  833,
 /*  2720 */   826,  832,  831,  825, 1300,  379,  821,  829,  830,  833,
 /*  2730 */   826,  832,  831,  825, 1300,  378,  821,  829,  830,  833,
 /*  2740 */   826,  832,  831,  825, 1300,  377,  821,  829,  830,  833,
 /*  2750 */   826,  832,  831,  825, 1300,  226, 1300,  821,  829,  830,
 /*  2760 */   833,  826,  832,  831,  825, 1300,  225,  821,  829,  830,
 /*  2770 */   833,  826,  832,  831,  825, 1300,  367,  787,  781,  833,
 /*  2780 */   826,  832,  831,  825, 1300, 1300, 1300, 1300,  389,  787,
 /*  2790 */   781,  833,  826,  832,  831,  825, 1300, 1300, 1300, 1300,
 /*  2800 */   435,  787,  781,  833,  826,  832,  831,  825, 1300, 1300,
 /*  2810 */  1300, 1300,  362,  787,  781,  833,  826,  832,  831,  825,
 /*  2820 */  1300, 1300, 1300, 1300,  436,  787,  781,  833,  826,  832,
 /*  2830 */   831,  825, 1300, 1300, 1300, 1300,  780, 1300,  787,  781,
 /*  2840 */   833,  826,  832,  831,  825, 1300, 1300, 1300, 1300,  776,
 /*  2850 */   787,  781,  833,  826,  832,  831,  825, 1300, 1300, 1300,
 /*  2860 */  1300,  779,  787,  781,  833,  826,  832,  831,  825, 1300,
 /*  2870 */  1300, 1300, 1300,  778,  787,  781,  833,  826,  832,  831,
 /*  2880 */   825, 1300, 1300, 1300, 1300,  777,  787,  781,  833,  826,
 /*  2890 */   832,  831,  825, 1300, 1300, 1300, 1300,  387,  787,  781,
 /*  2900 */   833,  826,  832,  831,  825, 1300, 1300, 1300, 1300,  432,
 /*  2910 */   787,  781,  833,  826,  832,  831,  825, 1300, 1300, 1300,
 /*  2920 */  1300,  431,  787,  781,  833,  826,  832,  831,  825, 1300,
 /*  2930 */  1300, 1300, 1300,  739,  787,  781,  833,  826,  832,  831,
 /*  2940 */   825, 1300, 1300, 1300, 1300,  738,  787,  781,  833,  826,
 /*  2950 */   832,  831,  825, 1300, 1300, 1300, 1300,  737,  787,  781,
 /*  2960 */   833,  826,  832,  831,  825, 1300, 1300, 1300, 1300,  376,
 /*  2970 */   787,  781,  833,  826,  832,  831,  825, 1300, 1300, 1300,
 /*  2980 */  1300,  375,  787,  781,  833,  826,  832,  831,  825, 1300,
 /*  2990 */  1300, 1300, 1300,  374,  787,  781,  833,  826,  832,  831,
 /*  3000 */   825, 1300, 1300, 1300, 1300,  373,  787,  781,  833,  826,
 /*  3010 */   832,  831,  825, 1300, 1300, 1300, 1300,  372,  787,  781,
 /*  3020 */   833,  826,  832,  831,  825, 1300, 1300, 1300, 1300,  371,
 /*  3030 */   787,  781,  833,  826,  832,  831,  825, 1300, 1300, 1300,
 /*  3040 */  1300,  370,  787,  781,  833,  826,  832,  831,  825, 1300,
 /*  3050 */  1300, 1300, 1300,  731,  787,  781,  833,  826,  832,  831,
 /*  3060 */   825, 1300, 1300, 1300, 1300,  335,  628,  619,  833,  826,
 /*  3070 */   832,  831,  825, 1300, 1300, 1300,  628,  619,  833,  826,
 /*  3080 */   832,  831,  825,  628,  619,  833,  826,  832,  831,  825,
 /*  3090 */   628,  619,  833,  826,  832,  831,  825, 1300, 1300, 1300,
 /*  3100 */  1300,  420,  628,  619,  833,  826,  832,  831,  825, 1300,
 /*  3110 */  1300,  618, 1300, 1300, 1300, 1300, 1300, 1300,  421, 1300,
 /*  3120 */  1300, 1300, 1300, 1300, 1300,  617,  628,  619,  833,  826,
 /*  3130 */   832,  831,  825, 1300, 1300, 1300, 1300,  616,  628,  619,
 /*  3140 */   833,  826,  832,  831,  825, 1300, 1300, 1300,  628,  619,
 /*  3150 */   833,  826,  832,  831,  825, 1300, 1300, 1300, 1300, 1300,
 /*  3160 */  1300,  615,  628,  619,  833,  826,  832,  831,  825, 1300,
 /*  3170 */  1300, 1300, 1300,  614,  628,  619,  833,  826,  832,  831,
 /*  3180 */   825, 1300, 1300,  418,  628,  619,  833,  826,  832,  831,
 /*  3190 */   825, 1300, 1300, 1300, 1300, 1300, 1300,  613,  628,  619,
 /*  3200 */   833,  826,  832,  831,  825, 1300, 1300, 1300, 1300,  612,
 /*  3210 */   628,  619,  833,  826,  832,  831,  825, 1300, 1300,  611,
 /*  3220 */   628,  619,  833,  826,  832,  831,  825, 1300, 1300, 1300,
 /*  3230 */  1300, 1300, 1300,  351,  628,  619,  833,  826,  832,  831,
 /*  3240 */   825, 1300, 1300, 1300, 1300,  350,  628,  619,  833,  826,
 /*  3250 */   832,  831,  825, 1300, 1300,  349,  628,  619,  833,  826,
 /*  3260 */   832,  831,  825, 1300, 1300, 1300, 1300, 1300, 1300,  348,
 /*  3270 */  1300, 1300, 1300,  628,  619,  833,  826,  832,  831,  825,
 /*  3280 */  1300,  347,  628,  619,  833,  826,  832,  831,  825, 1300,
 /*  3290 */  1300,  346,  628,  619,  833,  826,  832,  831,  825,  628,
 /*  3300 */   619,  833,  826,  832,  831,  825, 1300, 1300,  345,  628,
 /*  3310 */   619,  833,  826,  832,  831,  825, 1300,  606,  628,  619,
 /*  3320 */   833,  826,  832,  831,  825, 1300, 1300,  344, 1300, 1300,
 /*  3330 */   126, 1300, 1300, 1300,  343, 1300, 1300, 1300, 1300, 1300,
 /*  3340 */   125, 1300, 1300, 1300,  342, 1300, 1300, 1300,  124, 1300,
 /*  3350 */  1300, 1300, 1300,  341,   35,   34,   33,   37,   36, 1300,
 /*  3360 */  1300,   38, 1300, 1300,   35,   34,   33,   37,   36, 1300,
 /*  3370 */  1300,   38,   35,   34,   33,   37,   36, 1300, 1300,   38,
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
 /*   310 */    97,   98,  104,  105,  101,   97,   98,   70,  110,  101,
 /*   320 */   163,  154,  155,  156,  157,  158,  159,  160,  107,  108,
 /*   330 */   109,   48,   49,   50,  167,  168,  104,  105,  130,  131,
 /*   340 */   132,  184,  110,  176,  177,   70,   63,   64,  170,   66,
 /*   350 */   172,   98,   69,  101,   71,  102,    1,    2,   75,   26,
 /*   360 */     5,    6,  130,  131,  132,  198,  199,  200,  163,   94,
 /*   370 */    95,   96,   97,   98,   41,   42,  101,  154,  155,  156,
 /*   380 */   157,  158,  159,  160,  170,   70,  172,  104,  105,  184,
 /*   390 */   167,  168,    2,  110,   61,   62,  218,  174,  175,  176,
 /*   400 */   177,  178,  179,   48,   49,   50,   73,  107,  108,  109,
 /*   410 */   187,  188,   79,  130,  131,  132,   81,   72,   63,   64,
 /*   420 */   206,  207,  208,  209,   69,   98,   71,  213,  214,  102,
 /*   430 */    75,  226,  154,  155,  156,  157,  158,  159,  160,   94,
 /*   440 */    95,   96,   97,   98,   71,   14,  101,   16,   17,   18,
 /*   450 */    19,   20,   21,   81,  170,  177,  172,  219,  220,  221,
 /*   460 */   105,   71,    1,    2,   27,  110,    5,    6,  190,   90,
 /*   470 */    91,  193,  194,  195,  196,  197,   71,   98,   72,   48,
 /*   480 */    49,  222,  223,  224,   73,  130,  131,  132,   75,  205,
 /*   490 */   206,  207,  208,  209,  154,  155,  156,  157,  158,  159,
 /*   500 */   160,   96,   69,   73,   93,  227,  228,  229,  102,   48,
 /*   510 */    49,   50,   75,   80,   81,   82,   83,  177,   85,   86,
 /*   520 */    87,   88,  107,   93,   63,   64,  111,  187,  188,  170,
 /*   530 */   190,  172,   71,  193,  194,  195,  196,  197,  105,  106,
 /*   540 */   107,  108,  109,  130,  131,  154,  155,  156,  157,  158,
 /*   550 */   159,  160,  153,  154,  155,  156,  157,  158,  159,  160,
 /*   560 */   161,  162,   72,   41,  133,  104,  105,  130,  177,  155,
 /*   570 */   170,  110,  172,  155,  215,  216,  217,  218,  187,  188,
 /*   580 */   166,  190,   99,  100,  193,  194,  195,  196,  197,   72,
 /*   590 */    75,  130,  131,  132,  101,  177,  106,   73,   81,   82,
 /*   600 */    83,   79,   85,   86,   87,   88,  206,  207,  208,  209,
 /*   610 */   154,  155,  156,  157,  158,  159,  160,  199,   69,    2,
 /*   620 */   105,   72,  105,  106,  107,  108,  109,    1,    2,   80,
 /*   630 */    81,   82,   83,  177,   85,   86,   87,   88,  163,    1,
 /*   640 */     2,    1,    2,    5,    6,  130,  190,   27,   73,  193,
 /*   650 */   194,  195,  196,  197,  105,  106,  107,  108,  109,  184,
 /*   660 */   154,  155,  156,  157,  158,  159,  160,  105,  106,  107,
 /*   670 */   108,  109,   31,  167,  168,  154,  155,  156,  157,  158,
 /*   680 */   159,  160,  176,  177,  228,  229,   48,   49,   50,    1,
 /*   690 */     2,   75,   75,    5,    6,   75,   62,   31,  177,   96,
 /*   700 */    73,   63,   64,   72,  198,  199,  200,   74,  170,   71,
 /*   710 */   172,  190,  201,  202,  193,  194,  195,  196,  197,   72,
 /*   720 */    93,  154,  155,  156,  157,  158,  159,  160,   94,   95,
 /*   730 */    96,   97,   98,  102,   73,  101,   48,   49,   50,   95,
 /*   740 */    96,   97,   98,  105,  177,  101,  130,  130,  110,  102,
 /*   750 */   130,   63,   64,   72,  216,  217,  218,  190,  132,   71,
 /*   760 */   193,  194,  195,  196,  197,   72,   74,   72,  130,  131,
 /*   770 */   132,   72,  153,  154,  155,  156,  157,  158,  159,  160,
 /*   780 */   161,  162,   73,  102,   73,   73,  154,  155,  156,  157,
 /*   790 */   158,  159,  160,  105,   67,  102,   89,  102,  110,  167,
 /*   800 */   168,  102,   93,   72,   93,   71,  174,  175,  176,  177,
 /*   810 */   178,  179,  105,  106,  107,  108,  109,   55,  130,  131,
 /*   820 */   132,   94,   95,   96,   97,   98,    3,    4,  101,  106,
 /*   830 */    81,   82,   83,  102,   85,   86,   87,   88,   67,  154,
 /*   840 */   155,  156,  157,  158,  159,  160,  105,  106,  107,  108,
 /*   850 */   109,   76,  167,  168,  105,  106,  107,  108,  109,  174,
 /*   860 */   175,  176,  177,  178,  179,   94,   95,   96,   97,   98,
 /*   870 */    69,    2,  101,   72,   81,   82,   83,  102,   85,   86,
 /*   880 */    87,   88,   81,   82,   83,   72,   85,   86,   87,   88,
 /*   890 */    71,  154,  155,  156,  157,  158,  159,  160,  105,  106,
 /*   900 */   107,  108,  109,   71,  167,  168,  105,  106,  107,  108,
 /*   910 */   109,  174,  175,  176,  177,  178,  179,    2,  105,  106,
 /*   920 */   107,  108,  109,  154,  155,  156,  157,  158,  159,  160,
 /*   930 */    72,   94,   95,   96,   97,   98,  167,  168,  101,   72,
 /*   940 */   211,  212,    2,  174,  175,  176,  177,  178,  179,   71,
 /*   950 */   163,   71,   26,  154,  155,  156,  157,  158,  159,  160,
 /*   960 */   102,   94,   95,   96,   97,   98,  167,  168,  101,  185,
 /*   970 */   186,  184,   71,  174,  175,  176,  177,  178,  179,    2,
 /*   980 */    72,  220,  221,  102,  153,  154,  155,  156,  157,  158,
 /*   990 */   159,  160,  161,  162,  154,  155,  156,  157,  158,  159,
 /*  1000 */   160,    2,   94,   95,   96,   97,   98,  167,  168,  101,
 /*  1010 */    90,   91,  225,  226,  174,  175,  176,  177,  178,  179,
 /*  1020 */    94,   95,   96,   97,   98,   69,   73,  101,   73,   81,
 /*  1030 */    82,   83,  101,   85,   86,   87,   88,   81,   82,   83,
 /*  1040 */    72,   85,   86,   87,   88,  130,  154,  155,  156,  157,
 /*  1050 */   158,  159,  160,  105,  106,  107,  108,  109,   65,  167,
 /*  1060 */   168,  105,  106,  107,  108,  109,  174,  175,  176,  177,
 /*  1070 */   178,  179,   76,  105,  106,  107,  108,  109,  154,  155,
 /*  1080 */   156,  157,  158,  159,  160,  163,  203,  204,  223,  224,
 /*  1090 */    72,  167,  168,  105,  106,  107,  108,  109,  174,  175,
 /*  1100 */   176,  177,  178,  179,   41,   76,  184,   73,  154,  155,
 /*  1110 */   156,  157,  158,  159,  160,  105,  106,  107,  108,  109,
 /*  1120 */    76,  167,  168,  105,  106,  107,  108,  109,  174,  175,
 /*  1130 */   176,  177,  178,  179,  223,  224,  223,  224,  153,  154,
 /*  1140 */   155,  156,  157,  158,  159,  160,  102,  162,  226,  154,
 /*  1150 */   155,  156,  157,  158,  159,  160,   73,   94,   95,   96,
 /*  1160 */    97,   98,  167,  168,  101,  223,  224,  185,  186,  174,
 /*  1170 */   175,  176,  177,  178,  179,   94,   95,   96,   97,   98,
 /*  1180 */    69,   73,  101,   73,   81,   82,   83,   73,   85,   86,
 /*  1190 */    87,   88,   81,   82,   83,   72,   85,   86,   87,   88,
 /*  1200 */    73,  154,  155,  156,  157,  158,  159,  160,  105,  106,
 /*  1210 */   107,  108,  109,   73,  167,  168,  105,  106,  107,  108,
 /*  1220 */   109,  174,  175,  176,  177,  178,  179,   73,  105,  106,
 /*  1230 */   107,  108,  109,  154,  155,  156,  157,  158,  159,  160,
 /*  1240 */   163,  136,  137,   73,   73,   72,  167,  168,  105,  106,
 /*  1250 */   107,  108,  109,  174,  175,  176,  177,  178,  179,   73,
 /*  1260 */    73,  184,   73,  154,  155,  156,  157,  158,  159,  160,
 /*  1270 */   105,  106,  107,  108,  109,  163,  167,  168,  105,  106,
 /*  1280 */   107,  108,  109,  174,  175,  176,  177,  178,  179,   73,
 /*  1290 */    73,  130,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  1300 */    73,  162,  225,  226,  154,  155,  156,  157,  158,  159,
 /*  1310 */   160,   76,   94,   95,   96,   97,   98,  167,  168,  101,
 /*  1320 */   158,  131,  155,  231,  174,  175,  176,  177,  178,  179,
 /*  1330 */   130,   41,  105,  106,  107,  108,  109,  102,  158,   81,
 /*  1340 */    82,   83,  158,   85,   86,   87,   88,  154,  155,  156,
 /*  1350 */   157,  158,  159,  160,  234,   61,  154,  155,  156,  157,
 /*  1360 */   158,  159,  160,  105,  106,  107,  108,  109,  235,  167,
 /*  1370 */   168,   62,   70,  235,  155,  234,  174,  175,  176,  177,
 /*  1380 */   178,  179,  189,  155,  235,  234,  231,  234,  154,  155,
 /*  1390 */   156,  157,  158,  159,  160,  235,   94,   95,   96,   97,
 /*  1400 */    98,  167,  168,  101,    2,  202,  209,    2,  174,  175,
 /*  1410 */   176,  177,  178,  179,  233,   42,   26,  232,  154,  155,
 /*  1420 */   156,  157,  158,  159,  160,   76,  234,  231,   76,  235,
 /*  1430 */    81,  167,  168,   81,   73,  234,  231,  233,  174,  175,
 /*  1440 */   176,  177,  178,  179,  232,  235,  153,  154,  155,  156,
 /*  1450 */   157,  158,  159,  160,  170,  162,  172,  233,  232,  154,
 /*  1460 */   155,  156,  157,  158,  159,  160,  105,  106,  107,  108,
 /*  1470 */   109,  234,  167,  168,  235,  234,  155,  235,  234,  174,
 /*  1480 */   175,  176,  177,  178,  179,  235,  234,  231,  235,  205,
 /*  1490 */   206,  207,  208,  209,  154,  155,  156,  157,  158,  159,
 /*  1500 */   160,  234,  154,  155,  156,  157,  158,  159,  160,  169,
 /*  1510 */   235,  154,  155,  156,  157,  158,  159,  160,  233,  232,
 /*  1520 */   234,  181,  182,  183,  167,  168,   73,  235,  231,  155,
 /*  1530 */   232,  174,  175,  176,  177,  178,  179,  189,  234,  233,
 /*  1540 */   155,  231,  235,  154,  155,  156,  157,  158,  159,  160,
 /*  1550 */   233,  232,  234,  170,  234,  172,  167,  168,  105,  106,
 /*  1560 */   107,  108,  109,  174,  175,  176,  177,  178,  179,  235,
 /*  1570 */   235,  155,  235,  154,  155,  156,  157,  158,  159,  160,
 /*  1580 */   204,  231,  186,  157,  173,  173,  167,  168,  205,  206,
 /*  1590 */   207,  208,  209,  174,  175,  176,  177,  178,  179,  231,
 /*  1600 */   153,  154,  155,  156,  157,  158,  159,  160,  173,  162,
 /*  1610 */   170,  173,  172,   89,  154,  155,  156,  157,  158,  159,
 /*  1620 */   160,  173,  173,  173,  173,  173,  231,  167,  168,  105,
 /*  1630 */   106,  107,  108,  109,  173,  173,  176,  177,  173,  154,
 /*  1640 */   155,  156,  157,  158,  159,  160,  206,  207,  208,  209,
 /*  1650 */   173,  172,  167,  168,  232,  235,  232,  232,  198,  199,
 /*  1660 */   200,  176,  177,  232,  231,  172,  154,  155,  156,  157,
 /*  1670 */   158,  159,  160,   70,  172,  172,  154,  155,  156,  157,
 /*  1680 */   158,  159,  160,  198,  199,  200,  164,  165,  172,  177,
 /*  1690 */   154,  155,  156,  157,  158,  159,  160,   94,   95,   96,
 /*  1700 */    97,   98,  190,  172,  101,  193,  194,  195,  196,  197,
 /*  1710 */   172,  172,  172,  177,  170,    2,  172,    2,  154,  155,
 /*  1720 */   156,  157,  158,  159,  160,  254,  190,  254,  254,  193,
 /*  1730 */   194,  195,  196,  197,  254,  254,  254,  254,  254,  254,
 /*  1740 */   254,  177,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  1750 */   206,  207,  208,  209,  190,  254,  254,  193,  194,  195,
 /*  1760 */   196,  197,   70,  254,  254,  177,  254,  254,  254,  154,
 /*  1770 */   155,  156,  157,  158,  159,  160,  254,  170,  190,  172,
 /*  1780 */   254,  193,  194,  195,  196,  197,   94,   95,   96,   97,
 /*  1790 */    98,  254,  177,  101,  154,  155,  156,  157,  158,  159,
 /*  1800 */   160,   95,   96,   97,   98,  190,  254,  101,  193,  194,
 /*  1810 */   195,  196,  197,  206,  207,  208,  209,  177,  254,  254,
 /*  1820 */   254,  154,  155,  156,  157,  158,  159,  160,   70,  254,
 /*  1830 */   190,  254,  254,  193,  194,  195,  196,  197,    1,    2,
 /*  1840 */   254,    1,    5,    6,  177,  154,  155,  156,  157,  158,
 /*  1850 */   159,  160,   94,   95,   96,   97,   98,  190,  254,  101,
 /*  1860 */   193,  194,  195,  196,  197,  254,  254,  254,  177,  254,
 /*  1870 */   254,  254,  254,  154,  155,  156,  157,  158,  159,  160,
 /*  1880 */   254,  190,  254,  254,  193,  194,  195,  196,  197,    1,
 /*  1890 */     2,  254,  158,    5,    6,  254,  177,  163,  254,  254,
 /*  1900 */    63,   64,  254,  254,  254,  158,  254,  254,   71,  190,
 /*  1910 */   163,   71,  193,  194,  195,  196,  197,  254,  184,  254,
 /*  1920 */   254,  187,  188,  254,  154,  155,  156,  157,  158,  159,
 /*  1930 */   160,  184,  254,  254,  187,  188,   48,   49,   50,  169,
 /*  1940 */   254,  254,  105,  254,    1,  105,  254,  110,  254,  254,
 /*  1950 */   110,  181,  182,  183,  254,  254,  254,  254,  254,   71,
 /*  1960 */   154,  155,  156,  157,  158,  159,  160,  130,  131,  132,
 /*  1970 */   254,  254,  254,  254,  254,  169,  154,  155,  156,  157,
 /*  1980 */   158,  159,  160,  254,  254,  254,  254,  181,  182,  183,
 /*  1990 */   254,  169,  254,  105,  254,  254,  254,  254,  110,  254,
 /*  2000 */   254,  254,  254,  181,  182,  183,  154,  155,  156,  157,
 /*  2010 */   158,  159,  160,  254,   71,  254,  254,  254,  130,  131,
 /*  2020 */   132,  169,  254,  154,  155,  156,  157,  158,  159,  160,
 /*  2030 */   254,  254,  254,  181,  182,  183,  254,  254,  169,  154,
 /*  2040 */   155,  156,  157,  158,  159,  160,  254,  254,  105,  254,
 /*  2050 */   181,  182,  183,  110,  169,  154,  155,  156,  157,  158,
 /*  2060 */   159,  160,    1,    2,  254,  254,  181,  182,  183,  254,
 /*  2070 */   169,  154,  155,  156,  157,  158,  159,  160,  254,    1,
 /*  2080 */     2,  254,  181,  182,  183,  254,  169,  254,    1,  254,
 /*  2090 */   254,  154,  155,  156,  157,  158,  159,  160,  181,  182,
 /*  2100 */   183,  254,  254,  254,  254,  254,  169,  254,  254,   48,
 /*  2110 */    49,   50,  254,  254,  254,  254,  254,  254,  181,  182,
 /*  2120 */   183,  254,  254,  254,   63,   64,   48,   49,   50,  254,
 /*  2130 */   254,  254,   71,  254,  254,   48,   49,   50,  254,  254,
 /*  2140 */   254,   63,   64,  254,  254,  254,  254,  254,  254,   71,
 /*  2150 */    63,   64,  254,  254,  254,  254,  254,  254,   71,  154,
 /*  2160 */   155,  156,  157,  158,  159,  160,  105,  254,  254,  254,
 /*  2170 */   254,  110,  167,  168,  254,  254,  154,  155,  156,  157,
 /*  2180 */   158,  159,  160,  105,  254,  254,  254,  254,  110,  167,
 /*  2190 */   168,  254,  105,  254,  254,  254,  254,  110,  154,  155,
 /*  2200 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  254,
 /*  2210 */   254,  167,  168,  153,  154,  155,  156,  157,  158,  159,
 /*  2220 */   160,  254,  162,  254,  153,  154,  155,  156,  157,  158,
 /*  2230 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2240 */   159,  160,  254,  162,  254,  153,  154,  155,  156,  157,
 /*  2250 */   158,  159,  160,  254,  162,  254,  254,  254,  254,  254,
 /*  2260 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2270 */   254,  254,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2280 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2290 */   254,  162,  254,  254,  254,  153,  154,  155,  156,  157,
 /*  2300 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2310 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2320 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2330 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2340 */   158,  159,  160,  254,  162,  254,  153,  154,  155,  156,
 /*  2350 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2360 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2370 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2380 */   157,  158,  159,  160,  254,  162,  254,  153,  154,  155,
 /*  2390 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2400 */   156,  157,  158,  159,  160,  254,  162,  254,  153,  154,
 /*  2410 */   155,  156,  157,  158,  159,  160,  254,  162,  153,  154,
 /*  2420 */   155,  156,  157,  158,  159,  160,  254,  162,  153,  154,
 /*  2430 */   155,  156,  157,  158,  159,  160,  254,  162,  153,  154,
 /*  2440 */   155,  156,  157,  158,  159,  160,  254,  162,  254,  153,
 /*  2450 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2460 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2470 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2480 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  153,
 /*  2490 */   154,  155,  156,  157,  158,  159,  160,  254,  162,  254,
 /*  2500 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2510 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2520 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2530 */   153,  154,  155,  156,  157,  158,  159,  160,  254,  162,
 /*  2540 */   254,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2550 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2560 */   162,  254,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2570 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2580 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2590 */   254,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2600 */   254,  162,  254,  153,  154,  155,  156,  157,  158,  159,
 /*  2610 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2620 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2630 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2640 */   160,  254,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2650 */   160,  254,  162,  254,  153,  154,  155,  156,  157,  158,
 /*  2660 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2670 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2680 */   159,  160,  254,  162,  153,  154,  155,  156,  157,  158,
 /*  2690 */   159,  160,  254,  162,  254,  153,  154,  155,  156,  157,
 /*  2700 */   158,  159,  160,  254,  162,  153,  154,  155,  156,  157,
 /*  2710 */   158,  159,  160,  254,  162,  254,  153,  154,  155,  156,
 /*  2720 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2730 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2740 */   157,  158,  159,  160,  254,  162,  153,  154,  155,  156,
 /*  2750 */   157,  158,  159,  160,  254,  162,  254,  153,  154,  155,
 /*  2760 */   156,  157,  158,  159,  160,  254,  162,  153,  154,  155,
 /*  2770 */   156,  157,  158,  159,  160,  254,  162,  154,  155,  156,
 /*  2780 */   157,  158,  159,  160,  254,  254,  254,  254,  165,  154,
 /*  2790 */   155,  156,  157,  158,  159,  160,  254,  254,  254,  254,
 /*  2800 */   165,  154,  155,  156,  157,  158,  159,  160,  254,  254,
 /*  2810 */   254,  254,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2820 */   254,  254,  254,  254,  165,  154,  155,  156,  157,  158,
 /*  2830 */   159,  160,  254,  254,  254,  254,  165,  254,  154,  155,
 /*  2840 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  2850 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  2860 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2870 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  2880 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  2890 */   158,  159,  160,  254,  254,  254,  254,  165,  154,  155,
 /*  2900 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  2910 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  2920 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2930 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  2940 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  2950 */   158,  159,  160,  254,  254,  254,  254,  165,  154,  155,
 /*  2960 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  2970 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  2980 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  2990 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  3000 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  3010 */   158,  159,  160,  254,  254,  254,  254,  165,  154,  155,
 /*  3020 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  165,
 /*  3030 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3040 */   254,  165,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3050 */   254,  254,  254,  165,  154,  155,  156,  157,  158,  159,
 /*  3060 */   160,  254,  254,  254,  254,  165,  154,  155,  156,  157,
 /*  3070 */   158,  159,  160,  254,  254,  254,  154,  155,  156,  157,
 /*  3080 */   158,  159,  160,  154,  155,  156,  157,  158,  159,  160,
 /*  3090 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3100 */   254,  189,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3110 */   254,  189,  254,  254,  254,  254,  254,  254,  189,  254,
 /*  3120 */   254,  254,  254,  254,  254,  189,  154,  155,  156,  157,
 /*  3130 */   158,  159,  160,  254,  254,  254,  254,  189,  154,  155,
 /*  3140 */   156,  157,  158,  159,  160,  254,  254,  254,  154,  155,
 /*  3150 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  254,
 /*  3160 */   254,  189,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3170 */   254,  254,  254,  189,  154,  155,  156,  157,  158,  159,
 /*  3180 */   160,  254,  254,  189,  154,  155,  156,  157,  158,  159,
 /*  3190 */   160,  254,  254,  254,  254,  254,  254,  189,  154,  155,
 /*  3200 */   156,  157,  158,  159,  160,  254,  254,  254,  254,  189,
 /*  3210 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  189,
 /*  3220 */   154,  155,  156,  157,  158,  159,  160,  254,  254,  254,
 /*  3230 */   254,  254,  254,  189,  154,  155,  156,  157,  158,  159,
 /*  3240 */   160,  254,  254,  254,  254,  189,  154,  155,  156,  157,
 /*  3250 */   158,  159,  160,  254,  254,  189,  154,  155,  156,  157,
 /*  3260 */   158,  159,  160,  254,  254,  254,  254,  254,  254,  189,
 /*  3270 */   254,  254,  254,  154,  155,  156,  157,  158,  159,  160,
 /*  3280 */   254,  189,  154,  155,  156,  157,  158,  159,  160,  254,
 /*  3290 */   254,  189,  154,  155,  156,  157,  158,  159,  160,  154,
 /*  3300 */   155,  156,  157,  158,  159,  160,  254,  254,  189,  154,
 /*  3310 */   155,  156,  157,  158,  159,  160,  254,  189,  154,  155,
 /*  3320 */   156,  157,  158,  159,  160,  254,  254,  189,  254,  254,
 /*  3330 */    70,  254,  254,  254,  189,  254,  254,  254,  254,  254,
 /*  3340 */    70,  254,  254,  254,  189,  254,  254,  254,   70,  254,
 /*  3350 */   254,  254,  254,  189,   94,   95,   96,   97,   98,  254,
 /*  3360 */   254,  101,  254,  254,   94,   95,   96,   97,   98,  254,
 /*  3370 */   254,  101,   94,   95,   96,   97,   98,  254,  254,  101,
};
#define YY_SHIFT_USE_DFLT (-80)
#define YY_SHIFT_COUNT (544)
#define YY_SHIFT_MIN   (-79)
#define YY_SHIFT_MAX   (3278)
static const short yy_shift_ofst[] = {
 /*     0 */   -80,  118,  208,  283,  283,  208,  232,  232,  283,  283,
 /*    10 */   283,  283,  283,  283,  283,  283,  283,  283,  283,  283,
 /*    20 */   283,  283,  283,  283,  283,  283,  283,  283,  283,  283,
 /*    30 */   232,  232,  232,  232,  232,  232,  232,  232,  232,  232,
 /*    40 */   232,  232,  355,  355,  355,  355,  461,  461,  461,  461,
 /*    50 */   461,  461,  461,  461,  461,  461,  688,  688,  688,  688,
 /*    60 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*    70 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*    80 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*    90 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*   100 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*   110 */   688,  688,  688,  688,  688,  688,  688,  688,  688, 1888,
 /*   120 */  1888, 1888, 1837,  182,  638,  638,  638,  638,  638,  638,
 /*   130 */   638,  638,  638,  638,  638,  638,  638,  638,  638,  638,
 /*   140 */   638,  638,  638,  638,  638,  638,  638,  638,  638, 1837,
 /*   150 */  1837, 1837, 1837, 1837, 1837, 1837, 1837, 1837, 1837, 1837,
 /*   160 */  1837, 1837, 1837, 1837, 1837, 1837, 1837, 1837, 1837, 1837,
 /*   170 */  1837, 1837, 1837, 2078, 2078, 2078, 2078, 2078, 2061, 2078,
 /*   180 */  2078, 2078, 2061,  413,  413,  620,  437,  617, 2061,  515,
 /*   190 */   515,  616,  616,  390, 1715, 1713, 1943,  920,  390,  390,
 /*   200 */   390,  390,  823,  915,  920,  616,  616, 1715, 1713, 1402,
 /*   210 */  2087, 2087, 2087, 2087, 2087, 2087, 2087, 2087, 2087, 1063,
 /*   220 */   926,  926,  926,  926,  634,  110,  110, 1840, 1840, 1840,
 /*   230 */  1840, 1840, 1840, 1840, 1840, 1840, 1840, 1840, 1840, 1840,
 /*   240 */   379,  626,  153,  522,  823, 1309, 1200, 1309, 1294, 1309,
 /*   250 */  1294, 1390, 1373, 1290, 1200, 1309, 1294, 1390, 1373, 1290,
 /*   260 */  1200, 1309, 1294, 1390, 1373, 1290, 1200, 1309, 1294, 1309,
 /*   270 */  1294, 1309, 1294, 1309, 1294, 1309, 1294, 1390, 1373, 1290,
 /*   280 */  1309, 1294, 1390, 1373, 1290, 1309, 1294, 1390, 1373, 1405,
 /*   290 */  1402, 1309, 1294, 1290, 1309, 1294, 1200, 1309, 1294, 1200,
 /*   300 */  1309, 1294, 1190, 1190, 1290, 1200, 1190, 1161,  549,  433,
 /*   310 */   801,  517, 1111,  956, 1258, 1103,  948,  793,  749,  431,
 /*   320 */   867, 3278, 3270, 3260, 1758,  771,  -29,  908,  345, 1692,
 /*   330 */  1603, 1302,  275,  727,  333,   27,  837,  837, 1218, 1218,
 /*   340 */  1524,  -79,  -79,  -79,  -79,  -79,  -79,  -79,  -79,  -79,
 /*   350 */   -79,  -79,  837, 1453, 1361, 1227,  -21,  813,  707, 1173,
 /*   360 */  1081,  837, 1123, 1018,  968, 1165, 1706,  562,  741,  644,
 /*   370 */  1143, 1143, 1143, 1143, 1143, 1143, 1143,  562,  562,  562,
 /*   380 */   562,  562,  213,  562,  562,  562,  562, 1143, 1143, 1143,
 /*   390 */   562,  562,  562,  562,  562,  562,  562,  562,  562,  562,
 /*   400 */   562,  562,  562,  562,  562,  562,  562,  562,  562,  562,
 /*   410 */   562, 1010,  988,  562,  562,  111,  218,  218,  300,  300,
 /*   420 */   300,  300,  221,  221,  200,  197,   49,   83,   83,  128,
 /*   430 */   128,   56,   56,  -48,  -48,   56,   56,  -63,  -63,  -63,
 /*   440 */   -63,  -63,  -63,  -63,  -63,  640,  711,  709, 1352, 1349,
 /*   450 */   483,  483,  483,  483,  627, 1235,  430,  858, 1044,  411,
 /*   460 */   775,  731,  699,  490,  695,   50,  415,  109,  162,  693,
 /*   470 */   405,  115,  681,  327,  253,  647,  631,  406,   59,  -41,
 /*   480 */   993, 1217, 1216, 1189, 1187, 1186, 1171, 1170, 1154, 1140,
 /*   490 */  1127, 1114, 1110, 1108, 1083, 1034, 1029,  996,  931,  931,
 /*   500 */   955,  953,  999,  881,  977,  723,  901,  880,  878,  940,
 /*   510 */   832,  723,  723,  819,  869,  762,  734,  723,  692,  633,
 /*   520 */   603,  712,  661,  666,  575,  641,  524,  493,  493,  372,
 /*   530 */   335,  373,  252,  252,  315,  247,  224,  154,  140,   88,
 /*   540 */    74,   10,   -5,  -53,    2,
};
#define YY_REDUCE_USE_DFLT (-136)
#define YY_REDUCE_COUNT (307)
#define YY_REDUCE_MIN   (-135)
#define YY_REDUCE_MAX   (3164)
static const short yy_reduce_ofst[] = {
 /*     0 */  1105, -135,  278,  223, -120,  456,  391,  340, 1419, 1389,
 /*    10 */  1357, 1305, 1264, 1234, 1202, 1150, 1109, 1079, 1047,  995,
 /*    20 */   954,  924,  892,  840,  799,  769,  737,  685,  632,  -85,
 /*    30 */  1719, 1691, 1667, 1640, 1615, 1588, 1564, 1536, 1512,  567,
 /*    40 */   521,  -17, 1485, 1460,  506,  167, 1937, 1917, 1901, 1885,
 /*    50 */  1869, 1852, 1822, 1806, 1770, 1340,  831,  619,  399,  106,
 /*    60 */    41, 2614, 2604, 2593, 2583, 2573, 2563, 2552, 2542, 2531,
 /*    70 */  2521, 2511, 2501, 2490, 2480, 2470, 2460, 2450, 2439, 2429,
 /*    80 */  2419, 2409, 2398, 2388, 2377, 2367, 2357, 2347, 2336, 2326,
 /*    90 */  2316, 2306, 2296, 2285, 2275, 2265, 2255, 2244, 2234, 2223,
 /*   100 */  2213, 2203, 2193, 2182, 2172, 2162, 2152, 2142, 2129, 2119,
 /*   110 */  2107, 2092, 2081, 2071, 2060, 1447, 1293, 1139,  985, 2044,
 /*   120 */  2022, 2005, 1522,  214, 3164, 3155, 3145, 3138, 3128, 3119,
 /*   130 */  3102, 3092, 3080, 3066, 3056, 3044, 3030, 3020, 3008, 2994,
 /*   140 */  2984, 2972, 2948, 2936, 2929, 2922, 2912, 1348, 1193, 2900,
 /*   150 */  2888, 2876, 2864, 2852, 2840, 2828, 2816, 2804, 2792, 2780,
 /*   160 */  2768, 2756, 2744, 2732, 2720, 2708, 2696, 2684, 2671, 2659,
 /*   170 */  2647, 2635, 2623, 1383, 1284,  284, -110, 1607,  359, 1544,
 /*   180 */  1440,  400,  538, 1747, 1734, 1077,  787,  120,  178,  418,
 /*   190 */    92,  922,  205,  259,  238,   94,  105,  982,  942,  913,
 /*   200 */   911,  865,  883,  414,  784,  475,  157,  761,  729,  511,
 /*   210 */  1540, 1539, 1538, 1531, 1516, 1503, 1502, 1493, 1479, 1433,
 /*   220 */  1431, 1425, 1424, 1422, 1420, 1395, 1368, 1477, 1465, 1462,
 /*   230 */  1461, 1452, 1451, 1450, 1449, 1448, 1438, 1435, 1412, 1411,
 /*   240 */  1396, 1426, 1396, 1350, 1376, 1337, 1416, 1335, 1320, 1334,
 /*   250 */  1318, 1319, 1317, 1310, 1385, 1307, 1304, 1298, 1306, 1297,
 /*   260 */  1374, 1292, 1286, 1287, 1285, 1256, 1321, 1275, 1267, 1253,
 /*   270 */  1252, 1250, 1244, 1242, 1241, 1239, 1237, 1226, 1224, 1205,
 /*   280 */  1210, 1201, 1212, 1204, 1196, 1194, 1192, 1185, 1181, 1197,
 /*   290 */  1203, 1160, 1153, 1155, 1149, 1151, 1228, 1138, 1141, 1219,
 /*   300 */  1133, 1120, 1184, 1180, 1092, 1167, 1162, 1112,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   851, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*    10 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*    20 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*    30 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*    40 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*    50 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*    60 */  1298, 1298, 1298, 1298, 1298, 1059, 1063, 1058, 1062, 1146,
 /*    70 */  1142, 1147, 1143, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*    80 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*    90 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   100 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   110 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   120 */  1298, 1298, 1298, 1298, 1128, 1132, 1127, 1131, 1298, 1298,
 /*   130 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   140 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   150 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   160 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   170 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   180 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   190 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   200 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   210 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1247,
 /*   220 */  1249, 1249, 1249, 1249, 1255, 1247, 1247, 1298, 1298, 1298,
 /*   230 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   240 */  1298, 1298, 1298, 1247, 1298, 1255, 1298, 1255, 1253, 1255,
 /*   250 */  1253, 1249, 1251, 1247, 1298, 1255, 1253, 1249, 1251, 1247,
 /*   260 */  1298, 1255, 1253, 1249, 1251, 1247, 1298, 1255, 1253, 1255,
 /*   270 */  1253, 1255, 1253, 1255, 1253, 1255, 1253, 1249, 1251, 1247,
 /*   280 */  1255, 1253, 1249, 1251, 1247, 1255, 1253, 1249, 1251, 1298,
 /*   290 */  1298, 1255, 1253, 1247, 1255, 1253, 1298, 1255, 1253, 1298,
 /*   300 */  1255, 1253, 1298, 1298, 1247, 1298, 1298, 1298, 1298, 1298,
 /*   310 */  1298, 1298, 1298, 1298, 1094, 1298, 1023, 1023, 1298, 1298,
 /*   320 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   330 */  1298, 1298, 1298, 1298,  917, 1298, 1248, 1250, 1239, 1236,
 /*   340 */  1298, 1130, 1134, 1129, 1133, 1125, 1124, 1123, 1122, 1121,
 /*   350 */  1120, 1119, 1112, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   360 */  1254, 1246, 1298, 1298, 1298,  977, 1109, 1081,  976, 1037,
 /*   370 */  1049, 1048, 1047, 1046, 1045, 1044, 1043, 1029, 1061, 1065,
 /*   380 */  1060, 1064,  994, 1148, 1144, 1149, 1145, 1052,  890,  891,
 /*   390 */  1009, 1008, 1007, 1006, 1005, 1004, 1003, 1025, 1022, 1021,
 /*   400 */  1020, 1019, 1018, 1017, 1016, 1015, 1014, 1013, 1012, 1011,
 /*   410 */  1010, 1298, 1298,  887,  886, 1140, 1111, 1110, 1098, 1097,
 /*   420 */  1082, 1083,  982,  983, 1298, 1298, 1298,  971,  972, 1039,
 /*   430 */  1038,  944,  943,  996,  995,  957,  958,  919,  918,  924,
 /*   440 */   923,  929,  928,  903,  904, 1298, 1298,  978, 1298, 1298,
 /*   450 */  1213, 1217, 1216, 1214, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   460 */   962, 1298, 1298, 1298, 1298, 1298, 1161, 1298, 1298, 1298,
 /*   470 */  1298, 1298, 1298,  871, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   480 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298, 1298,
 /*   490 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1091, 1108, 1107,
 /*   500 */  1298, 1298, 1298, 1215, 1298, 1209, 1202, 1179, 1181, 1298,
 /*   510 */  1194, 1160, 1159, 1177, 1298, 1298, 1176, 1175, 1298, 1298,
 /*   520 */  1298, 1298, 1298, 1298, 1298, 1298, 1298, 1036, 1035, 1027,
 /*   530 */  1023,  875,  993,  992, 1298, 1298, 1298, 1298, 1050,  889,
 /*   540 */   885,  883,  879,  874, 1298, 1297, 1296, 1295, 1294, 1293,
 /*   550 */  1292, 1291, 1290, 1289, 1288, 1287, 1286, 1141, 1285, 1284,
 /*   560 */  1283, 1282, 1276, 1275, 1277, 1274, 1273, 1272, 1271, 1270,
 /*   570 */  1269, 1268, 1267, 1266, 1265, 1264, 1263, 1262, 1261, 1260,
 /*   580 */  1259, 1258, 1257, 1256, 1230, 1229, 1238, 1237, 1245, 1244,
 /*   590 */  1241, 1240, 1235, 1243, 1103, 1105, 1126, 1118, 1117, 1116,
 /*   600 */  1115, 1114, 1113, 1106, 1104, 1102, 1096, 1095, 1093, 1092,
 /*   610 */  1091, 1101, 1100, 1099, 1086, 1085, 1084, 1080, 1079, 1078,
 /*   620 */  1077, 1076, 1075, 1074, 1073, 1072, 1071, 1070, 1069, 1090,
 /*   630 */  1089, 1088, 1087, 1234, 1233, 1232, 1231,  986,  985,  984,
 /*   640 */   981,  980,  979,  978, 1224, 1223, 1225, 1222, 1227, 1228,
 /*   650 */  1226, 1221, 1219, 1218, 1220, 1212, 1207, 1210, 1211, 1208,
 /*   660 */  1206, 1197, 1200, 1205, 1203, 1201, 1199, 1198, 1196, 1172,
 /*   670 */  1180, 1182, 1195, 1193, 1192, 1191, 1190, 1189, 1188, 1187,
 /*   680 */  1186, 1185, 1184, 1183, 1178, 1174, 1170, 1169, 1168, 1167,
 /*   690 */  1166, 1165, 1164,  879, 1163, 1162,  975,  974,  973,  970,
 /*   700 */   969,  968,  967,  966,  965,  964,  963,  962, 1173, 1171,
 /*   710 */  1151, 1154, 1155, 1158, 1157, 1156, 1153, 1152, 1150, 1281,
 /*   720 */  1280, 1279, 1278, 1031, 1033, 1042, 1041, 1040, 1034, 1032,
 /*   730 */  1030,  942,  941,  940,  939,  938,  937,  947,  946,  945,
 /*   740 */   936,  935,  934,  933, 1252, 1026, 1028,  876,  988,  990,
 /*   750 */  1054, 1057, 1056, 1055, 1053, 1002, 1001, 1000,  999,  998,
 /*   760 */   997,  991,  989,  987,  915, 1140, 1139, 1138, 1137, 1136,
 /*   770 */  1135, 1024, 1067, 1068, 1066, 1051,  959,  961,  960,  956,
 /*   780 */   955,  954,  953,  952,  951,  950,  949,  948,  888,  922,
 /*   790 */   921,  920,  927,  926,  925,  917,  916,  915,  914,  913,
 /*   800 */   912,  932,  931,  930,  911,  910,  909,  908,  905,  907,
 /*   810 */   906,  902,  901,  900,  899,  898,  897,  896,  895,  894,
 /*   820 */   893,  892,  884,  882,  881,  880,  878,  877,  873,  869,
 /*   830 */   868,  872,  871,  870,  867,  866,  865,  864,  863,  862,
 /*   840 */   861,  860,  859,  858,  857,  856,  855,  854,  853,  852,
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
 /* 104 */ "term_no_const ::= constant",
 /* 105 */ "term_no_const ::= DASH term_no_const",
 /* 106 */ "term_no_const ::= ABS term_no_const",
 /* 107 */ "term_no_const ::= term_no_const DASH term_no_const",
 /* 108 */ "term_no_const ::= term_no_const PLUS term_no_const",
 /* 109 */ "term_no_const ::= term_no_const STAR term_no_const",
 /* 110 */ "term_no_const ::= term_no_const INT_DIV term_no_const",
 /* 111 */ "term_no_const ::= term_no_const MOD term_no_const",
 /* 112 */ "term_integral ::= INTEGER",
 /* 113 */ "term_integral ::= PAREN_L term_integral PAREN_R",
 /* 114 */ "term_integral ::= TRUE",
 /* 115 */ "term_integral ::= FALSE",
 /* 116 */ "term_integral ::= MAXSTEP",
 /* 117 */ "term_integral ::= MAXADDITIVE",
 /* 118 */ "term_integral ::= MAXAFVALUE",
 /* 119 */ "term_integral ::= DASH term_integral",
 /* 120 */ "term_integral ::= ABS term_integral",
 /* 121 */ "term_integral ::= term_integral DASH term_integral",
 /* 122 */ "term_integral ::= term_integral PLUS term_integral",
 /* 123 */ "term_integral ::= term_integral STAR term_integral",
 /* 124 */ "term_integral ::= term_integral INT_DIV term_integral",
 /* 125 */ "term_integral ::= term_integral MOD term_integral",
 /* 126 */ "num_range ::= term_integral DBL_PERIOD term_integral",
 /* 127 */ "num_range_eval ::= term_int_eval DBL_PERIOD term_int_eval",
 /* 128 */ "term_int_eval ::= INTEGER",
 /* 129 */ "term_int_eval ::= PAREN_L term_int_eval PAREN_R",
 /* 130 */ "term_int_eval ::= DASH term_int_eval",
 /* 131 */ "term_int_eval ::= ABS term_int_eval",
 /* 132 */ "term_int_eval ::= term_int_eval DASH term_int_eval",
 /* 133 */ "term_int_eval ::= term_int_eval PLUS term_int_eval",
 /* 134 */ "term_int_eval ::= term_int_eval STAR term_int_eval",
 /* 135 */ "term_int_eval ::= term_int_eval INT_DIV term_int_eval",
 /* 136 */ "term_int_eval ::= term_int_eval MOD term_int_eval",
 /* 137 */ "formula ::= formula_base",
 /* 138 */ "formula ::= PAREN_L formula PAREN_R",
 /* 139 */ "formula ::= NOT formula",
 /* 140 */ "formula ::= DASH formula",
 /* 141 */ "formula ::= formula AMP formula",
 /* 142 */ "formula ::= formula DBL_PLUS formula",
 /* 143 */ "formula ::= formula PIPE formula",
 /* 144 */ "formula ::= formula EQUIV formula",
 /* 145 */ "formula ::= formula IMPL formula",
 /* 146 */ "formula ::= formula ARROW_RDASH formula",
 /* 147 */ "formula_base ::= comparison",
 /* 148 */ "formula_base ::= atomic_formula",
 /* 149 */ "formula_base ::= formula_quant",
 /* 150 */ "formula_base ::= formula_card",
 /* 151 */ "formula_base ::= TRUE",
 /* 152 */ "formula_base ::= FALSE",
 /* 153 */ "comparison ::= term_strong EQ term",
 /* 154 */ "comparison ::= term_strong DBL_EQ term",
 /* 155 */ "comparison ::= term_strong NEQ term",
 /* 156 */ "comparison ::= term_strong LTHAN term",
 /* 157 */ "comparison ::= term_strong GTHAN term",
 /* 158 */ "comparison ::= term_strong LTHAN_EQ term",
 /* 159 */ "comparison ::= term_strong GTHAN_EQ term",
 /* 160 */ "comparison ::= term_strong_candidate EQ term",
 /* 161 */ "comparison ::= term_strong_candidate DBL_EQ term",
 /* 162 */ "comparison ::= term_strong_candidate NEQ term",
 /* 163 */ "comparison ::= term_strong_candidate LTHAN term",
 /* 164 */ "comparison ::= term_strong_candidate GTHAN term",
 /* 165 */ "comparison ::= term_strong_candidate LTHAN_EQ term",
 /* 166 */ "comparison ::= term_strong_candidate GTHAN_EQ term",
 /* 167 */ "comparison ::= constant DBL_EQ term",
 /* 168 */ "comparison ::= constant NEQ term",
 /* 169 */ "comparison ::= constant LTHAN term",
 /* 170 */ "comparison ::= constant GTHAN term",
 /* 171 */ "comparison ::= constant LTHAN_EQ term",
 /* 172 */ "comparison ::= constant GTHAN_EQ term",
 /* 173 */ "atomic_formula ::= constant",
 /* 174 */ "atomic_formula ::= TILDE constant",
 /* 175 */ "atomic_formula ::= constant EQ term",
 /* 176 */ "atomic_formula_anon ::= atomic_formula",
 /* 177 */ "atomic_formula_anon ::= const_anon",
 /* 178 */ "atomic_formula_anon ::= TILDE const_anon",
 /* 179 */ "atomic_formula_anon ::= const_anon EQ term",
 /* 180 */ "formula_no_const ::= formula_no_const_base",
 /* 181 */ "formula_no_const ::= PAREN_L formula_no_const PAREN_R",
 /* 182 */ "formula_no_const ::= NOT formula_no_const",
 /* 183 */ "formula_no_const ::= DASH formula_no_const",
 /* 184 */ "formula_no_const ::= formula_no_const AMP formula_no_const",
 /* 185 */ "formula_no_const ::= formula_no_const DBL_PLUS formula_no_const",
 /* 186 */ "formula_no_const ::= formula_no_const PIPE formula_no_const",
 /* 187 */ "formula_no_const ::= formula_no_const EQUIV formula_no_const",
 /* 188 */ "formula_no_const ::= formula_no_const IMPL formula_no_const",
 /* 189 */ "formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const",
 /* 190 */ "formula_no_const_base ::= comparison_no_const",
 /* 191 */ "formula_no_const_base ::= TRUE",
 /* 192 */ "formula_no_const_base ::= FALSE",
 /* 193 */ "comparison_no_const ::= term_no_const_strong EQ term_no_const",
 /* 194 */ "comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const",
 /* 195 */ "comparison_no_const ::= term_no_const_strong NEQ term_no_const",
 /* 196 */ "comparison_no_const ::= term_no_const_strong LTHAN term_no_const",
 /* 197 */ "comparison_no_const ::= term_no_const_strong GTHAN term_no_const",
 /* 198 */ "comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const",
 /* 199 */ "comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const",
 /* 200 */ "atomic_formula_one_const ::= constant_one_const",
 /* 201 */ "atomic_formula_one_const ::= TILDE constant_one_const",
 /* 202 */ "atomic_formula_one_const ::= constant_one_const EQ term_no_const",
 /* 203 */ "formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R",
 /* 204 */ "quant_lst ::= quant_op variable",
 /* 205 */ "quant_lst ::= quant_lst quant_op variable",
 /* 206 */ "quant_op ::= BIG_CONJ",
 /* 207 */ "quant_op ::= BIG_DISJ",
 /* 208 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 209 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 210 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 211 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 212 */ "formula_card ::= CBRACKET_L formula CBRACKET_R",
 /* 213 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R",
 /* 214 */ "formula_card ::= CBRACKET_L formula CBRACKET_R term",
 /* 215 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term",
 /* 216 */ "card_var_lst ::= card_var_lst_inner PIPE",
 /* 217 */ "card_var_lst_inner ::= variable",
 /* 218 */ "card_var_lst_inner ::= card_var_lst_inner COMMA variable",
 /* 219 */ "term_temporal ::= base_elem_no_const",
 /* 220 */ "term_temporal ::= INTEGER",
 /* 221 */ "term_temporal ::= STRING_LITERAL",
 /* 222 */ "term_temporal ::= PAREN_L term_temporal PAREN_R",
 /* 223 */ "term_temporal ::= TRUE",
 /* 224 */ "term_temporal ::= FALSE",
 /* 225 */ "term_temporal ::= MAXSTEP",
 /* 226 */ "term_temporal ::= MAXADDITIVE",
 /* 227 */ "term_temporal ::= MAXAFVALUE",
 /* 228 */ "term_temporal ::= constant",
 /* 229 */ "term_temporal ::= DASH term_temporal",
 /* 230 */ "term_temporal ::= ABS term_temporal",
 /* 231 */ "term_temporal ::= term_temporal COLON term",
 /* 232 */ "term_temporal ::= term_temporal DASH term_temporal",
 /* 233 */ "term_temporal ::= term_temporal PLUS term_temporal",
 /* 234 */ "term_temporal ::= term_temporal STAR term_temporal",
 /* 235 */ "term_temporal ::= term_temporal INT_DIV term_temporal",
 /* 236 */ "term_temporal ::= term_temporal MOD term_temporal",
 /* 237 */ "term_temporal_strong ::= base_elem_no_const",
 /* 238 */ "term_temporal_strong ::= INTEGER",
 /* 239 */ "term_temporal_strong ::= STRING_LITERAL",
 /* 240 */ "term_temporal_strong ::= PAREN_L term_temporal_strong PAREN_R",
 /* 241 */ "term_temporal_strong ::= MAXSTEP",
 /* 242 */ "term_temporal_strong ::= MAXADDITIVE",
 /* 243 */ "term_temporal_strong ::= MAXAFVALUE",
 /* 244 */ "term_temporal_strong ::= term_temporal_strong COLON term_strong",
 /* 245 */ "term_temporal_strong ::= DASH term_temporal_strong",
 /* 246 */ "term_temporal_strong ::= ABS term_temporal",
 /* 247 */ "term_temporal_strong ::= term_temporal_strong DASH term_temporal",
 /* 248 */ "term_temporal_strong ::= term_temporal_strong PLUS term_temporal",
 /* 249 */ "term_temporal_strong ::= term_temporal_strong STAR term_temporal",
 /* 250 */ "term_temporal_strong ::= term_temporal_strong INT_DIV term_temporal",
 /* 251 */ "term_temporal_strong ::= term_temporal_strong MOD term_temporal",
 /* 252 */ "formula_temporal ::= formula_temporal_base",
 /* 253 */ "formula_temporal ::= PAREN_L formula_temporal PAREN_R",
 /* 254 */ "formula_temporal ::= NOT formula_temporal",
 /* 255 */ "formula_temporal ::= DASH formula_temporal",
 /* 256 */ "formula_temporal ::= formula_temporal AMP formula_temporal",
 /* 257 */ "formula_temporal ::= formula_temporal DBL_PLUS formula_temporal",
 /* 258 */ "formula_temporal ::= formula_temporal PIPE formula_temporal",
 /* 259 */ "formula_temporal ::= formula_temporal EQUIV formula_temporal",
 /* 260 */ "formula_temporal ::= formula_temporal IMPL formula_temporal",
 /* 261 */ "formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal",
 /* 262 */ "formula_temporal ::= term_temporal_strong COLON formula",
 /* 263 */ "formula_temporal_base ::= comparison_temporal",
 /* 264 */ "formula_temporal_base ::= atomic_formula",
 /* 265 */ "formula_temporal_base ::= formula_temporal_quant",
 /* 266 */ "formula_temporal_base ::= formula_temporal_card",
 /* 267 */ "formula_temporal_base ::= TRUE",
 /* 268 */ "formula_temporal_base ::= FALSE",
 /* 269 */ "comparison_temporal ::= term_temporal_strong EQ term_temporal",
 /* 270 */ "comparison_temporal ::= term_temporal_strong DBL_EQ term_temporal",
 /* 271 */ "comparison_temporal ::= term_temporal_strong NEQ term_temporal",
 /* 272 */ "comparison_temporal ::= term_temporal_strong LTHAN term_temporal",
 /* 273 */ "comparison_temporal ::= term_temporal_strong GTHAN term_temporal",
 /* 274 */ "comparison_temporal ::= term_temporal_strong LTHAN_EQ term_temporal",
 /* 275 */ "comparison_temporal ::= term_temporal_strong GTHAN_EQ term_temporal",
 /* 276 */ "formula_temporal_quant ::= BRACKET_L quant_lst PIPE formula_temporal BRACKET_R",
 /* 277 */ "formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R",
 /* 278 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R",
 /* 279 */ "formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal",
 /* 280 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal",
 /* 281 */ "formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R",
 /* 282 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R",
 /* 283 */ "formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R term_temporal",
 /* 284 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R term_temporal",
 /* 285 */ "head_formula ::= comparison",
 /* 286 */ "head_formula ::= atomic_head_formula",
 /* 287 */ "head_formula ::= formula_smpl_card",
 /* 288 */ "head_formula ::= TRUE",
 /* 289 */ "head_formula ::= FALSE",
 /* 290 */ "atomic_head_formula ::= atomic_formula",
 /* 291 */ "atomic_head_formula ::= DASH constant",
 /* 292 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 293 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 294 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 295 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 296 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 297 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 298 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 299 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 300 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 301 */ "macro_def_lst ::= macro_bnd",
 /* 302 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 303 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 304 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 305 */ "macro_args ::= macro_arg",
 /* 306 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 307 */ "macro_arg ::= POUND_INTEGER",
 /* 308 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 309 */ "sort_lst ::= sort",
 /* 310 */ "sort_lst ::= sort_lst COMMA sort",
 /* 311 */ "sort ::= sort_id_nr",
 /* 312 */ "sort ::= sort_id_nr STAR",
 /* 313 */ "sort ::= sort_id_nr CARROT",
 /* 314 */ "sort ::= sort PLUS object_nullary",
 /* 315 */ "sort ::= sort PLUS IDENTIFIER",
 /* 316 */ "sort ::= sort PLUS INTEGER",
 /* 317 */ "sort_id_nr ::= sort_id",
 /* 318 */ "sort_id_nr ::= sort_nr",
 /* 319 */ "sort_nr ::= num_range",
 /* 320 */ "sort_id ::= IDENTIFIER",
 /* 321 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 322 */ "constant_bnd_lst ::= constant_bnd",
 /* 323 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 324 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 325 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 326 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 327 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 328 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 329 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 330 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 331 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 332 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 333 */ "constant_dcl_type ::= ABACTION",
 /* 334 */ "constant_dcl_type ::= ACTION",
 /* 335 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 336 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 337 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 338 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 339 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 340 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 341 */ "constant_dcl_type ::= RIGID",
 /* 342 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 343 */ "constant_dcl_type ::= SDFLUENT",
 /* 344 */ "attrib_spec ::= ATTRIBUTE",
 /* 345 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 346 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 347 */ "object_bnd_lst ::= object_bnd",
 /* 348 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 349 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 350 */ "object_lst ::= object_spec",
 /* 351 */ "object_lst ::= object_lst COMMA object_spec",
 /* 352 */ "object_spec ::= IDENTIFIER",
 /* 353 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 354 */ "object_spec ::= INTEGER",
 /* 355 */ "object_spec ::= num_range",
 /* 356 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 357 */ "variable_bnd_lst ::= variable_bnd",
 /* 358 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 359 */ "variable_bnd ::= variable_lst DBL_COLON sort",
 /* 360 */ "variable_lst ::= IDENTIFIER",
 /* 361 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 362 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 363 */ "sort_bnd_lst ::= sort_bnd",
 /* 364 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 365 */ "sort_bnd ::= sort_dcl_lst",
 /* 366 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 367 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 368 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 369 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 370 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 371 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 372 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 373 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 374 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 375 */ "show_lst ::= show_elem",
 /* 376 */ "show_lst ::= show_lst COMMA show_elem",
 /* 377 */ "show_lst ::= show_lst SEMICOLON show_elem",
 /* 378 */ "show_elem ::= atomic_formula_one_const",
 /* 379 */ "stmt_noconcurrency ::= NOCONCURRENCY PERIOD",
 /* 380 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD",
 /* 381 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_int_eval PERIOD",
 /* 382 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE DBL_COLON term_int_eval PERIOD",
 /* 383 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_int_eval PERIOD",
 /* 384 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE DBL_COLON term_int_eval PERIOD",
 /* 385 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 386 */ "query_lst ::= formula_temporal",
 /* 387 */ "query_lst ::= query_maxstep_decl",
 /* 388 */ "query_lst ::= query_label_decl",
 /* 389 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 390 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 391 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 392 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 393 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range_eval",
 /* 394 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 395 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 396 */ "clause_if ::= IF formula",
 /* 397 */ "clause_if ::=",
 /* 398 */ "clause_after ::= AFTER formula",
 /* 399 */ "clause_after ::=",
 /* 400 */ "clause_ifcons ::= IFCONS formula",
 /* 401 */ "clause_ifcons ::=",
 /* 402 */ "clause_unless ::= UNLESS atomic_formula_anon",
 /* 403 */ "clause_unless ::=",
 /* 404 */ "clause_where ::= WHERE formula_no_const",
 /* 405 */ "clause_where ::=",
 /* 406 */ "stmt_law ::= law_basic",
 /* 407 */ "stmt_law ::= law_caused",
 /* 408 */ "stmt_law ::= law_pcaused",
 /* 409 */ "stmt_law ::= law_impl",
 /* 410 */ "stmt_law ::= law_causes",
 /* 411 */ "stmt_law ::= law_increments",
 /* 412 */ "stmt_law ::= law_decrements",
 /* 413 */ "stmt_law ::= law_mcause",
 /* 414 */ "stmt_law ::= law_always",
 /* 415 */ "stmt_law ::= law_constraint",
 /* 416 */ "stmt_law ::= law_impossible",
 /* 417 */ "stmt_law ::= law_never",
 /* 418 */ "stmt_law ::= law_default",
 /* 419 */ "stmt_law ::= law_exogenous",
 /* 420 */ "stmt_law ::= law_inertial",
 /* 421 */ "stmt_law ::= law_nonexecutable",
 /* 422 */ "stmt_law ::= law_rigid",
 /* 423 */ "stmt_law ::= law_observed",
 /* 424 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 425 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 426 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 427 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 428 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 429 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 430 */ "law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 431 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 432 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 433 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 434 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 435 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 436 */ "law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 437 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 438 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 439 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 440 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 441 */ "law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD",
 /* 442 */ "stmt_code_blk ::= ASP_GR",
 /* 443 */ "stmt_code_blk ::= ASP_CP",
 /* 444 */ "stmt_code_blk ::= F2LP_GR",
 /* 445 */ "stmt_code_blk ::= F2LP_CP",
 /* 446 */ "stmt_code_blk ::= LUA_GR",
 /* 447 */ "stmt_code_blk ::= LUA_CP",
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
#line 2558 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 136: /* start */
    case 137: /* statement_lst */
    case 160: /* undeclared */
{
#line 209 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2567 "bcplus/parser/detail/lemon_parser.c"
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
#line 2580 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 139: /* stmt_macro_def */
{
#line 234 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy151));								
#line 2587 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_constant_def */
{
#line 236 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy431));								
#line 2594 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_object_def */
{
#line 238 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy468));								
#line 2601 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_variable_def */
{
#line 240 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy459));								
#line 2608 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_sort_def */
{
#line 242 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy61));								
#line 2615 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 148: /* stmt_noconcurrency */
{
#line 252 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy485));								
#line 2622 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_strong_noconcurrency */
{
#line 254 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy446));								
#line 2629 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 152: /* stmt_query */
{
#line 260 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy466));								
#line 2636 "bcplus/parser/detail/lemon_parser.c"
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
#line 2654 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 155: /* constant */
    case 163: /* constant_one_const */
    case 166: /* const_anon */
{
#line 298 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy225));								
#line 2663 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* object */
    case 157: /* object_nullary */
{
#line 300 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy422));								
#line 2671 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 158: /* variable */
{
#line 304 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy189));								
#line 2678 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* lua */
{
#line 306 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy117));								
#line 2685 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 161: /* term_lst */
    case 164: /* term_no_const_lst */
{
#line 310 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy259));								
#line 2693 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 170: /* num_range */
{
#line 709 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy345));								
#line 2700 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* num_range_eval */
{
#line 711 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy29));								
#line 2707 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 173: /* term_int_eval */
{
#line 715 "bcplus/parser/detail/lemon_parser.y"
 /* Initially left Blank */				
#line 2714 "bcplus/parser/detail/lemon_parser.c"
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
#line 816 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy17));								
#line 2732 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 177: /* atomic_formula */
    case 180: /* atomic_formula_anon */
    case 184: /* atomic_formula_one_const */
    case 199: /* atomic_head_formula */
{
#line 822 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy423));								
#line 2742 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 178: /* formula_quant */
    case 196: /* formula_temporal_quant */
{
#line 824 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy177));								
#line 2750 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_lst */
{
#line 998 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy221));								
#line 2757 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* quant_op */
{
#line 1000 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2764 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 187: /* card_var_lst */
    case 188: /* card_var_lst_inner */
{
#line 1037 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy375));								
#line 2772 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* formula_smpl_card */
{
#line 1343 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy133));								
#line 2779 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* macro_def_lst */
{
#line 1390 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy269));                              
#line 2786 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* macro_bnd */
{
#line 1392 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy359));                              
#line 2793 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* macro_args */
{
#line 1394 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy154));                              
#line 2800 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* macro_arg */
{
#line 1396 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy79));                              
#line 2807 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* sort_lst */
{
#line 1486 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy195));							
#line 2814 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 206: /* sort */
    case 207: /* sort_id_nr */
    case 208: /* sort_nr */
    case 209: /* sort_id */
{
#line 1488 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2824 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* constant_bnd_lst */
    case 211: /* constant_bnd */
{
#line 1597 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy409));									
#line 2832 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* constant_dcl_lst */
{
#line 1601 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy226));									
#line 2839 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* constant_dcl_type */
{
#line 1603 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2846 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* attrib_spec */
{
#line 1605 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2853 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* object_bnd_lst */
{
#line 1961 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy102));									
#line 2860 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* object_bnd */
{
#line 1963 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy294));									
#line 2867 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* object_lst */
    case 218: /* object_spec */
{
#line 1965 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy425));									
#line 2875 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* variable_bnd_lst */
    case 220: /* variable_bnd */
{
#line 2097 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy410));									
#line 2883 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 221: /* variable_lst */
{
#line 2101 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy52));									
#line 2890 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 222: /* sort_bnd_lst */
    case 223: /* sort_bnd */
    case 224: /* sort_dcl_lst */
{
#line 2184 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy196));									
#line 2899 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 225: /* show_lst */
{
#line 2288 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy123));									
#line 2906 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 226: /* show_elem */
    case 234: /* clause_unless */
{
#line 2290 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy423));									
#line 2914 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 227: /* query_lst */
{
#line 2442 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy489).l); DEALLOC((yypminor->yy489).maxstep); DEALLOC((yypminor->yy489).label);	
#line 2921 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 228: /* query_maxstep_decl */
{
#line 2444 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy29));												
#line 2928 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 230: /* query_label_Decl */
{
#line 2446 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2935 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 231: /* clause_if */
    case 232: /* clause_after */
    case 233: /* clause_ifcons */
    case 235: /* clause_where */
{
#line 2600 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy17));									
#line 2945 "bcplus/parser/detail/lemon_parser.c"
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
#line 2641 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy484));									
#line 2969 "bcplus/parser/detail/lemon_parser.c"
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
#line 3722 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 220 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy484;
			yymsp[0].minor.yy484  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3731 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy151; }
#line 3736 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy431; }
#line 3741 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy468; }
#line 3746 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy459; }
#line 3751 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy61; }
#line 3756 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 268 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy484; }
#line 3766 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy485; }
#line 3771 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 273 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy446; }
#line 3776 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 276 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy484 = yymsp[0].minor.yy466; }
#line 3781 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy225; }
#line 3786 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 42: /* term ::= base_elem */ yytestcase(yyruleno==42);
      case 58: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==58);
      case 83: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==83);
      case 98: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==98);
      case 219: /* term_temporal ::= base_elem_no_const */ yytestcase(yyruleno==219);
      case 237: /* term_temporal_strong ::= base_elem_no_const */ yytestcase(yyruleno==237);
#line 323 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy299; }
#line 3797 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 325 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy422;	}
#line 3802 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 326 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy189; }
#line 3807 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 327 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy299 = yymsp[0].minor.yy117; }
#line 3812 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 38: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==38);
#line 445 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy225, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3818 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 39: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==39);
#line 446 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy225, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3824 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* const_anon ::= IDENTIFIER */
#line 448 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy225, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true); }
#line 3829 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 449 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy225, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true);	}
#line 3834 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 452 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy422, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3839 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= object_nullary */
#line 453 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy422 = yymsp[0].minor.yy422; }
#line 3844 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* object_nullary ::= OBJECT_ID */
#line 454 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy422, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3849 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* object ::= undeclared */
#line 455 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3854 "bcplus/parser/detail/lemon_parser.c"
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
#line 3869 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 469 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy117, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy259, yymsp[0].minor.yy0); }
#line 3874 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* lua ::= AT_IDENTIFIER */
#line 470 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy117, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3879 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 471 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy217, yymsp[-3].minor.yy0, yymsp[-1].minor.yy259);   yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 3886 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* undeclared ::= IDENTIFIER */
#line 472 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy217, yymsp[0].minor.yy0, NULL); }
#line 3891 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 36: /* term_lst ::= term */
      case 40: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==40);
#line 475 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy259 = new TermList();
			yygotominor.yy259->push_back(yymsp[0].minor.yy299);
		}
#line 3900 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 37: /* term_lst ::= term_lst COMMA term */
      case 41: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==41);
#line 481 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy259 = yymsp[-2].minor.yy259;
			yymsp[-2].minor.yy259->push_back(yymsp[0].minor.yy299);
		  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3910 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 43: /* term ::= INTEGER */
      case 59: /* term_strong ::= INTEGER */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==99);
      case 112: /* term_integral ::= INTEGER */ yytestcase(yyruleno==112);
      case 220: /* term_temporal ::= INTEGER */ yytestcase(yyruleno==220);
      case 238: /* term_temporal_strong ::= INTEGER */ yytestcase(yyruleno==238);
#line 580 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy299, yymsp[0].minor.yy0);	}
#line 3921 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 44: /* term ::= STRING_LITERAL */
      case 46: /* term ::= TRUE */ yytestcase(yyruleno==46);
      case 47: /* term ::= FALSE */ yytestcase(yyruleno==47);
      case 60: /* term_strong ::= STRING_LITERAL */ yytestcase(yyruleno==60);
      case 85: /* term_no_const_strong ::= STRING_LITERAL */ yytestcase(yyruleno==85);
      case 100: /* term_no_const ::= STRING_LITERAL */ yytestcase(yyruleno==100);
      case 102: /* term_no_const ::= TRUE */ yytestcase(yyruleno==102);
      case 103: /* term_no_const ::= FALSE */ yytestcase(yyruleno==103);
      case 114: /* term_integral ::= TRUE */ yytestcase(yyruleno==114);
      case 115: /* term_integral ::= FALSE */ yytestcase(yyruleno==115);
      case 221: /* term_temporal ::= STRING_LITERAL */ yytestcase(yyruleno==221);
      case 223: /* term_temporal ::= TRUE */ yytestcase(yyruleno==223);
      case 224: /* term_temporal ::= FALSE */ yytestcase(yyruleno==224);
      case 239: /* term_temporal_strong ::= STRING_LITERAL */ yytestcase(yyruleno==239);
#line 581 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy299, yymsp[0].minor.yy0); }
#line 3939 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 45: /* term ::= PAREN_L term PAREN_R */
      case 61: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==61);
      case 86: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==86);
      case 101: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==101);
      case 113: /* term_integral ::= PAREN_L term_integral PAREN_R */ yytestcase(yyruleno==113);
      case 222: /* term_temporal ::= PAREN_L term_temporal PAREN_R */ yytestcase(yyruleno==222);
      case 240: /* term_temporal_strong ::= PAREN_L term_temporal_strong PAREN_R */ yytestcase(yyruleno==240);
#line 582 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy299, yymsp[-2].minor.yy0, yymsp[-1].minor.yy299, yymsp[0].minor.yy0); }
#line 3950 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 48: /* term ::= MAXSTEP */
      case 62: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==87);
      case 116: /* term_integral ::= MAXSTEP */ yytestcase(yyruleno==116);
      case 225: /* term_temporal ::= MAXSTEP */ yytestcase(yyruleno==225);
      case 241: /* term_temporal_strong ::= MAXSTEP */ yytestcase(yyruleno==241);
#line 585 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy299, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3960 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= MAXADDITIVE */
      case 63: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==63);
      case 88: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==88);
      case 117: /* term_integral ::= MAXADDITIVE */ yytestcase(yyruleno==117);
      case 226: /* term_temporal ::= MAXADDITIVE */ yytestcase(yyruleno==226);
      case 242: /* term_temporal_strong ::= MAXADDITIVE */ yytestcase(yyruleno==242);
#line 586 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy299, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3970 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= MAXAFVALUE */
      case 64: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==64);
      case 89: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==89);
      case 118: /* term_integral ::= MAXAFVALUE */ yytestcase(yyruleno==118);
      case 227: /* term_temporal ::= MAXAFVALUE */ yytestcase(yyruleno==227);
      case 243: /* term_temporal_strong ::= MAXAFVALUE */ yytestcase(yyruleno==243);
#line 587 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy299, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3980 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= DASH term */
      case 65: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==65);
      case 91: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==91);
      case 105: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==105);
      case 119: /* term_integral ::= DASH term_integral */ yytestcase(yyruleno==119);
      case 229: /* term_temporal ::= DASH term_temporal */ yytestcase(yyruleno==229);
      case 245: /* term_temporal_strong ::= DASH term_temporal_strong */ yytestcase(yyruleno==245);
#line 591 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, UnaryTerm::Operator::NEGATIVE); }
#line 3991 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= ABS term */
      case 66: /* term_strong ::= ABS term */ yytestcase(yyruleno==66);
      case 92: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==92);
      case 106: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==106);
      case 120: /* term_integral ::= ABS term_integral */ yytestcase(yyruleno==120);
      case 230: /* term_temporal ::= ABS term_temporal */ yytestcase(yyruleno==230);
      case 246: /* term_temporal_strong ::= ABS term_temporal */ yytestcase(yyruleno==246);
#line 592 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, UnaryTerm::Operator::ABS); }
#line 4002 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= term DASH term */
      case 68: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==68);
      case 78: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==78);
      case 93: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==93);
      case 107: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==107);
      case 121: /* term_integral ::= term_integral DASH term_integral */ yytestcase(yyruleno==121);
      case 232: /* term_temporal ::= term_temporal DASH term_temporal */ yytestcase(yyruleno==232);
      case 247: /* term_temporal_strong ::= term_temporal_strong DASH term_temporal */ yytestcase(yyruleno==247);
#line 596 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MINUS); }
#line 4014 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= term PLUS term */
      case 69: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==69);
      case 79: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==79);
      case 94: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==94);
      case 108: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==108);
      case 122: /* term_integral ::= term_integral PLUS term_integral */ yytestcase(yyruleno==122);
      case 233: /* term_temporal ::= term_temporal PLUS term_temporal */ yytestcase(yyruleno==233);
      case 248: /* term_temporal_strong ::= term_temporal_strong PLUS term_temporal */ yytestcase(yyruleno==248);
#line 597 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::PLUS); }
#line 4026 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= term STAR term */
      case 70: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==70);
      case 80: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==80);
      case 95: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==95);
      case 109: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==109);
      case 123: /* term_integral ::= term_integral STAR term_integral */ yytestcase(yyruleno==123);
      case 234: /* term_temporal ::= term_temporal STAR term_temporal */ yytestcase(yyruleno==234);
      case 249: /* term_temporal_strong ::= term_temporal_strong STAR term_temporal */ yytestcase(yyruleno==249);
#line 598 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::TIMES); }
#line 4038 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 56: /* term ::= term INT_DIV term */
      case 71: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==71);
      case 81: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==81);
      case 96: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==96);
      case 110: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==110);
      case 124: /* term_integral ::= term_integral INT_DIV term_integral */ yytestcase(yyruleno==124);
      case 235: /* term_temporal ::= term_temporal INT_DIV term_temporal */ yytestcase(yyruleno==235);
      case 250: /* term_temporal_strong ::= term_temporal_strong INT_DIV term_temporal */ yytestcase(yyruleno==250);
#line 599 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::DIVIDE); }
#line 4050 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 57: /* term ::= term MOD term */
      case 72: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==72);
      case 82: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==82);
      case 97: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==97);
      case 111: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==111);
      case 125: /* term_integral ::= term_integral MOD term_integral */ yytestcase(yyruleno==125);
      case 236: /* term_temporal ::= term_temporal MOD term_temporal */ yytestcase(yyruleno==236);
      case 251: /* term_temporal_strong ::= term_temporal_strong MOD term_temporal */ yytestcase(yyruleno==251);
#line 600 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MOD); }
#line 4062 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 67: /* term_strong_candidate ::= DASH constant */
#line 619 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy225, UnaryTerm::Operator::NEGATIVE); }
#line 4067 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 73: /* term_strong ::= constant DASH term */
#line 628 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MINUS); }
#line 4072 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant PLUS term */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::PLUS); }
#line 4077 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant STAR term */
#line 630 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::TIMES); }
#line 4082 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 76: /* term_strong ::= constant INT_DIV term */
#line 631 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::DIVIDE); }
#line 4087 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong ::= constant MOD term */
#line 632 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy299, yymsp[-2].minor.yy225, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BinaryTerm::Operator::MOD); }
#line 4092 "bcplus/parser/detail/lemon_parser.c"
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
#line 4103 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 104: /* term_no_const ::= constant */
#line 684 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy299 default to undeclared identifiers
		yygotominor.yy299 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy225;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy225->beginLoc());
		YYERROR;
	}
#line 4114 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 126: /* num_range ::= term_integral DBL_PERIOD term_integral */
#line 741 "bcplus/parser/detail/lemon_parser.y"
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
#line 4134 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 127: /* num_range_eval ::= term_int_eval DBL_PERIOD term_int_eval */
#line 759 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy396, r_ptr = yymsp[0].minor.yy396, s_ptr = yymsp[-1].minor.yy0;
	yygotominor.yy29 = new NumberRangeEval(yymsp[-2].minor.yy396->val(), yymsp[0].minor.yy396->val(), yymsp[-2].minor.yy396->beginLoc(), yymsp[0].minor.yy396->endLoc());
}
#line 4142 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 128: /* term_int_eval ::= INTEGER */
#line 765 "bcplus/parser/detail/lemon_parser.y"
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
#line 4157 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 129: /* term_int_eval ::= PAREN_L term_int_eval PAREN_R */
#line 777 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy396 = yymsp[-1].minor.yy396;
	yygotominor.yy396->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy396->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 4167 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* term_int_eval ::= DASH term_int_eval */
#line 797 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy396, yymsp[0].minor.yy396, -1 * yymsp[0].minor.yy396->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4173 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 131: /* term_int_eval ::= ABS term_int_eval */
#line 798 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy396, yymsp[0].minor.yy396, yymsp[0].minor.yy396->val() < 0 ? - yymsp[0].minor.yy396->val() : yymsp[0].minor.yy396->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 4179 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* term_int_eval ::= term_int_eval DASH term_int_eval */
#line 800 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() - yymsp[0].minor.yy396->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4185 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* term_int_eval ::= term_int_eval PLUS term_int_eval */
#line 801 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() + yymsp[0].minor.yy396->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 4191 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* term_int_eval ::= term_int_eval STAR term_int_eval */
#line 802 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() * yymsp[0].minor.yy396->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 4197 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* term_int_eval ::= term_int_eval INT_DIV term_int_eval */
#line 803 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() / yymsp[0].minor.yy396->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 4203 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* term_int_eval ::= term_int_eval MOD term_int_eval */
#line 804 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy396, yymsp[-2].minor.yy396, yymsp[0].minor.yy396, yymsp[-2].minor.yy396->val() % yymsp[0].minor.yy396->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 4209 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* formula ::= formula_base */
      case 180: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==180);
      case 252: /* formula_temporal ::= formula_temporal_base */ yytestcase(yyruleno==252);
#line 864 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy17;				}
#line 4216 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* formula ::= PAREN_L formula PAREN_R */
      case 181: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==181);
      case 253: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==253);
#line 865 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[-1].minor.yy17; yygotominor.yy17->parens(true); 	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4225 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* formula ::= NOT formula */
      case 182: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==182);
      case 254: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==254);
#line 866 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 4232 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* formula ::= DASH formula */
      case 183: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==183);
      case 255: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==255);
#line 867 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 4239 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* formula ::= formula AMP formula */
      case 184: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==184);
      case 256: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==256);
#line 868 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy17, yymsp[0].minor.yy17, yymsp[-2].minor.yy17->beginLoc(), yymsp[0].minor.yy17->endLoc());   yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 4247 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* formula ::= formula DBL_PLUS formula */
      case 143: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==143);
      case 185: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==185);
      case 186: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==186);
      case 257: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==257);
      case 258: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==258);
#line 869 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::OR); }
#line 4257 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 144: /* formula ::= formula EQUIV formula */
      case 187: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==187);
      case 259: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==259);
#line 871 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::EQUIV); }
#line 4264 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 145: /* formula ::= formula IMPL formula */
      case 146: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==146);
      case 188: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==188);
      case 189: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==189);
      case 260: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==260);
      case 261: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==261);
#line 872 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy17, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BinaryFormula::Operator::IMPL); }
#line 4274 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 147: /* formula_base ::= comparison */
      case 190: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==190);
      case 263: /* formula_temporal_base ::= comparison_temporal */ yytestcase(yyruleno==263);
      case 285: /* head_formula ::= comparison */ yytestcase(yyruleno==285);
#line 875 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy17; }
#line 4282 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 148: /* formula_base ::= atomic_formula */
      case 286: /* head_formula ::= atomic_head_formula */ yytestcase(yyruleno==286);
#line 876 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy423; }
#line 4288 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 149: /* formula_base ::= formula_quant */
      case 265: /* formula_temporal_base ::= formula_temporal_quant */ yytestcase(yyruleno==265);
#line 877 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = yymsp[0].minor.yy177; }
#line 4294 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 150: /* formula_base ::= formula_card */
      case 266: /* formula_temporal_base ::= formula_temporal_card */ yytestcase(yyruleno==266);
#line 879 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy17 = yymsp[0].minor.yy17;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy17->beginLoc());
			YYERROR;
		}
	}
#line 4306 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 151: /* formula_base ::= TRUE */
      case 191: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==191);
      case 267: /* formula_temporal_base ::= TRUE */ yytestcase(yyruleno==267);
      case 288: /* head_formula ::= TRUE */ yytestcase(yyruleno==288);
#line 886 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4314 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* formula_base ::= FALSE */
      case 192: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==192);
      case 268: /* formula_temporal_base ::= FALSE */ yytestcase(yyruleno==268);
      case 289: /* head_formula ::= FALSE */ yytestcase(yyruleno==289);
#line 887 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4322 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* comparison ::= term_strong EQ term */
      case 160: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==160);
      case 193: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==193);
      case 269: /* comparison_temporal ::= term_temporal_strong EQ term_temporal */ yytestcase(yyruleno==269);
#line 889 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4331 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* comparison ::= term_strong DBL_EQ term */
      case 161: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==161);
      case 194: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==194);
      case 270: /* comparison_temporal ::= term_temporal_strong DBL_EQ term_temporal */ yytestcase(yyruleno==270);
#line 890 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4340 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* comparison ::= term_strong NEQ term */
      case 162: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==162);
      case 195: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==195);
      case 271: /* comparison_temporal ::= term_temporal_strong NEQ term_temporal */ yytestcase(yyruleno==271);
#line 891 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4349 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* comparison ::= term_strong LTHAN term */
      case 163: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==163);
      case 196: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==196);
      case 272: /* comparison_temporal ::= term_temporal_strong LTHAN term_temporal */ yytestcase(yyruleno==272);
#line 892 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4358 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= term_strong GTHAN term */
      case 164: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==164);
      case 197: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==197);
      case 273: /* comparison_temporal ::= term_temporal_strong GTHAN term_temporal */ yytestcase(yyruleno==273);
#line 893 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4367 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* comparison ::= term_strong LTHAN_EQ term */
      case 165: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==165);
      case 198: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==198);
      case 274: /* comparison_temporal ::= term_temporal_strong LTHAN_EQ term_temporal */ yytestcase(yyruleno==274);
#line 894 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4376 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* comparison ::= term_strong GTHAN_EQ term */
      case 166: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==166);
      case 199: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==199);
      case 275: /* comparison_temporal ::= term_temporal_strong GTHAN_EQ term_temporal */ yytestcase(yyruleno==275);
#line 895 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy299, yymsp[0].minor.yy299, yymsp[-2].minor.yy299->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4385 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 167: /* comparison ::= constant DBL_EQ term */
#line 903 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4391 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 168: /* comparison ::= constant NEQ term */
#line 904 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4397 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 169: /* comparison ::= constant LTHAN term */
#line 905 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4403 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 170: /* comparison ::= constant GTHAN term */
#line 906 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4409 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 171: /* comparison ::= constant LTHAN_EQ term */
#line 907 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4415 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 172: /* comparison ::= constant GTHAN_EQ term */
#line 908 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4421 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 173: /* atomic_formula ::= constant */
      case 177: /* atomic_formula_anon ::= const_anon */ yytestcase(yyruleno==177);
      case 200: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==200);
#line 935 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy423, yymsp[0].minor.yy225, true); }
#line 4428 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 174: /* atomic_formula ::= TILDE constant */
      case 178: /* atomic_formula_anon ::= TILDE const_anon */ yytestcase(yyruleno==178);
      case 201: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==201);
#line 936 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy423, yymsp[0].minor.yy225, false);   yy_destructor(yypParser,75,&yymsp[-1].minor);
}
#line 4436 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 175: /* atomic_formula ::= constant EQ term */
      case 179: /* atomic_formula_anon ::= const_anon EQ term */ yytestcase(yyruleno==179);
      case 202: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==202);
#line 937 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = new AtomicFormula(yymsp[-2].minor.yy225, yymsp[0].minor.yy299, yymsp[-2].minor.yy225->beginLoc(), yymsp[0].minor.yy299->endLoc());	  yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4444 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 176: /* atomic_formula_anon ::= atomic_formula */
      case 290: /* atomic_head_formula ::= atomic_formula */ yytestcase(yyruleno==290);
      case 378: /* show_elem ::= atomic_formula_one_const */ yytestcase(yyruleno==378);
#line 939 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = yymsp[0].minor.yy423; }
#line 4451 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 203: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
      case 276: /* formula_temporal_quant ::= BRACKET_L quant_lst PIPE formula_temporal BRACKET_R */ yytestcase(yyruleno==276);
#line 1003 "bcplus/parser/detail/lemon_parser.y"
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
#line 4469 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 204: /* quant_lst ::= quant_op variable */
#line 1017 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy221 = new QuantifierFormula::QuantifierList();
		yygotominor.yy221->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy497, yymsp[0].minor.yy189));
	}
#line 4477 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 205: /* quant_lst ::= quant_lst quant_op variable */
#line 1023 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy221 = yymsp[-2].minor.yy221;
		yygotominor.yy221->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy497, yymsp[0].minor.yy189));
	}
#line 4485 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 206: /* quant_op ::= BIG_CONJ */
#line 1028 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy497 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,90,&yymsp[0].minor);
}
#line 4491 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 207: /* quant_op ::= BIG_DISJ */
#line 1029 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy497 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4497 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
      case 277: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==277);
#line 1075 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, NULL);  }
#line 4503 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 209: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
      case 278: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==278);
#line 1076 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy17,  yymsp[0].minor.yy0, NULL);  }
#line 4509 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 279: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==279);
#line 1077 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4515 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 280: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==280);
#line 1078 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-5].minor.yy299, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy17,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4521 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
      case 281: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==281);
#line 1079 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, NULL);  }
#line 4527 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
      case 282: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==282);
#line 1080 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-3].minor.yy299, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy17,  yymsp[0].minor.yy0, NULL);  }
#line 4533 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
      case 283: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==283);
#line 1081 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4539 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 215: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
      case 284: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==284);
#line 1082 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy17, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy17,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4545 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1086 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy375 = yymsp[-1].minor.yy375;
	  yy_destructor(yypParser,98,&yymsp[0].minor);
}
#line 4553 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* card_var_lst_inner ::= variable */
#line 1091 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy189;
		yygotominor.yy375 = new CardinalityFormula::VariableList();
		yygotominor.yy375->push_back(yymsp[0].minor.yy189->symbol());
	}
#line 4562 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 218: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1098 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy189;
		yygotominor.yy375 = yymsp[-2].minor.yy375;
		yygotominor.yy375->push_back(yymsp[0].minor.yy189->symbol());
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4572 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 228: /* term_temporal ::= constant */
#line 1152 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy299 default to undeclared identifiers
		yygotominor.yy299 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy225;
		parser->_parse_error("All constant symbols must be bound to a step using the i:F notation.", &yymsp[0].minor.yy225->beginLoc());
		YYERROR;
	}
#line 4583 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 231: /* term_temporal ::= term_temporal COLON term */
      case 244: /* term_temporal_strong ::= term_temporal_strong COLON term_strong */ yytestcase(yyruleno==244);
#line 1164 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy299, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy299, BindingTerm); }
#line 4589 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 262: /* formula_temporal ::= term_temporal_strong COLON formula */
#line 1245 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy17, yymsp[-2].minor.yy299, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, BindingFormula); }
#line 4594 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 264: /* formula_temporal_base ::= atomic_formula */
#line 1251 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for more useful error messages
		yygotominor.yy17 = NULL;
		ref_ptr<const Referenced> l_ptr = yymsp[0].minor.yy423;
		parser->_parse_error("All constant symbols must be bound to a step using the i:F notation.", &yymsp[0].minor.yy423->beginLoc());
		YYERROR;
	}
#line 4605 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* head_formula ::= formula_smpl_card */
#line 1348 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy17 = yymsp[0].minor.yy133;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy133->beginLoc());
			YYERROR;
		}
	}
#line 4616 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* atomic_head_formula ::= DASH constant */
#line 1361 "bcplus/parser/detail/lemon_parser.y"
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
#line 4632 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1374 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy423, yymsp[0].minor.yy0, NULL);  }
#line 4637 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1375 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, yymsp[-2].minor.yy375, yymsp[-1].minor.yy423,  yymsp[0].minor.yy0, NULL);  }
#line 4642 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 294: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1376 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4647 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1377 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-5].minor.yy299, yymsp[-4].minor.yy0, yymsp[-3].minor.yy375, yymsp[-2].minor.yy423,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4652 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1378 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy423, yymsp[0].minor.yy0, NULL);  }
#line 4657 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1379 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-3].minor.yy299, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy423,  yymsp[0].minor.yy0, NULL);  }
#line 4662 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1380 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4667 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1381 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy133, yymsp[-4].minor.yy299, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy423,  yymsp[-1].minor.yy0, yymsp[0].minor.yy299); 	}
#line 4672 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1400 "bcplus/parser/detail/lemon_parser.y"
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
#line 4702 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* macro_def_lst ::= macro_bnd */
#line 1428 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy269 = new MacroDeclaration::ElementList();
        yygotominor.yy269->push_back(yymsp[0].minor.yy359);
    }
#line 4710 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1434 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy269 = yymsp[-2].minor.yy269;
        yygotominor.yy269->push_back(yymsp[0].minor.yy359);
      yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4719 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1440 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy154;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy359 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy154);
      yy_destructor(yypParser,71,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4733 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1449 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy359 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4744 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* macro_args ::= macro_arg */
#line 1457 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy154 = new MacroSymbol::ArgumentList();
        yygotominor.yy154->push_back(yymsp[0].minor.yy79->str());
        delete yymsp[0].minor.yy79;
    }
#line 4753 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* macro_args ::= macro_args COMMA macro_arg */
#line 1463 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy154 = yymsp[-2].minor.yy154;
        yygotominor.yy154->push_back(yymsp[0].minor.yy79->str());
        delete yymsp[0].minor.yy79;
      yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4763 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* macro_arg ::= POUND_INTEGER */
      case 308: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==308);
#line 1470 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy79 = yymsp[0].minor.yy0;
    }
#line 4771 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* sort_lst ::= sort */
#line 1497 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy195 = new ConstantSymbol::SortList();
		yygotominor.yy195->push_back(yymsp[0].minor.yy93);
	}
#line 4779 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* sort_lst ::= sort_lst COMMA sort */
#line 1502 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy195 = yymsp[-2].minor.yy195;
		yygotominor.yy195->push_back(yymsp[0].minor.yy93);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4788 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* sort ::= sort_id_nr */
      case 317: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==317);
      case 318: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==318);
#line 1527 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy93 = yymsp[0].minor.yy93; }
#line 4795 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* sort ::= sort_id_nr STAR */
#line 1528 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::NONE)); }
#line 4800 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* sort ::= sort_id_nr CARROT */
#line 1529 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-1].minor.yy93, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::UNKNOWN)); }
#line 4805 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* sort ::= sort PLUS object_nullary */
#line 1531 "bcplus/parser/detail/lemon_parser.y"
{ u::ref_ptr<const Object> o_ptr = yymsp[0].minor.yy422; DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, yymsp[0].minor.yy422->symbol()); }
#line 4810 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* sort ::= sort PLUS IDENTIFIER */
#line 1534 "bcplus/parser/detail/lemon_parser.y"
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
#line 4827 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* sort ::= sort PLUS INTEGER */
#line 1548 "bcplus/parser/detail/lemon_parser.y"
{ 
												  ref_ptr<const Object> t_ptr;
												  BASIC_TERM(t_ptr, yymsp[0].minor.yy0);
												  DYNAMIC_SORT_PLUS(yygotominor.yy93, yymsp[-2].minor.yy93, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, t_ptr->symbol()); 
												}
#line 4836 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* sort_nr ::= num_range */
#line 1559 "bcplus/parser/detail/lemon_parser.y"
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
#line 4856 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 320: /* sort_id ::= IDENTIFIER */
#line 1577 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy93 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy93) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4869 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1608 "bcplus/parser/detail/lemon_parser.y"
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
#line 4888 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 322: /* constant_bnd_lst ::= constant_bnd */
#line 1625 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy409 = yymsp[0].minor.yy409;
	}
#line 4895 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 323: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1630 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy409;
		yygotominor.yy409 = yymsp[-2].minor.yy409;
		yygotominor.yy409->splice(yygotominor.yy409->end(), *yymsp[0].minor.yy409);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4905 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 324: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1650 "bcplus/parser/detail/lemon_parser.y"
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
#line 4933 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 325: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1672 "bcplus/parser/detail/lemon_parser.y"
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
#line 4948 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 326: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1683 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy226;
		yygotominor.yy409 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy226) {
			// attempt to declare each symbol
			ref_ptr<SortSymbol> s = parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN);

			// NOTE: additive constants default to the additive sort, not the boolean sort
			if (yymsp[0].minor.yy265 & ConstantSymbol::Type::M_ADDITIVE) s = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

			// external constants should have "unknown" in their sort
			else if (yymsp[0].minor.yy265 & ConstantSymbol::Type::M_EXTERNAL) s = parser->symtab()->carrot(s);

			// non-boolean abActions should contain "none"
			else if (yymsp[0].minor.yy265 == ConstantSymbol::Type::ABACTION && s->domainType() != DomainType::BOOLEAN) s = parser->symtab()->star(s);


			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[0].minor.yy265, decl.first->str(), parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN), decl.second);
			yygotominor.yy409->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 4975 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 327: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1706 "bcplus/parser/detail/lemon_parser.y"
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
#line 5004 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 328: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1730 "bcplus/parser/detail/lemon_parser.y"
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
#line 5085 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* constant_dcl_lst ::= IDENTIFIER */
#line 1806 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = new IdentifierDeclList();
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 5093 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 330: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1811 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = new IdentifierDeclList();
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy195));
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5103 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 331: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1816 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = yymsp[-2].minor.yy226;
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5112 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 332: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1821 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy226 = yymsp[-5].minor.yy226;
		yygotominor.yy226->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy195));
	  yy_destructor(yypParser,102,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5123 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 333: /* constant_dcl_type ::= ABACTION */
#line 1828 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5135 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 334: /* constant_dcl_type ::= ACTION */
#line 1837 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5147 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 335: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1846 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5159 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 336: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1855 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5171 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 337: /* constant_dcl_type ::= EXTERNALACTION */
#line 1864 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5183 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 338: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1873 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5195 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 339: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1882 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5207 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 340: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1891 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5219 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 341: /* constant_dcl_type ::= RIGID */
#line 1900 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5231 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 342: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1909 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5243 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 343: /* constant_dcl_type ::= SDFLUENT */
#line 1919 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy265 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5255 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 344: /* attrib_spec ::= ATTRIBUTE */
#line 1929 "bcplus/parser/detail/lemon_parser.y"
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
#line 5270 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 345: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1942 "bcplus/parser/detail/lemon_parser.y"
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
#line 5286 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1970 "bcplus/parser/detail/lemon_parser.y"
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
#line 5321 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* object_bnd_lst ::= object_bnd */
#line 2003 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy102 = new ObjectDeclaration::ElementList();
		yygotominor.yy102->push_back(yymsp[0].minor.yy294);
	}
#line 5329 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 348: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 2009 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy102 = yymsp[-2].minor.yy102;
		yygotominor.yy102->push_back(yymsp[0].minor.yy294);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5338 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 2015 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy294 = new ObjectDeclaration::Element(yymsp[0].minor.yy93, yymsp[-2].minor.yy425);
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5346 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* object_lst ::= object_spec */
#line 2020 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = yymsp[0].minor.yy425;
	}
#line 5353 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* object_lst ::= object_lst COMMA object_spec */
#line 2024 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy425 = yymsp[-2].minor.yy425;
		yygotominor.yy425->splice(yygotominor.yy425->end(), *yymsp[0].minor.yy425);
		delete yymsp[0].minor.yy425;
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5363 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* object_spec ::= IDENTIFIER */
#line 2033 "bcplus/parser/detail/lemon_parser.y"
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
#line 5379 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 2046 "bcplus/parser/detail/lemon_parser.y"
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
#line 5398 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* object_spec ::= INTEGER */
#line 2061 "bcplus/parser/detail/lemon_parser.y"
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
#line 5414 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* object_spec ::= num_range */
#line 2075 "bcplus/parser/detail/lemon_parser.y"
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
#line 5431 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 2104 "bcplus/parser/detail/lemon_parser.y"
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
#line 5469 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* variable_bnd_lst ::= variable_bnd */
#line 2140 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = yymsp[0].minor.yy410;
	}
#line 5476 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 2145 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = yymsp[-2].minor.yy410;
		yygotominor.yy410->splice(yygotominor.yy410->end(), *yymsp[0].minor.yy410);
		delete yymsp[0].minor.yy410;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5486 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* variable_bnd ::= variable_lst DBL_COLON sort */
#line 2152 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy410 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy52) {
			yygotominor.yy410->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy93));
		}



		delete yymsp[-2].minor.yy52;
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5502 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* variable_lst ::= IDENTIFIER */
#line 2165 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy52 = new TokenList();
		yygotominor.yy52->push_back(yymsp[0].minor.yy0);
	}
#line 5510 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 2170 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy52 = yymsp[-2].minor.yy52;
		yygotominor.yy52->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5519 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 2191 "bcplus/parser/detail/lemon_parser.y"
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
#line 5537 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* sort_bnd_lst ::= sort_bnd */
      case 365: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==365);
#line 2207 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[0].minor.yy196;
	}
#line 5545 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 2212 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[-2].minor.yy196;
		yygotominor.yy196->splice(yygotominor.yy196->end(), *yymsp[0].minor.yy196);
		delete yymsp[0].minor.yy196;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5555 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 2224 "bcplus/parser/detail/lemon_parser.y"
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
#line 5571 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 2236 "bcplus/parser/detail/lemon_parser.y"
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
#line 5586 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 2247 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy196 = yymsp[-1].minor.yy196;
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5595 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 369: /* sort_dcl_lst ::= IDENTIFIER */
#line 2252 "bcplus/parser/detail/lemon_parser.y"
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
#line 5612 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 370: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 2266 "bcplus/parser/detail/lemon_parser.y"
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
#line 5631 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 371: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 2293 "bcplus/parser/detail/lemon_parser.y"
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
#line 5647 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 372: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 2307 "bcplus/parser/detail/lemon_parser.y"
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
#line 5665 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 373: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 2324 "bcplus/parser/detail/lemon_parser.y"
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
#line 5681 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 374: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2338 "bcplus/parser/detail/lemon_parser.y"
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
#line 5699 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 375: /* show_lst ::= show_elem */
#line 2356 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = new ShowStatement::ElementList();
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	}
#line 5707 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 376: /* show_lst ::= show_lst COMMA show_elem */
#line 2361 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = yymsp[-2].minor.yy123;
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5716 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 377: /* show_lst ::= show_lst SEMICOLON show_elem */
#line 2366 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy123 = yymsp[-2].minor.yy123;
		yygotominor.yy123->push_back(yymsp[0].minor.yy423);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5725 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 379: /* stmt_noconcurrency ::= NOCONCURRENCY PERIOD */
#line 2394 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy485, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5730 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 380: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD */
#line 2395 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy446, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5735 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 381: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_int_eval PERIOD */
#line 2421 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5741 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 382: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE DBL_COLON term_int_eval PERIOD */
#line 2422 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,76,&yymsp[-2].minor);
}
#line 5747 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 383: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_int_eval PERIOD */
#line 2423 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5753 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 384: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE DBL_COLON term_int_eval PERIOD */
#line 2424 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy484, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy396, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,76,&yymsp[-2].minor);
}
#line 5759 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 385: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2449 "bcplus/parser/detail/lemon_parser.y"
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
#line 5796 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 386: /* query_lst ::= formula_temporal */
#line 2485 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = NULL;
		yygotominor.yy489.label = NULL;

		yygotominor.yy489.l->push_back(yymsp[0].minor.yy17);
	}
#line 5807 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 387: /* query_lst ::= query_maxstep_decl */
#line 2494 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = yymsp[0].minor.yy29;
		yygotominor.yy489.label = NULL;
	}
#line 5816 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 388: /* query_lst ::= query_label_decl */
#line 2501 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy489.l = new QueryStatement::FormulaList();
		yygotominor.yy489.maxstep = NULL;
		yygotominor.yy489.label = yymsp[0].minor.yy79;
	}
#line 5825 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 389: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2508 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy489 = yymsp[-2].minor.yy489;
		yymsp[-2].minor.yy489.l->push_back(yymsp[0].minor.yy17);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5834 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 390: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2514 "bcplus/parser/detail/lemon_parser.y"
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
#line 5850 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 391: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2527 "bcplus/parser/detail/lemon_parser.y"
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
#line 5866 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 392: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2553 "bcplus/parser/detail/lemon_parser.y"
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
#line 5891 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 393: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range_eval */
#line 2574 "bcplus/parser/detail/lemon_parser.y"
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
#line 5908 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 394: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 395: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==395);
#line 2588 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy79, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5915 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 396: /* clause_if ::= IF formula */
#line 2623 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_IF); 		}
#line 5920 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 397: /* clause_if ::= */
      case 399: /* clause_after ::= */ yytestcase(yyruleno==399);
      case 401: /* clause_ifcons ::= */ yytestcase(yyruleno==401);
      case 405: /* clause_where ::= */ yytestcase(yyruleno==405);
#line 2624 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy17 = NULL; }
#line 5928 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 398: /* clause_after ::= AFTER formula */
#line 2625 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_AFTER);	}
#line 5933 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 400: /* clause_ifcons ::= IFCONS formula */
#line 2627 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_IFCONS); 	}
#line 5938 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 402: /* clause_unless ::= UNLESS atomic_formula_anon */
#line 2629 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy423, yymsp[-1].minor.yy0, yymsp[0].minor.yy423, Language::Feature::CLAUSE_UNLESS); 	}
#line 5943 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 403: /* clause_unless ::= */
#line 2630 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy423 = NULL; }
#line 5948 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 404: /* clause_where ::= WHERE formula_no_const */
#line 2631 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy17, yymsp[-1].minor.yy0, yymsp[0].minor.yy17, Language::Feature::CLAUSE_WHERE); 	}
#line 5953 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 406: /* stmt_law ::= law_basic */
      case 407: /* stmt_law ::= law_caused */ yytestcase(yyruleno==407);
      case 408: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==408);
      case 409: /* stmt_law ::= law_impl */ yytestcase(yyruleno==409);
      case 410: /* stmt_law ::= law_causes */ yytestcase(yyruleno==410);
      case 411: /* stmt_law ::= law_increments */ yytestcase(yyruleno==411);
      case 412: /* stmt_law ::= law_decrements */ yytestcase(yyruleno==412);
      case 413: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==413);
      case 414: /* stmt_law ::= law_always */ yytestcase(yyruleno==414);
      case 415: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==415);
      case 416: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==416);
      case 417: /* stmt_law ::= law_never */ yytestcase(yyruleno==417);
      case 418: /* stmt_law ::= law_default */ yytestcase(yyruleno==418);
      case 419: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==419);
      case 420: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==420);
      case 421: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==421);
      case 422: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==422);
      case 423: /* stmt_law ::= law_observed */ yytestcase(yyruleno==423);
#line 2677 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy484 = yymsp[0].minor.yy484;}
#line 5975 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 424: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2794 "bcplus/parser/detail/lemon_parser.y"
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
#line 5990 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 425: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2806 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy17, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 5997 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 426: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2810 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy17, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 6004 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 427: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2814 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy484, yymsp[-4].minor.yy17, yymsp[-3].minor.yy0, yymsp[-2].minor.yy17, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 6010 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 428: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2817 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy484, yymsp[-6].minor.yy423, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 6016 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 429: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2821 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy484, yymsp[-8].minor.yy423, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-4].minor.yy299, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 6023 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 430: /* law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2824 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy484, yymsp[-8].minor.yy423, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-4].minor.yy299, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_DECREMENTS, DecrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 6030 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 431: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2828 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy484, yymsp[-6].minor.yy423, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 6036 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 432: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2832 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 6043 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 433: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2836 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 6050 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 434: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2840 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 6057 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 435: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2844 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 6064 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 436: /* law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2848 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy423, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 6071 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 437: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2852 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 6078 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 438: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2856 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy484, yymsp[-7].minor.yy0, yymsp[-6].minor.yy225, yymsp[-5].minor.yy17, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, 
																																														yymsp[-2].minor.yy423, yymsp[-1].minor.yy17, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 6085 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 439: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2860 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy484, yymsp[-5].minor.yy0, yymsp[-4].minor.yy17, yymsp[-3].minor.yy17, yymsp[-2].minor.yy423, yymsp[-1].minor.yy17,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 6091 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 440: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2864 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy484, yymsp[-3].minor.yy0, yymsp[-2].minor.yy225, yymsp[-1].minor.yy17, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 6097 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 441: /* law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD */
#line 2869 "bcplus/parser/detail/lemon_parser.y"
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
#line 6116 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 442: /* stmt_code_blk ::= ASP_GR */
#line 2903 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 6121 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 443: /* stmt_code_blk ::= ASP_CP */
#line 2904 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 6126 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 444: /* stmt_code_blk ::= F2LP_GR */
#line 2905 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 6131 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 445: /* stmt_code_blk ::= F2LP_CP */
#line 2906 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 6136 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 446: /* stmt_code_blk ::= LUA_GR */
#line 2907 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 6141 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 447: /* stmt_code_blk ::= LUA_CP */
#line 2908 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy484, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 6146 "bcplus/parser/detail/lemon_parser.c"
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
#line 6212 "bcplus/parser/detail/lemon_parser.c"
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
