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

		
#line 327 "bcplus/parser/detail/lemon_parser.y"

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

#line 516 "bcplus/parser/detail/lemon_parser.y"

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

#line 740 "bcplus/parser/detail/lemon_parser.y"

	#define NUM_UOP(t_new, t, val)																				\
		ref_ptr<const Referenced> t_ptr = t;																			\
		t_new = new Number(val, t->beginLoc(), t->endLoc());

	
	#define NUM_BOP(t_new, l, r, val)																			\
		ref_ptr<const Referenced> l_ptr = l, r_ptr = r;																\
		t_new = new Number(val, l->beginLoc(), r->endLoc());

#line 784 "bcplus/parser/detail/lemon_parser.y"

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

#line 864 "bcplus/parser/detail/lemon_parser.y"


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
	


#line 996 "bcplus/parser/detail/lemon_parser.y"

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



#line 1065 "bcplus/parser/detail/lemon_parser.y"

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

#line 1453 "bcplus/parser/detail/lemon_parser.y"

		
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
#line 1600 "bcplus/parser/detail/lemon_parser.y"

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
#line 2332 "bcplus/parser/detail/lemon_parser.y"

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

#line 2355 "bcplus/parser/detail/lemon_parser.y"

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
#line 2382 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRange const* maxstep;
		Token const* label;
	};

#line 2489 "bcplus/parser/detail/lemon_parser.y"

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

#line 2561 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2647 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2840 "bcplus/parser/detail/lemon_parser.y"

	#define CODE_BLK(stmt, code, feature, type) 												\
		ref_ptr<const Token> code_ptr = code;													\
		if (!parser->lang()->support(feature)) {												\
			stmt = NULL;																		\
			parser->_feature_error(feature, &code->beginLoc());									\
			YYERROR;																			\
		} else {																				\
			stmt = new type(code, code->beginLoc(), code->endLoc());							\
		}
#line 540 "bcplus/parser/detail/lemon_parser.c"
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
#define YYNOCODE 253
#define YYACTIONTYPE unsigned short int
#define lemon_parserTOKENTYPE  Token const* 								
typedef union {
  int yyinit;
  lemon_parserTOKENTYPE yy0;
  SortDeclaration::ElementList* yy7;
  ShowStatement::ElementList* yy11;
  TokenList* yy16;
  Variable* yy37;
  SortSymbol* yy49;
  Object* yy54;
  CardinalityFormula::VariableList* yy55;
  QuantifierFormula::QuantifierList* yy77;
  NumberRange* yy93;
  ConstantDeclaration::ElementList* yy97;
  QueryStatement* yy98;
  VariableDeclaration::ElementList* yy125;
  NCStatement* yy129;
  LuaTerm* yy145;
  QuantifierFormula::Operator::type yy153;
  ConstantSymbol::Type::type yy157;
  ObjectDeclaration::Element::ObjectList* yy189;
  ObjectDeclaration::ElementList* yy190;
  NumberRange const* yy192;
  MacroSymbol* yy235;
  SortDeclaration* yy237;
  MacroDeclaration* yy255;
  QuantifierFormula* yy269;
  ConstantDeclaration* yy271;
  CardinalityFormula* yy273;
  Token const* yy275;
  Term* yy283;
  ObjectDeclaration* yy288;
  ConstantSymbol::SortList* yy291;
  SortSymbol const* yy292;
  StrongNCStatement* yy298;
  ObjectDeclaration::Element* yy302;
  Constant* yy313;
  TermList* yy323;
  Statement* yy328;
  AtomicFormula* yy346;
  MacroDeclaration::ElementList* yy361;
  UNUSED yy409;
  Number* yy416;
  QueryData yy445;
  MacroSymbol::ArgumentList* yy458;
  VariableDeclaration* yy459;
  Formula* yy489;
  IdentifierDeclList* yy498;
  int yy505;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define lemon_parserARG_SDECL  BCParser* parser						;
#define lemon_parserARG_PDECL , BCParser* parser						
#define lemon_parserARG_FETCH  BCParser* parser						 = yypParser->parser						
#define lemon_parserARG_STORE yypParser->parser						 = parser						
#define YYNSTATE 815
#define YYNRULE 429
#define YYERRORSYMBOL 135
#define YYERRSYMDT yy505
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
#define YY_ACTTAB_COUNT (3270)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   814,  657,  343,  813,  812,  811,  810,  809,  808,  807,
 /*    10 */   806,  805,  804,  803,  802,  801,  800,  799,  622,  772,
 /*    20 */   305,  798,  791,  797,  796,  790,   60,  672,  656,  113,
 /*    30 */   111,  110,  301,  306,  815,  498,  445,  658,  659,  735,
 /*    40 */   398,   31,  497,  496,  609,  633,  343,  683,  653,  652,
 /*    50 */   651,  650,   12,  135,  134,  133,  491,  132,  131,  130,
 /*    60 */   129,  231,  734,  733,  649,  736,  647,  278,   93,   92,
 /*    70 */    91,  646,   90,   89,   88,   87,  711,  148,  139,  138,
 /*    80 */   137,  136,   59,  645,  643,  644,  648,  197,  438,  631,
 /*    90 */   437,  632,   98,   97,   96,   95,   94,  225,   58,  564,
 /*   100 */   563,  562,  561,  560,  559,  558,  557,  556,  555,  554,
 /*   110 */   553,  552,  551,  550,  549,  548,  547,  771,  521,  793,
 /*   120 */   655,  522,  770,  531,  530,  527,  526,  529,  528,  792,
 /*   130 */    57,  223,   17,   16,   20,   19,  221,  612,   21,  798,
 /*   140 */   791,  797,  796,  790,    8,  167,  165,  164,   29,  118,
 /*   150 */     6,    7,  122,  190,  229,  248,  788,  771,  521,  118,
 /*   160 */   242,  522,  770,  519,  764,  763,  765,    5,  474,  473,
 /*   170 */     4,  297,   28,  234,  326,  585,  582,  581,  580,  731,
 /*   180 */   732,  605,  619,  163,  307,  183,  118,  121,   72,   61,
 /*   190 */   739,  293,   20,   19,  294,  194,   21,  208,  178,   26,
 /*   200 */   186,  192,  193,  201,  764,  763,  765,  611,  521,  572,
 /*   210 */   571,  522,  610,  519,  146,  144,  142,  141,  140,  720,
 /*   220 */   721,  119,  203,  428,  617,    2,  104,   24,  499,  479,
 /*   230 */   480,  293,  619,  642,  525,  295,  611,  521,   50,   49,
 /*   240 */   522,  610,   51,  633,  343,  783,  524,  789,  523, 1017,
 /*   250 */   189,  674,  475, 1017,  589,  588,  476,  447,  639,  446,
 /*   260 */    22,   23,   47,   46,   50,   49,  104,  229,   51,  578,
 /*   270 */   579,  207,  196,  427,  617,   30,   21,   45,  117,  115,
 /*   280 */   113,  111,  110,  589,  588,  590,  524,  789,  523,  635,
 /*   290 */   520,  752,  746,  798,  791,  797,  796,  790,  578,  579,
 /*   300 */    71,  196,  372,  665,   30,  787,   45,  664,  771,  521,
 /*   310 */    43,   44,  522,  770,  185,  621,  128,  123,  786,  794,
 /*   320 */   795,  798,  791,  797,  796,  790,  458,  397,  772,  305,
 /*   330 */   798,  791,  452,  796,  790,  118,   70,  789,  523,   43,
 /*   340 */    44,  300,  306,  490,  191,  128,  318,  728,  725,  724,
 /*   350 */   723,  722,  657,  343,  616,  764,  763,  765,  519,   13,
 /*   360 */   453,   18,   17,   16,   20,   19,  789,  523,   21,   56,
 /*   370 */   731,  732,  222,  220,  219,   69,  183,  518,  121,   51,
 /*   380 */   707,  521,  293,  191,  522,  706,  444,  445,  658,  659,
 /*   390 */   786,  794,  795,  798,  791,  797,  796,  790,  457,  397,
 /*   400 */  1153,  772,  305,  798,  791,  452,  796,  790,   80,   41,
 /*   410 */    40,  769,  119,   42,  300,  306,  638,  104,   64,  320,
 /*   420 */   728,  725,  724,  723,  722,  277, 1153,  700,  699,  701,
 /*   430 */   607,  521,   14,  453,  522,  606,  182,  524,  789,  523,
 /*   440 */   614,  293,  690,  691,  109,  108,  107,  106,  105,  672,
 /*   450 */    54,  188,  786,  794,  795,  798,  791,  797,  796,  790,
 /*   460 */   456,  397,  633,  343,  612,  177,  798,  791,  797,  796,
 /*   470 */   790,  254,  226,  224,  222,  220,  219,  601,  600,  602,
 /*   480 */   426,  785,  521,   52,   53,  522,  784,  198,  199,  150,
 /*   490 */   753,  485,  603,  604,  712,  623,  524,  501,  297,  687,
 /*   500 */   147,  327,  585,  582,  581,  580,  637,  437,  632,  524,
 /*   510 */   789,  523,  772,  305,  798,  791,  797,  796,  790,  225,
 /*   520 */   172,   68,  198,  199,  118,  301,  306,  232,  779,  778,
 /*   530 */   780,   63,  735,  730,  145,  425,  568,  567,  519,  143,
 /*   540 */   142,  141,  140,  781,  782,   18,   17,   16,   20,   19,
 /*   550 */   225,  116,   21,  223,  292,  734,  733,  516,  221,  524,
 /*   560 */   789,  523,  117,  115,  113,  111,  110,  612,   62,  798,
 /*   570 */   791,  452,  796,  790,  772,  305,  798,  791,  797,  796,
 /*   580 */   790,  660,  661,  654,  223,  114,  686,  300,  306,  221,
 /*   590 */   112,  685,  345,  728,  725,  724,  723,  722,   34,  453,
 /*   600 */   719,  297,  570,  569,  310,  585,  582,  581,  580,  684,
 /*   610 */   524,  789,  523,  180,   25,  435,  626,  434,  749,  772,
 /*   620 */   305,  798,  791,  797,  796,  790,  641,   18,   17,   16,
 /*   630 */    20,   19,  298,  306,   21,  717,  716,  316,  728,  725,
 /*   640 */   724,  723,  722,   15,  772,  304,  798,  791,  797,  796,
 /*   650 */   790,  171,  169,  167,  165,  164,  180,  729,  306,  433,
 /*   660 */   429,  484,  714,  728,  725,  724,  723,  722,  786,  794,
 /*   670 */   795,  798,  791,  797,  796,  790,  455,  397,  117,  115,
 /*   680 */   113,  111,  110,  772,  305,  798,  791,  797,  796,  790,
 /*   690 */   500,  227,  681,  630,  450,  675,  300,  306,   67,  629,
 /*   700 */   434,  727,  728,  725,  724,  723,  722,  226,  224,  222,
 /*   710 */   220,  219,  663,  206,  772,  305,  798,  791,  797,  796,
 /*   720 */   790,  676,   18,   17,   16,   20,   19,  300,  306,   21,
 /*   730 */   678,  679,  726,  728,  725,  724,  723,  722,  657,  343,
 /*   740 */   772,  305,  798,  791,  797,  796,  790,  229,   48,   47,
 /*   750 */    46,   50,   49,  300,  306,   51,  717,  716,  514,  728,
 /*   760 */   725,  724,  723,  722,   36,  772,  305,  798,  791,  797,
 /*   770 */   796,  790,  492,  445,  658,  659,   66,  295,  300,  306,
 /*   780 */   181,  482,  494,  513,  728,  725,  724,  723,  722,  625,
 /*   790 */   673,  446,  772,  305,  798,  791,  797,  796,  790,  495,
 /*   800 */    18,   17,   16,   20,   19,  300,  306,   21,  512,  200,
 /*   810 */   365,  728,  725,  724,  723,  722,  176,  772,  305,  798,
 /*   820 */   791,  797,  796,  790,  179,   65,  489,   25,  481,  295,
 /*   830 */   300,  306,  520,  789,  175,  413,  728,  725,  724,  723,
 /*   840 */   722,  772,  305,  798,  791,  797,  796,  790,  640,   18,
 /*   850 */    17,   16,   20,   19,  300,  306,   21,  230,  290,  412,
 /*   860 */   728,  725,  724,  723,  722,    9,  519,  717,  716,  772,
 /*   870 */   305,  798,  791,  797,  796,  790,  295,   61,  180,  628,
 /*   880 */   634,  202,  300,  306,  520,  619,  174,  321,  728,  725,
 /*   890 */   724,  723,  722,  512,  173,  772,  305,  798,  791,  797,
 /*   900 */   796,  790,  146,  144,  142,  141,  140,  483,  300,  306,
 /*   910 */   180,  624,  511,  319,  728,  725,  724,  723,  722,  618,
 /*   920 */   772,  305,  798,  791,  797,  796,  790,  620,  577,  448,
 /*   930 */   677,  520,  730,  300,  306,  615,  524,  218,  317,  728,
 /*   940 */   725,  724,  723,  722, 1188,  432,  484,  772,  305,  798,
 /*   950 */   791,  797,  796,  790,  253,   39,   38,   37,   41,   40,
 /*   960 */   302,  306,   42,  573, 1188,  339,  728,  725,  724,  723,
 /*   970 */   722,  195,  772,  305,  798,  791,  797,  796,  790,  431,
 /*   980 */   484,  217,  713,   27,   42,  300,  306,  424,  430,  484,
 /*   990 */   325,  728,  725,  724,  723,  722,  772,  305,  798,  791,
 /*  1000 */   797,  796,  790,  149,   18,   17,   16,   20,   19,  300,
 /*  1010 */   306,   21,  405,  484,  324,  728,  725,  724,  723,  722,
 /*  1020 */   511,  524,  228,  290,  772,  305,  798,  791,  797,  796,
 /*  1030 */   790,   98,   97,   96,   95,   94,  566,  300,  306,  565,
 /*  1040 */   730,  546,  214,  728,  725,  724,  723,  722,  657,  343,
 /*  1050 */   772,  305,  798,  791,  797,  796,  790,  103,  102,  101,
 /*  1060 */   100,   99,  459,  300,  306,  545, 1245,    1,  213,  728,
 /*  1070 */   725,  724,  723,  722,  544,  772,  305,  798,  791,  797,
 /*  1080 */   796,  790,  442,  445,  658,  659,  127,  543,  300,  306,
 /*  1090 */   740,  542,  541,  212,  728,  725,  724,  723,  722,  574,
 /*  1100 */   343,  671,  772,  305,  798,  791,  797,  796,  790,  540,
 /*  1110 */    39,   38,   37,   41,   40,  300,  306,   42,  539,  537,
 /*  1120 */   211,  728,  725,  724,  723,  722,  613,  772,  305,  798,
 /*  1130 */   791,  797,  796,  790,  226,  224,  222,  220,  219,  536,
 /*  1140 */   300,  306,  535,  534,  533,  210,  728,  725,  724,  723,
 /*  1150 */   722,  772,  305,  798,  791,  797,  796,  790,  226,  224,
 /*  1160 */   222,  220,  219,    3,  300,  306,  769,  520,  738,  209,
 /*  1170 */   728,  725,  724,  723,  722,   79,   78,   77,   31,   76,
 /*  1180 */    75,   74,   73,  736,  289,  789,  524,  657,  343,   12,
 /*  1190 */   135,  134,  133,   25,  132,  131,  130,  129,  718,  109,
 /*  1200 */   108,  107,  106,  105,  288,  715,  772,  305,  798,  791,
 /*  1210 */   797,  796,  790,  507,  148,  139,  138,  137,  136,  301,
 /*  1220 */   306,  486,  445,  658,  659,  187,  735,  730,  612,   55,
 /*  1230 */   798,  791,  452,  796,  790,  786,  794,  795,  798,  791,
 /*  1240 */   797,  796,  790,  451,  397,  506,  505,  285,  281,  734,
 /*  1250 */   733,  504,  752,  746,  798,  791,  797,  796,  790,   35,
 /*  1260 */   453,  126,  297,  414,  282,  312,  585,  582,  581,  580,
 /*  1270 */   772,  305,  798,  791,  797,  796,  790,  503,  280,  682,
 /*  1280 */   279,  688,  449,  301,  306,   39,   38,   37,   41,   40,
 /*  1290 */   735,  730,   42,  502,  636,  772,  305,  798,  791,  797,
 /*  1300 */   796,  790,  274,   48,   47,   46,   50,   49,  301,  306,
 /*  1310 */    51,  656,  272,  734,  733,  735,  730,  275,   11,   10,
 /*  1320 */   752,  746,  798,  791,  797,  796,  790,  657,  343,  184,
 /*  1330 */   454,  371,  171,  169,  167,  165,  164,  267,  734,  733,
 /*  1340 */     3,   79,   78,   77,  273,   76,   75,   74,   73,  271,
 /*  1350 */   472,  269,   79,   78,   77,  270,   76,   75,   74,   73,
 /*  1360 */   443,  493,  445,  658,  659,  109,  108,  107,  106,  105,
 /*  1370 */   771,  521,  705,  470,  522,  770,  109,  108,  107,  106,
 /*  1380 */   105,  157,  156,  155,  266,  154,  153,  152,  151,  708,
 /*  1390 */   698,  798,  791,  797,  796,  790,   80,   86,   85,  265,
 /*  1400 */    84,   83,   82,   81,  303,  162,  161,  160,  159,  158,
 /*  1410 */   268,  264,  263,  538,  344,  695,  692,  764,  763,  765,
 /*  1420 */   103,  102,  101,  100,   99,  882,  882,  882,  469,  882,
 /*  1430 */   882,  882,  882,  751,  521,  261,  259,  522,  750,  468,
 /*  1440 */   121,  157,  156,  155,  467,  154,  153,  152,  151,  882,
 /*  1450 */   882,  882,  882,  882,  257,  255,   79,   78,   77,  466,
 /*  1460 */    76,   75,   74,   73,  465,  162,  161,  160,  159,  158,
 /*  1470 */   252,  251,  250,  247,  120,  249,  246,  464,  244,  104,
 /*  1480 */   109,  108,  107,  106,  105,  786,  794,  795,  798,  791,
 /*  1490 */   797,  796,  790,  245,  396,  747,  748,  243,  241,  524,
 /*  1500 */   789,  523,  463,  170,  240,  239,  462,  612,  238,  798,
 /*  1510 */   791,  797,  796,  790,  237,  235,  657,  343,  461,  233,
 /*  1520 */   612,  460,  798,  791,  797,  796,  790,  680,  276,  786,
 /*  1530 */   794,  795,  798,  791,  797,  796,  790,  168,  422,  291,
 /*  1540 */   662,  296,  166,  351,  308,  585,  582,  581,  580,  441,
 /*  1550 */   493,  445,  658,  659,  587,  406,  342,  576,  585,  582,
 /*  1560 */   581,  580,  524,  789,  523,  407,  226,  224,  222,  220,
 /*  1570 */   219,  670,  612,  666,  798,  791,  797,  796,  790,  669,
 /*  1580 */   612,  668,  798,  791,  797,  796,  790,  667,  612,  471,
 /*  1590 */   798,  791,  797,  796,  790,  341,  612,  286,  798,  791,
 /*  1600 */   797,  796,  790,  283,  340,  488,  297,  262,  260,  584,
 /*  1610 */   585,  582,  581,  580,  297,  627,  258,  583,  585,  582,
 /*  1620 */   581,  580,  297,  256,  236,  478,  585,  582,  581,  580,
 /*  1630 */   297, 1246, 1246,  477,  585,  582,  581,  580,  612, 1246,
 /*  1640 */   798,  791,  797,  796,  790,   38,   37,   41,   40, 1246,
 /*  1650 */   612,   42,  798,  791,  797,  796,  790, 1246,  612, 1246,
 /*  1660 */   798,  791,  797,  796,  790, 1246, 1246, 1246, 1246, 1246,
 /*  1670 */  1246,   33,  297, 1246,  284,  349,  585,  582,  581,  580,
 /*  1680 */  1246,  287, 1246, 1246,  297,   32, 1246,  400,  585,  582,
 /*  1690 */   581,  580,  297, 1246, 1246,  399,  585,  582,  581,  580,
 /*  1700 */   612, 1246,  798,  791,  797,  796,  790, 1246, 1246,  612,
 /*  1710 */  1246,  798,  791,  797,  796,  790, 1246,  612, 1246,  798,
 /*  1720 */   791,  797,  796,  790, 1246, 1246,  786,  794,  795,  798,
 /*  1730 */   791,  797,  796,  790,  297,  348, 1246,  313,  585,  582,
 /*  1740 */   581,  580, 1246,  297, 1246, 1246,  311,  585,  582,  581,
 /*  1750 */   580,  297, 1246, 1246,  309,  585,  582,  581,  580, 1246,
 /*  1760 */  1246,  708,  698,  798,  791,  797,  796,  790,  708,  698,
 /*  1770 */   798,  791,  797,  796,  790, 1246,  299, 1246, 1246, 1246,
 /*  1780 */   737, 1246, 1246,  697, 1246,  519,  315,  695,  692, 1246,
 /*  1790 */  1090, 1246, 1246,  689,  695,  692,  708,  698,  798,  791,
 /*  1800 */   797,  796,  790, 1246,  517, 1090, 1090,  205,  453, 1246,
 /*  1810 */  1246,  303, 1246,  708,  698,  798,  791,  797,  796,  790,
 /*  1820 */  1246,  694,  695,  692, 1246, 1090, 1090, 1246,  303, 1246,
 /*  1830 */   708,  698,  798,  791,  797,  796,  790, 1090,  693,  695,
 /*  1840 */   692, 1246, 1246, 1090, 1246,  303,  708,  698,  798,  791,
 /*  1850 */   797,  796,  790, 1246,  737,  509,  695,  692, 1246,  519,
 /*  1860 */  1246,  303, 1246, 1246,  708,  698,  798,  791,  797,  796,
 /*  1870 */   790,  508,  695,  692, 1246, 1246, 1246,  125,  515,  303,
 /*  1880 */  1246,  204,  453, 1246, 1246, 1246, 1246, 1246, 1246,  352,
 /*  1890 */   695,  692,  708,  698,  798,  791,  797,  796,  790, 1246,
 /*  1900 */  1246,   39,   38,   37,   41,   40, 1246,  303,   42,  708,
 /*  1910 */   698,  798,  791,  797,  796,  790, 1246,  409,  695,  692,
 /*  1920 */  1246, 1246, 1246, 1246,  303,  772,  394,  798,  791,  797,
 /*  1930 */   796,  790, 1246, 1246,  408,  695,  692, 1246,  347,  395,
 /*  1940 */  1246,  772,  760,  798,  791,  797,  796,  790, 1246, 1246,
 /*  1950 */  1246, 1246, 1246, 1246,  762,  395,  772,  322,  798,  791,
 /*  1960 */   797,  796,  790, 1246, 1246, 1246, 1246, 1246, 1246,  762,
 /*  1970 */   395,  786,  794,  795,  798,  791,  797,  796,  790, 1246,
 /*  1980 */   423, 1246,  786,  794,  795,  798,  791,  797,  796,  790,
 /*  1990 */  1246,  777,  786,  794,  795,  798,  791,  797,  796,  790,
 /*  2000 */    25,  773,  786,  794,  795,  798,  791,  797,  796,  790,
 /*  2010 */  1246,  776, 1246, 1246, 1246,  786,  794,  795,  798,  791,
 /*  2020 */   797,  796,  790, 1246,  775,  786,  794,  795,  798,  791,
 /*  2030 */   797,  796,  790, 1246,  774, 1246, 1246, 1246, 1246, 1246,
 /*  2040 */  1246, 1246, 1246,  786,  794,  795,  798,  791,  797,  796,
 /*  2050 */   790, 1246,  421,   18,   17,   16,   20,   19, 1246, 1246,
 /*  2060 */    21,  786,  794,  795,  798,  791,  797,  796,  790, 1246,
 /*  2070 */   420,  786,  794,  795,  798,  791,  797,  796,  790, 1246,
 /*  2080 */   768,  786,  794,  795,  798,  791,  797,  796,  790, 1246,
 /*  2090 */   767, 1246,  786,  794,  795,  798,  791,  797,  796,  790,
 /*  2100 */  1246,  766, 1246, 1246, 1246, 1246, 1246, 1246,  786,  794,
 /*  2110 */   795,  798,  791,  797,  796,  790, 1246,  761, 1246, 1246,
 /*  2120 */  1246, 1246, 1246,  786,  794,  795,  798,  791,  797,  796,
 /*  2130 */   790, 1246,  419, 1246,  786,  794,  795,  798,  791,  797,
 /*  2140 */   796,  790, 1246,  418, 1246,  786,  794,  795,  798,  791,
 /*  2150 */   797,  796,  790, 1246,  759,  786,  794,  795,  798,  791,
 /*  2160 */   797,  796,  790, 1246,  758,  786,  794,  795,  798,  791,
 /*  2170 */   797,  796,  790, 1246,  757,  786,  794,  795,  798,  791,
 /*  2180 */   797,  796,  790,  124,  417,  786,  794,  795,  798,  791,
 /*  2190 */   797,  796,  790, 1246,  416, 1246, 1246,  786,  794,  795,
 /*  2200 */   798,  791,  797,  796,  790, 1246,  756,   39,   38,   37,
 /*  2210 */    41,   40, 1246, 1246,   42,  786,  794,  795,  798,  791,
 /*  2220 */   797,  796,  790, 1246,  755,  786,  794,  795,  798,  791,
 /*  2230 */   797,  796,  790, 1246,  754,  786,  794,  795,  798,  791,
 /*  2240 */   797,  796,  790, 1246,  393, 1246,  786,  794,  795,  798,
 /*  2250 */   791,  797,  796,  790, 1246,  392, 1246, 1246, 1246, 1246,
 /*  2260 */  1246, 1246,  786,  794,  795,  798,  791,  797,  796,  790,
 /*  2270 */  1246,  391, 1246, 1246, 1246, 1246, 1246,  786,  794,  795,
 /*  2280 */   798,  791,  797,  796,  790, 1246,  390, 1246,  786,  794,
 /*  2290 */   795,  798,  791,  797,  796,  790, 1246,  389, 1246,  786,
 /*  2300 */   794,  795,  798,  791,  797,  796,  790, 1246,  388,  786,
 /*  2310 */   794,  795,  798,  791,  797,  796,  790, 1246,  387,  786,
 /*  2320 */   794,  795,  798,  791,  797,  796,  790, 1246,  386,  786,
 /*  2330 */   794,  795,  798,  791,  797,  796,  790, 1246,  385,  786,
 /*  2340 */   794,  795,  798,  791,  797,  796,  790, 1246,  384, 1246,
 /*  2350 */  1246,  786,  794,  795,  798,  791,  797,  796,  790, 1246,
 /*  2360 */   383,   39,   38,   37,   41,   40, 1246, 1246,   42,  786,
 /*  2370 */   794,  795,  798,  791,  797,  796,  790, 1246,  382,  786,
 /*  2380 */   794,  795,  798,  791,  797,  796,  790, 1246,  381,  786,
 /*  2390 */   794,  795,  798,  791,  797,  796,  790, 1246,  380, 1246,
 /*  2400 */   786,  794,  795,  798,  791,  797,  796,  790, 1246,  379,
 /*  2410 */  1246,  439,  487, 1246, 1246, 1246,  786,  794,  795,  798,
 /*  2420 */   791,  797,  796,  790, 1246,  378, 1246, 1246, 1246, 1246,
 /*  2430 */  1246,  786,  794,  795,  798,  791,  797,  796,  790, 1246,
 /*  2440 */   377, 1246,  786,  794,  795,  798,  791,  797,  796,  790,
 /*  2450 */  1246,  376, 1246,  786,  794,  795,  798,  791,  797,  796,
 /*  2460 */   790, 1246,  375,  786,  794,  795,  798,  791,  797,  796,
 /*  2470 */   790, 1246,  374,  786,  794,  795,  798,  791,  797,  796,
 /*  2480 */   790,  225,  373,  786,  794,  795,  798,  791,  797,  796,
 /*  2490 */   790, 1246,  369,  786,  794,  795,  798,  791,  797,  796,
 /*  2500 */   790, 1246,  368, 1246, 1246,  786,  794,  795,  798,  791,
 /*  2510 */   797,  796,  790, 1246,  367,  223, 1246, 1246, 1246, 1246,
 /*  2520 */   221, 1246, 1246,  786,  794,  795,  798,  791,  797,  796,
 /*  2530 */   790, 1246,  366,  786,  794,  795,  798,  791,  797,  796,
 /*  2540 */   790, 1246,  364,  786,  794,  795,  798,  791,  797,  796,
 /*  2550 */   790, 1246,  363, 1246,  786,  794,  795,  798,  791,  797,
 /*  2560 */   796,  790, 1246,  362, 1246, 1246, 1246, 1246, 1246, 1246,
 /*  2570 */   786,  794,  795,  798,  791,  797,  796,  790, 1246,  361,
 /*  2580 */  1246, 1246, 1246, 1246, 1246,  786,  794,  795,  798,  791,
 /*  2590 */   797,  796,  790, 1246,  360, 1246,  786,  794,  795,  798,
 /*  2600 */   791,  797,  796,  790, 1246,  216, 1246,  786,  794,  795,
 /*  2610 */   798,  791,  797,  796,  790, 1246,  215,  786,  794,  795,
 /*  2620 */   798,  791,  797,  796,  790, 1246,  350,  752,  746,  798,
 /*  2630 */   791,  797,  796,  790, 1246, 1246, 1246, 1246,  346,  752,
 /*  2640 */   746,  798,  791,  797,  796,  790, 1246, 1246, 1246, 1246,
 /*  2650 */   415,  752,  746,  798,  791,  797,  796,  790, 1246, 1246,
 /*  2660 */  1246, 1246,  745,  752,  746,  798,  791,  797,  796,  790,
 /*  2670 */  1246, 1246, 1246, 1246,  741, 1246, 1246, 1246,  752,  746,
 /*  2680 */   798,  791,  797,  796,  790, 1246, 1246, 1246, 1246,  744,
 /*  2690 */   752,  746,  798,  791,  797,  796,  790, 1246, 1246, 1246,
 /*  2700 */  1246,  743,  752,  746,  798,  791,  797,  796,  790, 1246,
 /*  2710 */  1246, 1246, 1246,  742,  752,  746,  798,  791,  797,  796,
 /*  2720 */   790, 1246, 1246, 1246, 1246,  370,  752,  746,  798,  791,
 /*  2730 */   797,  796,  790, 1246, 1246, 1246, 1246,  411, 1246, 1246,
 /*  2740 */   752,  746,  798,  791,  797,  796,  790, 1246, 1246, 1246,
 /*  2750 */  1246,  410,  752,  746,  798,  791,  797,  796,  790, 1246,
 /*  2760 */  1246, 1246, 1246,  704,  752,  746,  798,  791,  797,  796,
 /*  2770 */   790, 1246, 1246, 1246, 1246,  703,  752,  746,  798,  791,
 /*  2780 */   797,  796,  790, 1246, 1246, 1246, 1246,  702,  752,  746,
 /*  2790 */   798,  791,  797,  796,  790, 1246, 1246, 1246, 1246,  359,
 /*  2800 */   752,  746,  798,  791,  797,  796,  790, 1246, 1246, 1246,
 /*  2810 */  1246,  358,  752,  746,  798,  791,  797,  796,  790, 1246,
 /*  2820 */  1246, 1246, 1246,  357,  752,  746,  798,  791,  797,  796,
 /*  2830 */   790, 1246, 1246, 1246, 1246,  356,  752,  746,  798,  791,
 /*  2840 */   797,  796,  790, 1246, 1246, 1246, 1246,  355,  752,  746,
 /*  2850 */   798,  791,  797,  796,  790, 1246, 1246, 1246, 1246,  354,
 /*  2860 */   752,  746,  798,  791,  797,  796,  790, 1246, 1246, 1246,
 /*  2870 */  1246,  353,  752,  746,  798,  791,  797,  796,  790, 1246,
 /*  2880 */  1246, 1246, 1246,  696,  752,  746,  798,  791,  797,  796,
 /*  2890 */   790, 1246, 1246, 1246, 1246,  323,  608,  599,  798,  791,
 /*  2900 */   797,  796,  790, 1246, 1246, 1246,  608,  599,  798,  791,
 /*  2910 */   797,  796,  790,  608,  599,  798,  791,  797,  796,  790,
 /*  2920 */   608,  599,  798,  791,  797,  796,  790, 1246, 1246,  402,
 /*  2930 */   608,  599,  798,  791,  797,  796,  790, 1246, 1246,  314,
 /*  2940 */  1246, 1246, 1246, 1246, 1246, 1246,  403, 1246, 1246, 1246,
 /*  2950 */  1246, 1246, 1246,  598,  608,  599,  798,  791,  797,  796,
 /*  2960 */   790, 1246, 1246,  404, 1246, 1246,  608,  599,  798,  791,
 /*  2970 */   797,  796,  790,  608,  599,  798,  791,  797,  796,  790,
 /*  2980 */   608,  599,  798,  791,  797,  796,  790,  597, 1246, 1246,
 /*  2990 */   608,  599,  798,  791,  797,  796,  790, 1246, 1246,  596,
 /*  3000 */  1246, 1246, 1246, 1246, 1246, 1246,  595, 1246, 1246, 1246,
 /*  3010 */  1246, 1246, 1246,  594,  608,  599,  798,  791,  797,  796,
 /*  3020 */   790, 1246, 1246,  401, 1246, 1246,  608,  599,  798,  791,
 /*  3030 */   797,  796,  790,  608,  599,  798,  791,  797,  796,  790,
 /*  3040 */   608,  599,  798,  791,  797,  796,  790,  593, 1246, 1246,
 /*  3050 */   608,  599,  798,  791,  797,  796,  790, 1246, 1246,  592,
 /*  3060 */  1246, 1246, 1246, 1246, 1246, 1246,  591,  657,  343, 1246,
 /*  3070 */  1246, 1246, 1246,  338,  608,  599,  798,  791,  797,  796,
 /*  3080 */   790, 1246, 1246,  337,  608,  599,  798,  791,  797,  796,
 /*  3090 */   790,  608,  599,  798,  791,  797,  796,  790, 1246, 1246,
 /*  3100 */   440,  493,  445,  658,  659, 1246, 1246,  336,  608,  599,
 /*  3110 */   798,  791,  797,  796,  790, 1246, 1246,  335, 1246, 1246,
 /*  3120 */  1246, 1246, 1246, 1246,  334,  608,  599,  798,  791,  797,
 /*  3130 */   796,  790,  608,  599,  798,  791,  797,  796,  790, 1246,
 /*  3140 */  1246,  333,  608,  599,  798,  791,  797,  796,  790,  608,
 /*  3150 */   599,  798,  791,  797,  796,  790,   10, 1246,  332,   55,
 /*  3160 */   672,  656, 1246, 1246, 1246,  586, 1246, 1246,  608,  599,
 /*  3170 */   798,  791,  797,  796,  790,  331, 1246,  657,  343, 1246,
 /*  3180 */   575, 1246,  330,  608,  599,  798,  791,  797,  796,  790,
 /*  3190 */  1246,   18,   17,   16,   20,   19, 1246,  511,   21, 1246,
 /*  3200 */   532,  329,   39,   38,   37,   41,   40, 1246,  510,   42,
 /*  3210 */   436,  493,  445,  658,  659, 1246,  328,  710, 1246, 1246,
 /*  3220 */   709, 1246, 1246, 1246,   18,   17,   16,   20,   19, 1246,
 /*  3230 */   225,   21,  171,  169,  167,  165,  164, 1246, 1246, 1246,
 /*  3240 */  1246, 1246, 1246, 1246, 1246, 1246, 1246, 1246, 1246, 1246,
 /*  3250 */  1246, 1246, 1246, 1246, 1246, 1246, 1246, 1246, 1246, 1246,
 /*  3260 */  1246, 1246, 1246, 1246,  223, 1246, 1246, 1246, 1246,  221,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   135,  170,  171,  138,  139,  140,  141,  142,  143,  144,
 /*    10 */   145,  146,  147,  148,  149,  150,  151,  152,    2,  154,
 /*    20 */   155,  156,  157,  158,  159,  160,   71,    1,    2,  107,
 /*    30 */   108,  109,  167,  168,    0,  204,  205,  206,  207,  174,
 /*    40 */   175,   69,  211,  212,   72,  170,  171,   73,   22,   23,
 /*    50 */    24,   25,   80,   81,   82,   83,   30,   85,   86,   87,
 /*    60 */    88,  196,  197,  198,   38,  155,   40,   93,   81,   82,
 /*    70 */    83,   45,   85,   86,   87,   88,  166,  105,  106,  107,
 /*    80 */   108,  109,   71,   57,   58,   59,   60,   71,  213,  214,
 /*    90 */   215,  216,  105,  106,  107,  108,  109,   71,   71,  234,
 /*   100 */   235,  236,  237,  238,  239,  240,  241,  242,  243,  244,
 /*   110 */   245,  246,  247,  248,  249,  250,  251,    1,    2,   72,
 /*   120 */    72,    5,    6,    7,    8,    9,   10,   11,   12,   72,
 /*   130 */    71,  105,   95,   96,   97,   98,  110,  154,  101,  156,
 /*   140 */   157,  158,  159,  160,   28,  107,  108,  109,   32,  102,
 /*   150 */    34,   35,   71,   37,  106,   39,   72,    1,    2,  102,
 /*   160 */    44,    5,    6,  163,   48,   49,   50,   51,   52,   53,
 /*   170 */    54,  188,   56,   57,  191,  192,  193,  194,  195,   63,
 /*   180 */    64,   72,  182,   81,   68,   69,  102,   71,   70,   80,
 /*   190 */    98,   75,   97,   98,  102,   14,  101,   16,   17,   18,
 /*   200 */    19,   20,   21,   71,   48,   49,   50,    1,    2,  226,
 /*   210 */   227,    5,    6,  163,  105,  106,  107,  108,  109,   63,
 /*   220 */    64,  105,   66,  223,  224,   69,  110,   71,   96,   48,
 /*   230 */    49,   75,  182,   72,  118,   75,    1,    2,   97,   98,
 /*   240 */     5,    6,  101,  170,  171,   72,  130,  131,  132,   98,
 /*   250 */   134,   73,   46,  102,   48,   49,   50,  208,  209,  210,
 /*   260 */   104,  105,   95,   96,   97,   98,  110,  106,  101,   63,
 /*   270 */    64,   93,   66,  223,  224,   69,  101,   71,  105,  106,
 /*   280 */   107,  108,  109,   48,   49,   50,  130,  131,  132,  216,
 /*   290 */   130,  154,  155,  156,  157,  158,  159,  160,   63,   64,
 /*   300 */    70,   66,  165,  107,   69,   72,   71,  111,    1,    2,
 /*   310 */   104,  105,    5,    6,  133,   73,  110,   76,  153,  154,
 /*   320 */   155,  156,  157,  158,  159,  160,  161,  162,  154,  155,
 /*   330 */   156,  157,  158,  159,  160,  102,   70,  131,  132,  104,
 /*   340 */   105,  167,  168,  102,  102,  110,  172,  173,  174,  175,
 /*   350 */   176,  177,  170,  171,   73,   48,   49,   50,  163,  185,
 /*   360 */   186,   94,   95,   96,   97,   98,  131,  132,  101,   71,
 /*   370 */    63,   64,  107,  108,  109,   70,   69,  182,   71,  101,
 /*   380 */     1,    2,   75,  102,    5,    6,  204,  205,  206,  207,
 /*   390 */   153,  154,  155,  156,  157,  158,  159,  160,  161,  162,
 /*   400 */    76,  154,  155,  156,  157,  158,  159,  160,   81,   97,
 /*   410 */    98,   72,  105,  101,  167,  168,   73,  110,   81,  172,
 /*   420 */   173,  174,  175,  176,  177,   76,  102,   48,   49,   50,
 /*   430 */     1,    2,  185,  186,    5,    6,   93,  130,  131,  132,
 /*   440 */    73,   75,   63,   64,  105,  106,  107,  108,  109,    1,
 /*   450 */    71,  102,  153,  154,  155,  156,  157,  158,  159,  160,
 /*   460 */   161,  162,  170,  171,  154,   76,  156,  157,  158,  159,
 /*   470 */   160,  105,  105,  106,  107,  108,  109,   48,   49,   50,
 /*   480 */     1,    1,    2,  104,  105,    5,    6,   99,  100,  110,
 /*   490 */    72,  102,   63,   64,   72,   72,  130,   72,  188,   73,
 /*   500 */    71,  191,  192,  193,  194,  195,  214,  215,  216,  130,
 /*   510 */   131,  132,  154,  155,  156,  157,  158,  159,  160,   71,
 /*   520 */   102,   70,   99,  100,  102,  167,  168,  102,   48,   49,
 /*   530 */    50,   31,  174,  175,  105,  225,  226,  227,  163,  110,
 /*   540 */   107,  108,  109,   63,   64,   94,   95,   96,   97,   98,
 /*   550 */    71,   71,  101,  105,  196,  197,  198,  182,  110,  130,
 /*   560 */   131,  132,  105,  106,  107,  108,  109,  154,   31,  156,
 /*   570 */   157,  158,  159,  160,  154,  155,  156,  157,  158,  159,
 /*   580 */   160,    1,    2,   72,  105,  105,   73,  167,  168,  110,
 /*   590 */   110,   73,  172,  173,  174,  175,  176,  177,  185,  186,
 /*   600 */    67,  188,    1,    2,  191,  192,  193,  194,  195,   73,
 /*   610 */   130,  131,  132,  102,   41,  217,  218,  219,   72,  154,
 /*   620 */   155,  156,  157,  158,  159,  160,   72,   94,   95,   96,
 /*   630 */    97,   98,  167,  168,  101,   90,   91,  172,  173,  174,
 /*   640 */   175,  176,  177,   98,  154,  155,  156,  157,  158,  159,
 /*   650 */   160,  105,  106,  107,  108,  109,  102,  167,  168,  220,
 /*   660 */   221,  222,  172,  173,  174,  175,  176,  177,  153,  154,
 /*   670 */   155,  156,  157,  158,  159,  160,  161,  162,  105,  106,
 /*   680 */   107,  108,  109,  154,  155,  156,  157,  158,  159,  160,
 /*   690 */    96,   89,   74,   73,  199,  200,  167,  168,   70,  218,
 /*   700 */   219,  172,  173,  174,  175,  176,  177,  105,  106,  107,
 /*   710 */   108,  109,  132,   93,  154,  155,  156,  157,  158,  159,
 /*   720 */   160,   74,   94,   95,   96,   97,   98,  167,  168,  101,
 /*   730 */     3,    4,  172,  173,  174,  175,  176,  177,  170,  171,
 /*   740 */   154,  155,  156,  157,  158,  159,  160,  106,   94,   95,
 /*   750 */    96,   97,   98,  167,  168,  101,   90,   91,  172,  173,
 /*   760 */   174,  175,  176,  177,   98,  154,  155,  156,  157,  158,
 /*   770 */   159,  160,  204,  205,  206,  207,   70,   75,  167,  168,
 /*   780 */    71,   27,    2,  172,  173,  174,  175,  176,  177,   73,
 /*   790 */   209,  210,  154,  155,  156,  157,  158,  159,  160,   55,
 /*   800 */    94,   95,   96,   97,   98,  167,  168,  101,    2,   93,
 /*   810 */   172,  173,  174,  175,  176,  177,   71,  154,  155,  156,
 /*   820 */   157,  158,  159,  160,   71,   70,    2,   41,   27,   75,
 /*   830 */   167,  168,  130,  131,   71,  172,  173,  174,  175,  176,
 /*   840 */   177,  154,  155,  156,  157,  158,  159,  160,   72,   94,
 /*   850 */    95,   96,   97,   98,  167,  168,  101,  183,  184,  172,
 /*   860 */   173,  174,  175,  176,  177,   79,  163,   90,   91,  154,
 /*   870 */   155,  156,  157,  158,  159,  160,   75,   80,  102,    2,
 /*   880 */    72,   75,  167,  168,  130,  182,   71,  172,  173,  174,
 /*   890 */   175,  176,  177,    2,   71,  154,  155,  156,  157,  158,
 /*   900 */   159,  160,  105,  106,  107,  108,  109,  102,  167,  168,
 /*   910 */   102,    2,  155,  172,  173,  174,  175,  176,  177,   73,
 /*   920 */   154,  155,  156,  157,  158,  159,  160,  224,   67,  201,
 /*   930 */   202,  130,  175,  167,  168,   73,  130,   81,  172,  173,
 /*   940 */   174,  175,  176,  177,   73,  221,  222,  154,  155,  156,
 /*   950 */   157,  158,  159,  160,  197,   94,   95,   96,   97,   98,
 /*   960 */   167,  168,  101,   73,   93,  172,  173,  174,  175,  176,
 /*   970 */   177,   76,  154,  155,  156,  157,  158,  159,  160,  221,
 /*   980 */   222,   81,   72,   93,  101,  167,  168,   76,  221,  222,
 /*   990 */   172,  173,  174,  175,  176,  177,  154,  155,  156,  157,
 /*  1000 */   158,  159,  160,   65,   94,   95,   96,   97,   98,  167,
 /*  1010 */   168,  101,  221,  222,  172,  173,  174,  175,  176,  177,
 /*  1020 */   155,  130,  183,  184,  154,  155,  156,  157,  158,  159,
 /*  1030 */   160,  105,  106,  107,  108,  109,   73,  167,  168,   73,
 /*  1040 */   175,   73,  172,  173,  174,  175,  176,  177,  170,  171,
 /*  1050 */   154,  155,  156,  157,  158,  159,  160,  105,  106,  107,
 /*  1060 */   108,  109,  197,  167,  168,   73,  136,  137,  172,  173,
 /*  1070 */   174,  175,  176,  177,   73,  154,  155,  156,  157,  158,
 /*  1080 */   159,  160,  204,  205,  206,  207,   70,   73,  167,  168,
 /*  1090 */   163,   73,   73,  172,  173,  174,  175,  176,  177,  170,
 /*  1100 */   171,   72,  154,  155,  156,  157,  158,  159,  160,   73,
 /*  1110 */    94,   95,   96,   97,   98,  167,  168,  101,   73,   73,
 /*  1120 */   172,  173,  174,  175,  176,  177,   73,  154,  155,  156,
 /*  1130 */   157,  158,  159,  160,  105,  106,  107,  108,  109,   73,
 /*  1140 */   167,  168,   73,   73,   73,  172,  173,  174,  175,  176,
 /*  1150 */   177,  154,  155,  156,  157,  158,  159,  160,  105,  106,
 /*  1160 */   107,  108,  109,   69,  167,  168,   72,  130,  158,  172,
 /*  1170 */   173,  174,  175,  176,  177,   81,   82,   83,   69,   85,
 /*  1180 */    86,   87,   88,  155,  229,  131,  130,  170,  171,   80,
 /*  1190 */    81,   82,   83,   41,   85,   86,   87,   88,  158,  105,
 /*  1200 */   106,  107,  108,  109,  232,  158,  154,  155,  156,  157,
 /*  1210 */   158,  159,  160,  233,  105,  106,  107,  108,  109,  167,
 /*  1220 */   168,  204,  205,  206,  207,   61,  174,  175,  154,   62,
 /*  1230 */   156,  157,  158,  159,  160,  153,  154,  155,  156,  157,
 /*  1240 */   158,  159,  160,  161,  162,  155,  233,  232,  196,  197,
 /*  1250 */   198,  155,  154,  155,  156,  157,  158,  159,  160,  185,
 /*  1260 */   186,   70,  188,  165,  232,  191,  192,  193,  194,  195,
 /*  1270 */   154,  155,  156,  157,  158,  159,  160,  233,  229,  200,
 /*  1280 */   232,   72,    2,  167,  168,   94,   95,   96,   97,   98,
 /*  1290 */   174,  175,  101,  233,  207,  154,  155,  156,  157,  158,
 /*  1300 */   159,  160,  230,   94,   95,   96,   97,   98,  167,  168,
 /*  1310 */   101,    2,  196,  197,  198,  174,  175,  231,   42,   26,
 /*  1320 */   154,  155,  156,  157,  158,  159,  160,  170,  171,   69,
 /*  1330 */   164,  165,  105,  106,  107,  108,  109,  196,  197,  198,
 /*  1340 */    69,   81,   82,   83,  232,   85,   86,   87,   88,  229,
 /*  1350 */   233,  230,   81,   82,   83,  231,   85,   86,   87,   88,
 /*  1360 */   203,  204,  205,  206,  207,  105,  106,  107,  108,  109,
 /*  1370 */     1,    2,   72,  233,    5,    6,  105,  106,  107,  108,
 /*  1380 */   109,   81,   82,   83,  229,   85,   86,   87,   88,  154,
 /*  1390 */   155,  156,  157,  158,  159,  160,   81,   82,   83,  231,
 /*  1400 */    85,   86,   87,   88,  169,  105,  106,  107,  108,  109,
 /*  1410 */   232,  230,  232,  155,  179,  180,  181,   48,   49,   50,
 /*  1420 */   105,  106,  107,  108,  109,   81,   82,   83,  233,   85,
 /*  1430 */    86,   87,   88,    1,    2,  232,  232,    5,    6,  233,
 /*  1440 */    71,   81,   82,   83,  233,   85,   86,   87,   88,  105,
 /*  1450 */   106,  107,  108,  109,  232,  232,   81,   82,   83,  233,
 /*  1460 */    85,   86,   87,   88,  233,  105,  106,  107,  108,  109,
 /*  1470 */   229,  231,  230,  155,  105,  232,  229,  233,  230,  110,
 /*  1480 */   105,  106,  107,  108,  109,  153,  154,  155,  156,  157,
 /*  1490 */   158,  159,  160,  231,  162,   63,   64,  232,  155,  130,
 /*  1500 */   131,  132,  233,   71,  229,  231,  233,  154,  230,  156,
 /*  1510 */   157,  158,  159,  160,  232,  232,  170,  171,  233,  155,
 /*  1520 */   154,  233,  156,  157,  158,  159,  160,  202,  229,  153,
 /*  1530 */   154,  155,  156,  157,  158,  159,  160,  105,  162,  184,
 /*  1540 */   157,  188,  110,  171,  191,  192,  193,  194,  195,  203,
 /*  1550 */   204,  205,  206,  207,  188,  171,  171,  191,  192,  193,
 /*  1560 */   194,  195,  130,  131,  132,  171,  105,  106,  107,  108,
 /*  1570 */   109,  171,  154,  171,  156,  157,  158,  159,  160,  171,
 /*  1580 */   154,  171,  156,  157,  158,  159,  160,  171,  154,  233,
 /*  1590 */   156,  157,  158,  159,  160,  171,  154,  229,  156,  157,
 /*  1600 */   158,  159,  160,  229,  171,    2,  188,  230,  230,  191,
 /*  1610 */   192,  193,  194,  195,  188,    2,  230,  191,  192,  193,
 /*  1620 */   194,  195,  188,  230,  229,  191,  192,  193,  194,  195,
 /*  1630 */   188,  252,  252,  191,  192,  193,  194,  195,  154,  252,
 /*  1640 */   156,  157,  158,  159,  160,   95,   96,   97,   98,  252,
 /*  1650 */   154,  101,  156,  157,  158,  159,  160,  252,  154,  252,
 /*  1660 */   156,  157,  158,  159,  160,  252,  252,  252,  252,  252,
 /*  1670 */   252,   33,  188,  252,   36,  191,  192,  193,  194,  195,
 /*  1680 */   252,   43,  252,  252,  188,   47,  252,  191,  192,  193,
 /*  1690 */   194,  195,  188,  252,  252,  191,  192,  193,  194,  195,
 /*  1700 */   154,  252,  156,  157,  158,  159,  160,  252,  252,  154,
 /*  1710 */   252,  156,  157,  158,  159,  160,  252,  154,  252,  156,
 /*  1720 */   157,  158,  159,  160,  252,  252,  153,  154,  155,  156,
 /*  1730 */   157,  158,  159,  160,  188,  162,  252,  191,  192,  193,
 /*  1740 */   194,  195,  252,  188,  252,  252,  191,  192,  193,  194,
 /*  1750 */   195,  188,  252,  252,  191,  192,  193,  194,  195,  252,
 /*  1760 */   252,  154,  155,  156,  157,  158,  159,  160,  154,  155,
 /*  1770 */   156,  157,  158,  159,  160,  252,  169,  252,  252,  252,
 /*  1780 */   158,  252,  252,  169,  252,  163,  179,  180,  181,  252,
 /*  1790 */    26,  252,  252,  179,  180,  181,  154,  155,  156,  157,
 /*  1800 */   158,  159,  160,  252,  182,   41,   42,  185,  186,  252,
 /*  1810 */   252,  169,  252,  154,  155,  156,  157,  158,  159,  160,
 /*  1820 */   252,  179,  180,  181,  252,   61,   62,  252,  169,  252,
 /*  1830 */   154,  155,  156,  157,  158,  159,  160,   73,  179,  180,
 /*  1840 */   181,  252,  252,   79,  252,  169,  154,  155,  156,  157,
 /*  1850 */   158,  159,  160,  252,  158,  179,  180,  181,  252,  163,
 /*  1860 */   252,  169,  252,  252,  154,  155,  156,  157,  158,  159,
 /*  1870 */   160,  179,  180,  181,  252,  252,  252,   70,  182,  169,
 /*  1880 */   252,  185,  186,  252,  252,  252,  252,  252,  252,  179,
 /*  1890 */   180,  181,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  1900 */   252,   94,   95,   96,   97,   98,  252,  169,  101,  154,
 /*  1910 */   155,  156,  157,  158,  159,  160,  252,  179,  180,  181,
 /*  1920 */   252,  252,  252,  252,  169,  154,  155,  156,  157,  158,
 /*  1930 */   159,  160,  252,  252,  179,  180,  181,  252,  167,  168,
 /*  1940 */   252,  154,  155,  156,  157,  158,  159,  160,  252,  252,
 /*  1950 */   252,  252,  252,  252,  167,  168,  154,  155,  156,  157,
 /*  1960 */   158,  159,  160,  252,  252,  252,  252,  252,  252,  167,
 /*  1970 */   168,  153,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  1980 */   162,  252,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  1990 */   252,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2000 */    41,  162,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2010 */   252,  162,  252,  252,  252,  153,  154,  155,  156,  157,
 /*  2020 */   158,  159,  160,  252,  162,  153,  154,  155,  156,  157,
 /*  2030 */   158,  159,  160,  252,  162,  252,  252,  252,  252,  252,
 /*  2040 */   252,  252,  252,  153,  154,  155,  156,  157,  158,  159,
 /*  2050 */   160,  252,  162,   94,   95,   96,   97,   98,  252,  252,
 /*  2060 */   101,  153,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  2070 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  2080 */   162,  153,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  2090 */   162,  252,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2100 */   252,  162,  252,  252,  252,  252,  252,  252,  153,  154,
 /*  2110 */   155,  156,  157,  158,  159,  160,  252,  162,  252,  252,
 /*  2120 */   252,  252,  252,  153,  154,  155,  156,  157,  158,  159,
 /*  2130 */   160,  252,  162,  252,  153,  154,  155,  156,  157,  158,
 /*  2140 */   159,  160,  252,  162,  252,  153,  154,  155,  156,  157,
 /*  2150 */   158,  159,  160,  252,  162,  153,  154,  155,  156,  157,
 /*  2160 */   158,  159,  160,  252,  162,  153,  154,  155,  156,  157,
 /*  2170 */   158,  159,  160,  252,  162,  153,  154,  155,  156,  157,
 /*  2180 */   158,  159,  160,   70,  162,  153,  154,  155,  156,  157,
 /*  2190 */   158,  159,  160,  252,  162,  252,  252,  153,  154,  155,
 /*  2200 */   156,  157,  158,  159,  160,  252,  162,   94,   95,   96,
 /*  2210 */    97,   98,  252,  252,  101,  153,  154,  155,  156,  157,
 /*  2220 */   158,  159,  160,  252,  162,  153,  154,  155,  156,  157,
 /*  2230 */   158,  159,  160,  252,  162,  153,  154,  155,  156,  157,
 /*  2240 */   158,  159,  160,  252,  162,  252,  153,  154,  155,  156,
 /*  2250 */   157,  158,  159,  160,  252,  162,  252,  252,  252,  252,
 /*  2260 */   252,  252,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2270 */   252,  162,  252,  252,  252,  252,  252,  153,  154,  155,
 /*  2280 */   156,  157,  158,  159,  160,  252,  162,  252,  153,  154,
 /*  2290 */   155,  156,  157,  158,  159,  160,  252,  162,  252,  153,
 /*  2300 */   154,  155,  156,  157,  158,  159,  160,  252,  162,  153,
 /*  2310 */   154,  155,  156,  157,  158,  159,  160,  252,  162,  153,
 /*  2320 */   154,  155,  156,  157,  158,  159,  160,  252,  162,  153,
 /*  2330 */   154,  155,  156,  157,  158,  159,  160,  252,  162,  153,
 /*  2340 */   154,  155,  156,  157,  158,  159,  160,  252,  162,  252,
 /*  2350 */   252,  153,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  2360 */   162,   94,   95,   96,   97,   98,  252,  252,  101,  153,
 /*  2370 */   154,  155,  156,  157,  158,  159,  160,  252,  162,  153,
 /*  2380 */   154,  155,  156,  157,  158,  159,  160,  252,  162,  153,
 /*  2390 */   154,  155,  156,  157,  158,  159,  160,  252,  162,  252,
 /*  2400 */   153,  154,  155,  156,  157,  158,  159,  160,  252,  162,
 /*  2410 */   252,    1,    2,  252,  252,  252,  153,  154,  155,  156,
 /*  2420 */   157,  158,  159,  160,  252,  162,  252,  252,  252,  252,
 /*  2430 */   252,  153,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  2440 */   162,  252,  153,  154,  155,  156,  157,  158,  159,  160,
 /*  2450 */   252,  162,  252,  153,  154,  155,  156,  157,  158,  159,
 /*  2460 */   160,  252,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2470 */   160,  252,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2480 */   160,   71,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2490 */   160,  252,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2500 */   160,  252,  162,  252,  252,  153,  154,  155,  156,  157,
 /*  2510 */   158,  159,  160,  252,  162,  105,  252,  252,  252,  252,
 /*  2520 */   110,  252,  252,  153,  154,  155,  156,  157,  158,  159,
 /*  2530 */   160,  252,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2540 */   160,  252,  162,  153,  154,  155,  156,  157,  158,  159,
 /*  2550 */   160,  252,  162,  252,  153,  154,  155,  156,  157,  158,
 /*  2560 */   159,  160,  252,  162,  252,  252,  252,  252,  252,  252,
 /*  2570 */   153,  154,  155,  156,  157,  158,  159,  160,  252,  162,
 /*  2580 */   252,  252,  252,  252,  252,  153,  154,  155,  156,  157,
 /*  2590 */   158,  159,  160,  252,  162,  252,  153,  154,  155,  156,
 /*  2600 */   157,  158,  159,  160,  252,  162,  252,  153,  154,  155,
 /*  2610 */   156,  157,  158,  159,  160,  252,  162,  153,  154,  155,
 /*  2620 */   156,  157,  158,  159,  160,  252,  162,  154,  155,  156,
 /*  2630 */   157,  158,  159,  160,  252,  252,  252,  252,  165,  154,
 /*  2640 */   155,  156,  157,  158,  159,  160,  252,  252,  252,  252,
 /*  2650 */   165,  154,  155,  156,  157,  158,  159,  160,  252,  252,
 /*  2660 */   252,  252,  165,  154,  155,  156,  157,  158,  159,  160,
 /*  2670 */   252,  252,  252,  252,  165,  252,  252,  252,  154,  155,
 /*  2680 */   156,  157,  158,  159,  160,  252,  252,  252,  252,  165,
 /*  2690 */   154,  155,  156,  157,  158,  159,  160,  252,  252,  252,
 /*  2700 */   252,  165,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  2710 */   252,  252,  252,  165,  154,  155,  156,  157,  158,  159,
 /*  2720 */   160,  252,  252,  252,  252,  165,  154,  155,  156,  157,
 /*  2730 */   158,  159,  160,  252,  252,  252,  252,  165,  252,  252,
 /*  2740 */   154,  155,  156,  157,  158,  159,  160,  252,  252,  252,
 /*  2750 */   252,  165,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  2760 */   252,  252,  252,  165,  154,  155,  156,  157,  158,  159,
 /*  2770 */   160,  252,  252,  252,  252,  165,  154,  155,  156,  157,
 /*  2780 */   158,  159,  160,  252,  252,  252,  252,  165,  154,  155,
 /*  2790 */   156,  157,  158,  159,  160,  252,  252,  252,  252,  165,
 /*  2800 */   154,  155,  156,  157,  158,  159,  160,  252,  252,  252,
 /*  2810 */   252,  165,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  2820 */   252,  252,  252,  165,  154,  155,  156,  157,  158,  159,
 /*  2830 */   160,  252,  252,  252,  252,  165,  154,  155,  156,  157,
 /*  2840 */   158,  159,  160,  252,  252,  252,  252,  165,  154,  155,
 /*  2850 */   156,  157,  158,  159,  160,  252,  252,  252,  252,  165,
 /*  2860 */   154,  155,  156,  157,  158,  159,  160,  252,  252,  252,
 /*  2870 */   252,  165,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  2880 */   252,  252,  252,  165,  154,  155,  156,  157,  158,  159,
 /*  2890 */   160,  252,  252,  252,  252,  165,  154,  155,  156,  157,
 /*  2900 */   158,  159,  160,  252,  252,  252,  154,  155,  156,  157,
 /*  2910 */   158,  159,  160,  154,  155,  156,  157,  158,  159,  160,
 /*  2920 */   154,  155,  156,  157,  158,  159,  160,  252,  252,  187,
 /*  2930 */   154,  155,  156,  157,  158,  159,  160,  252,  252,  187,
 /*  2940 */   252,  252,  252,  252,  252,  252,  187,  252,  252,  252,
 /*  2950 */   252,  252,  252,  187,  154,  155,  156,  157,  158,  159,
 /*  2960 */   160,  252,  252,  187,  252,  252,  154,  155,  156,  157,
 /*  2970 */   158,  159,  160,  154,  155,  156,  157,  158,  159,  160,
 /*  2980 */   154,  155,  156,  157,  158,  159,  160,  187,  252,  252,
 /*  2990 */   154,  155,  156,  157,  158,  159,  160,  252,  252,  187,
 /*  3000 */   252,  252,  252,  252,  252,  252,  187,  252,  252,  252,
 /*  3010 */   252,  252,  252,  187,  154,  155,  156,  157,  158,  159,
 /*  3020 */   160,  252,  252,  187,  252,  252,  154,  155,  156,  157,
 /*  3030 */   158,  159,  160,  154,  155,  156,  157,  158,  159,  160,
 /*  3040 */   154,  155,  156,  157,  158,  159,  160,  187,  252,  252,
 /*  3050 */   154,  155,  156,  157,  158,  159,  160,  252,  252,  187,
 /*  3060 */   252,  252,  252,  252,  252,  252,  187,  170,  171,  252,
 /*  3070 */   252,  252,  252,  187,  154,  155,  156,  157,  158,  159,
 /*  3080 */   160,  252,  252,  187,  154,  155,  156,  157,  158,  159,
 /*  3090 */   160,  154,  155,  156,  157,  158,  159,  160,  252,  252,
 /*  3100 */   203,  204,  205,  206,  207,  252,  252,  187,  154,  155,
 /*  3110 */   156,  157,  158,  159,  160,  252,  252,  187,  252,  252,
 /*  3120 */   252,  252,  252,  252,  187,  154,  155,  156,  157,  158,
 /*  3130 */   159,  160,  154,  155,  156,  157,  158,  159,  160,  252,
 /*  3140 */   252,  187,  154,  155,  156,  157,  158,  159,  160,  154,
 /*  3150 */   155,  156,  157,  158,  159,  160,   26,  252,  187,   62,
 /*  3160 */     1,    2,  252,  252,  252,  187,  252,  252,  154,  155,
 /*  3170 */   156,  157,  158,  159,  160,  187,  252,  170,  171,  252,
 /*  3180 */    72,  252,  187,  154,  155,  156,  157,  158,  159,  160,
 /*  3190 */   252,   94,   95,   96,   97,   98,  252,  155,  101,  252,
 /*  3200 */    73,  187,   94,   95,   96,   97,   98,  252,  166,  101,
 /*  3210 */   203,  204,  205,  206,  207,  252,  187,  175,  252,  252,
 /*  3220 */   178,  252,  252,  252,   94,   95,   96,   97,   98,  252,
 /*  3230 */    71,  101,  105,  106,  107,  108,  109,  252,  252,  252,
 /*  3240 */   252,  252,  252,  252,  252,  252,  252,  252,  252,  252,
 /*  3250 */   252,  252,  252,  252,  252,  252,  252,  252,  252,  252,
 /*  3260 */   252,  252,  252,  252,  105,  252,  252,  252,  252,  110,
};
#define YY_SHIFT_USE_DFLT (-79)
#define YY_SHIFT_COUNT (525)
#define YY_SHIFT_MIN   (-78)
#define YY_SHIFT_MAX   (3159)
static const short yy_shift_ofst[] = {
 /*     0 */   -79,  116,  156,  156,  156,  156,  156,  156,  156,  156,
 /*    10 */   156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
 /*    20 */   156,  156,  156,  156,  156,  156,  206,  206,  307,  307,
 /*    30 */   235,  235,  307,  307,  235,  235,  235,  235,  235,  235,
 /*    40 */   235,  235,  235,  235,  235,  235,  379,  379,  379,  379,
 /*    50 */   379,  379,  379,  379,  379,  379,  480,  480,  480,  480,
 /*    60 */   480,  480,  480,  480,  480,  480,  480,  480,  480,  480,
 /*    70 */   480,  480,  480,  480,  480,  480,  480,  480,  480,  480,
 /*    80 */   480,  480,  480,  480,  480,  480,  480,  480,  480,  480,
 /*    90 */   480,  480,  480,  480,  480,  480,  480,  480,  480,  480,
 /*   100 */   480,  480,  480,  480,  480,  480,  480,  480,  480,  480,
 /*   110 */   480,  480,  480,  480,  480,  480,  480,  480,  480, 1369,
 /*   120 */  1369, 1369, 1432,   26,  429,  429,  429,  429,  429,  429,
 /*   130 */   429,  429,  429,  429,  429,  429,  429,  429,  429,  429,
 /*   140 */   429,  429,  429,  429,  429,  429,  429,  429,  429, 1432,
 /*   150 */  1432, 1432, 1432, 1432, 1432, 1432, 1432, 1432, 1432, 1432,
 /*   160 */  1432, 1432, 1432, 1432, 1432, 1432, 1432, 1432, 1432, 1432,
 /*   170 */  1432, 1432, 1432, 3159, 3159, 3159, 3159, 3159, 2410, 3159,
 /*   180 */  3159, 3159, 2410,  702,  702,  801,  754,  806, 2410,  366,
 /*   190 */   366,  160,   16, 1613, 1603,  479,  777,   16,   16,   16,
 /*   200 */    16,  727,  891,  777,  160,  160, 1613, 1603, 1280, 1959,
 /*   210 */  3130, 3130, 3130, 3130, 3097,  573,  573,  448,  448,  448,
 /*   220 */   448,  448,  448,  448,  448,  448,  448,  448,  666,  580,
 /*   230 */   545,  786,  727, 1167, 1056, 1167, 1164, 1167, 1164, 1293,
 /*   240 */  1276, 1152, 1056, 1167, 1164, 1293, 1276, 1152, 1056, 1167,
 /*   250 */  1164, 1293, 1276, 1152, 1056, 1167, 1164, 1167, 1164, 1167,
 /*   260 */  1164, 1167, 1164, 1167, 1164, 1293, 1276, 1152, 1167, 1164,
 /*   270 */  1293, 1276, 1152, 1167, 1164, 1293, 1276, 1309, 1280, 1167,
 /*   280 */  1164, 1152, 1167, 1164, 1056, 1167, 1164, 1056, 1167, 1164,
 /*   290 */  1054, 1054, 1152, 1056, 1054, 1037,  -28, 1109, 1094, 1300,
 /*   300 */  1271, 1260, 1375, 1360, 1344, 1315,  -13,  181, 3108, 2113,
 /*   310 */  1807, 1191, 1016,  861,  109, 1209,  910,  755,  706,  628,
 /*   320 */   451,  533, 1764, 3127,  267,  267, 2267, 2267,  797,  797,
 /*   330 */   797,  797,  797,  797,  797,  797,  797,  797,  797,  267,
 /*   340 */  1053,  367, 1029,  602,  654,  267,  546,  339,  173, 1550,
 /*   350 */   457, 1461,  167, 1227, 1227, 1227, 1227, 1227, 1227, 1227,
 /*   360 */   457,  457,  457,  457,  457,   37,  457,  457,  457,  457,
 /*   370 */  1227, 1227, 1227,  457,  457,  457,  457,  457,  457,  457,
 /*   380 */   457,  457,  457,  457,  457,  457,  457,  457,  457,  457,
 /*   390 */   457,  457,  457,  457,  952,  926,  457,  457, 1638,  312,
 /*   400 */   312,  433,  433,  433,  433,  423,  265,  265,  141,  141,
 /*   410 */    38,   38,   95,   95,   38,   38,  -78,  -78,  -78,  -78,
 /*   420 */   -78,  -78,  -78,  -78,  601,  890,  871,  281,  242,  388,
 /*   430 */   388,  388,  388,  716,  389,  620,  808,  349,  343,  324,
 /*   440 */   776,  554,  161,  511,   48,  196,  241,  178,  425,  132,
 /*   450 */   -26,  422,  151,   92,  418,  233,   84,   57,   47,  938,
 /*   460 */  1071, 1070, 1069, 1066, 1046, 1045, 1036, 1019, 1018, 1014,
 /*   470 */  1001,  992,  968,  966,  963,  911,  895,  883,  883,  900,
 /*   480 */   856,  862,  846,  909,  805,  877,  641,  823,  815,  763,
 /*   490 */   824,  753,  641,  641,  745,  780,  744,  709,  641,  647,
 /*   500 */   618,  594,  536,  518,  537,  513,  500,  426,  278,  278,
 /*   510 */   337,  327,  298,  175,  175,  305,  266,  230,  118,  102,
 /*   520 */    81,   59,   27,   11,  -45,   34,
};
#define YY_REDUCE_USE_DFLT (-170)
#define YY_REDUCE_COUNT (295)
#define YY_REDUCE_MIN   (-169)
#define YY_REDUCE_MAX   (3042)
static const short yy_reduce_ofst[] = {
 /*     0 */   930, -135,  247,  174,  997,  973,  948,  921,  896,  870,
 /*    10 */   842,  818,  793,  766,  741,  715,  687,  663,  638,  611,
 /*    20 */   586,  560,  529,  490,  465,  420,  310,  -17, 1141, 1116,
 /*    30 */  1074,  413, 1052,  358, 1563, 1555, 1546, 1504, 1496, 1484,
 /*    40 */  1442, 1434, 1426, 1418, 1366, 1353, 1755, 1738, 1710, 1692,
 /*    50 */  1676, 1659, 1642, 1614, 1607, 1235, 1082,  515,  299,  237,
 /*    60 */   165, 2464, 2454, 2443, 2432, 2417, 2401, 2390, 2380, 2370,
 /*    70 */  2352, 2340, 2330, 2320, 2310, 2300, 2289, 2278, 2263, 2247,
 /*    80 */  2236, 2226, 2216, 2198, 2186, 2176, 2166, 2156, 2146, 2135,
 /*    90 */  2124, 2109, 2093, 2082, 2072, 2062, 2044, 2032, 2022, 2012,
 /*   100 */  2002, 1992, 1981, 1970, 1955, 1939, 1928, 1918, 1908, 1890,
 /*   110 */  1872, 1862, 1849, 1839, 1829, 1818, 1573, 1376, 1332, 1802,
 /*   120 */  1787, 1771, 1166, -169, 3029, 3014, 2995, 2988, 2978, 2971,
 /*   130 */  2954, 2937, 2930, 2920, 2896, 2886, 2879, 2872, 2860, 2836,
 /*   140 */  2826, 2819, 2812, 2800, 2776, 2766, 2759, 2752, 2742, 2730,
 /*   150 */  2718, 2706, 2694, 2682, 2670, 2658, 2646, 2634, 2622, 2610,
 /*   160 */  2598, 2586, 2572, 2560, 2548, 2536, 2524, 2509, 2497, 2485,
 /*   170 */  2473, 1098,  137, 3007, 2897, 1346, 1157, 1017, -125,  878,
 /*   180 */   568,  182,  292, 1696, 1622,   50,    0, 3042,   73,  865,
 /*   190 */   757,  703,  439,  398,   49,  929,  839,  791,  767,  758,
 /*   200 */   724,  728,  -90,  674,  375,  195,  481,  581,  495, 1395,
 /*   210 */  1393, 1386, 1378, 1377, 1356, 1374, 1368, 1433, 1424, 1416,
 /*   220 */  1410, 1408, 1402, 1400, 1394, 1385, 1384, 1372, 1355, 1383,
 /*   230 */  1355, 1299, 1325, 1288, 1364, 1285, 1283, 1273, 1282, 1278,
 /*   240 */  1274, 1275, 1343, 1269, 1265, 1248, 1262, 1247, 1318, 1244,
 /*   250 */  1243, 1242, 1240, 1241, 1258, 1231, 1223, 1226, 1222, 1211,
 /*   260 */  1204, 1206, 1203, 1195, 1180, 1181, 1168, 1155, 1140, 1178,
 /*   270 */  1121, 1124, 1120, 1117, 1112, 1072, 1086, 1087, 1079, 1060,
 /*   280 */  1048, 1049, 1044, 1032, 1096, 1013, 1015, 1090,  980,  972,
 /*   290 */  1047, 1040,  955, 1028, 1010,  927,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   816, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*    10 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*    20 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*    30 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*    40 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*    50 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*    60 */  1244, 1244, 1244, 1244, 1244, 1009, 1013, 1008, 1012, 1095,
 /*    70 */  1091, 1096, 1092, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*    80 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*    90 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   100 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   110 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   120 */  1244, 1244, 1244, 1244, 1077, 1081, 1076, 1080, 1244, 1244,
 /*   130 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   140 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   150 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   160 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   170 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   180 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   190 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   200 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1193,
 /*   210 */  1195, 1195, 1195, 1195, 1201, 1193, 1193, 1244, 1244, 1244,
 /*   220 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   230 */  1244, 1193, 1244, 1201, 1244, 1201, 1199, 1201, 1199, 1195,
 /*   240 */  1197, 1193, 1244, 1201, 1199, 1195, 1197, 1193, 1244, 1201,
 /*   250 */  1199, 1195, 1197, 1193, 1244, 1201, 1199, 1201, 1199, 1201,
 /*   260 */  1199, 1201, 1199, 1201, 1199, 1195, 1197, 1193, 1201, 1199,
 /*   270 */  1195, 1197, 1193, 1201, 1199, 1195, 1197, 1244, 1244, 1201,
 /*   280 */  1199, 1193, 1201, 1199, 1244, 1201, 1199, 1244, 1201, 1199,
 /*   290 */  1244, 1244, 1193, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   300 */  1244, 1244, 1044, 1244,  973,  973, 1244, 1244, 1244, 1244,
 /*   310 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   320 */  1244, 1244,  882, 1244, 1194, 1196, 1185, 1182, 1079, 1083,
 /*   330 */  1078, 1082, 1074, 1073, 1072, 1071, 1070, 1069, 1068, 1062,
 /*   340 */  1244, 1244, 1244, 1244, 1200, 1192, 1244, 1244, 1244, 1059,
 /*   350 */  1031,  927,  987,  999,  998,  997,  996,  995,  994,  993,
 /*   360 */   979, 1011, 1015, 1010, 1014,  944, 1097, 1093, 1098, 1094,
 /*   370 */  1002,  855,  856,  959,  958,  957,  956,  955,  954,  953,
 /*   380 */   975,  972,  971,  970,  969,  968,  967,  966,  965,  964,
 /*   390 */   963,  962,  961,  960, 1244, 1244,  852,  851, 1089, 1061,
 /*   400 */  1060, 1048, 1047, 1032, 1033, 1244,  932,  933,  989,  988,
 /*   410 */   909,  908,  946,  945,  922,  923,  884,  883,  889,  888,
 /*   420 */   894,  893,  868,  869, 1244, 1244,  928, 1244, 1244, 1162,
 /*   430 */  1166, 1165, 1163, 1244, 1244, 1244, 1244, 1244, 1244,  928,
 /*   440 */  1244, 1244, 1244, 1244, 1244, 1110, 1244, 1244, 1244, 1244,
 /*   450 */  1244, 1244,  836, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   460 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
 /*   470 */  1244, 1244, 1244, 1244, 1244, 1244, 1041, 1058, 1057, 1244,
 /*   480 */  1244, 1244, 1244, 1244, 1164, 1244, 1158, 1151, 1128, 1130,
 /*   490 */  1244, 1143, 1109, 1108, 1126, 1244, 1244, 1125, 1124, 1244,
 /*   500 */  1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,  986,  985,
 /*   510 */   977,  973,  840,  943,  942, 1244, 1244, 1244, 1244, 1000,
 /*   520 */   854,  850,  848,  844,  839, 1244, 1243, 1242, 1241, 1240,
 /*   530 */  1239, 1238, 1237, 1236, 1235, 1234, 1233, 1232, 1090, 1231,
 /*   540 */  1230, 1229, 1228, 1222, 1221, 1223, 1220, 1219, 1218, 1217,
 /*   550 */  1216, 1215, 1214, 1213, 1212, 1211, 1210, 1209, 1208, 1207,
 /*   560 */  1206, 1205, 1204, 1203, 1202, 1178, 1177, 1184, 1183, 1191,
 /*   570 */  1190, 1187, 1186, 1181, 1189, 1053, 1055, 1075, 1067, 1066,
 /*   580 */  1065, 1064, 1063, 1056, 1054, 1052, 1046, 1045, 1043, 1042,
 /*   590 */  1041, 1051, 1050, 1049, 1036, 1035, 1034, 1030, 1029, 1028,
 /*   600 */  1027, 1026, 1025, 1024, 1023, 1022, 1021, 1020, 1019, 1040,
 /*   610 */  1039, 1038, 1037, 1180, 1179, 1173, 1172, 1174, 1171, 1176,
 /*   620 */  1175, 1170, 1168, 1167, 1169, 1161, 1156, 1159, 1160, 1157,
 /*   630 */  1155, 1146, 1149, 1154, 1152, 1150, 1148, 1147, 1145, 1121,
 /*   640 */  1129, 1131, 1144, 1142, 1141, 1140, 1139, 1138, 1137, 1136,
 /*   650 */  1135, 1134, 1133, 1132, 1127, 1123, 1119, 1118, 1117, 1116,
 /*   660 */  1115, 1114, 1113,  844, 1112, 1111,  934,  936,  935,  931,
 /*   670 */   930,  929,  928, 1122, 1120, 1100, 1103, 1104, 1107, 1106,
 /*   680 */  1105, 1102, 1101, 1099, 1227, 1226, 1225, 1224,  981,  983,
 /*   690 */   992,  991,  990,  984,  982,  980,  907,  906,  905,  904,
 /*   700 */   903,  902,  912,  911,  910,  901,  900,  899,  898, 1198,
 /*   710 */   976,  978,  841,  938,  940, 1004, 1007, 1006, 1005, 1003,
 /*   720 */   952,  951,  950,  949,  948,  947,  941,  939,  937,  880,
 /*   730 */  1089, 1088, 1087, 1086, 1085, 1084,  974, 1017, 1018, 1016,
 /*   740 */  1001,  924,  926,  925,  921,  920,  919,  918,  917,  916,
 /*   750 */   915,  914,  913,  853,  887,  886,  885,  892,  891,  890,
 /*   760 */   882,  881,  880,  879,  878,  877,  897,  896,  895,  876,
 /*   770 */   875,  874,  873,  870,  872,  871,  867,  866,  865,  864,
 /*   780 */   863,  862,  861,  860,  859,  858,  857,  849,  847,  846,
 /*   790 */   845,  843,  842,  838,  834,  833,  837,  836,  835,  832,
 /*   800 */   831,  830,  829,  828,  827,  826,  825,  824,  823,  822,
 /*   810 */   821,  820,  819,  818,  817,
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
  "term_strong_candidate",  "term_no_const_strong",  "num_range",     "term_numeric",
  "formula",       "formula_base",  "comparison",    "atomic_formula",
  "formula_quant",  "formula_card",  "atomic_formula_anon",  "formula_no_const",
  "formula_no_const_base",  "comparison_no_const",  "atomic_formula_one_const",  "quant_lst",   
  "quant_op",      "card_var_lst",  "card_var_lst_inner",  "term_temporal",
  "term_temporal_strong",  "term_temporal_strong_candidate",  "constant_temporal",  "formula_temporal",
  "formula_temporal_base",  "comparison_temporal",  "formula_temporal_quant",  "formula_temporal_card",
  "head_formula",  "atomic_head_formula",  "formula_smpl_card",  "macro_def_lst",
  "macro_bnd",     "macro_args",    "macro_arg",     "sort_lst",    
  "sort",          "sort_id_nr",    "sort_nr",       "sort_id",     
  "constant_bnd_lst",  "constant_bnd",  "constant_dcl_lst",  "constant_dcl_type",
  "attrib_spec",   "object_bnd_lst",  "object_bnd",    "object_lst",  
  "object_spec",   "variable_bnd_lst",  "variable_bnd",  "variable_lst",
  "sort_bnd_lst",  "sort_bnd",      "sort_dcl_lst",  "show_lst",    
  "show_elem",     "query_lst",     "query_maxstep_decl",  "query_label_decl",
  "query_label_Decl",  "clause_if",     "clause_after",  "clause_ifcons",
  "clause_unless",  "clause_where",  "law_basic",     "law_caused",  
  "law_pcaused",   "law_impl",      "law_causes",    "law_increments",
  "law_decrements",  "law_mcause",    "law_always",    "law_constraint",
  "law_impossible",  "law_never",     "law_default",   "law_exogenous",
  "law_inertial",  "law_nonexecutable",  "law_rigid",     "law_observed",
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
 /* 112 */ "num_range ::= term_numeric DBL_PERIOD term_numeric",
 /* 113 */ "term_numeric ::= INTEGER",
 /* 114 */ "term_numeric ::= PAREN_L term_numeric PAREN_R",
 /* 115 */ "term_numeric ::= DASH term_numeric",
 /* 116 */ "term_numeric ::= ABS term_numeric",
 /* 117 */ "term_numeric ::= term_numeric DASH term_numeric",
 /* 118 */ "term_numeric ::= term_numeric PLUS term_numeric",
 /* 119 */ "term_numeric ::= term_numeric STAR term_numeric",
 /* 120 */ "term_numeric ::= term_numeric INT_DIV term_numeric",
 /* 121 */ "term_numeric ::= term_numeric MOD term_numeric",
 /* 122 */ "formula ::= formula_base",
 /* 123 */ "formula ::= PAREN_L formula PAREN_R",
 /* 124 */ "formula ::= NOT formula",
 /* 125 */ "formula ::= DASH formula",
 /* 126 */ "formula ::= formula AMP formula",
 /* 127 */ "formula ::= formula DBL_PLUS formula",
 /* 128 */ "formula ::= formula PIPE formula",
 /* 129 */ "formula ::= formula EQUIV formula",
 /* 130 */ "formula ::= formula IMPL formula",
 /* 131 */ "formula ::= formula ARROW_RDASH formula",
 /* 132 */ "formula_base ::= comparison",
 /* 133 */ "formula_base ::= atomic_formula",
 /* 134 */ "formula_base ::= formula_quant",
 /* 135 */ "formula_base ::= formula_card",
 /* 136 */ "formula_base ::= TRUE",
 /* 137 */ "formula_base ::= FALSE",
 /* 138 */ "comparison ::= term_strong EQ term",
 /* 139 */ "comparison ::= term_strong DBL_EQ term",
 /* 140 */ "comparison ::= term_strong NEQ term",
 /* 141 */ "comparison ::= term_strong LTHAN term",
 /* 142 */ "comparison ::= term_strong GTHAN term",
 /* 143 */ "comparison ::= term_strong LTHAN_EQ term",
 /* 144 */ "comparison ::= term_strong GTHAN_EQ term",
 /* 145 */ "comparison ::= term_strong_candidate EQ term",
 /* 146 */ "comparison ::= term_strong_candidate DBL_EQ term",
 /* 147 */ "comparison ::= term_strong_candidate NEQ term",
 /* 148 */ "comparison ::= term_strong_candidate LTHAN term",
 /* 149 */ "comparison ::= term_strong_candidate GTHAN term",
 /* 150 */ "comparison ::= term_strong_candidate LTHAN_EQ term",
 /* 151 */ "comparison ::= term_strong_candidate GTHAN_EQ term",
 /* 152 */ "comparison ::= constant DBL_EQ term",
 /* 153 */ "comparison ::= constant NEQ term",
 /* 154 */ "comparison ::= constant LTHAN term",
 /* 155 */ "comparison ::= constant GTHAN term",
 /* 156 */ "comparison ::= constant LTHAN_EQ term",
 /* 157 */ "comparison ::= constant GTHAN_EQ term",
 /* 158 */ "atomic_formula ::= constant",
 /* 159 */ "atomic_formula ::= TILDE constant",
 /* 160 */ "atomic_formula ::= constant EQ term",
 /* 161 */ "atomic_formula_anon ::= atomic_formula",
 /* 162 */ "atomic_formula_anon ::= const_anon",
 /* 163 */ "atomic_formula_anon ::= TILDE const_anon",
 /* 164 */ "atomic_formula_anon ::= const_anon EQ term",
 /* 165 */ "formula_no_const ::= formula_no_const_base",
 /* 166 */ "formula_no_const ::= PAREN_L formula_no_const PAREN_R",
 /* 167 */ "formula_no_const ::= NOT formula_no_const",
 /* 168 */ "formula_no_const ::= DASH formula_no_const",
 /* 169 */ "formula_no_const ::= formula_no_const AMP formula_no_const",
 /* 170 */ "formula_no_const ::= formula_no_const DBL_PLUS formula_no_const",
 /* 171 */ "formula_no_const ::= formula_no_const PIPE formula_no_const",
 /* 172 */ "formula_no_const ::= formula_no_const EQUIV formula_no_const",
 /* 173 */ "formula_no_const ::= formula_no_const IMPL formula_no_const",
 /* 174 */ "formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const",
 /* 175 */ "formula_no_const_base ::= comparison_no_const",
 /* 176 */ "formula_no_const_base ::= TRUE",
 /* 177 */ "formula_no_const_base ::= FALSE",
 /* 178 */ "comparison_no_const ::= term_no_const_strong EQ term_no_const",
 /* 179 */ "comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const",
 /* 180 */ "comparison_no_const ::= term_no_const_strong NEQ term_no_const",
 /* 181 */ "comparison_no_const ::= term_no_const_strong LTHAN term_no_const",
 /* 182 */ "comparison_no_const ::= term_no_const_strong GTHAN term_no_const",
 /* 183 */ "comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const",
 /* 184 */ "comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const",
 /* 185 */ "atomic_formula_one_const ::= constant_one_const",
 /* 186 */ "atomic_formula_one_const ::= TILDE constant_one_const",
 /* 187 */ "atomic_formula_one_const ::= constant_one_const EQ term_no_const",
 /* 188 */ "formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R",
 /* 189 */ "quant_lst ::= quant_op variable",
 /* 190 */ "quant_lst ::= quant_lst quant_op variable",
 /* 191 */ "quant_op ::= BIG_CONJ",
 /* 192 */ "quant_op ::= BIG_DISJ",
 /* 193 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 194 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 195 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 196 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 197 */ "formula_card ::= CBRACKET_L formula CBRACKET_R",
 /* 198 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R",
 /* 199 */ "formula_card ::= CBRACKET_L formula CBRACKET_R term",
 /* 200 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term",
 /* 201 */ "card_var_lst ::= card_var_lst_inner PIPE",
 /* 202 */ "card_var_lst_inner ::= variable",
 /* 203 */ "card_var_lst_inner ::= card_var_lst_inner COMMA variable",
 /* 204 */ "term_temporal ::= base_elem_no_const",
 /* 205 */ "term_temporal ::= INTEGER",
 /* 206 */ "term_temporal ::= STRING_LITERAL",
 /* 207 */ "term_temporal ::= PAREN_L term_temporal PAREN_R",
 /* 208 */ "term_temporal ::= TRUE",
 /* 209 */ "term_temporal ::= FALSE",
 /* 210 */ "term_temporal ::= MAXSTEP",
 /* 211 */ "term_temporal ::= MAXADDITIVE",
 /* 212 */ "term_temporal ::= MAXAFVALUE",
 /* 213 */ "term_temporal ::= constant",
 /* 214 */ "term_temporal ::= DASH term_temporal",
 /* 215 */ "term_temporal ::= ABS term_temporal",
 /* 216 */ "term_temporal ::= term_temporal COLON term",
 /* 217 */ "term_temporal ::= term_temporal DASH term_temporal",
 /* 218 */ "term_temporal ::= term_temporal PLUS term_temporal",
 /* 219 */ "term_temporal ::= term_temporal STAR term_temporal",
 /* 220 */ "term_temporal ::= term_temporal INT_DIV term_temporal",
 /* 221 */ "term_temporal ::= term_temporal MOD term_temporal",
 /* 222 */ "term_temporal_strong ::= base_elem_no_const",
 /* 223 */ "term_temporal_strong ::= INTEGER",
 /* 224 */ "term_temporal_strong ::= STRING_LITERAL",
 /* 225 */ "term_temporal_strong ::= PAREN_L term_temporal_strong PAREN_R",
 /* 226 */ "term_temporal_strong ::= MAXSTEP",
 /* 227 */ "term_temporal_strong ::= MAXADDITIVE",
 /* 228 */ "term_temporal_strong ::= MAXAFVALUE",
 /* 229 */ "term_temporal_strong ::= term_temporal_strong COLON term_strong",
 /* 230 */ "term_temporal_strong ::= DASH term_temporal_strong",
 /* 231 */ "term_temporal_strong ::= ABS term_temporal",
 /* 232 */ "term_temporal_strong ::= term_temporal_strong DASH term_temporal",
 /* 233 */ "term_temporal_strong ::= term_temporal_strong PLUS term_temporal",
 /* 234 */ "term_temporal_strong ::= term_temporal_strong STAR term_temporal",
 /* 235 */ "term_temporal_strong ::= term_temporal_strong INT_DIV term_temporal",
 /* 236 */ "term_temporal_strong ::= term_temporal_strong MOD term_temporal",
 /* 237 */ "formula_temporal ::= formula_temporal_base",
 /* 238 */ "formula_temporal ::= PAREN_L formula_temporal PAREN_R",
 /* 239 */ "formula_temporal ::= NOT formula_temporal",
 /* 240 */ "formula_temporal ::= DASH formula_temporal",
 /* 241 */ "formula_temporal ::= formula_temporal AMP formula_temporal",
 /* 242 */ "formula_temporal ::= formula_temporal DBL_PLUS formula_temporal",
 /* 243 */ "formula_temporal ::= formula_temporal PIPE formula_temporal",
 /* 244 */ "formula_temporal ::= formula_temporal EQUIV formula_temporal",
 /* 245 */ "formula_temporal ::= formula_temporal IMPL formula_temporal",
 /* 246 */ "formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal",
 /* 247 */ "formula_temporal ::= term_temporal_strong COLON formula",
 /* 248 */ "formula_temporal_base ::= comparison_temporal",
 /* 249 */ "formula_temporal_base ::= formula_temporal_quant",
 /* 250 */ "formula_temporal_base ::= formula_temporal_card",
 /* 251 */ "formula_temporal_base ::= TRUE",
 /* 252 */ "formula_temporal_base ::= FALSE",
 /* 253 */ "comparison_temporal ::= term_temporal_strong EQ term_temporal",
 /* 254 */ "comparison_temporal ::= term_temporal_strong DBL_EQ term_temporal",
 /* 255 */ "comparison_temporal ::= term_temporal_strong NEQ term_temporal",
 /* 256 */ "comparison_temporal ::= term_temporal_strong LTHAN term_temporal",
 /* 257 */ "comparison_temporal ::= term_temporal_strong GTHAN term_temporal",
 /* 258 */ "comparison_temporal ::= term_temporal_strong LTHAN_EQ term_temporal",
 /* 259 */ "comparison_temporal ::= term_temporal_strong GTHAN_EQ term_temporal",
 /* 260 */ "formula_temporal_quant ::= BRACKET_L quant_lst PIPE formula_temporal BRACKET_R",
 /* 261 */ "formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R",
 /* 262 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R",
 /* 263 */ "formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal",
 /* 264 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal",
 /* 265 */ "formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R",
 /* 266 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R",
 /* 267 */ "formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R term_temporal",
 /* 268 */ "formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R term_temporal",
 /* 269 */ "head_formula ::= comparison",
 /* 270 */ "head_formula ::= atomic_head_formula",
 /* 271 */ "head_formula ::= formula_smpl_card",
 /* 272 */ "head_formula ::= TRUE",
 /* 273 */ "head_formula ::= FALSE",
 /* 274 */ "atomic_head_formula ::= atomic_formula",
 /* 275 */ "atomic_head_formula ::= DASH constant",
 /* 276 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 277 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 278 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 279 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 280 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 281 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 282 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 283 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 284 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 285 */ "macro_def_lst ::= macro_bnd",
 /* 286 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 287 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 288 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 289 */ "macro_args ::= macro_arg",
 /* 290 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 291 */ "macro_arg ::= POUND_INTEGER",
 /* 292 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 293 */ "sort_lst ::= sort",
 /* 294 */ "sort_lst ::= sort_lst COMMA sort",
 /* 295 */ "sort ::= sort_id_nr",
 /* 296 */ "sort ::= sort_id_nr STAR",
 /* 297 */ "sort ::= sort_id_nr CARROT",
 /* 298 */ "sort ::= sort PLUS object_nullary",
 /* 299 */ "sort ::= sort PLUS IDENTIFIER",
 /* 300 */ "sort ::= sort PLUS INTEGER",
 /* 301 */ "sort_id_nr ::= sort_id",
 /* 302 */ "sort_id_nr ::= sort_nr",
 /* 303 */ "sort_nr ::= num_range",
 /* 304 */ "sort_id ::= IDENTIFIER",
 /* 305 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 306 */ "constant_bnd_lst ::= constant_bnd",
 /* 307 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 308 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 309 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 310 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 311 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 312 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 313 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 314 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 315 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 316 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 317 */ "constant_dcl_type ::= ABACTION",
 /* 318 */ "constant_dcl_type ::= ACTION",
 /* 319 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 320 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 321 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 322 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 323 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 324 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 325 */ "constant_dcl_type ::= RIGID",
 /* 326 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 327 */ "constant_dcl_type ::= SDFLUENT",
 /* 328 */ "attrib_spec ::= ATTRIBUTE",
 /* 329 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 330 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 331 */ "object_bnd_lst ::= object_bnd",
 /* 332 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 333 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 334 */ "object_lst ::= object_spec",
 /* 335 */ "object_lst ::= object_lst COMMA object_spec",
 /* 336 */ "object_spec ::= IDENTIFIER",
 /* 337 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 338 */ "object_spec ::= INTEGER",
 /* 339 */ "object_spec ::= num_range",
 /* 340 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 341 */ "variable_bnd_lst ::= variable_bnd",
 /* 342 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 343 */ "variable_bnd ::= variable_lst DBL_COLON sort",
 /* 344 */ "variable_lst ::= IDENTIFIER",
 /* 345 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 346 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 347 */ "sort_bnd_lst ::= sort_bnd",
 /* 348 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 349 */ "sort_bnd ::= sort_dcl_lst",
 /* 350 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 351 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 352 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 353 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 354 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 355 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 356 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 357 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 358 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 359 */ "show_lst ::= show_elem",
 /* 360 */ "show_lst ::= show_lst COMMA show_elem",
 /* 361 */ "show_elem ::= atomic_formula_one_const",
 /* 362 */ "stmt_noconcurrency ::= NOCONCURRENCY PERIOD",
 /* 363 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD",
 /* 364 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD",
 /* 365 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD",
 /* 366 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 367 */ "query_lst ::= formula_temporal",
 /* 368 */ "query_lst ::= query_maxstep_decl",
 /* 369 */ "query_lst ::= query_label_decl",
 /* 370 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 371 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 372 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 373 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 374 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range",
 /* 375 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 376 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 377 */ "clause_if ::= IF formula",
 /* 378 */ "clause_if ::=",
 /* 379 */ "clause_after ::= AFTER formula",
 /* 380 */ "clause_after ::=",
 /* 381 */ "clause_ifcons ::= IFCONS formula",
 /* 382 */ "clause_ifcons ::=",
 /* 383 */ "clause_unless ::= UNLESS atomic_formula_anon",
 /* 384 */ "clause_unless ::=",
 /* 385 */ "clause_where ::= WHERE formula_no_const",
 /* 386 */ "clause_where ::=",
 /* 387 */ "stmt_law ::= law_basic",
 /* 388 */ "stmt_law ::= law_caused",
 /* 389 */ "stmt_law ::= law_pcaused",
 /* 390 */ "stmt_law ::= law_impl",
 /* 391 */ "stmt_law ::= law_causes",
 /* 392 */ "stmt_law ::= law_increments",
 /* 393 */ "stmt_law ::= law_decrements",
 /* 394 */ "stmt_law ::= law_mcause",
 /* 395 */ "stmt_law ::= law_always",
 /* 396 */ "stmt_law ::= law_constraint",
 /* 397 */ "stmt_law ::= law_impossible",
 /* 398 */ "stmt_law ::= law_never",
 /* 399 */ "stmt_law ::= law_default",
 /* 400 */ "stmt_law ::= law_exogenous",
 /* 401 */ "stmt_law ::= law_inertial",
 /* 402 */ "stmt_law ::= law_nonexecutable",
 /* 403 */ "stmt_law ::= law_rigid",
 /* 404 */ "stmt_law ::= law_observed",
 /* 405 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 406 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 407 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 408 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 409 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 410 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 411 */ "law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 412 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 413 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 414 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 415 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 416 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 417 */ "law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 418 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 419 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 420 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 421 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 422 */ "law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD",
 /* 423 */ "stmt_code_blk ::= ASP_GR",
 /* 424 */ "stmt_code_blk ::= ASP_CP",
 /* 425 */ "stmt_code_blk ::= F2LP_GR",
 /* 426 */ "stmt_code_blk ::= F2LP_CP",
 /* 427 */ "stmt_code_blk ::= LUA_GR",
 /* 428 */ "stmt_code_blk ::= LUA_CP",
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
#line 197 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));								
#line 2508 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 136: /* start */
    case 137: /* statement_lst */
    case 160: /* undeclared */
{
#line 207 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2517 "bcplus/parser/detail/lemon_parser.c"
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
#line 211 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy328));								
#line 2530 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 139: /* stmt_macro_def */
{
#line 232 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy255));								
#line 2537 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_constant_def */
{
#line 234 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy271));								
#line 2544 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_object_def */
{
#line 236 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy288));								
#line 2551 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_variable_def */
{
#line 238 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy459));								
#line 2558 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_sort_def */
{
#line 240 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy237));								
#line 2565 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 148: /* stmt_noconcurrency */
{
#line 250 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy129));								
#line 2572 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_strong_noconcurrency */
{
#line 252 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy298));								
#line 2579 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 152: /* stmt_query */
{
#line 258 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy98));								
#line 2586 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* base_elem */
    case 154: /* base_elem_no_const */
    case 162: /* term */
    case 165: /* term_no_const */
    case 167: /* term_strong */
    case 168: /* term_strong_candidate */
    case 169: /* term_no_const_strong */
    case 187: /* term_temporal */
    case 188: /* term_temporal_strong */
    case 189: /* term_temporal_strong_candidate */
    case 190: /* constant_temporal */
{
#line 292 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy283));								
#line 2603 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 155: /* constant */
    case 163: /* constant_one_const */
    case 166: /* const_anon */
{
#line 296 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy313));								
#line 2612 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* object */
    case 157: /* object_nullary */
{
#line 298 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy54));								
#line 2620 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 158: /* variable */
{
#line 302 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy37));								
#line 2627 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* lua */
{
#line 304 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy145));								
#line 2634 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 161: /* term_lst */
    case 164: /* term_no_const_lst */
{
#line 308 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy323));								
#line 2642 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 170: /* num_range */
{
#line 707 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy93));								
#line 2649 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* term_numeric */
{
#line 709 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy416));								
#line 2656 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 172: /* formula */
    case 173: /* formula_base */
    case 174: /* comparison */
    case 177: /* formula_card */
    case 179: /* formula_no_const */
    case 180: /* formula_no_const_base */
    case 181: /* comparison_no_const */
    case 191: /* formula_temporal */
    case 192: /* formula_temporal_base */
    case 193: /* comparison_temporal */
    case 195: /* formula_temporal_card */
    case 196: /* head_formula */
{
#line 770 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy489));								
#line 2674 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 175: /* atomic_formula */
    case 178: /* atomic_formula_anon */
    case 182: /* atomic_formula_one_const */
    case 197: /* atomic_head_formula */
{
#line 776 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy346));								
#line 2684 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 176: /* formula_quant */
    case 194: /* formula_temporal_quant */
{
#line 778 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy269));								
#line 2692 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 183: /* quant_lst */
{
#line 952 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy77));								
#line 2699 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 184: /* quant_op */
{
#line 954 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2706 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* card_var_lst */
    case 186: /* card_var_lst_inner */
{
#line 991 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy55));								
#line 2714 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 198: /* formula_smpl_card */
{
#line 1288 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy273));								
#line 2721 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 199: /* macro_def_lst */
{
#line 1335 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy361));                              
#line 2728 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* macro_bnd */
{
#line 1337 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy235));                              
#line 2735 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* macro_args */
{
#line 1339 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy458));                              
#line 2742 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* macro_arg */
{
#line 1341 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy275));                              
#line 2749 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* sort_lst */
{
#line 1431 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy291));							
#line 2756 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* sort */
    case 205: /* sort_id_nr */
    case 206: /* sort_nr */
    case 207: /* sort_id */
{
#line 1433 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2766 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 208: /* constant_bnd_lst */
    case 209: /* constant_bnd */
{
#line 1561 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy97));									
#line 2774 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* constant_dcl_lst */
{
#line 1565 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy498));									
#line 2781 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 211: /* constant_dcl_type */
{
#line 1567 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2788 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* attrib_spec */
{
#line 1569 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2795 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* object_bnd_lst */
{
#line 1925 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy190));									
#line 2802 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 214: /* object_bnd */
{
#line 1927 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy302));									
#line 2809 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* object_lst */
    case 216: /* object_spec */
{
#line 1929 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy189));									
#line 2817 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* variable_bnd_lst */
    case 218: /* variable_bnd */
{
#line 2054 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy125));									
#line 2825 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* variable_lst */
{
#line 2058 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy16));									
#line 2832 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 220: /* sort_bnd_lst */
    case 221: /* sort_bnd */
    case 222: /* sort_dcl_lst */
{
#line 2141 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy7));									
#line 2841 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 223: /* show_lst */
{
#line 2245 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy11));									
#line 2848 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 224: /* show_elem */
    case 232: /* clause_unless */
{
#line 2247 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy346));									
#line 2856 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 225: /* query_lst */
{
#line 2392 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy445).l); DEALLOC((yypminor->yy445).maxstep); DEALLOC((yypminor->yy445).label);	
#line 2863 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 226: /* query_maxstep_decl */
{
#line 2394 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy192));												
#line 2870 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 228: /* query_label_Decl */
{
#line 2396 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2877 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 229: /* clause_if */
    case 230: /* clause_after */
    case 231: /* clause_ifcons */
    case 233: /* clause_where */
{
#line 2550 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy489));									
#line 2887 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 234: /* law_basic */
    case 235: /* law_caused */
    case 236: /* law_pcaused */
    case 237: /* law_impl */
    case 238: /* law_causes */
    case 239: /* law_increments */
    case 240: /* law_decrements */
    case 241: /* law_mcause */
    case 242: /* law_always */
    case 243: /* law_constraint */
    case 244: /* law_impossible */
    case 245: /* law_never */
    case 246: /* law_default */
    case 247: /* law_exogenous */
    case 248: /* law_inertial */
    case 249: /* law_nonexecutable */
    case 250: /* law_rigid */
    case 251: /* law_observed */
{
#line 2591 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy328));									
#line 2911 "bcplus/parser/detail/lemon_parser.c"
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
  { 178, 1 },
  { 178, 2 },
  { 178, 3 },
  { 179, 1 },
  { 179, 3 },
  { 179, 2 },
  { 179, 2 },
  { 179, 3 },
  { 179, 3 },
  { 179, 3 },
  { 179, 3 },
  { 179, 3 },
  { 179, 3 },
  { 180, 1 },
  { 180, 1 },
  { 180, 1 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 181, 3 },
  { 182, 1 },
  { 182, 2 },
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
  { 187, 3 },
  { 187, 1 },
  { 187, 1 },
  { 187, 1 },
  { 187, 1 },
  { 187, 1 },
  { 187, 1 },
  { 187, 2 },
  { 187, 2 },
  { 187, 3 },
  { 187, 3 },
  { 187, 3 },
  { 187, 3 },
  { 187, 3 },
  { 187, 3 },
  { 188, 1 },
  { 188, 1 },
  { 188, 1 },
  { 188, 3 },
  { 188, 1 },
  { 188, 1 },
  { 188, 1 },
  { 188, 3 },
  { 188, 2 },
  { 188, 2 },
  { 188, 3 },
  { 188, 3 },
  { 188, 3 },
  { 188, 3 },
  { 188, 3 },
  { 191, 1 },
  { 191, 3 },
  { 191, 2 },
  { 191, 2 },
  { 191, 3 },
  { 191, 3 },
  { 191, 3 },
  { 191, 3 },
  { 191, 3 },
  { 191, 3 },
  { 191, 3 },
  { 192, 1 },
  { 192, 1 },
  { 192, 1 },
  { 192, 1 },
  { 192, 1 },
  { 193, 3 },
  { 193, 3 },
  { 193, 3 },
  { 193, 3 },
  { 193, 3 },
  { 193, 3 },
  { 193, 3 },
  { 194, 5 },
  { 195, 4 },
  { 195, 5 },
  { 195, 5 },
  { 195, 6 },
  { 195, 3 },
  { 195, 4 },
  { 195, 4 },
  { 195, 5 },
  { 196, 1 },
  { 196, 1 },
  { 196, 1 },
  { 196, 1 },
  { 196, 1 },
  { 197, 1 },
  { 197, 2 },
  { 198, 4 },
  { 198, 5 },
  { 198, 5 },
  { 198, 6 },
  { 198, 3 },
  { 198, 4 },
  { 198, 4 },
  { 198, 5 },
  { 139, 4 },
  { 199, 1 },
  { 199, 3 },
  { 200, 6 },
  { 200, 3 },
  { 201, 1 },
  { 201, 3 },
  { 202, 1 },
  { 202, 1 },
  { 203, 1 },
  { 203, 3 },
  { 204, 1 },
  { 204, 2 },
  { 204, 2 },
  { 204, 3 },
  { 204, 3 },
  { 204, 3 },
  { 205, 1 },
  { 205, 1 },
  { 206, 1 },
  { 207, 1 },
  { 140, 4 },
  { 208, 1 },
  { 208, 3 },
  { 209, 6 },
  { 209, 3 },
  { 209, 3 },
  { 209, 5 },
  { 209, 8 },
  { 210, 1 },
  { 210, 4 },
  { 210, 3 },
  { 210, 6 },
  { 211, 1 },
  { 211, 1 },
  { 211, 1 },
  { 211, 1 },
  { 211, 1 },
  { 211, 1 },
  { 211, 1 },
  { 211, 1 },
  { 211, 1 },
  { 211, 1 },
  { 211, 1 },
  { 212, 1 },
  { 212, 4 },
  { 141, 4 },
  { 213, 1 },
  { 213, 3 },
  { 214, 3 },
  { 215, 1 },
  { 215, 3 },
  { 216, 1 },
  { 216, 4 },
  { 216, 1 },
  { 216, 1 },
  { 142, 4 },
  { 217, 1 },
  { 217, 3 },
  { 218, 3 },
  { 219, 1 },
  { 219, 3 },
  { 143, 4 },
  { 220, 1 },
  { 220, 3 },
  { 221, 1 },
  { 221, 3 },
  { 221, 3 },
  { 221, 3 },
  { 222, 1 },
  { 222, 3 },
  { 146, 4 },
  { 146, 4 },
  { 147, 4 },
  { 147, 4 },
  { 223, 1 },
  { 223, 3 },
  { 224, 1 },
  { 148, 2 },
  { 149, 2 },
  { 150, 5 },
  { 151, 5 },
  { 152, 4 },
  { 225, 1 },
  { 225, 1 },
  { 225, 1 },
  { 225, 3 },
  { 225, 3 },
  { 225, 3 },
  { 226, 3 },
  { 226, 3 },
  { 227, 3 },
  { 227, 3 },
  { 229, 2 },
  { 229, 0 },
  { 230, 2 },
  { 230, 0 },
  { 231, 2 },
  { 231, 0 },
  { 232, 2 },
  { 232, 0 },
  { 233, 2 },
  { 233, 0 },
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
  { 234, 7 },
  { 235, 8 },
  { 236, 8 },
  { 237, 5 },
  { 238, 7 },
  { 239, 9 },
  { 240, 9 },
  { 241, 7 },
  { 242, 6 },
  { 243, 6 },
  { 244, 6 },
  { 245, 6 },
  { 246, 8 },
  { 247, 8 },
  { 248, 8 },
  { 249, 6 },
  { 250, 4 },
  { 251, 5 },
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
#line 213 "bcplus/parser/detail/lemon_parser.y"
{
  yy_destructor(yypParser,118,&yymsp[0].minor);
}
#line 3645 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 218 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy328;
			yymsp[0].minor.yy328  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3654 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 261 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy328 = yymsp[0].minor.yy255; }
#line 3659 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 262 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy328 = yymsp[0].minor.yy271; }
#line 3664 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy328 = yymsp[0].minor.yy288; }
#line 3669 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy328 = yymsp[0].minor.yy459; }
#line 3674 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy328 = yymsp[0].minor.yy237; }
#line 3679 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy328 = yymsp[0].minor.yy328; }
#line 3689 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 270 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy328 = yymsp[0].minor.yy129; }
#line 3694 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 271 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy328 = yymsp[0].minor.yy298; }
#line 3699 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 274 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy328 = yymsp[0].minor.yy98; }
#line 3704 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 320 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy283 = yymsp[0].minor.yy313; }
#line 3709 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 42: /* term ::= base_elem */ yytestcase(yyruleno==42);
      case 58: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==58);
      case 83: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==83);
      case 98: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==98);
      case 204: /* term_temporal ::= base_elem_no_const */ yytestcase(yyruleno==204);
      case 222: /* term_temporal_strong ::= base_elem_no_const */ yytestcase(yyruleno==222);
#line 321 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy283 = yymsp[0].minor.yy283; }
#line 3720 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 323 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy283 = yymsp[0].minor.yy54;	}
#line 3725 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 324 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy283 = yymsp[0].minor.yy37; }
#line 3730 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 325 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy283 = yymsp[0].minor.yy145; }
#line 3735 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 38: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==38);
#line 443 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy313, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy323, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3741 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 39: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==39);
#line 444 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy313, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3747 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* const_anon ::= IDENTIFIER */
#line 446 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy313, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true); }
#line 3752 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 447 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy313, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy323, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true);	}
#line 3757 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 450 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy54, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy323, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3762 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= object_nullary */
#line 451 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy54 = yymsp[0].minor.yy54; }
#line 3767 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* object_nullary ::= OBJECT_ID */
#line 452 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy54, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3772 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* object ::= undeclared */
#line 453 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3777 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* variable ::= VARIABLE_ID */
#line 456 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy37 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::VARIABLE)) {
			parser->_feature_error(Language::Feature::VARIABLE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			BASE_ELEM_BARE_DEF(yygotominor.yy37, yymsp[0].minor.yy0, Symbol::Type::VARIABLE, Variable, VariableSymbol); 
		}
	}
#line 3792 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 467 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy145, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy323, yymsp[0].minor.yy0); }
#line 3797 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* lua ::= AT_IDENTIFIER */
#line 468 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy145, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3802 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 469 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy409, yymsp[-3].minor.yy0, yymsp[-1].minor.yy323);   yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 3809 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* undeclared ::= IDENTIFIER */
#line 470 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy409, yymsp[0].minor.yy0, NULL); }
#line 3814 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 36: /* term_lst ::= term */
      case 40: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==40);
#line 473 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy323 = new TermList();
			yygotominor.yy323->push_back(yymsp[0].minor.yy283);
		}
#line 3823 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 37: /* term_lst ::= term_lst COMMA term */
      case 41: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==41);
#line 479 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy323 = yymsp[-2].minor.yy323;
			yymsp[-2].minor.yy323->push_back(yymsp[0].minor.yy283);
		  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3833 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 43: /* term ::= INTEGER */
      case 59: /* term_strong ::= INTEGER */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==99);
      case 205: /* term_temporal ::= INTEGER */ yytestcase(yyruleno==205);
      case 223: /* term_temporal_strong ::= INTEGER */ yytestcase(yyruleno==223);
#line 578 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy283, yymsp[0].minor.yy0);	}
#line 3843 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 44: /* term ::= STRING_LITERAL */
      case 46: /* term ::= TRUE */ yytestcase(yyruleno==46);
      case 47: /* term ::= FALSE */ yytestcase(yyruleno==47);
      case 60: /* term_strong ::= STRING_LITERAL */ yytestcase(yyruleno==60);
      case 85: /* term_no_const_strong ::= STRING_LITERAL */ yytestcase(yyruleno==85);
      case 100: /* term_no_const ::= STRING_LITERAL */ yytestcase(yyruleno==100);
      case 102: /* term_no_const ::= TRUE */ yytestcase(yyruleno==102);
      case 103: /* term_no_const ::= FALSE */ yytestcase(yyruleno==103);
      case 206: /* term_temporal ::= STRING_LITERAL */ yytestcase(yyruleno==206);
      case 208: /* term_temporal ::= TRUE */ yytestcase(yyruleno==208);
      case 209: /* term_temporal ::= FALSE */ yytestcase(yyruleno==209);
      case 224: /* term_temporal_strong ::= STRING_LITERAL */ yytestcase(yyruleno==224);
#line 579 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy283, yymsp[0].minor.yy0); }
#line 3859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 45: /* term ::= PAREN_L term PAREN_R */
      case 61: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==61);
      case 86: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==86);
      case 101: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==101);
      case 207: /* term_temporal ::= PAREN_L term_temporal PAREN_R */ yytestcase(yyruleno==207);
      case 225: /* term_temporal_strong ::= PAREN_L term_temporal_strong PAREN_R */ yytestcase(yyruleno==225);
#line 580 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy283, yymsp[-2].minor.yy0, yymsp[-1].minor.yy283, yymsp[0].minor.yy0); }
#line 3869 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 48: /* term ::= MAXSTEP */
      case 62: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==87);
      case 210: /* term_temporal ::= MAXSTEP */ yytestcase(yyruleno==210);
      case 226: /* term_temporal_strong ::= MAXSTEP */ yytestcase(yyruleno==226);
#line 583 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy283, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3878 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= MAXADDITIVE */
      case 63: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==63);
      case 88: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==88);
      case 211: /* term_temporal ::= MAXADDITIVE */ yytestcase(yyruleno==211);
      case 227: /* term_temporal_strong ::= MAXADDITIVE */ yytestcase(yyruleno==227);
#line 584 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy283, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3887 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= MAXAFVALUE */
      case 64: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==64);
      case 89: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==89);
      case 212: /* term_temporal ::= MAXAFVALUE */ yytestcase(yyruleno==212);
      case 228: /* term_temporal_strong ::= MAXAFVALUE */ yytestcase(yyruleno==228);
#line 585 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy283, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3896 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= DASH term */
      case 65: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==65);
      case 91: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==91);
      case 105: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==105);
      case 214: /* term_temporal ::= DASH term_temporal */ yytestcase(yyruleno==214);
      case 230: /* term_temporal_strong ::= DASH term_temporal_strong */ yytestcase(yyruleno==230);
#line 589 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy283, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, UnaryTerm::Operator::NEGATIVE); }
#line 3906 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= ABS term */
      case 66: /* term_strong ::= ABS term */ yytestcase(yyruleno==66);
      case 92: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==92);
      case 106: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==106);
      case 215: /* term_temporal ::= ABS term_temporal */ yytestcase(yyruleno==215);
      case 231: /* term_temporal_strong ::= ABS term_temporal */ yytestcase(yyruleno==231);
#line 590 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy283, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, UnaryTerm::Operator::ABS); }
#line 3916 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= term DASH term */
      case 68: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==68);
      case 78: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==78);
      case 93: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==93);
      case 107: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==107);
      case 217: /* term_temporal ::= term_temporal DASH term_temporal */ yytestcase(yyruleno==217);
      case 232: /* term_temporal_strong ::= term_temporal_strong DASH term_temporal */ yytestcase(yyruleno==232);
#line 594 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy283, yymsp[-2].minor.yy283, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BinaryTerm::Operator::MINUS); }
#line 3927 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= term PLUS term */
      case 69: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==69);
      case 79: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==79);
      case 94: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==94);
      case 108: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==108);
      case 218: /* term_temporal ::= term_temporal PLUS term_temporal */ yytestcase(yyruleno==218);
      case 233: /* term_temporal_strong ::= term_temporal_strong PLUS term_temporal */ yytestcase(yyruleno==233);
#line 595 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy283, yymsp[-2].minor.yy283, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BinaryTerm::Operator::PLUS); }
#line 3938 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= term STAR term */
      case 70: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==70);
      case 80: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==80);
      case 95: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==95);
      case 109: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==109);
      case 219: /* term_temporal ::= term_temporal STAR term_temporal */ yytestcase(yyruleno==219);
      case 234: /* term_temporal_strong ::= term_temporal_strong STAR term_temporal */ yytestcase(yyruleno==234);
#line 596 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy283, yymsp[-2].minor.yy283, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BinaryTerm::Operator::TIMES); }
#line 3949 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 56: /* term ::= term INT_DIV term */
      case 71: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==71);
      case 81: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==81);
      case 96: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==96);
      case 110: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==110);
      case 220: /* term_temporal ::= term_temporal INT_DIV term_temporal */ yytestcase(yyruleno==220);
      case 235: /* term_temporal_strong ::= term_temporal_strong INT_DIV term_temporal */ yytestcase(yyruleno==235);
#line 597 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy283, yymsp[-2].minor.yy283, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BinaryTerm::Operator::DIVIDE); }
#line 3960 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 57: /* term ::= term MOD term */
      case 72: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==72);
      case 82: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==82);
      case 97: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==97);
      case 111: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==111);
      case 221: /* term_temporal ::= term_temporal MOD term_temporal */ yytestcase(yyruleno==221);
      case 236: /* term_temporal_strong ::= term_temporal_strong MOD term_temporal */ yytestcase(yyruleno==236);
#line 598 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy283, yymsp[-2].minor.yy283, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BinaryTerm::Operator::MOD); }
#line 3971 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 67: /* term_strong_candidate ::= DASH constant */
#line 617 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy283, yymsp[-1].minor.yy0, yymsp[0].minor.yy313, UnaryTerm::Operator::NEGATIVE); }
#line 3976 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 73: /* term_strong ::= constant DASH term */
#line 626 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy283, yymsp[-2].minor.yy313, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BinaryTerm::Operator::MINUS); }
#line 3981 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant PLUS term */
#line 627 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy283, yymsp[-2].minor.yy313, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BinaryTerm::Operator::PLUS); }
#line 3986 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant STAR term */
#line 628 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy283, yymsp[-2].minor.yy313, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BinaryTerm::Operator::TIMES); }
#line 3991 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 76: /* term_strong ::= constant INT_DIV term */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy283, yymsp[-2].minor.yy313, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BinaryTerm::Operator::DIVIDE); }
#line 3996 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong ::= constant MOD term */
#line 630 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy283, yymsp[-2].minor.yy313, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BinaryTerm::Operator::MOD); }
#line 4001 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 90: /* term_no_const_strong ::= constant */
#line 652 "bcplus/parser/detail/lemon_parser.y"
{
		// error handling for constants so they don'yygotominor.yy283 default to undeclared identifiers
		yygotominor.yy283 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy313;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy313->beginLoc());
		YYERROR;
	}
#line 4012 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 104: /* term_no_const ::= constant */
      case 213: /* term_temporal ::= constant */ yytestcase(yyruleno==213);
#line 682 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy283 default to undeclared identifiers
		yygotominor.yy283 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy313;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy313->beginLoc());
		YYERROR;
	}
#line 4024 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 112: /* num_range ::= term_numeric DBL_PERIOD term_numeric */
#line 712 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy416, r_ptr = yymsp[0].minor.yy416, s_ptr = yymsp[-1].minor.yy0;

	yygotominor.yy93 = new NumberRange(yymsp[-2].minor.yy416->val(), yymsp[0].minor.yy416->val(), yymsp[-2].minor.yy416->beginLoc(), yymsp[0].minor.yy416->endLoc());

}
#line 4034 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 113: /* term_numeric ::= INTEGER */
#line 720 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> i_ptr = yymsp[0].minor.yy0;

	yygotominor.yy416 = 0;
	try {
		yygotominor.yy416 = new Number(boost::lexical_cast<int>(*yymsp[0].minor.yy0->str()), yymsp[0].minor.yy0->beginLoc());

	} catch (boost::bad_lexical_cast const& e) {
		parser->_parse_error("INTERNAL ERROR: Failed to parse integer \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
		YYERROR;
	}
}
#line 4050 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 114: /* term_numeric ::= PAREN_L term_numeric PAREN_R */
#line 733 "bcplus/parser/detail/lemon_parser.y"
{ 
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy416 = yymsp[-1].minor.yy416;  
	yygotominor.yy416->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy416->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 4060 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 115: /* term_numeric ::= DASH term_numeric */
#line 753 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy416, yymsp[0].minor.yy416, -1 * yymsp[0].minor.yy416->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4066 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 116: /* term_numeric ::= ABS term_numeric */
#line 754 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy416, yymsp[0].minor.yy416, yymsp[0].minor.yy416->val() < 0 ? - yymsp[0].minor.yy416->val() : yymsp[0].minor.yy416->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 4072 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 117: /* term_numeric ::= term_numeric DASH term_numeric */
#line 756 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() - yymsp[0].minor.yy416->val());   yy_destructor(yypParser,105,&yymsp[-1].minor);
}
#line 4078 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 118: /* term_numeric ::= term_numeric PLUS term_numeric */
#line 757 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() + yymsp[0].minor.yy416->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 4084 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 119: /* term_numeric ::= term_numeric STAR term_numeric */
#line 758 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() * yymsp[0].minor.yy416->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 4090 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 120: /* term_numeric ::= term_numeric INT_DIV term_numeric */
#line 759 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() / yymsp[0].minor.yy416->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 4096 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 121: /* term_numeric ::= term_numeric MOD term_numeric */
#line 760 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() % yymsp[0].minor.yy416->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 4102 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 122: /* formula ::= formula_base */
      case 165: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==165);
      case 237: /* formula_temporal ::= formula_temporal_base */ yytestcase(yyruleno==237);
#line 818 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = yymsp[0].minor.yy489;				}
#line 4109 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 123: /* formula ::= PAREN_L formula PAREN_R */
      case 166: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==166);
      case 238: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==238);
#line 819 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = yymsp[-1].minor.yy489; yygotominor.yy489->parens(true); 	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4118 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 124: /* formula ::= NOT formula */
      case 167: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==167);
      case 239: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==239);
#line 820 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy489, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 4125 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 125: /* formula ::= DASH formula */
      case 168: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==168);
      case 240: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==240);
#line 821 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy489, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 4132 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 126: /* formula ::= formula AMP formula */
      case 169: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==169);
      case 241: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==241);
#line 822 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy489, yymsp[0].minor.yy489, yymsp[-2].minor.yy489->beginLoc(), yymsp[0].minor.yy489->endLoc());   yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 4140 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 127: /* formula ::= formula DBL_PLUS formula */
      case 128: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==128);
      case 170: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==170);
      case 171: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==171);
      case 242: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==242);
      case 243: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==243);
#line 823 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy489, yymsp[-2].minor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy489, BinaryFormula::Operator::OR); }
#line 4150 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 129: /* formula ::= formula EQUIV formula */
      case 172: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==172);
      case 244: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==244);
#line 825 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy489, yymsp[-2].minor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy489, BinaryFormula::Operator::EQUIV); }
#line 4157 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* formula ::= formula IMPL formula */
      case 131: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==131);
      case 173: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==173);
      case 174: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==174);
      case 245: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==245);
      case 246: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==246);
#line 826 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy489, yymsp[-2].minor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy489, BinaryFormula::Operator::IMPL); }
#line 4167 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* formula_base ::= comparison */
      case 175: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==175);
      case 248: /* formula_temporal_base ::= comparison_temporal */ yytestcase(yyruleno==248);
      case 269: /* head_formula ::= comparison */ yytestcase(yyruleno==269);
#line 829 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = yymsp[0].minor.yy489; }
#line 4175 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* formula_base ::= atomic_formula */
      case 270: /* head_formula ::= atomic_head_formula */ yytestcase(yyruleno==270);
#line 830 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = yymsp[0].minor.yy346; }
#line 4181 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* formula_base ::= formula_quant */
      case 249: /* formula_temporal_base ::= formula_temporal_quant */ yytestcase(yyruleno==249);
#line 831 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = yymsp[0].minor.yy269; }
#line 4187 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* formula_base ::= formula_card */
      case 250: /* formula_temporal_base ::= formula_temporal_card */ yytestcase(yyruleno==250);
#line 833 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy489 = yymsp[0].minor.yy489;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy489->beginLoc());
			YYERROR;
		}
	}
#line 4199 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* formula_base ::= TRUE */
      case 176: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==176);
      case 251: /* formula_temporal_base ::= TRUE */ yytestcase(yyruleno==251);
      case 272: /* head_formula ::= TRUE */ yytestcase(yyruleno==272);
#line 840 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4207 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* formula_base ::= FALSE */
      case 177: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==177);
      case 252: /* formula_temporal_base ::= FALSE */ yytestcase(yyruleno==252);
      case 273: /* head_formula ::= FALSE */ yytestcase(yyruleno==273);
#line 841 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 4215 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* comparison ::= term_strong EQ term */
      case 145: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==145);
      case 178: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==178);
      case 253: /* comparison_temporal ::= term_temporal_strong EQ term_temporal */ yytestcase(yyruleno==253);
#line 843 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy283, yymsp[0].minor.yy283, yymsp[-2].minor.yy283->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4224 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* comparison ::= term_strong DBL_EQ term */
      case 146: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==146);
      case 179: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==179);
      case 254: /* comparison_temporal ::= term_temporal_strong DBL_EQ term_temporal */ yytestcase(yyruleno==254);
#line 844 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy283, yymsp[0].minor.yy283, yymsp[-2].minor.yy283->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4233 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* comparison ::= term_strong NEQ term */
      case 147: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==147);
      case 180: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==180);
      case 255: /* comparison_temporal ::= term_temporal_strong NEQ term_temporal */ yytestcase(yyruleno==255);
#line 845 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy283, yymsp[0].minor.yy283, yymsp[-2].minor.yy283->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4242 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* comparison ::= term_strong LTHAN term */
      case 148: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==148);
      case 181: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==181);
      case 256: /* comparison_temporal ::= term_temporal_strong LTHAN term_temporal */ yytestcase(yyruleno==256);
#line 846 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy283, yymsp[0].minor.yy283, yymsp[-2].minor.yy283->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4251 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* comparison ::= term_strong GTHAN term */
      case 149: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==149);
      case 182: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==182);
      case 257: /* comparison_temporal ::= term_temporal_strong GTHAN term_temporal */ yytestcase(yyruleno==257);
#line 847 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy283, yymsp[0].minor.yy283, yymsp[-2].minor.yy283->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4260 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* comparison ::= term_strong LTHAN_EQ term */
      case 150: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==150);
      case 183: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==183);
      case 258: /* comparison_temporal ::= term_temporal_strong LTHAN_EQ term_temporal */ yytestcase(yyruleno==258);
#line 848 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy283, yymsp[0].minor.yy283, yymsp[-2].minor.yy283->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4269 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 144: /* comparison ::= term_strong GTHAN_EQ term */
      case 151: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==151);
      case 184: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==184);
      case 259: /* comparison_temporal ::= term_temporal_strong GTHAN_EQ term_temporal */ yytestcase(yyruleno==259);
#line 849 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy283, yymsp[0].minor.yy283, yymsp[-2].minor.yy283->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4278 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* comparison ::= constant DBL_EQ term */
#line 857 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy313, yymsp[0].minor.yy283, yymsp[-2].minor.yy313->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4284 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* comparison ::= constant NEQ term */
#line 858 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy313, yymsp[0].minor.yy283, yymsp[-2].minor.yy313->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4290 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* comparison ::= constant LTHAN term */
#line 859 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy313, yymsp[0].minor.yy283, yymsp[-2].minor.yy313->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,85,&yymsp[-1].minor);
}
#line 4296 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* comparison ::= constant GTHAN term */
#line 860 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy313, yymsp[0].minor.yy283, yymsp[-2].minor.yy313->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4302 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* comparison ::= constant LTHAN_EQ term */
#line 861 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy313, yymsp[0].minor.yy283, yymsp[-2].minor.yy313->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4308 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= constant GTHAN_EQ term */
#line 862 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy313, yymsp[0].minor.yy283, yymsp[-2].minor.yy313->beginLoc(), yymsp[0].minor.yy283->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4314 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* atomic_formula ::= constant */
      case 162: /* atomic_formula_anon ::= const_anon */ yytestcase(yyruleno==162);
      case 185: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==185);
#line 889 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy346, yymsp[0].minor.yy313, true); }
#line 4321 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* atomic_formula ::= TILDE constant */
      case 163: /* atomic_formula_anon ::= TILDE const_anon */ yytestcase(yyruleno==163);
      case 186: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==186);
#line 890 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy346, yymsp[0].minor.yy313, false);   yy_destructor(yypParser,75,&yymsp[-1].minor);
}
#line 4329 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* atomic_formula ::= constant EQ term */
      case 164: /* atomic_formula_anon ::= const_anon EQ term */ yytestcase(yyruleno==164);
      case 187: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==187);
#line 891 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy346 = new AtomicFormula(yymsp[-2].minor.yy313, yymsp[0].minor.yy283, yymsp[-2].minor.yy313->beginLoc(), yymsp[0].minor.yy283->endLoc());	  yy_destructor(yypParser,81,&yymsp[-1].minor);
}
#line 4337 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 161: /* atomic_formula_anon ::= atomic_formula */
      case 274: /* atomic_head_formula ::= atomic_formula */ yytestcase(yyruleno==274);
      case 361: /* show_elem ::= atomic_formula_one_const */ yytestcase(yyruleno==361);
#line 893 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy346 = yymsp[0].minor.yy346; }
#line 4344 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 188: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
      case 260: /* formula_temporal_quant ::= BRACKET_L quant_lst PIPE formula_temporal BRACKET_R */ yytestcase(yyruleno==260);
#line 957 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy269=NULL;
		ref_ptr<const Token> bl_ptr = yymsp[-4].minor.yy0;
		ref_ptr<QuantifierFormula::QuantifierList> lst_ptr = yymsp[-3].minor.yy77;
		ref_ptr<Formula> sub_ptr = yymsp[-1].minor.yy489;
		ref_ptr<const Token> br_ptr = yymsp[0].minor.yy0;

		if (!parser->lang()->support(Language::Feature::FORMULA_QUANTIFIER)) {
			parser->_feature_error(Language::Feature::FORMULA_QUANTIFIER, &yymsp[-4].minor.yy0->beginLoc());
			YYERROR;
		} else yygotominor.yy269 = new QuantifierFormula(yymsp[-3].minor.yy77, yymsp[-1].minor.yy489, yymsp[-4].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
	  yy_destructor(yypParser,98,&yymsp[-2].minor);
}
#line 4362 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 189: /* quant_lst ::= quant_op variable */
#line 971 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy77 = new QuantifierFormula::QuantifierList();
		yygotominor.yy77->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy153, yymsp[0].minor.yy37));
	}
#line 4370 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 190: /* quant_lst ::= quant_lst quant_op variable */
#line 977 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy77 = yymsp[-2].minor.yy77;
		yygotominor.yy77->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy153, yymsp[0].minor.yy37));
	}
#line 4378 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 191: /* quant_op ::= BIG_CONJ */
#line 982 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy153 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,90,&yymsp[0].minor);
}
#line 4384 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 192: /* quant_op ::= BIG_DISJ */
#line 983 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy153 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4390 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 193: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
      case 261: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==261);
#line 1029 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy489, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy55, yymsp[-1].minor.yy489, yymsp[0].minor.yy0, NULL);  }
#line 4396 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 194: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
      case 262: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R */ yytestcase(yyruleno==262);
#line 1030 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy489, yymsp[-4].minor.yy283, yymsp[-3].minor.yy0, yymsp[-2].minor.yy55, yymsp[-1].minor.yy489,  yymsp[0].minor.yy0, NULL);  }
#line 4402 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 195: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 263: /* formula_temporal_card ::= CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==263);
#line 1031 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy489, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy55, yymsp[-2].minor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy283); 	}
#line 4408 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 196: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
      case 264: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L card_var_lst formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==264);
#line 1032 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy489, yymsp[-5].minor.yy283, yymsp[-4].minor.yy0, yymsp[-3].minor.yy55, yymsp[-2].minor.yy489,  yymsp[-1].minor.yy0, yymsp[0].minor.yy283); 	}
#line 4414 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 197: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
      case 265: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==265);
#line 1033 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy489, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy489, yymsp[0].minor.yy0, NULL);  }
#line 4420 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 198: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
      case 266: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R */ yytestcase(yyruleno==266);
#line 1034 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy489, yymsp[-3].minor.yy283, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy489,  yymsp[0].minor.yy0, NULL);  }
#line 4426 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 199: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
      case 267: /* formula_temporal_card ::= CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==267);
#line 1035 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy489, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy283); 	}
#line 4432 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 200: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
      case 268: /* formula_temporal_card ::= term_temporal_strong CBRACKET_L formula_temporal CBRACKET_R term_temporal */ yytestcase(yyruleno==268);
#line 1036 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy489, yymsp[-4].minor.yy283, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy489,  yymsp[-1].minor.yy0, yymsp[0].minor.yy283); 	}
#line 4438 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 201: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1040 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy55 = yymsp[-1].minor.yy55;
	  yy_destructor(yypParser,98,&yymsp[0].minor);
}
#line 4446 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 202: /* card_var_lst_inner ::= variable */
#line 1045 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy37;
		yygotominor.yy55 = new CardinalityFormula::VariableList();
		yygotominor.yy55->push_back(yymsp[0].minor.yy37->symbol());
	}
#line 4455 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 203: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1052 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy37;
		yygotominor.yy55 = yymsp[-2].minor.yy55;
		yygotominor.yy55->push_back(yymsp[0].minor.yy37->symbol());
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4465 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 216: /* term_temporal ::= term_temporal COLON term */
      case 229: /* term_temporal_strong ::= term_temporal_strong COLON term_strong */ yytestcase(yyruleno==229);
#line 1118 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy283, yymsp[-2].minor.yy283, yymsp[-1].minor.yy0, yymsp[0].minor.yy283, BindingTerm); }
#line 4471 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 247: /* formula_temporal ::= term_temporal_strong COLON formula */
#line 1199 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy489, yymsp[-2].minor.yy283, yymsp[-1].minor.yy0, yymsp[0].minor.yy489, BindingFormula); }
#line 4476 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 271: /* head_formula ::= formula_smpl_card */
#line 1293 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy489 = yymsp[0].minor.yy273;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy273->beginLoc());
			YYERROR;
		}
	}
#line 4487 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 275: /* atomic_head_formula ::= DASH constant */
#line 1306 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy346 = NULL;
		ref_ptr<const Token> d_ptr = yymsp[-1].minor.yy0;
		ref_ptr<Constant> c_ptr = yymsp[0].minor.yy313;

		if (!parser->lang()->support(Language::Feature::FORMULA_NOT_DASH_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_NOT_DASH_HEAD);
			YYERROR;
		} else {
			ATOMIC_FORMULA(yygotominor.yy346, yymsp[0].minor.yy313, false); 
		}
	}
#line 4503 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 276: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1319 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy273, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy55, yymsp[-1].minor.yy346, yymsp[0].minor.yy0, NULL);  }
#line 4508 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 277: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1320 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy273, yymsp[-4].minor.yy283, yymsp[-3].minor.yy0, yymsp[-2].minor.yy55, yymsp[-1].minor.yy346,  yymsp[0].minor.yy0, NULL);  }
#line 4513 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 278: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1321 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy273, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy55, yymsp[-2].minor.yy346, yymsp[-1].minor.yy0, yymsp[0].minor.yy283); 	}
#line 4518 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 279: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1322 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy273, yymsp[-5].minor.yy283, yymsp[-4].minor.yy0, yymsp[-3].minor.yy55, yymsp[-2].minor.yy346,  yymsp[-1].minor.yy0, yymsp[0].minor.yy283); 	}
#line 4523 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 280: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1323 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy273, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy346, yymsp[0].minor.yy0, NULL);  }
#line 4528 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 281: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1324 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy273, yymsp[-3].minor.yy283, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy346,  yymsp[0].minor.yy0, NULL);  }
#line 4533 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 282: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1325 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy273, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy346, yymsp[-1].minor.yy0, yymsp[0].minor.yy283); 	}
#line 4538 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 283: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1326 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy273, yymsp[-4].minor.yy283, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy346,  yymsp[-1].minor.yy0, yymsp[0].minor.yy283); 	}
#line 4543 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 284: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1345 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy255 = NULL;
        ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
        ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
        ref_ptr<MacroDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy361;
        ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;

        if (!parser->lang()->support(Language::Feature::DECL_MACRO)) {
            parser->_feature_error(Language::Feature::DECL_MACRO, &yymsp[-2].minor.yy0->beginLoc());
            YYERROR;
        } else {
		    BOOST_FOREACH(symbols::MacroSymbol* m, *yymsp[-1].minor.yy361) {
			    if (!parser->symtab()->create(m)) {
	    	        // Check if it's a duplicate
	    	        symbols::MacroSymbol* m2 = (symbols::MacroSymbol*)parser->symtab()->resolve(symbols::Symbol::Type::MACRO, *m->base(), m->arity());
		            if (!m2 || m2 != m) {
		                parser->_parse_error("Detected conflicting definition of symbol \"" + *m->name() + "\".", &yygotominor.yy255->beginLoc());
		            } else {
		                parser->_parse_error("Detected a duplicate definition of symbol \"" + *m->name() + "\".", &yygotominor.yy255->beginLoc());
		            }
		        }
		    }

			yygotominor.yy255 = new MacroDeclaration(yymsp[-1].minor.yy361, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
        }
    }
#line 4573 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 285: /* macro_def_lst ::= macro_bnd */
#line 1373 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy361 = new MacroDeclaration::ElementList();
        yygotominor.yy361->push_back(yymsp[0].minor.yy235);
    }
#line 4581 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 286: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1379 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy361 = yymsp[-2].minor.yy361;
        yygotominor.yy361->push_back(yymsp[0].minor.yy235);
      yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4590 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1385 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy458;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy235 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy458);
      yy_destructor(yypParser,71,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4604 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1394 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy235 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,96,&yymsp[-1].minor);
}
#line 4615 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* macro_args ::= macro_arg */
#line 1402 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy458 = new MacroSymbol::ArgumentList();
        yygotominor.yy458->push_back(yymsp[0].minor.yy275->str());
        delete yymsp[0].minor.yy275;
    }
#line 4624 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* macro_args ::= macro_args COMMA macro_arg */
#line 1408 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy458 = yymsp[-2].minor.yy458;
        yygotominor.yy458->push_back(yymsp[0].minor.yy275->str());
        delete yymsp[0].minor.yy275;
      yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4634 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* macro_arg ::= POUND_INTEGER */
      case 292: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==292);
#line 1415 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy275 = yymsp[0].minor.yy0;
    }
#line 4642 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* sort_lst ::= sort */
#line 1442 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy291 = new ConstantSymbol::SortList();
		yygotominor.yy291->push_back(yymsp[0].minor.yy49);
	}
#line 4650 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 294: /* sort_lst ::= sort_lst COMMA sort */
#line 1447 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy291 = yymsp[-2].minor.yy291;
		yygotominor.yy291->push_back(yymsp[0].minor.yy49);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 4659 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* sort ::= sort_id_nr */
      case 301: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==301);
      case 302: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==302);
#line 1472 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy49 = yymsp[0].minor.yy49; }
#line 4666 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* sort ::= sort_id_nr STAR */
#line 1473 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy49, yymsp[-1].minor.yy49, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::NONE)); }
#line 4671 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* sort ::= sort_id_nr CARROT */
#line 1474 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy49, yymsp[-1].minor.yy49, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::UNKNOWN)); }
#line 4676 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* sort ::= sort PLUS object_nullary */
#line 1476 "bcplus/parser/detail/lemon_parser.y"
{ u::ref_ptr<const Object> o_ptr = yymsp[0].minor.yy54; DYNAMIC_SORT_PLUS(yygotominor.yy49, yymsp[-2].minor.yy49, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, yymsp[0].minor.yy54->symbol()); }
#line 4681 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* sort ::= sort PLUS IDENTIFIER */
#line 1479 "bcplus/parser/detail/lemon_parser.y"
{
												  u::ref_ptr<const Referenced> s_ptr = yymsp[-2].minor.yy49, op_ptr = yymsp[-1].minor.yy0, id_ptr = yymsp[0].minor.yy0;
												  u::ref_ptr<const ObjectSymbol> obj = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
												  if(!obj) {
													if (parser->lang()->support(Language::Feature::SORT_PLUS)) 
														parser->_parse_error("\"" + *yymsp[0].minor.yy0->str() + "\" could not be declared as an object as this conflicts with a previous declarations of this identifier.", &yymsp[0].minor.yy0->beginLoc());
													else 
														parser->_feature_error(Language::Feature::SORT_PLUS, &yymsp[-1].minor.yy0->beginLoc());
													YYERROR;
												  } else {
													DYNAMIC_SORT_PLUS(yygotominor.yy49, yymsp[-2].minor.yy49, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, obj);
												  }
												}
#line 4698 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* sort ::= sort PLUS INTEGER */
#line 1493 "bcplus/parser/detail/lemon_parser.y"
{ 
												  ref_ptr<const Object> t_ptr;
												  BASIC_TERM(t_ptr, yymsp[0].minor.yy0);
												  DYNAMIC_SORT_PLUS(yygotominor.yy49, yymsp[-2].minor.yy49, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, t_ptr->symbol()); 
												}
#line 4707 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* sort_nr ::= num_range */
#line 1504 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy93;

		yygotominor.yy49 = NULL;

		if (!parser->lang()->support(Language::Feature::NUMRANGE_SORT)) {
			parser->_feature_error(Language::Feature::NUMRANGE_SORT, &yymsp[0].minor.yy93->beginLoc());
			YYERROR;
		}

		// X..Y becomes __sort_X_Y__
		std::string name = "__sort_" + boost::lexical_cast<std::string>(yymsp[0].minor.yy93->min()) + "__" + boost::lexical_cast<std::string>(yymsp[0].minor.yy93->max()) + "__";

		ref_ptr<SortSymbol::ObjectList> objs = new SortSymbol::ObjectList();

		// Generate the objects that it will have
		for (int i = yymsp[0].minor.yy93->min(); i <= yymsp[0].minor.yy93->max(); i++) {
			std::string obj_name = boost::lexical_cast<std::string>(i);
			ObjectSymbol const* sym = parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(obj_name)));

			if (!sym) {
				yygotominor.yy49 = NULL;
				parser->_parse_error("An error occurred creating symbol \"" + obj_name + "/0\".", &yymsp[0].minor.yy93->beginLoc());
				YYERROR;
			}
			objs->insert(sym);
		}		

		// dynamically declare the sort
		yygotominor.yy49 = parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name), objs));
		if (!yygotominor.yy49) {
				parser->_parse_error("An error occurred creating symbol \"" + name + "/0\".", &yymsp[0].minor.yy93->beginLoc());
				YYERROR;
		} 
	}
#line 4746 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* sort_id ::= IDENTIFIER */
#line 1541 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy49 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy49) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4759 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1572 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ConstantDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy97;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_CONSTANT)) {
			yygotominor.yy271 = NULL;
			parser->_feature_error(Language::Feature::DECL_CONSTANT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy271 = new ConstantDeclaration(yymsp[-1].minor.yy97, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

		}
	}
#line 4778 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* constant_bnd_lst ::= constant_bnd */
#line 1589 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = yymsp[0].minor.yy97;
	}
#line 4785 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1594 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy97;
		yygotominor.yy97 = yymsp[-2].minor.yy97;
		yygotominor.yy97->splice(yygotominor.yy97->end(), *yymsp[0].minor.yy97);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 4795 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1614 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const SortSymbol> s_ptr = yymsp[-1].minor.yy49;
		ref_ptr<const Referenced> names_ptr = yymsp[-5].minor.yy498;
		yygotominor.yy97 = new ConstantDeclaration::ElementList();

		// NOTE: additive constants default to the additive sort, not the boolean sort
		if (yymsp[-3].minor.yy157 & ConstantSymbol::Type::M_ADDITIVE) s_ptr = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

		// external constants should have "unknown" in their sort
		else if (yymsp[-3].minor.yy157 & ConstantSymbol::Type::M_EXTERNAL) s_ptr = parser->symtab()->carrot(yymsp[-1].minor.yy49);

		// non-boolean abActions should contain "none"
		else if (yymsp[-3].minor.yy157 == ConstantSymbol::Type::ABACTION && s_ptr->domainType() != DomainType::BOOLEAN) s_ptr = parser->symtab()->star(yymsp[-1].minor.yy49);

		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-5].minor.yy498) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[-3].minor.yy157, decl.first->str(), s_ptr, decl.second);
			yygotominor.yy97->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4823 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1636 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy498, s_ptr = yymsp[0].minor.yy49;
		yygotominor.yy97 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy498) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(ConstantSymbol::Type::RIGID, decl.first->str(), yymsp[0].minor.yy49, decl.second);
			yygotominor.yy97->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 4838 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1647 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy498;
		yygotominor.yy97 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy498) {
			// attempt to declare each symbol
			ref_ptr<SortSymbol> s = parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN);

			// NOTE: additive constants default to the additive sort, not the boolean sort
			if (yymsp[0].minor.yy157 & ConstantSymbol::Type::M_ADDITIVE) s = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

			// external constants should have "unknown" in their sort
			else if (yymsp[0].minor.yy157 & ConstantSymbol::Type::M_EXTERNAL) s = parser->symtab()->carrot(s);

			// non-boolean abActions should contain "none"
			else if (yymsp[0].minor.yy157 == ConstantSymbol::Type::ABACTION && s->domainType() != DomainType::BOOLEAN) s = parser->symtab()->star(s);


			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[0].minor.yy157, decl.first->str(), parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN), decl.second);
			yygotominor.yy97->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 4865 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1670 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-4].minor.yy498, s_ptr = yymsp[-2].minor.yy292, id_ptr = yymsp[0].minor.yy0;


		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[0].minor.yy0->str());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\" is not a valid constant symbol.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy97 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-4].minor.yy498) {
				ref_ptr<ConstantSymbol> c= new AttributeSymbol(decl.first->str(), yymsp[-2].minor.yy292, c, decl.second);
				yygotominor.yy97->push_back(c);
				CONSTANT_DECL(c, decl.first->beginLoc());
			}
		}
	  yy_destructor(yypParser,76,&yymsp[-3].minor);
  yy_destructor(yypParser,55,&yymsp[-1].minor);
}
#line 4894 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1694 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-7].minor.yy498, s_ptr = yymsp[-5].minor.yy292, id_ptr = yymsp[-3].minor.yy0, lst_ptr = yymsp[-1].minor.yy291;

		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy291->size());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy291->size()) + "\" is not a valid constant symbol.", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy291->size()) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// ensure that the sorts match the declaration of the symbol
			SortList::const_iterator it = yymsp[-1].minor.yy291->begin();
			BOOST_FOREACH(SortSymbol const* sort, *c) {
				if (*it != sort) {
					// check to see if it'yymsp[-5].minor.yy292 a subsort, which is also permissable
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

			yygotominor.yy97 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-7].minor.yy498) {
				// ensure that the sorts match the start of the sort list for each of the symbols
				if (decl.second->size() < yymsp[-1].minor.yy291->size()) {
					parser->_parse_error("Detected a malformed attribute declaration. An attribute must duplicate its parent'yymsp[-5].minor.yy292 parameters.", &decl.first->beginLoc());
					YYERROR;
				} else {
					bool good_sort = true;		
					it = decl.second->begin();
					BOOST_FOREACH(SortSymbol const* sort, *yymsp[-1].minor.yy291) {
						if (*it != sort) {
							// check to see if it'yymsp[-5].minor.yy292 a subsort, which is also permissable
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
						ref_ptr<ConstantSymbol> sym = new AttributeSymbol(decl.first->str(), yymsp[-5].minor.yy292, c, decl.second);
						yygotominor.yy97->push_back(sym);
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
#line 4975 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* constant_dcl_lst ::= IDENTIFIER */
#line 1770 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy498 = new IdentifierDeclList();
		yygotominor.yy498->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 4983 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1775 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy498 = new IdentifierDeclList();
		yygotominor.yy498->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy291));
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 4993 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1780 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy498 = yymsp[-2].minor.yy498;
		yygotominor.yy498->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5002 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1785 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy498 = yymsp[-5].minor.yy498;
		yygotominor.yy498->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy291));
	  yy_destructor(yypParser,102,&yymsp[-4].minor);
  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5013 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* constant_dcl_type ::= ABACTION */
#line 1792 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5025 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* constant_dcl_type ::= ACTION */
#line 1801 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5037 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1810 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5049 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 320: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1819 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5061 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* constant_dcl_type ::= EXTERNALACTION */
#line 1828 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5073 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 322: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1837 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5085 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 323: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1846 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5097 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 324: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1855 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5109 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 325: /* constant_dcl_type ::= RIGID */
#line 1864 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5121 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 326: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1873 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5133 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 327: /* constant_dcl_type ::= SDFLUENT */
#line 1883 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy157 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 5145 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 328: /* attrib_spec ::= ATTRIBUTE */
#line 1893 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy292 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[0].minor.yy0;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// grab the boolean sort and provide it
			yygotominor.yy292 = parser->symtab()->star(parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN));
		}
	}
#line 5160 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1906 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy292 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[-3].minor.yy0, s_ptr = yymsp[-1].minor.yy49;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy292 = parser->symtab()->star(yymsp[-1].minor.yy49);
		}
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5176 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 330: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1934 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ObjectDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy190;
		
		if (!parser->lang()->support(Language::Feature::DECL_OBJECT)) {
			yygotominor.yy288 = NULL;
			parser->_feature_error(Language::Feature::DECL_OBJECT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy288 = new ObjectDeclaration(yymsp[-1].minor.yy190, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ObjectDeclaration::Element* bnd, *yymsp[-1].minor.yy190) {
				BOOST_FOREACH(ObjectSymbol const* o, *bnd) {
						bnd->sort()->add(o);
				}
			}
		}
	}
#line 5201 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 331: /* object_bnd_lst ::= object_bnd */
#line 1957 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy190 = new ObjectDeclaration::ElementList();
		yygotominor.yy190->push_back(yymsp[0].minor.yy302);
	}
#line 5209 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 332: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 1963 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy190 = yymsp[-2].minor.yy190;
		yygotominor.yy190->push_back(yymsp[0].minor.yy302);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5218 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 333: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 1969 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy302 = new ObjectDeclaration::Element(yymsp[0].minor.yy49, yymsp[-2].minor.yy189);
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5226 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 334: /* object_lst ::= object_spec */
#line 1974 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy189 = yymsp[0].minor.yy189;
	}
#line 5233 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 335: /* object_lst ::= object_lst COMMA object_spec */
#line 1978 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy189 = yymsp[-2].minor.yy189;
		yygotominor.yy189->splice(yygotominor.yy189->end(), *yymsp[0].minor.yy189);
		delete yymsp[0].minor.yy189;
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5243 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 336: /* object_spec ::= IDENTIFIER */
#line 1987 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		yygotominor.yy189 = NULL;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy189 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy189->push_back(o);
		}
	}
#line 5259 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 337: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 2000 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy189 = NULL;
		ref_ptr<ObjectSymbol::SortList> lst_ptr = yymsp[-1].minor.yy291;
		ref_ptr<const Token> id_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy291));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(),yymsp[-1].minor.yy291->size()) + "\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy189 = new  ObjectDeclaration::Element::ObjectList();
			yygotominor.yy189->push_back(o);
		}
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5278 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 338: /* object_spec ::= INTEGER */
#line 2015 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy189 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy189 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy189->push_back(o);
		}
	}
#line 5294 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 339: /* object_spec ::= num_range */
#line 2029 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy189 = new ObjectDeclaration::Element::ObjectList();
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy93;

		// iterate over the range and add it to the list
		for (int i = yymsp[0].minor.yy93->min(); i <= yymsp[0].minor.yy93->max(); i++) {
			std::string name = boost::lexical_cast<std::string>(i);
			ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(name)));
			if (!o) {
				parser->_parse_error("INTERNAL ERROR: Could not create object symbol \"" + Symbol::genName(name, 0) + "\".", &yymsp[0].minor.yy93->beginLoc());
				YYERROR;
			} else {
				yygotominor.yy189->push_back(o);
			}
		}
	}
#line 5314 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 340: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 2061 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<VariableDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy125;
		
		if (!parser->lang()->support(Language::Feature::DECL_VARIABLE)) {
			yygotominor.yy459 = NULL;
			parser->_feature_error(Language::Feature::DECL_VARIABLE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {

			VariableSymbol* v2;

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ref_ptr<VariableSymbol>& v, *yymsp[-1].minor.yy125) {
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

			yygotominor.yy459 = new VariableDeclaration(yymsp[-1].minor.yy125, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());


		}
	}
#line 5352 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 341: /* variable_bnd_lst ::= variable_bnd */
#line 2097 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy125 = yymsp[0].minor.yy125;
	}
#line 5359 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 342: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 2102 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy125 = yymsp[-2].minor.yy125;
		yygotominor.yy125->splice(yygotominor.yy125->end(), *yymsp[0].minor.yy125);
		delete yymsp[0].minor.yy125;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5369 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 343: /* variable_bnd ::= variable_lst DBL_COLON sort */
#line 2109 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy125 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy16) {
			yygotominor.yy125->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy49));
		}



		delete yymsp[-2].minor.yy16;
	  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5385 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 344: /* variable_lst ::= IDENTIFIER */
#line 2122 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy16 = new TokenList();
		yygotominor.yy16->push_back(yymsp[0].minor.yy0);
	}
#line 5393 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 345: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 2127 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy16 = yymsp[-2].minor.yy16;
		yygotominor.yy16->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5402 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 346: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 2148 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<SortDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy7;
		
		if (!parser->lang()->support(Language::Feature::DECL_SORT)) {
			yygotominor.yy237 = NULL;
			parser->_feature_error(Language::Feature::DECL_SORT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy237 = new SortDeclaration(yymsp[-1].minor.yy7, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5420 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 347: /* sort_bnd_lst ::= sort_bnd */
      case 349: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==349);
#line 2164 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy7 = yymsp[0].minor.yy7;
	}
#line 5428 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 348: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 2169 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy7 = yymsp[-2].minor.yy7;
		yygotominor.yy7->splice(yygotominor.yy7->end(), *yymsp[0].minor.yy7);
		delete yymsp[0].minor.yy7;
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5438 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 2181 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy7) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy7) {
				sym2->addSubSort(sym);
			}
		}
		yygotominor.yy7 = yymsp[-2].minor.yy7;
		yygotominor.yy7->splice(yymsp[-2].minor.yy7->end(), *yymsp[0].minor.yy7);
		delete yymsp[0].minor.yy7;

	  yy_destructor(yypParser,100,&yymsp[-1].minor);
}
#line 5454 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 2193 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy7) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy7) {
				sym->addSubSort(sym2);
			}
		}
		yygotominor.yy7 = yymsp[-2].minor.yy7;
		yygotominor.yy7->splice(yymsp[-2].minor.yy7->end(), *yymsp[0].minor.yy7);
		delete yymsp[0].minor.yy7;
	  yy_destructor(yypParser,99,&yymsp[-1].minor);
}
#line 5469 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 2204 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy7 = yymsp[-1].minor.yy7;
	  yy_destructor(yypParser,71,&yymsp[-2].minor);
  yy_destructor(yypParser,72,&yymsp[0].minor);
}
#line 5478 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* sort_dcl_lst ::= IDENTIFIER */
#line 2209 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yygotominor.yy7 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy7 = new SortDeclaration::ElementList();
			yygotominor.yy7->push_back(s);
		}

		delete yymsp[0].minor.yy0;
	}
#line 5495 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 2223 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy7 = yymsp[-2].minor.yy7;
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yymsp[-2].minor.yy7 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yymsp[-2].minor.yy7->push_back(s);
		}

		delete yymsp[0].minor.yy0;

	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5514 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 2250 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy328 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<ShowStatement::ElementList> lst_ptr = yymsp[-1].minor.yy11;

		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy328 = new ShowStatement(yymsp[-1].minor.yy11, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5530 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 2264 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy328 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_SHOW_ALL)) {
			parser->_feature_error(Language::Feature::DECL_SHOW_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy328 = new ShowAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5548 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 2281 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy328 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<HideStatement::ElementList> lst_ptr = yymsp[-1].minor.yy11;

		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy328 = new HideStatement(yymsp[-1].minor.yy11, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5564 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2295 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy328 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_HIDE_ALL)) {
			parser->_feature_error(Language::Feature::DECL_HIDE_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy328 = new HideAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5582 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* show_lst ::= show_elem */
#line 2313 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy11 = new ShowStatement::ElementList();
		yygotominor.yy11->push_back(yymsp[0].minor.yy346);
	}
#line 5590 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* show_lst ::= show_lst COMMA show_elem */
#line 2318 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy11 = yymsp[-2].minor.yy11;
		yygotominor.yy11->push_back(yymsp[0].minor.yy346);
	  yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 5599 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* stmt_noconcurrency ::= NOCONCURRENCY PERIOD */
#line 2346 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy129, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5604 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD */
#line 2347 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy298, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5609 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD */
#line 2373 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy328, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy416, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5615 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 365: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD */
#line 2374 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy328, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy416, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,81,&yymsp[-2].minor);
}
#line 5621 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2399 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy98 = NULL;
		ref_ptr<const Referenced> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, data_l_ptr = yymsp[-1].minor.yy445.l, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Referenced> data_maxstep_ptr = yymsp[-1].minor.yy445.maxstep, data_label_ptr = yymsp[-1].minor.yy445.label;

		ref_ptr<const ReferencedString> label;
		if (yymsp[-1].minor.yy445.label) label = yymsp[-1].minor.yy445.label->str();
		else label = new ReferencedString("0");

		int min = -1, max = -1;
		if (yymsp[-1].minor.yy445.maxstep) {
			min = yymsp[-1].minor.yy445.maxstep->min();
			max = yymsp[-1].minor.yy445.maxstep->max();
		}

		if (!parser->lang()->support(Language::Feature::DECL_QUERY)) {
			parser->_feature_error(Language::Feature::DECL_QUERY, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			bool good = true;

			// resolve the query label
			ref_ptr<QuerySymbol> sym = new QuerySymbol(label, min, max);
			if (!parser->symtab()->create(sym)) {
				parser->_parse_error("Could not create query, the label \"" + *label + "\" already exists.", (yymsp[-1].minor.yy445.label ? &yymsp[-1].minor.yy445.label->beginLoc() : &yymsp[-2].minor.yy0->beginLoc()));
				good = false;
				YYERROR;
			}


			if (good) yygotominor.yy98 = new QueryStatement(sym, yymsp[-1].minor.yy445.l, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5658 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* query_lst ::= formula_temporal */
#line 2435 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy445.l = new QueryStatement::FormulaList();
		yygotominor.yy445.maxstep = NULL;
		yygotominor.yy445.label = NULL;

		yygotominor.yy445.l->push_back(yymsp[0].minor.yy489);
	}
#line 5669 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* query_lst ::= query_maxstep_decl */
#line 2444 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy445.l = new QueryStatement::FormulaList();
		yygotominor.yy445.maxstep = yymsp[0].minor.yy192;
		yygotominor.yy445.label = NULL;
	}
#line 5678 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 369: /* query_lst ::= query_label_decl */
#line 2451 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy445.l = new QueryStatement::FormulaList();
		yygotominor.yy445.maxstep = NULL;
		yygotominor.yy445.label = yymsp[0].minor.yy275;
	}
#line 5687 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 370: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2458 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy445 = yymsp[-2].minor.yy445;
		yymsp[-2].minor.yy445.l->push_back(yymsp[0].minor.yy489);
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5696 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 371: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2464 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy445 = yymsp[-2].minor.yy445;

		if (yygotominor.yy445.maxstep) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy192->beginLoc());
			delete yymsp[0].minor.yy192;
			YYERROR;
		} else {
			yygotominor.yy445.maxstep = yymsp[0].minor.yy192;
		}
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5712 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 372: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2477 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy445 = yymsp[-2].minor.yy445;
		if (yygotominor.yy445.label) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy275->beginLoc());
			delete yymsp[0].minor.yy275;
			YYERROR;

		} else {
			yygotominor.yy445.label = yymsp[0].minor.yy275;
		}
	  yy_destructor(yypParser,93,&yymsp[-1].minor);
}
#line 5728 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 373: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2503 "bcplus/parser/detail/lemon_parser.y"
{ 
	yygotominor.yy192 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, i_ptr = yymsp[0].minor.yy0;


	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {

		int max = -1;
		try {
			max = boost::lexical_cast<int>(*yymsp[0].minor.yy0->str());
			yygotominor.yy192 = new NumberRange(-1, max, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		} catch (boost::bad_lexical_cast const& e) {
			parser->_parse_error("INTERNAL ERROR: An error occurred extracting an integer from \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5753 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 374: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range */
#line 2524 "bcplus/parser/detail/lemon_parser.y"
{
	yygotominor.yy192 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, nr_ptr = yymsp[0].minor.yy93;

	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {
		yygotominor.yy192 = yymsp[0].minor.yy93;
		nr_ptr.release();
	}
  yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5770 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 375: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 376: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==376);
#line 2538 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy275, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 5777 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 377: /* clause_if ::= IF formula */
#line 2573 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy489, Language::Feature::CLAUSE_IF); 		}
#line 5782 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 378: /* clause_if ::= */
      case 380: /* clause_after ::= */ yytestcase(yyruleno==380);
      case 382: /* clause_ifcons ::= */ yytestcase(yyruleno==382);
      case 386: /* clause_where ::= */ yytestcase(yyruleno==386);
#line 2574 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy489 = NULL; }
#line 5790 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 379: /* clause_after ::= AFTER formula */
#line 2575 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy489, Language::Feature::CLAUSE_AFTER);	}
#line 5795 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 381: /* clause_ifcons ::= IFCONS formula */
#line 2577 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy489, Language::Feature::CLAUSE_IFCONS); 	}
#line 5800 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 383: /* clause_unless ::= UNLESS atomic_formula_anon */
#line 2579 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy346, yymsp[-1].minor.yy0, yymsp[0].minor.yy346, Language::Feature::CLAUSE_UNLESS); 	}
#line 5805 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 384: /* clause_unless ::= */
#line 2580 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy346 = NULL; }
#line 5810 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 385: /* clause_where ::= WHERE formula_no_const */
#line 2581 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy489, yymsp[-1].minor.yy0, yymsp[0].minor.yy489, Language::Feature::CLAUSE_WHERE); 	}
#line 5815 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 387: /* stmt_law ::= law_basic */
      case 388: /* stmt_law ::= law_caused */ yytestcase(yyruleno==388);
      case 389: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==389);
      case 390: /* stmt_law ::= law_impl */ yytestcase(yyruleno==390);
      case 391: /* stmt_law ::= law_causes */ yytestcase(yyruleno==391);
      case 392: /* stmt_law ::= law_increments */ yytestcase(yyruleno==392);
      case 393: /* stmt_law ::= law_decrements */ yytestcase(yyruleno==393);
      case 394: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==394);
      case 395: /* stmt_law ::= law_always */ yytestcase(yyruleno==395);
      case 396: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==396);
      case 397: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==397);
      case 398: /* stmt_law ::= law_never */ yytestcase(yyruleno==398);
      case 399: /* stmt_law ::= law_default */ yytestcase(yyruleno==399);
      case 400: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==400);
      case 401: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==401);
      case 402: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==402);
      case 403: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==403);
      case 404: /* stmt_law ::= law_observed */ yytestcase(yyruleno==404);
#line 2627 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy328 = yymsp[0].minor.yy328;}
#line 5837 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 405: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2744 "bcplus/parser/detail/lemon_parser.y"
{ 
		if (yymsp[-5].minor.yy489 || yymsp[-4].minor.yy489 || yymsp[-3].minor.yy489 || yymsp[-2].minor.yy346 || yymsp[-1].minor.yy489) {
			LAW_BASIC_FORM(yygotominor.yy328, NULL, yymsp[-6].minor.yy489, yymsp[-5].minor.yy489, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, 
				yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
				Language::Feature::LAW_BASIC_D, BasicLaw); 
		} else {
			LAW_BASIC_FORM(yygotominor.yy328, NULL, yymsp[-6].minor.yy489, yymsp[-5].minor.yy489, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, 
				yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_FACT, 
				Language::Feature::LAW_BASIC_FACT, BasicLaw); 
		}
	}
#line 5852 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 406: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2756 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy328, yymsp[-7].minor.yy0, yymsp[-6].minor.yy489, yymsp[-5].minor.yy489, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, 
																																														yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 5859 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 407: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2760 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy328, yymsp[-7].minor.yy0, yymsp[-6].minor.yy489, yymsp[-5].minor.yy489, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, 
																																														yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 5866 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 408: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2764 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy328, yymsp[-4].minor.yy489, yymsp[-3].minor.yy0, yymsp[-2].minor.yy489, yymsp[-1].minor.yy489, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 5872 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 409: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2767 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy328, yymsp[-6].minor.yy346, yymsp[-5].minor.yy0, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 5878 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 410: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2771 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy328, yymsp[-8].minor.yy346, yymsp[-7].minor.yy0, yymsp[-6].minor.yy313, yymsp[-4].minor.yy283, yymsp[-3].minor.yy489, yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5885 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 411: /* law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2774 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy328, yymsp[-8].minor.yy346, yymsp[-7].minor.yy0, yymsp[-6].minor.yy313, yymsp[-4].minor.yy283, yymsp[-3].minor.yy489, yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_DECREMENTS, DecrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5892 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 412: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2778 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy328, yymsp[-6].minor.yy346, yymsp[-5].minor.yy0, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 5898 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 413: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2782 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy328, yymsp[-5].minor.yy0, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 5905 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 414: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2786 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy328, yymsp[-5].minor.yy0, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 5912 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 415: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2790 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy328, yymsp[-5].minor.yy0, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 5919 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 416: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2794 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy328, yymsp[-5].minor.yy0, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 5926 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 417: /* law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2798 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy328, yymsp[-7].minor.yy0, yymsp[-6].minor.yy346, yymsp[-5].minor.yy489, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, 
																																														yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 5933 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 418: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2802 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy328, yymsp[-7].minor.yy0, yymsp[-6].minor.yy313, yymsp[-5].minor.yy489, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, 
																																														yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 5940 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 419: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2806 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy328, yymsp[-7].minor.yy0, yymsp[-6].minor.yy313, yymsp[-5].minor.yy489, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, 
																																														yymsp[-2].minor.yy346, yymsp[-1].minor.yy489, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 5947 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 420: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2810 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy328, yymsp[-5].minor.yy0, yymsp[-4].minor.yy489, yymsp[-3].minor.yy489, yymsp[-2].minor.yy346, yymsp[-1].minor.yy489,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 5953 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 421: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2814 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy328, yymsp[-3].minor.yy0, yymsp[-2].minor.yy313, yymsp[-1].minor.yy489, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 5959 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 422: /* law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD */
#line 2819 "bcplus/parser/detail/lemon_parser.y"
{ 
			yygotominor.yy328 = NULL;
			ref_ptr<const Token> kw_ptr = yymsp[-4].minor.yy0, p_ptr = yymsp[0].minor.yy0;
			ref_ptr<AtomicFormula> head_ptr = yymsp[-3].minor.yy346;
			ref_ptr<Term> t_ptr = yymsp[-1].minor.yy283;

			// make sure that the At clause is integral
			if (yymsp[-1].minor.yy283->domainType() != DomainType::INTEGRAL) {
				parser->_parse_error("Expected an integral expression.", &yymsp[-1].minor.yy283->beginLoc());
				YYERROR;
			} else {
				LAW_SIMPLE_FORM(yygotominor.yy328, yymsp[-4].minor.yy0, yymsp[-3].minor.yy346, yymsp[-1].minor.yy283, yymsp[0].minor.yy0, Language::Feature::LAW_OBSERVED, ObservedLaw); 
			}
		  yy_destructor(yypParser,65,&yymsp[-2].minor);
}
#line 5978 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 423: /* stmt_code_blk ::= ASP_GR */
#line 2853 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy328, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 5983 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 424: /* stmt_code_blk ::= ASP_CP */
#line 2854 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy328, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 5988 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 425: /* stmt_code_blk ::= F2LP_GR */
#line 2855 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy328, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 5993 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 426: /* stmt_code_blk ::= F2LP_CP */
#line 2856 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy328, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 5998 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 427: /* stmt_code_blk ::= LUA_GR */
#line 2857 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy328, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 6003 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 428: /* stmt_code_blk ::= LUA_CP */
#line 2858 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy328, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 6008 "bcplus/parser/detail/lemon_parser.c"
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
#line 198 "bcplus/parser/detail/lemon_parser.y"
 parser->_parse_error("Syntax error.");	
#line 6074 "bcplus/parser/detail/lemon_parser.c"
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
