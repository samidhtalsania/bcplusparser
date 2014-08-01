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

		
#line 328 "bcplus/parser/detail/lemon_parser.y"

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
	


#line 943 "bcplus/parser/detail/lemon_parser.y"

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

#line 1023 "bcplus/parser/detail/lemon_parser.y"

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



#line 1261 "bcplus/parser/detail/lemon_parser.y"

		
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
#line 1408 "bcplus/parser/detail/lemon_parser.y"

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
#line 2115 "bcplus/parser/detail/lemon_parser.y"

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

#line 2138 "bcplus/parser/detail/lemon_parser.y"

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
#line 2165 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRange const* maxstep;
		Token const* label;
	};

#line 2272 "bcplus/parser/detail/lemon_parser.y"

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

#line 2344 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2430 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2614 "bcplus/parser/detail/lemon_parser.y"

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
#define YYNOCODE 246
#define YYACTIONTYPE unsigned short int
#define lemon_parserTOKENTYPE  Token const* 								
typedef union {
  int yyinit;
  lemon_parserTOKENTYPE yy0;
  ConstantDeclaration* yy5;
  ConstantDeclaration::ElementList* yy55;
  ObjectDeclaration* yy78;
  NCStatement* yy87;
  Term* yy88;
  CardinalityFormula* yy91;
  Token const* yy93;
  VariableDeclaration::ElementList* yy97;
  MacroDeclaration* yy101;
  QuantifierFormula::Operator::type yy111;
  CardinalityFormula::VariableList* yy125;
  QueryStatement* yy140;
  QuantifierFormula::QuantifierList* yy147;
  ObjectDeclaration::Element::ObjectList* yy148;
  SortSymbol const* yy152;
  StrongNCStatement* yy158;
  Variable* yy163;
  ShowStatement::ElementList* yy165;
  MacroDeclaration::ElementList* yy179;
  ObjectDeclaration::Element* yy190;
  SortDeclaration* yy195;
  TermList* yy197;
  Object* yy208;
  MacroSymbol::ArgumentList* yy220;
  NumberRange const* yy221;
  Formula* yy223;
  Statement* yy230;
  SortDeclaration::ElementList* yy245;
  AtomicFormula* yy262;
  ConstantSymbol::SortList* yy305;
  LuaTerm* yy313;
  SortSymbol* yy315;
  Constant* yy327;
  NumberRange* yy331;
  QuantifierFormula* yy381;
  MacroSymbol* yy403;
  TokenList* yy408;
  ConstantSymbol::Type::type yy409;
  UNUSED yy423;
  QueryData yy431;
  Number* yy458;
  IdentifierDeclList* yy484;
  ObjectDeclaration::ElementList* yy485;
  VariableDeclaration* yy487;
  int yy491;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define lemon_parserARG_SDECL  BCParser* parser						;
#define lemon_parserARG_PDECL , BCParser* parser						
#define lemon_parserARG_FETCH  BCParser* parser						 = yypParser->parser						
#define lemon_parserARG_STORE yypParser->parser						 = parser						
#define YYNSTATE 726
#define YYNRULE 374
#define YYERRORSYMBOL 136
#define YYERRSYMDT yy491
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
#define YY_ACTTAB_COUNT (2816)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   725,  568,  292,  724,  723,  722,  721,  720,  719,  718,
 /*    10 */   717,  716,  715,  714,  713,  712,  711,  710,  566,  683,
 /*    20 */   271,  709,  702,  708,  707,  701,  168,  440,  388,  569,
 /*    30 */   570,  461,  268,  272,  439,  438,  107,  105,  104,  646,
 /*    40 */   346,  583,  429,  683,  271,  709,  702,  395,  707,  701,
 /*    50 */   529,  441,  195,  197,  645,  644,  267,  272,  136,  134,
 /*    60 */   133,  278,  639,  636,  635,  634,  633,  683,  271,  709,
 /*    70 */   702,  708,  707,  701,  726,   24,  396,   55,   31,   30,
 /*    80 */   267,  272,   32,  372,  527,  294,  639,  636,  635,  634,
 /*    90 */   633,  506,  505,  504,  503,  502,  501,  500,  499,  498,
 /*   100 */   497,  496,  495,  494,  493,  492,  491,  490,  489,  682,
 /*   110 */   463,  704,  192,  464,  681,  473,  472,  469,  468,  471,
 /*   120 */   470,  682,  463,  165,  166,  464,  681,  663,  657,  709,
 /*   130 */   702,  708,  707,  701,  571,  572,   10,  397,  319,   54,
 /*   140 */    38,  112,    8,    9,  703,  158,  190,  214,   53,  662,
 /*   150 */   463,  188,  208,  464,  661,   52,  675,  674,  676,    7,
 /*   160 */   417,  416,    6,  650,   37,  200,  418,  261,  675,  674,
 /*   170 */   419,  642,   40,  643,  112,  251,  116,  273,  151,  425,
 /*   180 */   115,  132,  254,  631,  260,  632,   39,  170,   66,  117,
 /*   190 */     4,   32,   23,  189,  187,  186,  260,  461,  682,  463,
 /*   200 */   544,  292,  464,  681,  683,  342,  709,  702,  708,  707,
 /*   210 */   701,  658,  594,  659,  113,  432,  529,  296,  343,   98,
 /*   220 */   139,  390,  550,  389,   65,   20,   21,  467,  262,  682,
 /*   230 */   463,   98,  245,  464,  681,  382,  542,  381,  543,  466,
 /*   240 */   700,  465,  699,  157,  461,  675,  674,  676,  585,  371,
 /*   250 */   527,  466,  700,  465,  137,   28,   27,   31,   30,  135,
 /*   260 */   631,   32,  632,  529,  170,   64,  574,    4,  174,   35,
 /*   270 */    45,   44,  112,  260,   46,  244,  675,  674,  676,  466,
 /*   280 */   700,  465,  162,  462,  175,  146,    2,  154,  160,  161,
 /*   290 */   939,  631,  576,  632,  939,  170,  575,  530,    4,  533,
 /*   300 */    23,  156,   33,   34,  260,  618,  463,   63,   98,  464,
 /*   310 */   617,  111,  109,  107,  105,  104,  422,  423,  683,  671,
 /*   320 */   709,  702,  708,  707,  701,  698,  165,  166,  466,  700,
 /*   330 */   465,  673,  343,   20,   21,   42,   41,   45,   44,   98,
 /*   340 */   664,   46,  243,  683,  271,  709,  702,  708,  707,  701,
 /*   350 */    51,  549,  611,  610,  612,  112,  264,  272,  583,  466,
 /*   360 */   700,  465,  521,  636,  635,  634,  633,  601,  428,  602,
 /*   370 */   141,  150,  287,   36,  583,  567,   49,  682,  463,   18,
 /*   380 */    17,  464,  681,   19,  697,  705,  706,  709,  702,  708,
 /*   390 */   707,  701,  401,  345,  453,  564,  563,  562,  561,  377,
 /*   400 */   373,  427,  153,  433,  260,  452,  369,  510,  509,   47,
 /*   410 */    48,  560,   11,  558,  621,  119,  647,  620,  557,   92,
 /*   420 */    91,   90,   89,   88,  675,  674,  676,  622,  623,  192,
 /*   430 */   556,  554,  555,  559,  220,  466,  700,  465,   74,  642,
 /*   440 */    46,  643,  379,  536,  378,  192,  151,  630,  115,  628,
 /*   450 */   627,  531,  260,    5,  553,   57,  680,   26,  112,  466,
 /*   460 */    36,  694,   58,  190,   22,   73,   72,   71,  188,   70,
 /*   470 */    69,   68,   67,  598,   29,   28,   27,   31,   30,  190,
 /*   480 */   159,   32,  113,   56,  188,  544,  292,   98,  195,  103,
 /*   490 */   102,  101,  100,   99,  111,  109,  107,  105,  104,  683,
 /*   500 */   271,  709,  702,  395,  707,  701,  454,  466,  700,  465,
 /*   510 */   696,  463,  267,  272,  464,  695,  597,  280,  639,  636,
 /*   520 */   635,  634,  633,  546,  596,  111,  109,  107,  105,  104,
 /*   530 */   424,   25,  396,  683,  271,  709,  702,  708,  707,  701,
 /*   540 */   541,   29,   28,   27,   31,   30,  264,  272,   32,  262,
 /*   550 */   512,  511,  521,  636,  635,  634,  633,  690,  689,  691,
 /*   560 */   173,  454,  286,  595,  683,  271,  709,  702,  708,  707,
 /*   570 */   701,  648,  692,  442,  693,  592,  461,  265,  272,  262,
 /*   580 */   169,  110,  276,  639,  636,  635,  634,  633,  683,  270,
 /*   590 */   709,  702,  708,  707,  701,  459,  587,  514,  513,  172,
 /*   600 */   396,  640,  272,  453,  462,  700,  625,  639,  636,  635,
 /*   610 */   634,  633,  589,  590,  195,  108,   15,   14,   18,   17,
 /*   620 */   106,  437,   19,  641,  149,  683,  271,  709,  702,  708,
 /*   630 */   707,  701,  568,  292,  462,  466,  443,  219,  267,  272,
 /*   640 */   466,  700,  465,  638,  639,  636,  635,  634,  633,  683,
 /*   650 */   271,  709,  702,  708,  707,  701,  568,  292,  387,  388,
 /*   660 */   569,  570,  267,  272,  436,  947,  198,  637,  639,  636,
 /*   670 */   635,  634,  633,  683,  271,  709,  702,  708,  707,  701,
 /*   680 */   947,  947,  434,  388,  569,  570,  267,  272,  393,  586,
 /*   690 */   466,  456,  639,  636,  635,  634,  633,  535, 1044,  145,
 /*   700 */   947,  947,  683,  271,  709,  702,  708,  707,  701,  568,
 /*   710 */   292,  565,  262,  947,  526,  267,  272,  167, 1044,  947,
 /*   720 */   455,  639,  636,  635,  634,  633,  683,  271,  709,  702,
 /*   730 */   708,  707,  701,  147,   62,  385,  388,  569,  570,  267,
 /*   740 */   272,  148,  431,  159,  313,  639,  636,  635,  634,  633,
 /*   750 */   683,  271,  709,  702,  708,  707,  701,  552,   29,   28,
 /*   760 */    27,   31,   30,  267,  272,   32,  515,  462,  357,  639,
 /*   770 */   636,  635,  634,  633,  584,  389,  683,  271,  709,  702,
 /*   780 */   708,  707,  701,  568,  292,  551,    3,  148,  144,  267,
 /*   790 */   272,  540,  378,  143,  356,  639,  636,  635,  634,  633,
 /*   800 */   683,  271,  709,  702,  708,  707,  701,  538,  386,  435,
 /*   810 */   388,  569,  570,  267,  272,  148,  142,  461,  281,  639,
 /*   820 */   636,  635,  634,  633,  683,  271,  709,  702,  708,  707,
 /*   830 */   701,  426,   61,  196,  257,  545,  460,  267,  272,  628,
 /*   840 */   627,  528,  279,  639,  636,  635,  634,  633,  683,  271,
 /*   850 */   709,  702,  708,  707,  701,  534,   29,   28,   27,   31,
 /*   860 */    30,  267,  272,   32,  524,  148,  277,  639,  636,  635,
 /*   870 */   634,  633,  683,  271,  709,  702,  708,  707,  701,  185,
 /*   880 */    43,   42,   41,   45,   44,  263,  272,   46,  624,   19,
 /*   890 */   461,  521,  636,  635,  634,  633,  193,  191,  189,  187,
 /*   900 */   186,  274,  683,  271,  709,  702,  708,  707,  701,  458,
 /*   910 */    29,   28,   27,   31,   30,  264,  272,   32,  599,  391,
 /*   920 */   588,  521,  636,  635,  634,  633,   97,   96,   95,   94,
 /*   930 */    93,  288,  683,  270,  709,  702,  708,  707,  701,  525,
 /*   940 */    43,   42,   41,   45,   44,  522,  272,   46,  532,  376,
 /*   950 */   427,  521,  636,  635,  634,  633,  140,  138,  136,  134,
 /*   960 */   133,  518,  683,  271,  709,  702,  708,  707,  701,  193,
 /*   970 */   191,  189,  187,  186,  184,  264,  272,   60,   12,  375,
 /*   980 */   427,  521,  636,  635,  634,  633,  374,  427,  349,  427,
 /*   990 */   163,  520,  368,  683,  271,  709,  702,  708,  707,  701,
 /*  1000 */   508,   29,   28,   27,   31,   30,  264,  272,   32,  517,
 /*  1010 */   516,  292,  521,  636,  635,  634,  633,  507,  164, 1101,
 /*  1020 */     1,  488,  519,  683,  271,  709,  702,  708,  707,  701,
 /*  1030 */   487,   16,   15,   14,   18,   17,  264,  272,   19,  486,
 /*  1040 */   485,   36,  521,  636,  635,  634,  633,   29,   28,   27,
 /*  1050 */    31,   30,  421,  370,   32,  484,  483,  683,  271,  709,
 /*  1060 */   702,  708,  707,  701,  482,  481,  462,  544,  292,  479,
 /*  1070 */   264,  272,  478,  477,  118,  476,  521,  636,  635,  634,
 /*  1080 */   633,  475,  651,  649,  647,  700,  420,  683,  271,  709,
 /*  1090 */   702,  708,  707,  701,  453,   29,   28,   27,   31,   30,
 /*  1100 */   264,  272,   32,  548,  381,  543,  521,  636,  635,  634,
 /*  1110 */   633,  466,  256,   36,  641,  155,  298,  683,  271,  709,
 /*  1120 */   702,  708,  707,  701,  192,  194,  629,  626,  402,  255,
 /*  1130 */   264,  272,   50,  449,  448,  252,  521,  636,  635,  634,
 /*  1140 */   633,  193,  191,  189,  187,  186,  348,  683,  271,  709,
 /*  1150 */   702,  708,  707,  701,  593,  447,  249,  392,  190,  446,
 /*  1160 */   264,  272,   59,  188,  247,  445,  521,  636,  635,  634,
 /*  1170 */   633,  444,  547,  246,  567,  539,  347,   13,  683,  271,
 /*  1180 */   709,  702,  708,  707,  701,   12,   29,   28,   27,   31,
 /*  1190 */    30,  267,  272,   32,  241,  415,  285,  639,  636,  635,
 /*  1200 */   634,  633,  683,  271,  709,  702,  708,  707,  701,  568,
 /*  1210 */   292,  240,  239,  236,  480,  267,  272,  582,  237,  234,
 /*  1220 */   284,  639,  636,  635,  634,  633,  683,  271,  709,  702,
 /*  1230 */   708,  707,  701,  235,  384,  435,  388,  569,  570,  267,
 /*  1240 */   272,  413,  232,  231,  181,  639,  636,  635,  634,  633,
 /*  1250 */   193,  191,  189,  187,  186,  230,  683,  271,  709,  702,
 /*  1260 */   708,  707,  701,  412,   16,   15,   14,   18,   17,  267,
 /*  1270 */   272,   19,  411,  229,  180,  639,  636,  635,  634,  633,
 /*  1280 */   227,  225,  213,  410,  223,  221,  683,  271,  709,  702,
 /*  1290 */   708,  707,  701,  409,  408,  218,  207,  212,  217,  267,
 /*  1300 */   272,  216,  215,  211,  179,  639,  636,  635,  634,  633,
 /*  1310 */   407,  210,  209,  204,  206,  406,  683,  271,  709,  702,
 /*  1320 */   708,  707,  701,  568,  292,  203,  205,  199,  201,  267,
 /*  1330 */   272,  405,  404,  403,  178,  639,  636,  635,  634,  633,
 /*  1340 */   683,  271,  709,  702,  708,  707,  701,  242,  383,  435,
 /*  1350 */   388,  569,  570,  267,  272,  591,  258,  573,  177,  639,
 /*  1360 */   636,  635,  634,  633,  683,  271,  709,  702,  708,  707,
 /*  1370 */   701,  568,  292,  299,  350,  291,    5,  267,  272,  680,
 /*  1380 */   351,  228,  176,  639,  636,  635,  634,  633,   73,   72,
 /*  1390 */    71,    5,   70,   69,   68,   67,  380,  435,  388,  569,
 /*  1400 */   570,  581,   22,   73,   72,   71,  577,   70,   69,   68,
 /*  1410 */    67,  580,  103,  102,  101,  100,   99,  697,  705,  706,
 /*  1420 */   709,  702,  708,  707,  701,  400,  345,  103,  102,  101,
 /*  1430 */   100,   99,  683,  271,  709,  702,  708,  707,  701,  682,
 /*  1440 */   463,  579,  578,  464,  681,  268,  272,  290,  289,  253,
 /*  1450 */   250,  414,  646,  641,  226,  224,  683,  271,  709,  702,
 /*  1460 */   708,  707,  701,  222,  202,  430,  259,  645,  644,  268,
 /*  1470 */   272,  537,  583,  567, 1102, 1102,  646,  641,  663,  657,
 /*  1480 */   709,  702,  708,  707,  701, 1102,  675,  674,  676,  320,
 /*  1490 */   248,  645,  644,  683,  271,  709,  702,  708,  707,  701,
 /*  1500 */   523, 1102, 1102, 1102, 1102, 1102,  268,  272, 1102, 1102,
 /*  1510 */   115, 1102, 1102,  646,  641, 1102, 1102,  683,  271,  709,
 /*  1520 */   702,  708,  707,  701, 1102, 1102, 1102,  238,  645,  644,
 /*  1530 */   268,  272,  193,  191,  189,  187,  186,  646,  641, 1102,
 /*  1540 */  1102, 1102, 1102,  192,  114, 1102, 1102,  152, 1102,   98,
 /*  1550 */  1102,  233,  645,  644, 1102,   50, 1102, 1102, 1102,   73,
 /*  1560 */    72,   71,    5,   70,   69,   68,   67, 1102, 1102,  466,
 /*  1570 */   700,  465, 1102, 1102,   73,   72,   71,  190,   70,   69,
 /*  1580 */    68,   67,  188,  103,  102,  101,  100,   99,   29,   28,
 /*  1590 */    27,   31,   30, 1102,  616,   32, 1102, 1102,  103,  102,
 /*  1600 */   101,  100,   99,  126,  125,  124, 1102,  123,  122,  121,
 /*  1610 */   120,   87,   86,   85, 1102,   84,   83,   82,   81, 1102,
 /*  1620 */  1102, 1102, 1102, 1102, 1102, 1102, 1102,  131,  130,  129,
 /*  1630 */   128,  127, 1102, 1102, 1102,   92,   91,   90,   89,   88,
 /*  1640 */  1102, 1102,   74,   80,   79, 1102,   78,   77,   76,   75,
 /*  1650 */  1102, 1102,  793,  793,  793, 1102,  793,  793,  793,  793,
 /*  1660 */  1102, 1102, 1102, 1102, 1102, 1102,   97,   96,   95,   94,
 /*  1670 */    93, 1102, 1102, 1102, 1102, 1102,  793,  793,  793,  793,
 /*  1680 */   793,  126,  125,  124, 1102,  123,  122,  121,  120, 1102,
 /*  1690 */   680, 1102,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  1700 */   399,  345, 1102, 1102, 1102,  131,  130,  129,  128,  127,
 /*  1710 */  1102, 1102, 1102, 1102, 1102, 1102,  619,  609,  709,  702,
 /*  1720 */   708,  707,  701,  103,  102,  101,  100,   99,  474, 1102,
 /*  1730 */  1102,  269,  619,  609,  709,  702,  708,  707,  701, 1102,
 /*  1740 */  1102,  293,  606,  603, 1102, 1102, 1102,  266, 1102, 1102,
 /*  1750 */   619,  609,  709,  702,  708,  707,  701,  275,  606,  603,
 /*  1760 */   140,  138,  136,  134,  133,  608,  619,  609,  709,  702,
 /*  1770 */   708,  707,  701, 1102, 1102,  600,  606,  603, 1102, 1102,
 /*  1780 */  1102,  269, 1102,  619,  609,  709,  702,  708,  707,  701,
 /*  1790 */  1102,  605,  606,  603, 1102, 1102, 1102, 1102,  269,  619,
 /*  1800 */   609,  709,  702,  708,  707,  701, 1102, 1102,  604,  606,
 /*  1810 */   603, 1102, 1102, 1102,  269,  619,  609,  709,  702,  708,
 /*  1820 */   707,  701, 1102, 1102,  451,  606,  603, 1102, 1102, 1102,
 /*  1830 */   269, 1102,  619,  609,  709,  702,  708,  707,  701, 1102,
 /*  1840 */   450,  606,  603, 1102, 1102, 1102, 1102,  269, 1102,  663,
 /*  1850 */   657,  709,  702,  708,  707,  701, 1102,  300,  606,  603,
 /*  1860 */   358,  619,  609,  709,  702,  708,  707,  701, 1102,  648,
 /*  1870 */  1102, 1102, 1102, 1102,  461, 1102,  269,  619,  609,  709,
 /*  1880 */   702,  708,  707,  701, 1102, 1102,  353,  606,  603, 1102,
 /*  1890 */  1102, 1102,  269,  457, 1102, 1102, 1102,  171,  396, 1102,
 /*  1900 */  1102, 1102,  352,  606,  603,  697,  705,  706,  709,  702,
 /*  1910 */   708,  707,  701,  398,  345, 1102, 1102, 1102, 1102, 1102,
 /*  1920 */   697,  705,  706,  709,  702,  708,  707,  701,  394,  345,
 /*  1930 */  1102,  683,  282,  709,  702,  708,  707,  701, 1102, 1102,
 /*  1940 */  1102, 1102, 1102, 1102,  673,  343,  697,  705,  706,  709,
 /*  1950 */   702,  708,  707,  701, 1102,  344,  697,  705,  706,  709,
 /*  1960 */   702,  708,  707,  701, 1102,  366, 1102, 1102, 1102,  697,
 /*  1970 */   705,  706,  709,  702,  708,  707,  701, 1102,  297,  697,
 /*  1980 */   705,  706,  709,  702,  708,  707,  701, 1102,  367,  697,
 /*  1990 */   705,  706,  709,  702,  708,  707,  701, 1102,  688, 1102,
 /*  2000 */  1102, 1102,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2010 */  1102,  684,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2020 */  1102,  687,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2030 */  1102,  686,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2040 */  1102,  685,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2050 */  1102,  365, 1102, 1102, 1102, 1102, 1102,  697,  705,  706,
 /*  2060 */   709,  702,  708,  707,  701, 1102,  364,  697,  705,  706,
 /*  2070 */   709,  702,  708,  707,  701, 1102,  679,  697,  705,  706,
 /*  2080 */   709,  702,  708,  707,  701, 1102,  678,  697,  705,  706,
 /*  2090 */   709,  702,  708,  707,  701, 1102,  677, 1102, 1102,  697,
 /*  2100 */   705,  706,  709,  702,  708,  707,  701, 1102,  672, 1102,
 /*  2110 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  363,
 /*  2120 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  362,
 /*  2130 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  670,
 /*  2140 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  669,
 /*  2150 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  668,
 /*  2160 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  361,
 /*  2170 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  360,
 /*  2180 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  667,
 /*  2190 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  666,
 /*  2200 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  665,
 /*  2210 */  1102, 1102,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2220 */  1102,  341,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2230 */  1102,  340,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2240 */  1102,  339,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2250 */  1102,  338, 1102, 1102,  697,  705,  706,  709,  702,  708,
 /*  2260 */   707,  701, 1102,  337, 1102,  697,  705,  706,  709,  702,
 /*  2270 */   708,  707,  701, 1102,  336,  697,  705,  706,  709,  702,
 /*  2280 */   708,  707,  701, 1102,  335,  697,  705,  706,  709,  702,
 /*  2290 */   708,  707,  701, 1102,  334,  697,  705,  706,  709,  702,
 /*  2300 */   708,  707,  701, 1102,  333,  697,  705,  706,  709,  702,
 /*  2310 */   708,  707,  701, 1102,  332,  697,  705,  706,  709,  702,
 /*  2320 */   708,  707,  701, 1102,  331,  697,  705,  706,  709,  702,
 /*  2330 */   708,  707,  701, 1102,  330,  697,  705,  706,  709,  702,
 /*  2340 */   708,  707,  701, 1102,  329,  697,  705,  706,  709,  702,
 /*  2350 */   708,  707,  701, 1102,  328,  697,  705,  706,  709,  702,
 /*  2360 */   708,  707,  701, 1102,  327, 1102, 1102,  697,  705,  706,
 /*  2370 */   709,  702,  708,  707,  701, 1102,  326,  697,  705,  706,
 /*  2380 */   709,  702,  708,  707,  701, 1102,  325,  697,  705,  706,
 /*  2390 */   709,  702,  708,  707,  701, 1102,  324,  697,  705,  706,
 /*  2400 */   709,  702,  708,  707,  701, 1102,  323, 1102, 1102,  697,
 /*  2410 */   705,  706,  709,  702,  708,  707,  701, 1102,  322, 1102,
 /*  2420 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  321,
 /*  2430 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  317,
 /*  2440 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  316,
 /*  2450 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  315,
 /*  2460 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  314,
 /*  2470 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  312,
 /*  2480 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  311,
 /*  2490 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  310,
 /*  2500 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  309,
 /*  2510 */   697,  705,  706,  709,  702,  708,  707,  701, 1102,  308,
 /*  2520 */  1102, 1102,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2530 */  1102,  183,  697,  705,  706,  709,  702,  708,  707,  701,
 /*  2540 */  1102,  182,  663,  657,  709,  702,  708,  707,  701, 1102,
 /*  2550 */  1102, 1102, 1102,  295,  663,  657,  709,  702,  708,  707,
 /*  2560 */   701, 1102, 1102, 1102, 1102,  359,  663,  657,  709,  702,
 /*  2570 */   708,  707,  701, 1102, 1102, 1102, 1102,  656,  663,  657,
 /*  2580 */   709,  702,  708,  707,  701, 1102, 1102, 1102, 1102,  652,
 /*  2590 */   663,  657,  709,  702,  708,  707,  701, 1102, 1102, 1102,
 /*  2600 */  1102,  655,  663,  657,  709,  702,  708,  707,  701, 1102,
 /*  2610 */  1102, 1102, 1102,  654, 1102,  663,  657,  709,  702,  708,
 /*  2620 */   707,  701, 1102, 1102, 1102, 1102,  653,  663,  657,  709,
 /*  2630 */   702,  708,  707,  701, 1102, 1102, 1102, 1102,  318,  663,
 /*  2640 */   657,  709,  702,  708,  707,  701, 1102, 1102, 1102, 1102,
 /*  2650 */   355,  663,  657,  709,  702,  708,  707,  701, 1102, 1102,
 /*  2660 */  1102, 1102,  354,  663,  657,  709,  702,  708,  707,  701,
 /*  2670 */  1102, 1102, 1102, 1102,  615,  663,  657,  709,  702,  708,
 /*  2680 */   707,  701, 1102, 1102, 1102, 1102,  614,  663,  657,  709,
 /*  2690 */   702,  708,  707,  701, 1102, 1102, 1102, 1102,  613,  663,
 /*  2700 */   657,  709,  702,  708,  707,  701, 1102, 1102, 1102, 1102,
 /*  2710 */   307,  663,  657,  709,  702,  708,  707,  701, 1102, 1102,
 /*  2720 */  1102, 1102,  306,  663,  657,  709,  702,  708,  707,  701,
 /*  2730 */  1102, 1102, 1102, 1102,  305,  663,  657,  709,  702,  708,
 /*  2740 */   707,  701, 1102, 1102, 1102, 1102,  304,  663,  657,  709,
 /*  2750 */   702,  708,  707,  701, 1102, 1102, 1102, 1102,  303,  663,
 /*  2760 */   657,  709,  702,  708,  707,  701, 1102, 1102, 1102, 1102,
 /*  2770 */   302,  663,  657,  709,  702,  708,  707,  701,  660, 1102,
 /*  2780 */  1102, 1102,  301,  663,  657,  709,  702,  708,  707,  701,
 /*  2790 */  1102, 1102, 1102, 1102,  607,  663,  657,  709,  702,  708,
 /*  2800 */   707,  701, 1102, 1102, 1102, 1102,  283, 1102, 1102, 1102,
 /*  2810 */  1102,  140,  138,  136,  134,  133,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   136,  171,  172,  139,  140,  141,  142,  143,  144,  145,
 /*    10 */   146,  147,  148,  149,  150,  151,  152,  153,   73,  155,
 /*    20 */   156,  157,  158,  159,  160,  161,   72,  197,  198,  199,
 /*    30 */   200,  164,  168,  169,  204,  205,  108,  109,  110,  175,
 /*    40 */   176,    1,    2,  155,  156,  157,  158,  159,  160,  161,
 /*    50 */   183,   97,  107,  189,  190,  191,  168,  169,  108,  109,
 /*    60 */   110,  173,  174,  175,  176,  177,  178,  155,  156,  157,
 /*    70 */   158,  159,  160,  161,    0,  187,  188,   72,   98,   99,
 /*    80 */   168,  169,  102,  216,  217,  173,  174,  175,  176,  177,
 /*    90 */   178,  227,  228,  229,  230,  231,  232,  233,  234,  235,
 /*   100 */   236,  237,  238,  239,  240,  241,  242,  243,  244,    1,
 /*   110 */     2,   73,   72,    5,    6,    7,    8,    9,   10,   11,
 /*   120 */    12,    1,    2,  100,  101,    5,    6,  155,  156,  157,
 /*   130 */   158,  159,  160,  161,    1,    2,   28,  165,  166,   72,
 /*   140 */    32,  103,   34,   35,   73,   37,  106,   39,   72,    1,
 /*   150 */     2,  111,   44,    5,    6,   72,   48,   49,   50,   51,
 /*   160 */    52,   53,   54,   99,   56,   57,   46,  103,   48,   49,
 /*   170 */    50,   63,   33,   65,  103,   36,   72,   69,   70,   27,
 /*   180 */    72,   82,   43,   63,   76,   65,   47,   67,   71,   77,
 /*   190 */    70,  102,   72,  108,  109,  110,   76,  164,    1,    2,
 /*   200 */   171,  172,    5,    6,  155,  156,  157,  158,  159,  160,
 /*   210 */   161,   63,   74,   65,  106,  103,  183,  168,  169,  111,
 /*   220 */    72,  201,  202,  203,   71,  105,  106,  119,   76,    1,
 /*   230 */     2,  111,   94,    5,    6,  206,  207,  208,  209,  131,
 /*   240 */   132,  133,   73,  135,  164,   48,   49,   50,   74,  216,
 /*   250 */   217,  131,  132,  133,  106,   96,   97,   98,   99,  111,
 /*   260 */    63,  102,   65,  183,   67,   71,  133,   70,   94,   72,
 /*   270 */    98,   99,  103,   76,  102,   77,   48,   49,   50,  131,
 /*   280 */   132,  133,   14,  131,   16,   17,   18,   19,   20,   21,
 /*   290 */    99,   63,  108,   65,  103,   67,  112,  217,   70,   73,
 /*   300 */    72,  103,  105,  106,   76,    1,    2,   71,  111,    5,
 /*   310 */     6,  106,  107,  108,  109,  110,   48,   49,  155,  156,
 /*   320 */   157,  158,  159,  160,  161,   73,  100,  101,  131,  132,
 /*   330 */   133,  168,  169,  105,  106,   96,   97,   98,   99,  111,
 /*   340 */    73,  102,   77,  155,  156,  157,  158,  159,  160,  161,
 /*   350 */    72,   74,   48,   49,   50,  103,  168,  169,    1,  131,
 /*   360 */   132,  133,  174,  175,  176,  177,  178,   63,  103,   65,
 /*   370 */   103,   94,  184,   41,    1,    2,   72,    1,    2,   98,
 /*   380 */    99,    5,    6,  102,  154,  155,  156,  157,  158,  159,
 /*   390 */   160,  161,  162,  163,  156,   22,   23,   24,   25,  213,
 /*   400 */   214,  215,  134,   30,   76,  167,  218,  219,  220,  105,
 /*   410 */   106,   38,   80,   40,  176,  111,  156,  179,   45,  106,
 /*   420 */   107,  108,  109,  110,   48,   49,   50,  167,   73,   72,
 /*   430 */    57,   58,   59,   60,  106,  131,  132,  133,   82,   63,
 /*   440 */   102,   65,  210,  211,  212,   72,   70,   68,   72,   91,
 /*   450 */    92,   74,   76,   70,   73,   31,   73,   99,  103,  131,
 /*   460 */    41,   73,   82,  106,   81,   82,   83,   84,  111,   86,
 /*   470 */    87,   88,   89,   74,   95,   96,   97,   98,   99,  106,
 /*   480 */   103,  102,  106,   31,  111,  171,  172,  111,  107,  106,
 /*   490 */   107,  108,  109,  110,  106,  107,  108,  109,  110,  155,
 /*   500 */   156,  157,  158,  159,  160,  161,    2,  131,  132,  133,
 /*   510 */     1,    2,  168,  169,    5,    6,   74,  173,  174,  175,
 /*   520 */   176,  177,  178,  209,   74,  106,  107,  108,  109,  110,
 /*   530 */    27,  187,  188,  155,  156,  157,  158,  159,  160,  161,
 /*   540 */    74,   95,   96,   97,   98,   99,  168,  169,  102,   76,
 /*   550 */     1,    2,  174,  175,  176,  177,  178,   48,   49,   50,
 /*   560 */    94,    2,  184,   74,  155,  156,  157,  158,  159,  160,
 /*   570 */   161,  159,   63,   97,   65,   75,  164,  168,  169,   76,
 /*   580 */    76,   72,  173,  174,  175,  176,  177,  178,  155,  156,
 /*   590 */   157,  158,  159,  160,  161,  183,   75,  219,  220,  187,
 /*   600 */   188,  168,  169,  156,  131,  132,  173,  174,  175,  176,
 /*   610 */   177,  178,    3,    4,  107,  106,   96,   97,   98,   99,
 /*   620 */   111,   55,  102,  176,   72,  155,  156,  157,  158,  159,
 /*   630 */   160,  161,  171,  172,  131,  131,   73,  190,  168,  169,
 /*   640 */   131,  132,  133,  173,  174,  175,  176,  177,  178,  155,
 /*   650 */   156,  157,  158,  159,  160,  161,  171,  172,  197,  198,
 /*   660 */   199,  200,  168,  169,    2,   26,  103,  173,  174,  175,
 /*   670 */   176,  177,  178,  155,  156,  157,  158,  159,  160,  161,
 /*   680 */    41,   42,  197,  198,  199,  200,  168,  169,  192,  193,
 /*   690 */   131,  173,  174,  175,  176,  177,  178,   74,   74,   72,
 /*   700 */    61,   62,  155,  156,  157,  158,  159,  160,  161,  171,
 /*   710 */   172,   73,   76,   74,   74,  168,  169,   94,   94,   80,
 /*   720 */   173,  174,  175,  176,  177,  178,  155,  156,  157,  158,
 /*   730 */   159,  160,  161,   72,   71,  197,  198,  199,  200,  168,
 /*   740 */   169,  103,    2,  103,  173,  174,  175,  176,  177,  178,
 /*   750 */   155,  156,  157,  158,  159,  160,  161,   73,   95,   96,
 /*   760 */    97,   98,   99,  168,  169,  102,   74,  131,  173,  174,
 /*   770 */   175,  176,  177,  178,  202,  203,  155,  156,  157,  158,
 /*   780 */   159,  160,  161,  171,  172,   73,   94,  103,   72,  168,
 /*   790 */   169,  211,  212,   72,  173,  174,  175,  176,  177,  178,
 /*   800 */   155,  156,  157,  158,  159,  160,  161,    2,  196,  197,
 /*   810 */   198,  199,  200,  168,  169,  103,   72,  164,  173,  174,
 /*   820 */   175,  176,  177,  178,  155,  156,  157,  158,  159,  160,
 /*   830 */   161,  103,   71,  185,  186,   73,  183,  168,  169,   91,
 /*   840 */    92,   74,  173,  174,  175,  176,  177,  178,  155,  156,
 /*   850 */   157,  158,  159,  160,  161,    2,   95,   96,   97,   98,
 /*   860 */    99,  168,  169,  102,   74,  103,  173,  174,  175,  176,
 /*   870 */   177,  178,  155,  156,  157,  158,  159,  160,  161,   82,
 /*   880 */    95,   96,   97,   98,   99,  168,  169,  102,   73,  102,
 /*   890 */   164,  174,  175,  176,  177,  178,  106,  107,  108,  109,
 /*   900 */   110,  184,  155,  156,  157,  158,  159,  160,  161,  183,
 /*   910 */    95,   96,   97,   98,   99,  168,  169,  102,   73,  194,
 /*   920 */   195,  174,  175,  176,  177,  178,  106,  107,  108,  109,
 /*   930 */   110,  184,  155,  156,  157,  158,  159,  160,  161,   74,
 /*   940 */    95,   96,   97,   98,   99,  168,  169,  102,    2,  214,
 /*   950 */   215,  174,  175,  176,  177,  178,  106,  107,  108,  109,
 /*   960 */   110,  184,  155,  156,  157,  158,  159,  160,  161,  106,
 /*   970 */   107,  108,  109,  110,   82,  168,  169,   71,   26,  214,
 /*   980 */   215,  174,  175,  176,  177,  178,  214,  215,  214,  215,
 /*   990 */    77,  184,   77,  155,  156,  157,  158,  159,  160,  161,
 /*  1000 */    74,   95,   96,   97,   98,   99,  168,  169,  102,   73,
 /*  1010 */   171,  172,  174,  175,  176,  177,  178,   74,   72,  137,
 /*  1020 */   138,   74,  184,  155,  156,  157,  158,  159,  160,  161,
 /*  1030 */    74,   95,   96,   97,   98,   99,  168,  169,  102,   74,
 /*  1040 */    74,   41,  174,  175,  176,  177,  178,   95,   96,   97,
 /*  1050 */    98,   99,  184,    1,  102,   74,   74,  155,  156,  157,
 /*  1060 */   158,  159,  160,  161,   74,   74,  131,  171,  172,   74,
 /*  1070 */   168,  169,   74,   74,   66,   74,  174,  175,  176,  177,
 /*  1080 */   178,   74,  164,  159,  156,  132,  184,  155,  156,  157,
 /*  1090 */   158,  159,  160,  161,  156,   95,   96,   97,   98,   99,
 /*  1100 */   168,  169,  102,  207,  208,  209,  174,  175,  176,  177,
 /*  1110 */   178,  131,  222,   41,  176,   61,  184,  155,  156,  157,
 /*  1120 */   158,  159,  160,  161,   72,   90,  159,  159,  190,  225,
 /*  1130 */   168,  169,   62,  226,  156,  225,  174,  175,  176,  177,
 /*  1140 */   178,  106,  107,  108,  109,  110,  184,  155,  156,  157,
 /*  1150 */   158,  159,  160,  161,  193,  226,  225,    2,  106,  156,
 /*  1160 */   168,  169,   71,  111,  222,  226,  174,  175,  176,  177,
 /*  1170 */   178,  226,  200,  225,    2,  200,  184,   42,  155,  156,
 /*  1180 */   157,  158,  159,  160,  161,   26,   95,   96,   97,   98,
 /*  1190 */    99,  168,  169,  102,  224,  226,  173,  174,  175,  176,
 /*  1200 */   177,  178,  155,  156,  157,  158,  159,  160,  161,  171,
 /*  1210 */   172,  223,  225,  224,  156,  168,  169,   73,  222,  225,
 /*  1220 */   173,  174,  175,  176,  177,  178,  155,  156,  157,  158,
 /*  1230 */   159,  160,  161,  223,  196,  197,  198,  199,  200,  168,
 /*  1240 */   169,  226,  222,  224,  173,  174,  175,  176,  177,  178,
 /*  1250 */   106,  107,  108,  109,  110,  223,  155,  156,  157,  158,
 /*  1260 */   159,  160,  161,  226,   95,   96,   97,   98,   99,  168,
 /*  1270 */   169,  102,  226,  225,  173,  174,  175,  176,  177,  178,
 /*  1280 */   225,  225,  156,  226,  225,  225,  155,  156,  157,  158,
 /*  1290 */   159,  160,  161,  226,  226,  222,  156,  222,  224,  168,
 /*  1300 */   169,  223,  225,  224,  173,  174,  175,  176,  177,  178,
 /*  1310 */   226,  223,  225,  223,  222,  226,  155,  156,  157,  158,
 /*  1320 */   159,  160,  161,  171,  172,  225,  224,  156,  225,  168,
 /*  1330 */   169,  226,  226,  226,  173,  174,  175,  176,  177,  178,
 /*  1340 */   155,  156,  157,  158,  159,  160,  161,  222,  196,  197,
 /*  1350 */   198,  199,  200,  168,  169,  195,  186,  158,  173,  174,
 /*  1360 */   175,  176,  177,  178,  155,  156,  157,  158,  159,  160,
 /*  1370 */   161,  171,  172,  172,  172,  172,   70,  168,  169,   73,
 /*  1380 */   172,  223,  173,  174,  175,  176,  177,  178,   82,   83,
 /*  1390 */    84,   70,   86,   87,   88,   89,  196,  197,  198,  199,
 /*  1400 */   200,  172,   81,   82,   83,   84,  172,   86,   87,   88,
 /*  1410 */    89,  172,  106,  107,  108,  109,  110,  154,  155,  156,
 /*  1420 */   157,  158,  159,  160,  161,  162,  163,  106,  107,  108,
 /*  1430 */   109,  110,  155,  156,  157,  158,  159,  160,  161,    1,
 /*  1440 */     2,  172,  172,    5,    6,  168,  169,  172,  172,  222,
 /*  1450 */   222,  226,  175,  176,  223,  223,  155,  156,  157,  158,
 /*  1460 */   159,  160,  161,  223,  222,    2,  189,  190,  191,  168,
 /*  1470 */   169,    2,    1,    2,  245,  245,  175,  176,  155,  156,
 /*  1480 */   157,  158,  159,  160,  161,  245,   48,   49,   50,  166,
 /*  1490 */   189,  190,  191,  155,  156,  157,  158,  159,  160,  161,
 /*  1500 */    74,  245,  245,  245,  245,  245,  168,  169,  245,  245,
 /*  1510 */    72,  245,  245,  175,  176,  245,  245,  155,  156,  157,
 /*  1520 */   158,  159,  160,  161,  245,  245,  245,  189,  190,  191,
 /*  1530 */   168,  169,  106,  107,  108,  109,  110,  175,  176,  245,
 /*  1540 */   245,  245,  245,   72,  106,  245,  245,   70,  245,  111,
 /*  1550 */   245,  189,  190,  191,  245,   62,  245,  245,  245,   82,
 /*  1560 */    83,   84,   70,   86,   87,   88,   89,  245,  245,  131,
 /*  1570 */   132,  133,  245,  245,   82,   83,   84,  106,   86,   87,
 /*  1580 */    88,   89,  111,  106,  107,  108,  109,  110,   95,   96,
 /*  1590 */    97,   98,   99,  245,   73,  102,  245,  245,  106,  107,
 /*  1600 */   108,  109,  110,   82,   83,   84,  245,   86,   87,   88,
 /*  1610 */    89,   82,   83,   84,  245,   86,   87,   88,   89,  245,
 /*  1620 */   245,  245,  245,  245,  245,  245,  245,  106,  107,  108,
 /*  1630 */   109,  110,  245,  245,  245,  106,  107,  108,  109,  110,
 /*  1640 */   245,  245,   82,   83,   84,  245,   86,   87,   88,   89,
 /*  1650 */   245,  245,   82,   83,   84,  245,   86,   87,   88,   89,
 /*  1660 */   245,  245,  245,  245,  245,  245,  106,  107,  108,  109,
 /*  1670 */   110,  245,  245,  245,  245,  245,  106,  107,  108,  109,
 /*  1680 */   110,   82,   83,   84,  245,   86,   87,   88,   89,  245,
 /*  1690 */    73,  245,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  1700 */   162,  163,  245,  245,  245,  106,  107,  108,  109,  110,
 /*  1710 */   245,  245,  245,  245,  245,  245,  155,  156,  157,  158,
 /*  1720 */   159,  160,  161,  106,  107,  108,  109,  110,   74,  245,
 /*  1730 */   245,  170,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  1740 */   245,  180,  181,  182,  245,  245,  245,  170,  245,  245,
 /*  1750 */   155,  156,  157,  158,  159,  160,  161,  180,  181,  182,
 /*  1760 */   106,  107,  108,  109,  110,  170,  155,  156,  157,  158,
 /*  1770 */   159,  160,  161,  245,  245,  180,  181,  182,  245,  245,
 /*  1780 */   245,  170,  245,  155,  156,  157,  158,  159,  160,  161,
 /*  1790 */   245,  180,  181,  182,  245,  245,  245,  245,  170,  155,
 /*  1800 */   156,  157,  158,  159,  160,  161,  245,  245,  180,  181,
 /*  1810 */   182,  245,  245,  245,  170,  155,  156,  157,  158,  159,
 /*  1820 */   160,  161,  245,  245,  180,  181,  182,  245,  245,  245,
 /*  1830 */   170,  245,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  1840 */   180,  181,  182,  245,  245,  245,  245,  170,  245,  155,
 /*  1850 */   156,  157,  158,  159,  160,  161,  245,  180,  181,  182,
 /*  1860 */   166,  155,  156,  157,  158,  159,  160,  161,  245,  159,
 /*  1870 */   245,  245,  245,  245,  164,  245,  170,  155,  156,  157,
 /*  1880 */   158,  159,  160,  161,  245,  245,  180,  181,  182,  245,
 /*  1890 */   245,  245,  170,  183,  245,  245,  245,  187,  188,  245,
 /*  1900 */   245,  245,  180,  181,  182,  154,  155,  156,  157,  158,
 /*  1910 */   159,  160,  161,  162,  163,  245,  245,  245,  245,  245,
 /*  1920 */   154,  155,  156,  157,  158,  159,  160,  161,  162,  163,
 /*  1930 */   245,  155,  156,  157,  158,  159,  160,  161,  245,  245,
 /*  1940 */   245,  245,  245,  245,  168,  169,  154,  155,  156,  157,
 /*  1950 */   158,  159,  160,  161,  245,  163,  154,  155,  156,  157,
 /*  1960 */   158,  159,  160,  161,  245,  163,  245,  245,  245,  154,
 /*  1970 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  154,
 /*  1980 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  154,
 /*  1990 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  245,
 /*  2000 */   245,  245,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2010 */   245,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2020 */   245,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2030 */   245,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2040 */   245,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2050 */   245,  163,  245,  245,  245,  245,  245,  154,  155,  156,
 /*  2060 */   157,  158,  159,  160,  161,  245,  163,  154,  155,  156,
 /*  2070 */   157,  158,  159,  160,  161,  245,  163,  154,  155,  156,
 /*  2080 */   157,  158,  159,  160,  161,  245,  163,  154,  155,  156,
 /*  2090 */   157,  158,  159,  160,  161,  245,  163,  245,  245,  154,
 /*  2100 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  245,
 /*  2110 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2120 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2130 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2140 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2150 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2160 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2170 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2180 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2190 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2200 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2210 */   245,  245,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2220 */   245,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2230 */   245,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2240 */   245,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2250 */   245,  163,  245,  245,  154,  155,  156,  157,  158,  159,
 /*  2260 */   160,  161,  245,  163,  245,  154,  155,  156,  157,  158,
 /*  2270 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2280 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2290 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2300 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2310 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2320 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2330 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2340 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2350 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2360 */   159,  160,  161,  245,  163,  245,  245,  154,  155,  156,
 /*  2370 */   157,  158,  159,  160,  161,  245,  163,  154,  155,  156,
 /*  2380 */   157,  158,  159,  160,  161,  245,  163,  154,  155,  156,
 /*  2390 */   157,  158,  159,  160,  161,  245,  163,  154,  155,  156,
 /*  2400 */   157,  158,  159,  160,  161,  245,  163,  245,  245,  154,
 /*  2410 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  245,
 /*  2420 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2430 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2440 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2450 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2460 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2470 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2480 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2490 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2500 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2510 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2520 */   245,  245,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2530 */   245,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2540 */   245,  163,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2550 */   245,  245,  245,  166,  155,  156,  157,  158,  159,  160,
 /*  2560 */   161,  245,  245,  245,  245,  166,  155,  156,  157,  158,
 /*  2570 */   159,  160,  161,  245,  245,  245,  245,  166,  155,  156,
 /*  2580 */   157,  158,  159,  160,  161,  245,  245,  245,  245,  166,
 /*  2590 */   155,  156,  157,  158,  159,  160,  161,  245,  245,  245,
 /*  2600 */   245,  166,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2610 */   245,  245,  245,  166,  245,  155,  156,  157,  158,  159,
 /*  2620 */   160,  161,  245,  245,  245,  245,  166,  155,  156,  157,
 /*  2630 */   158,  159,  160,  161,  245,  245,  245,  245,  166,  155,
 /*  2640 */   156,  157,  158,  159,  160,  161,  245,  245,  245,  245,
 /*  2650 */   166,  155,  156,  157,  158,  159,  160,  161,  245,  245,
 /*  2660 */   245,  245,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2670 */   245,  245,  245,  245,  166,  155,  156,  157,  158,  159,
 /*  2680 */   160,  161,  245,  245,  245,  245,  166,  155,  156,  157,
 /*  2690 */   158,  159,  160,  161,  245,  245,  245,  245,  166,  155,
 /*  2700 */   156,  157,  158,  159,  160,  161,  245,  245,  245,  245,
 /*  2710 */   166,  155,  156,  157,  158,  159,  160,  161,  245,  245,
 /*  2720 */   245,  245,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2730 */   245,  245,  245,  245,  166,  155,  156,  157,  158,  159,
 /*  2740 */   160,  161,  245,  245,  245,  245,  166,  155,  156,  157,
 /*  2750 */   158,  159,  160,  161,  245,  245,  245,  245,  166,  155,
 /*  2760 */   156,  157,  158,  159,  160,  161,  245,  245,  245,  245,
 /*  2770 */   166,  155,  156,  157,  158,  159,  160,  161,   73,  245,
 /*  2780 */   245,  245,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2790 */   245,  245,  245,  245,  166,  155,  156,  157,  158,  159,
 /*  2800 */   160,  161,  245,  245,  245,  245,  166,  245,  245,  245,
 /*  2810 */   245,  106,  107,  108,  109,  110,
};
#define YY_SHIFT_USE_DFLT (-73)
#define YY_SHIFT_COUNT (467)
#define YY_SHIFT_MIN   (-72)
#define YY_SHIFT_MAX   (2705)
static const short yy_shift_ofst[] = {
 /*     0 */   -73,  108,  120,  120,  197,  197,  197,  197,  197,  197,
 /*    10 */   197,  197,  197,  197,  228,  228,  228,  228,  228,  228,
 /*    20 */   228,  228,  228,  228,  197,  197,  197,  197,  197,  197,
 /*    30 */   197,  197,  197,  197,  197,  197,  197,  376,  376,  376,
 /*    40 */   376,  304,  304,  304,  304,  304,  304,  304,  304,  304,
 /*    50 */   304,  509,  509,  509,  509,  509,  509,  509,  509,  509,
 /*    60 */   509,  509,  509,  509,  509,  509,  509,  509,  509,  509,
 /*    70 */   509,  509,  509,  509,  509,  509,  509,  509,  509,  509,
 /*    80 */   509,  509,  509,  509,  509,  509,  509,  509,  509,  509,
 /*    90 */   509,  509,  509,  509,  509,  509,  509,  509,  509,  509,
 /*   100 */   509,  509,  509,  509,  509,  509,  509,  509,  509,  509,
 /*   110 */   509,  509,  509, 1438, 1438, 1438,  148,  373,  148,  148,
 /*   120 */   148,  148,  148,  148,  148,  148,  148,  148,  148,  148,
 /*   130 */   148,  148,  148,  148,  148,  148,  148,  148,  148,  148,
 /*   140 */   148,  148, 1471, 1471, 1471, 1471,   40, 1471, 1471, 1471,
 /*   150 */    40,  473,  473,  503,  152,  504,   40,  328,  328,  636,
 /*   160 */   946, 1469, 1463, 1052,  946,  946,  946,  946,  609,  559,
 /*   170 */   748,  636,  636, 1469, 1463, 1155, 1000,  952,  952,  952,
 /*   180 */   952, 1493,  419,  419,  357,  357,  357,  357,  357,  357,
 /*   190 */   357,  357,  357,  357,  357,  133,  358,  332,  609, 1070,
 /*   200 */   980, 1070, 1054, 1070, 1054, 1159, 1135, 1072,  980, 1070,
 /*   210 */  1054, 1159, 1135, 1072,  980, 1070, 1054, 1159, 1135, 1072,
 /*   220 */   980, 1070, 1054, 1070, 1054, 1070, 1054, 1070, 1054, 1070,
 /*   230 */  1054, 1159, 1135, 1072, 1070, 1054, 1159, 1135, 1072, 1070,
 /*   240 */  1054, 1159, 1135, 1172, 1172, 1155, 1070, 1054, 1072, 1070,
 /*   250 */  1054,  980, 1070, 1054,  980, 1070, 1054,  953,  953, 1072,
 /*   260 */   980,  953,  935,  383, 1321, 1306, 1521, 1492, 1477, 1599,
 /*   270 */  1570, 1560, 1529,  268,  936,  845,  815, 1091,  906,  761,
 /*   280 */   663,  379,  639, 1654,  446,  446, 1169, 1169, 1169, 1426,
 /*   290 */   790, 1144, 1035,  785,  446, 2705, 1617,  388,  520,  863,
 /*   300 */   239,  850,  850,  850,  850,  850,  850,  850,  205,  205,
 /*   310 */   205,  205,  205,  159,  205,  205,  205,  205,  850,  850,
 /*   320 */   850,  205,  205,  205,  205,  205,  205,  205,  205,  205,
 /*   330 */   205,  205,  205,  205,  205,  205,  205,  205,  205,  205,
 /*   340 */   205,  205,  820,  313,  205,  205,  139,  281,  281,  226,
 /*   350 */    85,   85,  172,  172,  -50,  -50,  -20,  -20,  -50,  -50,
 /*   360 */   -72,  -72,  -72,  -72,  -72,  -72,  -72,  -72,  549,  692,
 /*   370 */   624,  640,  377,   23,   23,   23,   23,  623,  265,  466,
 /*   380 */   762,  198,  277,  712,  684,  381,  638,  -55,  184,  112,
 /*   390 */   174,  563,  -46,  138,  355,  191,   64,  267,  252,  169,
 /*   400 */    71,   38, 1008, 1007, 1001,  999,  998,  995,  991,  990,
 /*   410 */   982,  981,  966,  965,  956,  947,  943,  926,  915,  913,
 /*   420 */   787,  787,  892,  797,  865,  767,  853,  728,  805,  744,
 /*   430 */   721,  716,  740,  661,  507,  507,  627,  662,  566,  552,
 /*   440 */   507,  521,  500,  476,  489,  450,  452,  442,  424,  399,
 /*   450 */   338,  338,  380,  356,  278,   89,   89,  236,  194,  153,
 /*   460 */   117,   99,  104,   83,   76,   67,    5,   74,
};
#define YY_REDUCE_USE_DFLT (-171)
#define YY_REDUCE_COUNT (262)
#define YY_REDUCE_MIN   (-170)
#define YY_REDUCE_MAX   (2640)
static const short yy_reduce_ofst[] = {
 /*     0 */   882, -136,  188,  378,  344, -112, 1209, 1185, 1161, 1131,
 /*    10 */  1101, 1071, 1047, 1023,  992,  962,  932,  902,  868,  838,
 /*    20 */   807,  777,  747,  717,  693,  669,  645,  621,  595,  571,
 /*    30 */   547,  518,  494,  470,  433,  409,  -88, 1362, 1338, 1301,
 /*    40 */  1277, 1722, 1706, 1677, 1660, 1644, 1628, 1611, 1595, 1577,
 /*    50 */  1561, 1766, 1751, 1538, 1263,  230, 2378, 2368, 2356, 2346,
 /*    60 */  2336, 2326, 2316, 2306, 2296, 2286, 2276, 2266, 2255, 2243,
 /*    70 */  2233, 2223, 2213, 2201, 2191, 2181, 2171, 2161, 2151, 2141,
 /*    80 */  2131, 2121, 2111, 2100, 2088, 2078, 2068, 2058, 2046, 2036,
 /*    90 */  2026, 2016, 2006, 1996, 1986, 1976, 1966, 1956, 1945, 1933,
 /*   100 */  1923, 1913, 1903, 1888, 1878, 1868, 1858, 1848, 1835, 1825,
 /*   110 */  1815, 1802, 1792, 1776,  163,   49,  -28, -170, 2640, 2628,
 /*   120 */  2616, 2604, 2592, 2580, 2568, 2556, 2544, 2532, 2520, 2508,
 /*   130 */  2496, 2484, 2472, 2460, 2447, 2435, 2423, 2411, 2399, 2387,
 /*   140 */  1694, 1323, 1200, 1152, 1038,  612,   29,  538,  485,  461,
 /*   150 */   896, 1710,  412,   33, -133,  238,  314,  938,  447,   80,
 /*   160 */   186,  232,   20,  839,  774,  772,  765,  735,  725,  260,
 /*   170 */   648,  726,  653,  580,  572,  496, 1242, 1240, 1232, 1231,
 /*   180 */  1158, 1225, 1228, 1227, 1276, 1275, 1270, 1269, 1239, 1234,
 /*   190 */  1229, 1208, 1203, 1202, 1201, 1199, 1170, 1125, 1160, 1107,
 /*   200 */  1171, 1106, 1103, 1105, 1100, 1090, 1102, 1092, 1140, 1089,
 /*   210 */  1087, 1088, 1079, 1075, 1126, 1084, 1077, 1078, 1074, 1073,
 /*   220 */  1058, 1068, 1060, 1067, 1059, 1057, 1056, 1046, 1055, 1037,
 /*   230 */  1048, 1032, 1019, 1020, 1015,  994, 1010,  989,  996,  969,
 /*   240 */   987,  988,  970,  975,  972,  961,  945,  948,  942,  939,
 /*   250 */   931, 1003,  929,  910,  978,  907,  904,  968,  967,  890,
 /*   260 */   928,  924,  918,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   727, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*    10 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*    20 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*    30 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*    40 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*    50 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,  931,
 /*    60 */   935,  930,  934,  952,  948,  953,  949, 1100, 1100, 1100,
 /*    70 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*    80 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*    90 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   100 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   110 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   120 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   130 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   140 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   150 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   160 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   170 */  1100, 1100, 1100, 1100, 1100, 1100, 1049, 1051, 1051, 1051,
 /*   180 */  1051, 1057, 1049, 1049, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   190 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1049, 1100, 1057,
 /*   200 */  1100, 1057, 1055, 1057, 1055, 1051, 1053, 1049, 1100, 1057,
 /*   210 */  1055, 1051, 1053, 1049, 1100, 1057, 1055, 1051, 1053, 1049,
 /*   220 */  1100, 1057, 1055, 1057, 1055, 1057, 1055, 1057, 1055, 1057,
 /*   230 */  1055, 1051, 1053, 1049, 1057, 1055, 1051, 1053, 1049, 1057,
 /*   240 */  1055, 1051, 1053, 1100, 1100, 1100, 1057, 1055, 1049, 1057,
 /*   250 */  1055, 1100, 1057, 1055, 1100, 1057, 1055, 1100, 1100, 1049,
 /*   260 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   270 */   884,  884, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   280 */  1100, 1100,  793, 1100, 1050, 1052, 1041, 1038,  924, 1100,
 /*   290 */  1100, 1100, 1100, 1056, 1048, 1100, 1100, 1100,  921,  838,
 /*   300 */   898,  910,  909,  908,  907,  906,  905,  904,  890,  933,
 /*   310 */   937,  932,  936,  855,  954,  950,  955,  951,  913,  766,
 /*   320 */   767,  870,  869,  868,  867,  866,  865,  864,  886,  883,
 /*   330 */   882,  881,  880,  879,  878,  877,  876,  875,  874,  873,
 /*   340 */   872,  871, 1100, 1100,  763,  762,  946,  923,  922, 1100,
 /*   350 */   843,  844,  900,  899,  820,  819,  857,  856,  833,  834,
 /*   360 */   795,  794,  800,  799,  805,  804,  779,  780, 1100, 1100,
 /*   370 */   839, 1100, 1100, 1018, 1022, 1021, 1019, 1100, 1100, 1100,
 /*   380 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,  967, 1100,
 /*   390 */  1100, 1100, 1100, 1100, 1100,  747, 1100, 1100, 1100, 1100,
 /*   400 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   410 */  1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,  788,
 /*   420 */   920,  919, 1100, 1100, 1100, 1100, 1100, 1020, 1100, 1008,
 /*   430 */   985,  987, 1100, 1000,  966,  965,  983, 1100, 1100,  982,
 /*   440 */   981, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100,
 /*   450 */   897,  896,  888,  884,  751,  854,  853, 1100, 1100, 1100,
 /*   460 */  1100,  911,  765,  761,  759,  755,  750, 1100, 1099, 1098,
 /*   470 */  1097, 1096, 1095, 1094, 1093, 1092, 1091, 1090, 1089, 1088,
 /*   480 */   947, 1087, 1086, 1085, 1084, 1078, 1077, 1079, 1076, 1075,
 /*   490 */  1074, 1073, 1072, 1071, 1070, 1069, 1068, 1067, 1066, 1065,
 /*   500 */  1064, 1063, 1062, 1061, 1060, 1059, 1058, 1034, 1033, 1040,
 /*   510 */  1039, 1047, 1046, 1043, 1042, 1037, 1045,  915,  917,  918,
 /*   520 */   916,  914,  791, 1036, 1035, 1029, 1028, 1030, 1027, 1032,
 /*   530 */  1031, 1026, 1024, 1023, 1025, 1017, 1012, 1015, 1016, 1014,
 /*   540 */  1013, 1011, 1003, 1006, 1010, 1009, 1007, 1005, 1004, 1002,
 /*   550 */   978,  986,  988, 1001,  999,  998,  997,  996,  995,  994,
 /*   560 */   993,  992,  991,  990,  989,  984,  980,  976,  975,  974,
 /*   570 */   973,  972,  971,  970,  755,  969,  968,  845,  847,  846,
 /*   580 */   842,  841,  840,  839,  979,  977,  957,  960,  961,  964,
 /*   590 */   963,  962,  959,  958,  956, 1083, 1082, 1081, 1080,  892,
 /*   600 */   894,  903,  902,  901,  895,  893,  891,  818,  817,  816,
 /*   610 */   815,  814,  813,  823,  822,  821,  812,  811,  810,  809,
 /*   620 */  1054,  887,  889,  752,  849,  851,  926,  929,  928,  927,
 /*   630 */   925,  863,  862,  861,  860,  859,  858,  852,  850,  848,
 /*   640 */   791,  946,  945,  944,  943,  942,  941,  885,  939,  940,
 /*   650 */   938,  912,  835,  837,  836,  832,  831,  830,  829,  828,
 /*   660 */   827,  826,  825,  824,  764,  798,  797,  796,  803,  802,
 /*   670 */   801,  793,  792,  791,  790,  789,  788,  808,  807,  806,
 /*   680 */   787,  786,  785,  784,  781,  783,  782,  778,  777,  776,
 /*   690 */   775,  774,  773,  772,  771,  770,  769,  768,  760,  758,
 /*   700 */   757,  756,  754,  753,  749,  745,  744,  748,  747,  746,
 /*   710 */   743,  742,  741,  740,  739,  738,  737,  736,  735,  734,
 /*   720 */   733,  732,  731,  730,  729,  728,
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
  "constant_one_const",  "term_no_const_lst",  "term_no_const",  "const_anon",  
  "term_strong",   "term_strong_candidate",  "term_no_const_strong",  "num_range",   
  "term_numeric",  "formula",       "formula_base",  "comparison",  
  "atomic_formula",  "formula_quant",  "formula_card",  "atomic_formula_anon",
  "formula_no_const",  "formula_no_const_base",  "comparison_no_const",  "atomic_formula_one_const",
  "formula_temporal",  "quant_lst",     "quant_op",      "card_var_lst",
  "card_var_lst_inner",  "head_formula",  "atomic_head_formula",  "formula_smpl_card",
  "macro_def_lst",  "macro_bnd",     "macro_args",    "macro_arg",   
  "sort_lst",      "sort",          "sort_id_nr",    "sort_nr",     
  "sort_id",       "constant_bnd_lst",  "constant_bnd",  "constant_dcl_lst",
  "constant_dcl_type",  "attrib_spec",   "object_bnd_lst",  "object_bnd",  
  "object_lst",    "object_spec",   "variable_bnd_lst",  "variable_bnd",
  "variable_lst",  "sort_bnd_lst",  "sort_bnd",      "sort_dcl_lst",
  "show_lst",      "show_elem",     "query_lst",     "query_maxstep_decl",
  "query_label_decl",  "query_label_Decl",  "clause_if",     "clause_after",
  "clause_ifcons",  "clause_unless",  "clause_where",  "law_basic",   
  "law_caused",    "law_pcaused",   "law_impl",      "law_causes",  
  "law_increments",  "law_decrements",  "law_mcause",    "law_always",  
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
 /* 188 */ "formula_temporal ::= formula_base",
 /* 189 */ "formula_temporal ::= PAREN_L formula_temporal PAREN_R",
 /* 190 */ "formula_temporal ::= NOT formula_temporal",
 /* 191 */ "formula_temporal ::= DASH formula_temporal",
 /* 192 */ "formula_temporal ::= formula_temporal AMP formula_temporal",
 /* 193 */ "formula_temporal ::= formula_temporal DBL_PLUS formula_temporal",
 /* 194 */ "formula_temporal ::= formula_temporal PIPE formula_temporal",
 /* 195 */ "formula_temporal ::= formula_temporal EQUIV formula_temporal",
 /* 196 */ "formula_temporal ::= formula_temporal IMPL formula_temporal",
 /* 197 */ "formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal",
 /* 198 */ "formula_temporal ::= term_strong COLON formula_temporal",
 /* 199 */ "formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R",
 /* 200 */ "quant_lst ::= quant_op variable",
 /* 201 */ "quant_lst ::= quant_lst quant_op variable",
 /* 202 */ "quant_op ::= BIG_CONJ",
 /* 203 */ "quant_op ::= BIG_DISJ",
 /* 204 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 205 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R",
 /* 206 */ "formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 207 */ "formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term",
 /* 208 */ "formula_card ::= CBRACKET_L formula CBRACKET_R",
 /* 209 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R",
 /* 210 */ "formula_card ::= CBRACKET_L formula CBRACKET_R term",
 /* 211 */ "formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term",
 /* 212 */ "card_var_lst ::= card_var_lst_inner PIPE",
 /* 213 */ "card_var_lst_inner ::= variable",
 /* 214 */ "card_var_lst_inner ::= card_var_lst_inner COMMA variable",
 /* 215 */ "head_formula ::= comparison",
 /* 216 */ "head_formula ::= atomic_head_formula",
 /* 217 */ "head_formula ::= formula_smpl_card",
 /* 218 */ "head_formula ::= TRUE",
 /* 219 */ "head_formula ::= FALSE",
 /* 220 */ "atomic_head_formula ::= atomic_formula",
 /* 221 */ "atomic_head_formula ::= DASH constant",
 /* 222 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 223 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 224 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 225 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 226 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 227 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 228 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 229 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 230 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 231 */ "macro_def_lst ::= macro_bnd",
 /* 232 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 233 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 234 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 235 */ "macro_args ::= macro_arg",
 /* 236 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 237 */ "macro_arg ::= POUND_INTEGER",
 /* 238 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 239 */ "sort_lst ::= sort",
 /* 240 */ "sort_lst ::= sort_lst COMMA sort",
 /* 241 */ "sort ::= sort_id_nr",
 /* 242 */ "sort ::= sort_id_nr STAR",
 /* 243 */ "sort ::= sort_id_nr CARROT",
 /* 244 */ "sort ::= sort PLUS object_nullary",
 /* 245 */ "sort ::= sort PLUS IDENTIFIER",
 /* 246 */ "sort ::= sort PLUS INTEGER",
 /* 247 */ "sort_id_nr ::= sort_id",
 /* 248 */ "sort_id_nr ::= sort_nr",
 /* 249 */ "sort_nr ::= num_range",
 /* 250 */ "sort_id ::= IDENTIFIER",
 /* 251 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 252 */ "constant_bnd_lst ::= constant_bnd",
 /* 253 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 254 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 255 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 256 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 257 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 258 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 259 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 260 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 261 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 262 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 263 */ "constant_dcl_type ::= ABACTION",
 /* 264 */ "constant_dcl_type ::= ACTION",
 /* 265 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 266 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 267 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 268 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 269 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 270 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 271 */ "constant_dcl_type ::= RIGID",
 /* 272 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 273 */ "constant_dcl_type ::= SDFLUENT",
 /* 274 */ "attrib_spec ::= ATTRIBUTE",
 /* 275 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 276 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 277 */ "object_bnd_lst ::= object_bnd",
 /* 278 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 279 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 280 */ "object_lst ::= object_spec",
 /* 281 */ "object_lst ::= object_lst COMMA object_spec",
 /* 282 */ "object_spec ::= IDENTIFIER",
 /* 283 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 284 */ "object_spec ::= num_range",
 /* 285 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 286 */ "variable_bnd_lst ::= variable_bnd",
 /* 287 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 288 */ "variable_bnd ::= variable_lst DBL_COLON sort_id",
 /* 289 */ "variable_lst ::= IDENTIFIER",
 /* 290 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 291 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 292 */ "sort_bnd_lst ::= sort_bnd",
 /* 293 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 294 */ "sort_bnd ::= sort_dcl_lst",
 /* 295 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 296 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 297 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 298 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 299 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 300 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 301 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 302 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 303 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 304 */ "show_lst ::= show_elem",
 /* 305 */ "show_lst ::= show_lst COMMA show_elem",
 /* 306 */ "show_elem ::= atomic_formula_one_const",
 /* 307 */ "stmt_noconcurrency ::= NOCONCURRENCY PERIOD",
 /* 308 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD",
 /* 309 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD",
 /* 310 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD",
 /* 311 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 312 */ "query_lst ::= formula_temporal",
 /* 313 */ "query_lst ::= query_maxstep_decl",
 /* 314 */ "query_lst ::= query_label_decl",
 /* 315 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 316 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 317 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 318 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 319 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range",
 /* 320 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 321 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 322 */ "clause_if ::= IF formula",
 /* 323 */ "clause_if ::=",
 /* 324 */ "clause_after ::= AFTER formula",
 /* 325 */ "clause_after ::=",
 /* 326 */ "clause_ifcons ::= IFCONS formula",
 /* 327 */ "clause_ifcons ::=",
 /* 328 */ "clause_unless ::= UNLESS atomic_formula_anon",
 /* 329 */ "clause_unless ::=",
 /* 330 */ "clause_where ::= WHERE formula_no_const",
 /* 331 */ "clause_where ::=",
 /* 332 */ "stmt_law ::= law_basic",
 /* 333 */ "stmt_law ::= law_caused",
 /* 334 */ "stmt_law ::= law_pcaused",
 /* 335 */ "stmt_law ::= law_impl",
 /* 336 */ "stmt_law ::= law_causes",
 /* 337 */ "stmt_law ::= law_increments",
 /* 338 */ "stmt_law ::= law_decrements",
 /* 339 */ "stmt_law ::= law_mcause",
 /* 340 */ "stmt_law ::= law_always",
 /* 341 */ "stmt_law ::= law_constraint",
 /* 342 */ "stmt_law ::= law_impossible",
 /* 343 */ "stmt_law ::= law_never",
 /* 344 */ "stmt_law ::= law_default",
 /* 345 */ "stmt_law ::= law_exogenous",
 /* 346 */ "stmt_law ::= law_inertial",
 /* 347 */ "stmt_law ::= law_nonexecutable",
 /* 348 */ "stmt_law ::= law_rigid",
 /* 349 */ "stmt_law ::= law_observed",
 /* 350 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 351 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 352 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 353 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 354 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 355 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 356 */ "law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 357 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 358 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 359 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 360 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 361 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 362 */ "law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 363 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 364 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 365 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 366 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 367 */ "law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD",
 /* 368 */ "stmt_code_blk ::= ASP_GR",
 /* 369 */ "stmt_code_blk ::= ASP_CP",
 /* 370 */ "stmt_code_blk ::= F2LP_GR",
 /* 371 */ "stmt_code_blk ::= F2LP_CP",
 /* 372 */ "stmt_code_blk ::= LUA_GR",
 /* 373 */ "stmt_code_blk ::= LUA_CP",
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
#line 2346 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 137: /* start */
    case 138: /* statement_lst */
    case 161: /* undeclared */
{
#line 208 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2355 "bcplus/parser/detail/lemon_parser.c"
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
 DEALLOC((yypminor->yy230));								
#line 2368 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_macro_def */
{
#line 233 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy101));								
#line 2375 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_constant_def */
{
#line 235 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy5));								
#line 2382 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_object_def */
{
#line 237 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy78));								
#line 2389 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_variable_def */
{
#line 239 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy487));								
#line 2396 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 144: /* stmt_sort_def */
{
#line 241 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy195));								
#line 2403 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_noconcurrency */
{
#line 251 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy87));								
#line 2410 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 150: /* stmt_strong_noconcurrency */
{
#line 253 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy158));								
#line 2417 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* stmt_query */
{
#line 259 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy140));								
#line 2424 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 154: /* base_elem */
    case 155: /* base_elem_no_const */
    case 163: /* term */
    case 166: /* term_no_const */
    case 168: /* term_strong */
    case 169: /* term_strong_candidate */
    case 170: /* term_no_const_strong */
{
#line 293 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy88));								
#line 2437 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* constant */
    case 164: /* constant_one_const */
    case 167: /* const_anon */
{
#line 297 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy327));								
#line 2446 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 157: /* object */
    case 158: /* object_nullary */
{
#line 299 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy208));								
#line 2454 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* variable */
{
#line 303 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy163));								
#line 2461 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 160: /* lua */
{
#line 305 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy313));								
#line 2468 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 162: /* term_lst */
    case 165: /* term_no_const_lst */
{
#line 309 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy197));								
#line 2476 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* num_range */
{
#line 707 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy331));								
#line 2483 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 172: /* term_numeric */
{
#line 709 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy458));								
#line 2490 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 173: /* formula */
    case 174: /* formula_base */
    case 175: /* comparison */
    case 178: /* formula_card */
    case 180: /* formula_no_const */
    case 181: /* formula_no_const_base */
    case 182: /* comparison_no_const */
    case 184: /* formula_temporal */
    case 189: /* head_formula */
{
#line 770 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy223));								
#line 2505 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 176: /* atomic_formula */
    case 179: /* atomic_formula_anon */
    case 183: /* atomic_formula_one_const */
    case 190: /* atomic_head_formula */
{
#line 776 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy262));								
#line 2515 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 177: /* formula_quant */
{
#line 778 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy381));								
#line 2522 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_lst */
{
#line 979 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy147));								
#line 2529 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* quant_op */
{
#line 981 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2536 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 187: /* card_var_lst */
    case 188: /* card_var_lst_inner */
{
#line 1018 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy125));								
#line 2544 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 191: /* formula_smpl_card */
{
#line 1096 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy91));								
#line 2551 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 192: /* macro_def_lst */
{
#line 1143 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy179));                              
#line 2558 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 193: /* macro_bnd */
{
#line 1145 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy403));                              
#line 2565 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 194: /* macro_args */
{
#line 1147 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy220));                              
#line 2572 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 195: /* macro_arg */
{
#line 1149 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy93));                              
#line 2579 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 196: /* sort_lst */
{
#line 1239 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy305));							
#line 2586 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 197: /* sort */
    case 198: /* sort_id_nr */
    case 199: /* sort_nr */
    case 200: /* sort_id */
{
#line 1241 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2596 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* constant_bnd_lst */
    case 202: /* constant_bnd */
{
#line 1369 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy55));									
#line 2604 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* constant_dcl_lst */
{
#line 1373 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy484));									
#line 2611 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* constant_dcl_type */
{
#line 1375 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2618 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* attrib_spec */
{
#line 1377 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2625 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 206: /* object_bnd_lst */
{
#line 1733 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy485));									
#line 2632 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 207: /* object_bnd */
{
#line 1735 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy190));									
#line 2639 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 208: /* object_lst */
    case 209: /* object_spec */
{
#line 1737 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy148));									
#line 2647 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* variable_bnd_lst */
    case 211: /* variable_bnd */
{
#line 1847 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy97));									
#line 2655 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* variable_lst */
{
#line 1851 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy408));									
#line 2662 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* sort_bnd_lst */
    case 214: /* sort_bnd */
    case 215: /* sort_dcl_lst */
{
#line 1924 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy245));									
#line 2671 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* show_lst */
{
#line 2028 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy165));									
#line 2678 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* show_elem */
    case 225: /* clause_unless */
{
#line 2030 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy262));									
#line 2686 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 218: /* query_lst */
{
#line 2175 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy431).l); DEALLOC((yypminor->yy431).maxstep); DEALLOC((yypminor->yy431).label);	
#line 2693 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* query_maxstep_decl */
{
#line 2177 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy221));												
#line 2700 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 221: /* query_label_Decl */
{
#line 2179 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2707 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 222: /* clause_if */
    case 223: /* clause_after */
    case 224: /* clause_ifcons */
    case 226: /* clause_where */
{
#line 2333 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy223));									
#line 2717 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 227: /* law_basic */
    case 228: /* law_caused */
    case 229: /* law_pcaused */
    case 230: /* law_impl */
    case 231: /* law_causes */
    case 232: /* law_increments */
    case 233: /* law_decrements */
    case 234: /* law_mcause */
    case 235: /* law_always */
    case 236: /* law_constraint */
    case 237: /* law_impossible */
    case 238: /* law_never */
    case 239: /* law_default */
    case 240: /* law_exogenous */
    case 241: /* law_inertial */
    case 242: /* law_nonexecutable */
    case 243: /* law_rigid */
    case 244: /* law_observed */
{
#line 2374 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy230));									
#line 2741 "bcplus/parser/detail/lemon_parser.c"
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
  { 167, 1 },
  { 167, 4 },
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
  { 168, 1 },
  { 168, 1 },
  { 168, 1 },
  { 168, 3 },
  { 168, 1 },
  { 168, 1 },
  { 168, 1 },
  { 168, 2 },
  { 168, 2 },
  { 169, 2 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 168, 3 },
  { 170, 1 },
  { 170, 1 },
  { 170, 1 },
  { 170, 3 },
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
  { 173, 1 },
  { 173, 3 },
  { 173, 2 },
  { 173, 2 },
  { 173, 3 },
  { 173, 3 },
  { 173, 3 },
  { 173, 3 },
  { 173, 3 },
  { 173, 3 },
  { 174, 1 },
  { 174, 1 },
  { 174, 1 },
  { 174, 1 },
  { 174, 1 },
  { 174, 1 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 175, 3 },
  { 176, 1 },
  { 176, 2 },
  { 176, 3 },
  { 179, 1 },
  { 179, 1 },
  { 179, 2 },
  { 179, 3 },
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
  { 177, 5 },
  { 185, 2 },
  { 185, 3 },
  { 186, 1 },
  { 186, 1 },
  { 178, 4 },
  { 178, 5 },
  { 178, 5 },
  { 178, 6 },
  { 178, 3 },
  { 178, 4 },
  { 178, 4 },
  { 178, 5 },
  { 187, 2 },
  { 188, 1 },
  { 188, 3 },
  { 189, 1 },
  { 189, 1 },
  { 189, 1 },
  { 189, 1 },
  { 189, 1 },
  { 190, 1 },
  { 190, 2 },
  { 191, 4 },
  { 191, 5 },
  { 191, 5 },
  { 191, 6 },
  { 191, 3 },
  { 191, 4 },
  { 191, 4 },
  { 191, 5 },
  { 140, 4 },
  { 192, 1 },
  { 192, 3 },
  { 193, 6 },
  { 193, 3 },
  { 194, 1 },
  { 194, 3 },
  { 195, 1 },
  { 195, 1 },
  { 196, 1 },
  { 196, 3 },
  { 197, 1 },
  { 197, 2 },
  { 197, 2 },
  { 197, 3 },
  { 197, 3 },
  { 197, 3 },
  { 198, 1 },
  { 198, 1 },
  { 199, 1 },
  { 200, 1 },
  { 141, 4 },
  { 201, 1 },
  { 201, 3 },
  { 202, 6 },
  { 202, 3 },
  { 202, 3 },
  { 202, 5 },
  { 202, 8 },
  { 203, 1 },
  { 203, 4 },
  { 203, 3 },
  { 203, 6 },
  { 204, 1 },
  { 204, 1 },
  { 204, 1 },
  { 204, 1 },
  { 204, 1 },
  { 204, 1 },
  { 204, 1 },
  { 204, 1 },
  { 204, 1 },
  { 204, 1 },
  { 204, 1 },
  { 205, 1 },
  { 205, 4 },
  { 142, 4 },
  { 206, 1 },
  { 206, 3 },
  { 207, 3 },
  { 208, 1 },
  { 208, 3 },
  { 209, 1 },
  { 209, 4 },
  { 209, 1 },
  { 143, 4 },
  { 210, 1 },
  { 210, 3 },
  { 211, 3 },
  { 212, 1 },
  { 212, 3 },
  { 144, 4 },
  { 213, 1 },
  { 213, 3 },
  { 214, 1 },
  { 214, 3 },
  { 214, 3 },
  { 214, 3 },
  { 215, 1 },
  { 215, 3 },
  { 147, 4 },
  { 147, 4 },
  { 148, 4 },
  { 148, 4 },
  { 216, 1 },
  { 216, 3 },
  { 217, 1 },
  { 149, 2 },
  { 150, 2 },
  { 151, 5 },
  { 152, 5 },
  { 153, 4 },
  { 218, 1 },
  { 218, 1 },
  { 218, 1 },
  { 218, 3 },
  { 218, 3 },
  { 218, 3 },
  { 219, 3 },
  { 219, 3 },
  { 220, 3 },
  { 220, 3 },
  { 222, 2 },
  { 222, 0 },
  { 223, 2 },
  { 223, 0 },
  { 224, 2 },
  { 224, 0 },
  { 225, 2 },
  { 225, 0 },
  { 226, 2 },
  { 226, 0 },
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
  { 146, 1 },
  { 227, 7 },
  { 228, 8 },
  { 229, 8 },
  { 230, 5 },
  { 231, 7 },
  { 232, 9 },
  { 233, 9 },
  { 234, 7 },
  { 235, 6 },
  { 236, 6 },
  { 237, 6 },
  { 238, 6 },
  { 239, 8 },
  { 240, 8 },
  { 241, 8 },
  { 242, 6 },
  { 243, 4 },
  { 244, 5 },
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
#line 3420 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 219 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy230;
			yymsp[0].minor.yy230  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3429 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 262 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy101; }
#line 3434 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy5; }
#line 3439 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy78; }
#line 3444 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy487; }
#line 3449 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy195; }
#line 3454 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy230; }
#line 3464 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 271 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy87; }
#line 3469 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy158; }
#line 3474 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 275 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy140; }
#line 3479 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 321 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy88 = yymsp[0].minor.yy327; }
#line 3484 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 42: /* term ::= base_elem */ yytestcase(yyruleno==42);
      case 58: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==58);
      case 83: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==83);
      case 98: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==98);
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy88 = yymsp[0].minor.yy88; }
#line 3493 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 324 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy88 = yymsp[0].minor.yy208;	}
#line 3498 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 325 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy88 = yymsp[0].minor.yy163; }
#line 3503 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 326 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy88 = yymsp[0].minor.yy313; }
#line 3508 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 38: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==38);
#line 443 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy327, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy197, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3514 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 39: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==39);
#line 444 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy327, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3520 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* const_anon ::= IDENTIFIER */
#line 446 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy327, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true); }
#line 3525 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 447 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy327, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy197, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true);	}
#line 3530 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 450 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy208, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy197, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3535 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= object_nullary */
#line 451 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy208 = yymsp[0].minor.yy208; }
#line 3540 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* object_nullary ::= OBJECT_ID */
#line 452 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy208, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3545 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* object ::= undeclared */
#line 453 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3550 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* variable ::= VARIABLE_ID */
#line 456 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy163 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::VARIABLE)) {
			parser->_feature_error(Language::Feature::VARIABLE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			BASE_ELEM_BARE_DEF(yygotominor.yy163, yymsp[0].minor.yy0, Symbol::Type::VARIABLE, Variable, VariableSymbol); 
		}
	}
#line 3565 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 467 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy313, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy197, yymsp[0].minor.yy0); }
#line 3570 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* lua ::= AT_IDENTIFIER */
#line 468 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy313, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3575 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 469 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy423, yymsp[-3].minor.yy0, yymsp[-1].minor.yy197);   yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3582 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* undeclared ::= IDENTIFIER */
#line 470 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy423, yymsp[0].minor.yy0, NULL); }
#line 3587 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 36: /* term_lst ::= term */
      case 40: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==40);
#line 473 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy197 = new TermList();
			yygotominor.yy197->push_back(yymsp[0].minor.yy88);
		}
#line 3596 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 37: /* term_lst ::= term_lst COMMA term */
      case 41: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==41);
#line 479 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy197 = yymsp[-2].minor.yy197;
			yymsp[-2].minor.yy197->push_back(yymsp[0].minor.yy88);
		  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 3606 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 43: /* term ::= INTEGER */
      case 59: /* term_strong ::= INTEGER */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==99);
#line 578 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy88, yymsp[0].minor.yy0);	}
#line 3614 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 44: /* term ::= STRING_LITERAL */
      case 46: /* term ::= TRUE */ yytestcase(yyruleno==46);
      case 47: /* term ::= FALSE */ yytestcase(yyruleno==47);
      case 60: /* term_strong ::= STRING_LITERAL */ yytestcase(yyruleno==60);
      case 85: /* term_no_const_strong ::= STRING_LITERAL */ yytestcase(yyruleno==85);
      case 100: /* term_no_const ::= STRING_LITERAL */ yytestcase(yyruleno==100);
      case 102: /* term_no_const ::= TRUE */ yytestcase(yyruleno==102);
      case 103: /* term_no_const ::= FALSE */ yytestcase(yyruleno==103);
#line 579 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy88, yymsp[0].minor.yy0); }
#line 3626 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 45: /* term ::= PAREN_L term PAREN_R */
      case 61: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==61);
      case 86: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==86);
      case 101: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==101);
#line 580 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy88, yymsp[-2].minor.yy0, yymsp[-1].minor.yy88, yymsp[0].minor.yy0); }
#line 3634 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 48: /* term ::= MAXSTEP */
      case 62: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==87);
#line 583 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy88, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3641 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= MAXADDITIVE */
      case 63: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==63);
      case 88: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==88);
#line 584 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy88, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3648 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= MAXAFVALUE */
      case 64: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==64);
      case 89: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==89);
#line 585 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy88, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3655 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= DASH term */
      case 65: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==65);
      case 91: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==91);
      case 105: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==105);
#line 589 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, UnaryTerm::Operator::NEGATIVE); }
#line 3663 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= ABS term */
      case 66: /* term_strong ::= ABS term */ yytestcase(yyruleno==66);
      case 92: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==92);
      case 106: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==106);
#line 590 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, UnaryTerm::Operator::ABS); }
#line 3671 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= term DASH term */
      case 68: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==68);
      case 78: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==78);
      case 93: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==93);
      case 107: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==107);
#line 594 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::MINUS); }
#line 3680 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= term PLUS term */
      case 69: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==69);
      case 79: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==79);
      case 94: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==94);
      case 108: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==108);
#line 595 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::PLUS); }
#line 3689 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= term STAR term */
      case 70: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==70);
      case 80: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==80);
      case 95: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==95);
      case 109: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==109);
#line 596 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::TIMES); }
#line 3698 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 56: /* term ::= term INT_DIV term */
      case 71: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==71);
      case 81: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==81);
      case 96: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==96);
      case 110: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==110);
#line 597 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::DIVIDE); }
#line 3707 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 57: /* term ::= term MOD term */
      case 72: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==72);
      case 82: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==82);
      case 97: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==97);
      case 111: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==111);
#line 598 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::MOD); }
#line 3716 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 67: /* term_strong_candidate ::= DASH constant */
#line 617 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy327, UnaryTerm::Operator::NEGATIVE); }
#line 3721 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 73: /* term_strong ::= constant DASH term */
#line 626 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy327, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::MINUS); }
#line 3726 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant PLUS term */
#line 627 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy327, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::PLUS); }
#line 3731 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant STAR term */
#line 628 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy327, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::TIMES); }
#line 3736 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 76: /* term_strong ::= constant INT_DIV term */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy327, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::DIVIDE); }
#line 3741 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong ::= constant MOD term */
#line 630 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy327, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::MOD); }
#line 3746 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 90: /* term_no_const_strong ::= constant */
#line 652 "bcplus/parser/detail/lemon_parser.y"
{
		// error handling for constants so they don'yygotominor.yy88 default to undeclared identifiers
		yygotominor.yy88 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy327;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy327->beginLoc());
		YYERROR;
	}
#line 3757 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 104: /* term_no_const ::= constant */
#line 682 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy88 default to undeclared identifiers
		yygotominor.yy88 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy327;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy327->beginLoc());
		YYERROR;
	}
#line 3768 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 112: /* num_range ::= term_numeric DBL_PERIOD term_numeric */
#line 712 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy458, r_ptr = yymsp[0].minor.yy458, s_ptr = yymsp[-1].minor.yy0;

	yygotominor.yy331 = new NumberRange(yymsp[-2].minor.yy458->val(), yymsp[0].minor.yy458->val(), yymsp[-2].minor.yy458->beginLoc(), yymsp[0].minor.yy458->endLoc());

}
#line 3778 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 113: /* term_numeric ::= INTEGER */
#line 720 "bcplus/parser/detail/lemon_parser.y"
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
#line 3794 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 114: /* term_numeric ::= PAREN_L term_numeric PAREN_R */
#line 733 "bcplus/parser/detail/lemon_parser.y"
{ 
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy458 = yymsp[-1].minor.yy458;  
	yygotominor.yy458->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy458->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 3804 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 115: /* term_numeric ::= DASH term_numeric */
#line 753 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy458, yymsp[0].minor.yy458, -1 * yymsp[0].minor.yy458->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3810 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 116: /* term_numeric ::= ABS term_numeric */
#line 754 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy458, yymsp[0].minor.yy458, yymsp[0].minor.yy458->val() < 0 ? - yymsp[0].minor.yy458->val() : yymsp[0].minor.yy458->val());   yy_destructor(yypParser,111,&yymsp[-1].minor);
}
#line 3816 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 117: /* term_numeric ::= term_numeric DASH term_numeric */
#line 756 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() - yymsp[0].minor.yy458->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3822 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 118: /* term_numeric ::= term_numeric PLUS term_numeric */
#line 757 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() + yymsp[0].minor.yy458->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 3828 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 119: /* term_numeric ::= term_numeric STAR term_numeric */
#line 758 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() * yymsp[0].minor.yy458->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 3834 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 120: /* term_numeric ::= term_numeric INT_DIV term_numeric */
#line 759 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() / yymsp[0].minor.yy458->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 3840 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 121: /* term_numeric ::= term_numeric MOD term_numeric */
#line 760 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() % yymsp[0].minor.yy458->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 3846 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 122: /* formula ::= formula_base */
      case 165: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==165);
      case 188: /* formula_temporal ::= formula_base */ yytestcase(yyruleno==188);
#line 818 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = yymsp[0].minor.yy223;				}
#line 3853 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 123: /* formula ::= PAREN_L formula PAREN_R */
      case 166: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==166);
      case 189: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==189);
#line 819 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = yymsp[-1].minor.yy223; yygotominor.yy223->parens(true); 	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3862 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 124: /* formula ::= NOT formula */
      case 167: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==167);
      case 190: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==190);
#line 820 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 3869 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 125: /* formula ::= DASH formula */
      case 168: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==168);
      case 191: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==191);
#line 821 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 3876 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 126: /* formula ::= formula AMP formula */
      case 169: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==169);
      case 192: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==192);
#line 822 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy223, yymsp[0].minor.yy223, yymsp[-2].minor.yy223->beginLoc(), yymsp[0].minor.yy223->endLoc());   yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3884 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 127: /* formula ::= formula DBL_PLUS formula */
      case 128: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==128);
      case 170: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==170);
      case 171: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==171);
      case 193: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==193);
      case 194: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==194);
#line 823 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy223, yymsp[-2].minor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, BinaryFormula::Operator::OR); }
#line 3894 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 129: /* formula ::= formula EQUIV formula */
      case 172: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==172);
      case 195: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==195);
#line 825 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy223, yymsp[-2].minor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, BinaryFormula::Operator::EQUIV); }
#line 3901 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* formula ::= formula IMPL formula */
      case 131: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==131);
      case 173: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==173);
      case 174: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==174);
      case 196: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==196);
      case 197: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==197);
#line 826 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy223, yymsp[-2].minor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, BinaryFormula::Operator::IMPL); }
#line 3911 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* formula_base ::= comparison */
      case 175: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==175);
      case 215: /* head_formula ::= comparison */ yytestcase(yyruleno==215);
#line 829 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = yymsp[0].minor.yy223; }
#line 3918 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* formula_base ::= atomic_formula */
      case 216: /* head_formula ::= atomic_head_formula */ yytestcase(yyruleno==216);
#line 830 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = yymsp[0].minor.yy262; }
#line 3924 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* formula_base ::= formula_quant */
#line 831 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = yymsp[0].minor.yy381; }
#line 3929 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* formula_base ::= formula_card */
#line 833 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy223 = yymsp[0].minor.yy223;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy223->beginLoc());
			YYERROR;
		}
	}
#line 3940 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* formula_base ::= TRUE */
      case 176: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==176);
      case 218: /* head_formula ::= TRUE */ yytestcase(yyruleno==218);
#line 840 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3947 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* formula_base ::= FALSE */
      case 177: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==177);
      case 219: /* head_formula ::= FALSE */ yytestcase(yyruleno==219);
#line 841 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3954 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* comparison ::= term_strong EQ term */
      case 145: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==145);
      case 178: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==178);
#line 843 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3962 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* comparison ::= term_strong DBL_EQ term */
      case 146: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==146);
      case 179: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==179);
#line 844 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3970 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* comparison ::= term_strong NEQ term */
      case 147: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==147);
      case 180: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==180);
#line 845 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3978 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* comparison ::= term_strong LTHAN term */
      case 148: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==148);
      case 181: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==181);
#line 846 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3986 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* comparison ::= term_strong GTHAN term */
      case 149: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==149);
      case 182: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==182);
#line 847 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3994 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* comparison ::= term_strong LTHAN_EQ term */
      case 150: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==150);
      case 183: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==183);
#line 848 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4002 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 144: /* comparison ::= term_strong GTHAN_EQ term */
      case 151: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==151);
      case 184: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==184);
#line 849 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 4010 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* comparison ::= constant DBL_EQ term */
#line 857 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4016 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* comparison ::= constant NEQ term */
#line 858 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 4022 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* comparison ::= constant LTHAN term */
#line 859 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4028 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* comparison ::= constant GTHAN term */
#line 860 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4034 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* comparison ::= constant LTHAN_EQ term */
#line 861 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4040 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= constant GTHAN_EQ term */
#line 862 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 4046 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* atomic_formula ::= constant */
      case 162: /* atomic_formula_anon ::= const_anon */ yytestcase(yyruleno==162);
      case 185: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==185);
#line 889 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy262, yymsp[0].minor.yy327, true); }
#line 4053 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* atomic_formula ::= TILDE constant */
      case 163: /* atomic_formula_anon ::= TILDE const_anon */ yytestcase(yyruleno==163);
      case 186: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==186);
#line 890 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy262, yymsp[0].minor.yy327, false);   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 4061 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* atomic_formula ::= constant EQ term */
      case 164: /* atomic_formula_anon ::= const_anon EQ term */ yytestcase(yyruleno==164);
      case 187: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==187);
#line 891 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy262 = new AtomicFormula(yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());	  yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4069 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 161: /* atomic_formula_anon ::= atomic_formula */
      case 220: /* atomic_head_formula ::= atomic_formula */ yytestcase(yyruleno==220);
      case 306: /* show_elem ::= atomic_formula_one_const */ yytestcase(yyruleno==306);
#line 893 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy262 = yymsp[0].minor.yy262; }
#line 4076 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 198: /* formula_temporal ::= term_strong COLON formula_temporal */
#line 972 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy223, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy223); }
#line 4081 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 199: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
#line 984 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy381=NULL;
		ref_ptr<const Token> bl_ptr = yymsp[-4].minor.yy0;
		ref_ptr<QuantifierFormula::QuantifierList> lst_ptr = yymsp[-3].minor.yy147;
		ref_ptr<Formula> sub_ptr = yymsp[-1].minor.yy223;
		ref_ptr<const Token> br_ptr = yymsp[0].minor.yy0;

		if (!parser->lang()->support(Language::Feature::FORMULA_QUANTIFIER)) {
			parser->_feature_error(Language::Feature::FORMULA_QUANTIFIER, &yymsp[-4].minor.yy0->beginLoc());
			YYERROR;
		} else yygotominor.yy381 = new QuantifierFormula(yymsp[-3].minor.yy147, yymsp[-1].minor.yy223, yymsp[-4].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
	  yy_destructor(yypParser,99,&yymsp[-2].minor);
}
#line 4098 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 200: /* quant_lst ::= quant_op variable */
#line 998 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy147 = new QuantifierFormula::QuantifierList();
		yygotominor.yy147->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy111, yymsp[0].minor.yy163));
	}
#line 4106 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 201: /* quant_lst ::= quant_lst quant_op variable */
#line 1004 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy147 = yymsp[-2].minor.yy147;
		yygotominor.yy147->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy111, yymsp[0].minor.yy163));
	}
#line 4114 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 202: /* quant_op ::= BIG_CONJ */
#line 1009 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy111 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4120 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 203: /* quant_op ::= BIG_DISJ */
#line 1010 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy111 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,92,&yymsp[0].minor);
}
#line 4126 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 204: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
#line 1056 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy125, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, NULL);  }
#line 4131 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 205: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
#line 1057 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, yymsp[-4].minor.yy88, yymsp[-3].minor.yy0, yymsp[-2].minor.yy125, yymsp[-1].minor.yy223,  yymsp[0].minor.yy0, NULL);  }
#line 4136 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 206: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 1058 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy125, yymsp[-2].minor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4141 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 207: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 1059 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, yymsp[-5].minor.yy88, yymsp[-4].minor.yy0, yymsp[-3].minor.yy125, yymsp[-2].minor.yy223,  yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4146 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
#line 1060 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, NULL);  }
#line 4151 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 209: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
#line 1061 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, yymsp[-3].minor.yy88, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy223,  yymsp[0].minor.yy0, NULL);  }
#line 4156 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
#line 1062 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4161 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
#line 1063 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, yymsp[-4].minor.yy88, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy223,  yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4166 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1067 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy125 = yymsp[-1].minor.yy125;
	  yy_destructor(yypParser,99,&yymsp[0].minor);
}
#line 4174 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* card_var_lst_inner ::= variable */
#line 1072 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy163;
		yygotominor.yy125 = new CardinalityFormula::VariableList();
		yygotominor.yy125->push_back(yymsp[0].minor.yy163->symbol());
	}
#line 4183 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1079 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy163;
		yygotominor.yy125 = yymsp[-2].minor.yy125;
		yygotominor.yy125->push_back(yymsp[0].minor.yy163->symbol());
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4193 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* head_formula ::= formula_smpl_card */
#line 1101 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy223 = yymsp[0].minor.yy91;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy91->beginLoc());
			YYERROR;
		}
	}
#line 4204 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 221: /* atomic_head_formula ::= DASH constant */
#line 1114 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy262 = NULL;
		ref_ptr<const Token> d_ptr = yymsp[-1].minor.yy0;
		ref_ptr<Constant> c_ptr = yymsp[0].minor.yy327;

		if (!parser->lang()->support(Language::Feature::FORMULA_NOT_DASH_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_NOT_DASH_HEAD);
			YYERROR;
		} else {
			ATOMIC_FORMULA(yygotominor.yy262, yymsp[0].minor.yy327, false); 
		}
	}
#line 4220 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1127 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy125, yymsp[-1].minor.yy262, yymsp[0].minor.yy0, NULL);  }
#line 4225 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 223: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1128 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, yymsp[-4].minor.yy88, yymsp[-3].minor.yy0, yymsp[-2].minor.yy125, yymsp[-1].minor.yy262,  yymsp[0].minor.yy0, NULL);  }
#line 4230 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 224: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1129 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy125, yymsp[-2].minor.yy262, yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4235 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 225: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1130 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, yymsp[-5].minor.yy88, yymsp[-4].minor.yy0, yymsp[-3].minor.yy125, yymsp[-2].minor.yy262,  yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4240 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 226: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1131 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy262, yymsp[0].minor.yy0, NULL);  }
#line 4245 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 227: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1132 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, yymsp[-3].minor.yy88, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy262,  yymsp[0].minor.yy0, NULL);  }
#line 4250 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 228: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1133 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy262, yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4255 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 229: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1134 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, yymsp[-4].minor.yy88, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy262,  yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4260 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 230: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1153 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy101 = NULL;
        ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
        ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
        ref_ptr<MacroDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy179;
        ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;

        if (!parser->lang()->support(Language::Feature::DECL_MACRO)) {
            parser->_feature_error(Language::Feature::DECL_MACRO, &yymsp[-2].minor.yy0->beginLoc());
            YYERROR;
        } else {
		    BOOST_FOREACH(symbols::MacroSymbol* m, *yymsp[-1].minor.yy179) {
			    if (!parser->symtab()->create(m)) {
	    	        // Check if it's a duplicate
	    	        symbols::MacroSymbol* m2 = (symbols::MacroSymbol*)parser->symtab()->resolve(symbols::Symbol::Type::MACRO, *m->base(), m->arity());
		            if (!m2 || m2 != m) {
		                parser->_parse_error("Detected conflicting definition of symbol \"" + *m->name() + "\".", &yygotominor.yy101->beginLoc());
		            } else {
		                parser->_parse_error("Detected a duplicate definition of symbol \"" + *m->name() + "\".", &yygotominor.yy101->beginLoc());
		            }
		        }
		    }

			yygotominor.yy101 = new MacroDeclaration(yymsp[-1].minor.yy179, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
        }
    }
#line 4290 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 231: /* macro_def_lst ::= macro_bnd */
#line 1181 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy179 = new MacroDeclaration::ElementList();
        yygotominor.yy179->push_back(yymsp[0].minor.yy403);
    }
#line 4298 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1187 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy179 = yymsp[-2].minor.yy179;
        yygotominor.yy179->push_back(yymsp[0].minor.yy403);
      yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4307 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 233: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1193 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy220;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy403 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy220);
      yy_destructor(yypParser,72,&yymsp[-4].minor);
  yy_destructor(yypParser,73,&yymsp[-2].minor);
  yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4321 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 234: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1202 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy403 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4332 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* macro_args ::= macro_arg */
#line 1210 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy220 = new MacroSymbol::ArgumentList();
        yygotominor.yy220->push_back(yymsp[0].minor.yy93->str());
        delete yymsp[0].minor.yy93;
    }
#line 4341 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 236: /* macro_args ::= macro_args COMMA macro_arg */
#line 1216 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy220 = yymsp[-2].minor.yy220;
        yygotominor.yy220->push_back(yymsp[0].minor.yy93->str());
        delete yymsp[0].minor.yy93;
      yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4351 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 237: /* macro_arg ::= POUND_INTEGER */
      case 238: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==238);
#line 1223 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy93 = yymsp[0].minor.yy0;
    }
#line 4359 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 239: /* sort_lst ::= sort */
#line 1250 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy305 = new ConstantSymbol::SortList();
		yygotominor.yy305->push_back(yymsp[0].minor.yy315);
	}
#line 4367 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 240: /* sort_lst ::= sort_lst COMMA sort */
#line 1255 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy305 = yymsp[-2].minor.yy305;
		yygotominor.yy305->push_back(yymsp[0].minor.yy315);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4376 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 241: /* sort ::= sort_id_nr */
      case 247: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==247);
      case 248: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==248);
#line 1280 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy315 = yymsp[0].minor.yy315; }
#line 4383 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 242: /* sort ::= sort_id_nr STAR */
#line 1281 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy315, yymsp[-1].minor.yy315, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::NONE)); }
#line 4388 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 243: /* sort ::= sort_id_nr CARROT */
#line 1282 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy315, yymsp[-1].minor.yy315, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::UNKNOWN)); }
#line 4393 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 244: /* sort ::= sort PLUS object_nullary */
#line 1284 "bcplus/parser/detail/lemon_parser.y"
{ u::ref_ptr<const Object> o_ptr = yymsp[0].minor.yy208; DYNAMIC_SORT_PLUS(yygotominor.yy315, yymsp[-2].minor.yy315, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, yymsp[0].minor.yy208->symbol()); }
#line 4398 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 245: /* sort ::= sort PLUS IDENTIFIER */
#line 1287 "bcplus/parser/detail/lemon_parser.y"
{
												  u::ref_ptr<const Referenced> s_ptr = yymsp[-2].minor.yy315, op_ptr = yymsp[-1].minor.yy0, id_ptr = yymsp[0].minor.yy0;
												  u::ref_ptr<const ObjectSymbol> obj = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
												  if(!obj) {
													if (parser->lang()->support(Language::Feature::SORT_PLUS)) 
														parser->_parse_error("\"" + *yymsp[0].minor.yy0->str() + "\" could not be declared as an object as this conflicts with a previous declarations of this identifier.", &yymsp[0].minor.yy0->beginLoc());
													else 
														parser->_feature_error(Language::Feature::SORT_PLUS, &yymsp[-1].minor.yy0->beginLoc());
													YYERROR;
												  } else {
													DYNAMIC_SORT_PLUS(yygotominor.yy315, yymsp[-2].minor.yy315, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, obj);
												  }
												}
#line 4415 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 246: /* sort ::= sort PLUS INTEGER */
#line 1301 "bcplus/parser/detail/lemon_parser.y"
{ 
												  ref_ptr<const Object> t_ptr;
												  BASIC_TERM(t_ptr, yymsp[0].minor.yy0);
												  DYNAMIC_SORT_PLUS(yygotominor.yy315, yymsp[-2].minor.yy315, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, t_ptr->symbol()); 
												}
#line 4424 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 249: /* sort_nr ::= num_range */
#line 1312 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy331;

		yygotominor.yy315 = NULL;

		if (!parser->lang()->support(Language::Feature::NUMRANGE_SORT)) {
			parser->_feature_error(Language::Feature::NUMRANGE_SORT, &yymsp[0].minor.yy331->beginLoc());
			YYERROR;
		}

		// X..Y becomes __sort_X_Y__
		std::string name = "__sort_" + boost::lexical_cast<std::string>(yymsp[0].minor.yy331->min()) + "__" + boost::lexical_cast<std::string>(yymsp[0].minor.yy331->max()) + "__";

		ref_ptr<SortSymbol::ObjectList> objs = new SortSymbol::ObjectList();

		// Generate the objects that it will have
		for (int i = yymsp[0].minor.yy331->min(); i <= yymsp[0].minor.yy331->max(); i++) {
			std::string obj_name = boost::lexical_cast<std::string>(i);
			ObjectSymbol const* sym = parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(obj_name)));

			if (!sym) {
				yygotominor.yy315 = NULL;
				parser->_parse_error("An error occurred creating symbol \"" + obj_name + "/0\".", &yymsp[0].minor.yy331->beginLoc());
				YYERROR;
			}
			objs->insert(sym);
		}		

		// dynamically declare the sort
		yygotominor.yy315 = parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name), objs));
		if (!yygotominor.yy315) {
				parser->_parse_error("An error occurred creating symbol \"" + name + "/0\".", &yymsp[0].minor.yy331->beginLoc());
				YYERROR;
		} 
	}
#line 4463 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 250: /* sort_id ::= IDENTIFIER */
#line 1349 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy315 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy315) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4476 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 251: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1380 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ConstantDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy55;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_CONSTANT)) {
			yygotominor.yy5 = NULL;
			parser->_feature_error(Language::Feature::DECL_CONSTANT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy5 = new ConstantDeclaration(yymsp[-1].minor.yy55, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

		}
	}
#line 4495 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 252: /* constant_bnd_lst ::= constant_bnd */
#line 1397 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy55 = yymsp[0].minor.yy55;
	}
#line 4502 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 253: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1402 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy55;
		yygotominor.yy55 = yymsp[-2].minor.yy55;
		yygotominor.yy55->splice(yygotominor.yy55->end(), *yymsp[0].minor.yy55);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4512 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 254: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1422 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const SortSymbol> s_ptr = yymsp[-1].minor.yy315;
		ref_ptr<const Referenced> names_ptr = yymsp[-5].minor.yy484;
		yygotominor.yy55 = new ConstantDeclaration::ElementList();

		// NOTE: additive constants default to the additive sort, not the boolean sort
		if (yymsp[-3].minor.yy409 & ConstantSymbol::Type::M_ADDITIVE) s_ptr = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

		// external constants should have "unknown" in their sort
		else if (yymsp[-3].minor.yy409 & ConstantSymbol::Type::M_EXTERNAL) s_ptr = parser->symtab()->carrot(yymsp[-1].minor.yy315);

		// non-boolean abActions should contain "none"
		else if (yymsp[-3].minor.yy409 == ConstantSymbol::Type::ABACTION && s_ptr->domainType() != DomainType::BOOLEAN) s_ptr = parser->symtab()->star(yymsp[-1].minor.yy315);

		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-5].minor.yy484) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[-3].minor.yy409, decl.first->str(), s_ptr, decl.second);
			yygotominor.yy55->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,77,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4540 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 255: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1444 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy484, s_ptr = yymsp[0].minor.yy315;
		yygotominor.yy55 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy484) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(ConstantSymbol::Type::RIGID, decl.first->str(), yymsp[0].minor.yy315, decl.second);
			yygotominor.yy55->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4555 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 256: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1455 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy484;
		yygotominor.yy55 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy484) {
			// attempt to declare each symbol
			ref_ptr<SortSymbol> s = parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN);

			// NOTE: additive constants default to the additive sort, not the boolean sort
			if (yymsp[0].minor.yy409 & ConstantSymbol::Type::M_ADDITIVE) s = parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE);

			// external constants should have "unknown" in their sort
			else if (yymsp[0].minor.yy409 & ConstantSymbol::Type::M_EXTERNAL) s = parser->symtab()->carrot(s);

			// non-boolean abActions should contain "none"
			else if (yymsp[0].minor.yy409 == ConstantSymbol::Type::ABACTION && s->domainType() != DomainType::BOOLEAN) s = parser->symtab()->star(s);


			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[0].minor.yy409, decl.first->str(), parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN), decl.second);
			yygotominor.yy55->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4582 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 257: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1478 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy55 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-4].minor.yy484, s_ptr = yymsp[-2].minor.yy152, id_ptr = yymsp[0].minor.yy0;


		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[0].minor.yy0->str());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\" is not a valid constant symbol.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[0].minor.yy0->str(), 0) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy55 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-4].minor.yy484) {
				ref_ptr<ConstantSymbol> c= new AttributeSymbol(decl.first->str(), yymsp[-2].minor.yy152, c, decl.second);
				yygotominor.yy55->push_back(c);
				CONSTANT_DECL(c, decl.first->beginLoc());
			}
		}
	  yy_destructor(yypParser,77,&yymsp[-3].minor);
  yy_destructor(yypParser,55,&yymsp[-1].minor);
}
#line 4611 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 258: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1502 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy55 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-7].minor.yy484, s_ptr = yymsp[-5].minor.yy152, id_ptr = yymsp[-3].minor.yy0, lst_ptr = yymsp[-1].minor.yy305;

		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy305->size());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy305->size()) + "\" is not a valid constant symbol.", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy305->size()) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// ensure that the sorts match the declaration of the symbol
			SortList::const_iterator it = yymsp[-1].minor.yy305->begin();
			BOOST_FOREACH(SortSymbol const* sort, *c) {
				if (*it != sort) {
					// check to see if it'yymsp[-5].minor.yy152 a subsort, which is also permissable
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

			yygotominor.yy55 = new ConstantDeclaration::ElementList();
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-7].minor.yy484) {
				// ensure that the sorts match the start of the sort list for each of the symbols
				if (decl.second->size() < yymsp[-1].minor.yy305->size()) {
					parser->_parse_error("Detected a malformed attribute declaration. An attribute must duplicate its parent'yymsp[-5].minor.yy152 parameters.", &decl.first->beginLoc());
					YYERROR;
				} else {
					bool good_sort = true;		
					it = decl.second->begin();
					BOOST_FOREACH(SortSymbol const* sort, *yymsp[-1].minor.yy305) {
						if (*it != sort) {
							// check to see if it'yymsp[-5].minor.yy152 a subsort, which is also permissable
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
						ref_ptr<ConstantSymbol> sym = new AttributeSymbol(decl.first->str(), yymsp[-5].minor.yy152, c, decl.second);
						yygotominor.yy55->push_back(sym);
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
#line 4692 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 259: /* constant_dcl_lst ::= IDENTIFIER */
#line 1578 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = new IdentifierDeclList();
		yygotominor.yy484->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 4700 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 260: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1583 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = new IdentifierDeclList();
		yygotominor.yy484->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy305));
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4710 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 261: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1588 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = yymsp[-2].minor.yy484;
		yygotominor.yy484->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4719 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 262: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1593 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = yymsp[-5].minor.yy484;
		yygotominor.yy484->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy305));
	  yy_destructor(yypParser,103,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4730 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 263: /* constant_dcl_type ::= ABACTION */
#line 1600 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4742 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 264: /* constant_dcl_type ::= ACTION */
#line 1609 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4754 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1618 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4766 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1627 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4778 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 267: /* constant_dcl_type ::= EXTERNALACTION */
#line 1636 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4790 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1645 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4802 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 269: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1654 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4814 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 270: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1663 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4826 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 271: /* constant_dcl_type ::= RIGID */
#line 1672 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4838 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 272: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1681 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4850 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 273: /* constant_dcl_type ::= SDFLUENT */
#line 1691 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4862 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 274: /* attrib_spec ::= ATTRIBUTE */
#line 1701 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy152 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[0].minor.yy0;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// grab the boolean sort and provide it
			yygotominor.yy152 = parser->symtab()->star(parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN));
		}
	}
#line 4877 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 275: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1714 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy152 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[-3].minor.yy0, s_ptr = yymsp[-1].minor.yy315;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy152 = parser->symtab()->star(yymsp[-1].minor.yy315);
		}
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4893 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 276: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1742 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ObjectDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy485;
		
		if (!parser->lang()->support(Language::Feature::DECL_OBJECT)) {
			yygotominor.yy78 = NULL;
			parser->_feature_error(Language::Feature::DECL_OBJECT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy78 = new ObjectDeclaration(yymsp[-1].minor.yy485, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ObjectDeclaration::Element* bnd, *yymsp[-1].minor.yy485) {
				BOOST_FOREACH(ObjectSymbol const* o, *bnd) {
						bnd->sort()->add(o);
				}
			}
		}
	}
#line 4918 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 277: /* object_bnd_lst ::= object_bnd */
#line 1765 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy485 = new ObjectDeclaration::ElementList();
		yygotominor.yy485->push_back(yymsp[0].minor.yy190);
	}
#line 4926 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 278: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 1771 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy485 = yymsp[-2].minor.yy485;
		yygotominor.yy485->push_back(yymsp[0].minor.yy190);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4935 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 279: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 1777 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy190 = new ObjectDeclaration::Element(yymsp[0].minor.yy315, yymsp[-2].minor.yy148);
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4943 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 280: /* object_lst ::= object_spec */
#line 1782 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy148 = yymsp[0].minor.yy148;
	}
#line 4950 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 281: /* object_lst ::= object_lst COMMA object_spec */
#line 1786 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy148 = yymsp[-2].minor.yy148;
		yygotominor.yy148->splice(yygotominor.yy148->end(), *yymsp[0].minor.yy148);
		delete yymsp[0].minor.yy148;
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4960 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 282: /* object_spec ::= IDENTIFIER */
#line 1795 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		yygotominor.yy148 = NULL;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy148 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy148->push_back(o);
		}
	}
#line 4976 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 283: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1808 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy148 = NULL;
		ref_ptr<ObjectSymbol::SortList> lst_ptr = yymsp[-1].minor.yy305;
		ref_ptr<const Token> id_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy305));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(),yymsp[-1].minor.yy305->size()) + "\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy148 = new  ObjectDeclaration::Element::ObjectList();
			yygotominor.yy148->push_back(o);
		}
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4995 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 284: /* object_spec ::= num_range */
#line 1822 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy148 = new ObjectDeclaration::Element::ObjectList();
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy331;

		// iterate over the range and add it to the list
		for (int i = yymsp[0].minor.yy331->min(); i <= yymsp[0].minor.yy331->max(); i++) {
			std::string name = boost::lexical_cast<std::string>(i);
			ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(name)));
			if (!o) {
				parser->_parse_error("INTERNAL ERROR: Could not create object symbol \"" + Symbol::genName(name, 0) + "\".", &yymsp[0].minor.yy331->beginLoc());
				YYERROR;
			} else {
				yygotominor.yy148->push_back(o);
			}
		}
	}
#line 5015 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 285: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 1854 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<VariableDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy97;
		
		if (!parser->lang()->support(Language::Feature::DECL_VARIABLE)) {
			yygotominor.yy487 = NULL;
			parser->_feature_error(Language::Feature::DECL_VARIABLE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy487 = new VariableDeclaration(yymsp[-1].minor.yy97, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(VariableSymbol* v, *yymsp[-1].minor.yy97) {
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
#line 5046 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 286: /* variable_bnd_lst ::= variable_bnd */
#line 1883 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = yymsp[0].minor.yy97;
	}
#line 5053 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 1888 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = yymsp[-2].minor.yy97;
		yygotominor.yy97->splice(yygotominor.yy97->end(), *yymsp[0].minor.yy97);
		delete yymsp[0].minor.yy97;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5063 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* variable_bnd ::= variable_lst DBL_COLON sort_id */
#line 1895 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy408) {
			yygotominor.yy97->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy315));
		}
		delete yymsp[-2].minor.yy408;
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5076 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* variable_lst ::= IDENTIFIER */
#line 1905 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy408 = new TokenList();
		yygotominor.yy408->push_back(yymsp[0].minor.yy0);
	}
#line 5084 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 1910 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy408 = yymsp[-2].minor.yy408;
		yygotominor.yy408->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5093 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 1931 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<SortDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy245;
		
		if (!parser->lang()->support(Language::Feature::DECL_SORT)) {
			yygotominor.yy195 = NULL;
			parser->_feature_error(Language::Feature::DECL_SORT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy195 = new SortDeclaration(yymsp[-1].minor.yy245, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5111 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* sort_bnd_lst ::= sort_bnd */
      case 294: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==294);
#line 1947 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy245 = yymsp[0].minor.yy245;
	}
#line 5119 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 293: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 1952 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy245 = yymsp[-2].minor.yy245;
		yygotominor.yy245->splice(yygotominor.yy245->end(), *yymsp[0].minor.yy245);
		delete yymsp[0].minor.yy245;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5129 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 1964 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy245) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy245) {
				sym2->addSubSort(sym);
			}
		}
		yygotominor.yy245 = yymsp[-2].minor.yy245;
		yygotominor.yy245->splice(yymsp[-2].minor.yy245->end(), *yymsp[0].minor.yy245);
		delete yymsp[0].minor.yy245;

	  yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 5145 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 1976 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy245) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy245) {
				sym->addSubSort(sym2);
			}
		}
		yygotominor.yy245 = yymsp[-2].minor.yy245;
		yygotominor.yy245->splice(yymsp[-2].minor.yy245->end(), *yymsp[0].minor.yy245);
		delete yymsp[0].minor.yy245;
	  yy_destructor(yypParser,100,&yymsp[-1].minor);
}
#line 5160 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 1987 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy245 = yymsp[-1].minor.yy245;
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 5169 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* sort_dcl_lst ::= IDENTIFIER */
#line 1992 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yygotominor.yy245 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy245 = new SortDeclaration::ElementList();
			yygotominor.yy245->push_back(s);
		}

		delete yymsp[0].minor.yy0;
	}
#line 5186 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 2006 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy245 = yymsp[-2].minor.yy245;
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yymsp[-2].minor.yy245 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yymsp[-2].minor.yy245->push_back(s);
		}

		delete yymsp[0].minor.yy0;

	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5205 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 2033 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy230 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<ShowStatement::ElementList> lst_ptr = yymsp[-1].minor.yy165;

		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy230 = new ShowStatement(yymsp[-1].minor.yy165, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5221 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 2047 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy230 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_SHOW_ALL)) {
			parser->_feature_error(Language::Feature::DECL_SHOW_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy230 = new ShowAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5239 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 2064 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy230 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<HideStatement::ElementList> lst_ptr = yymsp[-1].minor.yy165;

		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy230 = new HideStatement(yymsp[-1].minor.yy165, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5255 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2078 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy230 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_HIDE_ALL)) {
			parser->_feature_error(Language::Feature::DECL_HIDE_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy230 = new HideAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5273 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* show_lst ::= show_elem */
#line 2096 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165 = new ShowStatement::ElementList();
		yygotominor.yy165->push_back(yymsp[0].minor.yy262);
	}
#line 5281 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 305: /* show_lst ::= show_lst COMMA show_elem */
#line 2101 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165 = yymsp[-2].minor.yy165;
		yygotominor.yy165->push_back(yymsp[0].minor.yy262);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5290 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* stmt_noconcurrency ::= NOCONCURRENCY PERIOD */
#line 2129 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy87, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5295 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD */
#line 2130 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy158, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5300 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD */
#line 2156 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy230, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy458, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5306 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD */
#line 2157 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy230, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy458, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5312 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2182 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy140 = NULL;
		ref_ptr<const Referenced> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, data_l_ptr = yymsp[-1].minor.yy431.l, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Referenced> data_maxstep_ptr = yymsp[-1].minor.yy431.maxstep, data_label_ptr = yymsp[-1].minor.yy431.label;

		ref_ptr<const ReferencedString> label;
		if (yymsp[-1].minor.yy431.label) label = yymsp[-1].minor.yy431.label->str();
		else label = new ReferencedString("0");

		int min = -1, max = -1;
		if (yymsp[-1].minor.yy431.maxstep) {
			min = yymsp[-1].minor.yy431.maxstep->min();
			max = yymsp[-1].minor.yy431.maxstep->max();
		}

		if (!parser->lang()->support(Language::Feature::DECL_QUERY)) {
			parser->_feature_error(Language::Feature::DECL_QUERY, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			bool good = true;

			// resolve the query label
			ref_ptr<QuerySymbol> sym = new QuerySymbol(label, min, max);
			if (!parser->symtab()->create(sym)) {
				parser->_parse_error("Could not create query, the label \"" + *label + "\" already exists.", (yymsp[-1].minor.yy431.label ? &yymsp[-1].minor.yy431.label->beginLoc() : &yymsp[-2].minor.yy0->beginLoc()));
				good = false;
				YYERROR;
			}


			if (good) yygotominor.yy140 = new QueryStatement(sym, yymsp[-1].minor.yy431.l, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5349 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* query_lst ::= formula_temporal */
#line 2218 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy431.l = new QueryStatement::FormulaList();
		yygotominor.yy431.maxstep = NULL;
		yygotominor.yy431.label = NULL;

		yygotominor.yy431.l->push_back(yymsp[0].minor.yy223);
	}
#line 5360 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* query_lst ::= query_maxstep_decl */
#line 2227 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy431.l = new QueryStatement::FormulaList();
		yygotominor.yy431.maxstep = yymsp[0].minor.yy221;
		yygotominor.yy431.label = NULL;
	}
#line 5369 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* query_lst ::= query_label_decl */
#line 2234 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy431.l = new QueryStatement::FormulaList();
		yygotominor.yy431.maxstep = NULL;
		yygotominor.yy431.label = yymsp[0].minor.yy93;
	}
#line 5378 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2241 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy431 = yymsp[-2].minor.yy431;
		yymsp[-2].minor.yy431.l->push_back(yymsp[0].minor.yy223);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5387 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2247 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy431 = yymsp[-2].minor.yy431;

		if (yygotominor.yy431.maxstep) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy221->beginLoc());
			delete yymsp[0].minor.yy221;
			YYERROR;
		} else {
			yygotominor.yy431.maxstep = yymsp[0].minor.yy221;
		}
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5403 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2260 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy431 = yymsp[-2].minor.yy431;
		if (yygotominor.yy431.label) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy93->beginLoc());
			delete yymsp[0].minor.yy93;
			YYERROR;

		} else {
			yygotominor.yy431.label = yymsp[0].minor.yy93;
		}
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5419 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2286 "bcplus/parser/detail/lemon_parser.y"
{ 
	yygotominor.yy221 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, i_ptr = yymsp[0].minor.yy0;


	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {

		int max = -1;
		try {
			max = boost::lexical_cast<int>(*yymsp[0].minor.yy0->str());
			yygotominor.yy221 = new NumberRange(-1, max, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		} catch (boost::bad_lexical_cast const& e) {
			parser->_parse_error("INTERNAL ERROR: An error occurred extracting an integer from \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5444 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range */
#line 2307 "bcplus/parser/detail/lemon_parser.y"
{
	yygotominor.yy221 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, nr_ptr = yymsp[0].minor.yy331;

	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {
		yygotominor.yy221 = yymsp[0].minor.yy331;
		nr_ptr.release();
	}
  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5461 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 320: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 321: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==321);
#line 2321 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy93, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5468 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 322: /* clause_if ::= IF formula */
#line 2356 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, Language::Feature::CLAUSE_IF); 		}
#line 5473 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 323: /* clause_if ::= */
      case 325: /* clause_after ::= */ yytestcase(yyruleno==325);
      case 327: /* clause_ifcons ::= */ yytestcase(yyruleno==327);
      case 331: /* clause_where ::= */ yytestcase(yyruleno==331);
#line 2357 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = NULL; }
#line 5481 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 324: /* clause_after ::= AFTER formula */
#line 2358 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, Language::Feature::CLAUSE_AFTER);	}
#line 5486 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 326: /* clause_ifcons ::= IFCONS formula */
#line 2360 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, Language::Feature::CLAUSE_IFCONS); 	}
#line 5491 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 328: /* clause_unless ::= UNLESS atomic_formula_anon */
#line 2362 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy262, yymsp[-1].minor.yy0, yymsp[0].minor.yy262, Language::Feature::CLAUSE_UNLESS); 	}
#line 5496 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* clause_unless ::= */
#line 2363 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy262 = NULL; }
#line 5501 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 330: /* clause_where ::= WHERE formula_no_const */
#line 2364 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, Language::Feature::CLAUSE_WHERE); 	}
#line 5506 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 332: /* stmt_law ::= law_basic */
      case 333: /* stmt_law ::= law_caused */ yytestcase(yyruleno==333);
      case 334: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==334);
      case 335: /* stmt_law ::= law_impl */ yytestcase(yyruleno==335);
      case 336: /* stmt_law ::= law_causes */ yytestcase(yyruleno==336);
      case 337: /* stmt_law ::= law_increments */ yytestcase(yyruleno==337);
      case 338: /* stmt_law ::= law_decrements */ yytestcase(yyruleno==338);
      case 339: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==339);
      case 340: /* stmt_law ::= law_always */ yytestcase(yyruleno==340);
      case 341: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==341);
      case 342: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==342);
      case 343: /* stmt_law ::= law_never */ yytestcase(yyruleno==343);
      case 344: /* stmt_law ::= law_default */ yytestcase(yyruleno==344);
      case 345: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==345);
      case 346: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==346);
      case 347: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==347);
      case 348: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==348);
      case 349: /* stmt_law ::= law_observed */ yytestcase(yyruleno==349);
#line 2410 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy230 = yymsp[0].minor.yy230;}
#line 5528 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2526 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, NULL, yymsp[-6].minor.yy223, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
																																															Language::Feature::LAW_BASIC_D, BasicLaw); }
#line 5535 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2530 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, yymsp[-7].minor.yy0, yymsp[-6].minor.yy223, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 5542 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2534 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, yymsp[-7].minor.yy0, yymsp[-6].minor.yy223, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 5549 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2538 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy230, yymsp[-4].minor.yy223, yymsp[-3].minor.yy0, yymsp[-2].minor.yy223, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 5555 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2541 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy230, yymsp[-6].minor.yy262, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 5561 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2545 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy230, yymsp[-8].minor.yy262, yymsp[-7].minor.yy0, yymsp[-6].minor.yy327, yymsp[-4].minor.yy88, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5568 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2548 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy230, yymsp[-8].minor.yy262, yymsp[-7].minor.yy0, yymsp[-6].minor.yy327, yymsp[-4].minor.yy88, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_DECREMENTS, DecrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5575 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2552 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy230, yymsp[-6].minor.yy262, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 5581 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2556 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy230, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 5588 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2560 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy230, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 5595 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2564 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy230, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 5602 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2568 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy230, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 5609 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2572 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, yymsp[-7].minor.yy0, yymsp[-6].minor.yy262, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 5616 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2576 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, yymsp[-7].minor.yy0, yymsp[-6].minor.yy327, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 5623 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2580 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, yymsp[-7].minor.yy0, yymsp[-6].minor.yy327, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 5630 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 365: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2584 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy230, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 5636 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2588 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy230, yymsp[-3].minor.yy0, yymsp[-2].minor.yy327, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 5642 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD */
#line 2593 "bcplus/parser/detail/lemon_parser.y"
{ 
			yygotominor.yy230 = NULL;
			ref_ptr<const Token> kw_ptr = yymsp[-4].minor.yy0, p_ptr = yymsp[0].minor.yy0;
			ref_ptr<AtomicFormula> head_ptr = yymsp[-3].minor.yy262;
			ref_ptr<Term> t_ptr = yymsp[-1].minor.yy88;

			// make sure that the At clause is integral
			if (yymsp[-1].minor.yy88->domainType() != DomainType::INTEGRAL) {
				parser->_parse_error("Expected an integral expression.", &yymsp[-1].minor.yy88->beginLoc());
				YYERROR;
			} else {
				LAW_SIMPLE_FORM(yygotominor.yy230, yymsp[-4].minor.yy0, yymsp[-3].minor.yy262, yymsp[-1].minor.yy88, yymsp[0].minor.yy0, Language::Feature::LAW_OBSERVED, ObservedLaw); 
			}
		  yy_destructor(yypParser,66,&yymsp[-2].minor);
}
#line 5661 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* stmt_code_blk ::= ASP_GR */
#line 2627 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 5666 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 369: /* stmt_code_blk ::= ASP_CP */
#line 2628 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 5671 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 370: /* stmt_code_blk ::= F2LP_GR */
#line 2629 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 5676 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 371: /* stmt_code_blk ::= F2LP_CP */
#line 2630 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 5681 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 372: /* stmt_code_blk ::= LUA_GR */
#line 2631 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 5686 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 373: /* stmt_code_blk ::= LUA_CP */
#line 2632 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 5691 "bcplus/parser/detail/lemon_parser.c"
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
#line 5757 "bcplus/parser/detail/lemon_parser.c"
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
