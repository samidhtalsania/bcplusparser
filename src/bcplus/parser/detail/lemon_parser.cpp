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



#line 1259 "bcplus/parser/detail/lemon_parser.y"

		
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
#line 1406 "bcplus/parser/detail/lemon_parser.y"

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
#line 2113 "bcplus/parser/detail/lemon_parser.y"

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

#line 2136 "bcplus/parser/detail/lemon_parser.y"

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
#line 2163 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRange const* maxstep;
		Token const* label;
	};

#line 2270 "bcplus/parser/detail/lemon_parser.y"

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

#line 2342 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2428 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2612 "bcplus/parser/detail/lemon_parser.y"

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
#define YYNRULE 373
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
#define YY_ACTTAB_COUNT (2901)
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   725,  615,  295,  724,  723,  722,  721,  720,  719,  718,
 /*    10 */   717,  716,  715,  714,  713,  712,  711,  710,  726,  683,
 /*    20 */   271,  709,  702,  708,  707,  701,  168,  454,  391,  616,
 /*    30 */   617,  462,  268,  272,  453,  452,  107,  105,  104,  646,
 /*    40 */   347,  630,  443,  683,  271,  709,  702,  373,  707,  701,
 /*    50 */   576,  455,  704,  195,  645,  644,  267,  272,  136,  134,
 /*    60 */   133,  278,  567,  564,  563,  562,  561,  683,  271,  709,
 /*    70 */   702,  708,  707,  701,   55,   22,  397,  187,  185,  184,
 /*    80 */   265,  272,  112,  375,  574,  279,  567,  564,  563,  562,
 /*    90 */   561,  536,  535,  534,  533,  532,  531,  530,  529,  528,
 /*   100 */   527,  526,  525,  524,  523,  522,  521,  520,  519,  682,
 /*   110 */   464,  703,  190,  465,  681,  474,  473,  470,  469,  472,
 /*   120 */   471,  682,  464,  165,  166,  465,  681,  663,  657,  709,
 /*   130 */   702,  708,  707,  701,  618,  619,   10,  398,  320,   54,
 /*   140 */    40,  112,    8,    9,  439,  220,  188,  213,  117,  662,
 /*   150 */   464,  186,  207,  465,  661,  613,  675,  674,  676,    7,
 /*   160 */   429,  428,    6,   53,   39,  199,  430,  580,  675,  674,
 /*   170 */   431,  642,   38,  643,  446,  234,   14,  274,  151,   52,
 /*   180 */   115,  116,  237,  559,  260,  560,   37,  163,  427,  194,
 /*   190 */     4,  639,   36,  262,  165,  166,  260,  462,  682,  464,
 /*   200 */   591,  295,  465,  681,  683,  343,  709,  702,  708,  707,
 /*   210 */   701,  658,  132,  659,  113,   11,  576,  297,  344,   98,
 /*   220 */   139,  393,  597,  392,  641,   21,   34,  468,  650,  682,
 /*   230 */   464,   98,  261,  465,  681,  385,  589,  384,  590,  467,
 /*   240 */   700,  466,  258,  197,  259,  675,  674,  676,  463,  374,
 /*   250 */   574,  467,  700,  466,  137,   26,   25,   29,   28,  135,
 /*   260 */   559,   30,  560,  462,  163,  578,  621,    4,  156,   36,
 /*   270 */    29,   28,  632,  260,   30,   66,  675,  674,  676,  467,
 /*   280 */   700,  466,  461,  683,  671,  709,  702,  708,  707,  701,
 /*   290 */   647,  559,  172,  560,  157,  163,  673,  344,    4,  456,
 /*   300 */    33,  517,   21,   34,  260,   19,   18,  596,   98,   20,
 /*   310 */   514,  504,  709,  702,  708,  707,  701,  467,  683,  273,
 /*   320 */   709,  702,  708,  707,  701,  269,  262,  150,  467,  700,
 /*   330 */   466,  268,  272,   31,   32,  285,  501,  498,  646,   98,
 /*   340 */   591,  295,  427,  683,  271,  709,  702,  708,  707,  701,
 /*   350 */   634,  192,  250,  645,  644,  694,  264,  272,   65,  467,
 /*   360 */   700,  466,  551,  564,  563,  562,  561,  191,  189,  187,
 /*   370 */   185,  184,  290,  588,  630,  614,  595,  384,  590,  699,
 /*   380 */   160,  463,  173,  146,    2,  155,  158,  159,  111,  109,
 /*   390 */   107,  105,  104,  171,   64,  611,  610,  609,  608,   16,
 /*   400 */    15,   19,   18,  447,   63,   20,  371,  540,  539,  112,
 /*   410 */   600,  607,  194,  605,  436,  437,  162,  149,  604,  697,
 /*   420 */   705,  706,  709,  702,  708,  707,  701,  402,  346,  573,
 /*   430 */   603,  601,  602,  606,  683,  271,  709,  702,  373,  707,
 /*   440 */   701,  450,   45,   44,  194,  190,   46,  267,  272,  615,
 /*   450 */   295,  451,  281,  567,  564,  563,  562,  561,  157,  663,
 /*   460 */   657,  709,  702,  708,  707,  701,   23,  397,  513,  464,
 /*   470 */   321,  467,  465,  512,  389,  449,  391,  616,  617,  188,
 /*   480 */   382,  583,  381,  698,  186,  683,  271,  709,  702,  708,
 /*   490 */   707,  701,  111,  109,  107,  105,  104,  648,  264,  272,
 /*   500 */   154,  664,  462,  262,  551,  564,  563,  562,  561,  380,
 /*   510 */   376,  441,  145,  112,  289,  506,  505,  507,  682,  464,
 /*   520 */    62,  460,  465,  681,    5,  170,  397,  680,  615,  295,
 /*   530 */   496,  141,  497,  462,  445,   35,   73,   72,   71,   49,
 /*   540 */    70,   69,   68,   67,   27,   26,   25,   29,   28,  544,
 /*   550 */   543,   30,  576,  387,  449,  391,  616,  617,  463,  700,
 /*   560 */   103,  102,  101,  100,   99,  675,  674,  676,  556,  555,
 /*   570 */   623,  457,   47,   48,  622,  939,   24,  579,  119,  939,
 /*   580 */   642,  147,  643,  144,  542,  541,  577,  151,  585,  115,
 /*   590 */   143,  696,  464,  615,  295,  465,  695,  612,  467,  700,
 /*   600 */   466,  196,  697,  705,  706,  709,  702,  708,  707,  701,
 /*   610 */   401,  346,  683,  271,  709,  702,  708,  707,  701,  390,
 /*   620 */   391,  616,  617,  113,  599,  263,  272,  148,   98,  440,
 /*   630 */   598,  551,  564,  563,  562,  561,  582, 1043,  690,  689,
 /*   640 */   691,  276,  462,  946,  142,  615,  295,  164,  467,  700,
 /*   650 */   466,  591,  295,  692,  148,  693,  167, 1043,  946,  946,
 /*   660 */   148,  459,  110,  683,  271,  709,  702,  708,  707,  701,
 /*   670 */   386,  449,  391,  616,  617,  545,  264,  272,  946,  946,
 /*   680 */   581,  438,  551,  564,  563,  562,  561,  615,  295,  593,
 /*   690 */   575,  946,  291,  396,  633,    3,  108,  946,  572,  636,
 /*   700 */   637,  106,  592,  587,  381,  683,  270,  709,  702,  708,
 /*   710 */   707,  701,  383,  449,  391,  616,  617,  558,  569,  272,
 /*   720 */   183,  467,  700,  466,  551,  564,  563,  562,  561,  680,
 /*   730 */   262,  518,  148,  182,  548,   30,  683,  270,  709,  702,
 /*   740 */   708,  707,  701,   20,   27,   26,   25,   29,   28,  568,
 /*   750 */   272,   30,  394,  635,  553,  567,  564,  563,  562,  561,
 /*   760 */   161,  112,  103,  102,  101,  100,   99,  257,  683,  271,
 /*   770 */   709,  702,  708,  707,  701,  631,  392,  370,   61,  379,
 /*   780 */   441,  267,  272,  378,  441,  463,  566,  567,  564,  563,
 /*   790 */   562,  561,  538,  442,  683,  271,  709,  702,  708,  707,
 /*   800 */   701,  537,   27,   26,   25,   29,   28,  267,  272,   30,
 /*   810 */   377,  441,  565,  567,  564,  563,  562,  561,  683,  271,
 /*   820 */   709,  702,  708,  707,  701,  356,  441,   46,  552,  193,
 /*   830 */   255,  267,  272,  556,  555,   51,  435,  567,  564,  563,
 /*   840 */   562,  561,  683,  271,  709,  702,  708,  707,  701,   74,
 /*   850 */    27,   26,   25,   29,   28,  267,  272,   30,   58,  493,
 /*   860 */   434,  567,  564,  563,  562,  561,  683,  271,  709,  702,
 /*   870 */   708,  707,  701,  492,   60,   42,   41,   45,   44,  267,
 /*   880 */   272,   46, 1100,    1,  313,  567,  564,  563,  562,  561,
 /*   890 */   683,  271,  709,  702,  708,  707,  701,  491,   27,   26,
 /*   900 */    25,   29,   28,  267,  272,   30,   57,  490,  355,  567,
 /*   910 */   564,  563,  562,  561,  683,  271,  709,  702,  708,  707,
 /*   920 */   701,  489,   59,  546,  295,   56,  488,  267,  272,  487,
 /*   930 */   118,  486,  354,  567,  564,  563,  562,  561,  683,  271,
 /*   940 */   709,  702,  708,  707,  701,  485,   27,   26,   25,   29,
 /*   950 */    28,  267,  272,   30,   14,  484,  282,  567,  564,  563,
 /*   960 */   562,  561,  483,  683,  271,  709,  702,  708,  707,  701,
 /*   970 */   482,  480,  479,  547,  651,  478,  267,  272,  477,  476,
 /*   980 */   463,  280,  567,  564,  563,  562,  561,  683,  271,  709,
 /*   990 */   702,  708,  707,  701,  649,   17,   16,   15,   19,   18,
 /*  1000 */   267,  272,   20,  571,  700,  277,  567,  564,  563,  562,
 /*  1010 */   561,  683,  271,  709,  702,  708,  707,  701,  647,  111,
 /*  1020 */   109,  107,  105,  104,  264,  272,   12,  494,  395,  640,
 /*  1030 */   551,  564,  563,  562,  561,  191,  189,  187,  185,  184,
 /*  1040 */   550,  683,  271,  709,  702,  708,  707,  701,  467,   43,
 /*  1050 */    42,   41,   45,   44,  264,  272,   46,   13,  594,  614,
 /*  1060 */   551,  564,  563,  562,  561,   92,   91,   90,   89,   88,
 /*  1070 */   549,  683,  271,  709,  702,  708,  707,  701,  586,  557,
 /*  1080 */   554,  660,   12,  253,  264,  272,  252,  422,  153,  249,
 /*  1090 */   551,  564,  563,  562,  561,   27,   26,   25,   29,   28,
 /*  1100 */   433,  251,   30,   50,   14,  615,  295,  683,  271,  709,
 /*  1110 */   702,  708,  707,  701,  140,  138,  136,  134,  133,  248,
 /*  1120 */   264,  272,  247,  417,  244,  239,  551,  564,  563,  562,
 /*  1130 */   561,  448,  391,  616,  617,  246,  432,  683,  271,  709,
 /*  1140 */   702,  708,  707,  701,  420,   17,   16,   15,   19,   18,
 /*  1150 */   264,  272,   20,  241,  243,  242,  551,  564,  563,  562,
 /*  1160 */   561,   97,   96,   95,   94,   93,  308,  683,  271,  709,
 /*  1170 */   702,  708,  707,  701,  140,  138,  136,  134,  133,  419,
 /*  1180 */   264,  272,  238,  230,  418,  235,  551,  564,  563,  562,
 /*  1190 */   561,  191,  189,  187,  185,  184,  353,  683,  271,  709,
 /*  1200 */   702,  708,  707,  701,  416,   27,   26,   25,   29,   28,
 /*  1210 */   264,  272,   30,  232,  415,  414,  551,  564,  563,  562,
 /*  1220 */   561,  229,  218,  413,  227,  219,  352,  683,  271,  709,
 /*  1230 */   702,  708,  707,  701,  225,   43,   42,   41,   45,   44,
 /*  1240 */   267,  272,   46,  412,  411,  288,  567,  564,  563,  562,
 /*  1250 */   561,  410,  481,  223,  221,  217,  683,  271,  709,  702,
 /*  1260 */   708,  707,  701,  409,  216,  615,  295,  215,  214,  267,
 /*  1270 */   272,  408,  630,  614,  287,  567,  564,  563,  562,  561,
 /*  1280 */   212,   50,  683,  271,  709,  702,  708,  707,  701,  211,
 /*  1290 */    14,  388,  391,  616,  617,  267,  272,  210,  209,  208,
 /*  1300 */   286,  567,  564,  563,  562,  561,  683,  271,  709,  702,
 /*  1310 */   708,  707,  701,  407,   27,   26,   25,   29,   28,  267,
 /*  1320 */   272,   30,  206,  204,  179,  567,  564,  563,  562,  561,
 /*  1330 */   205,  202,  203,  406,  198,  403,  683,  271,  709,  702,
 /*  1340 */   708,  707,  701,  190,   27,   26,   25,   29,   28,  267,
 /*  1350 */   272,   30,  405,  630,  178,  567,  564,  563,  562,  561,
 /*  1360 */   200,  254,  404,  638,  620,  256,  683,  271,  709,  702,
 /*  1370 */   708,  707,  701,  314,  357,  294,  358,  188,  236,  267,
 /*  1380 */   272,  628,  186,  426,  177,  567,  564,  563,  562,  561,
 /*  1390 */   624,  233,  627,  626,  425,  625,  683,  271,  709,  702,
 /*  1400 */   708,  707,  701,  516,  293,  292,  515,  228,  421,  267,
 /*  1410 */   272,  226,  224,  222,  176,  567,  564,  563,  562,  561,
 /*  1420 */   201, 1101,  444,  584,  190,  683,  271,  709,  702,  708,
 /*  1430 */   707,  701, 1101, 1101, 1101, 1101, 1101, 1101,  267,  272,
 /*  1440 */  1101, 1101,  629,  175,  567,  564,  563,  562,  561, 1101,
 /*  1450 */  1101,  683,  271,  709,  702,  708,  707,  701,  188, 1101,
 /*  1460 */  1101, 1101,  648,  186,  267,  272, 1101,  462, 1101,  174,
 /*  1470 */   567,  564,  563,  562,  561,  191,  189,  187,  185,  184,
 /*  1480 */     5, 1101, 1101,  680, 1101, 1101,  458, 1101, 1101,    5,
 /*  1490 */   169,  397,   73,   72,   71, 1101,   70,   69,   68,   67,
 /*  1500 */    35,   73,   72,   71, 1101,   70,   69,   68,   67, 1101,
 /*  1510 */  1101, 1101, 1101, 1101, 1101, 1101,  103,  102,  101,  100,
 /*  1520 */    99, 1101,  152, 1101, 1101,  103,  102,  101,  100,   99,
 /*  1530 */  1101, 1101, 1101, 1101,   73,   72,   71, 1101,   70,   69,
 /*  1540 */    68,   67,  683,  283,  709,  702,  708,  707,  701, 1101,
 /*  1550 */  1101,    5, 1101, 1101, 1101,  673,  344, 1101,  103,  102,
 /*  1560 */   101,  100,   99,   73,   72,   71,  511,   70,   69,   68,
 /*  1570 */    67, 1101, 1101, 1101, 1101,  126,  125,  124, 1101,  123,
 /*  1580 */   122,  121,  120,  570, 1101, 1101, 1101,  103,  102,  101,
 /*  1590 */   100,   99,  682,  464, 1101,  372,  465,  681, 1101,  131,
 /*  1600 */   130,  129,  128,  127, 1101, 1101, 1101, 1101,  683,  273,
 /*  1610 */   709,  702,  708,  707,  701,  191,  189,  187,  185,  184,
 /*  1620 */   475,  268,  272, 1101, 1101, 1101, 1101, 1101,  646,  697,
 /*  1630 */   705,  706,  709,  702,  708,  707,  701,  400,  346,  675,
 /*  1640 */   674,  676,  245,  645,  644,  683,  273,  709,  702,  708,
 /*  1650 */   707,  701,  140,  138,  136,  134,  133, 1101,  268,  272,
 /*  1660 */  1101, 1101, 1101,  115, 1101,  646,  190, 1101, 1101, 1101,
 /*  1670 */  1101, 1101,  683,  273,  709,  702,  708,  707,  701,  240,
 /*  1680 */   645,  644, 1101, 1101, 1101,  268,  272, 1101, 1101, 1101,
 /*  1690 */  1101, 1101,  646, 1101, 1101, 1101, 1101,  114, 1101, 1101,
 /*  1700 */   188, 1101,   98, 1101, 1101,  186,  231,  645,  644, 1101,
 /*  1710 */  1101,   87,   86,   85, 1101,   84,   83,   82,   81, 1101,
 /*  1720 */  1101, 1101,  467,  700,  466,   74,   80,   79, 1101,   78,
 /*  1730 */    77,   76,   75, 1101, 1101,   92,   91,   90,   89,   88,
 /*  1740 */  1101,  793,  793,  793, 1101,  793,  793,  793,  793,   97,
 /*  1750 */    96,   95,   94,   93, 1101,  126,  125,  124, 1101,  123,
 /*  1760 */   122,  121,  120, 1101, 1101,  793,  793,  793,  793,  793,
 /*  1770 */    80,   79, 1101,   78,   77,   76,   75, 1101, 1101,  131,
 /*  1780 */   130,  129,  128,  127, 1101, 1101, 1101, 1101, 1101, 1101,
 /*  1790 */  1101, 1101, 1101,   97,   96,   95,   94,   93, 1101,  514,
 /*  1800 */   504,  709,  702,  708,  707,  701,  514,  504,  709,  702,
 /*  1810 */   708,  707,  701, 1101,  266, 1101, 1101, 1101, 1101, 1101,
 /*  1820 */  1101,  503, 1101, 1101,  275,  501,  498, 1101, 1101, 1101,
 /*  1830 */  1101,  495,  501,  498,  514,  504,  709,  702,  708,  707,
 /*  1840 */   701,  514,  504,  709,  702,  708,  707,  701, 1101,  269,
 /*  1850 */  1101, 1101, 1101, 1101, 1101, 1101,  269, 1101, 1101,  500,
 /*  1860 */   501,  498, 1101, 1101, 1101, 1101,  499,  501,  498,  514,
 /*  1870 */   504,  709,  702,  708,  707,  701, 1101, 1101, 1101, 1101,
 /*  1880 */  1101, 1101, 1101, 1101,  269,  514,  504,  709,  702,  708,
 /*  1890 */   707,  701, 1101, 1101,  424,  501,  498, 1101, 1101, 1101,
 /*  1900 */   269,  514,  504,  709,  702,  708,  707,  701, 1101, 1101,
 /*  1910 */   423,  501,  498, 1101, 1101, 1101,  269,  514,  504,  709,
 /*  1920 */   702,  708,  707,  701, 1101, 1101,  299,  501,  498, 1101,
 /*  1930 */  1101, 1101,  269, 1101, 1101,  514,  504,  709,  702,  708,
 /*  1940 */   707,  701,  349,  501,  498, 1101, 1101, 1101, 1101, 1101,
 /*  1950 */   269, 1101, 1101, 1101, 1101, 1101, 1101, 1101, 1101, 1101,
 /*  1960 */   348,  501,  498,  697,  705,  706,  709,  702,  708,  707,
 /*  1970 */   701,  399,  346, 1101, 1101, 1101,  697,  705,  706,  709,
 /*  1980 */   702,  708,  707,  701,  369,  346,  697,  705,  706,  709,
 /*  1990 */   702,  708,  707,  701, 1101,  345,  697,  705,  706,  709,
 /*  2000 */   702,  708,  707,  701, 1101,  367, 1101, 1101, 1101, 1101,
 /*  2010 */  1101,  697,  705,  706,  709,  702,  708,  707,  701, 1101,
 /*  2020 */   298,  697,  705,  706,  709,  702,  708,  707,  701, 1101,
 /*  2030 */   368, 1101, 1101, 1101, 1101, 1101, 1101, 1101, 1101,  697,
 /*  2040 */   705,  706,  709,  702,  708,  707,  701, 1101,  688,  697,
 /*  2050 */   705,  706,  709,  702,  708,  707,  701, 1101,  684,  697,
 /*  2060 */   705,  706,  709,  702,  708,  707,  701, 1101,  687, 1101,
 /*  2070 */  1101,  697,  705,  706,  709,  702,  708,  707,  701, 1101,
 /*  2080 */   686,  697,  705,  706,  709,  702,  708,  707,  701, 1101,
 /*  2090 */   685,  697,  705,  706,  709,  702,  708,  707,  701, 1101,
 /*  2100 */   366, 1101, 1101, 1101, 1101,  697,  705,  706,  709,  702,
 /*  2110 */   708,  707,  701, 1101,  365,  697,  705,  706,  709,  702,
 /*  2120 */   708,  707,  701, 1101,  679,  697,  705,  706,  709,  702,
 /*  2130 */   708,  707,  701, 1101,  678,  697,  705,  706,  709,  702,
 /*  2140 */   708,  707,  701, 1101,  677,  697,  705,  706,  709,  702,
 /*  2150 */   708,  707,  701, 1101,  672,  697,  705,  706,  709,  702,
 /*  2160 */   708,  707,  701, 1101,  364, 1101,  697,  705,  706,  709,
 /*  2170 */   702,  708,  707,  701, 1101,  363,  697,  705,  706,  709,
 /*  2180 */   702,  708,  707,  701, 1101,  670, 1101, 1101, 1101, 1101,
 /*  2190 */  1101, 1101, 1101, 1101,  697,  705,  706,  709,  702,  708,
 /*  2200 */   707,  701, 1101,  669,  697,  705,  706,  709,  702,  708,
 /*  2210 */   707,  701, 1101,  668,  697,  705,  706,  709,  702,  708,
 /*  2220 */   707,  701, 1101,  362, 1101, 1101,  697,  705,  706,  709,
 /*  2230 */   702,  708,  707,  701, 1101,  361,  697,  705,  706,  709,
 /*  2240 */   702,  708,  707,  701, 1101,  667,  697,  705,  706,  709,
 /*  2250 */   702,  708,  707,  701, 1101,  666, 1101, 1101, 1101, 1101,
 /*  2260 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  665,
 /*  2270 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  342,
 /*  2280 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  341,
 /*  2290 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  340,
 /*  2300 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  339,
 /*  2310 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  338,
 /*  2320 */  1101,  697,  705,  706,  709,  702,  708,  707,  701, 1101,
 /*  2330 */   337,  697,  705,  706,  709,  702,  708,  707,  701, 1101,
 /*  2340 */   336, 1101, 1101, 1101, 1101, 1101, 1101, 1101, 1101,  697,
 /*  2350 */   705,  706,  709,  702,  708,  707,  701, 1101,  335,  697,
 /*  2360 */   705,  706,  709,  702,  708,  707,  701, 1101,  334,  697,
 /*  2370 */   705,  706,  709,  702,  708,  707,  701, 1101,  333, 1101,
 /*  2380 */  1101,  697,  705,  706,  709,  702,  708,  707,  701, 1101,
 /*  2390 */   332,  697,  705,  706,  709,  702,  708,  707,  701, 1101,
 /*  2400 */   331,  697,  705,  706,  709,  702,  708,  707,  701, 1101,
 /*  2410 */   330, 1101, 1101, 1101, 1101,  697,  705,  706,  709,  702,
 /*  2420 */   708,  707,  701, 1101,  329,  697,  705,  706,  709,  702,
 /*  2430 */   708,  707,  701, 1101,  328,  697,  705,  706,  709,  702,
 /*  2440 */   708,  707,  701, 1101,  327,  697,  705,  706,  709,  702,
 /*  2450 */   708,  707,  701, 1101,  326,  697,  705,  706,  709,  702,
 /*  2460 */   708,  707,  701, 1101,  325,  697,  705,  706,  709,  702,
 /*  2470 */   708,  707,  701, 1101,  324, 1101,  697,  705,  706,  709,
 /*  2480 */   702,  708,  707,  701, 1101,  323,  697,  705,  706,  709,
 /*  2490 */   702,  708,  707,  701, 1101,  322, 1101, 1101, 1101, 1101,
 /*  2500 */  1101, 1101, 1101, 1101,  697,  705,  706,  709,  702,  708,
 /*  2510 */   707,  701, 1101,  318,  697,  705,  706,  709,  702,  708,
 /*  2520 */   707,  701, 1101,  317,  697,  705,  706,  709,  702,  708,
 /*  2530 */   707,  701, 1101,  316, 1101, 1101,  697,  705,  706,  709,
 /*  2540 */   702,  708,  707,  701, 1101,  315,  697,  705,  706,  709,
 /*  2550 */   702,  708,  707,  701, 1101,  312,  697,  705,  706,  709,
 /*  2560 */   702,  708,  707,  701, 1101,  311, 1101, 1101, 1101, 1101,
 /*  2570 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  310,
 /*  2580 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  309,
 /*  2590 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  307,
 /*  2600 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  181,
 /*  2610 */   697,  705,  706,  709,  702,  708,  707,  701, 1101,  180,
 /*  2620 */   663,  657,  709,  702,  708,  707,  701, 1101, 1101, 1101,
 /*  2630 */  1101,  359,  663,  657,  709,  702,  708,  707,  701, 1101,
 /*  2640 */  1101, 1101, 1101,  296,  663,  657,  709,  702,  708,  707,
 /*  2650 */   701, 1101, 1101, 1101, 1101,  360, 1101, 1101, 1101, 1101,
 /*  2660 */   663,  657,  709,  702,  708,  707,  701, 1101, 1101, 1101,
 /*  2670 */  1101,  656,  663,  657,  709,  702,  708,  707,  701, 1101,
 /*  2680 */  1101, 1101, 1101,  652,  663,  657,  709,  702,  708,  707,
 /*  2690 */   701, 1101, 1101, 1101, 1101,  655,  663,  657,  709,  702,
 /*  2700 */   708,  707,  701, 1101, 1101, 1101, 1101,  654,  663,  657,
 /*  2710 */   709,  702,  708,  707,  701, 1101, 1101, 1101, 1101,  653,
 /*  2720 */  1101,  663,  657,  709,  702,  708,  707,  701, 1101, 1101,
 /*  2730 */  1101, 1101,  319,  663,  657,  709,  702,  708,  707,  701,
 /*  2740 */  1101, 1101, 1101, 1101,  351,  663,  657,  709,  702,  708,
 /*  2750 */   707,  701, 1101, 1101, 1101, 1101,  350,  663,  657,  709,
 /*  2760 */   702,  708,  707,  701, 1101, 1101, 1101, 1101,  510,  663,
 /*  2770 */   657,  709,  702,  708,  707,  701, 1101, 1101, 1101, 1101,
 /*  2780 */   509,  663,  657,  709,  702,  708,  707,  701, 1101, 1101,
 /*  2790 */  1101, 1101,  508,  663,  657,  709,  702,  708,  707,  701,
 /*  2800 */  1101, 1101, 1101, 1101,  306,  663,  657,  709,  702,  708,
 /*  2810 */   707,  701, 1101, 1101, 1101, 1101,  305,  663,  657,  709,
 /*  2820 */   702,  708,  707,  701, 1101, 1101, 1101, 1101,  304,  663,
 /*  2830 */   657,  709,  702,  708,  707,  701, 1101, 1101, 1101, 1101,
 /*  2840 */   303,  663,  657,  709,  702,  708,  707,  701, 1101, 1101,
 /*  2850 */  1101, 1101,  302,  663,  657,  709,  702,  708,  707,  701,
 /*  2860 */  1101, 1101, 1101, 1101,  301,  663,  657,  709,  702,  708,
 /*  2870 */   707,  701, 1101, 1101, 1101, 1101,  300,  663,  657,  709,
 /*  2880 */   702,  708,  707,  701, 1101, 1101, 1101, 1101,  502,  663,
 /*  2890 */   657,  709,  702,  708,  707,  701, 1101, 1101, 1101, 1101,
 /*  2900 */   284,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   136,  171,  172,  139,  140,  141,  142,  143,  144,  145,
 /*    10 */   146,  147,  148,  149,  150,  151,  152,  153,    0,  155,
 /*    20 */   156,  157,  158,  159,  160,  161,   72,  197,  198,  199,
 /*    30 */   200,  164,  168,  169,  204,  205,  108,  109,  110,  175,
 /*    40 */   176,    1,    2,  155,  156,  157,  158,  159,  160,  161,
 /*    50 */   183,   97,   73,  189,  190,  191,  168,  169,  108,  109,
 /*    60 */   110,  173,  174,  175,  176,  177,  178,  155,  156,  157,
 /*    70 */   158,  159,  160,  161,   72,  187,  188,  108,  109,  110,
 /*    80 */   168,  169,  103,  216,  217,  173,  174,  175,  176,  177,
 /*    90 */   178,  227,  228,  229,  230,  231,  232,  233,  234,  235,
 /*   100 */   236,  237,  238,  239,  240,  241,  242,  243,  244,    1,
 /*   110 */     2,   73,   72,    5,    6,    7,    8,    9,   10,   11,
 /*   120 */    12,    1,    2,  100,  101,    5,    6,  155,  156,  157,
 /*   130 */   158,  159,  160,  161,    1,    2,   28,  165,  166,   72,
 /*   140 */    32,  103,   34,   35,   27,   37,  106,   39,   77,    1,
 /*   150 */     2,  111,   44,    5,    6,   73,   48,   49,   50,   51,
 /*   160 */    52,   53,   54,   72,   56,   57,   46,   73,   48,   49,
 /*   170 */    50,   63,   33,   65,  103,   36,   41,   69,   70,   72,
 /*   180 */    72,   72,   43,   63,   76,   65,   47,   67,    2,  107,
 /*   190 */    70,   75,   72,   76,  100,  101,   76,  164,    1,    2,
 /*   200 */   171,  172,    5,    6,  155,  156,  157,  158,  159,  160,
 /*   210 */   161,   63,   82,   65,  106,   80,  183,  168,  169,  111,
 /*   220 */    72,  201,  202,  203,   74,  105,  106,  119,   99,    1,
 /*   230 */     2,  111,  103,    5,    6,  206,  207,  208,  209,  131,
 /*   240 */   132,  133,   77,  135,   94,   48,   49,   50,  131,  216,
 /*   250 */   217,  131,  132,  133,  106,   96,   97,   98,   99,  111,
 /*   260 */    63,  102,   65,  164,   67,   74,  133,   70,  103,   72,
 /*   270 */    98,   99,   74,   76,  102,   71,   48,   49,   50,  131,
 /*   280 */   132,  133,  183,  155,  156,  157,  158,  159,  160,  161,
 /*   290 */   156,   63,   94,   65,  103,   67,  168,  169,   70,   97,
 /*   300 */    72,  167,  105,  106,   76,   98,   99,   74,  111,  102,
 /*   310 */   155,  156,  157,  158,  159,  160,  161,  131,  155,  156,
 /*   320 */   157,  158,  159,  160,  161,  170,   76,   94,  131,  132,
 /*   330 */   133,  168,  169,  105,  106,  180,  181,  182,  175,  111,
 /*   340 */   171,  172,    2,  155,  156,  157,  158,  159,  160,  161,
 /*   350 */    75,   90,  189,  190,  191,   73,  168,  169,   71,  131,
 /*   360 */   132,  133,  174,  175,  176,  177,  178,  106,  107,  108,
 /*   370 */   109,  110,  184,   74,    1,    2,  207,  208,  209,   73,
 /*   380 */    14,  131,   16,   17,   18,   19,   20,   21,  106,  107,
 /*   390 */   108,  109,  110,   94,   71,   22,   23,   24,   25,   96,
 /*   400 */    97,   98,   99,   30,   71,  102,  218,  219,  220,  103,
 /*   410 */    73,   38,  107,   40,   48,   49,   76,   72,   45,  154,
 /*   420 */   155,  156,  157,  158,  159,  160,  161,  162,  163,   74,
 /*   430 */    57,   58,   59,   60,  155,  156,  157,  158,  159,  160,
 /*   440 */   161,    2,   98,   99,  107,   72,  102,  168,  169,  171,
 /*   450 */   172,   55,  173,  174,  175,  176,  177,  178,  103,  155,
 /*   460 */   156,  157,  158,  159,  160,  161,  187,  188,    1,    2,
 /*   470 */   166,  131,    5,    6,  196,  197,  198,  199,  200,  106,
 /*   480 */   210,  211,  212,   73,  111,  155,  156,  157,  158,  159,
 /*   490 */   160,  161,  106,  107,  108,  109,  110,  159,  168,  169,
 /*   500 */   134,   73,  164,   76,  174,  175,  176,  177,  178,  213,
 /*   510 */   214,  215,   72,  103,  184,   48,   49,   50,    1,    2,
 /*   520 */    71,  183,    5,    6,   70,  187,  188,   73,  171,  172,
 /*   530 */    63,  103,   65,  164,    2,   81,   82,   83,   84,   72,
 /*   540 */    86,   87,   88,   89,   95,   96,   97,   98,   99,  219,
 /*   550 */   220,  102,  183,  196,  197,  198,  199,  200,  131,  132,
 /*   560 */   106,  107,  108,  109,  110,   48,   49,   50,   91,   92,
 /*   570 */   108,   73,  105,  106,  112,   99,   99,    2,  111,  103,
 /*   580 */    63,   72,   65,   72,    1,    2,  217,   70,    2,   72,
 /*   590 */    72,    1,    2,  171,  172,    5,    6,   73,  131,  132,
 /*   600 */   133,  103,  154,  155,  156,  157,  158,  159,  160,  161,
 /*   610 */   162,  163,  155,  156,  157,  158,  159,  160,  161,  197,
 /*   620 */   198,  199,  200,  106,   73,  168,  169,  103,  111,  103,
 /*   630 */    73,  174,  175,  176,  177,  178,   74,   74,   48,   49,
 /*   640 */    50,  184,  164,   26,   72,  171,  172,   72,  131,  132,
 /*   650 */   133,  171,  172,   63,  103,   65,   94,   94,   41,   42,
 /*   660 */   103,  183,   72,  155,  156,  157,  158,  159,  160,  161,
 /*   670 */   196,  197,  198,  199,  200,   74,  168,  169,   61,   62,
 /*   680 */     2,   27,  174,  175,  176,  177,  178,  171,  172,  209,
 /*   690 */    74,   74,  184,  192,  193,   94,  106,   80,   74,    3,
 /*   700 */     4,  111,   73,  211,  212,  155,  156,  157,  158,  159,
 /*   710 */   160,  161,  196,  197,  198,  199,  200,   68,  168,  169,
 /*   720 */    82,  131,  132,  133,  174,  175,  176,  177,  178,   73,
 /*   730 */    76,   73,  103,   82,  184,  102,  155,  156,  157,  158,
 /*   740 */   159,  160,  161,  102,   95,   96,   97,   98,   99,  168,
 /*   750 */   169,  102,  194,  195,  173,  174,  175,  176,  177,  178,
 /*   760 */    77,  103,  106,  107,  108,  109,  110,   77,  155,  156,
 /*   770 */   157,  158,  159,  160,  161,  202,  203,   77,   71,  214,
 /*   780 */   215,  168,  169,  214,  215,  131,  173,  174,  175,  176,
 /*   790 */   177,  178,   74,  103,  155,  156,  157,  158,  159,  160,
 /*   800 */   161,   74,   95,   96,   97,   98,   99,  168,  169,  102,
 /*   810 */   214,  215,  173,  174,  175,  176,  177,  178,  155,  156,
 /*   820 */   157,  158,  159,  160,  161,  214,  215,  102,   73,  185,
 /*   830 */   186,  168,  169,   91,   92,   72,  173,  174,  175,  176,
 /*   840 */   177,  178,  155,  156,  157,  158,  159,  160,  161,   82,
 /*   850 */    95,   96,   97,   98,   99,  168,  169,  102,   82,   74,
 /*   860 */   173,  174,  175,  176,  177,  178,  155,  156,  157,  158,
 /*   870 */   159,  160,  161,   74,   71,   96,   97,   98,   99,  168,
 /*   880 */   169,  102,  137,  138,  173,  174,  175,  176,  177,  178,
 /*   890 */   155,  156,  157,  158,  159,  160,  161,   74,   95,   96,
 /*   900 */    97,   98,   99,  168,  169,  102,   31,   74,  173,  174,
 /*   910 */   175,  176,  177,  178,  155,  156,  157,  158,  159,  160,
 /*   920 */   161,   74,   71,  171,  172,   31,   74,  168,  169,   74,
 /*   930 */    66,   74,  173,  174,  175,  176,  177,  178,  155,  156,
 /*   940 */   157,  158,  159,  160,  161,   74,   95,   96,   97,   98,
 /*   950 */    99,  168,  169,  102,   41,   74,  173,  174,  175,  176,
 /*   960 */   177,  178,   74,  155,  156,  157,  158,  159,  160,  161,
 /*   970 */    74,   74,   74,   73,  164,   74,  168,  169,   74,   74,
 /*   980 */   131,  173,  174,  175,  176,  177,  178,  155,  156,  157,
 /*   990 */   158,  159,  160,  161,  159,   95,   96,   97,   98,   99,
 /*  1000 */   168,  169,  102,   74,  132,  173,  174,  175,  176,  177,
 /*  1010 */   178,  155,  156,  157,  158,  159,  160,  161,  156,  106,
 /*  1020 */   107,  108,  109,  110,  168,  169,   26,   73,    2,  193,
 /*  1030 */   174,  175,  176,  177,  178,  106,  107,  108,  109,  110,
 /*  1040 */   184,  155,  156,  157,  158,  159,  160,  161,  131,   95,
 /*  1050 */    96,   97,   98,   99,  168,  169,  102,   42,  200,    2,
 /*  1060 */   174,  175,  176,  177,  178,  106,  107,  108,  109,  110,
 /*  1070 */   184,  155,  156,  157,  158,  159,  160,  161,  200,  159,
 /*  1080 */   159,   73,   26,  224,  168,  169,  223,  226,   61,  222,
 /*  1090 */   174,  175,  176,  177,  178,   95,   96,   97,   98,   99,
 /*  1100 */   184,  225,  102,   62,   41,  171,  172,  155,  156,  157,
 /*  1110 */   158,  159,  160,  161,  106,  107,  108,  109,  110,  224,
 /*  1120 */   168,  169,  223,  156,  222,  222,  174,  175,  176,  177,
 /*  1130 */   178,  197,  198,  199,  200,  225,  184,  155,  156,  157,
 /*  1140 */   158,  159,  160,  161,  226,   95,   96,   97,   98,   99,
 /*  1150 */   168,  169,  102,  225,  224,  223,  174,  175,  176,  177,
 /*  1160 */   178,  106,  107,  108,  109,  110,  184,  155,  156,  157,
 /*  1170 */   158,  159,  160,  161,  106,  107,  108,  109,  110,  226,
 /*  1180 */   168,  169,  225,  222,  226,  225,  174,  175,  176,  177,
 /*  1190 */   178,  106,  107,  108,  109,  110,  184,  155,  156,  157,
 /*  1200 */   158,  159,  160,  161,  226,   95,   96,   97,   98,   99,
 /*  1210 */   168,  169,  102,  225,  156,  226,  174,  175,  176,  177,
 /*  1220 */   178,  225,  190,  226,  225,  106,  184,  155,  156,  157,
 /*  1230 */   158,  159,  160,  161,  225,   95,   96,   97,   98,   99,
 /*  1240 */   168,  169,  102,  226,  226,  173,  174,  175,  176,  177,
 /*  1250 */   178,  226,  156,  225,  225,  222,  155,  156,  157,  158,
 /*  1260 */   159,  160,  161,  226,  224,  171,  172,  223,  225,  168,
 /*  1270 */   169,  226,    1,    2,  173,  174,  175,  176,  177,  178,
 /*  1280 */   156,   62,  155,  156,  157,  158,  159,  160,  161,  222,
 /*  1290 */    41,  197,  198,  199,  200,  168,  169,  224,  223,  225,
 /*  1300 */   173,  174,  175,  176,  177,  178,  155,  156,  157,  158,
 /*  1310 */   159,  160,  161,  226,   95,   96,   97,   98,   99,  168,
 /*  1320 */   169,  102,  156,  224,  173,  174,  175,  176,  177,  178,
 /*  1330 */   222,  225,  223,  226,  156,  190,  155,  156,  157,  158,
 /*  1340 */   159,  160,  161,   72,   95,   96,   97,   98,   99,  168,
 /*  1350 */   169,  102,  226,    1,  173,  174,  175,  176,  177,  178,
 /*  1360 */   225,  222,  226,  195,  158,  186,  155,  156,  157,  158,
 /*  1370 */   159,  160,  161,  172,  172,  172,  172,  106,  222,  168,
 /*  1380 */   169,  172,  111,  156,  173,  174,  175,  176,  177,  178,
 /*  1390 */   172,  222,  172,  172,  167,  172,  155,  156,  157,  158,
 /*  1400 */   159,  160,  161,  176,  172,  172,  179,  223,  226,  168,
 /*  1410 */   169,  223,  223,  223,  173,  174,  175,  176,  177,  178,
 /*  1420 */   222,  245,    2,    2,   72,  155,  156,  157,  158,  159,
 /*  1430 */   160,  161,  245,  245,  245,  245,  245,  245,  168,  169,
 /*  1440 */   245,  245,   73,  173,  174,  175,  176,  177,  178,  245,
 /*  1450 */   245,  155,  156,  157,  158,  159,  160,  161,  106,  245,
 /*  1460 */   245,  245,  159,  111,  168,  169,  245,  164,  245,  173,
 /*  1470 */   174,  175,  176,  177,  178,  106,  107,  108,  109,  110,
 /*  1480 */    70,  245,  245,   73,  245,  245,  183,  245,  245,   70,
 /*  1490 */   187,  188,   82,   83,   84,  245,   86,   87,   88,   89,
 /*  1500 */    81,   82,   83,   84,  245,   86,   87,   88,   89,  245,
 /*  1510 */   245,  245,  245,  245,  245,  245,  106,  107,  108,  109,
 /*  1520 */   110,  245,   70,  245,  245,  106,  107,  108,  109,  110,
 /*  1530 */   245,  245,  245,  245,   82,   83,   84,  245,   86,   87,
 /*  1540 */    88,   89,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  1550 */   245,   70,  245,  245,  245,  168,  169,  245,  106,  107,
 /*  1560 */   108,  109,  110,   82,   83,   84,   73,   86,   87,   88,
 /*  1570 */    89,  245,  245,  245,  245,   82,   83,   84,  245,   86,
 /*  1580 */    87,   88,   89,   74,  245,  245,  245,  106,  107,  108,
 /*  1590 */   109,  110,    1,    2,  245,    1,    5,    6,  245,  106,
 /*  1600 */   107,  108,  109,  110,  245,  245,  245,  245,  155,  156,
 /*  1610 */   157,  158,  159,  160,  161,  106,  107,  108,  109,  110,
 /*  1620 */    74,  168,  169,  245,  245,  245,  245,  245,  175,  154,
 /*  1630 */   155,  156,  157,  158,  159,  160,  161,  162,  163,   48,
 /*  1640 */    49,   50,  189,  190,  191,  155,  156,  157,  158,  159,
 /*  1650 */   160,  161,  106,  107,  108,  109,  110,  245,  168,  169,
 /*  1660 */   245,  245,  245,   72,  245,  175,   72,  245,  245,  245,
 /*  1670 */   245,  245,  155,  156,  157,  158,  159,  160,  161,  189,
 /*  1680 */   190,  191,  245,  245,  245,  168,  169,  245,  245,  245,
 /*  1690 */   245,  245,  175,  245,  245,  245,  245,  106,  245,  245,
 /*  1700 */   106,  245,  111,  245,  245,  111,  189,  190,  191,  245,
 /*  1710 */   245,   82,   83,   84,  245,   86,   87,   88,   89,  245,
 /*  1720 */   245,  245,  131,  132,  133,   82,   83,   84,  245,   86,
 /*  1730 */    87,   88,   89,  245,  245,  106,  107,  108,  109,  110,
 /*  1740 */   245,   82,   83,   84,  245,   86,   87,   88,   89,  106,
 /*  1750 */   107,  108,  109,  110,  245,   82,   83,   84,  245,   86,
 /*  1760 */    87,   88,   89,  245,  245,  106,  107,  108,  109,  110,
 /*  1770 */    83,   84,  245,   86,   87,   88,   89,  245,  245,  106,
 /*  1780 */   107,  108,  109,  110,  245,  245,  245,  245,  245,  245,
 /*  1790 */   245,  245,  245,  106,  107,  108,  109,  110,  245,  155,
 /*  1800 */   156,  157,  158,  159,  160,  161,  155,  156,  157,  158,
 /*  1810 */   159,  160,  161,  245,  170,  245,  245,  245,  245,  245,
 /*  1820 */   245,  170,  245,  245,  180,  181,  182,  245,  245,  245,
 /*  1830 */   245,  180,  181,  182,  155,  156,  157,  158,  159,  160,
 /*  1840 */   161,  155,  156,  157,  158,  159,  160,  161,  245,  170,
 /*  1850 */   245,  245,  245,  245,  245,  245,  170,  245,  245,  180,
 /*  1860 */   181,  182,  245,  245,  245,  245,  180,  181,  182,  155,
 /*  1870 */   156,  157,  158,  159,  160,  161,  245,  245,  245,  245,
 /*  1880 */   245,  245,  245,  245,  170,  155,  156,  157,  158,  159,
 /*  1890 */   160,  161,  245,  245,  180,  181,  182,  245,  245,  245,
 /*  1900 */   170,  155,  156,  157,  158,  159,  160,  161,  245,  245,
 /*  1910 */   180,  181,  182,  245,  245,  245,  170,  155,  156,  157,
 /*  1920 */   158,  159,  160,  161,  245,  245,  180,  181,  182,  245,
 /*  1930 */   245,  245,  170,  245,  245,  155,  156,  157,  158,  159,
 /*  1940 */   160,  161,  180,  181,  182,  245,  245,  245,  245,  245,
 /*  1950 */   170,  245,  245,  245,  245,  245,  245,  245,  245,  245,
 /*  1960 */   180,  181,  182,  154,  155,  156,  157,  158,  159,  160,
 /*  1970 */   161,  162,  163,  245,  245,  245,  154,  155,  156,  157,
 /*  1980 */   158,  159,  160,  161,  162,  163,  154,  155,  156,  157,
 /*  1990 */   158,  159,  160,  161,  245,  163,  154,  155,  156,  157,
 /*  2000 */   158,  159,  160,  161,  245,  163,  245,  245,  245,  245,
 /*  2010 */   245,  154,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2020 */   163,  154,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2030 */   163,  245,  245,  245,  245,  245,  245,  245,  245,  154,
 /*  2040 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  154,
 /*  2050 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  154,
 /*  2060 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  245,
 /*  2070 */   245,  154,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2080 */   163,  154,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2090 */   163,  154,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2100 */   163,  245,  245,  245,  245,  154,  155,  156,  157,  158,
 /*  2110 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2120 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2130 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2140 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2150 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2160 */   159,  160,  161,  245,  163,  245,  154,  155,  156,  157,
 /*  2170 */   158,  159,  160,  161,  245,  163,  154,  155,  156,  157,
 /*  2180 */   158,  159,  160,  161,  245,  163,  245,  245,  245,  245,
 /*  2190 */   245,  245,  245,  245,  154,  155,  156,  157,  158,  159,
 /*  2200 */   160,  161,  245,  163,  154,  155,  156,  157,  158,  159,
 /*  2210 */   160,  161,  245,  163,  154,  155,  156,  157,  158,  159,
 /*  2220 */   160,  161,  245,  163,  245,  245,  154,  155,  156,  157,
 /*  2230 */   158,  159,  160,  161,  245,  163,  154,  155,  156,  157,
 /*  2240 */   158,  159,  160,  161,  245,  163,  154,  155,  156,  157,
 /*  2250 */   158,  159,  160,  161,  245,  163,  245,  245,  245,  245,
 /*  2260 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2270 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2280 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2290 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2300 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2310 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2320 */   245,  154,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2330 */   163,  154,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2340 */   163,  245,  245,  245,  245,  245,  245,  245,  245,  154,
 /*  2350 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  154,
 /*  2360 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  154,
 /*  2370 */   155,  156,  157,  158,  159,  160,  161,  245,  163,  245,
 /*  2380 */   245,  154,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2390 */   163,  154,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2400 */   163,  154,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2410 */   163,  245,  245,  245,  245,  154,  155,  156,  157,  158,
 /*  2420 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2430 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2440 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2450 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2460 */   159,  160,  161,  245,  163,  154,  155,  156,  157,  158,
 /*  2470 */   159,  160,  161,  245,  163,  245,  154,  155,  156,  157,
 /*  2480 */   158,  159,  160,  161,  245,  163,  154,  155,  156,  157,
 /*  2490 */   158,  159,  160,  161,  245,  163,  245,  245,  245,  245,
 /*  2500 */   245,  245,  245,  245,  154,  155,  156,  157,  158,  159,
 /*  2510 */   160,  161,  245,  163,  154,  155,  156,  157,  158,  159,
 /*  2520 */   160,  161,  245,  163,  154,  155,  156,  157,  158,  159,
 /*  2530 */   160,  161,  245,  163,  245,  245,  154,  155,  156,  157,
 /*  2540 */   158,  159,  160,  161,  245,  163,  154,  155,  156,  157,
 /*  2550 */   158,  159,  160,  161,  245,  163,  154,  155,  156,  157,
 /*  2560 */   158,  159,  160,  161,  245,  163,  245,  245,  245,  245,
 /*  2570 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2580 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2590 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2600 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2610 */   154,  155,  156,  157,  158,  159,  160,  161,  245,  163,
 /*  2620 */   155,  156,  157,  158,  159,  160,  161,  245,  245,  245,
 /*  2630 */   245,  166,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2640 */   245,  245,  245,  166,  155,  156,  157,  158,  159,  160,
 /*  2650 */   161,  245,  245,  245,  245,  166,  245,  245,  245,  245,
 /*  2660 */   155,  156,  157,  158,  159,  160,  161,  245,  245,  245,
 /*  2670 */   245,  166,  155,  156,  157,  158,  159,  160,  161,  245,
 /*  2680 */   245,  245,  245,  166,  155,  156,  157,  158,  159,  160,
 /*  2690 */   161,  245,  245,  245,  245,  166,  155,  156,  157,  158,
 /*  2700 */   159,  160,  161,  245,  245,  245,  245,  166,  155,  156,
 /*  2710 */   157,  158,  159,  160,  161,  245,  245,  245,  245,  166,
 /*  2720 */   245,  155,  156,  157,  158,  159,  160,  161,  245,  245,
 /*  2730 */   245,  245,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2740 */   245,  245,  245,  245,  166,  155,  156,  157,  158,  159,
 /*  2750 */   160,  161,  245,  245,  245,  245,  166,  155,  156,  157,
 /*  2760 */   158,  159,  160,  161,  245,  245,  245,  245,  166,  155,
 /*  2770 */   156,  157,  158,  159,  160,  161,  245,  245,  245,  245,
 /*  2780 */   166,  155,  156,  157,  158,  159,  160,  161,  245,  245,
 /*  2790 */   245,  245,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2800 */   245,  245,  245,  245,  166,  155,  156,  157,  158,  159,
 /*  2810 */   160,  161,  245,  245,  245,  245,  166,  155,  156,  157,
 /*  2820 */   158,  159,  160,  161,  245,  245,  245,  245,  166,  155,
 /*  2830 */   156,  157,  158,  159,  160,  161,  245,  245,  245,  245,
 /*  2840 */   166,  155,  156,  157,  158,  159,  160,  161,  245,  245,
 /*  2850 */   245,  245,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2860 */   245,  245,  245,  245,  166,  155,  156,  157,  158,  159,
 /*  2870 */   160,  161,  245,  245,  245,  245,  166,  155,  156,  157,
 /*  2880 */   158,  159,  160,  161,  245,  245,  245,  245,  166,  155,
 /*  2890 */   156,  157,  158,  159,  160,  161,  245,  245,  245,  245,
 /*  2900 */   166,
};
#define YY_SHIFT_USE_DFLT (-73)
#define YY_SHIFT_COUNT (468)
#define YY_SHIFT_MIN   (-72)
#define YY_SHIFT_MAX   (1687)
static const short yy_shift_ofst[] = {
 /*     0 */   -73,  108,  120,  120,  228,  228,  228,  228,  228,  228,
 /*    10 */   228,  228,  228,  228,  228,  197,  197,  197,  197,  197,
 /*    20 */   197,  197,  228,  228,  228,  228,  228,  228,  228,  228,
 /*    30 */   228,  228,  228,  228,  197,  197,  197,  517,  517,  517,
 /*    40 */   517,  467,  467,  467,  467,  467,  467,  467,  467,  467,
 /*    50 */   467,  590,  590,  590,  590,  590,  590,  590,  590,  590,
 /*    60 */   590,  590,  590,  590,  590,  590,  590,  590,  590,  590,
 /*    70 */   590,  590,  590,  590,  590,  590,  590,  590,  590,  590,
 /*    80 */   590,  590,  590,  590,  590,  590,  590,  590,  590,  590,
 /*    90 */   590,  590,  590,  590,  590,  590,  590,  590,  590,  590,
 /*   100 */   590,  590,  590,  590,  590,  590,  590,  590,  590,  590,
 /*   110 */   590,  590,  590, 1591, 1591, 1591,  148,  373,  148,  148,
 /*   120 */   148,  148,  148,  148,  148,  148,  148,  148,  148,  148,
 /*   130 */   148,  148,  148,  148,  148,  148,  148,  148,  148,  148,
 /*   140 */   148,  148, 1271, 1271, 1271, 1271,   40, 1271, 1271, 1271,
 /*   150 */    40,  427,  427,  340,  654,  117,   40,  250,  575, 1421,
 /*   160 */  1420, 1594,  186,  742,  575,  575,  575,  575,  696,  250,
 /*   170 */   250, 1421, 1420, 1026, 1249, 1000, 1000, 1000, 1000, 1219,
 /*   180 */   913,  913, 1352, 1352, 1352, 1352, 1352, 1352, 1352, 1352,
 /*   190 */  1352, 1352, 1352,  477,  133,  135,  696, 1119, 1041,  917,
 /*   200 */  1041, 1027, 1041, 1027, 1056, 1015, 1063,  917, 1041, 1027,
 /*   210 */  1056, 1015, 1063,  917, 1041, 1027, 1056, 1015, 1063,  917,
 /*   220 */  1119, 1041, 1027, 1041, 1027, 1041, 1027, 1041, 1027, 1041,
 /*   230 */  1027, 1063, 1041, 1027,  917, 1041, 1027,  917, 1041, 1027,
 /*   240 */  1063, 1041, 1027, 1056, 1015, 1063, 1041, 1027, 1056, 1015,
 /*   250 */  1063, 1041, 1027, 1056, 1015,  872,  872, 1057, 1057, 1026,
 /*   260 */   917,  872,  849,  454, 1419, 1410, 1493, 1481, 1452, 1673,
 /*   270 */  1659, 1643, 1629, 1687,  366,  954,  900,  851,  803,  755,
 /*   280 */   707,  449,  649,  617, 1546, 1140, 1110, 1110, 1110, 1050,
 /*   290 */  1050, 1050, 1509,  929, 1369,  261, 1008,  656,  282,  779,
 /*   300 */  1068, 1068, 1068, 1068, 1068, 1068, 1068,  386,  303,  386,
 /*   310 */   386,  386,  386,  159, 1085,  386,  386,  386,  386, 1068,
 /*   320 */  1068, 1068,  386,  386,  386,  386,  386,  386,  386,  386,
 /*   330 */   386,  386,  386,  386,  386,  386,  386,  386,  386,  386,
 /*   340 */   386,  386,  386, 1055,  959,  386,  386,  139,  344,  344,
 /*   350 */   -50,  -50,  207,  207,  172,  172,   94,  -31,  -31,  -50,
 /*   360 */   -50,  -72,  -72,  -72,  -72,  -72,  -72,  -72,  -72,  658,
 /*   370 */   583,  601,  563,  476,  355,  191,   23,   23,   23,   23,
 /*   380 */   562,  690,  299,  629,  165,  233,  557,  551,  337,  524,
 /*   390 */    82,  462,   71,  198,  498,  -46,  150,  129,  428,  410,
 /*   400 */   306,   38,  -21,  864,  905,  904,  901,  898,  897,  896,
 /*   410 */   888,  881,  871,  857,  855,  894,  852,  875,  847,  833,
 /*   420 */   823,  799,  785,  725,  725,  776,  767,  763,  727,  718,
 /*   430 */   700,  683,  641,  641,  633,  633,  651,  638,  624,  616,
 /*   440 */   678,  526,  586,  572,  518,  511,  532,  509,  305,  305,
 /*   450 */   440,  439,  396,  345,  305,  275,  116,  202,  333,  323,
 /*   460 */   287,  204,  130,  109,  107,   91,   67,    2,   18,
};
#define YY_REDUCE_USE_DFLT (-171)
#define YY_REDUCE_COUNT (262)
#define YY_REDUCE_MIN   (-170)
#define YY_REDUCE_MAX   (2734)
static const short yy_reduce_ofst[] = {
 /*     0 */   745, -136,  188,  330,  279, -112, 1296, 1270, 1241, 1211,
 /*    10 */  1181, 1151, 1127, 1101, 1072, 1042, 1012,  982,  952,  916,
 /*    20 */   886,  856,  832,  808,  783,  759,  735,  711,  687,  663,
 /*    30 */   639,  613,  581,  -88,  550,  508,  457, 1517, 1490, 1453,
 /*    40 */   163, 1780, 1762, 1746, 1730, 1714, 1686, 1679, 1651, 1644,
 /*    50 */   155, 1822, 1809, 1475,  448,  265, 2456, 2446, 2436, 2426,
 /*    60 */  2416, 2402, 2392, 2382, 2370, 2360, 2350, 2332, 2322, 2311,
 /*    70 */  2301, 2291, 2281, 2271, 2261, 2247, 2237, 2227, 2215, 2205,
 /*    80 */  2195, 2177, 2167, 2156, 2146, 2136, 2126, 2116, 2106, 2092,
 /*    90 */  2082, 2072, 2060, 2050, 2040, 2022, 2012, 2001, 1991, 1981,
 /*   100 */  1971, 1961, 1951, 1937, 1927, 1917, 1905, 1895, 1885, 1867,
 /*   110 */  1857, 1842, 1832, 1387,  128,   49,  -28, -170, 2734, 2722,
 /*   120 */  2710, 2698, 2686, 2674, 2662, 2650, 2638, 2626, 2614, 2602,
 /*   130 */  2590, 2578, 2566, 2553, 2541, 2529, 2517, 2505, 2489, 2477,
 /*   140 */  2465,  304,  516,  474,  357,  278,   29, 1094,  934,  422,
 /*   150 */   169, 1303,  338, 1227,   33, -133,  480,  369,  296,  270,
 /*   160 */    20,  752,  134,  644,  611,  596,  569,  565,  558,  478,
 /*   170 */    99,  492,  573,  501, 1198, 1190, 1189, 1188, 1184, 1182,
 /*   180 */  1169, 1156, 1233, 1232, 1223, 1221, 1220, 1218, 1209, 1204,
 /*   190 */  1203, 1202, 1201, 1179, 1206, 1139, 1168, 1145, 1136, 1178,
 /*   200 */  1126, 1135, 1107, 1106, 1109, 1099, 1108, 1166, 1087, 1074,
 /*   210 */  1075, 1073, 1067, 1124, 1045, 1043, 1044, 1040, 1033, 1096,
 /*   220 */  1032, 1037, 1029, 1025, 1028, 1018, 1009, 1017,  999,  997,
 /*   230 */   996,  961,  989,  988, 1058,  978,  960,  967,  958,  957,
 /*   240 */   903,  953,  928,  932,  930,  902,  918,  910,  899,  895,
 /*   250 */   867,  861,  876,  863,  859,  921,  920,  878,  858,  836,
 /*   260 */   862,  835,  810,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   727, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*    10 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*    20 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*    30 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*    40 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*    50 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,  931,
 /*    60 */   935,  930,  934,  951,  947,  952,  948, 1099, 1099, 1099,
 /*    70 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*    80 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*    90 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   100 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   110 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   120 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   130 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   140 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   150 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   160 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   170 */  1099, 1099, 1099, 1099, 1048, 1050, 1050, 1050, 1050, 1056,
 /*   180 */  1048, 1048, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   190 */  1099, 1099, 1099, 1099, 1099, 1048, 1099, 1099, 1056, 1099,
 /*   200 */  1056, 1054, 1056, 1054, 1050, 1052, 1048, 1099, 1056, 1054,
 /*   210 */  1050, 1052, 1048, 1099, 1056, 1054, 1050, 1052, 1048, 1099,
 /*   220 */  1099, 1056, 1054, 1056, 1054, 1056, 1054, 1056, 1054, 1056,
 /*   230 */  1054, 1048, 1056, 1054, 1099, 1056, 1054, 1099, 1056, 1054,
 /*   240 */  1048, 1056, 1054, 1050, 1052, 1048, 1056, 1054, 1050, 1052,
 /*   250 */  1048, 1056, 1054, 1050, 1052, 1099, 1099, 1099, 1099, 1099,
 /*   260 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   270 */   884,  884, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   280 */  1099, 1099, 1099,  793, 1099, 1055, 1049, 1051, 1047, 1040,
 /*   290 */  1037,  924, 1099, 1099, 1099, 1099, 1099, 1099, 1099,  898,
 /*   300 */   910,  909,  908,  907,  906,  905,  904,  890,  921,  933,
 /*   310 */   937,  932,  936,  855,  838,  953,  949,  954,  950,  913,
 /*   320 */   766,  767,  870,  869,  868,  867,  866,  865,  864,  886,
 /*   330 */   883,  882,  881,  880,  879,  878,  877,  876,  875,  874,
 /*   340 */   873,  872,  871, 1099, 1099,  763,  762, 1099,  900,  899,
 /*   350 */   820,  819,  923,  922,  857,  856, 1099,  843,  844,  833,
 /*   360 */   834,  795,  794,  800,  799,  805,  804,  779,  780, 1099,
 /*   370 */  1099, 1099,  839,  747, 1099, 1099, 1017, 1021, 1020, 1018,
 /*   380 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   390 */  1099,  966, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   400 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   410 */  1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
 /*   420 */  1099, 1099, 1099,  897,  896,  888,  884,  751, 1099, 1099,
 /*   430 */  1099,  788,  920,  919,  854,  853, 1099, 1099, 1099, 1099,
 /*   440 */  1099, 1019, 1099, 1007,  984,  986, 1099,  999,  965,  964,
 /*   450 */   982, 1099, 1099,  981,  980, 1099, 1099, 1099, 1099, 1099,
 /*   460 */  1099, 1099,  911,  765,  761,  759,  755,  750, 1099, 1098,
 /*   470 */  1097, 1096, 1095, 1094, 1093, 1092, 1091, 1090, 1089, 1088,
 /*   480 */  1087,  946, 1086, 1085, 1084, 1083, 1082, 1081, 1080, 1079,
 /*   490 */  1077, 1076, 1078, 1075,  892,  894,  903,  902,  901,  895,
 /*   500 */   893,  891,  818,  817,  816,  815,  814,  813,  823,  822,
 /*   510 */   821,  812,  811,  810,  809, 1053,  887,  889,  752, 1074,
 /*   520 */  1073, 1072, 1071, 1070, 1069, 1068, 1067, 1066, 1065, 1064,
 /*   530 */  1063, 1062, 1061, 1060, 1059, 1058, 1057, 1033, 1032, 1039,
 /*   540 */  1038, 1046, 1045, 1042, 1041, 1036, 1044,  915,  917,  918,
 /*   550 */   916,  914,  849,  851,  926,  929,  928,  927,  925,  863,
 /*   560 */   862,  861,  860,  859,  858,  852,  850,  848,  791,  791,
 /*   570 */  1035, 1034, 1028, 1027, 1029, 1026, 1031, 1030, 1025, 1023,
 /*   580 */  1022, 1024, 1016, 1011, 1014, 1015, 1013, 1012, 1010, 1002,
 /*   590 */  1005, 1009, 1008, 1006, 1004, 1003, 1001,  977,  985,  987,
 /*   600 */  1000,  998,  997,  996,  995,  994,  993,  992,  991,  990,
 /*   610 */   989,  988,  983,  979,  975,  974,  973,  972,  971,  970,
 /*   620 */   969,  755,  968,  967,  845,  847,  846,  842,  841,  840,
 /*   630 */   839,  978,  976,  956,  959,  960,  963,  962,  961,  958,
 /*   640 */   957,  955,  945,  944,  943,  942,  941,  885,  939,  940,
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
 /* 220 */ "atomic_head_formula ::= DASH constant",
 /* 221 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 222 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R",
 /* 223 */ "formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 224 */ "formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term",
 /* 225 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 226 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R",
 /* 227 */ "formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 228 */ "formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term",
 /* 229 */ "stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD",
 /* 230 */ "macro_def_lst ::= macro_bnd",
 /* 231 */ "macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd",
 /* 232 */ "macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING",
 /* 233 */ "macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING",
 /* 234 */ "macro_args ::= macro_arg",
 /* 235 */ "macro_args ::= macro_args COMMA macro_arg",
 /* 236 */ "macro_arg ::= POUND_INTEGER",
 /* 237 */ "macro_arg ::= POUND_IDENTIFIER",
 /* 238 */ "sort_lst ::= sort",
 /* 239 */ "sort_lst ::= sort_lst COMMA sort",
 /* 240 */ "sort ::= sort_id_nr",
 /* 241 */ "sort ::= sort_id_nr STAR",
 /* 242 */ "sort ::= sort_id_nr CARROT",
 /* 243 */ "sort ::= sort PLUS object_nullary",
 /* 244 */ "sort ::= sort PLUS IDENTIFIER",
 /* 245 */ "sort ::= sort PLUS INTEGER",
 /* 246 */ "sort_id_nr ::= sort_id",
 /* 247 */ "sort_id_nr ::= sort_nr",
 /* 248 */ "sort_nr ::= num_range",
 /* 249 */ "sort_id ::= IDENTIFIER",
 /* 250 */ "stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD",
 /* 251 */ "constant_bnd_lst ::= constant_bnd",
 /* 252 */ "constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd",
 /* 253 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R",
 /* 254 */ "constant_bnd ::= constant_dcl_lst DBL_COLON sort",
 /* 255 */ "constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type",
 /* 256 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER",
 /* 257 */ "constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 258 */ "constant_dcl_lst ::= IDENTIFIER",
 /* 259 */ "constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 260 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER",
 /* 261 */ "constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 262 */ "constant_dcl_type ::= ABACTION",
 /* 263 */ "constant_dcl_type ::= ACTION",
 /* 264 */ "constant_dcl_type ::= ADDITIVEACTION",
 /* 265 */ "constant_dcl_type ::= ADDITIVEFLUENT",
 /* 266 */ "constant_dcl_type ::= EXTERNALACTION",
 /* 267 */ "constant_dcl_type ::= EXTERNALFLUENT",
 /* 268 */ "constant_dcl_type ::= EXOGENOUSACTION",
 /* 269 */ "constant_dcl_type ::= INERTIALFLUENT",
 /* 270 */ "constant_dcl_type ::= RIGID",
 /* 271 */ "constant_dcl_type ::= SIMPLEFLUENT",
 /* 272 */ "constant_dcl_type ::= SDFLUENT",
 /* 273 */ "attrib_spec ::= ATTRIBUTE",
 /* 274 */ "attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R",
 /* 275 */ "stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD",
 /* 276 */ "object_bnd_lst ::= object_bnd",
 /* 277 */ "object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd",
 /* 278 */ "object_bnd ::= object_lst DBL_COLON sort_id",
 /* 279 */ "object_lst ::= object_spec",
 /* 280 */ "object_lst ::= object_lst COMMA object_spec",
 /* 281 */ "object_spec ::= IDENTIFIER",
 /* 282 */ "object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R",
 /* 283 */ "object_spec ::= num_range",
 /* 284 */ "stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD",
 /* 285 */ "variable_bnd_lst ::= variable_bnd",
 /* 286 */ "variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd",
 /* 287 */ "variable_bnd ::= variable_lst DBL_COLON sort_id",
 /* 288 */ "variable_lst ::= IDENTIFIER",
 /* 289 */ "variable_lst ::= variable_lst COMMA IDENTIFIER",
 /* 290 */ "stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD",
 /* 291 */ "sort_bnd_lst ::= sort_bnd",
 /* 292 */ "sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd",
 /* 293 */ "sort_bnd ::= sort_dcl_lst",
 /* 294 */ "sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd",
 /* 295 */ "sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd",
 /* 296 */ "sort_bnd ::= PAREN_L sort_bnd PAREN_R",
 /* 297 */ "sort_dcl_lst ::= IDENTIFIER",
 /* 298 */ "sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER",
 /* 299 */ "stmt_show ::= COLON_DASH SHOW show_lst PERIOD",
 /* 300 */ "stmt_show ::= COLON_DASH SHOW ALL PERIOD",
 /* 301 */ "stmt_hide ::= COLON_DASH HIDE show_lst PERIOD",
 /* 302 */ "stmt_hide ::= COLON_DASH HIDE ALL PERIOD",
 /* 303 */ "show_lst ::= show_elem",
 /* 304 */ "show_lst ::= show_lst COMMA show_elem",
 /* 305 */ "show_elem ::= atomic_formula_one_const",
 /* 306 */ "stmt_noconcurrency ::= NOCONCURRENCY PERIOD",
 /* 307 */ "stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD",
 /* 308 */ "stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD",
 /* 309 */ "stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD",
 /* 310 */ "stmt_query ::= COLON_DASH QUERY query_lst PERIOD",
 /* 311 */ "query_lst ::= formula_temporal",
 /* 312 */ "query_lst ::= query_maxstep_decl",
 /* 313 */ "query_lst ::= query_label_decl",
 /* 314 */ "query_lst ::= query_lst SEMICOLON formula_temporal",
 /* 315 */ "query_lst ::= query_lst SEMICOLON query_maxstep_decl",
 /* 316 */ "query_lst ::= query_lst SEMICOLON query_label_decl",
 /* 317 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER",
 /* 318 */ "query_maxstep_decl ::= MAXSTEP DBL_COLON num_range",
 /* 319 */ "query_label_decl ::= LABEL DBL_COLON INTEGER",
 /* 320 */ "query_label_decl ::= LABEL DBL_COLON IDENTIFIER",
 /* 321 */ "clause_if ::= IF formula",
 /* 322 */ "clause_if ::=",
 /* 323 */ "clause_after ::= AFTER formula",
 /* 324 */ "clause_after ::=",
 /* 325 */ "clause_ifcons ::= IFCONS formula",
 /* 326 */ "clause_ifcons ::=",
 /* 327 */ "clause_unless ::= UNLESS atomic_formula_anon",
 /* 328 */ "clause_unless ::=",
 /* 329 */ "clause_where ::= WHERE formula_no_const",
 /* 330 */ "clause_where ::=",
 /* 331 */ "stmt_law ::= law_basic",
 /* 332 */ "stmt_law ::= law_caused",
 /* 333 */ "stmt_law ::= law_pcaused",
 /* 334 */ "stmt_law ::= law_impl",
 /* 335 */ "stmt_law ::= law_causes",
 /* 336 */ "stmt_law ::= law_increments",
 /* 337 */ "stmt_law ::= law_decrements",
 /* 338 */ "stmt_law ::= law_mcause",
 /* 339 */ "stmt_law ::= law_always",
 /* 340 */ "stmt_law ::= law_constraint",
 /* 341 */ "stmt_law ::= law_impossible",
 /* 342 */ "stmt_law ::= law_never",
 /* 343 */ "stmt_law ::= law_default",
 /* 344 */ "stmt_law ::= law_exogenous",
 /* 345 */ "stmt_law ::= law_inertial",
 /* 346 */ "stmt_law ::= law_nonexecutable",
 /* 347 */ "stmt_law ::= law_rigid",
 /* 348 */ "stmt_law ::= law_observed",
 /* 349 */ "law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 350 */ "law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 351 */ "law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 352 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 353 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 354 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 355 */ "law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 356 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 357 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 358 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 359 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 360 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 361 */ "law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 362 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 363 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 364 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 365 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 366 */ "law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD",
 /* 367 */ "stmt_code_blk ::= ASP_GR",
 /* 368 */ "stmt_code_blk ::= ASP_CP",
 /* 369 */ "stmt_code_blk ::= F2LP_GR",
 /* 370 */ "stmt_code_blk ::= F2LP_CP",
 /* 371 */ "stmt_code_blk ::= LUA_GR",
 /* 372 */ "stmt_code_blk ::= LUA_CP",
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
#line 2363 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 137: /* start */
    case 138: /* statement_lst */
    case 161: /* undeclared */
{
#line 208 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2372 "bcplus/parser/detail/lemon_parser.c"
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
#line 2385 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_macro_def */
{
#line 233 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy101));								
#line 2392 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_constant_def */
{
#line 235 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy5));								
#line 2399 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_object_def */
{
#line 237 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy78));								
#line 2406 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_variable_def */
{
#line 239 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy487));								
#line 2413 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 144: /* stmt_sort_def */
{
#line 241 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy195));								
#line 2420 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_noconcurrency */
{
#line 251 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy87));								
#line 2427 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 150: /* stmt_strong_noconcurrency */
{
#line 253 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy158));								
#line 2434 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* stmt_query */
{
#line 259 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy140));								
#line 2441 "bcplus/parser/detail/lemon_parser.c"
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
#line 2454 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* constant */
    case 164: /* constant_one_const */
    case 167: /* const_anon */
{
#line 297 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy327));								
#line 2463 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 157: /* object */
    case 158: /* object_nullary */
{
#line 299 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy208));								
#line 2471 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* variable */
{
#line 303 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy163));								
#line 2478 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 160: /* lua */
{
#line 305 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy313));								
#line 2485 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 162: /* term_lst */
    case 165: /* term_no_const_lst */
{
#line 309 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy197));								
#line 2493 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* num_range */
{
#line 707 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy331));								
#line 2500 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 172: /* term_numeric */
{
#line 709 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy458));								
#line 2507 "bcplus/parser/detail/lemon_parser.c"
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
#line 2522 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 176: /* atomic_formula */
    case 179: /* atomic_formula_anon */
    case 183: /* atomic_formula_one_const */
    case 190: /* atomic_head_formula */
{
#line 776 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy262));								
#line 2532 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 177: /* formula_quant */
{
#line 778 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy381));								
#line 2539 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_lst */
{
#line 979 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy147));								
#line 2546 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* quant_op */
{
#line 981 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2553 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 187: /* card_var_lst */
    case 188: /* card_var_lst_inner */
{
#line 1018 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy125));								
#line 2561 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 191: /* formula_smpl_card */
{
#line 1096 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy91));								
#line 2568 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 192: /* macro_def_lst */
{
#line 1141 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy179));                              
#line 2575 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 193: /* macro_bnd */
{
#line 1143 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy403));                              
#line 2582 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 194: /* macro_args */
{
#line 1145 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy220));                              
#line 2589 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 195: /* macro_arg */
{
#line 1147 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy93));                              
#line 2596 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 196: /* sort_lst */
{
#line 1237 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy305));							
#line 2603 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 197: /* sort */
    case 198: /* sort_id_nr */
    case 199: /* sort_nr */
    case 200: /* sort_id */
{
#line 1239 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2613 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 201: /* constant_bnd_lst */
    case 202: /* constant_bnd */
{
#line 1367 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy55));									
#line 2621 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* constant_dcl_lst */
{
#line 1371 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy484));									
#line 2628 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* constant_dcl_type */
{
#line 1373 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2635 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* attrib_spec */
{
#line 1375 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2642 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 206: /* object_bnd_lst */
{
#line 1731 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy485));									
#line 2649 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 207: /* object_bnd */
{
#line 1733 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy190));									
#line 2656 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 208: /* object_lst */
    case 209: /* object_spec */
{
#line 1735 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy148));									
#line 2664 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 210: /* variable_bnd_lst */
    case 211: /* variable_bnd */
{
#line 1845 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy97));									
#line 2672 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* variable_lst */
{
#line 1849 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy408));									
#line 2679 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 213: /* sort_bnd_lst */
    case 214: /* sort_bnd */
    case 215: /* sort_dcl_lst */
{
#line 1922 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy245));									
#line 2688 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* show_lst */
{
#line 2026 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy165));									
#line 2695 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* show_elem */
    case 225: /* clause_unless */
{
#line 2028 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy262));									
#line 2703 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 218: /* query_lst */
{
#line 2173 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy431).l); DEALLOC((yypminor->yy431).maxstep); DEALLOC((yypminor->yy431).label);	
#line 2710 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 219: /* query_maxstep_decl */
{
#line 2175 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy221));												
#line 2717 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 221: /* query_label_Decl */
{
#line 2177 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2724 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 222: /* clause_if */
    case 223: /* clause_after */
    case 224: /* clause_ifcons */
    case 226: /* clause_where */
{
#line 2331 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy223));									
#line 2734 "bcplus/parser/detail/lemon_parser.c"
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
#line 2372 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy230));									
#line 2758 "bcplus/parser/detail/lemon_parser.c"
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
#line 3436 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 219 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy230;
			yymsp[0].minor.yy230  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3445 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 262 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy101; }
#line 3450 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy5; }
#line 3455 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy78; }
#line 3460 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy487; }
#line 3465 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy195; }
#line 3470 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy230; }
#line 3480 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 271 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy87; }
#line 3485 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy158; }
#line 3490 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 275 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy230 = yymsp[0].minor.yy140; }
#line 3495 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 321 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy88 = yymsp[0].minor.yy327; }
#line 3500 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 42: /* term ::= base_elem */ yytestcase(yyruleno==42);
      case 58: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==58);
      case 83: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==83);
      case 98: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==98);
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy88 = yymsp[0].minor.yy88; }
#line 3509 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 324 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy88 = yymsp[0].minor.yy208;	}
#line 3514 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 325 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy88 = yymsp[0].minor.yy163; }
#line 3519 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 326 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy88 = yymsp[0].minor.yy313; }
#line 3524 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 38: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==38);
#line 443 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy327, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy197, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3530 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 39: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==39);
#line 444 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy327, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3536 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* const_anon ::= IDENTIFIER */
#line 446 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy327, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true); }
#line 3541 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 447 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy327, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy197, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true);	}
#line 3546 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 450 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy208, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy197, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3551 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= object_nullary */
#line 451 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy208 = yymsp[0].minor.yy208; }
#line 3556 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* object_nullary ::= OBJECT_ID */
#line 452 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy208, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3561 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* object ::= undeclared */
#line 453 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3566 "bcplus/parser/detail/lemon_parser.c"
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
#line 3581 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 467 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy313, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy197, yymsp[0].minor.yy0); }
#line 3586 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* lua ::= AT_IDENTIFIER */
#line 468 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy313, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3591 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 469 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy423, yymsp[-3].minor.yy0, yymsp[-1].minor.yy197);   yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3598 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* undeclared ::= IDENTIFIER */
#line 470 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy423, yymsp[0].minor.yy0, NULL); }
#line 3603 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 36: /* term_lst ::= term */
      case 40: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==40);
#line 473 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy197 = new TermList();
			yygotominor.yy197->push_back(yymsp[0].minor.yy88);
		}
#line 3612 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 37: /* term_lst ::= term_lst COMMA term */
      case 41: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==41);
#line 479 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy197 = yymsp[-2].minor.yy197;
			yymsp[-2].minor.yy197->push_back(yymsp[0].minor.yy88);
		  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 3622 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 43: /* term ::= INTEGER */
      case 59: /* term_strong ::= INTEGER */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==99);
#line 578 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy88, yymsp[0].minor.yy0);	}
#line 3630 "bcplus/parser/detail/lemon_parser.c"
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
#line 3642 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 45: /* term ::= PAREN_L term PAREN_R */
      case 61: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==61);
      case 86: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==86);
      case 101: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==101);
#line 580 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy88, yymsp[-2].minor.yy0, yymsp[-1].minor.yy88, yymsp[0].minor.yy0); }
#line 3650 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 48: /* term ::= MAXSTEP */
      case 62: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==87);
#line 583 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy88, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3657 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= MAXADDITIVE */
      case 63: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==63);
      case 88: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==88);
#line 584 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy88, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3664 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= MAXAFVALUE */
      case 64: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==64);
      case 89: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==89);
#line 585 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy88, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3671 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= DASH term */
      case 65: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==65);
      case 91: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==91);
      case 105: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==105);
#line 589 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, UnaryTerm::Operator::NEGATIVE); }
#line 3679 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= ABS term */
      case 66: /* term_strong ::= ABS term */ yytestcase(yyruleno==66);
      case 92: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==92);
      case 106: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==106);
#line 590 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, UnaryTerm::Operator::ABS); }
#line 3687 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= term DASH term */
      case 68: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==68);
      case 78: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==78);
      case 93: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==93);
      case 107: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==107);
#line 594 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::MINUS); }
#line 3696 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= term PLUS term */
      case 69: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==69);
      case 79: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==79);
      case 94: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==94);
      case 108: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==108);
#line 595 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::PLUS); }
#line 3705 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= term STAR term */
      case 70: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==70);
      case 80: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==80);
      case 95: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==95);
      case 109: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==109);
#line 596 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::TIMES); }
#line 3714 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 56: /* term ::= term INT_DIV term */
      case 71: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==71);
      case 81: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==81);
      case 96: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==96);
      case 110: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==110);
#line 597 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::DIVIDE); }
#line 3723 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 57: /* term ::= term MOD term */
      case 72: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==72);
      case 82: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==82);
      case 97: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==97);
      case 111: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==111);
#line 598 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::MOD); }
#line 3732 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 67: /* term_strong_candidate ::= DASH constant */
#line 617 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy327, UnaryTerm::Operator::NEGATIVE); }
#line 3737 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 73: /* term_strong ::= constant DASH term */
#line 626 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy327, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::MINUS); }
#line 3742 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant PLUS term */
#line 627 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy327, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::PLUS); }
#line 3747 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant STAR term */
#line 628 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy327, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::TIMES); }
#line 3752 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 76: /* term_strong ::= constant INT_DIV term */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy327, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::DIVIDE); }
#line 3757 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong ::= constant MOD term */
#line 630 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy88, yymsp[-2].minor.yy327, yymsp[-1].minor.yy0, yymsp[0].minor.yy88, BinaryTerm::Operator::MOD); }
#line 3762 "bcplus/parser/detail/lemon_parser.c"
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
#line 3773 "bcplus/parser/detail/lemon_parser.c"
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
#line 3784 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 112: /* num_range ::= term_numeric DBL_PERIOD term_numeric */
#line 712 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy458, r_ptr = yymsp[0].minor.yy458, s_ptr = yymsp[-1].minor.yy0;

	yygotominor.yy331 = new NumberRange(yymsp[-2].minor.yy458->val(), yymsp[0].minor.yy458->val(), yymsp[-2].minor.yy458->beginLoc(), yymsp[0].minor.yy458->endLoc());

}
#line 3794 "bcplus/parser/detail/lemon_parser.c"
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
#line 3810 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 114: /* term_numeric ::= PAREN_L term_numeric PAREN_R */
#line 733 "bcplus/parser/detail/lemon_parser.y"
{ 
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy458 = yymsp[-1].minor.yy458;  
	yygotominor.yy458->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy458->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 3820 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 115: /* term_numeric ::= DASH term_numeric */
#line 753 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy458, yymsp[0].minor.yy458, -1 * yymsp[0].minor.yy458->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3826 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 116: /* term_numeric ::= ABS term_numeric */
#line 754 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy458, yymsp[0].minor.yy458, yymsp[0].minor.yy458->val() < 0 ? - yymsp[0].minor.yy458->val() : yymsp[0].minor.yy458->val());   yy_destructor(yypParser,111,&yymsp[-1].minor);
}
#line 3832 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 117: /* term_numeric ::= term_numeric DASH term_numeric */
#line 756 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() - yymsp[0].minor.yy458->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3838 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 118: /* term_numeric ::= term_numeric PLUS term_numeric */
#line 757 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() + yymsp[0].minor.yy458->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 3844 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 119: /* term_numeric ::= term_numeric STAR term_numeric */
#line 758 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() * yymsp[0].minor.yy458->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 3850 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 120: /* term_numeric ::= term_numeric INT_DIV term_numeric */
#line 759 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() / yymsp[0].minor.yy458->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 3856 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 121: /* term_numeric ::= term_numeric MOD term_numeric */
#line 760 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy458, yymsp[-2].minor.yy458, yymsp[0].minor.yy458, yymsp[-2].minor.yy458->val() % yymsp[0].minor.yy458->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 3862 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 122: /* formula ::= formula_base */
      case 165: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==165);
      case 188: /* formula_temporal ::= formula_base */ yytestcase(yyruleno==188);
#line 818 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = yymsp[0].minor.yy223;				}
#line 3869 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 123: /* formula ::= PAREN_L formula PAREN_R */
      case 166: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==166);
      case 189: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==189);
#line 819 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = yymsp[-1].minor.yy223; yygotominor.yy223->parens(true); 	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3878 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 124: /* formula ::= NOT formula */
      case 167: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==167);
      case 190: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==190);
#line 820 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 3885 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 125: /* formula ::= DASH formula */
      case 168: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==168);
      case 191: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==191);
#line 821 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 3892 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 126: /* formula ::= formula AMP formula */
      case 169: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==169);
      case 192: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==192);
#line 822 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy223, yymsp[0].minor.yy223, yymsp[-2].minor.yy223->beginLoc(), yymsp[0].minor.yy223->endLoc());   yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3900 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 127: /* formula ::= formula DBL_PLUS formula */
      case 128: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==128);
      case 170: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==170);
      case 171: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==171);
      case 193: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==193);
      case 194: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==194);
#line 823 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy223, yymsp[-2].minor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, BinaryFormula::Operator::OR); }
#line 3910 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 129: /* formula ::= formula EQUIV formula */
      case 172: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==172);
      case 195: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==195);
#line 825 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy223, yymsp[-2].minor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, BinaryFormula::Operator::EQUIV); }
#line 3917 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* formula ::= formula IMPL formula */
      case 131: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==131);
      case 173: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==173);
      case 174: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==174);
      case 196: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==196);
      case 197: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==197);
#line 826 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy223, yymsp[-2].minor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, BinaryFormula::Operator::IMPL); }
#line 3927 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* formula_base ::= comparison */
      case 175: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==175);
      case 215: /* head_formula ::= comparison */ yytestcase(yyruleno==215);
#line 829 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = yymsp[0].minor.yy223; }
#line 3934 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* formula_base ::= atomic_formula */
      case 216: /* head_formula ::= atomic_head_formula */ yytestcase(yyruleno==216);
#line 830 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = yymsp[0].minor.yy262; }
#line 3940 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* formula_base ::= formula_quant */
#line 831 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = yymsp[0].minor.yy381; }
#line 3945 "bcplus/parser/detail/lemon_parser.c"
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
#line 3956 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* formula_base ::= TRUE */
      case 176: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==176);
      case 218: /* head_formula ::= TRUE */ yytestcase(yyruleno==218);
#line 840 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3963 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* formula_base ::= FALSE */
      case 177: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==177);
      case 219: /* head_formula ::= FALSE */ yytestcase(yyruleno==219);
#line 841 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3970 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* comparison ::= term_strong EQ term */
      case 145: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==145);
      case 178: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==178);
#line 843 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3978 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* comparison ::= term_strong DBL_EQ term */
      case 146: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==146);
      case 179: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==179);
#line 844 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3986 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* comparison ::= term_strong NEQ term */
      case 147: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==147);
      case 180: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==180);
#line 845 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3994 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* comparison ::= term_strong LTHAN term */
      case 148: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==148);
      case 181: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==181);
#line 846 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4002 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* comparison ::= term_strong GTHAN term */
      case 149: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==149);
      case 182: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==182);
#line 847 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4010 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* comparison ::= term_strong LTHAN_EQ term */
      case 150: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==150);
      case 183: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==183);
#line 848 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4018 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 144: /* comparison ::= term_strong GTHAN_EQ term */
      case 151: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==151);
      case 184: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==184);
#line 849 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy88, yymsp[0].minor.yy88, yymsp[-2].minor.yy88->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 4026 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* comparison ::= constant DBL_EQ term */
#line 857 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4032 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* comparison ::= constant NEQ term */
#line 858 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 4038 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* comparison ::= constant LTHAN term */
#line 859 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4044 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* comparison ::= constant GTHAN term */
#line 860 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4050 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* comparison ::= constant LTHAN_EQ term */
#line 861 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4056 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= constant GTHAN_EQ term */
#line 862 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 4062 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* atomic_formula ::= constant */
      case 162: /* atomic_formula_anon ::= const_anon */ yytestcase(yyruleno==162);
      case 185: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==185);
#line 889 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy262, yymsp[0].minor.yy327, true); }
#line 4069 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* atomic_formula ::= TILDE constant */
      case 163: /* atomic_formula_anon ::= TILDE const_anon */ yytestcase(yyruleno==163);
      case 186: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==186);
#line 890 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy262, yymsp[0].minor.yy327, false);   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 4077 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* atomic_formula ::= constant EQ term */
      case 164: /* atomic_formula_anon ::= const_anon EQ term */ yytestcase(yyruleno==164);
      case 187: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==187);
#line 891 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy262 = new AtomicFormula(yymsp[-2].minor.yy327, yymsp[0].minor.yy88, yymsp[-2].minor.yy327->beginLoc(), yymsp[0].minor.yy88->endLoc());	  yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4085 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 161: /* atomic_formula_anon ::= atomic_formula */
      case 305: /* show_elem ::= atomic_formula_one_const */ yytestcase(yyruleno==305);
#line 893 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy262 = yymsp[0].minor.yy262; }
#line 4091 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 198: /* formula_temporal ::= term_strong COLON formula_temporal */
#line 972 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy223, yymsp[-2].minor.yy88, yymsp[-1].minor.yy0, yymsp[0].minor.yy223); }
#line 4096 "bcplus/parser/detail/lemon_parser.c"
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
#line 4113 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 200: /* quant_lst ::= quant_op variable */
#line 998 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy147 = new QuantifierFormula::QuantifierList();
		yygotominor.yy147->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy111, yymsp[0].minor.yy163));
	}
#line 4121 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 201: /* quant_lst ::= quant_lst quant_op variable */
#line 1004 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy147 = yymsp[-2].minor.yy147;
		yygotominor.yy147->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy111, yymsp[0].minor.yy163));
	}
#line 4129 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 202: /* quant_op ::= BIG_CONJ */
#line 1009 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy111 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4135 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 203: /* quant_op ::= BIG_DISJ */
#line 1010 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy111 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,92,&yymsp[0].minor);
}
#line 4141 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 204: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
#line 1056 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy125, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, NULL);  }
#line 4146 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 205: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
#line 1057 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, yymsp[-4].minor.yy88, yymsp[-3].minor.yy0, yymsp[-2].minor.yy125, yymsp[-1].minor.yy223,  yymsp[0].minor.yy0, NULL);  }
#line 4151 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 206: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 1058 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy125, yymsp[-2].minor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4156 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 207: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 1059 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, yymsp[-5].minor.yy88, yymsp[-4].minor.yy0, yymsp[-3].minor.yy125, yymsp[-2].minor.yy223,  yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4161 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
#line 1060 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, NULL);  }
#line 4166 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 209: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
#line 1061 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, yymsp[-3].minor.yy88, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy223,  yymsp[0].minor.yy0, NULL);  }
#line 4171 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
#line 1062 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4176 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
#line 1063 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy223, yymsp[-4].minor.yy88, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy223,  yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4181 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1067 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy125 = yymsp[-1].minor.yy125;
	  yy_destructor(yypParser,99,&yymsp[0].minor);
}
#line 4189 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* card_var_lst_inner ::= variable */
#line 1072 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy163;
		yygotominor.yy125 = new CardinalityFormula::VariableList();
		yygotominor.yy125->push_back(yymsp[0].minor.yy163->symbol());
	}
#line 4198 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1079 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy163;
		yygotominor.yy125 = yymsp[-2].minor.yy125;
		yygotominor.yy125->push_back(yymsp[0].minor.yy163->symbol());
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4208 "bcplus/parser/detail/lemon_parser.c"
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
#line 4219 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 220: /* atomic_head_formula ::= DASH constant */
#line 1112 "bcplus/parser/detail/lemon_parser.y"
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
#line 4235 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 221: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1125 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy125, yymsp[-1].minor.yy262, yymsp[0].minor.yy0, NULL);  }
#line 4240 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1126 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, yymsp[-4].minor.yy88, yymsp[-3].minor.yy0, yymsp[-2].minor.yy125, yymsp[-1].minor.yy262,  yymsp[0].minor.yy0, NULL);  }
#line 4245 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 223: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1127 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy125, yymsp[-2].minor.yy262, yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4250 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 224: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1128 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, yymsp[-5].minor.yy88, yymsp[-4].minor.yy0, yymsp[-3].minor.yy125, yymsp[-2].minor.yy262,  yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4255 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 225: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1129 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy262, yymsp[0].minor.yy0, NULL);  }
#line 4260 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 226: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1130 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, yymsp[-3].minor.yy88, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy262,  yymsp[0].minor.yy0, NULL);  }
#line 4265 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 227: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1131 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy262, yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4270 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 228: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1132 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy91, yymsp[-4].minor.yy88, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy262,  yymsp[-1].minor.yy0, yymsp[0].minor.yy88); 	}
#line 4275 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 229: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1151 "bcplus/parser/detail/lemon_parser.y"
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
#line 4305 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 230: /* macro_def_lst ::= macro_bnd */
#line 1179 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy179 = new MacroDeclaration::ElementList();
        yygotominor.yy179->push_back(yymsp[0].minor.yy403);
    }
#line 4313 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 231: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1185 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy179 = yymsp[-2].minor.yy179;
        yygotominor.yy179->push_back(yymsp[0].minor.yy403);
      yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4322 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1191 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy220;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy403 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy220);
      yy_destructor(yypParser,72,&yymsp[-4].minor);
  yy_destructor(yypParser,73,&yymsp[-2].minor);
  yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4336 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 233: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1200 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy403 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4347 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 234: /* macro_args ::= macro_arg */
#line 1208 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy220 = new MacroSymbol::ArgumentList();
        yygotominor.yy220->push_back(yymsp[0].minor.yy93->str());
        delete yymsp[0].minor.yy93;
    }
#line 4356 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* macro_args ::= macro_args COMMA macro_arg */
#line 1214 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy220 = yymsp[-2].minor.yy220;
        yygotominor.yy220->push_back(yymsp[0].minor.yy93->str());
        delete yymsp[0].minor.yy93;
      yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4366 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 236: /* macro_arg ::= POUND_INTEGER */
      case 237: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==237);
#line 1221 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy93 = yymsp[0].minor.yy0;
    }
#line 4374 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 238: /* sort_lst ::= sort */
#line 1248 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy305 = new ConstantSymbol::SortList();
		yygotominor.yy305->push_back(yymsp[0].minor.yy315);
	}
#line 4382 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 239: /* sort_lst ::= sort_lst COMMA sort */
#line 1253 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy305 = yymsp[-2].minor.yy305;
		yygotominor.yy305->push_back(yymsp[0].minor.yy315);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4391 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 240: /* sort ::= sort_id_nr */
      case 246: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==246);
      case 247: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==247);
#line 1278 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy315 = yymsp[0].minor.yy315; }
#line 4398 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 241: /* sort ::= sort_id_nr STAR */
#line 1279 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy315, yymsp[-1].minor.yy315, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::NONE)); }
#line 4403 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 242: /* sort ::= sort_id_nr CARROT */
#line 1280 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy315, yymsp[-1].minor.yy315, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::UNKNOWN)); }
#line 4408 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 243: /* sort ::= sort PLUS object_nullary */
#line 1282 "bcplus/parser/detail/lemon_parser.y"
{ u::ref_ptr<const Object> o_ptr = yymsp[0].minor.yy208; DYNAMIC_SORT_PLUS(yygotominor.yy315, yymsp[-2].minor.yy315, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, yymsp[0].minor.yy208->symbol()); }
#line 4413 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 244: /* sort ::= sort PLUS IDENTIFIER */
#line 1285 "bcplus/parser/detail/lemon_parser.y"
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
#line 4430 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 245: /* sort ::= sort PLUS INTEGER */
#line 1299 "bcplus/parser/detail/lemon_parser.y"
{ 
												  ref_ptr<const Object> t_ptr;
												  BASIC_TERM(t_ptr, yymsp[0].minor.yy0);
												  DYNAMIC_SORT_PLUS(yygotominor.yy315, yymsp[-2].minor.yy315, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, t_ptr->symbol()); 
												}
#line 4439 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 248: /* sort_nr ::= num_range */
#line 1310 "bcplus/parser/detail/lemon_parser.y"
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
#line 4478 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 249: /* sort_id ::= IDENTIFIER */
#line 1347 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy315 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy315) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4491 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 250: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1378 "bcplus/parser/detail/lemon_parser.y"
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
#line 4510 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 251: /* constant_bnd_lst ::= constant_bnd */
#line 1395 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy55 = yymsp[0].minor.yy55;
	}
#line 4517 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 252: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1400 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy55;
		yygotominor.yy55 = yymsp[-2].minor.yy55;
		yygotominor.yy55->splice(yygotominor.yy55->end(), *yymsp[0].minor.yy55);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4527 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 253: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1420 "bcplus/parser/detail/lemon_parser.y"
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
#line 4555 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 254: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1442 "bcplus/parser/detail/lemon_parser.y"
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
#line 4570 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 255: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1453 "bcplus/parser/detail/lemon_parser.y"
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
#line 4597 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 256: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1476 "bcplus/parser/detail/lemon_parser.y"
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
#line 4626 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 257: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1500 "bcplus/parser/detail/lemon_parser.y"
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
#line 4707 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 258: /* constant_dcl_lst ::= IDENTIFIER */
#line 1576 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = new IdentifierDeclList();
		yygotominor.yy484->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 4715 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 259: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1581 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = new IdentifierDeclList();
		yygotominor.yy484->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy305));
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4725 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 260: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1586 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = yymsp[-2].minor.yy484;
		yygotominor.yy484->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4734 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 261: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1591 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy484 = yymsp[-5].minor.yy484;
		yygotominor.yy484->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy305));
	  yy_destructor(yypParser,103,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4745 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 262: /* constant_dcl_type ::= ABACTION */
#line 1598 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4757 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 263: /* constant_dcl_type ::= ACTION */
#line 1607 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4769 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 264: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1616 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4781 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1625 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4793 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* constant_dcl_type ::= EXTERNALACTION */
#line 1634 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4805 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 267: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1643 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4817 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1652 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4829 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 269: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1661 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4841 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 270: /* constant_dcl_type ::= RIGID */
#line 1670 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4853 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 271: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1679 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4865 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 272: /* constant_dcl_type ::= SDFLUENT */
#line 1689 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy409 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4877 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 273: /* attrib_spec ::= ATTRIBUTE */
#line 1699 "bcplus/parser/detail/lemon_parser.y"
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
#line 4892 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 274: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1712 "bcplus/parser/detail/lemon_parser.y"
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
#line 4908 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 275: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1740 "bcplus/parser/detail/lemon_parser.y"
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
#line 4933 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 276: /* object_bnd_lst ::= object_bnd */
#line 1763 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy485 = new ObjectDeclaration::ElementList();
		yygotominor.yy485->push_back(yymsp[0].minor.yy190);
	}
#line 4941 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 277: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 1769 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy485 = yymsp[-2].minor.yy485;
		yygotominor.yy485->push_back(yymsp[0].minor.yy190);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4950 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 278: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 1775 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy190 = new ObjectDeclaration::Element(yymsp[0].minor.yy315, yymsp[-2].minor.yy148);
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4958 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 279: /* object_lst ::= object_spec */
#line 1780 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy148 = yymsp[0].minor.yy148;
	}
#line 4965 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 280: /* object_lst ::= object_lst COMMA object_spec */
#line 1784 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy148 = yymsp[-2].minor.yy148;
		yygotominor.yy148->splice(yygotominor.yy148->end(), *yymsp[0].minor.yy148);
		delete yymsp[0].minor.yy148;
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4975 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 281: /* object_spec ::= IDENTIFIER */
#line 1793 "bcplus/parser/detail/lemon_parser.y"
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
#line 4991 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 282: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1806 "bcplus/parser/detail/lemon_parser.y"
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
#line 5010 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 283: /* object_spec ::= num_range */
#line 1820 "bcplus/parser/detail/lemon_parser.y"
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
#line 5030 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 284: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 1852 "bcplus/parser/detail/lemon_parser.y"
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
#line 5061 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 285: /* variable_bnd_lst ::= variable_bnd */
#line 1881 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = yymsp[0].minor.yy97;
	}
#line 5068 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 286: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 1886 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = yymsp[-2].minor.yy97;
		yygotominor.yy97->splice(yygotominor.yy97->end(), *yymsp[0].minor.yy97);
		delete yymsp[0].minor.yy97;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5078 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* variable_bnd ::= variable_lst DBL_COLON sort_id */
#line 1893 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy408) {
			yygotominor.yy97->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy315));
		}
		delete yymsp[-2].minor.yy408;
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5091 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* variable_lst ::= IDENTIFIER */
#line 1903 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy408 = new TokenList();
		yygotominor.yy408->push_back(yymsp[0].minor.yy0);
	}
#line 5099 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 1908 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy408 = yymsp[-2].minor.yy408;
		yygotominor.yy408->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5108 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 1929 "bcplus/parser/detail/lemon_parser.y"
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
#line 5126 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* sort_bnd_lst ::= sort_bnd */
      case 293: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==293);
#line 1945 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy245 = yymsp[0].minor.yy245;
	}
#line 5134 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 1950 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy245 = yymsp[-2].minor.yy245;
		yygotominor.yy245->splice(yygotominor.yy245->end(), *yymsp[0].minor.yy245);
		delete yymsp[0].minor.yy245;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5144 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 294: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 1962 "bcplus/parser/detail/lemon_parser.y"
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
#line 5160 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 1974 "bcplus/parser/detail/lemon_parser.y"
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
#line 5175 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 1985 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy245 = yymsp[-1].minor.yy245;
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 5184 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* sort_dcl_lst ::= IDENTIFIER */
#line 1990 "bcplus/parser/detail/lemon_parser.y"
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
#line 5201 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 2004 "bcplus/parser/detail/lemon_parser.y"
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
#line 5220 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 2031 "bcplus/parser/detail/lemon_parser.y"
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
#line 5236 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 2045 "bcplus/parser/detail/lemon_parser.y"
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
#line 5254 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 2062 "bcplus/parser/detail/lemon_parser.y"
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
#line 5270 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2076 "bcplus/parser/detail/lemon_parser.y"
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
#line 5288 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* show_lst ::= show_elem */
#line 2094 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165 = new ShowStatement::ElementList();
		yygotominor.yy165->push_back(yymsp[0].minor.yy262);
	}
#line 5296 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* show_lst ::= show_lst COMMA show_elem */
#line 2099 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy165 = yymsp[-2].minor.yy165;
		yygotominor.yy165->push_back(yymsp[0].minor.yy262);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5305 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* stmt_noconcurrency ::= NOCONCURRENCY PERIOD */
#line 2127 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy87, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5310 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD */
#line 2128 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy158, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5315 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD */
#line 2154 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy230, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy458, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5321 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD */
#line 2155 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy230, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy458, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5327 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2180 "bcplus/parser/detail/lemon_parser.y"
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
#line 5364 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* query_lst ::= formula_temporal */
#line 2216 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy431.l = new QueryStatement::FormulaList();
		yygotominor.yy431.maxstep = NULL;
		yygotominor.yy431.label = NULL;

		yygotominor.yy431.l->push_back(yymsp[0].minor.yy223);
	}
#line 5375 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* query_lst ::= query_maxstep_decl */
#line 2225 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy431.l = new QueryStatement::FormulaList();
		yygotominor.yy431.maxstep = yymsp[0].minor.yy221;
		yygotominor.yy431.label = NULL;
	}
#line 5384 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* query_lst ::= query_label_decl */
#line 2232 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy431.l = new QueryStatement::FormulaList();
		yygotominor.yy431.maxstep = NULL;
		yygotominor.yy431.label = yymsp[0].minor.yy93;
	}
#line 5393 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2239 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy431 = yymsp[-2].minor.yy431;
		yymsp[-2].minor.yy431.l->push_back(yymsp[0].minor.yy223);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5402 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2245 "bcplus/parser/detail/lemon_parser.y"
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
#line 5418 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2258 "bcplus/parser/detail/lemon_parser.y"
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
#line 5434 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2284 "bcplus/parser/detail/lemon_parser.y"
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
#line 5459 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range */
#line 2305 "bcplus/parser/detail/lemon_parser.y"
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
#line 5476 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 320: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==320);
#line 2319 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy93, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5483 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* clause_if ::= IF formula */
#line 2354 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, Language::Feature::CLAUSE_IF); 		}
#line 5488 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 322: /* clause_if ::= */
      case 324: /* clause_after ::= */ yytestcase(yyruleno==324);
      case 326: /* clause_ifcons ::= */ yytestcase(yyruleno==326);
      case 330: /* clause_where ::= */ yytestcase(yyruleno==330);
#line 2355 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy223 = NULL; }
#line 5496 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 323: /* clause_after ::= AFTER formula */
#line 2356 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, Language::Feature::CLAUSE_AFTER);	}
#line 5501 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 325: /* clause_ifcons ::= IFCONS formula */
#line 2358 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, Language::Feature::CLAUSE_IFCONS); 	}
#line 5506 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 327: /* clause_unless ::= UNLESS atomic_formula_anon */
#line 2360 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy262, yymsp[-1].minor.yy0, yymsp[0].minor.yy262, Language::Feature::CLAUSE_UNLESS); 	}
#line 5511 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 328: /* clause_unless ::= */
#line 2361 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy262 = NULL; }
#line 5516 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* clause_where ::= WHERE formula_no_const */
#line 2362 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy223, yymsp[-1].minor.yy0, yymsp[0].minor.yy223, Language::Feature::CLAUSE_WHERE); 	}
#line 5521 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 331: /* stmt_law ::= law_basic */
      case 332: /* stmt_law ::= law_caused */ yytestcase(yyruleno==332);
      case 333: /* stmt_law ::= law_pcaused */ yytestcase(yyruleno==333);
      case 334: /* stmt_law ::= law_impl */ yytestcase(yyruleno==334);
      case 335: /* stmt_law ::= law_causes */ yytestcase(yyruleno==335);
      case 336: /* stmt_law ::= law_increments */ yytestcase(yyruleno==336);
      case 337: /* stmt_law ::= law_decrements */ yytestcase(yyruleno==337);
      case 338: /* stmt_law ::= law_mcause */ yytestcase(yyruleno==338);
      case 339: /* stmt_law ::= law_always */ yytestcase(yyruleno==339);
      case 340: /* stmt_law ::= law_constraint */ yytestcase(yyruleno==340);
      case 341: /* stmt_law ::= law_impossible */ yytestcase(yyruleno==341);
      case 342: /* stmt_law ::= law_never */ yytestcase(yyruleno==342);
      case 343: /* stmt_law ::= law_default */ yytestcase(yyruleno==343);
      case 344: /* stmt_law ::= law_exogenous */ yytestcase(yyruleno==344);
      case 345: /* stmt_law ::= law_inertial */ yytestcase(yyruleno==345);
      case 346: /* stmt_law ::= law_nonexecutable */ yytestcase(yyruleno==346);
      case 347: /* stmt_law ::= law_rigid */ yytestcase(yyruleno==347);
      case 348: /* stmt_law ::= law_observed */ yytestcase(yyruleno==348);
#line 2408 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy230 = yymsp[0].minor.yy230;}
#line 5543 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2524 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, NULL, yymsp[-6].minor.yy223, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
																																															Language::Feature::LAW_BASIC_D, BasicLaw); }
#line 5550 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2528 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, yymsp[-7].minor.yy0, yymsp[-6].minor.yy223, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 5557 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* law_pcaused ::= POSSIBLY_CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2532 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, yymsp[-7].minor.yy0, yymsp[-6].minor.yy223, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 5564 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2536 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy230, yymsp[-4].minor.yy223, yymsp[-3].minor.yy0, yymsp[-2].minor.yy223, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 5570 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2539 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy230, yymsp[-6].minor.yy262, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 5576 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2543 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy230, yymsp[-8].minor.yy262, yymsp[-7].minor.yy0, yymsp[-6].minor.yy327, yymsp[-4].minor.yy88, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5583 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2546 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy230, yymsp[-8].minor.yy262, yymsp[-7].minor.yy0, yymsp[-6].minor.yy327, yymsp[-4].minor.yy88, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_DECREMENTS, DecrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5590 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2550 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy230, yymsp[-6].minor.yy262, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 5596 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2554 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy230, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 5603 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2558 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy230, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 5610 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2562 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy230, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 5617 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2566 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy230, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 5624 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* law_default ::= DEFAULT atomic_head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2570 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, yymsp[-7].minor.yy0, yymsp[-6].minor.yy262, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 5631 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2574 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, yymsp[-7].minor.yy0, yymsp[-6].minor.yy327, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 5638 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2578 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy230, yymsp[-7].minor.yy0, yymsp[-6].minor.yy327, yymsp[-5].minor.yy223, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, 
																																														yymsp[-2].minor.yy262, yymsp[-1].minor.yy223, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 5645 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2582 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy230, yymsp[-5].minor.yy0, yymsp[-4].minor.yy223, yymsp[-3].minor.yy223, yymsp[-2].minor.yy262, yymsp[-1].minor.yy223,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 5651 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 365: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2586 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy230, yymsp[-3].minor.yy0, yymsp[-2].minor.yy327, yymsp[-1].minor.yy223, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 5657 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* law_observed ::= OBSERVED atomic_head_formula AT term_no_const PERIOD */
#line 2591 "bcplus/parser/detail/lemon_parser.y"
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
#line 5676 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* stmt_code_blk ::= ASP_GR */
#line 2625 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 5681 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* stmt_code_blk ::= ASP_CP */
#line 2626 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 5686 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 369: /* stmt_code_blk ::= F2LP_GR */
#line 2627 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 5691 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 370: /* stmt_code_blk ::= F2LP_CP */
#line 2628 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 5696 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 371: /* stmt_code_blk ::= LUA_GR */
#line 2629 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 5701 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 372: /* stmt_code_blk ::= LUA_CP */
#line 2630 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy230, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 5706 "bcplus/parser/detail/lemon_parser.c"
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
#line 5772 "bcplus/parser/detail/lemon_parser.c"
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
