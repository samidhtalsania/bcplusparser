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



#line 1256 "bcplus/parser/detail/lemon_parser.y"

		
	#define DYNAMIC_SORT_PLUS(new_s, s, op, feature, o)																												\
		new_s = NULL;																																				\
		ref_ptr<const Referenced> s_ptr = s, op_ptr = op;																											\
																																									\
																																									\
		if (!parser->lang()->support(feature)) {																													\
			parser->_feature_error(feature, &op->beginLoc());																										\
			YYERROR;																																				\
		} else {																																					\
			std::string name = *s->base() + "__" + *o->base() + "_" + boost::lexical_cast<std::string>(o->arity()) + "__";											\
			new_s = parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name)));																	\
			if (!new_s) {																																			\
				parser->_parse_error("An error occurred creating sort \"" + name + "\".", &op->beginLoc());															\
				YYERROR;																																			\
			} else {																																				\
				new_s->addSubSort(s);																																\
				new_s->add(o);																																		\
			}																																						\
		}																																						
#line 1393 "bcplus/parser/detail/lemon_parser.y"

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
#line 2083 "bcplus/parser/detail/lemon_parser.y"

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

#line 2106 "bcplus/parser/detail/lemon_parser.y"

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
#line 2133 "bcplus/parser/detail/lemon_parser.y"

	struct QueryData {
		QueryStatement::FormulaList* l;
		NumberRange const* maxstep;
		Token const* label;
	};

#line 2236 "bcplus/parser/detail/lemon_parser.y"

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

#line 2308 "bcplus/parser/detail/lemon_parser.y"

	#define CLAUSE(elem, kw, f, feature) 														\
		ref_ptr<const Token> kw_ptr = kw;														\
		elem = f;																				\
		if (!parser->lang()->support(feature)) {												\
			parser->_feature_error(feature, &kw->beginLoc());									\
			YYERROR;																			\
		}
#line 2394 "bcplus/parser/detail/lemon_parser.y"

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
		

#line 2578 "bcplus/parser/detail/lemon_parser.y"

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
#define YYCODETYPE unsigned char
#define YYNOCODE 245
#define YYACTIONTYPE unsigned short int
#define lemon_parserTOKENTYPE  Token const* 								
typedef union {
  int yyinit;
  lemon_parserTOKENTYPE yy0;
  IdentifierDeclList* yy42;
  ConstantDeclaration* yy55;
  ObjectDeclaration::Element* yy70;
  ConstantDeclaration::ElementList* yy97;
  MacroDeclaration::ElementList* yy105;
  VariableDeclaration::ElementList* yy117;
  AtomicFormula* yy138;
  QueryStatement* yy146;
  TokenList* yy152;
  Term* yy163;
  VariableDeclaration* yy171;
  TermList* yy187;
  ObjectDeclaration::ElementList* yy198;
  UNUSED yy209;
  NCStatement* yy210;
  Statement* yy224;
  SortSymbol const* yy228;
  Object* yy238;
  QuantifierFormula::Operator::type yy249;
  Token const* yy251;
  QuantifierFormula::QuantifierList* yy261;
  QuantifierFormula* yy285;
  ObjectDeclaration* yy296;
  QueryData yy301;
  NumberRange* yy309;
  ConstantSymbol::Type::type yy310;
  LuaTerm* yy313;
  MacroSymbol* yy315;
  CardinalityFormula::VariableList* yy319;
  SortDeclaration::ElementList* yy320;
  ObjectDeclaration::Element::ObjectList* yy341;
  Constant* yy345;
  Formula* yy353;
  MacroSymbol::ArgumentList* yy354;
  CardinalityFormula* yy369;
  NumberRange const* yy392;
  SortSymbol* yy393;
  ConstantSymbol::SortList* yy411;
  Number* yy416;
  ShowStatement::ElementList* yy451;
  Variable* yy453;
  MacroDeclaration* yy455;
  SortDeclaration* yy469;
  StrongNCStatement* yy482;
  int yy489;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define lemon_parserARG_SDECL  BCParser* parser						;
#define lemon_parserARG_PDECL , BCParser* parser						
#define lemon_parserARG_FETCH  BCParser* parser						 = yypParser->parser						
#define lemon_parserARG_STORE yypParser->parser						 = parser						
#define YYNSTATE 724
#define YYNRULE 373
#define YYERRORSYMBOL 136
#define YYERRSYMDT yy489
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
 /*     0 */   723,   49,  262,  722,  721,  720,  719,  718,  717,  716,
 /*    10 */   715,  714,  713,  712,  711,  710,  709,  708,  262,  681,
 /*    20 */   271,  707,  700,  706,  705,  699,  110,  108,  106,  104,
 /*    30 */   103,  460,  268,  272,   29,   28,   27,   31,   30,  644,
 /*    40 */   346,   32,  681,  271,  707,  700,  393,  705,  699,  531,
 /*    50 */   527,  702,  724,  196,  642,  267,  272,  461,  698,  460,
 /*    60 */   278,  638,  635,  634,  633,  632,  681,  271,  707,  700,
 /*    70 */   706,  705,  699,  461,   24,  394,  165,  166,  527,  267,
 /*    80 */   272,  111,  372,  525,  294,  638,  635,  634,  633,  632,
 /*    90 */   504,  503,  502,  501,  500,  499,  498,  497,  496,  495,
 /*   100 */   494,  493,  492,  491,  490,  489,  488,  487,  680,  462,
 /*   110 */   371,  525,  463,  679,  472,  471,  468,  467,  470,  469,
 /*   120 */   680,  462,  168,  131,  463,  679,  661,  655,  707,  700,
 /*   130 */   706,  705,  699,  453,  648,   10,  395,  319,  261,   37,
 /*   140 */    54,    8,    9,  701,  162,  697,  214,  440,  660,  462,
 /*   150 */   937,  208,  463,  659,  937,  673,  672,  674,    7,  415,
 /*   160 */   414,    6,  116,  163,  200,  416,  696,  673,  672,  417,
 /*   170 */   640,   39,  641,  111,  251,  111,  273,  150,   53,  114,
 /*   180 */    52,  254,  630,  260,  631,   38,  170,   65,  430,    4,
 /*   190 */    51,   23,  106,  104,  103,  260,  111,  680,  462,  542,
 /*   200 */   292,  463,  679,  681,  342,  707,  700,  706,  705,  699,
 /*   210 */   656,  593,  657,  112,  165,  166,  296,  343,   97,  138,
 /*   220 */   542,  292,  510,  509,   20,   21,  466,  260,  680,  462,
 /*   230 */    97,  245,  463,  679,  546,  381,  541,  662,  465,  698,
 /*   240 */   464,  460,  161,  622,  673,  672,  674,  388,  548,  387,
 /*   250 */   465,  698,  464,  136,  382,  540,  381,  541,  134,  630,
 /*   260 */   527,  631,  465,  170,   64,  442,    4,  140,   35,  135,
 /*   270 */   133,  132,  260,  111,  243,  673,  672,  674,  465,  698,
 /*   280 */   464,  159,  465,  175,  145,    2,  153,  157,  158,  115,
 /*   290 */   630,   63,  631,  528,  170,  198,   62,    4,   50,   23,
 /*   300 */   155,   33,   34,  260,  617,  462,   73,   97,  463,  616,
 /*   310 */    91,   90,   89,   88,   87,  420,  421,  681,  669,  707,
 /*   320 */   700,  706,  705,  699,  379,  534,  378,  465,  698,  464,
 /*   330 */   671,  343,   20,   21,  584,   31,   30,  563,   97,   32,
 /*   340 */   529,   32,  681,  271,  707,  700,  706,  705,  699,  627,
 /*   350 */   626,  610,  609,  611,  174,  264,  272,   26,  465,  698,
 /*   360 */   464,  519,  635,  634,  633,  632,  600,  147,  601,  156,
 /*   370 */    57,  287,   45,  582,  568,   48,  680,  462,   44,   43,
 /*   380 */   463,  679,   45,  695,  703,  704,  707,  700,  706,  705,
 /*   390 */   699,  399,  345,  547,  562,  561,  560,  559,  377,  373,
 /*   400 */   425,  152,  432,  575,  369,  508,  507,  574,   46,   47,
 /*   410 */   558,  550,  556,  149,  118,  588,  589,  555,   96,   95,
 /*   420 */    94,   93,   92,  673,  672,  674,  189,  187,  186,  554,
 /*   430 */   552,  553,  557,  539,  465,  698,  464,  549,  640,  597,
 /*   440 */   641,  147,  542,  292,  192,  150,  629,  114,  391,  585,
 /*   450 */    56,  260,    5,  173,  543,  678,   28,   27,   31,   30,
 /*   460 */   692,  242,   32,   22,   72,   71,   70,  147,   69,   68,
 /*   470 */    67,   66,  570,   29,   28,   27,   31,   30,  190,  544,
 /*   480 */    32,  112,  596,  188,  147,   55,   97,  426,  102,  101,
 /*   490 */   100,   99,   98,  110,  108,  106,  104,  103,  681,  271,
 /*   500 */   707,  700,  393,  705,  699,  453,  465,  698,  464,  694,
 /*   510 */   462,  267,  272,  463,  693,  524,  280,  638,  635,  634,
 /*   520 */   633,  632,   41,   40,   44,   43,  595,  944,   45,  533,
 /*   530 */    25,  394,  681,  271,  707,  700,  706,  705,  699, 1041,
 /*   540 */   513,  594,  944,  944,  156,  264,  272,  583,  387,  167,
 /*   550 */   530,  519,  635,  634,  633,  632,  688,  687,  689, 1041,
 /*   560 */     3,  286,  944,  944,  681,  271,  707,  700,  706,  705,
 /*   570 */   699,  690,  441,  691,   36,  944,  148,  265,  272,  169,
 /*   580 */   109,  944,  276,  638,  635,  634,  633,  632,  681,  270,
 /*   590 */   707,  700,  706,  705,  699,  512,  511,  591,  538,  378,
 /*   600 */   567,  639,  272,  460,  573,  586,  624,  638,  635,  634,
 /*   610 */   633,  632,  460,   11,  107,   15,   14,   18,   17,  105,
 /*   620 */   164,   19,  459,  244,  681,  271,  707,  700,  706,  705,
 /*   630 */   699,  457,  569,  292,  465,  195,  257,  267,  272,  465,
 /*   640 */   698,  464,  637,  638,  635,  634,  633,  632,  681,  271,
 /*   650 */   707,  700,  706,  705,  699,  569,  292,  436,  386,  439,
 /*   660 */   438,  267,  272,  678,  197,  434,  636,  638,  635,  634,
 /*   670 */   633,  632,  681,  271,  707,  700,  706,  705,  699,  433,
 /*   680 */   566,  386,  439,  438,  646,  267,  272,  437,  435,  460,
 /*   690 */   455,  638,  635,  634,  633,  632,  102,  101,  100,   99,
 /*   700 */    98,  681,  271,  707,  700,  706,  705,  699,  458,   61,
 /*   710 */   627,  626,  172,  394,  267,  272,  389,  587,  144,  454,
 /*   720 */   638,  635,  634,  633,  632,  681,  271,  707,  700,  706,
 /*   730 */   705,  699,  146,   29,   28,   27,   31,   30,  267,  272,
 /*   740 */    32,  376,  425,  313,  638,  635,  634,  633,  632,  681,
 /*   750 */   271,  707,  700,  706,  705,  699,   18,   17,  551,   60,
 /*   760 */    19,  429,  267,  272,  375,  425,  143,  357,  638,  635,
 /*   770 */   634,  633,  632,  374,  425,  681,  271,  707,  700,  706,
 /*   780 */   705,  699,  142,   29,   28,   27,   31,   30,  267,  272,
 /*   790 */    32,  349,  425,  356,  638,  635,  634,  633,  632,  681,
 /*   800 */   271,  707,  700,  706,  705,  699,  141,   59,  514,  292,
 /*   810 */   452,  536,  267,  272,  423,  422,  424,  281,  638,  635,
 /*   820 */   634,  633,  632,  681,  271,  707,  700,  706,  705,  699,
 /*   830 */   232,   29,   28,   27,   31,   30,  267,  272,   32,  452,
 /*   840 */   532,  279,  638,  635,  634,  633,  632,  681,  271,  707,
 /*   850 */   700,  706,  705,  699,  526,   58,  523, 1098,    1,  219,
 /*   860 */   267,  272,  658,  262,  262,  277,  638,  635,  634,  633,
 /*   870 */   632,  681,  271,  707,  700,  706,  705,  699,  645,   29,
 /*   880 */    28,   27,   31,   30,  263,  272,   32,  623,  452,  621,
 /*   890 */   519,  635,  634,  633,  632,  139,  137,  135,  133,  132,
 /*   900 */   274,  681,  271,  707,  700,  706,  705,  699,  400,   29,
 /*   910 */    28,   27,   31,   30,  264,  272,   32,  598,  461,  461,
 /*   920 */   519,  635,  634,  633,  632,  139,  137,  135,  133,  132,
 /*   930 */   288,  681,  270,  707,  700,  706,  705,  699,  185,   42,
 /*   940 */    41,   40,   44,   43,  520,  272,   45,  515,  184,   19,
 /*   950 */   519,  635,  634,  633,  632,  193,  191,  189,  187,  186,
 /*   960 */   516,  681,  271,  707,  700,  706,  705,  699,  160,   16,
 /*   970 */    15,   14,   18,   17,  264,  272,   19,   12,  506,  368,
 /*   980 */   519,  635,  634,  633,  632,  505,  486,  485,  484,  483,
 /*   990 */   518,  482,  681,  271,  707,  700,  706,  705,  699,  481,
 /*  1000 */    29,   28,   27,   31,   30,  264,  272,   32,  480,  649,
 /*  1010 */   479,  519,  635,  634,  633,  632,  478,  477,  476,  475,
 /*  1020 */   474,  517,  681,  271,  707,  700,  706,  705,  699,  117,
 /*  1030 */    42,   41,   40,   44,   43,  264,  272,   45,  461,  647,
 /*  1040 */    36,  519,  635,  634,  633,  632,   29,   28,   27,   31,
 /*  1050 */    30,  419,  698,   32,  645,  465,  681,  271,  707,  700,
 /*  1060 */   706,  705,  699,  256,  194,   36,  154,  628,  625,  264,
 /*  1070 */   272,  448,  255,   49,  447,  519,  635,  634,  633,  632,
 /*  1080 */   193,  191,  189,  187,  186,  418,  681,  271,  707,  700,
 /*  1090 */   706,  705,  699,  252,   29,   28,   27,   31,   30,  264,
 /*  1100 */   272,   32,  446,  445,  444,  519,  635,  634,  633,  632,
 /*  1110 */   249,  247,  246,  592,  443,  298,  681,  271,  707,  700,
 /*  1120 */   706,  705,  699,  390,   16,   15,   14,   18,   17,  264,
 /*  1130 */   272,   19,  545,  452,  572,  519,  635,  634,  633,  632,
 /*  1140 */   573,  568,  240,   13,  451,  348,  681,  271,  707,  700,
 /*  1150 */   706,  705,  699,  620,  537,   12,  619,  413,  236,  264,
 /*  1160 */   272,  239,  582,  427,  238,  519,  635,  634,  633,  632,
 /*  1170 */   213,  235,  234,  233,  231,  347,  229,  681,  271,  707,
 /*  1180 */   700,  706,  705,  699,  569,  292,  258,  411,  230,  410,
 /*  1190 */   267,  272,  582,  568,  228,  285,  638,  635,  634,  633,
 /*  1200 */   632,  681,  271,  707,  700,  706,  705,  699,  385,  565,
 /*  1210 */   386,  439,  438,  646,  267,  272,  226,  224,  460,  284,
 /*  1220 */   638,  635,  634,  633,  632,  681,  271,  707,  700,  706,
 /*  1230 */   705,  699,  222,  192,  409,  408,  220,  456,  267,  272,
 /*  1240 */   407,  171,  394,  181,  638,  635,  634,  633,  632,  218,
 /*  1250 */   406,  215,  212,  217,  216,  681,  271,  707,  700,  706,
 /*  1260 */   705,  699,  405,  192,  211,  210,  209,  190,  267,  272,
 /*  1270 */   404,  207,  188,  180,  638,  635,  634,  633,  632,  206,
 /*  1280 */   205,  204,  199,  590,  203,  681,  271,  707,  700,  706,
 /*  1290 */   705,  699,  403,  201,  571,  241,  402,  190,  267,  272,
 /*  1300 */   401,  253,  188,  179,  638,  635,  634,  633,  632,  299,
 /*  1310 */   350,  250,  291,  351,  580,  681,  271,  707,  700,  706,
 /*  1320 */   705,  699,  569,  292,  576,  579,  578,  577,  267,  272,
 /*  1330 */   290,  289,  412,  178,  638,  635,  634,  633,  632,  681,
 /*  1340 */   271,  707,  700,  706,  705,  699,  384,  565,  386,  439,
 /*  1350 */   438,  428,  267,  272,  227,  225,  223,  177,  638,  635,
 /*  1360 */   634,  633,  632,  681,  271,  707,  700,  706,  705,  699,
 /*  1370 */   221,  569,  292,  535, 1099,    5,  267,  272,  678, 1099,
 /*  1380 */  1099,  176,  638,  635,  634,  633,  632,   72,   71,   70,
 /*  1390 */     5,   69,   68,   67,   66,  383,  565,  386,  439,  438,
 /*  1400 */   202,   22,   72,   71,   70,  151,   69,   68,   67,   66,
 /*  1410 */  1099,  102,  101,  100,   99,   98, 1099,   72,   71,   70,
 /*  1420 */  1099,   69,   68,   67,   66, 1099,  102,  101,  100,   99,
 /*  1430 */    98,  680,  462, 1099,    5,  463,  679, 1099, 1099, 1099,
 /*  1440 */  1099,  102,  101,  100,   99,   98,   72,   71,   70,  615,
 /*  1450 */    69,   68,   67,   66, 1099, 1099, 1099, 1099,  125,  124,
 /*  1460 */   123, 1099,  122,  121,  120,  119, 1099, 1099, 1099, 1099,
 /*  1470 */   102,  101,  100,   99,   98, 1099, 1099, 1099,  673,  672,
 /*  1480 */   674, 1099,  130,  129,  128,  127,  126, 1099, 1099, 1099,
 /*  1490 */  1099,  681,  271,  707,  700,  706,  705,  699, 1099, 1099,
 /*  1500 */  1099, 1099,  114, 1099,  268,  272, 1099, 1099, 1099,  581,
 /*  1510 */  1099,  644,  643, 1099,  681,  271,  707,  700,  706,  705,
 /*  1520 */   699, 1099, 1099, 1099, 1099,  259,  642,  268,  272,  569,
 /*  1530 */   292, 1099, 1099, 1099,  644,  643,  113, 1099, 1099,  522,
 /*  1540 */  1099,   97,  193,  191,  189,  187,  186,  521,  248,  642,
 /*  1550 */  1099, 1099, 1099,  380,  565,  386,  439,  438, 1099, 1099,
 /*  1560 */  1099,  465,  698,  464,  681,  271,  707,  700,  706,  705,
 /*  1570 */   699,  193,  191,  189,  187,  186, 1099,  268,  272,  193,
 /*  1580 */   191,  189,  187,  186,  644,  643, 1099,   86,   85,   84,
 /*  1590 */  1099,   83,   82,   81,   80, 1099, 1099, 1099,  237,  642,
 /*  1600 */    73,   79,   78, 1099,   77,   76,   75,   74, 1099, 1099,
 /*  1610 */  1099,   91,   90,   89,   88,   87,  681,  282,  707,  700,
 /*  1620 */   706,  705,  699, 1099,   96,   95,   94,   93,   92,  671,
 /*  1630 */   343,  791,  791,  791, 1099,  791,  791,  791,  791, 1099,
 /*  1640 */  1099,   36, 1099, 1099,  125,  124,  123,  473,  122,  121,
 /*  1650 */   120,  119, 1099, 1099, 1099,  791,  791,  791,  791,  791,
 /*  1660 */  1099,  618,  608,  707,  700,  706,  705,  699,  130,  129,
 /*  1670 */   128,  127,  126, 1099, 1099, 1099,  269, 1099, 1099,  139,
 /*  1680 */   137,  135,  133,  132, 1099, 1099,  293,  605,  602, 1099,
 /*  1690 */   618,  608,  707,  700,  706,  705,  699,  618,  608,  707,
 /*  1700 */   700,  706,  705,  699, 1099,  266,  110,  108,  106,  104,
 /*  1710 */   103, 1099,  607, 1099, 1099,  275,  605,  602, 1099, 1099,
 /*  1720 */  1099, 1099,  599,  605,  602,  618,  608,  707,  700,  706,
 /*  1730 */   705,  699, 1099,  618,  608,  707,  700,  706,  705,  699,
 /*  1740 */   269,  618,  608,  707,  700,  706,  705,  699,  269, 1099,
 /*  1750 */   604,  605,  602, 1099, 1099, 1099,  269, 1099,  603,  605,
 /*  1760 */   602, 1099, 1099, 1099, 1099, 1099,  450,  605,  602,  618,
 /*  1770 */   608,  707,  700,  706,  705,  699,  695,  703,  704,  707,
 /*  1780 */   700,  706,  705,  699,  269,  344, 1099,  618,  608,  707,
 /*  1790 */   700,  706,  705,  699,  449,  605,  602, 1099, 1099, 1099,
 /*  1800 */  1099, 1099,  269, 1099,  618,  608,  707,  700,  706,  705,
 /*  1810 */   699, 1099,  300,  605,  602, 1099,  569,  292, 1099,  269,
 /*  1820 */   618,  608,  707,  700,  706,  705,  699, 1099, 1099,  353,
 /*  1830 */   605,  602, 1099, 1099, 1099,  269, 1099, 1099, 1099, 1099,
 /*  1840 */  1099,  564,  386,  439,  438,  352,  605,  602,  695,  703,
 /*  1850 */   704,  707,  700,  706,  705,  699,  398,  345,  695,  703,
 /*  1860 */   704,  707,  700,  706,  705,  699,  397,  345, 1099, 1099,
 /*  1870 */   695,  703,  704,  707,  700,  706,  705,  699,  396,  345,
 /*  1880 */   695,  703,  704,  707,  700,  706,  705,  699,  392,  345,
 /*  1890 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  366,
 /*  1900 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  297,
 /*  1910 */  1099,  695,  703,  704,  707,  700,  706,  705,  699, 1099,
 /*  1920 */   367,  695,  703,  704,  707,  700,  706,  705,  699, 1099,
 /*  1930 */   686,  695,  703,  704,  707,  700,  706,  705,  699, 1099,
 /*  1940 */   682, 1099,  695,  703,  704,  707,  700,  706,  705,  699,
 /*  1950 */  1099,  685, 1099, 1099, 1099, 1099, 1099,  695,  703,  704,
 /*  1960 */   707,  700,  706,  705,  699, 1099,  684,  695,  703,  704,
 /*  1970 */   707,  700,  706,  705,  699, 1099,  683,  695,  703,  704,
 /*  1980 */   707,  700,  706,  705,  699, 1099,  365, 1099, 1099, 1099,
 /*  1990 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  364,
 /*  2000 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  677,
 /*  2010 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  676,
 /*  2020 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  675,
 /*  2030 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  670,
 /*  2040 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  363,
 /*  2050 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  362,
 /*  2060 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  668,
 /*  2070 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  667,
 /*  2080 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  666,
 /*  2090 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  361,
 /*  2100 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  360,
 /*  2110 */  1099, 1099,  695,  703,  704,  707,  700,  706,  705,  699,
 /*  2120 */  1099,  665,  695,  703,  704,  707,  700,  706,  705,  699,
 /*  2130 */  1099,  664,  695,  703,  704,  707,  700,  706,  705,  699,
 /*  2140 */  1099,  663, 1099, 1099, 1099,  695,  703,  704,  707,  700,
 /*  2150 */   706,  705,  699, 1099,  341,  695,  703,  704,  707,  700,
 /*  2160 */   706,  705,  699, 1099,  340,  695,  703,  704,  707,  700,
 /*  2170 */   706,  705,  699, 1099,  339,  695,  703,  704,  707,  700,
 /*  2180 */   706,  705,  699, 1099,  338,  695,  703,  704,  707,  700,
 /*  2190 */   706,  705,  699, 1099,  337,  695,  703,  704,  707,  700,
 /*  2200 */   706,  705,  699, 1099,  336,  695,  703,  704,  707,  700,
 /*  2210 */   706,  705,  699, 1099,  335,  695,  703,  704,  707,  700,
 /*  2220 */   706,  705,  699, 1099,  334,  695,  703,  704,  707,  700,
 /*  2230 */   706,  705,  699, 1099,  333,  695,  703,  704,  707,  700,
 /*  2240 */   706,  705,  699, 1099,  332,  695,  703,  704,  707,  700,
 /*  2250 */   706,  705,  699, 1099,  331,  695,  703,  704,  707,  700,
 /*  2260 */   706,  705,  699, 1099,  330, 1099, 1099,  695,  703,  704,
 /*  2270 */   707,  700,  706,  705,  699, 1099,  329,  695,  703,  704,
 /*  2280 */   707,  700,  706,  705,  699, 1099,  328,  695,  703,  704,
 /*  2290 */   707,  700,  706,  705,  699, 1099,  327, 1099, 1099, 1099,
 /*  2300 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  326,
 /*  2310 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  325,
 /*  2320 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  324,
 /*  2330 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  323,
 /*  2340 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  322,
 /*  2350 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  321,
 /*  2360 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  317,
 /*  2370 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  316,
 /*  2380 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  315,
 /*  2390 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  314,
 /*  2400 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  312,
 /*  2410 */   695,  703,  704,  707,  700,  706,  705,  699, 1099,  311,
 /*  2420 */  1099, 1099,  695,  703,  704,  707,  700,  706,  705,  699,
 /*  2430 */  1099,  310,  695,  703,  704,  707,  700,  706,  705,  699,
 /*  2440 */  1099,  309,  695,  703,  704,  707,  700,  706,  705,  699,
 /*  2450 */  1099,  308, 1099, 1099, 1099,  695,  703,  704,  707,  700,
 /*  2460 */   706,  705,  699, 1099,  183,  695,  703,  704,  707,  700,
 /*  2470 */   706,  705,  699, 1099,  182,  661,  655,  707,  700,  706,
 /*  2480 */   705,  699, 1099, 1099, 1099, 1099,  320,  661,  655,  707,
 /*  2490 */   700,  706,  705,  699, 1099, 1099, 1099, 1099,  358,  661,
 /*  2500 */   655,  707,  700,  706,  705,  699, 1099, 1099, 1099, 1099,
 /*  2510 */   295,  661,  655,  707,  700,  706,  705,  699, 1099, 1099,
 /*  2520 */  1099, 1099,  359,  661,  655,  707,  700,  706,  705,  699,
 /*  2530 */  1099, 1099, 1099, 1099,  654,  661,  655,  707,  700,  706,
 /*  2540 */   705,  699, 1099, 1099, 1099, 1099,  650,  661,  655,  707,
 /*  2550 */   700,  706,  705,  699, 1099, 1099, 1099, 1099,  653,  661,
 /*  2560 */   655,  707,  700,  706,  705,  699, 1099, 1099, 1099, 1099,
 /*  2570 */   652,  661,  655,  707,  700,  706,  705,  699, 1099, 1099,
 /*  2580 */  1099, 1099,  651,  661,  655,  707,  700,  706,  705,  699,
 /*  2590 */  1099, 1099, 1099, 1099,  318,  661,  655,  707,  700,  706,
 /*  2600 */   705,  699, 1099, 1099, 1099, 1099,  355,  661,  655,  707,
 /*  2610 */   700,  706,  705,  699,  569,  292, 1099, 1099,  354, 1099,
 /*  2620 */   661,  655,  707,  700,  706,  705,  699, 1099, 1099, 1099,
 /*  2630 */  1099,  614,  661,  655,  707,  700,  706,  705,  699,  431,
 /*  2640 */   386,  439,  438,  613,  661,  655,  707,  700,  706,  705,
 /*  2650 */   699, 1099, 1099, 1099, 1099,  612,  661,  655,  707,  700,
 /*  2660 */   706,  705,  699, 1099,  582, 1099,  370,  307,  661,  655,
 /*  2670 */   707,  700,  706,  705,  699, 1099, 1099, 1099, 1099,  306,
 /*  2680 */   661,  655,  707,  700,  706,  705,  699, 1099, 1099, 1099,
 /*  2690 */  1099,  305,  661,  655,  707,  700,  706,  705,  699, 1099,
 /*  2700 */  1099, 1099, 1099,  304,  661,  655,  707,  700,  706,  705,
 /*  2710 */   699, 1099, 1099, 1099, 1099,  303,  661,  655,  707,  700,
 /*  2720 */   706,  705,  699, 1099, 1099, 1099, 1099,  302,  661,  655,
 /*  2730 */   707,  700,  706,  705,  699,  192, 1099,  192, 1099,  301,
 /*  2740 */   661,  655,  707,  700,  706,  705,  699, 1099, 1099, 1099,
 /*  2750 */  1099,  606,  661,  655,  707,  700,  706,  705,  699, 1099,
 /*  2760 */  1099, 1099, 1099,  283, 1099, 1099, 1099, 1099, 1099,  190,
 /*  2770 */  1099,  190, 1099, 1099,  188, 1099,  188,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */   136,   62,   76,  139,  140,  141,  142,  143,  144,  145,
 /*    10 */   146,  147,  148,  149,  150,  151,  152,  153,   76,  155,
 /*    20 */   156,  157,  158,  159,  160,  161,  106,  107,  108,  109,
 /*    30 */   110,  164,  168,  169,   95,   96,   97,   98,   99,  175,
 /*    40 */   176,  102,  155,  156,  157,  158,  159,  160,  161,   73,
 /*    50 */   183,   73,    0,  189,  190,  168,  169,  131,  132,  164,
 /*    60 */   173,  174,  175,  176,  177,  178,  155,  156,  157,  158,
 /*    70 */   159,  160,  161,  131,  187,  188,  100,  101,  183,  168,
 /*    80 */   169,  103,  215,  216,  173,  174,  175,  176,  177,  178,
 /*    90 */   226,  227,  228,  229,  230,  231,  232,  233,  234,  235,
 /*   100 */   236,  237,  238,  239,  240,  241,  242,  243,    1,    2,
 /*   110 */   215,  216,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   120 */     1,    2,   72,   82,    5,    6,  155,  156,  157,  158,
 /*   130 */   159,  160,  161,    2,   99,   28,  165,  166,  103,   32,
 /*   140 */    72,   34,   35,   73,   37,   73,   39,   97,    1,    2,
 /*   150 */    99,   44,    5,    6,  103,   48,   49,   50,   51,   52,
 /*   160 */    53,   54,   77,   56,   57,   46,   73,   48,   49,   50,
 /*   170 */    63,   33,   65,  103,   36,  103,   69,   70,   72,   72,
 /*   180 */    72,   43,   63,   76,   65,   47,   67,   71,  103,   70,
 /*   190 */    72,   72,  108,  109,  110,   76,  103,    1,    2,  171,
 /*   200 */   172,    5,    6,  155,  156,  157,  158,  159,  160,  161,
 /*   210 */    63,   74,   65,  106,  100,  101,  168,  169,  111,   72,
 /*   220 */   171,  172,    1,    2,  105,  106,  119,   76,    1,    2,
 /*   230 */   111,   94,    5,    6,  206,  207,  208,   73,  131,  132,
 /*   240 */   133,  164,  135,   73,   48,   49,   50,  200,  201,  202,
 /*   250 */   131,  132,  133,  106,  205,  206,  207,  208,  111,   63,
 /*   260 */   183,   65,  131,   67,   71,   73,   70,  103,   72,  108,
 /*   270 */   109,  110,   76,  103,   77,   48,   49,   50,  131,  132,
 /*   280 */   133,   14,  131,   16,   17,   18,   19,   20,   21,   72,
 /*   290 */    63,   71,   65,  216,   67,  103,   71,   70,   72,   72,
 /*   300 */   103,  105,  106,   76,    1,    2,   82,  111,    5,    6,
 /*   310 */   106,  107,  108,  109,  110,   48,   49,  155,  156,  157,
 /*   320 */   158,  159,  160,  161,  209,  210,  211,  131,  132,  133,
 /*   330 */   168,  169,  105,  106,   74,   98,   99,   73,  111,  102,
 /*   340 */    74,  102,  155,  156,  157,  158,  159,  160,  161,   91,
 /*   350 */    92,   48,   49,   50,   94,  168,  169,   99,  131,  132,
 /*   360 */   133,  174,  175,  176,  177,  178,   63,  103,   65,  103,
 /*   370 */    82,  184,  102,    1,    2,   72,    1,    2,   98,   99,
 /*   380 */     5,    6,  102,  154,  155,  156,  157,  158,  159,  160,
 /*   390 */   161,  162,  163,   74,   22,   23,   24,   25,  212,  213,
 /*   400 */   214,  134,   30,  108,  217,  218,  219,  112,  105,  106,
 /*   410 */    38,   73,   40,   94,  111,    3,    4,   45,  106,  107,
 /*   420 */   108,  109,  110,   48,   49,   50,  108,  109,  110,   57,
 /*   430 */    58,   59,   60,   74,  131,  132,  133,   73,   63,   74,
 /*   440 */    65,  103,  171,  172,   72,   70,   68,   72,  191,  192,
 /*   450 */    31,   76,   70,   94,   73,   73,   96,   97,   98,   99,
 /*   460 */    73,   77,  102,   81,   82,   83,   84,  103,   86,   87,
 /*   470 */    88,   89,    1,   95,   96,   97,   98,   99,  106,  208,
 /*   480 */   102,  106,   74,  111,  103,   31,  111,  103,  106,  107,
 /*   490 */   108,  109,  110,  106,  107,  108,  109,  110,  155,  156,
 /*   500 */   157,  158,  159,  160,  161,    2,  131,  132,  133,    1,
 /*   510 */     2,  168,  169,    5,    6,   74,  173,  174,  175,  176,
 /*   520 */   177,  178,   96,   97,   98,   99,   74,   26,  102,   74,
 /*   530 */   187,  188,  155,  156,  157,  158,  159,  160,  161,   74,
 /*   540 */    74,   74,   41,   42,  103,  168,  169,  201,  202,   94,
 /*   550 */     2,  174,  175,  176,  177,  178,   48,   49,   50,   94,
 /*   560 */    94,  184,   61,   62,  155,  156,  157,  158,  159,  160,
 /*   570 */   161,   63,   97,   65,   41,   74,   72,  168,  169,   76,
 /*   580 */    72,   80,  173,  174,  175,  176,  177,  178,  155,  156,
 /*   590 */   157,  158,  159,  160,  161,  218,  219,   75,  210,  211,
 /*   600 */    73,  168,  169,  164,  133,   75,  173,  174,  175,  176,
 /*   610 */   177,  178,  164,   80,  106,   96,   97,   98,   99,  111,
 /*   620 */    72,  102,  183,  107,  155,  156,  157,  158,  159,  160,
 /*   630 */   161,  183,  171,  172,  131,  185,  186,  168,  169,  131,
 /*   640 */   132,  133,  173,  174,  175,  176,  177,  178,  155,  156,
 /*   650 */   157,  158,  159,  160,  161,  171,  172,  196,  197,  198,
 /*   660 */   199,  168,  169,   73,  107,   55,  173,  174,  175,  176,
 /*   670 */   177,  178,  155,  156,  157,  158,  159,  160,  161,    2,
 /*   680 */   196,  197,  198,  199,  159,  168,  169,  203,  204,  164,
 /*   690 */   173,  174,  175,  176,  177,  178,  106,  107,  108,  109,
 /*   700 */   110,  155,  156,  157,  158,  159,  160,  161,  183,   71,
 /*   710 */    91,   92,  187,  188,  168,  169,  193,  194,   72,  173,
 /*   720 */   174,  175,  176,  177,  178,  155,  156,  157,  158,  159,
 /*   730 */   160,  161,   72,   95,   96,   97,   98,   99,  168,  169,
 /*   740 */   102,  213,  214,  173,  174,  175,  176,  177,  178,  155,
 /*   750 */   156,  157,  158,  159,  160,  161,   98,   99,   73,   71,
 /*   760 */   102,    2,  168,  169,  213,  214,   72,  173,  174,  175,
 /*   770 */   176,  177,  178,  213,  214,  155,  156,  157,  158,  159,
 /*   780 */   160,  161,   72,   95,   96,   97,   98,   99,  168,  169,
 /*   790 */   102,  213,  214,  173,  174,  175,  176,  177,  178,  155,
 /*   800 */   156,  157,  158,  159,  160,  161,   72,   71,  171,  172,
 /*   810 */   156,    2,  168,  169,   27,   27,  103,  173,  174,  175,
 /*   820 */   176,  177,  178,  155,  156,  157,  158,  159,  160,  161,
 /*   830 */   176,   95,   96,   97,   98,   99,  168,  169,  102,  156,
 /*   840 */     2,  173,  174,  175,  176,  177,  178,  155,  156,  157,
 /*   850 */   158,  159,  160,  161,   74,   71,   74,  137,  138,  176,
 /*   860 */   168,  169,   73,   76,   76,  173,  174,  175,  176,  177,
 /*   870 */   178,  155,  156,  157,  158,  159,  160,  161,  156,   95,
 /*   880 */    96,   97,   98,   99,  168,  169,  102,   73,  156,  167,
 /*   890 */   174,  175,  176,  177,  178,  106,  107,  108,  109,  110,
 /*   900 */   184,  155,  156,  157,  158,  159,  160,  161,  176,   95,
 /*   910 */    96,   97,   98,   99,  168,  169,  102,   73,  131,  131,
 /*   920 */   174,  175,  176,  177,  178,  106,  107,  108,  109,  110,
 /*   930 */   184,  155,  156,  157,  158,  159,  160,  161,   82,   95,
 /*   940 */    96,   97,   98,   99,  168,  169,  102,   73,   82,  102,
 /*   950 */   174,  175,  176,  177,  178,  106,  107,  108,  109,  110,
 /*   960 */   184,  155,  156,  157,  158,  159,  160,  161,   77,   95,
 /*   970 */    96,   97,   98,   99,  168,  169,  102,   26,   74,   77,
 /*   980 */   174,  175,  176,  177,  178,   74,   74,   74,   74,   74,
 /*   990 */   184,   74,  155,  156,  157,  158,  159,  160,  161,   74,
 /*  1000 */    95,   96,   97,   98,   99,  168,  169,  102,   74,  164,
 /*  1010 */    74,  174,  175,  176,  177,  178,   74,   74,   74,   74,
 /*  1020 */    74,  184,  155,  156,  157,  158,  159,  160,  161,   66,
 /*  1030 */    95,   96,   97,   98,   99,  168,  169,  102,  131,  159,
 /*  1040 */    41,  174,  175,  176,  177,  178,   95,   96,   97,   98,
 /*  1050 */    99,  184,  132,  102,  156,  131,  155,  156,  157,  158,
 /*  1060 */   159,  160,  161,  221,   90,   41,   61,  159,  159,  168,
 /*  1070 */   169,  225,  224,   62,  156,  174,  175,  176,  177,  178,
 /*  1080 */   106,  107,  108,  109,  110,  184,  155,  156,  157,  158,
 /*  1090 */   159,  160,  161,  224,   95,   96,   97,   98,   99,  168,
 /*  1100 */   169,  102,  225,  156,  225,  174,  175,  176,  177,  178,
 /*  1110 */   224,  221,  224,  192,  225,  184,  155,  156,  157,  158,
 /*  1120 */   159,  160,  161,    2,   95,   96,   97,   98,   99,  168,
 /*  1130 */   169,  102,  199,  156,  158,  174,  175,  176,  177,  178,
 /*  1140 */   133,    2,  223,   42,  167,  184,  155,  156,  157,  158,
 /*  1150 */   159,  160,  161,  176,  199,   26,  179,  225,  221,  168,
 /*  1160 */   169,  222,    1,    2,  224,  174,  175,  176,  177,  178,
 /*  1170 */   156,  223,  222,  224,  221,  184,  222,  155,  156,  157,
 /*  1180 */   158,  159,  160,  161,  171,  172,  186,  225,  223,  225,
 /*  1190 */   168,  169,    1,    2,  224,  173,  174,  175,  176,  177,
 /*  1200 */   178,  155,  156,  157,  158,  159,  160,  161,  195,  196,
 /*  1210 */   197,  198,  199,  159,  168,  169,  224,  224,  164,  173,
 /*  1220 */   174,  175,  176,  177,  178,  155,  156,  157,  158,  159,
 /*  1230 */   160,  161,  224,   72,  225,  225,  224,  183,  168,  169,
 /*  1240 */   225,  187,  188,  173,  174,  175,  176,  177,  178,  221,
 /*  1250 */   225,  224,  221,  223,  222,  155,  156,  157,  158,  159,
 /*  1260 */   160,  161,  225,   72,  223,  222,  224,  106,  168,  169,
 /*  1270 */   225,  156,  111,  173,  174,  175,  176,  177,  178,  221,
 /*  1280 */   223,  222,  156,  194,  224,  155,  156,  157,  158,  159,
 /*  1290 */   160,  161,  225,  224,  158,  221,  225,  106,  168,  169,
 /*  1300 */   225,  221,  111,  173,  174,  175,  176,  177,  178,  172,
 /*  1310 */   172,  221,  172,  172,  172,  155,  156,  157,  158,  159,
 /*  1320 */   160,  161,  171,  172,  172,  172,  172,  172,  168,  169,
 /*  1330 */   172,  172,  225,  173,  174,  175,  176,  177,  178,  155,
 /*  1340 */   156,  157,  158,  159,  160,  161,  195,  196,  197,  198,
 /*  1350 */   199,    2,  168,  169,  222,  222,  222,  173,  174,  175,
 /*  1360 */   176,  177,  178,  155,  156,  157,  158,  159,  160,  161,
 /*  1370 */   222,  171,  172,    2,  244,   70,  168,  169,   73,  244,
 /*  1380 */   244,  173,  174,  175,  176,  177,  178,   82,   83,   84,
 /*  1390 */    70,   86,   87,   88,   89,  195,  196,  197,  198,  199,
 /*  1400 */   221,   81,   82,   83,   84,   70,   86,   87,   88,   89,
 /*  1410 */   244,  106,  107,  108,  109,  110,  244,   82,   83,   84,
 /*  1420 */   244,   86,   87,   88,   89,  244,  106,  107,  108,  109,
 /*  1430 */   110,    1,    2,  244,   70,    5,    6,  244,  244,  244,
 /*  1440 */   244,  106,  107,  108,  109,  110,   82,   83,   84,   73,
 /*  1450 */    86,   87,   88,   89,  244,  244,  244,  244,   82,   83,
 /*  1460 */    84,  244,   86,   87,   88,   89,  244,  244,  244,  244,
 /*  1470 */   106,  107,  108,  109,  110,  244,  244,  244,   48,   49,
 /*  1480 */    50,  244,  106,  107,  108,  109,  110,  244,  244,  244,
 /*  1490 */   244,  155,  156,  157,  158,  159,  160,  161,  244,  244,
 /*  1500 */   244,  244,   72,  244,  168,  169,  244,  244,  244,   73,
 /*  1510 */   244,  175,  176,  244,  155,  156,  157,  158,  159,  160,
 /*  1520 */   161,  244,  244,  244,  244,  189,  190,  168,  169,  171,
 /*  1530 */   172,  244,  244,  244,  175,  176,  106,  244,  244,   74,
 /*  1540 */   244,  111,  106,  107,  108,  109,  110,   74,  189,  190,
 /*  1550 */   244,  244,  244,  195,  196,  197,  198,  199,  244,  244,
 /*  1560 */   244,  131,  132,  133,  155,  156,  157,  158,  159,  160,
 /*  1570 */   161,  106,  107,  108,  109,  110,  244,  168,  169,  106,
 /*  1580 */   107,  108,  109,  110,  175,  176,  244,   82,   83,   84,
 /*  1590 */   244,   86,   87,   88,   89,  244,  244,  244,  189,  190,
 /*  1600 */    82,   83,   84,  244,   86,   87,   88,   89,  244,  244,
 /*  1610 */   244,  106,  107,  108,  109,  110,  155,  156,  157,  158,
 /*  1620 */   159,  160,  161,  244,  106,  107,  108,  109,  110,  168,
 /*  1630 */   169,   82,   83,   84,  244,   86,   87,   88,   89,  244,
 /*  1640 */   244,   41,  244,  244,   82,   83,   84,   74,   86,   87,
 /*  1650 */    88,   89,  244,  244,  244,  106,  107,  108,  109,  110,
 /*  1660 */   244,  155,  156,  157,  158,  159,  160,  161,  106,  107,
 /*  1670 */   108,  109,  110,  244,  244,  244,  170,  244,  244,  106,
 /*  1680 */   107,  108,  109,  110,  244,  244,  180,  181,  182,  244,
 /*  1690 */   155,  156,  157,  158,  159,  160,  161,  155,  156,  157,
 /*  1700 */   158,  159,  160,  161,  244,  170,  106,  107,  108,  109,
 /*  1710 */   110,  244,  170,  244,  244,  180,  181,  182,  244,  244,
 /*  1720 */   244,  244,  180,  181,  182,  155,  156,  157,  158,  159,
 /*  1730 */   160,  161,  244,  155,  156,  157,  158,  159,  160,  161,
 /*  1740 */   170,  155,  156,  157,  158,  159,  160,  161,  170,  244,
 /*  1750 */   180,  181,  182,  244,  244,  244,  170,  244,  180,  181,
 /*  1760 */   182,  244,  244,  244,  244,  244,  180,  181,  182,  155,
 /*  1770 */   156,  157,  158,  159,  160,  161,  154,  155,  156,  157,
 /*  1780 */   158,  159,  160,  161,  170,  163,  244,  155,  156,  157,
 /*  1790 */   158,  159,  160,  161,  180,  181,  182,  244,  244,  244,
 /*  1800 */   244,  244,  170,  244,  155,  156,  157,  158,  159,  160,
 /*  1810 */   161,  244,  180,  181,  182,  244,  171,  172,  244,  170,
 /*  1820 */   155,  156,  157,  158,  159,  160,  161,  244,  244,  180,
 /*  1830 */   181,  182,  244,  244,  244,  170,  244,  244,  244,  244,
 /*  1840 */   244,  196,  197,  198,  199,  180,  181,  182,  154,  155,
 /*  1850 */   156,  157,  158,  159,  160,  161,  162,  163,  154,  155,
 /*  1860 */   156,  157,  158,  159,  160,  161,  162,  163,  244,  244,
 /*  1870 */   154,  155,  156,  157,  158,  159,  160,  161,  162,  163,
 /*  1880 */   154,  155,  156,  157,  158,  159,  160,  161,  162,  163,
 /*  1890 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  1900 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  1910 */   244,  154,  155,  156,  157,  158,  159,  160,  161,  244,
 /*  1920 */   163,  154,  155,  156,  157,  158,  159,  160,  161,  244,
 /*  1930 */   163,  154,  155,  156,  157,  158,  159,  160,  161,  244,
 /*  1940 */   163,  244,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  1950 */   244,  163,  244,  244,  244,  244,  244,  154,  155,  156,
 /*  1960 */   157,  158,  159,  160,  161,  244,  163,  154,  155,  156,
 /*  1970 */   157,  158,  159,  160,  161,  244,  163,  154,  155,  156,
 /*  1980 */   157,  158,  159,  160,  161,  244,  163,  244,  244,  244,
 /*  1990 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2000 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2010 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2020 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2030 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2040 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2050 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2060 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2070 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2080 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2090 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2100 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2110 */   244,  244,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2120 */   244,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2130 */   244,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2140 */   244,  163,  244,  244,  244,  154,  155,  156,  157,  158,
 /*  2150 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2160 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2170 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2180 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2190 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2200 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2210 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2220 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2230 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2240 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2250 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2260 */   159,  160,  161,  244,  163,  244,  244,  154,  155,  156,
 /*  2270 */   157,  158,  159,  160,  161,  244,  163,  154,  155,  156,
 /*  2280 */   157,  158,  159,  160,  161,  244,  163,  154,  155,  156,
 /*  2290 */   157,  158,  159,  160,  161,  244,  163,  244,  244,  244,
 /*  2300 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2310 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2320 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2330 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2340 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2350 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2360 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2370 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2380 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2390 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2400 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2410 */   154,  155,  156,  157,  158,  159,  160,  161,  244,  163,
 /*  2420 */   244,  244,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2430 */   244,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2440 */   244,  163,  154,  155,  156,  157,  158,  159,  160,  161,
 /*  2450 */   244,  163,  244,  244,  244,  154,  155,  156,  157,  158,
 /*  2460 */   159,  160,  161,  244,  163,  154,  155,  156,  157,  158,
 /*  2470 */   159,  160,  161,  244,  163,  155,  156,  157,  158,  159,
 /*  2480 */   160,  161,  244,  244,  244,  244,  166,  155,  156,  157,
 /*  2490 */   158,  159,  160,  161,  244,  244,  244,  244,  166,  155,
 /*  2500 */   156,  157,  158,  159,  160,  161,  244,  244,  244,  244,
 /*  2510 */   166,  155,  156,  157,  158,  159,  160,  161,  244,  244,
 /*  2520 */   244,  244,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2530 */   244,  244,  244,  244,  166,  155,  156,  157,  158,  159,
 /*  2540 */   160,  161,  244,  244,  244,  244,  166,  155,  156,  157,
 /*  2550 */   158,  159,  160,  161,  244,  244,  244,  244,  166,  155,
 /*  2560 */   156,  157,  158,  159,  160,  161,  244,  244,  244,  244,
 /*  2570 */   166,  155,  156,  157,  158,  159,  160,  161,  244,  244,
 /*  2580 */   244,  244,  166,  155,  156,  157,  158,  159,  160,  161,
 /*  2590 */   244,  244,  244,  244,  166,  155,  156,  157,  158,  159,
 /*  2600 */   160,  161,  244,  244,  244,  244,  166,  155,  156,  157,
 /*  2610 */   158,  159,  160,  161,  171,  172,  244,  244,  166,  244,
 /*  2620 */   155,  156,  157,  158,  159,  160,  161,  244,  244,  244,
 /*  2630 */   244,  166,  155,  156,  157,  158,  159,  160,  161,  196,
 /*  2640 */   197,  198,  199,  166,  155,  156,  157,  158,  159,  160,
 /*  2650 */   161,  244,  244,  244,  244,  166,  155,  156,  157,  158,
 /*  2660 */   159,  160,  161,  244,    1,  244,    1,  166,  155,  156,
 /*  2670 */   157,  158,  159,  160,  161,  244,  244,  244,  244,  166,
 /*  2680 */   155,  156,  157,  158,  159,  160,  161,  244,  244,  244,
 /*  2690 */   244,  166,  155,  156,  157,  158,  159,  160,  161,  244,
 /*  2700 */   244,  244,  244,  166,  155,  156,  157,  158,  159,  160,
 /*  2710 */   161,  244,  244,  244,  244,  166,  155,  156,  157,  158,
 /*  2720 */   159,  160,  161,  244,  244,  244,  244,  166,  155,  156,
 /*  2730 */   157,  158,  159,  160,  161,   72,  244,   72,  244,  166,
 /*  2740 */   155,  156,  157,  158,  159,  160,  161,  244,  244,  244,
 /*  2750 */   244,  166,  155,  156,  157,  158,  159,  160,  161,  244,
 /*  2760 */   244,  244,  244,  166,  244,  244,  244,  244,  244,  106,
 /*  2770 */   244,  106,  244,  244,  111,  244,  111,
};
#define YY_SHIFT_USE_DFLT (-81)
#define YY_SHIFT_COUNT (466)
#define YY_SHIFT_MIN   (-80)
#define YY_SHIFT_MAX   (2665)
static const short yy_shift_ofst[] = {
 /*     0 */   -81,  107,  119,  119,  196,  196,  196,  196,  196,  196,
 /*    10 */   196,  196,  196,  196,  227,  227,  227,  227,  227,  227,
 /*    20 */   227,  227,  227,  227,  196,  196,  196,  196,  196,  196,
 /*    30 */   196,  196,  196,  196,  196,  196,  196,  375,  375,  375,
 /*    40 */   303,  303,  303,  303,  303,  303,  303,  303,  303,  303,
 /*    50 */   508,  508,  508,  508,  508,  508,  508,  508,  508,  508,
 /*    60 */   508,  508,  508,  508,  508,  508,  508,  508,  508,  508,
 /*    70 */   508,  508,  508,  508,  508,  508,  508,  508,  508,  508,
 /*    80 */   508,  508,  508,  508,  508,  508,  508,  508,  508,  508,
 /*    90 */   508,  508,  508,  508,  508,  508,  508,  508,  508,  508,
 /*   100 */   508,  508,  508,  508,  508,  508,  508,  508,  508,  508,
 /*   110 */   508,  508, 1430, 1430, 1430,  147,  372,  147,  147,  147,
 /*   120 */   147,  147,  147,  147,  147,  147,  147,  147,  147,  147,
 /*   130 */   147,  147,  147,  147,  147,  147,  147,  147,  147,  147,
 /*   140 */   147, 1191, 1191, 1191, 1191, 1161, 1191, 1191, 1191, 1161,
 /*   150 */   -74,  -74,  788,  787,  503, 1161,  -58,  548, 1371, 1349,
 /*   160 */  2665,  151,  151,  151,  548,  548,  548,  548,  412,  131,
 /*   170 */   619,  -58,  -58, 1371, 1349, 1121,  999,  951,  951,  951,
 /*   180 */   951,  -61, 1600, 1600, 2663, 2663, 2663, 2663, 2663, 2663,
 /*   190 */  2663, 2663, 2663, 2663, 2663,  258,  533,  471,  412, 1011,
 /*   200 */   924, 1011, 1005, 1011, 1005, 1129, 1101, 1024,  924, 1011,
 /*   210 */  1005, 1129, 1101, 1024,  924, 1011, 1005, 1129, 1101, 1024,
 /*   220 */  1011, 1005, 1011, 1005, 1011, 1005, 1011, 1005, 1011, 1005,
 /*   230 */  1129, 1101, 1024, 1011, 1005, 1129, 1101, 1024, 1011, 1005,
 /*   240 */  1129, 1101, 1139, 1139, 1007, 1121, 1011, 1005, 1024, 1011,
 /*   250 */  1005,  924, 1011, 1005,  924, 1011, 1005,  920,  920, 1024,
 /*   260 */   924,  920,  907,  382, 1320, 1305, 1376, 1364, 1335, 1562,
 /*   270 */  1549, 1518, 1505,  267,  874,  844,  814,  784,  736,  688,
 /*   280 */   638,  378,  501, 1573,  905,  905, 1029, 1029, 1029, 1473,
 /*   290 */  1465, 1436,  974,  935,  905,  789,  590,  387,  519,  849,
 /*   300 */   426,  819,  819,  819,  819,  819,  819,  819,  -80,  -80,
 /*   310 */   -80,  -80,  -80,  360,  -80,  -80,  -80,  -80,  819,  819,
 /*   320 */   819,  -80,  -80,  -80,  -80,  -80,  -80,  -80,  -80,  -80,
 /*   330 */   -80,  -80,  -80,  -80,  -80,  -80,  -80,  -80,  -80,  -80,
 /*   340 */   -80,  -80,  312,  204,  -80,  -80,  138,  658,  658,  -24,
 /*   350 */   318,  318,  280,  280,  161,  161,  237,  237,  161,  161,
 /*   360 */    84,   84,   84,   84,   84,   84,   84,   84,  221,  466,
 /*   370 */   465,  441,  266,  114,  114,  114,  114,  455,  384,  359,
 /*   380 */   381,  197,  319,  364,  338,  264,  295,   85,  260,  192,
 /*   390 */    50,  137,  170,   51,   35,  164,   93,   72,   70,  -22,
 /*   400 */   963,  946,  945,  944,  943,  942,  936,  934,  925,  917,
 /*   410 */   915,  914,  913,  912,  911,  904,  902,  891,  847,  847,
 /*   420 */   866,  856,  782,  780,  838,  713,  809,  734,  710,  694,
 /*   430 */   759,  685,  660,  646,  677,  610,  527,  504,  557,  516,
 /*   440 */   530,  522,  475,  467,  452,  454,  408,  419,  365,  270,
 /*   450 */   270,  288,  224,  226,  239,  239,  225,  220,  193,  116,
 /*   460 */    41,  217,  118,  108,  106,   68,   52,
};
#define YY_REDUCE_USE_DFLT (-137)
#define YY_REDUCE_COUNT (262)
#define YY_REDUCE_MIN   (-136)
#define YY_REDUCE_MAX   (2597)
static const short yy_reduce_ofst[] = {
 /*     0 */   720, -136,  187,  377,  343, -113, 1208, 1184, 1160, 1130,
 /*    10 */  1100, 1070, 1046, 1022,  991,  961,  931,  901,  867,  837,
 /*    20 */   806,  776,  746,  716,  692,  668,  644,  620,  594,  570,
 /*    30 */   546,  517,  493,  469,  433,  409,  -89, 1409, 1359, 1336,
 /*    40 */  1665, 1649, 1632, 1614, 1586, 1578, 1570, 1542, 1535, 1506,
 /*    50 */  1726, 1716, 1704, 1694,  229, 2311, 2301, 2288, 2278, 2268,
 /*    60 */  2256, 2246, 2236, 2226, 2216, 2206, 2196, 2186, 2176, 2166,
 /*    70 */  2156, 2146, 2133, 2123, 2113, 2101, 2091, 2081, 2071, 2061,
 /*    80 */  2051, 2041, 2031, 2021, 2011, 2001, 1991, 1978, 1968, 1958,
 /*    90 */  1946, 1936, 1926, 1916, 1906, 1896, 1886, 1876, 1866, 1856,
 /*   100 */  1846, 1836, 1823, 1813, 1803, 1788, 1777, 1767, 1757, 1746,
 /*   110 */  1736, 1622, 1461,  162,   48,  -29,  484, 2597, 2585, 2573,
 /*   120 */  2561, 2549, 2537, 2525, 2513, 2501, 2489, 2477, 2465, 2452,
 /*   130 */  2440, 2428, 2416, 2404, 2392, 2380, 2368, 2356, 2344, 2332,
 /*   140 */  2320, 1358, 1200, 1151, 1013,   49, 2443, 1645,  461,   28,
 /*   150 */  1054,  525, -105, -133,  977,  271,   77,  186,  115,   47,
 /*   160 */   637,  732,  683,  654,  578,  560,  551,  528,  523,  722,
 /*   170 */   450,  448,  439,  388,  346,  257, 1179, 1148, 1134, 1133,
 /*   180 */  1132, 1107, 1090, 1080, 1159, 1158, 1155, 1154, 1153, 1152,
 /*   190 */  1142, 1141, 1140, 1138, 1137, 1000, 1074, 1136, 1089, 1075,
 /*   200 */  1126, 1071, 1069, 1067, 1060, 1059, 1057, 1058, 1115, 1045,
 /*   210 */  1042, 1043, 1041, 1031, 1014, 1037, 1027, 1032, 1030, 1028,
 /*   220 */  1025, 1012, 1015, 1008, 1010,  993, 1009,  992,  964,  970,
 /*   230 */   954,  965,  953,  962,  949,  950,  948,  937,  932,  940,
 /*   240 */   939,  919,  955,  933,  976,  921,  889,  888,  890,  879,
 /*   250 */   886,  947,  877,  869,  918,  846,  848,  909,  908,  842,
 /*   260 */   898,  880,  845,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   725, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*    10 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*    20 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*    30 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*    40 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*    50 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,  929,  933,
 /*    60 */   928,  932,  949,  945,  950,  946, 1097, 1097, 1097, 1097,
 /*    70 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*    80 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*    90 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   100 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   110 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   120 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   130 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   140 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   150 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   160 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   170 */  1097, 1097, 1097, 1097, 1097, 1097, 1046, 1048, 1048, 1048,
 /*   180 */  1048, 1054, 1046, 1046, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   190 */  1097, 1097, 1097, 1097, 1097, 1097, 1046, 1097, 1097, 1054,
 /*   200 */  1097, 1054, 1052, 1054, 1052, 1048, 1050, 1046, 1097, 1054,
 /*   210 */  1052, 1048, 1050, 1046, 1097, 1054, 1052, 1048, 1050, 1046,
 /*   220 */  1054, 1052, 1054, 1052, 1054, 1052, 1054, 1052, 1054, 1052,
 /*   230 */  1048, 1050, 1046, 1054, 1052, 1048, 1050, 1046, 1054, 1052,
 /*   240 */  1048, 1050, 1097, 1097, 1097, 1097, 1054, 1052, 1046, 1054,
 /*   250 */  1052, 1097, 1054, 1052, 1097, 1054, 1052, 1097, 1097, 1046,
 /*   260 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   270 */   882,  882, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   280 */  1097, 1097,  791, 1097, 1047, 1049, 1038, 1035,  922, 1097,
 /*   290 */  1097, 1097, 1097, 1053, 1045, 1097, 1097, 1097,  919,  836,
 /*   300 */   896,  908,  907,  906,  905,  904,  903,  902,  888,  931,
 /*   310 */   935,  930,  934,  853,  951,  947,  952,  948,  911,  764,
 /*   320 */   765,  868,  867,  866,  865,  864,  863,  862,  884,  881,
 /*   330 */   880,  879,  878,  877,  876,  875,  874,  873,  872,  871,
 /*   340 */   870,  869, 1097, 1097,  761,  760,  940,  921,  920, 1097,
 /*   350 */   841,  842,  898,  897,  818,  817,  855,  854,  831,  832,
 /*   360 */   793,  792,  798,  797,  803,  802,  777,  778, 1097, 1097,
 /*   370 */   837, 1097, 1097, 1015, 1019, 1018, 1016, 1097, 1097, 1097,
 /*   380 */  1097, 1097, 1097, 1097, 1097, 1097,  964, 1097, 1097, 1097,
 /*   390 */  1097, 1097, 1097,  745, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   400 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,
 /*   410 */  1097, 1097, 1097, 1097, 1097, 1097, 1097,  786,  918,  917,
 /*   420 */  1097, 1097, 1097, 1097, 1097, 1017, 1097, 1005,  982,  984,
 /*   430 */  1097, 1097,  997,  980, 1097, 1097, 1097,  979,  970,  971,
 /*   440 */  1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1097,  895,
 /*   450 */   894,  886,  882,  749,  852,  851, 1097, 1097, 1097, 1097,
 /*   460 */   909,  763,  759,  757,  753,  748, 1097, 1096, 1095, 1094,
 /*   470 */  1093, 1092, 1091, 1090, 1089, 1088, 1087, 1086, 1085, 1084,
 /*   480 */  1083, 1082, 1081, 1075, 1074, 1076, 1073, 1072, 1071, 1070,
 /*   490 */  1069, 1068, 1067, 1066, 1065, 1064, 1063, 1062, 1061, 1060,
 /*   500 */  1059, 1058, 1057, 1056, 1055, 1031, 1030, 1037, 1036, 1044,
 /*   510 */  1043, 1040, 1039, 1034, 1042,  913,  915,  916,  914,  912,
 /*   520 */   789, 1033, 1032, 1026, 1025, 1027, 1024, 1029, 1028, 1023,
 /*   530 */  1021, 1020, 1022, 1014, 1009, 1012, 1013, 1011, 1010, 1008,
 /*   540 */  1000, 1003, 1007, 1006, 1004, 1002, 1001,  999,  975,  983,
 /*   550 */   985,  998,  996,  995,  994,  993,  992,  991,  990,  989,
 /*   560 */   988,  987,  986,  981,  963,  962,  978,  977,  973,  972,
 /*   570 */   969,  968,  967,  753,  966,  965,  843,  845,  844,  840,
 /*   580 */   839,  838,  837,  976,  974,  954,  957,  958,  961,  960,
 /*   590 */   959,  956,  955,  953, 1080, 1079, 1078, 1077,  890,  892,
 /*   600 */   901,  900,  899,  893,  891,  889,  816,  815,  814,  813,
 /*   610 */   812,  811,  821,  820,  819,  810,  809,  808,  807, 1051,
 /*   620 */   885,  887,  750,  847,  849,  924,  927,  926,  925,  923,
 /*   630 */   861,  860,  859,  858,  857,  856,  850,  848,  846,  789,
 /*   640 */   943,  942,  941,  940,  939,  883,  937,  938,  936,  910,
 /*   650 */   833,  835,  834,  830,  829,  828,  827,  826,  825,  824,
 /*   660 */   823,  822,  762,  796,  795,  794,  801,  800,  799,  791,
 /*   670 */   790,  789,  788,  787,  786,  806,  805,  804,  785,  784,
 /*   680 */   783,  782,  779,  781,  780,  776,  775,  774,  773,  772,
 /*   690 */   771,  770,  769,  768,  767,  766,  758,  756,  755,  754,
 /*   700 */   752,  751,  747,  743,  742,  746,  745,  744,  741,  740,
 /*   710 */   739,  738,  737,  736,  735,  734,  733,  732,  731,  730,
 /*   720 */   729,  728,  727,  726,
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
  "card_var_lst_inner",  "head_formula",  "formula_smpl_card",  "macro_def_lst",
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
 /* 216 */ "head_formula ::= atomic_formula",
 /* 217 */ "head_formula ::= formula_smpl_card",
 /* 218 */ "head_formula ::= TRUE",
 /* 219 */ "head_formula ::= FALSE",
 /* 220 */ "head_formula ::= DASH constant",
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
 /* 243 */ "sort ::= sort_nr PLUS object_nullary",
 /* 244 */ "sort ::= sort_id PLUS object_nullary",
 /* 245 */ "sort ::= sort_id PLUS INTEGER",
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
 /* 351 */ "law_pcaused ::= POSSIBLY_CAUSED atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 352 */ "law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD",
 /* 353 */ "law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD",
 /* 354 */ "law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 355 */ "law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD",
 /* 356 */ "law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD",
 /* 357 */ "law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD",
 /* 358 */ "law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD",
 /* 359 */ "law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD",
 /* 360 */ "law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD",
 /* 361 */ "law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 362 */ "law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 363 */ "law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD",
 /* 364 */ "law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD",
 /* 365 */ "law_rigid ::= RIGID constant clause_where PERIOD",
 /* 366 */ "law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD",
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
#line 2344 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 137: /* start */
    case 138: /* statement_lst */
    case 161: /* undeclared */
{
#line 208 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2353 "bcplus/parser/detail/lemon_parser.c"
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
 DEALLOC((yypminor->yy224));								
#line 2366 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 140: /* stmt_macro_def */
{
#line 233 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy455));								
#line 2373 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 141: /* stmt_constant_def */
{
#line 235 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy55));								
#line 2380 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 142: /* stmt_object_def */
{
#line 237 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy296));								
#line 2387 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 143: /* stmt_variable_def */
{
#line 239 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy171));								
#line 2394 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 144: /* stmt_sort_def */
{
#line 241 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy469));								
#line 2401 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 149: /* stmt_noconcurrency */
{
#line 251 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy210));								
#line 2408 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 150: /* stmt_strong_noconcurrency */
{
#line 253 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy482));								
#line 2415 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 153: /* stmt_query */
{
#line 259 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy146));								
#line 2422 "bcplus/parser/detail/lemon_parser.c"
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
 DEALLOC((yypminor->yy163));								
#line 2435 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 156: /* constant */
    case 164: /* constant_one_const */
    case 167: /* const_anon */
{
#line 297 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy345));								
#line 2444 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 157: /* object */
    case 158: /* object_nullary */
{
#line 299 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy238));								
#line 2452 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 159: /* variable */
{
#line 303 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy453));								
#line 2459 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 160: /* lua */
{
#line 305 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy313));								
#line 2466 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 162: /* term_lst */
    case 165: /* term_no_const_lst */
{
#line 309 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy187));								
#line 2474 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 171: /* num_range */
{
#line 707 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy309));								
#line 2481 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 172: /* term_numeric */
{
#line 709 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy416));								
#line 2488 "bcplus/parser/detail/lemon_parser.c"
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
 DEALLOC((yypminor->yy353));								
#line 2503 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 176: /* atomic_formula */
    case 179: /* atomic_formula_anon */
    case 183: /* atomic_formula_one_const */
{
#line 776 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy138));								
#line 2512 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 177: /* formula_quant */
{
#line 778 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy285));								
#line 2519 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 185: /* quant_lst */
{
#line 979 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy261));								
#line 2526 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 186: /* quant_op */
{
#line 981 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */			
#line 2533 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 187: /* card_var_lst */
    case 188: /* card_var_lst_inner */
{
#line 1018 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy319));								
#line 2541 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 190: /* formula_smpl_card */
{
#line 1094 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy369));								
#line 2548 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 191: /* macro_def_lst */
{
#line 1138 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy105));                              
#line 2555 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 192: /* macro_bnd */
{
#line 1140 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy315));                              
#line 2562 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 193: /* macro_args */
{
#line 1142 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy354));                              
#line 2569 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 194: /* macro_arg */
{
#line 1144 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy251));                              
#line 2576 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 195: /* sort_lst */
{
#line 1234 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy411));							
#line 2583 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 196: /* sort */
    case 197: /* sort_id_nr */
    case 198: /* sort_nr */
    case 199: /* sort_id */
{
#line 1236 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */		
#line 2593 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 200: /* constant_bnd_lst */
    case 201: /* constant_bnd */
{
#line 1354 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy97));									
#line 2601 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 202: /* constant_dcl_lst */
{
#line 1358 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy42));									
#line 2608 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 203: /* constant_dcl_type */
{
#line 1360 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2615 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 204: /* attrib_spec */
{
#line 1362 "bcplus/parser/detail/lemon_parser.y"
 /* Intentionally left blank */				
#line 2622 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 205: /* object_bnd_lst */
{
#line 1701 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy198));									
#line 2629 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 206: /* object_bnd */
{
#line 1703 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy70));									
#line 2636 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 207: /* object_lst */
    case 208: /* object_spec */
{
#line 1705 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy341));									
#line 2644 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 209: /* variable_bnd_lst */
    case 210: /* variable_bnd */
{
#line 1815 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy117));									
#line 2652 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 211: /* variable_lst */
{
#line 1819 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy152));									
#line 2659 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 212: /* sort_bnd_lst */
    case 213: /* sort_bnd */
    case 214: /* sort_dcl_lst */
{
#line 1892 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy320));									
#line 2668 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 215: /* show_lst */
{
#line 1996 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy451));									
#line 2675 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 216: /* show_elem */
    case 224: /* clause_unless */
{
#line 1998 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy138));									
#line 2683 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 217: /* query_lst */
{
#line 2143 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy301).l); DEALLOC((yypminor->yy301).maxstep); DEALLOC((yypminor->yy301).label);	
#line 2690 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 218: /* query_maxstep_decl */
{
#line 2145 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy392));												
#line 2697 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 220: /* query_label_Decl */
{
#line 2147 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy0));												
#line 2704 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 221: /* clause_if */
    case 222: /* clause_after */
    case 223: /* clause_ifcons */
    case 225: /* clause_where */
{
#line 2297 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy353));									
#line 2714 "bcplus/parser/detail/lemon_parser.c"
}
      break;
    case 226: /* law_basic */
    case 227: /* law_caused */
    case 228: /* law_pcaused */
    case 229: /* law_impl */
    case 230: /* law_causes */
    case 231: /* law_increments */
    case 232: /* law_decrements */
    case 233: /* law_mcause */
    case 234: /* law_always */
    case 235: /* law_constraint */
    case 236: /* law_impossible */
    case 237: /* law_never */
    case 238: /* law_default */
    case 239: /* law_exogenous */
    case 240: /* law_inertial */
    case 241: /* law_nonexecutable */
    case 242: /* law_rigid */
    case 243: /* law_observed */
{
#line 2338 "bcplus/parser/detail/lemon_parser.y"
 DEALLOC((yypminor->yy224));									
#line 2738 "bcplus/parser/detail/lemon_parser.c"
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
  { 189, 2 },
  { 190, 4 },
  { 190, 5 },
  { 190, 5 },
  { 190, 6 },
  { 190, 3 },
  { 190, 4 },
  { 190, 4 },
  { 190, 5 },
  { 140, 4 },
  { 191, 1 },
  { 191, 3 },
  { 192, 6 },
  { 192, 3 },
  { 193, 1 },
  { 193, 3 },
  { 194, 1 },
  { 194, 1 },
  { 195, 1 },
  { 195, 3 },
  { 196, 1 },
  { 196, 2 },
  { 196, 2 },
  { 196, 3 },
  { 196, 3 },
  { 196, 3 },
  { 197, 1 },
  { 197, 1 },
  { 198, 1 },
  { 199, 1 },
  { 141, 4 },
  { 200, 1 },
  { 200, 3 },
  { 201, 6 },
  { 201, 3 },
  { 201, 3 },
  { 201, 5 },
  { 201, 8 },
  { 202, 1 },
  { 202, 4 },
  { 202, 3 },
  { 202, 6 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 203, 1 },
  { 204, 1 },
  { 204, 4 },
  { 142, 4 },
  { 205, 1 },
  { 205, 3 },
  { 206, 3 },
  { 207, 1 },
  { 207, 3 },
  { 208, 1 },
  { 208, 4 },
  { 208, 1 },
  { 143, 4 },
  { 209, 1 },
  { 209, 3 },
  { 210, 3 },
  { 211, 1 },
  { 211, 3 },
  { 144, 4 },
  { 212, 1 },
  { 212, 3 },
  { 213, 1 },
  { 213, 3 },
  { 213, 3 },
  { 213, 3 },
  { 214, 1 },
  { 214, 3 },
  { 147, 4 },
  { 147, 4 },
  { 148, 4 },
  { 148, 4 },
  { 215, 1 },
  { 215, 3 },
  { 216, 1 },
  { 149, 2 },
  { 150, 2 },
  { 151, 5 },
  { 152, 5 },
  { 153, 4 },
  { 217, 1 },
  { 217, 1 },
  { 217, 1 },
  { 217, 3 },
  { 217, 3 },
  { 217, 3 },
  { 218, 3 },
  { 218, 3 },
  { 219, 3 },
  { 219, 3 },
  { 221, 2 },
  { 221, 0 },
  { 222, 2 },
  { 222, 0 },
  { 223, 2 },
  { 223, 0 },
  { 224, 2 },
  { 224, 0 },
  { 225, 2 },
  { 225, 0 },
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
  { 226, 7 },
  { 227, 8 },
  { 228, 8 },
  { 229, 5 },
  { 230, 7 },
  { 231, 9 },
  { 232, 9 },
  { 233, 7 },
  { 234, 6 },
  { 235, 6 },
  { 236, 6 },
  { 237, 6 },
  { 238, 8 },
  { 239, 8 },
  { 240, 8 },
  { 241, 6 },
  { 242, 4 },
  { 243, 5 },
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
#line 3416 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 3: /* statement_lst ::= statement_lst statement */
#line 219 "bcplus/parser/detail/lemon_parser.y"
{
			ref_ptr<Statement> ptr = yymsp[0].minor.yy224;
			yymsp[0].minor.yy224  = NULL;
			parser->_handle_stmt(ptr);
		}
#line 3425 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 4: /* statement ::= stmt_macro_def */
#line 262 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy455; }
#line 3430 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 5: /* statement ::= stmt_constant_def */
#line 263 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy55; }
#line 3435 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 6: /* statement ::= stmt_object_def */
#line 264 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy296; }
#line 3440 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 7: /* statement ::= stmt_variable_def */
#line 265 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy171; }
#line 3445 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 8: /* statement ::= stmt_sort_def */
#line 266 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy469; }
#line 3450 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 9: /* statement ::= stmt_code_blk */
      case 10: /* statement ::= stmt_law */ yytestcase(yyruleno==10);
      case 11: /* statement ::= stmt_show */ yytestcase(yyruleno==11);
      case 12: /* statement ::= stmt_hide */ yytestcase(yyruleno==12);
      case 15: /* statement ::= stmt_maxafvalue */ yytestcase(yyruleno==15);
      case 16: /* statement ::= stmt_maxadditive */ yytestcase(yyruleno==16);
#line 267 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy224; }
#line 3460 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 13: /* statement ::= stmt_noconcurrency */
#line 271 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy210; }
#line 3465 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 14: /* statement ::= stmt_strong_noconcurrency */
#line 272 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy482; }
#line 3470 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 17: /* statement ::= stmt_query */
#line 275 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy224 = yymsp[0].minor.yy146; }
#line 3475 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 18: /* base_elem ::= constant */
#line 321 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy163 = yymsp[0].minor.yy345; }
#line 3480 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 19: /* base_elem ::= base_elem_no_const */
      case 42: /* term ::= base_elem */ yytestcase(yyruleno==42);
      case 58: /* term_strong ::= base_elem_no_const */ yytestcase(yyruleno==58);
      case 83: /* term_no_const_strong ::= base_elem_no_const */ yytestcase(yyruleno==83);
      case 98: /* term_no_const ::= base_elem_no_const */ yytestcase(yyruleno==98);
#line 322 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy163 = yymsp[0].minor.yy163; }
#line 3489 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 20: /* base_elem_no_const ::= object */
#line 324 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy163 = yymsp[0].minor.yy238;	}
#line 3494 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 21: /* base_elem_no_const ::= variable */
#line 325 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy163 = yymsp[0].minor.yy453; }
#line 3499 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 22: /* base_elem_no_const ::= lua */
#line 326 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy163 = yymsp[0].minor.yy313; }
#line 3504 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 23: /* constant ::= CONSTANT_ID PAREN_L term_lst PAREN_R */
      case 38: /* constant_one_const ::= CONSTANT_ID PAREN_L term_no_const_lst PAREN_R */ yytestcase(yyruleno==38);
#line 443 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy345, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy187, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol);	}
#line 3510 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 24: /* constant ::= CONSTANT_ID */
      case 39: /* constant_one_const ::= CONSTANT_ID */ yytestcase(yyruleno==39);
#line 444 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy345, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol); }
#line 3516 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 25: /* const_anon ::= IDENTIFIER */
#line 446 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy345, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true); }
#line 3521 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 26: /* const_anon ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 447 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF9(yygotominor.yy345, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy187, yymsp[0].minor.yy0, Symbol::Type::CONSTANT, Constant, ConstantSymbol, true);	}
#line 3526 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 27: /* object ::= OBJECT_ID PAREN_L term_lst PAREN_R */
#line 450 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy238, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy187, yymsp[0].minor.yy0, Symbol::Type::OBJECT, Object, ObjectSymbol);	}
#line 3531 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 28: /* object ::= object_nullary */
#line 451 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy238 = yymsp[0].minor.yy238; }
#line 3536 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 29: /* object_nullary ::= OBJECT_ID */
#line 452 "bcplus/parser/detail/lemon_parser.y"
{ BASE_ELEM_DEF(yygotominor.yy238, yymsp[0].minor.yy0, NULL, NULL, NULL, Symbol::Type::OBJECT, Object, ObjectSymbol); }
#line 3541 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 30: /* object ::= undeclared */
#line 453 "bcplus/parser/detail/lemon_parser.y"
{ /* never called */ }
#line 3546 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 31: /* variable ::= VARIABLE_ID */
#line 456 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy453 = NULL;
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::VARIABLE)) {
			parser->_feature_error(Language::Feature::VARIABLE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			BASE_ELEM_BARE_DEF(yygotominor.yy453, yymsp[0].minor.yy0, Symbol::Type::VARIABLE, Variable, VariableSymbol); 
		}
	}
#line 3561 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 32: /* lua ::= AT_IDENTIFIER PAREN_L term_lst PAREN_R */
#line 467 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy313, yymsp[-3].minor.yy0, yymsp[-2].minor.yy0, yymsp[-1].minor.yy187, yymsp[0].minor.yy0); }
#line 3566 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 33: /* lua ::= AT_IDENTIFIER */
#line 468 "bcplus/parser/detail/lemon_parser.y"
{ BASE_LUA_ELEM(yygotominor.yy313, yymsp[0].minor.yy0, NULL, NULL, NULL); }
#line 3571 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 34: /* undeclared ::= IDENTIFIER PAREN_L term_lst PAREN_R */
#line 469 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy209, yymsp[-3].minor.yy0, yymsp[-1].minor.yy187);   yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3578 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 35: /* undeclared ::= IDENTIFIER */
#line 470 "bcplus/parser/detail/lemon_parser.y"
{ UNDECLARED(yygotominor.yy209, yymsp[0].minor.yy0, NULL); }
#line 3583 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 36: /* term_lst ::= term */
      case 40: /* term_no_const_lst ::= term_no_const */ yytestcase(yyruleno==40);
#line 473 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy187 = new TermList();
			yygotominor.yy187->push_back(yymsp[0].minor.yy163);
		}
#line 3592 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 37: /* term_lst ::= term_lst COMMA term */
      case 41: /* term_no_const_lst ::= term_no_const_lst COMMA term_no_const */ yytestcase(yyruleno==41);
#line 479 "bcplus/parser/detail/lemon_parser.y"
{
			yygotominor.yy187 = yymsp[-2].minor.yy187;
			yymsp[-2].minor.yy187->push_back(yymsp[0].minor.yy163);
		  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 3602 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 43: /* term ::= INTEGER */
      case 59: /* term_strong ::= INTEGER */ yytestcase(yyruleno==59);
      case 84: /* term_no_const_strong ::= INTEGER */ yytestcase(yyruleno==84);
      case 99: /* term_no_const ::= INTEGER */ yytestcase(yyruleno==99);
#line 578 "bcplus/parser/detail/lemon_parser.y"
{ BASIC_TERM(yygotominor.yy163, yymsp[0].minor.yy0);	}
#line 3610 "bcplus/parser/detail/lemon_parser.c"
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
{ BASIC_TERM(yygotominor.yy163, yymsp[0].minor.yy0); }
#line 3622 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 45: /* term ::= PAREN_L term PAREN_R */
      case 61: /* term_strong ::= PAREN_L term_strong PAREN_R */ yytestcase(yyruleno==61);
      case 86: /* term_no_const_strong ::= PAREN_L term_no_const_strong PAREN_R */ yytestcase(yyruleno==86);
      case 101: /* term_no_const ::= PAREN_L term_no_const PAREN_R */ yytestcase(yyruleno==101);
#line 580 "bcplus/parser/detail/lemon_parser.y"
{ TERM_PARENS(yygotominor.yy163, yymsp[-2].minor.yy0, yymsp[-1].minor.yy163, yymsp[0].minor.yy0); }
#line 3630 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 48: /* term ::= MAXSTEP */
      case 62: /* term_strong ::= MAXSTEP */ yytestcase(yyruleno==62);
      case 87: /* term_no_const_strong ::= MAXSTEP */ yytestcase(yyruleno==87);
#line 583 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy163, yymsp[0].minor.yy0, Language::Feature::MAXSTEP, NullaryTerm::Operator::MAXSTEP); }
#line 3637 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 49: /* term ::= MAXADDITIVE */
      case 63: /* term_strong ::= MAXADDITIVE */ yytestcase(yyruleno==63);
      case 88: /* term_no_const_strong ::= MAXADDITIVE */ yytestcase(yyruleno==88);
#line 584 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy163, yymsp[0].minor.yy0, Language::Feature::MAXADDITIVE, NullaryTerm::Operator::MAXADDITIVE); }
#line 3644 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 50: /* term ::= MAXAFVALUE */
      case 64: /* term_strong ::= MAXAFVALUE */ yytestcase(yyruleno==64);
      case 89: /* term_no_const_strong ::= MAXAFVALUE */ yytestcase(yyruleno==89);
#line 585 "bcplus/parser/detail/lemon_parser.y"
{ NULLARY_TERM(yygotominor.yy163, yymsp[0].minor.yy0, Language::Feature::MAXAFVALUE, NullaryTerm::Operator::MAXAFVALUE); }
#line 3651 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 51: /* term ::= DASH term */
      case 65: /* term_strong ::= DASH term_strong */ yytestcase(yyruleno==65);
      case 91: /* term_no_const_strong ::= DASH term_no_const_strong */ yytestcase(yyruleno==91);
      case 105: /* term_no_const ::= DASH term_no_const */ yytestcase(yyruleno==105);
#line 589 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, UnaryTerm::Operator::NEGATIVE); }
#line 3659 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 52: /* term ::= ABS term */
      case 66: /* term_strong ::= ABS term */ yytestcase(yyruleno==66);
      case 92: /* term_no_const_strong ::= ABS term_no_const */ yytestcase(yyruleno==92);
      case 106: /* term_no_const ::= ABS term_no_const */ yytestcase(yyruleno==106);
#line 590 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, UnaryTerm::Operator::ABS); }
#line 3667 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 53: /* term ::= term DASH term */
      case 68: /* term_strong ::= term_strong_candidate DASH term */ yytestcase(yyruleno==68);
      case 78: /* term_strong ::= term_strong DASH term */ yytestcase(yyruleno==78);
      case 93: /* term_no_const_strong ::= term_no_const_strong DASH term_no_const */ yytestcase(yyruleno==93);
      case 107: /* term_no_const ::= term_no_const DASH term_no_const */ yytestcase(yyruleno==107);
#line 594 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::MINUS); }
#line 3676 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 54: /* term ::= term PLUS term */
      case 69: /* term_strong ::= term_strong_candidate PLUS term */ yytestcase(yyruleno==69);
      case 79: /* term_strong ::= term_strong PLUS term */ yytestcase(yyruleno==79);
      case 94: /* term_no_const_strong ::= term_no_const_strong PLUS term_no_const */ yytestcase(yyruleno==94);
      case 108: /* term_no_const ::= term_no_const PLUS term_no_const */ yytestcase(yyruleno==108);
#line 595 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::PLUS); }
#line 3685 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 55: /* term ::= term STAR term */
      case 70: /* term_strong ::= term_strong_candidate STAR term */ yytestcase(yyruleno==70);
      case 80: /* term_strong ::= term_strong STAR term */ yytestcase(yyruleno==80);
      case 95: /* term_no_const_strong ::= term_no_const_strong STAR term_no_const */ yytestcase(yyruleno==95);
      case 109: /* term_no_const ::= term_no_const STAR term_no_const */ yytestcase(yyruleno==109);
#line 596 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::TIMES); }
#line 3694 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 56: /* term ::= term INT_DIV term */
      case 71: /* term_strong ::= term_strong_candidate INT_DIV term */ yytestcase(yyruleno==71);
      case 81: /* term_strong ::= term_strong INT_DIV term */ yytestcase(yyruleno==81);
      case 96: /* term_no_const_strong ::= term_no_const_strong INT_DIV term_no_const */ yytestcase(yyruleno==96);
      case 110: /* term_no_const ::= term_no_const INT_DIV term_no_const */ yytestcase(yyruleno==110);
#line 597 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::DIVIDE); }
#line 3703 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 57: /* term ::= term MOD term */
      case 72: /* term_strong ::= term_strong_candidate MOD term */ yytestcase(yyruleno==72);
      case 82: /* term_strong ::= term_strong MOD term */ yytestcase(yyruleno==82);
      case 97: /* term_no_const_strong ::= term_no_const_strong MOD term_no_const */ yytestcase(yyruleno==97);
      case 111: /* term_no_const ::= term_no_const MOD term_no_const */ yytestcase(yyruleno==111);
#line 598 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::MOD); }
#line 3712 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 67: /* term_strong_candidate ::= DASH constant */
#line 617 "bcplus/parser/detail/lemon_parser.y"
{ UNARY_ARITH(yygotominor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy345, UnaryTerm::Operator::NEGATIVE); }
#line 3717 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 73: /* term_strong ::= constant DASH term */
#line 626 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy345, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::MINUS); }
#line 3722 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 74: /* term_strong ::= constant PLUS term */
#line 627 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy345, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::PLUS); }
#line 3727 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 75: /* term_strong ::= constant STAR term */
#line 628 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy345, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::TIMES); }
#line 3732 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 76: /* term_strong ::= constant INT_DIV term */
#line 629 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy345, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::DIVIDE); }
#line 3737 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 77: /* term_strong ::= constant MOD term */
#line 630 "bcplus/parser/detail/lemon_parser.y"
{ BINARY_ARITH(yygotominor.yy163, yymsp[-2].minor.yy345, yymsp[-1].minor.yy0, yymsp[0].minor.yy163, BinaryTerm::Operator::MOD); }
#line 3742 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 90: /* term_no_const_strong ::= constant */
#line 652 "bcplus/parser/detail/lemon_parser.y"
{
		// error handling for constants so they don'yygotominor.yy163 default to undeclared identifiers
		yygotominor.yy163 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy345;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy345->beginLoc());
		YYERROR;
	}
#line 3753 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 104: /* term_no_const ::= constant */
#line 682 "bcplus/parser/detail/lemon_parser.y"
{
		// error handline for constants so they don'yygotominor.yy163 default to undeclared identifiers
		yygotominor.yy163 = NULL;
		ref_ptr<const Referenced> c_ptr = yymsp[0].minor.yy345;
		parser->_parse_error("Encountered unexpected constant symbol.", &yymsp[0].minor.yy345->beginLoc());
		YYERROR;
	}
#line 3764 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 112: /* num_range ::= term_numeric DBL_PERIOD term_numeric */
#line 712 "bcplus/parser/detail/lemon_parser.y"
{
	ref_ptr<const Referenced> l_ptr = yymsp[-2].minor.yy416, r_ptr = yymsp[0].minor.yy416, s_ptr = yymsp[-1].minor.yy0;

	yygotominor.yy309 = new NumberRange(yymsp[-2].minor.yy416->val(), yymsp[0].minor.yy416->val(), yymsp[-2].minor.yy416->beginLoc(), yymsp[0].minor.yy416->endLoc());

}
#line 3774 "bcplus/parser/detail/lemon_parser.c"
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
#line 3790 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 114: /* term_numeric ::= PAREN_L term_numeric PAREN_R */
#line 733 "bcplus/parser/detail/lemon_parser.y"
{ 
	ref_ptr<const Referenced> pl_ptr = yymsp[-2].minor.yy0, pr_ptr = yymsp[0].minor.yy0;
	yygotominor.yy416 = yymsp[-1].minor.yy416;  
	yygotominor.yy416->beginLoc(yymsp[-2].minor.yy0->beginLoc());
	yygotominor.yy416->endLoc(yymsp[0].minor.yy0->endLoc());
}
#line 3800 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 115: /* term_numeric ::= DASH term_numeric */
#line 753 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy416, yymsp[0].minor.yy416, -1 * yymsp[0].minor.yy416->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3806 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 116: /* term_numeric ::= ABS term_numeric */
#line 754 "bcplus/parser/detail/lemon_parser.y"
{ NUM_UOP(yygotominor.yy416, yymsp[0].minor.yy416, yymsp[0].minor.yy416->val() < 0 ? - yymsp[0].minor.yy416->val() : yymsp[0].minor.yy416->val());   yy_destructor(yypParser,111,&yymsp[-1].minor);
}
#line 3812 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 117: /* term_numeric ::= term_numeric DASH term_numeric */
#line 756 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() - yymsp[0].minor.yy416->val());   yy_destructor(yypParser,106,&yymsp[-1].minor);
}
#line 3818 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 118: /* term_numeric ::= term_numeric PLUS term_numeric */
#line 757 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() + yymsp[0].minor.yy416->val());   yy_destructor(yypParser,107,&yymsp[-1].minor);
}
#line 3824 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 119: /* term_numeric ::= term_numeric STAR term_numeric */
#line 758 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() * yymsp[0].minor.yy416->val());   yy_destructor(yypParser,108,&yymsp[-1].minor);
}
#line 3830 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 120: /* term_numeric ::= term_numeric INT_DIV term_numeric */
#line 759 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() / yymsp[0].minor.yy416->val());   yy_destructor(yypParser,109,&yymsp[-1].minor);
}
#line 3836 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 121: /* term_numeric ::= term_numeric MOD term_numeric */
#line 760 "bcplus/parser/detail/lemon_parser.y"
{ NUM_BOP(yygotominor.yy416, yymsp[-2].minor.yy416, yymsp[0].minor.yy416, yymsp[-2].minor.yy416->val() % yymsp[0].minor.yy416->val());   yy_destructor(yypParser,110,&yymsp[-1].minor);
}
#line 3842 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 122: /* formula ::= formula_base */
      case 165: /* formula_no_const ::= formula_no_const_base */ yytestcase(yyruleno==165);
      case 188: /* formula_temporal ::= formula_base */ yytestcase(yyruleno==188);
#line 818 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = yymsp[0].minor.yy353;				}
#line 3849 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 123: /* formula ::= PAREN_L formula PAREN_R */
      case 166: /* formula_no_const ::= PAREN_L formula_no_const PAREN_R */ yytestcase(yyruleno==166);
      case 189: /* formula_temporal ::= PAREN_L formula_temporal PAREN_R */ yytestcase(yyruleno==189);
#line 819 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = yymsp[-1].minor.yy353; yygotominor.yy353->parens(true); 	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 3858 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 124: /* formula ::= NOT formula */
      case 167: /* formula_no_const ::= NOT formula_no_const */ yytestcase(yyruleno==167);
      case 190: /* formula_temporal ::= NOT formula_temporal */ yytestcase(yyruleno==190);
#line 820 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_KEYWORD); }
#line 3865 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 125: /* formula ::= DASH formula */
      case 168: /* formula_no_const ::= DASH formula_no_const */ yytestcase(yyruleno==168);
      case 191: /* formula_temporal ::= DASH formula_temporal */ yytestcase(yyruleno==191);
#line 821 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_UOP(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, UnaryFormula::Operator::NOT, Language::Feature::FORMULA_NOT_DASH); }
#line 3872 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 126: /* formula ::= formula AMP formula */
      case 169: /* formula_no_const ::= formula_no_const AMP formula_no_const */ yytestcase(yyruleno==169);
      case 192: /* formula_temporal ::= formula_temporal AMP formula_temporal */ yytestcase(yyruleno==192);
#line 822 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new BinaryFormula(BinaryFormula::Operator::AND, yymsp[-2].minor.yy353, yymsp[0].minor.yy353, yymsp[-2].minor.yy353->beginLoc(), yymsp[0].minor.yy353->endLoc());   yy_destructor(yypParser,102,&yymsp[-1].minor);
}
#line 3880 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 127: /* formula ::= formula DBL_PLUS formula */
      case 128: /* formula ::= formula PIPE formula */ yytestcase(yyruleno==128);
      case 170: /* formula_no_const ::= formula_no_const DBL_PLUS formula_no_const */ yytestcase(yyruleno==170);
      case 171: /* formula_no_const ::= formula_no_const PIPE formula_no_const */ yytestcase(yyruleno==171);
      case 193: /* formula_temporal ::= formula_temporal DBL_PLUS formula_temporal */ yytestcase(yyruleno==193);
      case 194: /* formula_temporal ::= formula_temporal PIPE formula_temporal */ yytestcase(yyruleno==194);
#line 823 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy353, yymsp[-2].minor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, BinaryFormula::Operator::OR); }
#line 3890 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 129: /* formula ::= formula EQUIV formula */
      case 172: /* formula_no_const ::= formula_no_const EQUIV formula_no_const */ yytestcase(yyruleno==172);
      case 195: /* formula_temporal ::= formula_temporal EQUIV formula_temporal */ yytestcase(yyruleno==195);
#line 825 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy353, yymsp[-2].minor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, BinaryFormula::Operator::EQUIV); }
#line 3897 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 130: /* formula ::= formula IMPL formula */
      case 131: /* formula ::= formula ARROW_RDASH formula */ yytestcase(yyruleno==131);
      case 173: /* formula_no_const ::= formula_no_const IMPL formula_no_const */ yytestcase(yyruleno==173);
      case 174: /* formula_no_const ::= formula_no_const ARROW_RDASH formula_no_const */ yytestcase(yyruleno==174);
      case 196: /* formula_temporal ::= formula_temporal IMPL formula_temporal */ yytestcase(yyruleno==196);
      case 197: /* formula_temporal ::= formula_temporal ARROW_RDASH formula_temporal */ yytestcase(yyruleno==197);
#line 826 "bcplus/parser/detail/lemon_parser.y"
{ NESTED_BOP(yygotominor.yy353, yymsp[-2].minor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, BinaryFormula::Operator::IMPL); }
#line 3907 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 132: /* formula_base ::= comparison */
      case 175: /* formula_no_const_base ::= comparison_no_const */ yytestcase(yyruleno==175);
      case 215: /* head_formula ::= comparison */ yytestcase(yyruleno==215);
#line 829 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = yymsp[0].minor.yy353; }
#line 3914 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 133: /* formula_base ::= atomic_formula */
      case 216: /* head_formula ::= atomic_formula */ yytestcase(yyruleno==216);
#line 830 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = yymsp[0].minor.yy138; }
#line 3920 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 134: /* formula_base ::= formula_quant */
#line 831 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = yymsp[0].minor.yy285; }
#line 3925 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 135: /* formula_base ::= formula_card */
#line 833 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy353 = yymsp[0].minor.yy353;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_BODY)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_BODY, &yymsp[0].minor.yy353->beginLoc());
			YYERROR;
		}
	}
#line 3936 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 136: /* formula_base ::= TRUE */
      case 176: /* formula_no_const_base ::= TRUE */ yytestcase(yyruleno==176);
      case 218: /* head_formula ::= TRUE */ yytestcase(yyruleno==218);
#line 840 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new NullaryFormula(NullaryFormula::Operator::TRUE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3943 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 137: /* formula_base ::= FALSE */
      case 177: /* formula_no_const_base ::= FALSE */ yytestcase(yyruleno==177);
      case 219: /* head_formula ::= FALSE */ yytestcase(yyruleno==219);
#line 841 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new NullaryFormula(NullaryFormula::Operator::FALSE, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc()); }
#line 3950 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 138: /* comparison ::= term_strong EQ term */
      case 145: /* comparison ::= term_strong_candidate EQ term */ yytestcase(yyruleno==145);
      case 178: /* comparison_no_const ::= term_no_const_strong EQ term_no_const */ yytestcase(yyruleno==178);
#line 843 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 3958 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 139: /* comparison ::= term_strong DBL_EQ term */
      case 146: /* comparison ::= term_strong_candidate DBL_EQ term */ yytestcase(yyruleno==146);
      case 179: /* comparison_no_const ::= term_no_const_strong DBL_EQ term_no_const */ yytestcase(yyruleno==179);
#line 844 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 3966 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 140: /* comparison ::= term_strong NEQ term */
      case 147: /* comparison ::= term_strong_candidate NEQ term */ yytestcase(yyruleno==147);
      case 180: /* comparison_no_const ::= term_no_const_strong NEQ term_no_const */ yytestcase(yyruleno==180);
#line 845 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 3974 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 141: /* comparison ::= term_strong LTHAN term */
      case 148: /* comparison ::= term_strong_candidate LTHAN term */ yytestcase(yyruleno==148);
      case 181: /* comparison_no_const ::= term_no_const_strong LTHAN term_no_const */ yytestcase(yyruleno==181);
#line 846 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 3982 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 142: /* comparison ::= term_strong GTHAN term */
      case 149: /* comparison ::= term_strong_candidate GTHAN term */ yytestcase(yyruleno==149);
      case 182: /* comparison_no_const ::= term_no_const_strong GTHAN term_no_const */ yytestcase(yyruleno==182);
#line 847 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 3990 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 143: /* comparison ::= term_strong LTHAN_EQ term */
      case 150: /* comparison ::= term_strong_candidate LTHAN_EQ term */ yytestcase(yyruleno==150);
      case 183: /* comparison_no_const ::= term_no_const_strong LTHAN_EQ term_no_const */ yytestcase(yyruleno==183);
#line 848 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 3998 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 144: /* comparison ::= term_strong GTHAN_EQ term */
      case 151: /* comparison ::= term_strong_candidate GTHAN_EQ term */ yytestcase(yyruleno==151);
      case 184: /* comparison_no_const ::= term_no_const_strong GTHAN_EQ term_no_const */ yytestcase(yyruleno==184);
#line 849 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy163, yymsp[0].minor.yy163, yymsp[-2].minor.yy163->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 4006 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 152: /* comparison ::= constant DBL_EQ term */
#line 857 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::EQ, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,83,&yymsp[-1].minor);
}
#line 4012 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 153: /* comparison ::= constant NEQ term */
#line 858 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::NEQ, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,84,&yymsp[-1].minor);
}
#line 4018 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 154: /* comparison ::= constant LTHAN term */
#line 859 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,86,&yymsp[-1].minor);
}
#line 4024 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 155: /* comparison ::= constant GTHAN term */
#line 860 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,87,&yymsp[-1].minor);
}
#line 4030 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 156: /* comparison ::= constant LTHAN_EQ term */
#line 861 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::LTHAN_EQ, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,88,&yymsp[-1].minor);
}
#line 4036 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 157: /* comparison ::= constant GTHAN_EQ term */
#line 862 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = new ComparisonFormula(ComparisonFormula::Operator::GTHAN_EQ, yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());   yy_destructor(yypParser,89,&yymsp[-1].minor);
}
#line 4042 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 158: /* atomic_formula ::= constant */
      case 162: /* atomic_formula_anon ::= const_anon */ yytestcase(yyruleno==162);
      case 185: /* atomic_formula_one_const ::= constant_one_const */ yytestcase(yyruleno==185);
#line 889 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy138, yymsp[0].minor.yy345, true); }
#line 4049 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 159: /* atomic_formula ::= TILDE constant */
      case 163: /* atomic_formula_anon ::= TILDE const_anon */ yytestcase(yyruleno==163);
      case 186: /* atomic_formula_one_const ::= TILDE constant_one_const */ yytestcase(yyruleno==186);
#line 890 "bcplus/parser/detail/lemon_parser.y"
{ ATOMIC_FORMULA(yygotominor.yy138, yymsp[0].minor.yy345, false);   yy_destructor(yypParser,76,&yymsp[-1].minor);
}
#line 4057 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 160: /* atomic_formula ::= constant EQ term */
      case 164: /* atomic_formula_anon ::= const_anon EQ term */ yytestcase(yyruleno==164);
      case 187: /* atomic_formula_one_const ::= constant_one_const EQ term_no_const */ yytestcase(yyruleno==187);
#line 891 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy138 = new AtomicFormula(yymsp[-2].minor.yy345, yymsp[0].minor.yy163, yymsp[-2].minor.yy345->beginLoc(), yymsp[0].minor.yy163->endLoc());	  yy_destructor(yypParser,82,&yymsp[-1].minor);
}
#line 4065 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 161: /* atomic_formula_anon ::= atomic_formula */
      case 305: /* show_elem ::= atomic_formula_one_const */ yytestcase(yyruleno==305);
#line 893 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy138 = yymsp[0].minor.yy138; }
#line 4071 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 198: /* formula_temporal ::= term_strong COLON formula_temporal */
#line 972 "bcplus/parser/detail/lemon_parser.y"
{ BINDING(yygotominor.yy353, yymsp[-2].minor.yy163, yymsp[-1].minor.yy0, yymsp[0].minor.yy353); }
#line 4076 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 199: /* formula_quant ::= BRACKET_L quant_lst PIPE formula BRACKET_R */
#line 984 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy285=NULL;
		ref_ptr<const Token> bl_ptr = yymsp[-4].minor.yy0;
		ref_ptr<QuantifierFormula::QuantifierList> lst_ptr = yymsp[-3].minor.yy261;
		ref_ptr<Formula> sub_ptr = yymsp[-1].minor.yy353;
		ref_ptr<const Token> br_ptr = yymsp[0].minor.yy0;

		if (!parser->lang()->support(Language::Feature::FORMULA_QUANTIFIER)) {
			parser->_feature_error(Language::Feature::FORMULA_QUANTIFIER, &yymsp[-4].minor.yy0->beginLoc());
			YYERROR;
		} else yygotominor.yy285 = new QuantifierFormula(yymsp[-3].minor.yy261, yymsp[-1].minor.yy353, yymsp[-4].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
	  yy_destructor(yypParser,99,&yymsp[-2].minor);
}
#line 4093 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 200: /* quant_lst ::= quant_op variable */
#line 998 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy261 = new QuantifierFormula::QuantifierList();
		yygotominor.yy261->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy249, yymsp[0].minor.yy453));
	}
#line 4101 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 201: /* quant_lst ::= quant_lst quant_op variable */
#line 1004 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy261 = yymsp[-2].minor.yy261;
		yygotominor.yy261->push_back(QuantifierFormula::Quantifier(yymsp[-1].minor.yy249, yymsp[0].minor.yy453));
	}
#line 4109 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 202: /* quant_op ::= BIG_CONJ */
#line 1009 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy249 = QuantifierFormula::Operator::CONJ;   yy_destructor(yypParser,91,&yymsp[0].minor);
}
#line 4115 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 203: /* quant_op ::= BIG_DISJ */
#line 1010 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy249 = QuantifierFormula::Operator::DISJ;   yy_destructor(yypParser,92,&yymsp[0].minor);
}
#line 4121 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 204: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R */
#line 1056 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy319, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, NULL);  }
#line 4126 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 205: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R */
#line 1057 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, yymsp[-4].minor.yy163, yymsp[-3].minor.yy0, yymsp[-2].minor.yy319, yymsp[-1].minor.yy353,  yymsp[0].minor.yy0, NULL);  }
#line 4131 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 206: /* formula_card ::= CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 1058 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy319, yymsp[-2].minor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4136 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 207: /* formula_card ::= term_strong CBRACKET_L card_var_lst formula CBRACKET_R term */
#line 1059 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, yymsp[-5].minor.yy163, yymsp[-4].minor.yy0, yymsp[-3].minor.yy319, yymsp[-2].minor.yy353,  yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4141 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 208: /* formula_card ::= CBRACKET_L formula CBRACKET_R */
#line 1060 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, NULL);  }
#line 4146 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 209: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R */
#line 1061 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, yymsp[-3].minor.yy163, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy353,  yymsp[0].minor.yy0, NULL);  }
#line 4151 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 210: /* formula_card ::= CBRACKET_L formula CBRACKET_R term */
#line 1062 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4156 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 211: /* formula_card ::= term_strong CBRACKET_L formula CBRACKET_R term */
#line 1063 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy353, yymsp[-4].minor.yy163, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy353,  yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4161 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 212: /* card_var_lst ::= card_var_lst_inner PIPE */
#line 1067 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy319 = yymsp[-1].minor.yy319;
	  yy_destructor(yypParser,99,&yymsp[0].minor);
}
#line 4169 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 213: /* card_var_lst_inner ::= variable */
#line 1072 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy453;
		yygotominor.yy319 = new CardinalityFormula::VariableList();
		yygotominor.yy319->push_back(yymsp[0].minor.yy453->symbol());
	}
#line 4178 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 214: /* card_var_lst_inner ::= card_var_lst_inner COMMA variable */
#line 1079 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> v_ptr = yymsp[0].minor.yy453;
		yygotominor.yy319 = yymsp[-2].minor.yy319;
		yygotominor.yy319->push_back(yymsp[0].minor.yy453->symbol());
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4188 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 217: /* head_formula ::= formula_smpl_card */
#line 1099 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy353 = yymsp[0].minor.yy369;
		if (!parser->lang()->support(Language::Feature::FORMULA_CARDINALITY_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_CARDINALITY_HEAD, &yymsp[0].minor.yy369->beginLoc());
			YYERROR;
		}
	}
#line 4199 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 220: /* head_formula ::= DASH constant */
#line 1109 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy353 = NULL;
		ref_ptr<const Token> d_ptr = yymsp[-1].minor.yy0;
		ref_ptr<Constant> c_ptr = yymsp[0].minor.yy345;

		if (!parser->lang()->support(Language::Feature::FORMULA_NOT_DASH_HEAD)) {
			parser->_feature_error(Language::Feature::FORMULA_NOT_DASH_HEAD);
			YYERROR;
		} else {
			ATOMIC_FORMULA(yygotominor.yy353, yymsp[0].minor.yy345, false); 
		}
	}
#line 4215 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 221: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1122 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, NULL, yymsp[-3].minor.yy0, yymsp[-2].minor.yy319, yymsp[-1].minor.yy138, yymsp[0].minor.yy0, NULL);  }
#line 4220 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 222: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R */
#line 1123 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, yymsp[-4].minor.yy163, yymsp[-3].minor.yy0, yymsp[-2].minor.yy319, yymsp[-1].minor.yy138,  yymsp[0].minor.yy0, NULL);  }
#line 4225 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 223: /* formula_smpl_card ::= CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1124 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, NULL, yymsp[-4].minor.yy0, yymsp[-3].minor.yy319, yymsp[-2].minor.yy138, yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4230 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 224: /* formula_smpl_card ::= term_strong CBRACKET_L card_var_lst atomic_formula_one_const CBRACKET_R term */
#line 1125 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, yymsp[-5].minor.yy163, yymsp[-4].minor.yy0, yymsp[-3].minor.yy319, yymsp[-2].minor.yy138,  yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4235 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 225: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1126 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, NULL, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy138, yymsp[0].minor.yy0, NULL);  }
#line 4240 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 226: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R */
#line 1127 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, yymsp[-3].minor.yy163, yymsp[-2].minor.yy0, NULL, yymsp[-1].minor.yy138,  yymsp[0].minor.yy0, NULL);  }
#line 4245 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 227: /* formula_smpl_card ::= CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1128 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, NULL, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy138, yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4250 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 228: /* formula_smpl_card ::= term_strong CBRACKET_L atomic_formula_one_const CBRACKET_R term */
#line 1129 "bcplus/parser/detail/lemon_parser.y"
{ CARD_FORMULA(yygotominor.yy369, yymsp[-4].minor.yy163, yymsp[-3].minor.yy0, NULL, yymsp[-2].minor.yy138,  yymsp[-1].minor.yy0, yymsp[0].minor.yy163); 	}
#line 4255 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 229: /* stmt_macro_def ::= COLON_DASH MACROS macro_def_lst PERIOD */
#line 1148 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy455 = NULL;
        ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
        ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
        ref_ptr<MacroDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy105;
        ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;

        if (!parser->lang()->support(Language::Feature::DECL_MACRO)) {
            parser->_feature_error(Language::Feature::DECL_MACRO, &yymsp[-2].minor.yy0->beginLoc());
            YYERROR;
        } else {
		    BOOST_FOREACH(symbols::MacroSymbol* m, *yymsp[-1].minor.yy105) {
			    if (!parser->symtab()->create(m)) {
	    	        // Check if it's a duplicate
	    	        symbols::MacroSymbol* m2 = (symbols::MacroSymbol*)parser->symtab()->resolve(symbols::Symbol::Type::MACRO, *m->base(), m->arity());
		            if (!m2 || m2 != m) {
		                parser->_parse_error("Detected conflicting definition of symbol \"" + *m->name() + "\".", &yygotominor.yy455->beginLoc());
		            } else {
		                parser->_parse_error("Detected a duplicate definition of symbol \"" + *m->name() + "\".", &yygotominor.yy455->beginLoc());
		            }
		        }
		    }

			yygotominor.yy455 = new MacroDeclaration(yymsp[-1].minor.yy105, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
        }
    }
#line 4285 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 230: /* macro_def_lst ::= macro_bnd */
#line 1176 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy105 = new MacroDeclaration::ElementList();
        yygotominor.yy105->push_back(yymsp[0].minor.yy315);
    }
#line 4293 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 231: /* macro_def_lst ::= macro_def_lst SEMICOLON macro_bnd */
#line 1182 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy105 = yymsp[-2].minor.yy105;
        yygotominor.yy105->push_back(yymsp[0].minor.yy315);
      yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4302 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 232: /* macro_bnd ::= IDENTIFIER PAREN_L macro_args PAREN_R ARROW_RDASH MACRO_STRING */
#line 1188 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-5].minor.yy0;
        ref_ptr<MacroSymbol::ArgumentList> args_ptr = yymsp[-3].minor.yy354;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy315 = new MacroSymbol(yymsp[-5].minor.yy0->str(), yymsp[0].minor.yy0->str(), yymsp[-3].minor.yy354);
      yy_destructor(yypParser,72,&yymsp[-4].minor);
  yy_destructor(yypParser,73,&yymsp[-2].minor);
  yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4316 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 233: /* macro_bnd ::= IDENTIFIER ARROW_RDASH MACRO_STRING */
#line 1197 "bcplus/parser/detail/lemon_parser.y"
{
        ref_ptr<const Token> id_ptr = yymsp[-2].minor.yy0;
        ref_ptr<const Token> def_ptr = yymsp[0].minor.yy0;

        yygotominor.yy315 = new MacroSymbol(yymsp[-2].minor.yy0->str(), yymsp[0].minor.yy0->str());
      yy_destructor(yypParser,97,&yymsp[-1].minor);
}
#line 4327 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 234: /* macro_args ::= macro_arg */
#line 1205 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy354 = new MacroSymbol::ArgumentList();
        yygotominor.yy354->push_back(yymsp[0].minor.yy251->str());
        delete yymsp[0].minor.yy251;
    }
#line 4336 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 235: /* macro_args ::= macro_args COMMA macro_arg */
#line 1211 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy354 = yymsp[-2].minor.yy354;
        yygotominor.yy354->push_back(yymsp[0].minor.yy251->str());
        delete yymsp[0].minor.yy251;
      yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4346 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 236: /* macro_arg ::= POUND_INTEGER */
      case 237: /* macro_arg ::= POUND_IDENTIFIER */ yytestcase(yyruleno==237);
#line 1218 "bcplus/parser/detail/lemon_parser.y"
{
        yygotominor.yy251 = yymsp[0].minor.yy0;
    }
#line 4354 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 238: /* sort_lst ::= sort */
#line 1245 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy411 = new ConstantSymbol::SortList();
		yygotominor.yy411->push_back(yymsp[0].minor.yy393);
	}
#line 4362 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 239: /* sort_lst ::= sort_lst COMMA sort */
#line 1250 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy411 = yymsp[-2].minor.yy411;
		yygotominor.yy411->push_back(yymsp[0].minor.yy393);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4371 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 240: /* sort ::= sort_id_nr */
      case 246: /* sort_id_nr ::= sort_id */ yytestcase(yyruleno==246);
      case 247: /* sort_id_nr ::= sort_nr */ yytestcase(yyruleno==247);
#line 1279 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy393 = yymsp[0].minor.yy393; }
#line 4378 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 241: /* sort ::= sort_id_nr STAR */
#line 1280 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy393, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, Language::Feature::STAR_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::NONE)); }
#line 4383 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 242: /* sort ::= sort_id_nr CARROT */
#line 1281 "bcplus/parser/detail/lemon_parser.y"
{ DYNAMIC_SORT_PLUS(yygotominor.yy393, yymsp[-1].minor.yy393, yymsp[0].minor.yy0, Language::Feature::CARROT_SORT, parser->symtab()->bobj(SymbolTable::BuiltinObject::UNKNOWN)); }
#line 4388 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 243: /* sort ::= sort_nr PLUS object_nullary */
      case 244: /* sort ::= sort_id PLUS object_nullary */ yytestcase(yyruleno==244);
#line 1283 "bcplus/parser/detail/lemon_parser.y"
{ u::ref_ptr<const Object> o_ptr = yymsp[0].minor.yy238; DYNAMIC_SORT_PLUS(yygotominor.yy393, yymsp[-2].minor.yy393, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, yymsp[0].minor.yy238->symbol()); }
#line 4394 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 245: /* sort ::= sort_id PLUS INTEGER */
#line 1286 "bcplus/parser/detail/lemon_parser.y"
{ 
												  ref_ptr<const Object> t_ptr;
												  BASIC_TERM(t_ptr, yymsp[0].minor.yy0);
												  DYNAMIC_SORT_PLUS(yygotominor.yy393, yymsp[-2].minor.yy393, yymsp[-1].minor.yy0, Language::Feature::SORT_PLUS, t_ptr->symbol()); 
												}
#line 4403 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 248: /* sort_nr ::= num_range */
#line 1297 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy309;

		yygotominor.yy393 = NULL;

		if (!parser->lang()->support(Language::Feature::NUMRANGE_SORT)) {
			parser->_feature_error(Language::Feature::NUMRANGE_SORT, &yymsp[0].minor.yy309->beginLoc());
			YYERROR;
		}

		// X..Y becomes __sort_X_Y__
		std::string name = "__sort_" + boost::lexical_cast<std::string>(yymsp[0].minor.yy309->min()) + "__" + boost::lexical_cast<std::string>(yymsp[0].minor.yy309->max()) + "__";

		ref_ptr<SortSymbol::ObjectList> objs = new SortSymbol::ObjectList();

		// Generate the objects that it will have
		for (int i = yymsp[0].minor.yy309->min(); i <= yymsp[0].minor.yy309->max(); i++) {
			std::string obj_name = boost::lexical_cast<std::string>(i);
			ObjectSymbol const* sym = parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(obj_name)));

			if (!sym) {
				yygotominor.yy393 = NULL;
				parser->_parse_error("An error occurred creating symbol \"" + obj_name + "/0\".", &yymsp[0].minor.yy309->beginLoc());
				YYERROR;
			}
			objs->insert(sym);
		}		

		// dynamically declare the sort
		yygotominor.yy393 = parser->symtab()->resolveOrCreate(new SortSymbol(new ReferencedString(name), objs));
		if (!yygotominor.yy393) {
				parser->_parse_error("An error occurred creating symbol \"" + name + "/0\".", &yymsp[0].minor.yy309->beginLoc());
				YYERROR;
		} 
	}
#line 4442 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 249: /* sort_id ::= IDENTIFIER */
#line 1334 "bcplus/parser/detail/lemon_parser.y"
{
		// dynamically declare the sort
		yygotominor.yy393 = (SortSymbol*)parser->symtab()->resolve(Symbol::Type::SORT, *yymsp[0].minor.yy0->str());
		if (!yygotominor.yy393) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\" is not a declared sort.", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
		delete yymsp[0].minor.yy0;
	}
#line 4455 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 250: /* stmt_constant_def ::= COLON_DASH CONSTANTS constant_bnd_lst PERIOD */
#line 1365 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ConstantDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy97;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_CONSTANT)) {
			yygotominor.yy55 = NULL;
			parser->_feature_error(Language::Feature::DECL_CONSTANT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy55 = new ConstantDeclaration(yymsp[-1].minor.yy97, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

		}
	}
#line 4474 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 251: /* constant_bnd_lst ::= constant_bnd */
#line 1382 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = yymsp[0].minor.yy97;
	}
#line 4481 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 252: /* constant_bnd_lst ::= constant_bnd_lst SEMICOLON constant_bnd */
#line 1387 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<ConstantDeclaration::ElementList> bnd_ptr = yymsp[0].minor.yy97;
		yygotominor.yy97 = yymsp[-2].minor.yy97;
		yygotominor.yy97->splice(yygotominor.yy97->end(), *yymsp[0].minor.yy97);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4491 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 253: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type PAREN_L sort PAREN_R */
#line 1407 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-5].minor.yy42, s_ptr = yymsp[-1].minor.yy393;
		yygotominor.yy97 = new ConstantDeclaration::ElementList();


		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-5].minor.yy42) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[-3].minor.yy310, decl.first->str(), yymsp[-1].minor.yy393, decl.second);
			yygotominor.yy97->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,77,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4510 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 254: /* constant_bnd ::= constant_dcl_lst DBL_COLON sort */
#line 1420 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy42, s_ptr = yymsp[0].minor.yy393;
		yygotominor.yy97 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy42) {
			// attempt to declare each symbol
			ref_ptr<ConstantSymbol> c = new ConstantSymbol(ConstantSymbol::Type::RIGID, decl.first->str(), yymsp[0].minor.yy393, decl.second);
			yygotominor.yy97->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4525 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 255: /* constant_bnd ::= constant_dcl_lst DBL_COLON constant_dcl_type */
#line 1431 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Referenced> names_ptr = yymsp[-2].minor.yy42;
		yygotominor.yy97 = new ConstantDeclaration::ElementList();
		BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-2].minor.yy42) {
			// attempt to declare each symbol
			// NOTE: additive constants default to the additive sort, not the boolean sort
			ref_ptr<const SortSymbol> s = (yymsp[0].minor.yy310 & ConstantSymbol::Type::M_ADDITIVE ? parser->symtab()->bsort(SymbolTable::BuiltinSort::ADDITIVE) : parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN));


			ref_ptr<ConstantSymbol> c = new ConstantSymbol(yymsp[0].minor.yy310, decl.first->str(), parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN), decl.second);
			yygotominor.yy97->push_back(c);
			CONSTANT_DECL(c, decl.first->beginLoc());
		}
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4544 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 256: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER */
#line 1446 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-4].minor.yy42, s_ptr = yymsp[-2].minor.yy228, id_ptr = yymsp[0].minor.yy0;


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
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-4].minor.yy42) {
				ref_ptr<ConstantSymbol> c= new AttributeSymbol(decl.first->str(), yymsp[-2].minor.yy228, c, decl.second);
				yygotominor.yy97->push_back(c);
				CONSTANT_DECL(c, decl.first->beginLoc());
			}
		}
	  yy_destructor(yypParser,77,&yymsp[-3].minor);
  yy_destructor(yypParser,55,&yymsp[-1].minor);
}
#line 4573 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 257: /* constant_bnd ::= constant_dcl_lst DBL_COLON attrib_spec OF IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1470 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy97 = NULL;
		ref_ptr<const Referenced> names_ptr = yymsp[-7].minor.yy42, s_ptr = yymsp[-5].minor.yy228, id_ptr = yymsp[-3].minor.yy0, lst_ptr = yymsp[-1].minor.yy411;

		// attempt to resolve the attribute parent symbol
		ConstantSymbol const* c = (ConstantSymbol const*) parser->symtab()->resolve(Symbol::Type::CONSTANT, *yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy411->size());

		if (!c) {
			parser->_parse_error("\"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy411->size()) + "\" is not a valid constant symbol.", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else if (c->constType() != ConstantSymbol::Type::ABACTION && c->constType() != ConstantSymbol::Type::ACTION && c->constType() != ConstantSymbol::Type::EXOGENOUSACTION) {
			parser->_parse_error("Unexpected constant type of attribute parent \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy411->size()) + "\". Attribute parents must be an \"abAction\", \"action\", or \"exogenousAction\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// ensure that the sorts match the declaration of the symbol
			SortList::const_iterator it = yymsp[-1].minor.yy411->begin();
			BOOST_FOREACH(SortSymbol const* sort, *c) {
				if (*it != sort) {
					// check to see if it'yymsp[-5].minor.yy228 a subsort, which is also permissable
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
			BOOST_FOREACH(IdentifierDecl& decl, *yymsp[-7].minor.yy42) {
				// ensure that the sorts match the start of the sort list for each of the symbols
				if (decl.second->size() < yymsp[-1].minor.yy411->size()) {
					parser->_parse_error("Detected a malformed attribute declaration. An attribute must duplicate its parent'yymsp[-5].minor.yy228 parameters.", &decl.first->beginLoc());
					YYERROR;
				} else {
					bool good_sort = true;		
					it = decl.second->begin();
					BOOST_FOREACH(SortSymbol const* sort, *yymsp[-1].minor.yy411) {
						if (*it != sort) {
							// check to see if it'yymsp[-5].minor.yy228 a subsort, which is also permissable
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
						ref_ptr<ConstantSymbol> sym = new AttributeSymbol(decl.first->str(), yymsp[-5].minor.yy228, c, decl.second);
						yygotominor.yy97->push_back(sym);
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
#line 4654 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 258: /* constant_dcl_lst ::= IDENTIFIER */
#line 1546 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy42 = new IdentifierDeclList();
		yygotominor.yy42->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	}
#line 4662 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 259: /* constant_dcl_lst ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1551 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy42 = new IdentifierDeclList();
		yygotominor.yy42->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy411));
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4672 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 260: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER */
#line 1556 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy42 = yymsp[-2].minor.yy42;
		yygotominor.yy42->push_back(IdentifierDecl(yymsp[0].minor.yy0, NULL));
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4681 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 261: /* constant_dcl_lst ::= constant_dcl_lst COMMA IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1561 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy42 = yymsp[-5].minor.yy42;
		yygotominor.yy42->push_back(IdentifierDecl(yymsp[-3].minor.yy0, yymsp[-1].minor.yy411));
	  yy_destructor(yypParser,103,&yymsp[-4].minor);
  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4692 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 262: /* constant_dcl_type ::= ABACTION */
#line 1568 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::ABACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ABACTION)) {
			parser->_feature_error(Language::Feature::CONST_ABACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4704 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 263: /* constant_dcl_type ::= ACTION */
#line 1577 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::ACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ACTION)) {
			parser->_feature_error(Language::Feature::CONST_ACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4716 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 264: /* constant_dcl_type ::= ADDITIVEACTION */
#line 1586 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::ADDITIVEACTION;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEACTION)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4728 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 265: /* constant_dcl_type ::= ADDITIVEFLUENT */
#line 1595 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::ADDITIVEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_ADDITIVEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_ADDITIVEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4740 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 266: /* constant_dcl_type ::= EXTERNALACTION */
#line 1604 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::EXTERNALACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4752 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 267: /* constant_dcl_type ::= EXTERNALFLUENT */
#line 1613 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::EXTERNALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_EXTERNALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_EXTERNALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4764 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 268: /* constant_dcl_type ::= EXOGENOUSACTION */
#line 1622 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::EXOGENOUSACTION;
		if (!parser->lang()->support(Language::Feature::CONST_EXOGENOUSACTION)) {
			parser->_feature_error(Language::Feature::CONST_EXOGENOUSACTION, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4776 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 269: /* constant_dcl_type ::= INERTIALFLUENT */
#line 1631 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::INERTIALFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_INERTIALFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_INERTIALFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4788 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 270: /* constant_dcl_type ::= RIGID */
#line 1640 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::RIGID;
		if (!parser->lang()->support(Language::Feature::CONST_RIGID)) {
			parser->_feature_error(Language::Feature::CONST_RIGID, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4800 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 271: /* constant_dcl_type ::= SIMPLEFLUENT */
#line 1649 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::SIMPLEFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SIMPLEFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SIMPLEFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4812 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 272: /* constant_dcl_type ::= SDFLUENT */
#line 1659 "bcplus/parser/detail/lemon_parser.y"
{ 
		ref_ptr<const Token> tok_ptr = yymsp[0].minor.yy0;
		yygotominor.yy310 = ConstantSymbol::Type::SDFLUENT;
		if (!parser->lang()->support(Language::Feature::CONST_SDFLUENT)) {
			parser->_feature_error(Language::Feature::CONST_SDFLUENT, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
#line 4824 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 273: /* attrib_spec ::= ATTRIBUTE */
#line 1669 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy228 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[0].minor.yy0;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			// grab the boolean sort and provide it
			yygotominor.yy228 = parser->symtab()->bsort(SymbolTable::BuiltinSort::BOOLEAN);
		}
	}
#line 4839 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 274: /* attrib_spec ::= ATTRIBUTE PAREN_L sort PAREN_R */
#line 1682 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy228 = NULL;
		ref_ptr<const Referenced> kw_ptr = yymsp[-3].minor.yy0, s_ptr = yymsp[-1].minor.yy393;
		if (!parser->lang()->support(Language::Feature::CONST_ATTRIBUTE)) {
			parser->_feature_error(Language::Feature::CONST_ATTRIBUTE, &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy228 = yymsp[-1].minor.yy393;
		}
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4855 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 275: /* stmt_object_def ::= COLON_DASH OBJECTS object_bnd_lst PERIOD */
#line 1710 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<ObjectDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy198;
		
		if (!parser->lang()->support(Language::Feature::DECL_OBJECT)) {
			yygotominor.yy296 = NULL;
			parser->_feature_error(Language::Feature::DECL_OBJECT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy296 = new ObjectDeclaration(yymsp[-1].minor.yy198, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(ObjectDeclaration::Element* bnd, *yymsp[-1].minor.yy198) {
				BOOST_FOREACH(ObjectSymbol const* o, *bnd) {
						bnd->sort()->add(o);
				}
			}
		}
	}
#line 4880 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 276: /* object_bnd_lst ::= object_bnd */
#line 1733 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy198 = new ObjectDeclaration::ElementList();
		yygotominor.yy198->push_back(yymsp[0].minor.yy70);
	}
#line 4888 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 277: /* object_bnd_lst ::= object_bnd_lst SEMICOLON object_bnd */
#line 1739 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy198 = yymsp[-2].minor.yy198;
		yygotominor.yy198->push_back(yymsp[0].minor.yy70);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 4897 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 278: /* object_bnd ::= object_lst DBL_COLON sort_id */
#line 1745 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy70 = new ObjectDeclaration::Element(yymsp[0].minor.yy393, yymsp[-2].minor.yy341);
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 4905 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 279: /* object_lst ::= object_spec */
#line 1750 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy341 = yymsp[0].minor.yy341;
	}
#line 4912 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 280: /* object_lst ::= object_lst COMMA object_spec */
#line 1754 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy341 = yymsp[-2].minor.yy341;
		yygotominor.yy341->splice(yygotominor.yy341->end(), *yymsp[0].minor.yy341);
		delete yymsp[0].minor.yy341;
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 4922 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 281: /* object_spec ::= IDENTIFIER */
#line 1763 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> id_ptr = yymsp[0].minor.yy0;
		yygotominor.yy341 = NULL;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[0].minor.yy0->str()));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy341 = new ObjectDeclaration::Element::ObjectList();
			yygotominor.yy341->push_back(o);
		}
	}
#line 4938 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 282: /* object_spec ::= IDENTIFIER PAREN_L sort_lst PAREN_R */
#line 1776 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy341 = NULL;
		ref_ptr<ObjectSymbol::SortList> lst_ptr = yymsp[-1].minor.yy411;
		ref_ptr<const Token> id_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(yymsp[-3].minor.yy0->str(), yymsp[-1].minor.yy411));
		if (!o) {
			parser->_parse_error("Detected a conflicting definition of \"" + Symbol::genName(*yymsp[-3].minor.yy0->str(),yymsp[-1].minor.yy411->size()) + "\".", &yymsp[-3].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy341 = new  ObjectDeclaration::Element::ObjectList();
			yygotominor.yy341->push_back(o);
		}
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 4957 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 283: /* object_spec ::= num_range */
#line 1790 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy341 = new ObjectDeclaration::Element::ObjectList();
		ref_ptr<const Referenced> nr_ptr = yymsp[0].minor.yy309;

		// iterate over the range and add it to the list
		for (int i = yymsp[0].minor.yy309->min(); i <= yymsp[0].minor.yy309->max(); i++) {
			std::string name = boost::lexical_cast<std::string>(i);
			ref_ptr<const ObjectSymbol> o = parser->symtab()->resolveOrCreate(new ObjectSymbol(new ReferencedString(name)));
			if (!o) {
				parser->_parse_error("INTERNAL ERROR: Could not create object symbol \"" + Symbol::genName(name, 0) + "\".", &yymsp[0].minor.yy309->beginLoc());
				YYERROR;
			} else {
				yygotominor.yy341->push_back(o);
			}
		}
	}
#line 4977 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 284: /* stmt_variable_def ::= COLON_DASH VARIABLES variable_bnd_lst PERIOD */
#line 1822 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<VariableDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy117;
		
		if (!parser->lang()->support(Language::Feature::DECL_VARIABLE)) {
			yygotominor.yy171 = NULL;
			parser->_feature_error(Language::Feature::DECL_VARIABLE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy171 = new VariableDeclaration(yymsp[-1].minor.yy117, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());

			// Go ahead and add them to the symbol table
			BOOST_FOREACH(VariableSymbol* v, *yymsp[-1].minor.yy117) {
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
#line 5008 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 285: /* variable_bnd_lst ::= variable_bnd */
#line 1851 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy117 = yymsp[0].minor.yy117;
	}
#line 5015 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 286: /* variable_bnd_lst ::= variable_bnd_lst SEMICOLON variable_bnd */
#line 1856 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy117 = yymsp[-2].minor.yy117;
		yygotominor.yy117->splice(yygotominor.yy117->end(), *yymsp[0].minor.yy117);
		delete yymsp[0].minor.yy117;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5025 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 287: /* variable_bnd ::= variable_lst DBL_COLON sort_id */
#line 1863 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy117 = new VariableDeclaration::ElementList();

		BOOST_FOREACH(Token const* tok, *yymsp[-2].minor.yy152) {
			yygotominor.yy117->push_back(new VariableSymbol(tok->str(), yymsp[0].minor.yy393));
		}
		delete yymsp[-2].minor.yy152;
	  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5038 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 288: /* variable_lst ::= IDENTIFIER */
#line 1873 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy152 = new TokenList();
		yygotominor.yy152->push_back(yymsp[0].minor.yy0);
	}
#line 5046 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 289: /* variable_lst ::= variable_lst COMMA IDENTIFIER */
#line 1878 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy152 = yymsp[-2].minor.yy152;
		yygotominor.yy152->push_back(yymsp[0].minor.yy0);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5055 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 290: /* stmt_sort_def ::= COLON_DASH SORTS sort_bnd_lst PERIOD */
#line 1899 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0;
		ref_ptr<const Token> p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Token> kw_ptr = yymsp[-2].minor.yy0;
		ref_ptr<SortDeclaration::ElementList> l_ptr = yymsp[-1].minor.yy320;
		
		if (!parser->lang()->support(Language::Feature::DECL_SORT)) {
			yygotominor.yy469 = NULL;
			parser->_feature_error(Language::Feature::DECL_SORT, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy469 = new SortDeclaration(yymsp[-1].minor.yy320, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5073 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 291: /* sort_bnd_lst ::= sort_bnd */
      case 293: /* sort_bnd ::= sort_dcl_lst */ yytestcase(yyruleno==293);
#line 1915 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy320 = yymsp[0].minor.yy320;
	}
#line 5081 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 292: /* sort_bnd_lst ::= sort_bnd_lst SEMICOLON sort_bnd */
#line 1920 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy320 = yymsp[-2].minor.yy320;
		yygotominor.yy320->splice(yygotominor.yy320->end(), *yymsp[0].minor.yy320);
		delete yymsp[0].minor.yy320;
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5091 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 294: /* sort_bnd ::= sort_bnd DBL_LTHAN sort_bnd */
#line 1932 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy320) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy320) {
				sym2->addSubSort(sym);
			}
		}
		yygotominor.yy320 = yymsp[-2].minor.yy320;
		yygotominor.yy320->splice(yymsp[-2].minor.yy320->end(), *yymsp[0].minor.yy320);
		delete yymsp[0].minor.yy320;

	  yy_destructor(yypParser,101,&yymsp[-1].minor);
}
#line 5107 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 295: /* sort_bnd ::= sort_bnd DBL_GTHAN sort_bnd */
#line 1944 "bcplus/parser/detail/lemon_parser.y"
{
		BOOST_FOREACH(SortSymbol* sym, *yymsp[-2].minor.yy320) {
			BOOST_FOREACH(SortSymbol* sym2, *yymsp[0].minor.yy320) {
				sym->addSubSort(sym2);
			}
		}
		yygotominor.yy320 = yymsp[-2].minor.yy320;
		yygotominor.yy320->splice(yymsp[-2].minor.yy320->end(), *yymsp[0].minor.yy320);
		delete yymsp[0].minor.yy320;
	  yy_destructor(yypParser,100,&yymsp[-1].minor);
}
#line 5122 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 296: /* sort_bnd ::= PAREN_L sort_bnd PAREN_R */
#line 1955 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy320 = yymsp[-1].minor.yy320;
	  yy_destructor(yypParser,72,&yymsp[-2].minor);
  yy_destructor(yypParser,73,&yymsp[0].minor);
}
#line 5131 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 297: /* sort_dcl_lst ::= IDENTIFIER */
#line 1960 "bcplus/parser/detail/lemon_parser.y"
{
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yygotominor.yy320 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy320 = new SortDeclaration::ElementList();
			yygotominor.yy320->push_back(s);
		}

		delete yymsp[0].minor.yy0;
	}
#line 5148 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 298: /* sort_dcl_lst ::= sort_dcl_lst COMMA IDENTIFIER */
#line 1974 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy320 = yymsp[-2].minor.yy320;
		ref_ptr<SortSymbol> s = parser->symtab()->resolveOrCreate(new SortSymbol(yymsp[0].minor.yy0->str()));
		if (!s) {
			yymsp[-2].minor.yy320 = NULL;
			parser->_parse_error("Detected conflicting definition of sort symbol \"" + Symbol::genName(*yymsp[0].minor.yy0->str(),0) + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yymsp[-2].minor.yy320->push_back(s);
		}

		delete yymsp[0].minor.yy0;

	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5167 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 299: /* stmt_show ::= COLON_DASH SHOW show_lst PERIOD */
#line 2001 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy224 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<ShowStatement::ElementList> lst_ptr = yymsp[-1].minor.yy451;

		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy224 = new ShowStatement(yymsp[-1].minor.yy451, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5183 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 300: /* stmt_show ::= COLON_DASH SHOW ALL PERIOD */
#line 2015 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy224 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_SHOW)) {
			parser->_feature_error(Language::Feature::DECL_SHOW, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_SHOW_ALL)) {
			parser->_feature_error(Language::Feature::DECL_SHOW_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy224 = new ShowAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5201 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 301: /* stmt_hide ::= COLON_DASH HIDE show_lst PERIOD */
#line 2032 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy224 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<HideStatement::ElementList> lst_ptr = yymsp[-1].minor.yy451;

		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy224 = new HideStatement(yymsp[-1].minor.yy451, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5217 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 302: /* stmt_hide ::= COLON_DASH HIDE ALL PERIOD */
#line 2046 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy224 = NULL;
		ref_ptr<const Token> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, p_ptr = yymsp[0].minor.yy0, all_ptr = yymsp[-1].minor.yy0;
		
		if (!parser->lang()->support(Language::Feature::DECL_HIDE)) {
			parser->_feature_error(Language::Feature::DECL_HIDE, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else if (!parser->lang()->support(Language::Feature::DECL_HIDE_ALL)) {
			parser->_feature_error(Language::Feature::DECL_HIDE_ALL, &yymsp[-1].minor.yy0->beginLoc());
			YYERROR;
		} else {
			yygotominor.yy224 = new HideAllStatement(yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5235 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 303: /* show_lst ::= show_elem */
#line 2064 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy451 = new ShowStatement::ElementList();
		yygotominor.yy451->push_back(yymsp[0].minor.yy138);
	}
#line 5243 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 304: /* show_lst ::= show_lst COMMA show_elem */
#line 2069 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy451 = yymsp[-2].minor.yy451;
		yygotominor.yy451->push_back(yymsp[0].minor.yy138);
	  yy_destructor(yypParser,103,&yymsp[-1].minor);
}
#line 5252 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 306: /* stmt_noconcurrency ::= NOCONCURRENCY PERIOD */
#line 2097 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy210, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::NOCONCURRENCY, NCStatement); }
#line 5257 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 307: /* stmt_strong_noconcurrency ::= STRONG_NOCONCURRENCY PERIOD */
#line 2098 "bcplus/parser/detail/lemon_parser.y"
{ NC_STATEMENT(yygotominor.yy482, yymsp[-1].minor.yy0, yymsp[0].minor.yy0, Language::Feature::STRONG_NOCONCURRENCY, StrongNCStatement); }
#line 5262 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 308: /* stmt_maxafvalue ::= COLON_DASH MAXAFVALUE EQ term_numeric PERIOD */
#line 2124 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy224, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy416, yymsp[0].minor.yy0, Language::Feature::DECL_MAXAFVALUE, MaxAFValueStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5268 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 309: /* stmt_maxadditive ::= COLON_DASH MAXADDITIVE EQ term_numeric PERIOD */
#line 2125 "bcplus/parser/detail/lemon_parser.y"
{ VALUE_DECL(yygotominor.yy224, yymsp[-4].minor.yy0, yymsp[-3].minor.yy0, yymsp[-1].minor.yy416, yymsp[0].minor.yy0, Language::Feature::DECL_MAXADDITIVE, MaxAdditiveStatement);   yy_destructor(yypParser,82,&yymsp[-2].minor);
}
#line 5274 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 310: /* stmt_query ::= COLON_DASH QUERY query_lst PERIOD */
#line 2150 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy146 = NULL;
		ref_ptr<const Referenced> cd_ptr = yymsp[-3].minor.yy0, kw_ptr = yymsp[-2].minor.yy0, data_l_ptr = yymsp[-1].minor.yy301.l, p_ptr = yymsp[0].minor.yy0;
		ref_ptr<const Referenced> data_maxstep_ptr = yymsp[-1].minor.yy301.maxstep, data_label_ptr = yymsp[-1].minor.yy301.label;

		int min = -1, max = -1;
		if (yymsp[-1].minor.yy301.maxstep) {
			min = yymsp[-1].minor.yy301.maxstep->min();
			max = yymsp[-1].minor.yy301.maxstep->max();
		}

		if (!parser->lang()->support(Language::Feature::DECL_QUERY)) {
			parser->_feature_error(Language::Feature::DECL_QUERY, &yymsp[-2].minor.yy0->beginLoc());
			YYERROR;
		} else {
			bool good = true;

			// resolve the query label
			ref_ptr<QuerySymbol> sym = new QuerySymbol(yymsp[-1].minor.yy301.label->str(), min, max);
			if (!parser->symtab()->create(sym)) {
				parser->_parse_error("Could not create query, the label \"" + *yymsp[-1].minor.yy301.label->str() + "\" already exists.", &yymsp[-1].minor.yy301.label->beginLoc());
				good = false;
				YYERROR;
			}


			if (good) yygotominor.yy146 = new QueryStatement(sym, yymsp[-1].minor.yy301.l, yymsp[-3].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		}
	}
#line 5307 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 311: /* query_lst ::= formula_temporal */
#line 2182 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy301.l = new QueryStatement::FormulaList();
		yygotominor.yy301.maxstep = NULL;
		yygotominor.yy301.label = NULL;

		yygotominor.yy301.l->push_back(yymsp[0].minor.yy353);
	}
#line 5318 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 312: /* query_lst ::= query_maxstep_decl */
#line 2191 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy301.l = new QueryStatement::FormulaList();
		yygotominor.yy301.maxstep = yymsp[0].minor.yy392;
		yygotominor.yy301.label = NULL;
	}
#line 5327 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 313: /* query_lst ::= query_label_decl */
#line 2198 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy301.l = new QueryStatement::FormulaList();
		yygotominor.yy301.maxstep = NULL;
		yygotominor.yy301.label = yymsp[0].minor.yy251;
	}
#line 5336 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 314: /* query_lst ::= query_lst SEMICOLON formula_temporal */
#line 2205 "bcplus/parser/detail/lemon_parser.y"
{ 
		yygotominor.yy301 = yymsp[-2].minor.yy301;
		yymsp[-2].minor.yy301.l->push_back(yymsp[0].minor.yy353);
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5345 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 315: /* query_lst ::= query_lst SEMICOLON query_maxstep_decl */
#line 2211 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy301 = yymsp[-2].minor.yy301;

		if (yygotominor.yy301.maxstep) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy392->beginLoc());
			delete yymsp[0].minor.yy392;
			YYERROR;
		} else {
			yygotominor.yy301.maxstep = yymsp[0].minor.yy392;
		}
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5361 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 316: /* query_lst ::= query_lst SEMICOLON query_label_decl */
#line 2224 "bcplus/parser/detail/lemon_parser.y"
{
		yygotominor.yy301 = yymsp[-2].minor.yy301;
		if (yygotominor.yy301.label) {
			parser->_parse_error("Encountered multiple maxstep definitions within a query.", &yymsp[0].minor.yy251->beginLoc());
			delete yymsp[0].minor.yy251;
			YYERROR;

		} else {
			yygotominor.yy301.label = yymsp[0].minor.yy251;
		}
	  yy_destructor(yypParser,94,&yymsp[-1].minor);
}
#line 5377 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 317: /* query_maxstep_decl ::= MAXSTEP DBL_COLON INTEGER */
#line 2250 "bcplus/parser/detail/lemon_parser.y"
{ 
	yygotominor.yy392 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, i_ptr = yymsp[0].minor.yy0;


	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {

		int max = -1;
		try {
			max = boost::lexical_cast<int>(*yymsp[0].minor.yy0->str());
			yygotominor.yy392 = new NumberRange(-1, max, yymsp[0].minor.yy0->beginLoc(), yymsp[0].minor.yy0->endLoc());
		} catch (boost::bad_lexical_cast const& e) {
			parser->_parse_error("INTERNAL ERROR: An error occurred extracting an integer from \"" + *yymsp[0].minor.yy0->str() + "\".", &yymsp[0].minor.yy0->beginLoc());
			YYERROR;
		}
	}
  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5402 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 318: /* query_maxstep_decl ::= MAXSTEP DBL_COLON num_range */
#line 2271 "bcplus/parser/detail/lemon_parser.y"
{
	yygotominor.yy392 = NULL;
	ref_ptr<const Referenced> kw_ptr = yymsp[-2].minor.yy0, nr_ptr = yymsp[0].minor.yy309;

	if (!parser->lang()->support(Language::Feature::QUERY_MAXSTEP)) {
		parser->_feature_error(Language::Feature::QUERY_MAXSTEP, &yymsp[-2].minor.yy0->beginLoc());
		YYERROR;
	} else {
		yygotominor.yy392 = yymsp[0].minor.yy309;
		nr_ptr.release();
	}
  yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5419 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 319: /* query_label_decl ::= LABEL DBL_COLON INTEGER */
      case 320: /* query_label_decl ::= LABEL DBL_COLON IDENTIFIER */ yytestcase(yyruleno==320);
#line 2285 "bcplus/parser/detail/lemon_parser.y"
{ QUERY_DECL(yygotominor.yy251, yymsp[-2].minor.yy0, yymsp[0].minor.yy0, Language::Feature::QUERY_LABEL);   yy_destructor(yypParser,77,&yymsp[-1].minor);
}
#line 5426 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 321: /* clause_if ::= IF formula */
#line 2320 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, Language::Feature::CLAUSE_IF); 		}
#line 5431 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 322: /* clause_if ::= */
      case 324: /* clause_after ::= */ yytestcase(yyruleno==324);
      case 326: /* clause_ifcons ::= */ yytestcase(yyruleno==326);
      case 330: /* clause_where ::= */ yytestcase(yyruleno==330);
#line 2321 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy353 = NULL; }
#line 5439 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 323: /* clause_after ::= AFTER formula */
#line 2322 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, Language::Feature::CLAUSE_AFTER);	}
#line 5444 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 325: /* clause_ifcons ::= IFCONS formula */
#line 2324 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, Language::Feature::CLAUSE_IFCONS); 	}
#line 5449 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 327: /* clause_unless ::= UNLESS atomic_formula_anon */
#line 2326 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy138, yymsp[-1].minor.yy0, yymsp[0].minor.yy138, Language::Feature::CLAUSE_UNLESS); 	}
#line 5454 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 328: /* clause_unless ::= */
#line 2327 "bcplus/parser/detail/lemon_parser.y"
{ yygotominor.yy138 = NULL; }
#line 5459 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 329: /* clause_where ::= WHERE formula_no_const */
#line 2328 "bcplus/parser/detail/lemon_parser.y"
{ CLAUSE(yygotominor.yy353, yymsp[-1].minor.yy0, yymsp[0].minor.yy353, Language::Feature::CLAUSE_WHERE); 	}
#line 5464 "bcplus/parser/detail/lemon_parser.c"
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
#line 2374 "bcplus/parser/detail/lemon_parser.y"
{yygotominor.yy224 = yymsp[0].minor.yy224;}
#line 5486 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 349: /* law_basic ::= head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2490 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, NULL, yymsp[-6].minor.yy353, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_BASIC_S, 
																																															Language::Feature::LAW_BASIC_D, BasicLaw); }
#line 5493 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 350: /* law_caused ::= CAUSED head_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2494 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, yymsp[-7].minor.yy0, yymsp[-6].minor.yy353, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_CAUSED_S, 
																																															Language::Feature::LAW_CAUSED_D, CausedLaw); }
#line 5500 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 351: /* law_pcaused ::= POSSIBLY_CAUSED atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2498 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, yymsp[-7].minor.yy0, yymsp[-6].minor.yy138, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_PCAUSED_S, 
																																															Language::Feature::LAW_PCAUSED_D, PossiblyCausedLaw); }
#line 5507 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 352: /* law_impl ::= head_formula ARROW_LDASH formula clause_where PERIOD */
#line 2502 "bcplus/parser/detail/lemon_parser.y"
{ LAW_IMPL_FORM(yygotominor.yy224, yymsp[-4].minor.yy353, yymsp[-3].minor.yy0, yymsp[-2].minor.yy353, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, 
																																														Language::Feature::LAW_IMPL, ImplicationLaw); }
#line 5513 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 353: /* law_causes ::= atomic_formula CAUSES head_formula clause_if clause_unless clause_where PERIOD */
#line 2505 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy224, yymsp[-6].minor.yy138, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CAUSES, CausesLaw); }
#line 5519 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 354: /* law_increments ::= atomic_formula INCREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2509 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy224, yymsp[-8].minor.yy138, yymsp[-7].minor.yy0, yymsp[-6].minor.yy345, yymsp[-4].minor.yy163, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_INCREMENTS, IncrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5526 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 355: /* law_decrements ::= atomic_formula DECREMENTS constant BY term clause_if clause_unless clause_where PERIOD */
#line 2512 "bcplus/parser/detail/lemon_parser.y"
{ LAW_INCREMENTAL_FORM(yygotominor.yy224, yymsp[-8].minor.yy138, yymsp[-7].minor.yy0, yymsp[-6].minor.yy345, yymsp[-4].minor.yy163, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_DECREMENTS, DecrementsLaw);   yy_destructor(yypParser,31,&yymsp[-5].minor);
}
#line 5533 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 356: /* law_mcause ::= atomic_formula MAY_CAUSE head_formula clause_if clause_unless clause_where PERIOD */
#line 2516 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_FORM(yygotominor.yy224, yymsp[-6].minor.yy138, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_MCAUSE, MayCauseLaw); }
#line 5539 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 357: /* law_always ::= ALWAYS formula clause_after clause_unless clause_where PERIOD */
#line 2520 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy224, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_ALWAYS_S, 
																																															Language::Feature::LAW_ALWAYS_D, AlwaysLaw); }
#line 5546 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 358: /* law_constraint ::= CONSTRAINT formula clause_after clause_unless clause_where PERIOD */
#line 2524 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy224, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_CONSTRAINT_S, 
																																															Language::Feature::LAW_CONSTRAINT_D, ConstraintLaw); }
#line 5553 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 359: /* law_impossible ::= IMPOSSIBLE formula clause_after clause_unless clause_where PERIOD */
#line 2528 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy224, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_IMPOSSIBLE_S, 
																																															Language::Feature::LAW_IMPOSSIBLE_D, ImpossibleLaw); }
#line 5560 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 360: /* law_never ::= NEVER formula clause_after clause_unless clause_where PERIOD */
#line 2532 "bcplus/parser/detail/lemon_parser.y"
{ LAW_CONSTRAINT_FORM(yygotominor.yy224, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_NEVER_S, 
																																															Language::Feature::LAW_NEVER_D, NeverLaw); }
#line 5567 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 361: /* law_default ::= DEFAULT atomic_formula clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2536 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, yymsp[-7].minor.yy0, yymsp[-6].minor.yy138, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_DEFAULT_S,
																																															Language::Feature::LAW_DEFAULT_D, DefaultLaw); }
#line 5574 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 362: /* law_exogenous ::= EXOGENOUS constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2540 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, yymsp[-7].minor.yy0, yymsp[-6].minor.yy345, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_EXOGENOUS_S,
																																															Language::Feature::LAW_EXOGENOUS_D, ExogenousLaw); }
#line 5581 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 363: /* law_inertial ::= INERTIAL constant clause_if clause_ifcons clause_after clause_unless clause_where PERIOD */
#line 2544 "bcplus/parser/detail/lemon_parser.y"
{ LAW_BASIC_FORM(yygotominor.yy224, yymsp[-7].minor.yy0, yymsp[-6].minor.yy345, yymsp[-5].minor.yy353, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, 
																																														yymsp[-2].minor.yy138, yymsp[-1].minor.yy353, yymsp[0].minor.yy0, Language::Feature::LAW_INERTIAL_S,
																																															Language::Feature::LAW_INERTIAL_D, InertialLaw); }
#line 5588 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 364: /* law_nonexecutable ::= NONEXECUTABLE formula clause_if clause_unless clause_where PERIOD */
#line 2548 "bcplus/parser/detail/lemon_parser.y"
{ LAW_DYNAMIC_CONSTRAINT_FORM(yygotominor.yy224, yymsp[-5].minor.yy0, yymsp[-4].minor.yy353, yymsp[-3].minor.yy353, yymsp[-2].minor.yy138, yymsp[-1].minor.yy353,
																																														yymsp[0].minor.yy0, Language::Feature::LAW_NONEXECUTABLE, NonexecutableLaw); }
#line 5594 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 365: /* law_rigid ::= RIGID constant clause_where PERIOD */
#line 2552 "bcplus/parser/detail/lemon_parser.y"
{ LAW_SIMPLE_FORM(yygotominor.yy224, yymsp[-3].minor.yy0, yymsp[-2].minor.yy345, yymsp[-1].minor.yy353, yymsp[0].minor.yy0,
																																														Language::Feature::LAW_RIGID, RigidLaw); }
#line 5600 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 366: /* law_observed ::= OBSERVED atomic_formula AT term_no_const PERIOD */
#line 2557 "bcplus/parser/detail/lemon_parser.y"
{ 
			yygotominor.yy224 = NULL;
			ref_ptr<const Token> kw_ptr = yymsp[-4].minor.yy0, p_ptr = yymsp[0].minor.yy0;
			ref_ptr<AtomicFormula> head_ptr = yymsp[-3].minor.yy138;
			ref_ptr<Term> t_ptr = yymsp[-1].minor.yy163;

			// make sure that the At clause is integral
			if (yymsp[-1].minor.yy163->domainType() != DomainType::INTEGRAL) {
				parser->_parse_error("Expected an integral expression.", &yymsp[-1].minor.yy163->beginLoc());
				YYERROR;
			} else {
				LAW_SIMPLE_FORM(yygotominor.yy224, yymsp[-4].minor.yy0, yymsp[-3].minor.yy138, yymsp[-1].minor.yy163, yymsp[0].minor.yy0, Language::Feature::LAW_OBSERVED, ObservedLaw); 
			}
		  yy_destructor(yypParser,66,&yymsp[-2].minor);
}
#line 5619 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 367: /* stmt_code_blk ::= ASP_GR */
#line 2591 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_GR, ASPBlock);	}
#line 5624 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 368: /* stmt_code_blk ::= ASP_CP */
#line 2592 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_ASP_CP, ASPBlock);	}
#line 5629 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 369: /* stmt_code_blk ::= F2LP_GR */
#line 2593 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_GR, F2LPBlock);	}
#line 5634 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 370: /* stmt_code_blk ::= F2LP_CP */
#line 2594 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_F2LP_CP, F2LPBlock); }
#line 5639 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 371: /* stmt_code_blk ::= LUA_GR */
#line 2595 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_GR, LUABlock);   }
#line 5644 "bcplus/parser/detail/lemon_parser.c"
        break;
      case 372: /* stmt_code_blk ::= LUA_CP */
#line 2596 "bcplus/parser/detail/lemon_parser.y"
{ CODE_BLK(yygotominor.yy224, yymsp[0].minor.yy0, Language::Feature::CODE_LUA_CP, LUABlock);   }
#line 5649 "bcplus/parser/detail/lemon_parser.c"
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
#line 5715 "bcplus/parser/detail/lemon_parser.c"
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
