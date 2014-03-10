#pragma once

#include "parser/symbols/ObjectSymbol.h"
#include "parser/symbols/VariableSymbol.h"
#include "parser/symbols/ConstantSymbol.h"
#include "parser/elements/detail/ElementClass.h"
#include "parser/elements/detail/BinaryElement.h"
#include "parser/elements/detail/UnaryElement.h"
#include "parser/elements/detail/IdentifierElement.h"

namespace bcplus {
namespace parser {
namespace elements {

namespace detail {

/// Container for the types of terms that we can have.
struct TermType {
	enum Value {
		BINARY,
		UNARY,
		OBJECT,
		VARIABLE,
		CONSTANT
	};
};


/// Container describing the binary operators for terms
struct BinaryTermOperator {
		/// The possible binary operators.
		enum Value {
			PLUS,
			MINUS,
			TIMES,
			DIVIDE,
			MOD
		};

	/// Functor converting binary operators to cstrings
	struct cstr {
			char const* operator()(Value op) {
				switch (op) {
				case PLUS:		return " + ";
				case MINUS:		return " - ";
				case TIMES:		return " * ";
				case DIVIDE:	return " / ";
				case MOD:		return " mod ";
				}
			};

	};

};

/// Container describing unary oeprators for terms
struct UnaryTermOperator {
	/// The possible unary operators
	enum Value {
		NEGATIVE,
		ABS
	};

	/// Functor mapping each operator to its prenix cstring
	struct pre_cstr {

			char const* operator()(Value op) {
				switch (op) {
				case NEGATIVE:		return "-";
				case ABS:			return "abs ";
				}
			}
	};

	/// Functor mapping each operator to its postnix cstring
	struct post_cstr {

			char const* operator()(Value op) {
				switch (op) {
				default:			return "";
				}
			}
	};

};

}

/**
 * @brief An abritrary term.
 */
typedef detail::ElementClass<
	Element::Type::TERM, 
	detail::TermType> Term;

/**
 * @brief A binary operator term of the form T @ V where T,V are terms and @ is a binary operator.
 */
typedef detail::BinaryElement<
	Term, 
	detail::TermType::BINARY, 
	detail::BinaryTermOperator::Value, 
	detail::BinaryTermOperator::cstr> BinaryTerm;
/**
 * @brief A unary operator term of the form @T where @ is a unary operator and T is a term.
 */
typedef detail::UnaryElement<
	Term, 
	detail::TermType::UNARY,  
	detail::UnaryTermOperator::Value, 
	detail::UnaryTermOperator::pre_cstr, 
	detail::UnaryTermOperator::post_cstr> UnaryTerm;

/**
 * @brief An object constant 't'
 */
typedef detail::IdentifierElement<
	Term, 
	detail::TermType::OBJECT, 
	symbols::ObjectSymbol, 
	Term> Object;

/**
 * @brief A variable 'V'.
 */
typedef detail::IdentifierElement_bare<
	Term, 
	detail::TermType::VARIABLE, 
	symbols::VariableSymbol> Variable;
/** 
 * @brief A bare constant 'c'
 */
typedef detail::IdentifierElement<
	Term,
	detail::TermType::CONSTANT, 
	symbols::ConstantSymbol, 
	Term> Constant;

}}}

