#pragma once

#include "bcplus/elements/detail/ElementClass.h"
#include "bcplus/elements/detail/BinaryElement.h"
#include "bcplus/elements/detail/UnaryElement.h"

namespace bcplus {
namespace elements {

namespace detail {

/// Container of enumeration of possible types
struct FormulaType {
	enum Value {
		BINARY,
		UNARY,
		QUANTIFIER,
		ATOMIC
	};
};

/// Structure descrbing binary formula operators.
struct BinaryFormulaOperator {
		enum Value {
			AND,
			OR,
			EQUIV,
			IMPL,
			REV_IMPL, // fake operator to simulate F <- G.
			EQ,
			NEQ,
			DBL_EQ,
			LTHAN,
			GTHAN,
			LTHAN_EQ,
			GTHAN_EQ,
			BIND_STEP
		};

	/// Functor to convert binary formula operators to cstrings.
	struct cstr {

		char const* operator() (Value op) {
			switch (op) {
			case AND:			return " & ";
			case OR:			return " ++ ";
			case EQUIV:			return " <-> ";
			case IMPL:			return " -> ";
			case REV_IMPL:		return " <- ";
			case EQ:			return " = ";
			case NEQ:			return " \\= ";
			case DBL_EQ:		return " == ";
			case LTHAN:			return " < ";
			case GTHAN:			return " > ";
			case LTHAN_EQ:		return " =< ";
			case GTHAN_EQ:		return " >= ";
			case BIND_STEP:		return " : ";
			}
		}
	};
};

/// Structure describing unary formula operators.
struct UnaryFormulaOperator {
	enum Value {
			NOT,
            NOT_DASH,
            STRONG_NOT,
            NEGATIVE,
            ABS,
            EXOGENOUS,
            INERTIAL,
            RIGID,
            CHOICE
	};

	/// Functor to map unary formula operators to prenix symbols.
	struct pre_cstr {
			char const* operator()(Value op) {
				switch (op) {
				case NOT: 			return "not ";
				case NOT_DASH:		return "-";
				case STRONG_NOT:	return "~";
				case NEGATIVE:		return "-";
				case ABS:			return "abs ";
				case EXOGENOUS:		return "exogenous ";
				case INERTIAL:		return "inertial ";
				case RIGID:			return "rigid ";
				case CHOICE:		return "{";
				}
			}
	};

	/// Functor to map unary formula operators to postnix symbols.
	struct post_cstr {
			char const* operator()(Value op) {
				switch (op) {
				case CHOICE:		return "}";
				default:			return "";
				}
			}
	};

};

}

/**
 * @brief An arbitrary formula that evaluates to true or false.
 */
typedef detail::ElementClass<
	Element::Type::FORMULA, 
	detail::FormulaType > Formula;

/** 
 * @brief A binary formula of the form (F @ G) where '@' is some operator.
 */
typedef detail::BinaryElement<
	Formula, 
	detail::FormulaType::BINARY, 
	detail::BinaryFormulaOperator::Value, 
	detail::BinaryFormulaOperator::cstr > BinaryFormula;

/**
 * @breif A unary formula of the form @F where '@' is some operator.
 */
typedef detail::UnaryElement<
	Formula, 
	detail::FormulaType::UNARY,
	detail::UnaryFormulaOperator::Value, 
	detail::UnaryFormulaOperator::pre_cstr, 
	detail::UnaryFormulaOperator::post_cstr > UnaryFormula;

}}

#include "bcplus/elements/AtomicFormula.h"
#include "bcplus/elements/QuantifierFormula.h"

