#pragma once

#include "babb/utils/memory.h"
#include "bcplus/Location.h"

namespace bcplus {
namespace parser {
namespace detail {


/// Simple base class for all scanner source
class ScannerSource : public babb::utils::Referenced {
public:
	/*****************************************************************************************************/
	/* Public Types */
	/*****************************************************************************************************/
	/// Encapsulating structure for subtypes
	struct Type {
		enum Value {
			RAW,			///< The input is raw text
			TOKENS			///< The input is a preparsed token stream
		};
	};


private:
	/*****************************************************************************************************/
	/* Private members */
	/*****************************************************************************************************/
	/// The source type
	Type::Value _type;


public:
	/*****************************************************************************************************/
	/* Constructor / Destructor */
	/*****************************************************************************************************/
	/// Basic Constructor
	/// @param type The subtype of this source
	ScannerSource(Type::Value type);

	/// Destructor stub
	virtual ~ScannerSource();


	/*****************************************************************************************************/
	/* Public Members */
	/*****************************************************************************************************/
	/// Get the subtype of this source
	inline Type::Value type() const													{ return _type; }


	/// Get the location that the source stream is at
	virtual Location loc() const = 0;

};

}}}
