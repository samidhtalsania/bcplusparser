#pragma once

#include <string>
#include <iostream>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include "Referenced.h"
#include "referencedwrappers.h"
#include "Configuration.h"

#include "parser/Location.h"


namespace bcplus {
namespace parser {
namespace detail {

/// A simple base class for working with various sources in the scanner
class ScannerSource : public Referenced {
public:
	/***********************************************************/
	/* Public Types */
	/***********************************************************/

	enum Status {
		STAT_OK,
		STAT_FAIL,
		STAT_CLOSED,
		STAT_EOF
	};

private:
	
	/***********************************************************/
	/* Private Members */
	/***********************************************************/

	/// Configuration
	ref_ptr<Configuration> _config;

	/// Scanner State
	char const* _cursor;
	char const* _limit;
	char const* _token;
	char const* _marker;

public:
	/***********************************************************/
	/* Constructors */
	/***********************************************************/
	/// @param config The system wide configuration information
	ScannerSource(Configuration* config);

	/// Destructor
	virtual ~ScannerSource();

	/***********************************************************/
	/* Public Functions */
	/***********************************************************/

	/// Gets the system configuration
	inline Configuration const* config() const	{ return _config; }
	inline Configuration* config()				{ return _config; }

	/// Scanner state accessors / mutators

	inline char const*& cursor()				{ return _cursor; }
	inline char const* const& cursor() const	{ return _cursor; }
	inline void cursor(char const* c)			{ _cursor = c; }

	inline char const*& limit() 				{ return _limit; }
	inline char const* const& limit() const		{ return _limit; }
	inline void limit(char const* l)			{ _limit = l; }
	
	inline char const*& token() 				{ return _token; }
	inline char const* const& token() const		{ return _token; }
	inline void token(char const* t)			{ _token = t; }
	
	inline char const*& marker() 				{ return _marker; }
	inline char const* const& marker() const	{ return _marker; }
	inline void marker(char const* m)			{ _marker = m; }

	/// Determines if the source is ready to read from
	inline bool good() const					{ return status() == STAT_OK; }

	/// Callback which indicates that the scanner found a newline and the location should be updated
	virtual void newline() = 0;

	/// Gets the current location of the source
	virtual Location loc() const = 0;
	
	/// Determines the status of the source
	virtual Status status() const = 0;

	/// Closes the source
	virtual void close() = 0; 
	
	/// Refills the buffer to the desired level
	/// @param n The minimum number of characters to fill.
	virtual void fill(size_t n) = 0;

};

}}}


