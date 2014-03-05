#pragma once

#include <string>
#include <iostream>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include "Referenced.h"
#include "referencedwrappers.h"
#include "Configuration.h"

#include "parser/Location.h"
#include "parser/Token.h"
#include "parser/detail/ScannerSource.h"

namespace bcplus  {
namespace parser  {
namespace details {


class Scanner : public Referenced {

private:
	/***********************************************************/
	/* Private Types */
	/***********************************************************/
	/// Condition type for the scanner
	enum YYCONDTYPE {
		yycNORMAL,
		yycSTRING,
		yycCOMMENT
	};;

	/***********************************************************/
	/* Private Members */
	/***********************************************************/

	/// Input sources
	std::list<ref_ptr<ScannerSource> > _sources;

	/// Scanner State
	YYCONDTYPE _cond;

	/// The last token read in
	ref_ptr<Token> _last_token;

	/// System-wide configuration information
	ref_ptr<Configuration> _config;
public:
	/***********************************************************/
	/* Constructors */
	/***********************************************************/
	/// @param config System wide configuration information
	Scanner(Configuration* config);

	/// Destructor
	virtual ~Scanner();

	/***********************************************************/
	/* Public Functions */
	/***********************************************************/


	/// Attempts to open the provided file and inject it into the front of the stream being read by the scanner.
	/// @param file The file to read from.
	/// @param squelch Whether to silence all errors resulting from this call.
	/// @return True if successful, false otherwise.
	bool push_front(boost::filesystem::path const& file, bool squelch = false);

	/// Attempts to open the provided file and append it to the end of the stream being read by the scanner.
	/// @param file The file to read from.
	/// @param squelch Whether to silence all errors resulting from this call.
	/// @return True if successful, false otherwise.
	bool push_back(boost::filesystem::path const& file, bool squelch = false);

	/// Injects the provided null-terminated buffer into the front of the stream being read by the scanner.
	/// @param buffer The null-terminated buffer to read from.
	/// @param loc The location to display in messages partaining to this buffer.
	void push_front(char const* buffer, Location const& loc = Location(NULL, 0, 0));
	
	/// Appends the provided null-terminated buffer to the end of the stream being read by the scanner.
	/// @param buffer The null-terminated buffer to read from.
	/// @param loc The location to display in messages partaining to this buffer.
	void push_back(char const* buffer, Location const& loc = Location(NULL, 0, 0));

	/// Reads a token from the currently open file.
	/// @return The token that was read in.
	Token* readToken();

	/// Gets the last token retrieved
	inline Token const* lastToken() const						{ return _last_token; }

	/// Gets the current location of the scanner
	inline Location loc() const									{ return (_sources.size() ? _sources.front()->loc() : Location(NULL, 0, 0)); }

	/// Closes all streams open by the scanner and resets its state
	void close();

private:

	/// Scanner state accessors
	inline char const* marker()									{ return (_sources.size() ? _sources.front()->marker() : NULL); }
	inline char const* cursor()									{ return (_sources.size() ? _sources.front()->cursor() : NULL); }
	inline char const* limit()									{ return (_sources.size() ? _sources.front()->limit()  : NULL); }
	inline char const* token()									{ return (_sources.size() ? _sources.front()->token()  : NULL); }

	/// Called to handle newlines
	inline void newline()										{ if (_sources.size()) _sources.front()->newline(); }


	/// Refills the buffer to the desired level
	/// @param n The minimum number of characters to fill.
	inline void fill(size_t n)									{ if (_sources.size()) _sources.front()->fill(n); }

};

}}}

