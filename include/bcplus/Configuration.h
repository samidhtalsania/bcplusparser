#pragma once
#include <string>
#include <iostream>

#ifndef BOOST_FILESYSTEM_VERSION
#define BOOST_FILESYSTEM_VERSION 3
#endif
#include <boost/filesystem.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/device/null.hpp>

#include "babb/utils/memory.h"
#include "memwrappers.h"

namespace bcplus {

/// Container for option enumeration information
struct Option {
	enum Value {
		SYMTAB_INPUT,		///< Input file to initialize symbol table from.
		SYMTAB_OUTPUT,		///< Output file to save symbol table to.
		VERBOSITY,			///< Specify the application verbosity level.
		HELP,				///< Show help dialog.
		VERSION,			///< Show the application version
		BAD					///< Unknown option.
	};

	enum Status {
		STAT_OK,		///< Everything is a-ok.
		STAT_BAD_ARG,	///< The user provided an option we don't recognize.
		STAT_HELP,		///< The user needs help.
		STAT_VERSION	///< The user wants the application version.
	};

};

/// Container for verbosity levels
struct Verb {

	enum Level {
		ERROR = 0,			/// Error Messages
		WARN = 1,			/// warning messages
		STD = 2,			/// Standard Messages
		OP = 3,				/// Operation-level messages
		DETAIL = 4,			/// Detailed debug Messages
		TRACE = 5			/// Extremely verbose debug messages
	};
};



class Configuration : public babb::utils::Referenced
{
public:
	/***************************************************************************/
	/* Constants */
	/***************************************************************************/

	/// Default verbosity level
	static const size_t DEFAULT_VERB_LEVEL;

private:

	/***************************************************************************/
	/* Private Members */
	/***************************************************************************/
	babb::utils::ref_ptr<const ReferencedString> _version;		///< The version of this application.
	babb::utils::ref_ptr<const ReferencedString> _bin_name;		///< The name of the executable.

	babb::utils::ref_ptr<const ReferencedPath> _symtab_in;		///< Input file (if any) for the symbol table.
	babb::utils::ref_ptr<const ReferencedPath> _symtab_out;		///< Output file (if any) for the symbol table.
	size_t _verb;									///< The user defined verbosity level.	

	boost::iostreams::stream<boost::iostreams::null_sink>	_nullstream;		///< Output stream to a null device in the event the verbosity is squelched.


public:
	/***************************************************************************/
	/* Constructors */
	/***************************************************************************/
	/// Default constructor
	/// Initializes all parameters to defaults.
	/// @param bin The name of the binary
	/// @param version The system version.
	Configuration(char const* bin, char const* version);

	/// Destructor
	virtual ~Configuration();

	/***************************************************************************/
	/* Public Methods */
	/***************************************************************************/
	/// Loads configuration parameters from the provided arguments list.
	/// @param argc The number of arguments.
	/// @param argv An array of these arguments.
	/// @return The status of the load
	Option::Status load(int argc, char const** argv);

	/// Verifies the integrity of the configuration parameters.
	/// @return True if the parameters appear to be correct. False otherwise.
	bool good();

	// -------------------------------------------------------------------------

	/// Get the application version
	inline ReferencedString const* version() const				{ return _version; }

	/// Get the name of this executable file
	inline ReferencedString const* binaryName() const			{ return _bin_name; }

	/// Get/set the input file for the symbol table
	inline ReferencedPath const* symtabInput() const			{ return _symtab_in; }
	inline void symtabInput(ReferencedPath const* input)		{ _symtab_in = input; }

	/// Get/set the output file for the symbol table
	inline ReferencedPath const* symtabOutput() const			{ return _symtab_out; }
	inline void symtabOutput(ReferencedPath const* output)		{ _symtab_out = output; }
	
	/// Get/set the application verbosity level
	inline size_t verbosity() const								{ return _verb; }
	inline void verbosity(size_t verb)							{ _verb = verb; }

	/// Get the output stream for the provided verbosity level
	/// This will return a null-sink, std::cerr, or std::cout.
	std::ostream& ostream(Verb::Level v);

	// --------------------------------------------------------------------------
	/// Outputs the current configuration to a stream.
	/// @param out The output stream.
	/// @return The output stream.
	std::ostream& output(std::ostream& out) const;

	/// Outputs the application version information.
	/// @param out The output stream to write to.
	/// @return out
	std::ostream& outputVersion(std::ostream& out) const;

	/// Outputs a help dialog to the provided output stream.
	/// @param out The output stream to write the help dialog to.
	/// @return out
	std::ostream& outputHelp(std::ostream& out) const;

private:
	/***************************************************************************/
	/* Private Methods */
	/***************************************************************************/
	/// Parses the string and determines the command line option it contains.
	/// @param opt The option string to parse.
	/// @param[out] val The value the option is set to, where applicable.
	/// @return The option contained within the string or BAD.
	Option::Value parseOption(char const* opt, char const*& val);

};

};
