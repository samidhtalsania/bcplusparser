#pragma once

#include <sstream>

namespace utils {

/// String conversion
template <typename T>
inline bool fromString(char const* str, T& val) {
	std::istringstream iss(str);
	iss >> std::ws >> val >> std::ws;
	return !iss.fail() && iss.eof();
}


};

