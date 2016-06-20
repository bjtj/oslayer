#ifndef __REGEX_HPP__
#define __REGEX_HPP__

#include "Iterator.hpp"

namespace UTIL {

	/**
	 * @brief 
	 */
	class Quantity {
		bool _occured;
		int _initial;
		int _limit;
	public:
		Quantity();
		virtual ~Quantity();
		bool & occured();
		int & initial();
		int & limit();
		std::string toString();
	};

	/**
	 * @brief 
	 */
	class Position {
	private:
		bool _start_of_line;
		bool _end_of_line;
	public:
		Position();
		virtual ~Position();
		bool & start_of_line();
		bool & end_of_line();
	};

	/**
	 * @brief 
	 */
	class MatchResult {
	private:
		bool _matched;
		size_t _length;
		std::vector<std::string> _group;
	public:
		MatchResult();
		virtual ~MatchResult();
		bool & matched();
		size_t & length();
		std::vector<std::string> & group();
	};

	/**
	 * @brief 
	 */
	class Matcher {
	private:
		Quantity _quantity;
		bool _reverse;
		bool _any;
		Position _position;
		std::string _charset;
		std::vector<Matcher> _elements;
		std::vector<Matcher> _alters;
	public:
		Matcher();
		virtual ~Matcher();
		Quantity & quantity();
		bool & reverse();
		bool & any();
		Position & position();
		std::string & charset();
		std::vector<Matcher> & elements();
		std::vector<Matcher> & alters();
		void altering();	
		MatchResult match(const std::string & text);
		std::string toString();
	};

	/**
	 * @brief 
	 */
	class Regex {
	public:
		Regex();
		virtual ~Regex();
		std::vector<std::string> tokenize(const std::string & regex);
		void makeMatcher(Iterator<std::string> & tokens_iter, Matcher & parent);
	};
}

#endif
