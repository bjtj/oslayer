#ifndef __REGEX_HPP__
#define __REGEX_HPP__

#include <iostream>
#include "os.hpp"
#include "Text.hpp"
#include "Iterator.hpp"
#include "AutoRef.hpp"

namespace osl {

    /**
     * @brief quantity
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
	int const_initial() const;
	int & limit();
	int const_limit() const;
	bool testInRange(int i) const;
	bool testInLimit(int i) const;
	bool testUnderLimit(int i) const;
	std::string toString() const;
    };

    /**
     * @brief position
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
     * @brief match type
     */
    class MatchType {
    public:
	static const int NOT_MATCHED;
	static const int FULL_MATCHED;
	static const int PARTIAL_MATCHED;
	static const std::string NOT_MATCHED_STR;
	static const std::string FULL_MATCHED_STR;
	static const std::string PARTIAL_MATCHED_STR;
	int _type;

    public:
	MatchType();
	MatchType(int type);
	virtual ~MatchType();
	std::string toString() const;
	bool operator== (int type) const;
	bool operator== (const MatchType & other) const;
	bool operator!= (int type) const;
	bool operator!= (const MatchType & other) const;
	static std::string toString(int type);
	static MatchType toType(const std::string & type);
	friend std::ostream & operator<< (std::ostream & stream, const MatchType & type);
    };

    /**
     * @brief match result
     */
    class MatchResult {
    private:
	MatchType _matchType;
	size_t _length;
	std::vector<std::string> _groups;
    public:
	MatchResult();
	virtual ~MatchResult();
	bool fully_matched();
	bool not_matched();
	bool partial_matched();
	MatchType & matchType();
	size_t & length();
	std::vector<std::string> & groups();
	void appendGroup(const std::string & group);
	void appendGroups(const std::vector<std::string> & groups);
    };

    /**
     * @brief matcher
     */
    class Matcher {
    private:
	static bool _debug;
	Matcher * _parent;
	Quantity _quantity;
	bool _reverse;
	bool _any;
	Position _position;
	std::string _charset;
	std::vector< AutoRef<Matcher> > _elements;
	std::vector< AutoRef<Matcher> > _alters;
	bool _group;
    public:
	Matcher();
	virtual ~Matcher();
	Quantity & quantity();
	static bool & debug();
	bool & reverse();
	bool & any();
	Position & position();
	std::string & charset();
	void setParent(Matcher * parent);
	Matcher * getParent();
	std::vector< AutoRef<Matcher> > & elements();
	std::vector< AutoRef<Matcher> > & alters();
	void addChild(AutoRef<Matcher> child);
	bool & group();
	void altering();
	MatchResult match(const std::string & text);
	MatchResult match(Matcher * parent, const std::string & text);
	AutoRef<Matcher> nextMatcher(Matcher * target);
	std::string toString() const;
    };

    /**
     * @brief Range
     */
    class Range {
    private:
	int _start;
	MatchResult _match_result;
    public:
	Range();
	Range(int start, MatchResult match_result);
	virtual ~Range();
	int & start();
	MatchResult & matchResult();
    };

    /**
     * @brief Regex
     */
    class Regex {
    private:
	static bool _debug;
	std::string _regex;
    public:
	Regex();
	Regex(const std::string & regex);
	virtual ~Regex();
	static bool & debug();
	std::string & regex();
	MatchResult match(const std::string & text);
	Range search(const std::string & text);
	AutoRef<Matcher> makeMatcher();
	static AutoRef<Matcher> makeMatcher(const std::string & regex);
	static AutoRef<Matcher> makeMatcher(Iterator<std::string> & tokens_iter);
	static void makeMatcher_r(Iterator<std::string> & tokens_iter, AutoRef<Matcher> parent);
	static std::vector<std::string> tokenize(const std::string & regex);
    };
}

#endif
