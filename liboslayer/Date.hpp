#ifndef __DATE_HPP__
#define __DATE_HPP__

#include "os.hpp"

namespace osl {

	/**
	 * @brief 
	 */
	typedef struct _osl_time_t {
		uint64_t sec;
		uint64_t nano;

		bool operator< (const struct _osl_time_t & other) const;
		bool operator<= (const struct _osl_time_t & other) const;
		bool operator> (const struct _osl_time_t & other) const;
		bool operator>= (const struct _osl_time_t & other) const;
		bool operator== (const struct _osl_time_t & other) const;
		bool operator!= (const struct _osl_time_t & other) const;
		struct _osl_time_t operator+ (const struct _osl_time_t & other) const;
		struct _osl_time_t operator- (const struct _osl_time_t & other) const;
		struct _osl_time_t & operator+= (const struct _osl_time_t & other);
		struct _osl_time_t & operator-= (const struct _osl_time_t & other);
		
	} osl_time_t;

	/**
	 * @brief 
	 */
	osl_time_t osl_get_time();
	osl_time_t osl_get_time_unix();
	osl_time_t osl_get_time_network();
	osl_time_t osl_system_time_to_unix_time(osl_time_t t);
	osl_time_t osl_system_time_to_network_time(osl_time_t t);
	osl_time_t osl_unix_time_to_system_time(osl_time_t t);
	osl_time_t osl_network_time_to_system_time(osl_time_t t);

	/**
	 * @brief date
	 */
	class Date {
	private:
	public:
		static std::string DEFAULT_FORMAT;
		int gmtoffset;
		std::string timezone;
		int year;
		int month;
		int day;
		int wday;
		int hour;
		int minute;
		int second;
		int millisecond;
	public:
		Date();
		Date(struct tm & info);
		Date(osl_time_t time);
		Date(osl_time_t time, int gmtoffset);
#if defined(USE_MS_WIN)
		Date(const FILETIME ft);
		Date(const SYSTEMTIME st);
#endif
		virtual ~Date();
		static Date now();
		static std::string format(const Date & date);
        static std::string format(const Date & date, const std::string & fmt);
		static std::string formatRfc8601(const Date & date);
		static Date parseRfc8601(const std::string & date);
		static std::string formatRfc1123(const Date & date);
		static Date parseRfc1123(const std::string & date);
		static std::string formatRfc1036(const Date & date);
		static Date parseRfc1036(const std::string & date);
		static int getSystemGmtOffset();
		static Date toGmt(const Date & from);
		Date toGmt() const;
		void setGmtOffset(int gmtoffset);
		void setTimezone(const std::string & timezone);
		void setYear(int year);
		void setMonth(int month);
		void setDay(int day);
		void setDayOfWeek(int wday);
		void setHour(int hour);
		void setMinute(int minute);
		void setSecond(int second);
		void setMillisecond(int millisecond);
		int getGmtOffset() const;
		std::string getTimezone() const;
		int getYear() const;
		int getMonth() const;
		int getDay() const;
		int getDayOfWeek() const;
		int getHour() const;
		int getMinute() const;
		int getSecond() const;
		int getMillisecond() const;
		osl_time_t getTime() const;
		void setTime(const osl_time_t t);
		void setTime(const osl_time_t t, int gmtoffset);
		static Date toDate(osl_time_t time);
		bool operator< (const Date & other) const;
		bool operator<= (const Date & other) const;
		bool operator> (const Date & other) const;
		bool operator>= (const Date & other) const;
		bool operator== (const Date & other) const;
		bool operator!= (const Date & other) const;
		Date operator+ (const Date & other) const;
		Date operator- (const Date & other) const;
		Date & operator+= (const Date & other);
		Date & operator-= (const Date & other);
	};
}

#endif
