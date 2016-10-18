#include "Date.hpp"

namespace OS {

	using namespace std;
	
#if defined(USE_MS_WIN)

	static Date s_systemtime_to_date(SYSTEMTIME t) {
		Date date;
		date.setYear(t.wYear);
		date.setMonth(t.wMonth - 1);
		date.setDay(t.wDay);
		date.setDayOfWeek(t.wDayOfWeek);
		date.setHour(t.wHour);
		date.setMinute(t.wMinute);
		date.setSecond(t.wSecond);
		date.setMillisecond(t.wMilliseconds);
		return date;
	}

	static SYSTEMTIME s_filetime_to_systemtime(FILETIME ftime) {
		SYSTEMTIME stime;
		FILETIME localtime;
		FileTimeToLocalFileTime(&ftime, &localtime);
		FileTimeToSystemTime(&localtime, &stime);
		return stime;
	}

	/**
	 * @ref https://msdn.microsoft.com/ko-kr/library/windows/desktop/ms724284(v=vs.85).aspx
	 */
	static osl_time_t s_filetime_to_osl_time(const FILETIME * ft) {
		osl_time_t t = {0,};
		ULARGE_INTEGER time;
		time.LowPart = ft->dwLowDateTime;
		time.HighPart = ft->dwHighDateTime;
		unsigned __int64 uit = time.QuadPart;
		t.sec = (uint64_t)(uit / 10000000);
		t.nano = (unsigned long)(uit % 10000000) * 100;

		return t;
	}

	/**
	 * @ref https://support.microsoft.com/ko-kr/kb/167296
	 */
	static FILETIME s_osl_time_to_filetime(const osl_time_t * ot) {
		unsigned __int64 uit = 0;
		uit = ((unsigned __int64)ot->sec) * 10000000ULL;
		uit += ((unsigned __int64)ot->nano) / 100ULL;
		FILETIME t = {0,};
		t.dwLowDateTime = (DWORD)uit;
		t.dwHighDateTime= (DWORD)(uit >> 32);
		return t;
	}

#endif

	/**
	 * @brief 
	 */
	osl_time_t osl_get_time() {
		
#if defined(USE_APPLE_STD)
        
        // @ref http://stackoverflow.com/a/11681069

		osl_time_t ti = {0,};
        
        clock_serv_t cclock;
        mach_timespec_t mts;
        host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
        clock_get_time(cclock, &mts);
        mach_port_deallocate(mach_task_self(), cclock);

		ti.sec = mts.tv_sec;
		ti.nano = mts.tv_nsec;

		return ti;
        
#elif defined(USE_POSIX_STD)

		osl_time_t ti = {0,};
        
        struct timespec spec;
        clock_gettime(CLOCK_REALTIME, &spec);
		
		ti.sec = spec.tv_sec;
		ti.nano = spec.tv_nsec;

		return ti;
        
#elif defined(USE_MS_WIN)

		FILETIME ft;
		GetSystemTimeAsFileTime(&ft);

		return s_filetime_to_osl_time(&ft);
#else
		throw Exception("Not implemented");
#endif
	}

	/**
	 * @brief Jan. 1, 1970 based
	 */
	osl_time_t osl_get_time_unix() {
		osl_time_t ti = osl_get_time();

#if defined(USE_MS_WIN)
		// 11644473600 seconds (1601-01-01 ~ 1970-01-01)
		// @ref http://stackoverflow.com/a/6161842
		ti.sec -= (uint64_t)11644473600ULL;
#endif

		return ti;
	}

	/**
	 * @brief Jan. 1, 1900 based
	 */
	osl_time_t osl_get_time_network() {
		osl_time_t ti = osl_get_time_unix();
		// 2208988800 seconds (1900-01-01 ~ 1970-01-01)
		// @ref http://stackoverflow.com/a/29138806
		ti.sec += 2208988800ULL;
		return ti;
	}

	osl_time_t osl_system_time_to_unix_time(osl_time_t t) {
#if defined(USE_MS_WIN)
		// 11644473600 seconds (1601-01-01 ~ 1970-01-01)
		// @ref http://stackoverflow.com/a/6161842
		t.sec -= (uint64_t)11644473600ULL;
#endif
		return t;
	}

	osl_time_t osl_system_time_to_network_time(osl_time_t t) {
		t = osl_system_time_to_unix_time(t);
		// 2208988800 seconds (1900-01-01 ~ 1970-01-01)
		// @ref http://stackoverflow.com/a/29138806
		t.sec += 2208988800ULL;
		return t;
	}

	osl_time_t osl_unix_time_to_system_time(osl_time_t t) {
#if defined(USE_MS_WIN)
		// 11644473600 seconds (1601-01-01 ~ 1970-01-01)
		// @ref http://stackoverflow.com/a/6161842
		t.sec += (uint64_t)11644473600ULL;
#endif
		return t;
	}

	osl_time_t osl_network_time_to_system_time(osl_time_t t) {
		// 2208988800 seconds (1900-01-01 ~ 1970-01-01)
		// @ref http://stackoverflow.com/a/29138806
		t.sec -= 2208988800ULL;
		
		return osl_unix_time_to_system_time(t);
	}

	/**
	 * @return gmtoffset in minute
	 */
	static int s_get_gmt_offset() {
#if defined(USE_APPLE_STD) || defined(USE_POSIX_STD)
		struct tm info;
		time_t t = 0;
		localtime_r(&t, &info);
		time_t local = mktime(&info);
		gmtime_r(&t, &info);
		time_t gmt = mktime(&info);
		double offset = difftime(local, gmt);
		return (int)(offset / 60);
#elif defined(USE_MS_WIN)
		// http://stackoverflow.com/a/597562
		TIME_ZONE_INFORMATION TimeZoneInfo;
		GetTimeZoneInformation(&TimeZoneInfo);
		return -((int)TimeZoneInfo.Bias);
#else
		throw Exception("Not implemented");
#endif
	}

	static Date s_get_localtime() {
		Date date;

#if defined(USE_APPLE_STD)
        
        // @ref http://stackoverflow.com/a/11681069
        
        clock_serv_t cclock;
        mach_timespec_t mts;
        host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
        clock_get_time(cclock, &mts);
        mach_port_deallocate(mach_task_self(), cclock);
        time_t t = (time_t)mts.tv_sec;
        struct tm info;
        localtime_r(&t, &info);
        date.setYear(1900 + info.tm_year);
        date.setMonth(info.tm_mon);
        date.setDay(info.tm_mday);
		date.setDayOfWeek(info.tm_wday);
        date.setHour(info.tm_hour);
        date.setMinute(info.tm_min);
        date.setSecond(info.tm_sec);
        date.setMillisecond(mts.tv_nsec / 1000000);

		date.setGmtOffset(s_get_gmt_offset());
        
#elif defined(USE_POSIX_STD)
        
        struct timespec spec;
        clock_gettime(CLOCK_REALTIME, &spec);
        time_t t = (time_t)spec.tv_sec;
        struct tm info;
        localtime_r(&t, &info);
        date.setYear(1900 + info.tm_year);
        date.setMonth(info.tm_mon);
        date.setDay(info.tm_mday);
		date.setDayOfWeek(info.tm_wday);
        date.setHour(info.tm_hour);
        date.setMinute(info.tm_min);
        date.setSecond(info.tm_sec);
        date.setMillisecond(spec.tv_nsec / 1000000);

		date.setGmtOffset(s_get_gmt_offset());
        
#elif defined(USE_MS_WIN)
        
        SYSTEMTIME now;
        GetLocalTime(&now);
		date.setYear(now.wYear);
		date.setMonth(now.wMonth - 1);
		date.setDay(now.wDay);
		date.setDayOfWeek(now.wDayOfWeek);
		date.setHour(now.wHour);
		date.setMinute(now.wMinute);
		date.setSecond(now.wSecond);
		date.setMillisecond(now.wMilliseconds);

		date.setGmtOffset(s_get_gmt_offset());
        
#endif
		return date;
	}

	// http://www.cplusplus.com/reference/ctime/strftime/

	string Date::DEFAULT_FORMAT = "%Y-%c-%d %H:%i:%s";

	Date::Date()
		: gmtoffset(0), year(0), month(0), day(0), wday(0),
		  hour(0), minute(0), second(0), millisecond(0) {
	}

	Date::Date(struct tm & info)
		: gmtoffset(0), year(0), month(0), day(0), wday(0),
		  hour(0), minute(0), second(0), millisecond(0)
	{

		setYear(1900 + info.tm_year);
        setMonth(info.tm_mon);
        setDay(info.tm_mday);
		setDayOfWeek(info.tm_wday);
        setHour(info.tm_hour);
        setMinute(info.tm_min);
        setSecond(info.tm_sec);
	}

	Date::Date(osl_time_t time)
		: gmtoffset(0), year(0), month(0), day(0), wday(0),
		  hour(0), minute(0), second(0), millisecond(0) 
	{
		setTime(time);
	}

	Date::Date(osl_time_t time, int gmtoffset)
		: gmtoffset(0), year(0), month(0), day(0), wday(0),
		  hour(0), minute(0), second(0), millisecond(0)
	{
		setTime(time, gmtoffset);
	}

#if defined(USE_MS_WIN)
	Date::Date(const FILETIME ft)
		: gmtoffset(0), year(0), month(0), day(0), wday(0),
		  hour(0), minute(0), second(0), millisecond(0)
	{
		*this = s_systemtime_to_date(s_filetime_to_systemtime(ft);
	}
#endif

	Date::~Date() {
	}
	
	Date Date::now() {		
		return s_get_localtime();
	}

	static string s_to_string(int i) {
		char num[512] = {0,};
        snprintf(num, sizeof(num), "%d", i);
        return string(num);
	}

	static string s_to_format_string(const char * fmt, int i) {
		char num[512] = {0,};
        snprintf(num, sizeof(num), fmt, i);
        return string(num);
	}

	string Date::format(const Date & date) {
		return format(date, DEFAULT_FORMAT);
	}

	/**
	 * @brief seconds to string
	 * @ref http://stackoverflow.com/questions/10446526/get-last-modified-time-of-file-in-linux
	 * @ref http://www.cplusplus.com/reference/ctime/strftime/
	 */
    string Date::format(const Date & date, const string & fmt) {
		if (fmt.empty()) {
			return "";
		}
		string ret;
		for (size_t i = 0; i < fmt.size(); i++) {
			char ch = fmt[i];
			if (ch == '%') {
				if (i >= fmt.size() - 1) {
					ret.append(1, '%');
					break;
				}
				i++;
				switch (fmt[i]) {
				case 'Y':
					ret.append(s_to_format_string("%04d", date.getYear()));
					break;
				case 'y':
					ret.append(s_to_format_string("%02d", date.getYear() % 100));
					break;
				case 'c':
					ret.append(s_to_format_string("%02d", date.getMonth() + 1));
					break;
				case 'e':
					ret.append(s_to_string(date.getMonth()));
					break;
				case 'd':
					ret.append(s_to_format_string("%02d", date.getDay()));
					break;
				case 'H':
					ret.append(s_to_format_string("%02d", date.getHour()));
					break;
				case 'h':
					ret.append(s_to_format_string("%02d", date.getHour() % 12));
					break;
				case 'i':
					ret.append(s_to_format_string("%02d", date.getMinute()));
					break;
				case 's':
					ret.append(s_to_format_string("%02d", date.getSecond()));
					break;
				case 'f':
					ret.append(s_to_string(date.getMillisecond()));
					break;
				case 'p':
					ret.append(date.getHour() >= 12 ? "PM" : "AM");
					break;
				default:
					ret.append(1, fmt[i]);
					break;
				}
			} else {
				ret.append(1, ch);
			}
		}
		return ret;
    }
	
	string Date::formatRfc1123(const Date & date) {
		static const char * wkday[] = {"Sun", "Mon", "Tue",
									   "Wed", "Thu", "Fri", "Sat"};
		static const char * month[] = {"Jan", "Feb", "Mar",
									   "Apr", "May", "Jun",
									   "Jul", "Aug", "Sep",
									   "Oct", "Nov", "Dec"};
		Date gmt = date.toGmt();
		char buffer[64] = {0,};
		snprintf(buffer, sizeof(buffer), "%s, %02d %s %04d %02d:%02d:%02d GMT",
				 wkday[gmt.getDayOfWeek()],
				 gmt.getDay(),
				 month[gmt.getMonth()],
				 gmt.getYear(),
				 gmt.getHour(),
				 gmt.getMinute(),
				 gmt.getSecond());
		return string(buffer);
	}

	
	string Date::formatRfc1036(const Date & date) {
		// TODO: implement
		throw NotImplementedException("Not implemented");
	}

	int Date::getSystemGmtOffset() {
		return now().getGmtOffset();
	}
	Date Date::toGmt(const Date & from) {
		if (from.getGmtOffset() == 0) {
			return from;
		}
		
#if defined(USE_MS_WIN)
		int defaultOffset = Date::getSystemGmtOffset();
		osl_time_t t = from.getTime();
		t.sec -= (defaultOffset * 60);
		return Date(t, 0);
#else
		int defaultOffset = Date::getSystemGmtOffset();
		int offset = from.getGmtOffset();
		osl_time_t time = from.getTime();
		uint64_t seconds = time.sec;
		time_t x = (time_t)(seconds + ((defaultOffset - offset) * 60));
		struct tm info;
		gmtime_r(&x, &info);
		Date date = Date(info);
		date.setMillisecond(from.getMillisecond());
		return date;
#endif
	}
	Date Date::toGmt() const {
		return toGmt(*this);
	}
	void Date::setGmtOffset(int gmtoffset) {
		this->gmtoffset = gmtoffset;
	}
	void Date::setTimezone(const string & timezone) {
		this->timezone = timezone;
	}
	void Date::setYear(int year) {
		this->year = year;
	}
	void Date::setMonth(int month) {
		this->month = month;
	}
	void Date::setDay(int day) {
		this->day = day;
	}
	void Date::setDayOfWeek(int wday) {
		this->wday = wday;
	}
	void Date::setHour(int hour) {
		this->hour = hour;
	}
	void Date::setMinute(int minute) {
		this->minute = minute;
	}
	void Date::setSecond(int second) {
		this->second = second;
	}
	void Date::setMillisecond(int millisecond) {
		this->millisecond = millisecond;
	}
	int Date::getGmtOffset() const {
		return gmtoffset;
	}
	string Date::getTimezone() const {
		return timezone;
	}
	int Date::getYear() const {
		return year;
	}
	int Date::getMonth() const {
		return month;
	}
	int Date::getDay() const {
		return day;
	}
	int Date::getDayOfWeek() const {
		return wday;
	}
	int Date::getHour() const {
		return hour;
	}
	int Date::getMinute() const {
		return minute;
	}
	int Date::getSecond() const {
		return second;
	}
	int Date::getMillisecond() const {
		return millisecond;
	}
	osl_time_t Date::getTime() const {

#if defined(USE_MS_WIN)

		SYSTEMTIME st = {0,};
		FILETIME ft = {0,};
		
		st.wYear = year;
		st.wMonth = month + 1;
		st.wDay = day;
		st.wHour = hour;
		st.wMinute = minute;
		st.wSecond = second;
		st.wMilliseconds = millisecond;

		SystemTimeToFileTime(&st, &ft);
		osl_time_t ret = s_filetime_to_osl_time(&ft);
		ret.sec -= (this->gmtoffset * 60);
		return ret;

#else
		struct tm info = {0,};
		info.tm_year = year - 1900;
		info.tm_mon = month;
		info.tm_mday = day;
		info.tm_hour = hour;
		info.tm_min = minute;
		info.tm_sec = second;
		time_t t = mktime(&info);
		time_t base = {0,};
		double seconds = difftime(t, base);
		osl_time_t ret;
		ret.sec = (uint64_t)seconds;
		ret.nano = millisecond * (1000 * 1000);
		return ret;
#endif
	}

	void Date::setTime(const osl_time_t t) {
		setTime(t, s_get_gmt_offset());
	}

	void Date::setTime(const osl_time_t time, int gmtoffset) {
#if defined(USE_MS_WIN)
		*this = s_systemtime_to_date(s_filetime_to_systemtime(s_osl_time_to_filetime(&time)));
		setGmtOffset(gmtoffset);
#else
		time_t t = (time_t)time.sec;
        struct tm info;
		localtime_r(&t, &info);
        setYear(1900 + info.tm_year);
        setMonth(info.tm_mon);
        setDay(info.tm_mday);
		setDayOfWeek(info.tm_wday);
        setHour(info.tm_hour);
        setMinute(info.tm_min);
        setSecond(info.tm_sec);
		setMillisecond((int)(time.nano / (1000 * 1000)));
		setGmtOffset(gmtoffset);
#endif
	}

	Date Date::toDate(osl_time_t time) {
		return Date(time);
	}
	
}
