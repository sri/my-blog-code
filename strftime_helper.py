# Strftime helper -- cause I can't ever remember the
#  control chars
"""
From http://docs.python.org/lib/module-time.html

%a  	Locale's abbreviated weekday name.  	
%A 	Locale's full weekday name. 	
%b 	Locale's abbreviated month name. 	
%B 	Locale's full month name. 	
%c 	Locale's appropriate date and time representation. 	
%d 	Day of the month as a decimal number [01,31]. 	
%H 	Hour (24-hour clock) as a decimal number [00,23]. 	
%I 	Hour (12-hour clock) as a decimal number [01,12]. 	
%j 	Day of the year as a decimal number [001,366]. 	
%m 	Month as a decimal number [01,12]. 	
%M 	Minute as a decimal number [00,59]. 	
%p 	Locale's equivalent of either AM or PM. 	        (1)
%S 	Second as a decimal number [00,61]. 	                (2)
%U 	Week number of the year (Sunday as the first day of the week)
        as a decimal number [00,53]. All days in a new year preceding
        the first Sunday are considered to be in week 0. 	(3)
%w 	Weekday as a decimal number [0(Sunday),6]. 	
%W 	Week number of the year (Monday as the first day of the week)
        as a decimal number [00,53]. All days in a new year preceding
        the first Monday are considered to be in week 0. 	(3)
%x 	Locale's appropriate date representation. 	
%X 	Locale's appropriate time representation. 	
%y 	Year without century as a decimal number [00,99]. 	
%Y 	Year with century as a decimal number. 	
%Z 	Time zone name (no characters if no time zone exists). 	
%% 	A literal "%" character. 	

Notes:

(1)
    When used with the strptime() function, the %p directive only affects
    the output hour field if the %I directive is used to parse the hour. 
(2)
    The range really is 0 to 61; this accounts for leap seconds and the
    (very rare) double leap seconds. 
(3)
    When used with the strptime() function, %U and %W are only used in
    calculations when the day of the week and the year are specified. 

"""

class StrftimeHelper(Cmd):
    localedt      = 'localedt'
    day_of_month  = 'dom'
    hour24        = 'hh24'
    hour          = 'hh'
    day_of_year   = 'doy'
    minutes       = 'mm'            
    ampm1         = 'am'
    ampm2         = 'pm'
    seconds       = 'ss'
    week_of_yearS = 'swoy'
    day_of_week   = 'dow'
    week_of_yearM = 'mwoy'
    localedate    = 'localed'
    localetime    = 'localet'
    yearnocentury = 'yy'
    yearcentury   = 'yyyy'
    timezone      = 'tz'
    fullweekday   = ('monday', 'tuesday', 'wednesday', 'thursday',
                     'friday', 'saturday', 'sunday')
    abbrevweekday = tuple(x[:3] for x in fullweekday)
    fullmonth     = ('january', 'feburary', 'march', 'april', 'may',
                     'june', 'july', 'august', 'september',
                     'october', 'november', 'december')
    abbrevmonth   = tuple(x[:3] for x in fullmonth)


    def install(self, specialcmds, simplecmds):
        simplecmds['pd'] = self.help

    def help(self):
        print self.pdhelpstring
        while True:
            try:
                s = raw_input('? ').strip()
                if not s:
                    break
                __builtins__._ = self.help1(s)
            except EOFError:
                print
                break

    def help1(self, s, dt=None):
        t = s.strip().lower()
        t = t.replace(self.yearcentury,   '%Y') # must be before yearnocentury
        t = t.replace(self.yearnocentury, '%y')
        t = t.replace(self.timezone,      '%Z')
        t = t.replace(self.localetime,    '%X')
        t = t.replace(self.localedt,      '%c') # must be before localedate
        t = t.replace(self.localedate,    '%x')
        t = t.replace(self.week_of_yearM, '%W')
        t = t.replace(self.day_of_week,   '%w')
        t = t.replace(self.week_of_yearS, '%U')
        t = t.replace(self.seconds,       '%S')
        t = t.replace(self.ampm2,         '%p')
        t = t.replace(self.ampm1,         '%p')
        t = t.replace(self.minutes,       '%M')
        t = t.replace(self.day_of_year,   '%j')
        t = t.replace(self.hour24,        '%H') # must be before hour
        t = t.replace(self.hour,          '%I')
        t = t.replace(self.day_of_month,  '%d')

        for x in self.fullmonth:
            t = t.replace(x, '%B')
        for x in self.abbrevmonth:
            t = t.replace(x, '%b')
        for x in self.fullweekday:
            t = t.replace(x, '%A')
        for x in self.abbrevweekday:
            t = t.replace(x, '%a')

        if not dt:
            dt = datetime.datetime.now()
        dt = dt.replace(microsecond=0)

        print '%s formats to\n%s\n%s' % (
            dt,
            dt.strftime(t),
            repr(t))

        return t
        


    pdhelpstring = """\
===============================================================================
= pd help =

localet, localed, localedt  - Locale Time, Locale Date, Locale Date & Time
hh24, hh, mm, ss, am, pm    - 24-Hour, Hour, Minute, Second, Am, Pm
dow, dom, doy               - Day Of {Week, Month, & Year} respectively
swoy, mwoy                  - Week Of Year considering either Sunday or Monday
                                as first day of week
yy, yyyy                    - Year without century & Year with century
tz                          - Timezone
mon..sun, monday..sunday    - Weekday in Abbrev or Long form
jan..dec, january..december - Month in Abbrev or Long form
"""

# this is not used
"""Example
=======
>>> pd('jun dom, yyyy')
2007-06-14 06:13:14 formats to
Jun 14, 2007
'%b %d, %Y'
==============================================================================="""
