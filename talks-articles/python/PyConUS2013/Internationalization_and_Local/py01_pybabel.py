#!/usr/bin/env python
# pre-req: pip install babel, pytz  # though talk says pybabel, I can just find babel
# http://babel.edgewall.org/wiki/Documentation/dates.html

from babel import dates, numbers, Locale
from datetime import date, datetime, time
from pytz import timezone, utc

dt = datetime(2007, 04, 01, 15, 30, tzinfo=utc) #date(2007, 4, 1)
tz = timezone('US/Eastern')
print dates.format_datetime(dt, format='full', locale='de', tzinfo=tz)

print numbers.format_decimal(1.2345, locale='de')

locale = Locale('es')
month_names = locale.months['format']['wide'].items()
month_names.sort()
for idx, name in month_names:
  print name
