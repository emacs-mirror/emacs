# The source file contains non-ASCII characters, supposed to be saved
# in UTF-8 encoding.  Tell Perl about that, just in case.
use utf8;

# Following code is the example from the report Bug#22355 which needed
# attention in perl-mode.

printf qq
{<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
 <Document>
  <Folder><name>台灣 %s 廣播電台</name>
  <description><![CDATA[http://radioscanningtw.wikia.com/wiki/台描:地圖 %d-%02d-%02d]]></description>
}, uc( substr( $ARGV[0], 0, 2 ) ), $year + 1900, $mon + 1, $mday;
