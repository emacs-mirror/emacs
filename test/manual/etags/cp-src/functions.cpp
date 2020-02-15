#include "main.hpp"
#pragma ident	"@(#)functions.cpp	1.0	98/11/12 (c) Rupak Rathore"

// Constructor default argument initializes to today's values
void Date::setDate ( int d , int m , int y ){
	time_t t;
	struct tm * ptm;
	t = time ( NULL ) ;
	if ( date != NULL )
		delete date;
	date = NULL;
	if ( d == 0 && m == 0 && y == 0 ) //Explicitly called or default constructor hence leave it.
		return;
	if ( d < 0 && m < 0 && d < 0 ) //Special instruction to initialize to today's value
		d=m=y=0;
	date = new tm;
	ptm=localtime ( &t ) ;
	*date=(*ptm);
	if ( d )
		date->tm_mday = d;
	if ( m )
		date->tm_mon = m - 1; // Months are counted from January
	if ( y > 1900 ) // Complete year specified so take into account
		y -= 1900;
	if ( y )
		date->tm_year = y;
	date->tm_sec=date->tm_min=date->tm_hour=0;
	t = mktime ( date ) ;
}

// Addition operation ::: Warning ::: A combination of addition and subtraction does not give a proper result
void Date::plus ( int days , int month , int year ){
	if ( ! set () )
		return;
	date->tm_mday += days ;
	date->tm_mon += month ;
	date->tm_year += year ;
	mktime ( date );
}

//Subtraction operation ::: Warning ::: A combination of addition and subtraction does not give a proper result
void Date::minus ( int days , int month , int year ){
	if ( ! set () )
		return;
	date->tm_mday -= days ;
	date->tm_mon -= month ;
	date->tm_year -= year ;
	mktime ( date );
}


void Date::shift ( void ){//Shift this date to previous working days (useful for benchmarks)
	if ( ! set() )
		return ;
	while(isHoliday(*this)||isweekend()){
		date->tm_mday -= 1 ;
		mktime ( date );
	}
}

// Assignment
Date & Date::operator = ( Date d ){
	if ( d.set() )
		setDate ( d.date->tm_mday, d.date->tm_mon + 1, d.date->tm_year );
	return(*this);
}

// Add number of days
Date & Date::operator += ( int days ){
	if ( set () ){
		date->tm_mday += days ;
		mktime ( date );
	}
	return(*this);
}

// Subtract number of days
Date & Date::operator -= ( int days ){
	if ( set () ){
		date->tm_mday -= days ;
		mktime ( date );
	}
	return(*this);
}

// Advance one day
Date & Date::operator ++ ( void ){
	if ( set () ){
		date->tm_mday += 1 ;
		mktime ( date );
	}
	return(*this);
}

// Backwards one day
Date & Date::operator -- ( void ){
	if ( set () ){
		date->tm_mday -= 1 ;
		mktime ( date );
	}
	return(*this);
}

int Date::operator - ( Date d ){
	long l;
	if (( ! set() ) || (! d.set()))
		return(0);
	l=(mktime(date)-mktime(d.date))/(3600*24);
	return((int)l);
}

int Date::operator < ( Date d ) {
	return ( unidate() < d.unidate() );
}

int Date::operator > ( Date d ) {
	return ( unidate() > d.unidate() );
}

int Date::operator == ( Date d ) {
	return ( unidate() == d.unidate() );
}

ostream& operator <<  ( ostream &c, Date d ) {
	if ( ! d.set() )
		c << "Null";
	else
		c << d.date->tm_mday << ":" << d.date->tm_mon + 1 << ":" << d.date->tm_year + 1900 ;
	return ( c );
}

// Modified to read date in yyyymmdd format.
istream& operator >> ( istream &i, Date & dd ){
	int d,m,y,tmp;
	i >> tmp;
	d=tmp%100;
	tmp/=100;
	m=tmp%100;
	tmp/=100;
	y=tmp;
	dd.setDate(d,m,y);
	return(i);
}
/*
istream& operator >>  ( istream &i, Date &dd ) {
	char input[11];
	int d,m,y;
	cout << "Enter the date ( dd-mm-yyyy ) : ";
	i >> input ;
	d = ( input[0] - '0' ) * 10 + ( input[1] - '0' );
	m = ( input[3] - '0' ) * 10 + ( input[4] - '0' );
	y = ( input[6] - '0' ) * 1000 + ( input[7] - '0' ) * 100 + ( input[8] - '0' ) * 10 + ( input[9] - '0' );
	dd.setDate ( d, m, y );
	return ( i );
}
*/

// Check whether given year is leap or not
bool isLeap ( int year ){
	return ( (year%100==0) ? (year%400==0) : (year%4==0) );
}

bool isHoliday ( Date d ){
	long int ld;
	ld = ( d.year()*100 + d.month() )*100 + d.day();
	for ( int i=0; i<no_of_vacations;i++)
		if ( ld == vacation[i] )
			return(true);
	return(false);
}

// Sort the given array in ascending order
void asort(int *a, int num){
	int i,k,mini,tmp;
	for ( k=1; k<num; k++ ){
		mini=k-1;
		for ( i=k; i<num; i++ )
			if ( a[mini] > a[i] ) {
				tmp=a[i];
				a[i]=a[mini];
				a[mini]=tmp;
			}
	}
}

void ReadVacation ( char *filename ) {
	// cerr << filename;
	ifstream vacfile(filename);
	if ( ! vacfile.good() )
		d_error("ReadVacation","Unable to find the vacation and holidays file");
	// cerr << filename ;
	d_silent("ReadVacation","vacation file successfully opened.");
	no_of_vacations = 0;
	while ( !vacfile.eof() )
		vacfile >> vacation[no_of_vacations++];
	--no_of_vacations;
	d_silent("ReadVacation","Finished Reading file");
	vacfile.close();
}

void Debug ( int lineno, int level, char* func , char* mesg ) // error_level, function, message
{
	if ( debug_level <= level )
		cerr << PROGNAME << ": " << func << ": " << lineno << ": " << debug_string[level] << ": " << mesg << endl;
	if ( level == ERROR ){
		cerr << PROGNAME << ": Exiting because of fatal error." <<endl ;
		exit(2);
	}
}

int WorkingDays(Date a, Date b){
	Date tmp;
	int wdays=0,days=0;
	if ( (! a.set()) || (! b.set()) )
		return(0);
	days=b-a+1; // Inclusive
	tmp=a;
	for ( int i=0;i<days;i++){
		if((!isHoliday(tmp))&&(!tmp.isweekend()))
			wdays++;
		tmp++;
	}
	return(wdays);
}

Date StartDay(Date a,int days){//Function to calculate the apropriate start day to finish in days working days
	Date tmp;
	int wdays=0;
	if ( ! a.set() )
		return (a);
	tmp=a;
	while(wdays<days){
		if((!isHoliday(tmp))&&(!tmp.isweekend()))
			wdays++;
		tmp--;
	}
	tmp++;
	return(tmp);
}
