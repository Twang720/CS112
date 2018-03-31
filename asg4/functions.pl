% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $

%
% Prolog version of not.
%
not(X) :- X,!,fail.
not(_).

%
% Calculates the distance between two airports in miles.
%
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

%
% Calculates hours, minutes of decimal hours. 
%
hours(X,Y) :- Y is floor(X). 
minutes(X,K) :- hours(X,Z), Y is (X - Z)*60, K is round(Y).

%
% Converts X degrees and M minutes into Rad radians.
%
degree_radians(X,M,Rad) :-
    Rad is (X + (M/60)) * pi/180.

%
% Calculates the time T it takes between two airports X and Y.
%
flight_time( X, Y, T) :-
   airport( X , _ , degmin( D1, T1 ) , degmin( D2, T2 )),
   airport( Y , _ , degmin( D3, T3 ) , degmin( D4, T4 )),
   degree_radians( D1, T1, Lat1 ),
   degree_radians( D2, T2, Lon1 ),
   degree_radians( D3, T3, Lat2 ),
   degree_radians( D4, T4, Lon2 ),
   haversine_radians( Lat1, Lon1, Lat2, Lon2, DelX ),
   T is (DelX/500).

%
% For formatting Time 
%
print_time( H , M) :- 
   print_digit( H ),write( ':' ),print_digit( M ).

print_digit( X ) :- 
   ( X < 10 -> write( 0 ),write( X ) ; write( X )). 

%
% Base case for writepath
%
writepath( [] ) :-
   nl,!.

%
% Writes flight paths to console, with time in hours:minutes.
%
writepath( [flight( Node, Next, time( H, M))| Tail]) :-
   airport( Node, Dept, _, _),
   airport( Next, Arr, _, _),
   nl,write( 'depart ' ),write( Node ),write( ' ' ),write( Dept ),
   write( ' ' ),print_time(H,M),nl,
   flight_time( Node, Next, T),
   Arr_time is T + H + (M/60),
   hours( Arr_time , H_ ),
   minutes( Arr_time, M_ ),
   write('arrive '),write(Next),write( ' ' ),write(Arr),
   write( ' ' ),print_time( H_, M_),
   writepath( Tail ).

%   
% Called by user to write out flight paths.
%
fly( Node, End) :-
   atom( Node ), atom( End ), 
   listpath( Node, End, Outlist),writepath( Outlist ),!. 

%
% Base case for when two airports are the same.
%
fly( Node, Node) :- 
   write('Error! Cannot fly to and from same destination!'),nl,!,fail.

%
% Top level for listpath.
%
listpath( Node, End, [flight( Node, Next, Time)| FlightList] ) :-
   flight( Node, Next, Time),
   listpath( Next, End, [flight( Node, Next, Time)], FlightList).

%
% Base case for list path.
%
listpath( Airport, Airport, _, [] ).

%
% Finds flight path from Node to End, employing DFS type algorithm.
%
listpath( Node, End, 
   [flight( PrevNode1, PrevNode2, time( H1, M1)) | PrevFlights], 
   [flight( Node, NextNode, time( H2, M2))| NextFlights] ) :-
   not(Node = End),
   flight( Node, NextNode, time( H2, M2)),
   flight_time( PrevNode1, PrevNode2, T),
   % calculate arrival time of previous flight to airport
   Arr_time is T + H1 + (M1/60),
   % calculate departure time of next airport 
   Depart_time is H2 + (M2/60),
   % in decimal hours, check if at least 30 minute difference
   Temp is Arr_time + 0.50,
   Temp =< Depart_time,
   % calculate time of next flight and see if less than 24 hours
   flight_time( Node, NextNode, T2),
   T2 + Depart_time < 24.0,
   % append to visited list of flights
   PrevFlights2 = 
   append( [flight( PrevNode1, 
   PrevNode2, time(H1,M1) )],PrevFlights),
   not( member( NextNode, PrevFlights2 )),
   not( NextNode = PrevNode2 ),
   not( PrevNode1 = NextNode),
   listpath( NextNode, End, 
      [flight( Node, NextNode, time(H2,M2) )| PrevFlights2], 
      NextFlights ).
