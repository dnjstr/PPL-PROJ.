/* ========================================================================
 * EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM
 * Logic Paradigm - GNU Prolog Implementation
 * 
 * File: event_scheduling.pl
 * To run in GNU Prolog:
 *   1. Compile: gplc event_scheduling.pl
 *   2. Run: ./event_scheduling
 * Or interactively:
 *   1. Load: gprolog --consult-file event_scheduling.pl
 *   2. Query: | ?- run_scheduling_system.
 * ======================================================================== */

/* FACTS - Knowledge base representing events */
/* event(Title, StartHour, StartMin, EndHour, EndMin, Location, Resource, Description) */

event('Math Seminar', 9, 0, 10, 30, 'Room 201', 'Prof. A', 'Linear Algebra Review').
event('CS Department Meeting', 10, 0, 11, 0, 'Room 201', 'Prof. B, Prof. C', 
      'Curriculum planning discussions').
event('Project Review Session', 9, 45, 10, 15, 'Room 101', 'Prof. A', 
      'Student capstone reviews').
event('Lab Equipment Maintenance', 11, 0, 12, 0, 'Computer Lab', 'Technician Joe', 
      'Monthly maintenance checks').

/* ========================================================================
 * UTILITY PREDICATES - GNU Prolog compatible
 * ======================================================================== */

/* Check if substring exists in string (GNU Prolog compatible) */
contains_substring(String, Substring) :-
    atom_codes(String, StringCodes),
    atom_codes(Substring, SubCodes),
    append(_, Rest, StringCodes),
    append(SubCodes, _, Rest), !.

/* Remove duplicates from list */
remove_duplicates([], []).
remove_duplicates([H|T], Result) :-
    member(H, T), !,
    remove_duplicates(T, Result).
remove_duplicates([H|T], [H|Result]) :-
    remove_duplicates(T, Result).

/* ========================================================================
 * RULES - Logical relationships and inference
 * ======================================================================== */

/* Convert time to minutes since midnight */
time_to_minutes(Hour, Min, Minutes) :-
    Minutes is Hour * 60 + Min.

/* Check if two time intervals overlap */
time_overlaps(StartH1, StartM1, EndH1, EndM1, StartH2, StartM2, EndH2, EndM2) :-
    time_to_minutes(StartH1, StartM1, Start1),
    time_to_minutes(EndH1, EndM1, End1),
    time_to_minutes(StartH2, StartM2, Start2),
    time_to_minutes(EndH2, EndM2, End2),
    Start1 < End2,
    End1 > Start2.

/* Rule: Two events have a location conflict */
location_conflict(Event1, Event2) :-
    event(Title1, SH1, SM1, EH1, EM1, Loc, Res1, Desc1),
    event(Title2, SH2, SM2, EH2, EM2, Loc, Res2, Desc2),
    Title1 \= Title2,
    time_overlaps(SH1, SM1, EH1, EM1, SH2, SM2, EH2, EM2),
    Event1 = event(Title1, SH1, SM1, EH1, EM1, Loc, Res1, Desc1),
    Event2 = event(Title2, SH2, SM2, EH2, EM2, Loc, Res2, Desc2).

/* Rule: Two events have a resource conflict */
resource_conflict(Event1, Event2) :-
    event(Title1, SH1, SM1, EH1, EM1, Loc1, Res, Desc1),
    event(Title2, SH2, SM2, EH2, EM2, Loc2, Res, Desc2),
    Title1 \= Title2,
    Loc1 \= Loc2,  /* Different locations to avoid duplicate conflicts */
    time_overlaps(SH1, SM1, EH1, EM1, SH2, SM2, EH2, EM2),
    Event1 = event(Title1, SH1, SM1, EH1, EM1, Loc1, Res, Desc1),
    Event2 = event(Title2, SH2, SM2, EH2, EM2, Loc2, Res, Desc2).

/* Rule: An event has any conflict */
has_conflict(EventTitle) :-
    event(EventTitle, SH, SM, EH, EM, Loc, Res, Desc),
    (   location_conflict(event(EventTitle, SH, SM, EH, EM, Loc, Res, Desc), _)
    ;   resource_conflict(event(EventTitle, SH, SM, EH, EM, Loc, Res, Desc), _)
    ;   location_conflict(_, event(EventTitle, SH, SM, EH, EM, Loc, Res, Desc))
    ;   resource_conflict(_, event(EventTitle, SH, SM, EH, EM, Loc, Res, Desc))
    ), !.

/* Rule: Events involving a specific resource */
events_by_resource(Resource, EventTitle) :-
    event(EventTitle, _, _, _, _, _, ResourceList, _),
    contains_substring(ResourceList, Resource).

/* Rule: Count total events */
count_events(Count) :-
    findall(T, event(T, _, _, _, _, _, _, _), Events),
    length(Events, Count).

/* Rule: Count conflicting events */
count_conflicts(Count) :-
    findall(T, has_conflict(T), Conflicts),
    remove_duplicates(Conflicts, UniqueConflicts),
    length(UniqueConflicts, Count).

/* ========================================================================
 * QUERIES AND REPORTING PREDICATES
 * ======================================================================== */

/* Print formatted time (HH:MM format) - GNU Prolog compatible */
print_time(Hour, Min) :-
    (Hour < 10 -> write('0') ; true),
    write(Hour),
    write(':'),
    (Min < 10 -> write('0') ; true),
    write(Min).

/* Print a single event with all details */
print_event(Title, SH, SM, EH, EM, Loc, Res, Desc) :-
    print_time(SH, SM),
    write(' - '),
    print_time(EH, EM),
    write(': '), write(Title),
    write(' ('), write(Loc), write(')'), nl,
    write('  Resource: '), write(Res), nl,
    (has_conflict(Title) -> 
        write('  Status: Conflicting\n') ; 
        write('  Status: Successfully Scheduled\n')),
    write('  Description: '), write(Desc), nl, nl.

/* Print all events in chronological order */
print_chronological_schedule :-
    nl, write('========================================'), nl,
    write('CHRONOLOGICAL SCHEDULE DISPLAY'), nl,
    write('========================================'), nl, nl,
    
    count_events(TotalCount),
    count_conflicts(ConflictCount),
    write('Summary: '), write(TotalCount), write(' total events, '),
    write(ConflictCount), write(' events affected'), nl, nl,
    write('Event List (Temporal Order):'), nl, nl,
    
    /* Collect and sort all events by start time */
    findall([SH, SM, Title, EH, EM, Loc, Res, Desc],
            event(Title, SH, SM, EH, EM, Loc, Res, Desc),
            Events),
    sort(Events, SortedEvents),
    
    /* Print each event in sorted order */
    print_all_events(SortedEvents).

/* Helper to print all events from sorted list */
print_all_events([]).
print_all_events([[SH, SM, Title, EH, EM, Loc, Res, Desc]|Rest]) :-
    print_event(Title, SH, SM, EH, EM, Loc, Res, Desc),
    print_all_events(Rest).

/* Print conflict report */
print_conflict_report :-
    nl, write('========================================'), nl,
    write('CONFLICT DETECTION AND RESOLUTION REPORT'), nl,
    write('========================================'), nl, nl,
    
    print_location_conflicts,
    print_resource_conflicts.

/* Print all location conflicts */
print_location_conflicts :-
    findall(
        conflict(T1, SH1, SM1, EH1, EM1, Loc, T2, SH2, SM2, EH2, EM2),
        (location_conflict(
            event(T1, SH1, SM1, EH1, EM1, Loc, _, _),
            event(T2, SH2, SM2, EH2, EM2, Loc, _, _)),
         T1 @< T2),
        LocationConflicts
    ),
    print_location_conflicts_numbered(LocationConflicts, 1).

/* Helper predicate to print location conflicts with numbering */
print_location_conflicts_numbered([], _).
print_location_conflicts_numbered([conflict(T1, SH1, SM1, EH1, EM1, Loc, T2, SH2, SM2, EH2, EM2)|Rest], Num) :-
    write('Conflict '), write(Num), write(': Location Overlap'), nl,
    write('Type: Location Double-Booking'), nl,
    write('Location: '), write(Loc), nl,
    write('Conflicting Events:'), nl,
    write('  - '), write(T1), write(' ('),
    print_time(SH1, SM1), write(' - '),
    print_time(EH1, EM1), write(')'), nl,
    write('  - '), write(T2), write(' ('),
    print_time(SH2, SM2), write(' - '),
    print_time(EH2, EM2), write(')'), nl,
    write('Resolution Guidance:'), nl,
    write('  - Relocate one event to an available room'), nl,
    write('  - Adjust event times to avoid overlap'), nl, nl,
    NextNum is Num + 1,
    print_location_conflicts_numbered(Rest, NextNum).

/* Print all resource conflicts */
print_resource_conflicts :-
    findall(
        conflict(T1, SH1, SM1, EH1, EM1, L1, Res, T2, SH2, SM2, EH2, EM2, L2),
        (resource_conflict(
            event(T1, SH1, SM1, EH1, EM1, L1, Res, _),
            event(T2, SH2, SM2, EH2, EM2, L2, Res, _)),
         T1 @< T2),
        ResourceConflicts
    ),
    print_resource_conflicts_list(ResourceConflicts).

/* Helper predicate to print resource conflicts */
print_resource_conflicts_list([]).
print_resource_conflicts_list([conflict(T1, SH1, SM1, EH1, EM1, L1, Res, T2, SH2, SM2, EH2, EM2, L2)|Rest]) :-
    write('Conflict: Resource Overlap'), nl,
    write('Type: Resource Double-Booking'), nl,
    write('Resource: '), write(Res), nl,
    write('Conflicting Events:'), nl,
    write('  - '), write(T1), write(' ('),
    print_time(SH1, SM1), write(' - '),
    print_time(EH1, EM1), write(') in '), write(L1), nl,
    write('  - '), write(T2), write(' ('),
    print_time(SH2, SM2), write(' - '),
    print_time(EH2, EM2), write(') in '), write(L2), nl,
    write('Resolution Guidance:'), nl,
    write('  - Assign an alternate resource'), nl,
    write('  - Reschedule one of the events'), nl, nl,
    print_resource_conflicts_list(Rest).

/* Print filtered view for a specific resource */
print_filtered_view(Resource) :-
    nl, write('========================================'), nl,
    write('FILTERED VIEW: '), write(Resource), nl,
    write('========================================'), nl, nl,
    
    findall(Title, events_by_resource(Resource, Title), EventTitles),
    length(EventTitles, EventCount),
    
    (EventCount = 0 ->
        write('No events found for this resource.'), nl
    ;
        print_resource_events(Resource),
        
        findall(T, (events_by_resource(Resource, T), has_conflict(T)), Conflicts),
        length(Conflicts, ConflictCount),
        write('Summary: '), write(EventCount), write(' events found, '),
        write(ConflictCount), write(' conflicts'), nl,
        
        (ConflictCount = EventCount, EventCount > 1 ->
            write('WARNING: 100% conflict rate - severe over-scheduling detected!'), nl,
            write('Recommendation: Implement buffer time between events.'), nl
        ; true)
    ).

/* Helper to print events for a resource */
print_resource_events(Resource) :-
    events_by_resource(Resource, Title),
    event(Title, SH, SM, EH, EM, Loc, _, _),
    print_time(SH, SM), write(' - '),
    print_time(EH, EM), write(': '),
    write(Title), write(' ('), write(Loc), write(')'), nl,
    (has_conflict(Title) ->
        write('  Status: Conflicting') ;
        write('  Status: No Conflict')),
    nl, nl,
    fail.
print_resource_events(_).

/* ========================================================================
 * MAIN EXECUTION PREDICATE
 * ======================================================================== */

/* Main predicate to run all reports */
run_scheduling_system :-
    write('EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM'), nl,
    write('Logic Paradigm Implementation (Prolog)'), nl,
    write('==============================================='), nl,
    
    print_conflict_report,
    print_chronological_schedule,
    print_filtered_view('Prof. A').

/* Entry point for compiled executable */
:- initialization(run_scheduling_system).

/* ========================================================================
 * END OF GNU PROLOG IMPLEMENTATION
 * ========================================================================*/