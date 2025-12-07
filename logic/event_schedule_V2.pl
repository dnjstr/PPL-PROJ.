/*
 * EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM
 * Logic Programming Paradigm - GNU Prolog Implementation
 * All warnings and errors fixed
 */

:- dynamic(event/7).
:- dynamic(next_id/1).
:- dynamic(exit_requested/0).

% Seed events
event(1, 'Database Design Lecture', time(9,0), time(10,30), 'Room 101', 'Prof. Smith', 'Introduction to database normalization').
event(2, 'Web Development Lab', time(10,0), time(11,30), 'Lab A', 'Prof. Johnson', 'HTML/CSS/JavaScript fundamentals').
event(3, 'Data Structures Seminar', time(11,0), time(12,30), 'Room 201', 'Prof. Williams', 'Advanced tree and graph algorithms').
event(4, 'Database Design Practical', time(10,30), time(12,0), 'Room 101', 'Prof. Smith', 'Hands-on database design exercise').
event(5, 'Algorithms Workshop', time(13,0), time(14,30), 'Lab B', 'Prof. Brown', 'Algorithm optimization techniques').

/* Initialize next ID */
next_id(6).

/*
 * UTILITY PREDICATES
 */

/* Convert atom to number */
my_atom_number(Atom, Number) :-
    atom_codes(Atom, Codes),
    number_codes(Number, Codes).

/* String to lowercase - NO SINGLETON WARNINGS */
to_lower(Input, Lower) :-
    atom_codes(Input, Codes),
    to_lower_codes(Codes, LowerCodes),
    atom_codes(Lower, LowerCodes).

to_lower_codes([], []).
to_lower_codes([Code|Rest], [LCode|LRest]) :-
    (Code >= 65, Code =< 90 ->
        LCode is Code + 32
    ;
        LCode = Code
    ),
    to_lower_codes(Rest, LRest).

/*
 * CORE PREDICATES
 */

/* Convert time to minutes */
time_to_minutes(time(Hour, Minute), Minutes) :-
    Minutes is Hour * 60 + Minute.

/* Check if two time ranges overlap */
time_overlaps(Start1, End1, Start2, End2) :-
    time_to_minutes(Start1, S1),
    time_to_minutes(End1, E1),
    time_to_minutes(Start2, S2),
    time_to_minutes(End2, E2),
    S1 < E2,
    E1 > S2.

/* Check if two events have time overlap */
events_overlap(ID1, ID2) :-
    event(ID1, _, Start1, End1, _, _, _),
    event(ID2, _, Start2, End2, _, _, _),
    ID1 \= ID2,
    time_overlaps(Start1, End1, Start2, End2).

/* Location conflict */
location_conflict(ID1, ID2, Location) :-
    event(ID1, _, _, _, Location, _, _),
    event(ID2, _, _, _, Location, _, _),
    events_overlap(ID1, ID2).

/* Resource conflict */
resource_conflict(ID1, ID2, Resource1) :-
    event(ID1, _, _, _, _, Resource1, _),
    event(ID2, _, _, _, _, Resource2, _),
    events_overlap(ID1, ID2),
    (sub_atom(Resource1, _, _, _, Resource2);
     sub_atom(Resource2, _, _, _, Resource1)).

/* Check if event has conflict */
has_conflict(ID) :-
    (location_conflict(ID, _, _);
     resource_conflict(ID, _, _)), !.

/* Find all conflicts */
find_all_conflicts(Conflicts) :-
    findall(conflict(location, ID1, ID2, Loc),
            (location_conflict(ID1, ID2, Loc), ID1 < ID2),
            LocationConflicts),
    findall(conflict(resource, ID1, ID2, Res),
            (resource_conflict(ID1, ID2, Res), 
             ID1 < ID2,
             \+ location_conflict(ID1, ID2, _)),
            ResourceConflicts),
    append(LocationConflicts, ResourceConflicts, Conflicts).

/*
 * EVENT MANAGEMENT
 */

/* Add event - FIX SINGLETON WARNINGS */
add_event(Title, StartTime, EndTime, Location, Resource, Description) :-
    next_id(ID),
    assertz(event(ID, Title, StartTime, EndTime, Location, Resource, Description)),
    NewID is ID + 1,
    retract(next_id(ID)),
    assertz(next_id(NewID)),
    nl, write('Event added successfully!'), nl,
    write('Event [ID:'), write(ID), write(']: '),
    format_time(StartTime),
    write(' - '),
    format_time(EndTime),
    write(': '), write(Title), write(' ('), write(Location), write(')'), nl, nl,
    (has_conflict(ID) ->
        write('WARNING: This event has conflicts!'), nl,
        write('Use option 3 to see details.'), nl, nl
    ;
        true
    ).

/* Delete event */
delete_event_confirmed(ID) :-
    event(ID, _, _, _, _, _, _), !,
    retract(event(ID, _, _, _, _, _, _)),
    nl, write('Event deleted successfully!'), nl, nl.

delete_event_confirmed(_) :-
    nl, write('Event not found!'), nl, nl.

/* Edit predicates - FIX SINGLETON WARNINGS */
edit_event_title(ID, NewTitle) :-
    event(ID, OldTitle, Start, End, Location, Resource, Description),
    retract(event(ID, OldTitle, Start, End, Location, Resource, Description)),
    assertz(event(ID, NewTitle, Start, End, Location, Resource, Description)).

edit_event_time(ID, NewStart, NewEnd) :-
    event(ID, Title, OldStart, OldEnd, Location, Resource, Description),
    retract(event(ID, Title, OldStart, OldEnd, Location, Resource, Description)),
    assertz(event(ID, Title, NewStart, NewEnd, Location, Resource, Description)).

edit_event_location(ID, NewLocation) :-
    event(ID, Title, Start, End, OldLocation, Resource, Description),
    retract(event(ID, Title, Start, End, OldLocation, Resource, Description)),
    assertz(event(ID, Title, Start, End, NewLocation, Resource, Description)).

edit_event_resource(ID, NewResource) :-
    event(ID, Title, Start, End, Location, OldResource, Description),
    retract(event(ID, Title, Start, End, Location, OldResource, Description)),
    assertz(event(ID, Title, Start, End, Location, NewResource, Description)).

edit_event_description(ID, NewDescription) :-
    event(ID, Title, Start, End, Location, Resource, OldDescription),
    retract(event(ID, Title, Start, End, Location, Resource, OldDescription)),
    assertz(event(ID, Title, Start, End, Location, Resource, NewDescription)).

edit_event_all(ID, NewTitle, NewStart, NewEnd, NewLocation, NewResource, NewDescription) :-
    retract(event(ID, _, _, _, _, _, _)),
    assertz(event(ID, NewTitle, NewStart, NewEnd, NewLocation, NewResource, NewDescription)).

/*
 * DISPLAY PREDICATES
 */

/* Format time */
format_time(time(Hour, Minute)) :-
    (Hour < 10 -> write('0') ; true), write(Hour),
    write(':'),
    (Minute < 10 -> write('0') ; true), write(Minute).

/* Display event */
display_event(ID) :-
    event(ID, Title, Start, End, Location, Resource, Description),
    write('[ID:'), write(ID), write('] '),
    format_time(Start),
    write(' - '),
    format_time(End),
    write(': '), write(Title), write(' ('), write(Location), write(')'), nl,
    write('  Resource: '), write(Resource), nl,
    write('  Status: '),
    (has_conflict(ID) -> write('CONFLICTING') ; write('OK')),
    nl, write('  Description: '), write(Description), nl, nl.

/* List all events */
list_all_events :-
    nl, write('========================================'), nl,
    write('ALL EVENTS'), nl,
    write('========================================'), nl, nl,
    (event(_, _, _, _, _, _, _) ->
        findall(ID, event(ID, _, _, _, _, _, _), IDs),
        display_all_events(IDs)
    ;
        write('No events scheduled.'), nl, nl
    ).

display_all_events([]).
display_all_events([ID|Rest]) :-
    display_event(ID),
    display_all_events(Rest).

/* List conflicting events */
list_conflicting_events :-
    nl, write('========================================'), nl,
    write('CONFLICTING EVENTS'), nl,
    write('========================================'), nl, nl,
    findall(ID, (event(ID, _, _, _, _, _, _), has_conflict(ID)), ConflictIDs),
    (ConflictIDs = [] ->
        write('No conflicting events.'), nl, nl
    ;
        display_conflicting_list(ConflictIDs),
        length(ConflictIDs, Count),
        write('Total conflicting events: '), write(Count), nl, nl
    ).

display_conflicting_list([]).
display_conflicting_list([ID|Rest]) :-
    display_event_brief(ID),
    display_conflicting_list(Rest).

display_event_brief(ID) :-
    event(ID, Title, Start, End, Location, Resource, Description),
    write('[ID:'), write(ID), write('] '),
    format_time(Start),
    write(' - '),
    format_time(End),
    write(': '), write(Title), write(' ('), write(Location), write(')'), nl,
    write('  Resource: '), write(Resource), nl,
    write('  Description: '), write(Description), nl, nl.

/* Print conflict report */
print_conflict_report :-
    nl, write('========================================'), nl,
    write('CONFLICT DETECTION REPORT'), nl,
    write('========================================'), nl, nl,
    find_all_conflicts(Conflicts),
    (Conflicts = [] ->
        write('No conflicts detected.'), nl, nl
    ;
        print_conflicts(Conflicts, 1)
    ).

print_conflicts([], _).
print_conflicts([conflict(Type, ID1, ID2, Resource)|Rest], Num) :-
    nl, write('Conflict '), write(Num), write(': '),
    (Type = location ->
        write('Location Overlap'), nl,
        write('Type: Location Double-Booking'), nl,
        write('Location: '), write(Resource), nl
    ;
        write('Resource Overlap'), nl,
        write('Type: Resource Double-Booking'), nl,
        write('Resource: '), write(Resource), nl
    ),
    write('Conflicting Events:'), nl,
    event(ID1, Title1, Start1, End1, Loc1, _, _),
    event(ID2, Title2, Start2, End2, Loc2, _, _),
    write('  - [ID:'), write(ID1), write('] '), write(Title1), write(' ('),
    format_time(Start1), write(' - '), format_time(End1), write(')'),
    (Type = resource -> (write(' in '), write(Loc1)) ; true), nl,
    write('  - [ID:'), write(ID2), write('] '), write(Title2), write(' ('),
    format_time(Start2), write(' - '), format_time(End2), write(')'),
    (Type = resource -> (write(' in '), write(Loc2)) ; true), nl,
    write('Resolution Guidance:'), nl,
    (Type = location ->
        (write('  - Relocate one event'), nl,
         write('  - Adjust event times'), nl, nl)
    ;
        (write('  - Assign alternate resource'), nl,
         write('  - Reschedule one event'), nl, nl)
    ),
    NextNum is Num + 1,
    print_conflicts(Rest, NextNum).

/* Chronological schedule */
print_chronological_schedule :-
    nl, write('========================================'), nl,
    write('CHRONOLOGICAL SCHEDULE'), nl,
    write('========================================'), nl, nl,
    findall(event(ID, Title, Start, End, Location, Resource, Description),
            event(ID, Title, Start, End, Location, Resource, Description),
            Events),
    sort_events_by_time(Events, SortedEvents),
    findall(ID, has_conflict(ID), ConflictIDs),
    length(Events, TotalEvents),
    length(ConflictIDs, ConflictCount),
    find_all_conflicts(AllConflicts),
    length(AllConflicts, NumConflicts),
    write('Summary: '), write(TotalEvents), write(' total events, '),
    write(NumConflicts), write(' conflicts, '),
    write(ConflictCount), write(' events affected'), nl, nl,
    write('Event List (Temporal Order):'), nl, nl,
    display_sorted_events(SortedEvents).

/* Sort events */
sort_events_by_time(Events, Sorted) :-
    bubble_sort(Events, Sorted).

bubble_sort(List, Sorted) :-
    swap_events(List, List1), !,
    bubble_sort(List1, Sorted).
bubble_sort(Sorted, Sorted).

swap_events([event(ID1, T1, time(H1, M1), E1, L1, R1, D1),
             event(ID2, T2, time(H2, M2), E2, L2, R2, D2) | Rest],
            [event(ID2, T2, time(H2, M2), E2, L2, R2, D2),
             event(ID1, T1, time(H1, M1), E1, L1, R1, D1) | Rest]) :-
    Min1 is H1 * 60 + M1,
    Min2 is H2 * 60 + M2,
    Min1 > Min2.
swap_events([E | Rest], [E | Rest1]) :-
    swap_events(Rest, Rest1).

display_sorted_events([]).
display_sorted_events([event(ID, Title, Start, End, Location, Resource, Description)|Rest]) :-
    write('[ID:'), write(ID), write('] '),
    format_time(Start), write(' - '), format_time(End),
    write(': '), write(Title), write(' ('), write(Location), write(')'), nl,
    write('  Resource: '), write(Resource), nl,
    write('  Status: '),
    (has_conflict(ID) -> write('Conflicting') ; write('Successfully Scheduled')),
    nl, write('  Description: '), write(Description), nl, nl,
    display_sorted_events(Rest).

/* Filter by resource - FIX SINGLETON WARNING */
print_filtered_view(ResourceName) :-
    nl, write('========================================'), nl,
    write('FILTERED VIEW: '), write(ResourceName), nl,
    write('========================================'), nl, nl,
    findall(ID, (event(ID, _, _, _, _, Resource, _),
                 sub_atom(Resource, _, _, _, ResourceName)), FilteredIDs),
    (FilteredIDs = [] ->
        write('No events found.'), nl
    ;
        display_filtered_events(FilteredIDs),
        length(FilteredIDs, Count),
        findall(ConflictID, (member(ConflictID, FilteredIDs), has_conflict(ConflictID)), ConflictIDs),
        length(ConflictIDs, ConflictCount),
        write('Summary: '), write(Count), write(' events, '),
        write(ConflictCount), write(' conflicts'), nl
    ).

display_filtered_events([]).
display_filtered_events([ID|Rest]) :-
    event(ID, Title, Start, End, Location, _, _),
    write('[ID:'), write(ID), write('] '),
    format_time(Start), write(' - '), format_time(End),
    write(': '), write(Title), write(' ('), write(Location), write(')'), nl,
    write('  Status: '),
    (has_conflict(ID) -> write('Conflicting') ; write('No Conflict')),
    nl, nl,
    display_filtered_events(Rest).

/*
 * USER INTERACTION
 */

/* Read input */
read_line(Input) :-
    read(Input).

/* Parse time HH:MM */
parse_time_string(TimeStr, time(Hour, Minute)) :-
    atom_codes(TimeStr, Codes),
    append(HourCodes, [58|MinCodes], Codes),
    number_codes(Hour, HourCodes),
    number_codes(Minute, MinCodes),
    Hour >= 0, Hour =< 23,
    Minute >= 0, Minute =< 59.

/* Display menu */
display_menu :-
    nl, write('========================================'), nl,
    write('MAIN MENU'), nl,
    write('========================================'), nl,
    write('1. Add New Event'), nl,
    write('2. View All Events'), nl,
    write('3. Detect Conflicts'), nl,
    write('4. View Conflicting Events'), nl,
    write('5. Edit Event'), nl,
    write('6. Delete Event'), nl,
    write('7. Chronological Schedule'), nl,
    write('8. Filter by Resource'), nl,
    write('9. Exit'), nl,
    write('========================================'), nl.

/* Add event from input */
add_event_from_input :-
    nl, write('========================================'), nl,
    write('ADD NEW EVENT'), nl,
    write('========================================'), nl,
    write('Event Title: '), read_line(Title),
    write('Start Time (HH:MM): '), read_line(StartStr),
    write('End Time (HH:MM): '), read_line(EndStr),
    (parse_time_string(StartStr, Start), parse_time_string(EndStr, End),
     time_to_minutes(Start, SM), time_to_minutes(End, EM), SM < EM ->
        write('Location: '), read_line(Location),
        write('Resource: '), read_line(Resource),
        write('Description: '), read_line(Description),
        add_event(Title, Start, End, Location, Resource, Description)
    ;
        nl, write('Error: Invalid time format.'), nl, nl
    ).

/* Edit event - FIX SINGLETON WARNINGS */
edit_event_interface :-
    nl, write('========================================'), nl,
    write('EDIT EVENT'), nl,
    write('========================================'), nl,
    list_all_events,
    write('Enter Event ID: '), read_line(ID),
    (integer(ID), event(ID, Title, Start, End, Location, Resource, Description) ->
        nl, write('Current: '),
        format_time(Start), write(' - '), format_time(End),
        write(': '), write(Title), nl,
        write('Location: '), write(Location), nl,
        write('Resource: '), write(Resource), nl,
        write('Description: '), write(Description), nl, nl,
        write('1. Title / 2. Time / 3. Location / 4. Resource / 5. Description / 6. All / 7. Cancel'), nl,
        write('Choice: '), read_line(Choice),
        process_edit_choice(ID, Choice)
    ;
        nl, write('Event not found!'), nl, nl
    ).

process_edit_choice(_, 7) :- 
    nl, write('Cancelled.'), nl, nl, !.

process_edit_choice(ID, 1) :-
    write('New Title: '), read_line(NewTitle),
    edit_event_title(ID, NewTitle),
    show_update_message(ID).

process_edit_choice(ID, 2) :-
    write('New Start (HH:MM): '), read_line(StartStr),
    write('New End (HH:MM): '), read_line(EndStr),
    (parse_time_string(StartStr, Start), parse_time_string(EndStr, End) ->
        edit_event_time(ID, Start, End),
        show_update_message(ID)
    ;
        nl, write('Invalid time.'), nl, nl
    ).

process_edit_choice(ID, 3) :-
    write('New Location: '), read_line(NewLocation),
    edit_event_location(ID, NewLocation),
    show_update_message(ID).

process_edit_choice(ID, 4) :-
    write('New Resource: '), read_line(NewResource),
    edit_event_resource(ID, NewResource),
    show_update_message(ID).

process_edit_choice(ID, 5) :-
    write('New Description: '), read_line(NewDescription),
    edit_event_description(ID, NewDescription),
    show_update_message(ID).

process_edit_choice(ID, 6) :-
    write('New Title: '), read_line(NewTitle),
    write('Start (HH:MM): '), read_line(StartStr),
    write('End (HH:MM): '), read_line(EndStr),
    (parse_time_string(StartStr, Start), parse_time_string(EndStr, End) ->
        write('Location: '), read_line(NewLocation),
        write('Resource: '), read_line(NewResource),
        write('Description: '), read_line(NewDescription),
        edit_event_all(ID, NewTitle, Start, End, NewLocation, NewResource, NewDescription),
        show_update_message(ID)
    ;
        nl, write('Invalid time.'), nl, nl
    ).

process_edit_choice(_, _) :-
    nl, write('Invalid choice.'), nl, nl.

show_update_message(ID) :-
    nl, write('Updated successfully!'), nl,
    (has_conflict(ID) ->
        write('WARNING: Still has conflicts!'), nl, nl
    ;
        write('No conflicts.'), nl, nl
    ).

/* Delete event - FIX SINGLETON WARNINGS */
delete_event_interface :-
    nl, write('========================================'), nl,
    write('DELETE EVENT'), nl,
    write('========================================'), nl,
    list_all_events,
    write('Enter Event ID: '), read_line(ID),
    (integer(ID), event(ID, Title, Start, End, Location, _Resource, _Description) ->
        nl, write('Delete: ['), write(ID), write('] '),
        format_time(Start), write(' - '), format_time(End),
        write(': '), write(Title), write(' ('), write(Location), write(')'), nl,
        write('Confirm (yes/no): '), read_line(Confirm),
        to_lower(Confirm, ConfirmLower),
        (ConfirmLower = yes ; ConfirmLower = y ->
            delete_event_confirmed(ID)
        ;
            nl, write('Cancelled.'), nl, nl
        )
    ;
        nl, write('Event not found!'), nl, nl
    ).

/* Process menu - NO HALT, USE FLAG */
process_choice(1) :- add_event_from_input, !.
process_choice(2) :- 
    (event(_, _, _, _, _, _, _) -> list_all_events
    ; (nl, write('No events yet.'), nl, nl)), !.
process_choice(3) :-
    (event(_, _, _, _, _, _, _) -> print_conflict_report
    ; (nl, write('No events yet.'), nl, nl)), !.
process_choice(4) :-
    (event(_, _, _, _, _, _, _) -> list_conflicting_events
    ; (nl, write('No events yet.'), nl, nl)), !.
process_choice(5) :-
    (event(_, _, _, _, _, _, _) -> edit_event_interface
    ; (nl, write('No events yet.'), nl, nl)), !.
process_choice(6) :-
    (event(_, _, _, _, _, _, _) -> delete_event_interface
    ; (nl, write('No events yet.'), nl, nl)), !.
process_choice(7) :-
    (event(_, _, _, _, _, _, _) -> print_chronological_schedule
    ; (nl, write('No events yet.'), nl, nl)), !.
process_choice(8) :-
    (event(_, _, _, _, _, _, _) ->
        (nl, write('Resource name: '), read_line(ResourceName),
         print_filtered_view(ResourceName))
    ; (nl, write('No events yet.'), nl, nl)), !.
process_choice(9) :-
    nl, write('Thank you! Program will exit.'), nl,
    assertz(exit_requested), !.
process_choice(_) :-
    nl, write('Invalid. Choose 1-9.'), nl, nl, !.

/* Main loop - CHECK EXIT FLAG */
main_loop :-
    exit_requested, !.
main_loop :-
    display_menu,
    write('Choice: '),
    read_line(Input),
    (integer(Input) -> Choice = Input ; Choice = 0),
    process_choice(Choice),
    main_loop.

/* Start system */
run_scheduling_system :-
    nl,
    write('==============================================='), nl,
    write('EVENT SCHEDULING SYSTEM'), nl,
    write('Logic Programming - GNU Prolog'), nl,
    write('==============================================='), nl,
    write('Sample data: 5 events loaded'), nl,
    main_loop.

/* Entry point */
:- initialization(run_scheduling_system).