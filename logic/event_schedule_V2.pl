/*
 * EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM
 * Logic Programming Paradigm - GNU Prolog Implementation with Edit/Delete
 * 
 * File: event_schedule.pl
 * Compile: gplc event_schedule.pl
 * Run: ./event_schedule
 */

:- dynamic(event/7).
:- dynamic(next_id/1).

/* Initialize next ID */
next_id(1).

/*
 * UTILITY PREDICATES
 */

/* Convert atom to number */
my_atom_number(Atom, Number) :-
    atom_chars(Atom, Chars),
    number_chars(Number, Chars).

/* String to lowercase */
to_lower(Input, Lower) :-
    atom_chars(Input, Chars),
    to_lower_chars(Chars, LowerChars),
    atom_chars(Lower, LowerChars).

to_lower_chars([], []).
to_lower_chars([C|Rest], [LC|LRest]) :-
    (C >= 'A', C =< 'Z' ->
        char_code(C, Code),
        LCode is Code + 32,
        char_code(LC, LCode)
    ;
        LC = C
    ),
    to_lower_chars(Rest, LRest).

/*
 * CORE PREDICATES - Logic Programming Approach
 */

/* Convert time to minutes for comparison */
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

/* Location conflict: same location and overlapping time */
location_conflict(ID1, ID2, Location) :-
    event(ID1, _, _, _, Location, _, _),
    event(ID2, _, _, _, Location, _, _),
    events_overlap(ID1, ID2).

/* Resource conflict: overlapping resource and time */
resource_conflict(ID1, ID2, Resource1) :-
    event(ID1, _, _, _, _, Resource1, _),
    event(ID2, _, _, _, _, Resource2, _),
    events_overlap(ID1, ID2),
    (atom_concat(_, Resource2, Resource1);
     atom_concat(_, Resource1, Resource2)).

/* Check if an event has any conflict */
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
 * EVENT MANAGEMENT PREDICATES
 */

/* Add a new event */
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
        write('WARNING: This event has conflicts with existing events!'), nl,
        write('Use option 3 to see conflict details.'), nl, nl
    ;
        true
    ).

/* Delete an event by ID */
delete_event_confirmed(ID) :-
    event(ID, _, _, _, _, _, _),
    retract(event(ID, _, _, _, _, _, _)),
    nl, write('Event deleted successfully!'), nl, nl,
    write('Conflicts have been re-evaluated.'), nl, nl.

delete_event_confirmed(ID) :-
    \+ event(ID, _, _, _, _, _, _),
    nl, write('Event not found!'), nl, nl.

/* Edit event - update title */
edit_event_title(ID, NewTitle) :-
    event(ID, _, Start, End, Location, Resource, Description),
    retract(event(ID, _, Start, End, Location, Resource, Description)),
    assertz(event(ID, NewTitle, Start, End, Location, Resource, Description)).

/* Edit event - update time */
edit_event_time(ID, NewStart, NewEnd) :-
    event(ID, Title, _, _, Location, Resource, Description),
    retract(event(ID, Title, _, _, Location, Resource, Description)),
    assertz(event(ID, Title, NewStart, NewEnd, Location, Resource, Description)).

/* Edit event - update location */
edit_event_location(ID, NewLocation) :-
    event(ID, Title, Start, End, _, Resource, Description),
    retract(event(ID, Title, Start, End, _, Resource, Description)),
    assertz(event(ID, Title, Start, End, NewLocation, Resource, Description)).

/* Edit event - update resource */
edit_event_resource(ID, NewResource) :-
    event(ID, Title, Start, End, Location, _, Description),
    retract(event(ID, Title, Start, End, Location, _, Description)),
    assertz(event(ID, Title, Start, End, Location, NewResource, Description)).

/* Edit event - update description */
edit_event_description(ID, NewDescription) :-
    event(ID, Title, Start, End, Location, Resource, _),
    retract(event(ID, Title, Start, End, Location, Resource, _)),
    assertz(event(ID, Title, Start, End, Location, Resource, NewDescription)).

/* Edit all fields */
edit_event_all(ID, NewTitle, NewStart, NewEnd, NewLocation, NewResource, NewDescription) :-
    retract(event(ID, _, _, _, _, _, _)),
    assertz(event(ID, NewTitle, NewStart, NewEnd, NewLocation, NewResource, NewDescription)).

/*
 * DISPLAY PREDICATES
 */

/* Format time for display */
format_time(time(Hour, Minute)) :-
    (Hour < 10 -> write('0') ; true), write(Hour),
    write(':'),
    (Minute < 10 -> write('0') ; true), write(Minute).

/* Display a single event */
display_event(ID) :-
    event(ID, Title, Start, End, Location, Resource, Description),
    write('[ID:'), write(ID), write('] '),
    format_time(Start),
    write(' - '),
    format_time(End),
    write(': '), write(Title), write(' ('), write(Location), write(')'), nl,
    write('  Resource: '), write(Resource), nl,
    write('  Status: '),
    (has_conflict(ID) ->
        write('CONFLICTING')
    ;
        write('OK')
    ),
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

/* List only conflicting events */
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

/* Display event briefly */
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
    write('CONFLICT DETECTION AND RESOLUTION REPORT'), nl,
    write('========================================'), nl, nl,
    find_all_conflicts(Conflicts),
    (Conflicts = [] ->
        write('No conflicts detected.'), nl, nl
    ;
        print_conflicts(Conflicts, 1)
    ).

/* Print individual conflicts */
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
    format_time(Start1),
    write(' - '),
    format_time(End1),
    write(')'),
    (Type = resource ->
        write(' in '), write(Loc1)
    ;
        true
    ),
    nl,
    write('  - [ID:'), write(ID2), write('] '), write(Title2), write(' ('),
    format_time(Start2),
    write(' - '),
    format_time(End2),
    write(')'),
    (Type = resource ->
        write(' in '), write(Loc2)
    ;
        true
    ),
    nl,
    write('Resolution Guidance:'), nl,
    (Type = location ->
        write('  - Relocate one event to an available room'), nl,
        write('  - Adjust event times to avoid overlap'), nl, nl
    ;
        write('  - Assign an alternate resource'), nl,
        write('  - Reschedule one of the events'), nl, nl
    ),
    NextNum is Num + 1,
    print_conflicts(Rest, NextNum).

/* Print chronological schedule */
print_chronological_schedule :-
    nl, write('========================================'), nl,
    write('CHRONOLOGICAL SCHEDULE DISPLAY'), nl,
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
    write(NumConflicts), write(' conflicts detected, '),
    write(ConflictCount), write(' events affected'), nl, nl,
    write('Event List (Temporal Order):'), nl, nl,
    display_sorted_events(SortedEvents).

/* Sort events by start time - simple bubble sort */
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

/* Display sorted events */
display_sorted_events([]).
display_sorted_events([event(ID, Title, Start, End, Location, Resource, Description)|Rest]) :-
    write('[ID:'), write(ID), write('] '),
    format_time(Start),
    write(' - '),
    format_time(End),
    write(': '), write(Title), write(' ('), write(Location), write(')'), nl,
    write('  Resource: '), write(Resource), nl,
    write('  Status: '),
    (has_conflict(ID) ->
        write('Conflicting')
    ;
        write('Successfully Scheduled')
    ),
    nl, write('  Description: '), write(Description), nl, nl,
    display_sorted_events(Rest).

/* Filter events by resource */
print_filtered_view(ResourceName) :-
    nl, write('========================================'), nl,
    write('FILTERED VIEW: '), write(ResourceName), nl,
    write('========================================'), nl, nl,
    findall(ID, (event(ID, _, _, _, _, Resource, _),
                 atom_concat(_, ResourceName, Resource)), FilteredIDs),
    (FilteredIDs = [] ->
        write('No events found for this resource.'), nl
    ;
        display_filtered_events(FilteredIDs),
        length(FilteredIDs, Count),
        findall(ID, (member(ID, FilteredIDs), has_conflict(ID)), ConflictIDs),
        length(ConflictIDs, ConflictCount),
        write('Summary: '), write(Count), write(' events found, '),
        write(ConflictCount), write(' conflicts'), nl,
        (Count > 1, ConflictCount = Count ->
            write('WARNING: 100% conflict rate - severe over-scheduling detected!'), nl,
            write('Recommendation: Implement buffer time between events.'), nl
        ;
            true
        )
    ).

display_filtered_events([]).
display_filtered_events([ID|Rest]) :-
    event(ID, Title, Start, End, Location, _, _),
    write('[ID:'), write(ID), write('] '),
    format_time(Start),
    write(' - '),
    format_time(End),
    write(': '), write(Title), write(' ('), write(Location), write(')'), nl,
    write('  Status: '),
    (has_conflict(ID) ->
        write('Conflicting')
    ;
        write('No Conflict')
    ),
    nl, nl,
    display_filtered_events(Rest).

/*
 * USER INTERACTION PREDICATES
 */

/* Display menu */
display_menu :-
    write('========================================'), nl,
    write('MAIN MENU'), nl,
    write('========================================'), nl,
    write('1. Add New Event'), nl,
    write('2. View All Events'), nl,
    write('3. Detect and Display Conflicts'), nl,
    write('4. View Conflicting Events Only'), nl,
    write('5. Edit Event'), nl,
    write('6. Delete Event'), nl,
    write('7. Display Chronological Schedule'), nl,
    write('8. Filter Events by Resource'), nl,
    write('9. Exit'), nl,
    write('========================================'), nl.

/* Read a line of input - use line_count to read properly */
read_line(Line) :-
    flush_output,
    current_input(Stream),
    get_line(Stream, Codes),
    atom_codes(Line, Codes).

/* Helper to get a line as character codes */
get_line(Stream, Codes) :-
    get_code(Stream, C),
    (C = 10 -> Codes = []  /* newline */
    ; C = -1 -> Codes = []  /* EOF */
    ; Codes = [C|Rest], get_line(Stream, Rest)
    ).

/* Parse time input HH:MM */
parse_time_string(TimeStr, time(Hour, Minute)) :-
    atom_chars(TimeStr, Chars),
    append(HourChars, [':'|MinChars], Chars),
    number_chars(Hour, HourChars),
    number_chars(Minute, MinChars),
    Hour >= 0, Hour =< 23,
    Minute >= 0, Minute =< 59.

/* Add event from user input */
add_event_from_input :-
    nl, write('========================================'), nl,
    write('ADD NEW EVENT'), nl,
    write('========================================'), nl,
    write('Event Title: '), read_line(Title),
    write('Start Time (HH:MM in 24-hour format): '), read_line(StartStr),
    write('End Time (HH:MM in 24-hour format): '), read_line(EndStr),
    (parse_time_string(StartStr, Start), parse_time_string(EndStr, End),
     time_to_minutes(Start, SM), time_to_minutes(End, EM), SM < EM ->
        write('Location: '), read_line(Location),
        write('Resource (e.g., Prof. Name, Equipment): '), read_line(Resource),
        write('Description: '), read_line(Description),
        add_event(Title, Start, End, Location, Resource, Description)
    ;
        nl, write('Error: Invalid time format or start time is not before end time.'), nl, nl
    ).

/* Edit event interface */
edit_event_interface :-
    nl, write('========================================'), nl,
    write('EDIT EVENT'), nl,
    write('========================================'), nl,
    list_all_events,
    write('Enter Event ID to edit: '), read_line(IDStr),
    catch(my_atom_number(IDStr, ID), _, fail),
    (event(ID, Title, Start, End, Location, Resource, Description) ->
        nl, write('Current Event Details:'), nl,
        write('[ID:'), write(ID), write('] '),
        format_time(Start),
        write(' - '),
        format_time(End),
        write(': '), write(Title), write(' ('), write(Location), write(')'), nl,
        write('Resource: '), write(Resource), nl,
        write('Description: '), write(Description), nl, nl,
        write('What would you like to edit?'), nl,
        write('1. Title'), nl,
        write('2. Time'), nl,
        write('3. Location'), nl,
        write('4. Resource'), nl,
        write('5. Description'), nl,
        write('6. Edit All'), nl,
        write('7. Cancel'), nl,
        write('Choice: '), read_line(ChoiceStr),
        catch(my_atom_number(ChoiceStr, Choice), _, fail),
        process_edit_choice(ID, Choice)
    ;
        nl, write('Event not found!'), nl, nl
    ).

edit_event_interface :-
    nl, write('Invalid input.'), nl, nl.

/* Process edit choice */
process_edit_choice(_, 7) :- 
    nl, write('Edit cancelled.'), nl, nl, !.

process_edit_choice(ID, 1) :-
    write('New Title: '), read_line(NewTitle),
    edit_event_title(ID, NewTitle),
    show_update_message(ID).

process_edit_choice(ID, 2) :-
    write('New Start Time (HH:MM): '), read_line(StartStr),
    write('New End Time (HH:MM): '), read_line(EndStr),
    (parse_time_string(StartStr, Start), parse_time_string(EndStr, End),
     time_to_minutes(Start, SM), time_to_minutes(End, EM), SM < EM ->
        edit_event_time(ID, Start, End),
        show_update_message(ID)
    ;
        nl, write('Error: Invalid time.'), nl, nl
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
    write('New Start Time (HH:MM): '), read_line(StartStr),
    write('New End Time (HH:MM): '), read_line(EndStr),
    (parse_time_string(StartStr, Start), parse_time_string(EndStr, End) ->
        write('New Location: '), read_line(NewLocation),
        write('New Resource: '), read_line(NewResource),
        write('New Description: '), read_line(NewDescription),
        edit_event_all(ID, NewTitle, Start, End, NewLocation, NewResource, NewDescription),
        show_update_message(ID)
    ;
        nl, write('Error: Invalid time format.'), nl, nl
    ).

process_edit_choice(_, _) :-
    nl, write('Invalid choice.'), nl, nl.

/* Show update message after editing */
show_update_message(ID) :-
    event(ID, Title, Start, End, Location, _, _),
    nl, write('Event updated successfully!'), nl,
    write('Updated Event [ID:'), write(ID), write(']: '),
    format_time(Start),
    write(' - '),
    format_time(End),
    write(': '), write(Title), write(' ('), write(Location), write(')'), nl, nl,
    (has_conflict(ID) ->
        write('WARNING: This event still has conflicts!'), nl,
        write('Use option 3 to see conflict details.'), nl, nl
    ;
        write('This event has no conflicts.'), nl, nl
    ).

/* Delete event interface */
delete_event_interface :-
    nl, write('========================================'), nl,
    write('DELETE EVENT'), nl,
    write('========================================'), nl,
    list_all_events,
    write('Enter Event ID to delete: '), read_line(IDStr),
    catch(my_atom_number(IDStr, ID), _, fail),
    (event(ID, Title, Start, End, Location, _, _) ->
        nl, write('Event to delete:'), nl,
        write('[ID:'), write(ID), write('] '),
        format_time(Start),
        write(' - '),
        format_time(End),
        write(': '), write(Title), write(' ('), write(Location), write(')'), nl,
        nl, write('Are you sure you want to delete this event? (yes/no): '),
        read_line(Confirm),
        to_lower(Confirm, ConfirmLower),
        (ConfirmLower = yes ; ConfirmLower = y ->
            delete_event_confirmed(ID)
        ;
            nl, write('Deletion cancelled.'), nl, nl
        )
    ;
        nl, write('Event not found!'), nl, nl
    ).

delete_event_interface :-
    nl, write('Invalid input.'), nl, nl.

/* Process menu choice */
process_choice(1) :- add_event_from_input, !.
process_choice(2) :- 
    (event(_, _, _, _, _, _, _) ->
        list_all_events
    ;
        nl, write('No events to display. Please add events first.'), nl, nl
    ), !.
process_choice(3) :-
    (event(_, _, _, _, _, _, _) ->
        print_conflict_report
    ;
        nl, write('No events to check. Please add events first.'), nl, nl
    ), !.
process_choice(4) :-
    (event(_, _, _, _, _, _, _) ->
        list_conflicting_events
    ;
        nl, write('No events to check. Please add events first.'), nl, nl
    ), !.
process_choice(5) :-
    (event(_, _, _, _, _, _, _) ->
        edit_event_interface
    ;
        nl, write('No events to edit. Please add events first.'), nl, nl
    ), !.
process_choice(6) :-
    (event(_, _, _, _, _, _, _) ->
        delete_event_interface
    ;
        nl, write('No events to delete. Please add events first.'), nl, nl
    ), !.
process_choice(7) :-
    (event(_, _, _, _, _, _, _) ->
        print_chronological_schedule
    ;
        nl, write('No events to display. Please add events first.'), nl, nl
    ), !.
process_choice(8) :-
    (event(_, _, _, _, _, _, _) ->
        nl, write('Enter resource name to filter: '), read_line(ResourceName),
        print_filtered_view(ResourceName)
    ;
        nl, write('No events to filter. Please add events first.'), nl, nl
    ), !.
process_choice(9) :-
    nl, write('Thank you for using the Event Scheduling System!'), nl,
    halt.
process_choice(_) :-
    nl, write('Invalid choice. Please select 1-9.'), nl, nl, !.

/* Main loop */
main_loop :-
    display_menu,
    write('Enter your choice: '),
    read_line(Input),
    catch(my_atom_number(Input, Choice), _, fail),
    !,
    process_choice(Choice),
    (Choice = 9 -> true ; main_loop).

main_loop :-
    nl, write('Invalid input. Please enter a number.'), nl, nl,
    main_loop.

/* Start the system */
run_scheduling_system :-
    write('==============================================='), nl,
    write('EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM'), nl,
    write('Logic Programming Paradigm Implementation (GNU Prolog)'), nl,
    write('==============================================='), nl, nl,
    main_loop.

/* Main entry point */
:- initialization(run_scheduling_system).

/*
 * USAGE INSTRUCTIONS:
 * 
 * 1. Compile: gplc event_schedule.pl
 * 2. Run: ./event_schedule
 * 3. Or interpret: gprolog --consult-file event_schedule.pl
 * 
 * Time Format: Use 24-hour format (e.g., 09:00, 14:30)
 */