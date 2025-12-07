/*
 * EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM
 * Imperative Paradigm - C Implementation with User Input
 * 
 * File: EventSchedulingSystem.c
 * Compile: gcc EventSchedulingSystem.c -o EventSchedulingSystem
 * Run: ./EventSchedulingSystem
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX_EVENTS 100
#define MAX_STRING 200

/* Structure definitions for imperative approach */
typedef struct {
    int hour;
    int minute;
} Time;

typedef struct {
    char title[MAX_STRING];
    Time start_time;
    Time end_time;
    char location[MAX_STRING];
    char resource[MAX_STRING];
    char description[MAX_STRING];
    int is_conflicting;
} Event;

/* Global variables - characteristic of imperative programming */
Event events[MAX_EVENTS];
int event_count = 0;

/* Function prototypes */
int time_to_minutes(Time t);
int check_time_overlap(Event e1, Event e2);
void detect_conflicts();
void print_conflict_report();
void print_chronological_schedule();
void print_filtered_view(char* resource_name);
void add_event(char* title, int sh, int sm, int eh, int em, 
               char* loc, char* res, char* desc);
void display_menu();
void add_event_from_input();
void clear_input_buffer();

/* Convert time to minutes for easy comparison */
int time_to_minutes(Time t) {
    return t.hour * 60 + t.minute;
}

/* Check if two events have time overlap */
int check_time_overlap(Event e1, Event e2) {
    int e1_start = time_to_minutes(e1.start_time);
    int e1_end = time_to_minutes(e1.end_time);
    int e2_start = time_to_minutes(e2.start_time);
    int e2_end = time_to_minutes(e2.end_time);
    
    return (e1_start < e2_end && e1_end > e2_start);
}

/* Add an event to the system */
void add_event(char* title, int sh, int sm, int eh, int em, 
               char* loc, char* res, char* desc) {
    if (event_count >= MAX_EVENTS) {
        printf("\nError: Maximum event limit reached (%d events).\n\n", MAX_EVENTS);
        return;
    }
    
    strcpy(events[event_count].title, title);
    events[event_count].start_time.hour = sh;
    events[event_count].start_time.minute = sm;
    events[event_count].end_time.hour = eh;
    events[event_count].end_time.minute = em;
    strcpy(events[event_count].location, loc);
    strcpy(events[event_count].resource, res);
    strcpy(events[event_count].description, desc);
    events[event_count].is_conflicting = 0;
    event_count++;
}

/* Clear input buffer */
void clear_input_buffer() {
    int c;
    while ((c = getchar()) != '\n' && c != EOF);
}

/* Display main menu */
void display_menu() {
    printf("========================================\n");
    printf("MAIN MENU\n");
    printf("========================================\n");
    printf("1. Add New Event\n");
    printf("2. Detect and Display Conflicts\n");
    printf("3. Display Chronological Schedule\n");
    printf("4. Filter Events by Resource\n");
    printf("5. Exit\n");
    printf("========================================\n");
}

/* Add event from user input */
void add_event_from_input() {
    char title[MAX_STRING];
    char location[MAX_STRING];
    char resource[MAX_STRING];
    char description[MAX_STRING];
    int sh, sm, eh, em;
    
    printf("\n========================================\n");
    printf("ADD NEW EVENT\n");
    printf("========================================\n");
    
    printf("Event Title: ");
    fgets(title, MAX_STRING, stdin);
    title[strcspn(title, "\n")] = 0;  // Remove newline
    
    printf("Start Time (HH MM in 24-hour format): ");
    if (scanf("%d %d", &sh, &sm) != 2) {
        printf("\nError: Invalid time format.\n\n");
        clear_input_buffer();
        return;
    }
    
    printf("End Time (HH MM in 24-hour format): ");
    if (scanf("%d %d", &eh, &em) != 2) {
        printf("\nError: Invalid time format.\n\n");
        clear_input_buffer();
        return;
    }
    clear_input_buffer();
    
    /* Validate time */
    if (sh < 0 || sh > 23 || sm < 0 || sm > 59 ||
        eh < 0 || eh > 23 || em < 0 || em > 59) {
        printf("\nError: Invalid time. Hours must be 0-23, minutes 0-59.\n\n");
        return;
    }
    
    if (sh * 60 + sm >= eh * 60 + em) {
        printf("\nError: Start time must be before end time.\n\n");
        return;
    }
    
    printf("Location: ");
    fgets(location, MAX_STRING, stdin);
    location[strcspn(location, "\n")] = 0;
    
    printf("Resource (e.g., Prof. Name, Equipment): ");
    fgets(resource, MAX_STRING, stdin);
    resource[strcspn(resource, "\n")] = 0;
    
    printf("Description: ");
    fgets(description, MAX_STRING, stdin);
    description[strcspn(description, "\n")] = 0;
    
    add_event(title, sh, sm, eh, em, location, resource, description);
    
    printf("\nEvent added successfully!\n");
    printf("Event: %02d:%02d - %02d:%02d: %s (%s)\n\n",
           sh, sm, eh, em, title, location);
}

/* Detect all conflicts between events */
void detect_conflicts() {
    int i, j;
    
    /* Reset conflict flags */
    for (i = 0; i < event_count; i++) {
        events[i].is_conflicting = 0;
    }
    
    /* Nested loops - imperative control structure */
    for (i = 0; i < event_count; i++) {
        for (j = i + 1; j < event_count; j++) {
            if (check_time_overlap(events[i], events[j])) {
                /* Check for location conflict */
                if (strcmp(events[i].location, events[j].location) == 0) {
                    events[i].is_conflicting = 1;
                    events[j].is_conflicting = 1;
                }
                
                /* Check for resource conflict */
                if (strstr(events[i].resource, events[j].resource) != NULL ||
                    strstr(events[j].resource, events[i].resource) != NULL) {
                    events[i].is_conflicting = 1;
                    events[j].is_conflicting = 1;
                }
            }
        }
    }
}

/* Print detailed conflict report */
void print_conflict_report() {
    int i, j;
    int conflict_num = 1;
    int has_conflicts = 0;
    
    printf("\n========================================\n");
    printf("CONFLICT DETECTION AND RESOLUTION REPORT\n");
    printf("========================================\n\n");
    
    for (i = 0; i < event_count; i++) {
        for (j = i + 1; j < event_count; j++) {
            if (check_time_overlap(events[i], events[j])) {
                /* Location conflict */
                if (strcmp(events[i].location, events[j].location) == 0) {
                    has_conflicts = 1;
                    printf("Conflict %d: Location Overlap\n", conflict_num++);
                    printf("Type: Location Double-Booking\n");
                    printf("Location: %s\n", events[i].location);
                    printf("Conflicting Events:\n");
                    printf("  - %s (%02d:%02d - %02d:%02d)\n", 
                           events[i].title, 
                           events[i].start_time.hour, events[i].start_time.minute,
                           events[i].end_time.hour, events[i].end_time.minute);
                    printf("  - %s (%02d:%02d - %02d:%02d)\n", 
                           events[j].title,
                           events[j].start_time.hour, events[j].start_time.minute,
                           events[j].end_time.hour, events[j].end_time.minute);
                    printf("Resolution Guidance:\n");
                    printf("  - Relocate one event to an available room\n");
                    printf("  - Adjust event times to avoid overlap\n\n");
                }
                
                /* Resource conflict */
                if (strstr(events[i].resource, events[j].resource) != NULL ||
                    strstr(events[j].resource, events[i].resource) != NULL) {
                    has_conflicts = 1;
                    printf("Conflict %d: Resource Overlap\n", conflict_num++);
                    printf("Type: Resource Double-Booking (Critical)\n");
                    printf("Resource: %s\n", events[i].resource);
                    printf("Conflicting Events:\n");
                    printf("  - %s (%02d:%02d - %02d:%02d) in %s\n", 
                           events[i].title,
                           events[i].start_time.hour, events[i].start_time.minute,
                           events[i].end_time.hour, events[i].end_time.minute,
                           events[i].location);
                    printf("  - %s (%02d:%02d - %02d:%02d) in %s\n", 
                           events[j].title,
                           events[j].start_time.hour, events[j].start_time.minute,
                           events[j].end_time.hour, events[j].end_time.minute,
                           events[j].location);
                    printf("Resolution Guidance:\n");
                    printf("  - Assign an alternate resource\n");
                    printf("  - Reschedule one of the events\n\n");
                }
            }
        }
    }
    
    if (!has_conflicts) {
        printf("No conflicts detected.\n\n");
    }
}

/* Print events in chronological order */
void print_chronological_schedule() {
    int i, j;
    Event temp;
    Event sorted[MAX_EVENTS];
    int conflict_count = 0;
    
    /* Copy events to sorted array */
    for (i = 0; i < event_count; i++) {
        sorted[i] = events[i];
        if (events[i].is_conflicting) conflict_count++;
    }
    
    /* Bubble sort - imperative sorting algorithm */
    for (i = 0; i < event_count - 1; i++) {
        for (j = 0; j < event_count - i - 1; j++) {
            if (time_to_minutes(sorted[j].start_time) > 
                time_to_minutes(sorted[j + 1].start_time)) {
                temp = sorted[j];
                sorted[j] = sorted[j + 1];
                sorted[j + 1] = temp;
            }
        }
    }
    
    printf("\n========================================\n");
    printf("CHRONOLOGICAL SCHEDULE DISPLAY\n");
    printf("========================================\n\n");
    printf("Summary: %d total events, %d conflicts detected, %d events affected\n\n",
           event_count, conflict_count / 2, conflict_count);
    printf("Event List (Temporal Order):\n\n");
    
    for (i = 0; i < event_count; i++) {
        printf("%02d:%02d - %02d:%02d: %s (%s)\n",
               sorted[i].start_time.hour, sorted[i].start_time.minute,
               sorted[i].end_time.hour, sorted[i].end_time.minute,
               sorted[i].title, sorted[i].location);
        printf("  Resource: %s\n", sorted[i].resource);
        printf("  Status: %s\n", 
               sorted[i].is_conflicting ? "Conflicting" : "Successfully Scheduled");
        printf("  Description: %s\n\n", sorted[i].description);
    }
}

/* Print filtered view for specific resource */
void print_filtered_view(char* resource_name) {
    int i;
    int found_count = 0;
    int conflict_count = 0;
    
    printf("\n========================================\n");
    printf("FILTERED VIEW: %s\n", resource_name);
    printf("========================================\n\n");
    
    for (i = 0; i < event_count; i++) {
        if (strstr(events[i].resource, resource_name) != NULL) {
            found_count++;
            printf("%02d:%02d - %02d:%02d: %s (%s)\n",
                   events[i].start_time.hour, events[i].start_time.minute,
                   events[i].end_time.hour, events[i].end_time.minute,
                   events[i].title, events[i].location);
            printf("  Status: %s\n\n", 
                   events[i].is_conflicting ? "Conflicting" : "No Conflict");
            if (events[i].is_conflicting) conflict_count++;
        }
    }
    
    if (found_count == 0) {
        printf("No events found for this resource.\n");
    } else {
        printf("Summary: %d events found, %d conflicts\n", found_count, conflict_count);
        if (conflict_count == found_count && found_count > 1) {
            printf("WARNING: 100%% conflict rate - severe over-scheduling detected!\n");
            printf("Recommendation: Implement buffer time between events.\n");
        }
    }
    printf("\n");
}

/* Main function - entry point */
int main() {
    int choice;
    char resource_filter[MAX_STRING];
    
    printf("===============================================\n");
    printf("EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM\n");
    printf("Imperative Paradigm Implementation (C Language)\n");
    printf("===============================================\n\n");
    
    /* Main program loop */
    while (1) {
        display_menu();
        printf("Enter your choice: ");
        
        if (scanf("%d", &choice) != 1) {
            printf("\nInvalid input. Please enter a number.\n\n");
            clear_input_buffer();
            continue;
        }
        clear_input_buffer();
        
        switch (choice) {
            case 1:
                add_event_from_input();
                break;
                
            case 2:
                if (event_count == 0) {
                    printf("\nNo events to check. Please add events first.\n\n");
                } else {
                    detect_conflicts();
                    print_conflict_report();
                }
                break;
                
            case 3:
                if (event_count == 0) {
                    printf("\nNo events to display. Please add events first.\n\n");
                } else {
                    detect_conflicts();
                    print_chronological_schedule();
                }
                break;
                
            case 4:
                if (event_count == 0) {
                    printf("\nNo events to filter. Please add events first.\n\n");
                } else {
                    printf("\nEnter resource name to filter: ");
                    fgets(resource_filter, MAX_STRING, stdin);
                    resource_filter[strcspn(resource_filter, "\n")] = 0;
                    detect_conflicts();
                    print_filtered_view(resource_filter);
                }
                break;
                
            case 5:
                printf("\nThank you for using the Event Scheduling System!\n");
                return 0;
                
            default:
                printf("\nInvalid choice. Please select 1-5.\n\n");
        }
    }
    
    return 0;
}

/*
 * USAGE INSTRUCTIONS:
 * 
 * 1. Compile: gcc EventSchedulingSystem.c -o EventSchedulingSystem
 * 2. Run: ./EventSchedulingSystem (Linux/Mac) or EventSchedulingSystem.exe (Windows)
 * 3. Follow the menu prompts to:
 *    - Add events with title, time, location, resource, and description
 *    - Detect conflicts between events
 *    - View chronological schedule
 *    - Filter events by resource
 * 
 * Time Format: Enter hours and minutes separated by space (e.g., 9 0 for 09:00)
 * Example Event Input:
 *   Title: Math Seminar
 *   Start: 9 0
 *   End: 10 30
 *   Location: Room 201
 *   Resource: Prof. A
 *   Description: Linear Algebra Review
 */