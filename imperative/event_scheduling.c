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
void initialize_events();
int time_to_minutes(Time t);
int check_time_overlap(Event e1, Event e2);
void detect_conflicts();
void print_conflict_report();
void print_chronological_schedule();
void print_filtered_view(char* resource_name);
void add_event(char* title, int sh, int sm, int eh, int em, 
               char* loc, char* res, char* desc);

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

/* Initialize sample events from the problem */
void initialize_events() {
    add_event("Math Seminar", 9, 0, 10, 30, 
              "Room 201", "Prof. A", "Linear Algebra Review");
    add_event("CS Department Meeting", 10, 0, 11, 0, 
              "Room 201", "Prof. B, Prof. C", "Curriculum planning discussions");
    add_event("Project Review Session", 9, 45, 10, 15, 
              "Room 101", "Prof. A", "Student capstone reviews");
    add_event("Lab Equipment Maintenance", 11, 0, 12, 0, 
              "Computer Lab", "Technician Joe", "Monthly maintenance checks");
}

/* Detect all conflicts between events */
void detect_conflicts() {
    int i, j;
    
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
    
    printf("\n========================================\n");
    printf("CONFLICT DETECTION AND RESOLUTION REPORT\n");
    printf("========================================\n\n");
    
    for (i = 0; i < event_count; i++) {
        for (j = i + 1; j < event_count; j++) {
            if (check_time_overlap(events[i], events[j])) {
                /* Location conflict */
                if (strcmp(events[i].location, events[j].location) == 0) {
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
}

/* Main function - entry point */
int main() {
    printf("EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM\n");
    printf("Imperative Paradigm Implementation (C Language)\n");
    printf("===============================================\n");
    
    /* Sequential execution - characteristic of imperative programming */
    initialize_events();
    detect_conflicts();
    print_conflict_report();
    print_chronological_schedule();
    print_filtered_view("Prof. A");
    
    return 0;
}