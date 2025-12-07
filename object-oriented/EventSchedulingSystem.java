/*
 * EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM
 * Object-Oriented Paradigm - Java Implementation with Edit/Delete
 * 
 * File: EventSchedulingSystem.java
 * Compile: javac EventSchedulingSystem.java
 * Run: java EventSchedulingSystem
 */

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Scanner;

/*
 * Class representing time with encapsulation
 */
class Time {
    private int hour;
    private int minute;
    
    // Constructor
    public Time(int hour, int minute) {
        this.hour = hour;
        this.minute = minute;
    }
    
    // Getters demonstrating encapsulation
    public int getHour() { return hour; }
    public int getMinute() { return minute; }
    
    // Convert to minutes for comparison
    public int toMinutes() {
        return hour * 60 + minute;
    }
    
    // Override toString for easy printing - polymorphism
    @Override
    public String toString() {
        return String.format("%02d:%02d", hour, minute);
    }
}

/*
 * Core Event class demonstrating OOP principles
 */
class Event {
    private static int nextId = 1;
    private int id;
    private String title;
    private Time startTime;
    private Time endTime;
    private String location;
    private String resource;
    private String description;
    private boolean isConflicting;
    
    // Constructor with parameters
    public Event(String title, Time startTime, Time endTime, 
                 String location, String resource, String description) {
        this.id = nextId++;
        this.title = title;
        this.startTime = startTime;
        this.endTime = endTime;
        this.location = location;
        this.resource = resource;
        this.description = description;
        this.isConflicting = false;
    }
    
    // Getters (Encapsulation)
    public int getId() { return id; }
    public String getTitle() { return title; }
    public Time getStartTime() { return startTime; }
    public Time getEndTime() { return endTime; }
    public String getLocation() { return location; }
    public String getResource() { return resource; }
    public String getDescription() { return description; }
    public boolean isConflicting() { return isConflicting; }
    
    // Setters for editing
    public void setTitle(String title) { this.title = title; }
    public void setStartTime(Time startTime) { this.startTime = startTime; }
    public void setEndTime(Time endTime) { this.endTime = endTime; }
    public void setLocation(String location) { this.location = location; }
    public void setResource(String resource) { this.resource = resource; }
    public void setDescription(String description) { this.description = description; }
    public void setConflicting(boolean conflicting) { 
        this.isConflicting = conflicting; 
    }
    
    // Method to check time overlap with another event
    public boolean hasTimeOverlapWith(Event other) {
        int thisStart = this.startTime.toMinutes();
        int thisEnd = this.endTime.toMinutes();
        int otherStart = other.startTime.toMinutes();
        int otherEnd = other.endTime.toMinutes();
        
        return (thisStart < otherEnd && thisEnd > otherStart);
    }
    
    // Method to check location conflict
    public boolean hasLocationConflictWith(Event other) {
        return this.hasTimeOverlapWith(other) && 
               this.location.equals(other.location);
    }
    
    // Method to check resource conflict
    public boolean hasResourceConflictWith(Event other) {
        return this.hasTimeOverlapWith(other) && 
               (this.resource.contains(other.resource) || 
                other.resource.contains(this.resource));
    }
    
    // Override toString
    @Override
    public String toString() {
        return String.format("[ID:%d] %s - %s: %s (%s)", 
                           id, startTime, endTime, title, location);
    }
}

/*
 * Class representing a conflict between events
 */
class Conflict {
    private String type;
    private Event event1;
    private Event event2;
    private String conflictResource;
    
    public Conflict(String type, Event e1, Event e2, String resource) {
        this.type = type;
        this.event1 = e1;
        this.event2 = e2;
        this.conflictResource = resource;
    }
    
    // Method to generate conflict report
    public void printReport(int conflictNumber) {
        System.out.println("Conflict " + conflictNumber + ": " + type);
        System.out.println("Type: " + type + " Double-Booking");
        System.out.println("Conflicting " + 
                         (type.equals("Location") ? "Location" : "Resource") + 
                         ": " + conflictResource);
        System.out.println("Conflicting Events:");
        System.out.println("  - " + event1);
        System.out.println("  - " + event2);
        System.out.println("Resolution Guidance:");
        if (type.equals("Location")) {
            System.out.println("  - Relocate one event to an available room");
            System.out.println("  - Adjust event times to avoid overlap");
        } else {
            System.out.println("  - Assign an alternate resource");
            System.out.println("  - Reschedule one of the events");
        }
        System.out.println();
    }
}

/*
 * Main class managing the scheduling system - demonstrates composition
 */
class ScheduleManager {
    private List<Event> events;
    private List<Conflict> conflicts;
    
    // Constructor
    public ScheduleManager() {
        this.events = new ArrayList<>();
        this.conflicts = new ArrayList<>();
    }
    
    // Method to add event
    public void addEvent(Event event) {
        events.add(event);
    }
    
    // Method to get event by ID
    public Event getEventById(int id) {
        for (Event e : events) {
            if (e.getId() == id) {
                return e;
            }
        }
        return null;
    }
    
    // Method to delete event by ID
    public boolean deleteEvent(int id) {
        Event eventToRemove = getEventById(id);
        if (eventToRemove != null) {
            events.remove(eventToRemove);
            return true;
        }
        return false;
    }
    
    // Method to detect all conflicts
    public void detectConflicts() {
        conflicts.clear();
        
        // Reset all conflict flags
        for (Event e : events) {
            e.setConflicting(false);
        }
        
        for (int i = 0; i < events.size(); i++) {
            for (int j = i + 1; j < events.size(); j++) {
                Event e1 = events.get(i);
                Event e2 = events.get(j);
                
                // Check location conflict
                if (e1.hasLocationConflictWith(e2)) {
                    e1.setConflicting(true);
                    e2.setConflicting(true);
                    conflicts.add(new Conflict("Location", e1, e2, 
                                             e1.getLocation()));
                }
                
                // Check resource conflict
                if (e1.hasResourceConflictWith(e2)) {
                    e1.setConflicting(true);
                    e2.setConflicting(true);
                    conflicts.add(new Conflict("Resource", e1, e2, 
                                             e1.getResource()));
                }
            }
        }
    }
    
    // Method to print conflict report
    public void printConflictReport() {
        System.out.println("\n========================================");
        System.out.println("CONFLICT DETECTION AND RESOLUTION REPORT");
        System.out.println("========================================\n");
        
        if (conflicts.isEmpty()) {
            System.out.println("No conflicts detected.\n");
        } else {
            for (int i = 0; i < conflicts.size(); i++) {
                conflicts.get(i).printReport(i + 1);
            }
        }
    }
    
    // Method to list all events
    public void listAllEvents() {
        System.out.println("\n========================================");
        System.out.println("ALL EVENTS");
        System.out.println("========================================\n");
        
        if (events.isEmpty()) {
            System.out.println("No events scheduled.\n");
            return;
        }
        
        for (Event e : events) {
            System.out.println(e);
            System.out.println("  Resource: " + e.getResource());
            System.out.println("  Status: " + 
                             (e.isConflicting() ? "CONFLICTING" : "OK"));
            System.out.println("  Description: " + e.getDescription() + "\n");
        }
    }
    
    // Method to list conflicting events
    public void listConflictingEvents() {
        System.out.println("\n========================================");
        System.out.println("CONFLICTING EVENTS");
        System.out.println("========================================\n");
        
        List<Event> conflictingEvents = new ArrayList<>();
        for (Event e : events) {
            if (e.isConflicting()) {
                conflictingEvents.add(e);
            }
        }
        
        if (conflictingEvents.isEmpty()) {
            System.out.println("No conflicting events.\n");
            return;
        }
        
        for (Event e : conflictingEvents) {
            System.out.println(e);
            System.out.println("  Resource: " + e.getResource());
            System.out.println("  Description: " + e.getDescription() + "\n");
        }
        
        System.out.println("Total conflicting events: " + conflictingEvents.size() + "\n");
    }
    
    // Method to print chronological schedule
    public void printChronologicalSchedule() {
        System.out.println("\n========================================");
        System.out.println("CHRONOLOGICAL SCHEDULE DISPLAY");
        System.out.println("========================================\n");
        
        // Sort events by start time - using Java Collections
        List<Event> sortedEvents = new ArrayList<>(events);
        Collections.sort(sortedEvents, new Comparator<Event>() {
            @Override
            public int compare(Event e1, Event e2) {
                return Integer.compare(e1.getStartTime().toMinutes(), 
                                     e2.getStartTime().toMinutes());
            }
        });
        
        int conflictCount = 0;
        for (Event e : events) {
            if (e.isConflicting()) conflictCount++;
        }
        
        System.out.printf("Summary: %d total events, %d conflicts detected, " +
                        "%d events affected\n\n", 
                        events.size(), conflicts.size(), conflictCount);
        System.out.println("Event List (Temporal Order):\n");
        
        for (Event e : sortedEvents) {
            System.out.println(e);
            System.out.println("  Resource: " + e.getResource());
            System.out.println("  Status: " + 
                             (e.isConflicting() ? "Conflicting" : 
                              "Successfully Scheduled"));
            System.out.println("  Description: " + e.getDescription() + "\n");
        }
    }
    
    // Method to print filtered view
    public void printFilteredView(String resourceName) {
        System.out.println("\n========================================");
        System.out.println("FILTERED VIEW: " + resourceName);
        System.out.println("========================================\n");
        
        List<Event> filteredEvents = new ArrayList<>();
        int conflictCount = 0;
        
        for (Event e : events) {
            if (e.getResource().contains(resourceName)) {
                filteredEvents.add(e);
                if (e.isConflicting()) conflictCount++;
            }
        }
        
        if (filteredEvents.isEmpty()) {
            System.out.println("No events found for this resource.\n");
        } else {
            for (Event e : filteredEvents) {
                System.out.println(e);
                System.out.println("  Status: " + 
                                 (e.isConflicting() ? "Conflicting" : 
                                  "No Conflict") + "\n");
            }
            
            System.out.printf("Summary: %d events found, %d conflicts\n", 
                            filteredEvents.size(), conflictCount);
            
            if (conflictCount == filteredEvents.size() && 
                filteredEvents.size() > 1) {
                System.out.println("WARNING: 100% conflict rate - " +
                                 "severe over-scheduling detected!");
                System.out.println("Recommendation: Implement buffer time " +
                                 "between events.");
            }
        }
    }
    
    public boolean hasEvents() {
        return !events.isEmpty();
    }
    
    public boolean hasConflicts() {
        return !conflicts.isEmpty();
    }
}

/*
 * Main driver class with user input
 */
public class EventSchedulingSystem {
    
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        ScheduleManager manager = new ScheduleManager();
        
        System.out.println("===============================================");
        System.out.println("EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM");
        System.out.println("Object-Oriented Paradigm Implementation (Java)");
        System.out.println("===============================================\n");
        
        while (true) {
            displayMenu();
            System.out.print("Enter your choice: ");
            
            int choice = 0;
            try {
                choice = Integer.parseInt(scanner.nextLine());
            } catch (NumberFormatException e) {
                System.out.println("\nInvalid input. Please enter a number.\n");
                continue;
            }
            
            switch (choice) {
                case 1:
                    addEventFromInput(scanner, manager);
                    break;
                case 2:
                    if (manager.hasEvents()) {
                        manager.detectConflicts();
                        manager.listAllEvents();
                    } else {
                        System.out.println("\nNo events to display. Please add events first.\n");
                    }
                    break;
                case 3:
                    if (manager.hasEvents()) {
                        manager.detectConflicts();
                        manager.printConflictReport();
                    } else {
                        System.out.println("\nNo events to check. Please add events first.\n");
                    }
                    break;
                case 4:
                    if (manager.hasEvents()) {
                        manager.detectConflicts();
                        manager.listConflictingEvents();
                    } else {
                        System.out.println("\nNo events to check. Please add events first.\n");
                    }
                    break;
                case 5:
                    if (manager.hasEvents()) {
                        editEvent(scanner, manager);
                    } else {
                        System.out.println("\nNo events to edit. Please add events first.\n");
                    }
                    break;
                case 6:
                    if (manager.hasEvents()) {
                        deleteEvent(scanner, manager);
                    } else {
                        System.out.println("\nNo events to delete. Please add events first.\n");
                    }
                    break;
                case 7:
                    if (manager.hasEvents()) {
                        manager.detectConflicts();
                        manager.printChronologicalSchedule();
                    } else {
                        System.out.println("\nNo events to display. Please add events first.\n");
                    }
                    break;
                case 8:
                    if (manager.hasEvents()) {
                        System.out.print("\nEnter resource name to filter: ");
                        String resourceName = scanner.nextLine();
                        manager.detectConflicts();
                        manager.printFilteredView(resourceName);
                    } else {
                        System.out.println("\nNo events to filter. Please add events first.\n");
                    }
                    break;
                case 9:
                    System.out.println("\nThank you for using the Event Scheduling System!");
                    scanner.close();
                    return;
                default:
                    System.out.println("\nInvalid choice. Please select 1-9.\n");
            }
        }
    }
    
    private static void displayMenu() {
        System.out.println("========================================");
        System.out.println("MAIN MENU");
        System.out.println("========================================");
        System.out.println("1. Add New Event");
        System.out.println("2. View All Events");
        System.out.println("3. Detect and Display Conflicts");
        System.out.println("4. View Conflicting Events Only");
        System.out.println("5. Edit Event");
        System.out.println("6. Delete Event");
        System.out.println("7. Display Chronological Schedule");
        System.out.println("8. Filter Events by Resource");
        System.out.println("9. Exit");
        System.out.println("========================================");
    }
    
    private static void addEventFromInput(Scanner scanner, ScheduleManager manager) {
        System.out.println("\n========================================");
        System.out.println("ADD NEW EVENT");
        System.out.println("========================================");
        
        try {
            System.out.print("Event Title: ");
            String title = scanner.nextLine();
            
            System.out.print("Start Time (HH:MM in 24-hour format): ");
            String startTimeStr = scanner.nextLine();
            String[] startParts = startTimeStr.split(":");
            int startHour = Integer.parseInt(startParts[0]);
            int startMinute = Integer.parseInt(startParts[1]);
            
            System.out.print("End Time (HH:MM in 24-hour format): ");
            String endTimeStr = scanner.nextLine();
            String[] endParts = endTimeStr.split(":");
            int endHour = Integer.parseInt(endParts[0]);
            int endMinute = Integer.parseInt(endParts[1]);
            
            System.out.print("Location: ");
            String location = scanner.nextLine();
            
            System.out.print("Resource (e.g., Prof. Name, Equipment): ");
            String resource = scanner.nextLine();
            
            System.out.print("Description: ");
            String description = scanner.nextLine();
            
            // Validate time
            if (startHour < 0 || startHour > 23 || startMinute < 0 || startMinute > 59 ||
                endHour < 0 || endHour > 23 || endMinute < 0 || endMinute > 59) {
                System.out.println("\nInvalid time format. Please use valid hours (0-23) and minutes (0-59).\n");
                return;
            }
            
            Time start = new Time(startHour, startMinute);
            Time end = new Time(endHour, endMinute);
            
            if (start.toMinutes() >= end.toMinutes()) {
                System.out.println("\nError: Start time must be before end time.\n");
                return;
            }
            
            Event event = new Event(title, start, end, location, resource, description);
            manager.addEvent(event);
            
            System.out.println("\nEvent added successfully!");
            System.out.println("Event: " + event + "\n");
            
            // Check for conflicts immediately
            manager.detectConflicts();
            if (event.isConflicting()) {
                System.out.println("WARNING: This event has conflicts with existing events!");
                System.out.println("Use 'Detect and Display Conflicts' to see details.\n");
            }
            
        } catch (Exception e) {
            System.out.println("\nError adding event. Please check your input format.\n");
        }
    }
    
    private static void editEvent(Scanner scanner, ScheduleManager manager) {
        System.out.println("\n========================================");
        System.out.println("EDIT EVENT");
        System.out.println("========================================");
        
        // Show all events first
        manager.detectConflicts();
        manager.listAllEvents();
        
        System.out.print("Enter Event ID to edit: ");
        try {
            int id = Integer.parseInt(scanner.nextLine());
            Event event = manager.getEventById(id);
            
            if (event == null) {
                System.out.println("\nEvent not found!\n");
                return;
            }
            
            System.out.println("\nCurrent Event Details:");
            System.out.println(event);
            System.out.println("Resource: " + event.getResource());
            System.out.println("Description: " + event.getDescription());
            
            System.out.println("\nWhat would you like to edit?");
            System.out.println("1. Title");
            System.out.println("2. Time");
            System.out.println("3. Location");
            System.out.println("4. Resource");
            System.out.println("5. Description");
            System.out.println("6. Edit All");
            System.out.println("7. Cancel");
            System.out.print("Choice: ");
            
            int editChoice = Integer.parseInt(scanner.nextLine());
            
            switch (editChoice) {
                case 1:
                    System.out.print("New Title: ");
                    event.setTitle(scanner.nextLine());
                    break;
                case 2:
                    System.out.print("New Start Time (HH:MM): ");
                    String[] startParts = scanner.nextLine().split(":");
                    System.out.print("New End Time (HH:MM): ");
                    String[] endParts = scanner.nextLine().split(":");
                    
                    int sh = Integer.parseInt(startParts[0]);
                    int sm = Integer.parseInt(startParts[1]);
                    int eh = Integer.parseInt(endParts[0]);
                    int em = Integer.parseInt(endParts[1]);
                    
                    if (sh < 0 || sh > 23 || sm < 0 || sm > 59 ||
                        eh < 0 || eh > 23 || em < 0 || em > 59) {
                        System.out.println("\nInvalid time format.\n");
                        return;
                    }
                    
                    Time start = new Time(sh, sm);
                    Time end = new Time(eh, em);
                    
                    if (start.toMinutes() >= end.toMinutes()) {
                        System.out.println("\nError: Start time must be before end time.\n");
                        return;
                    }
                    
                    event.setStartTime(start);
                    event.setEndTime(end);
                    break;
                case 3:
                    System.out.print("New Location: ");
                    event.setLocation(scanner.nextLine());
                    break;
                case 4:
                    System.out.print("New Resource: ");
                    event.setResource(scanner.nextLine());
                    break;
                case 5:
                    System.out.print("New Description: ");
                    event.setDescription(scanner.nextLine());
                    break;
                case 6:
                    System.out.print("New Title: ");
                    event.setTitle(scanner.nextLine());
                    
                    System.out.print("New Start Time (HH:MM): ");
                    String[] sp = scanner.nextLine().split(":");
                    System.out.print("New End Time (HH:MM): ");
                    String[] ep = scanner.nextLine().split(":");
                    
                    event.setStartTime(new Time(Integer.parseInt(sp[0]), Integer.parseInt(sp[1])));
                    event.setEndTime(new Time(Integer.parseInt(ep[0]), Integer.parseInt(ep[1])));
                    
                    System.out.print("New Location: ");
                    event.setLocation(scanner.nextLine());
                    
                    System.out.print("New Resource: ");
                    event.setResource(scanner.nextLine());
                    
                    System.out.print("New Description: ");
                    event.setDescription(scanner.nextLine());
                    break;
                case 7:
                    System.out.println("\nEdit cancelled.\n");
                    return;
                default:
                    System.out.println("\nInvalid choice.\n");
                    return;
            }
            
            System.out.println("\nEvent updated successfully!");
            System.out.println("Updated Event: " + event + "\n");
            
            // Recheck conflicts
            manager.detectConflicts();
            if (event.isConflicting()) {
                System.out.println("WARNING: This event still has conflicts!");
                System.out.println("Use 'Detect and Display Conflicts' to see details.\n");
            } else {
                System.out.println("This event has no conflicts.\n");
            }
            
        } catch (Exception e) {
            System.out.println("\nError editing event. Please check your input.\n");
        }
    }
    
    private static void deleteEvent(Scanner scanner, ScheduleManager manager) {
        System.out.println("\n========================================");
        System.out.println("DELETE EVENT");
        System.out.println("========================================");
        
        // Show all events first
        manager.detectConflicts();
        manager.listAllEvents();
        
        System.out.print("Enter Event ID to delete: ");
        try {
            int id = Integer.parseInt(scanner.nextLine());
            Event event = manager.getEventById(id);
            
            if (event == null) {
                System.out.println("\nEvent not found!\n");
                return;
            }
            
            System.out.println("\nEvent to delete:");
            System.out.println(event);
            System.out.print("\nAre you sure you want to delete this event? (yes/no): ");
            String confirm = scanner.nextLine();
            
            if (confirm.equalsIgnoreCase("yes") || confirm.equalsIgnoreCase("y")) {
                if (manager.deleteEvent(id)) {
                    System.out.println("\nEvent deleted successfully!\n");
                    
                    // Recheck conflicts after deletion
                    manager.detectConflicts();
                    System.out.println("Conflicts have been re-evaluated.\n");
                } else {
                    System.out.println("\nFailed to delete event.\n");
                }
            } else {
                System.out.println("\nDeletion cancelled.\n");
            }
            
        } catch (Exception e) {
            System.out.println("\nError deleting event. Please check your input.\n");
        }
    }
}

/*
 * USAGE INSTRUCTIONS:
 * 
 * 1. Compile: javac EventSchedulingSystem.java
 * 2. Run: java EventSchedulingSystem
 * 3. Features:
 *    - Add events with automatic conflict detection
 *    - View all events with their status
 *    - View only conflicting events
 *    - Edit events (title, time, location, resource, description)
 *    - Delete events with confirmation
 *    - Automatic conflict re-evaluation after edits/deletes
 * 
 * Time Format: Use 24-hour format (e.g., 09:00, 14:30)
 */