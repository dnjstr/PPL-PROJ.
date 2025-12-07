/*
 * EVENT SCHEDULING AND CONFLICT RESOLUTION SYSTEM
 * Object-Oriented Paradigm - Java Implementation with User Input
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
        this.title = title;
        this.startTime = startTime;
        this.endTime = endTime;
        this.location = location;
        this.resource = resource;
        this.description = description;
        this.isConflicting = false;
    }
    
    // Getters (Encapsulation)
    public String getTitle() { return title; }
    public Time getStartTime() { return startTime; }
    public Time getEndTime() { return endTime; }
    public String getLocation() { return location; }
    public String getResource() { return resource; }
    public String getDescription() { return description; }
    public boolean isConflicting() { return isConflicting; }
    
    // Setters
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
        return String.format("%s - %s: %s (%s)", 
                           startTime, endTime, title, location);
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
    
    // Method to detect all conflicts
    public void detectConflicts() {
        conflicts.clear();
        
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
                        manager.printConflictReport();
                    } else {
                        System.out.println("\nNo events to check. Please add events first.\n");
                    }
                    break;
                case 3:
                    if (manager.hasEvents()) {
                        manager.detectConflicts();
                        manager.printChronologicalSchedule();
                    } else {
                        System.out.println("\nNo events to display. Please add events first.\n");
                    }
                    break;
                case 4:
                    if (manager.hasEvents()) {
                        System.out.print("\nEnter resource name to filter: ");
                        String resourceName = scanner.nextLine();
                        manager.detectConflicts();
                        manager.printFilteredView(resourceName);
                    } else {
                        System.out.println("\nNo events to filter. Please add events first.\n");
                    }
                    break;
                case 5:
                    System.out.println("\nThank you for using the Event Scheduling System!");
                    scanner.close();
                    return;
                default:
                    System.out.println("\nInvalid choice. Please select 1-5.\n");
            }
        }
    }
    
    private static void displayMenu() {
        System.out.println("========================================");
        System.out.println("MAIN MENU");
        System.out.println("========================================");
        System.out.println("1. Add New Event");
        System.out.println("2. Detect and Display Conflicts");
        System.out.println("3. Display Chronological Schedule");
        System.out.println("4. Filter Events by Resource");
        System.out.println("5. Exit");
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
            
        } catch (Exception e) {
            System.out.println("\nError adding event. Please check your input format.\n");
        }
    }
}

/*
 * USAGE INSTRUCTIONS:
 * 
 * 1. Compile: javac EventSchedulingSystem.java
 * 2. Run: java EventSchedulingSystem
 * 3. Follow the menu prompts to:
 *    - Add events with title, time, location, resource, and description
 *    - Detect conflicts between events
 *    - View chronological schedule
 *    - Filter events by resource
 * 
 * Time Format: Use 24-hour format (e.g., 09:00, 14:30)
 * Example Event:
 *   Title: Math Seminar
 *   Start: 09:00
 *   End: 10:30
 *   Location: Room 201
 *   Resource: Prof. A
 *   Description: Linear Algebra Review
 */