// Person.h - Generated by GSL for entity Person

#ifndef PERSON_H
#define PERSON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// --- Definition for Person struct ---
typedef struct Person {
  int id; // Unique identifier for the person;
  char* firstname; // First name of the person;
  char* lastname; // Last name of the person;
  int age; // Age of the person;
} Person;

// --- Definition for PersonNode (Linked List Node) ---
typedef struct PersonNode {
  Person data; // Embeds the actual entity data
  struct PersonNode *next;
} PersonNode;

// --- Function Prototypes for Person Linked List ---
PersonNode* createPersonNode(
  int id,
  const char* firstname,
  const char* lastname,
  int age
);

void addPersonToList(PersonNode** head, PersonNode* newNode);
void printPerson(const Person* entityData);
void printPersonList(const PersonNode* head);
void freePersonList(PersonNode** head);

#endif // PERSON_H
