#include "grade_school.h"
#include <stdlib.h>
#include <string.h>

void init_roster(roster_t *roster) {
  roster->count = 0;
  memset(roster->students, 0, sizeof(roster->students));
}

/**
 * Sort students by grade and then by name within each grade.
 *
 * Takes two pointers to student_t as params.
 */
int sort_by_grade_name_desc(const void *lhs, const void *rhs) {
  student_t *a = (student_t *)lhs;
  student_t *b = (student_t *)rhs;

  if (a->grade - b->grade)
    return a->grade - b->grade;

  return strcmp(a->name, b->name);
}

/**
 * Add a student to the roster.
 *
 * If a student with that name already exists, DO NOT add it and
 * return false; else, add the student and return true.
 */
bool add_student(roster_t *roster,
                 const char *name,
                 uint8_t grade) {
  /* Check if student with that name already exists in the roster. */
  for (size_t i = 0; i < roster->count; ++i)
    if (!strcmp(roster->students[i].name, name))
      return 0;

  strcpy(roster->students[roster->count].name, name);
  roster->students[roster->count].grade = grade;
  roster->count++;

  qsort(roster->students,
        roster->count,
        sizeof(student_t),
        sort_by_grade_name_desc);

  return 1;
}

/**
 * Returns a roster with students from a specific grade only.
 */
roster_t get_grade(roster_t *roster, uint8_t grade) {
  roster_t grade_roster;
  init_roster(&grade_roster);

  for (size_t i = 0; i < roster->count; ++i) {
    if (roster->students[i].grade != grade)
      continue;

    grade_roster.students[grade_roster.count].grade = roster->students[i].grade;
    strcpy(grade_roster.students[grade_roster.count].name, roster->students[i].name);

    grade_roster.count++;
  }

  return grade_roster;
}
